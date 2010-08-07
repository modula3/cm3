(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Thu Mar 30 14:54:37 PST 1995 by mcjones    *)
(*      modified on Wed Jun 22 16:45:37 PDT 1994 by kalsow     *)
(*      modified on Mon Feb 22 11:41:21 PST 1993 by mjordan    *)

UNSAFE MODULE ProcessPosixOS2 EXPORTS Process2;

IMPORT Atom, AtomList, Ctypes, Env, File, FilePosix, M3toC, OSError,
  OSErrorPosix, Pathname, RTLinker, RTProcess, RTSignal,
  SchedulerPosix, Text, Thread, Unix, Uerror, Uexec, Uprocess, Ustat,
  Utime, Uugid, Word;

IMPORT Msg, Fmt;
 
CONST
 UNIX_F_GETFL  = 1;
 UNIX_F_SETFL  = 2;
 UNIX_F_GETFD  = 3;
 UNIX_F_SETFD  = 4;
 UNIX_F_DUPFD  = 5;

REVEAL T = BRANDED REF RECORD
    pid: INTEGER;
    waitOk := TRUE
  END;

PROCEDURE Create(
    cmd: Pathname.T;
    READONLY params: ARRAY OF TEXT; 
    env: REF ARRAY OF TEXT := NIL;
    wd: Pathname.T := NIL; 
    stdin, stdout, stderr: File.T := NIL)
  : T RAISES {OSError.E} =
  VAR
    argx: ArrCStr;
    envx: ArrCStr;
    envp: Ctypes.char_star_star;
    wdstr: Ctypes.char_star;
    oit, nit: Utime.struct_itimerval;
    forkResult, execResult: INTEGER;
    forkErrno, execErrno: Ctypes.int;
    waitStatus: Uexec.w_A;
  BEGIN
    WITH path = GetPathToExec(cmd) DO
      Msg.D("cmd path = " & path);
      argx := AllocArgs(path, Pathname.Base(cmd), params)
    END;
    IF env # NIL THEN
      envx := AllocEnv(env^);
      envp := ADR(envx[0])
    ELSE
      envx := NIL;
      envp := LOOPHOLE(RTLinker.info.envp, Ctypes.char_star_star)
    END;
    IF wd # NIL AND NOT Text.Empty(wd) THEN
      wdstr := M3toC.TtoS(wd)
    ELSE
     wdstr := NIL
    END;

    (* Turn off the interval timer (so it won't be running in child). *)
    nit := Utime.struct_itimerval {
             it_interval := Utime.struct_timeval {0, 0},
             it_value    := Utime.struct_timeval {0, 0}}; 
    IF Utime.setitimer(Utime.ITIMER_VIRTUAL, nit, oit) < 0 THEN
      <* ASSERT FALSE *>
    END;

    (* Disable the scheduler. *)
    SchedulerPosix.DisableSwitching ();

    execResult := 0;
    forkResult := Unix.vfork();
    IF forkResult = 0 THEN (* in the child *)
      Msg.D("child process born");
      ExecChild(argx, envp, wdstr, stdin, stdout, stderr);
     (* If ExecChild returns, the execve.  Let's try to leave a note
        for our parent, in case we're still sharing their address
        space. *)
      Msg.D("child process execve failed");
      execResult := -1;
      execErrno := Uerror.errno;
      Unix.underscore_exit(99)
    END;

    (* Back in parent. *)

    forkErrno := Uerror.errno;
    Msg.D("errno after forking: " & Fmt.Int(forkErrno));

    (* Enable scheduler. *)
    SchedulerPosix.EnableSwitching ();

    (* Restore previous virtual timer. *)
    IF Utime.setitimer(Utime.ITIMER_VIRTUAL, oit, nit) < 0 THEN
      <* ASSERT FALSE *>
    END;

    FreeArgs(argx);
    IF envx # NIL THEN FreeEnv(envx) END;

    IF forkResult < 0 THEN OSErrorPosix.Raise0(forkErrno) END;

    (* The vfork succeeded.  Did the execve succeed? *)
    IF execResult < 0 THEN
      (* No, clean up child process. *)
      EVAL Uexec.waitpid(forkResult, ADR(waitStatus), 0);
      OSErrorPosix.Raise0(execErrno)
    END;

    RETURN NEW(T, pid := forkResult)
  END Create;

TYPE ArrCStr = UNTRACED REF ARRAY OF Ctypes.char_star;

PROCEDURE GetPathToExec(pn: Pathname.T): Pathname.T RAISES {OSError.E} =
(* Return the filename to execute given "base" and the value of the "PATH"
   environment variable. Based on Mike Burrows's preexec(). *)
  VAR
    path, prog, ext: TEXT;
    start, i, end, result, uid, gid: INTEGER;
    statBuf: Ustat.struct_stat;
  CONST MaskXXX = Ustat.S_IEXEC + Ustat.S_GEXEC + Ustat.S_OEXEC;
  BEGIN
    IF Text.FindChar(pn, '/') < 0 AND
       Text.FindChar(pn, '\\') < 0 THEN
      path := Env.Get("PATH");
      IF path = NIL THEN path := ";/bin;/usr/bin" END;
      IF Text.FindChar(pn, '.') < 0 THEN
        ext := "exe";
      ELSE
        ext := NIL;
      END;
      uid := -1; gid := -1;
      start := 0;
      LOOP
        i := Text.FindChar(path, ';', start);
        IF i < 0 THEN end := Text.Length(path) ELSE end := i END;
        prog := Pathname.Join(
          Text.Sub(path, start, end - start),
          pn,
          ext);
        Msg.D("trying prog = " & prog);
        result := Ustat.stat(M3toC.TtoS(prog), ADR(statBuf));
        IF result = 0 AND Word.And(statBuf.st_mode, Ustat.S_IFMT) = Ustat.S_IFREG THEN
          statBuf.st_mode := Word.And(statBuf.st_mode, MaskXXX);
          IF statBuf.st_mode # 0 THEN
            IF statBuf.st_mode = MaskXXX THEN RETURN prog END;
            IF uid < 0 THEN uid := Uugid.geteuid() END;
            IF uid = statBuf.st_uid THEN
	      statBuf.st_mode := Word.And(statBuf.st_mode, Ustat.S_IEXEC)
            ELSE
              IF gid < 0 THEN gid := Uugid.getegid() END;
              IF gid = statBuf.st_gid THEN
                statBuf.st_mode := Word.And(statBuf.st_mode, Ustat.S_GEXEC)
              ELSE
        	statBuf.st_mode := Word.And(statBuf.st_mode, Ustat.S_OEXEC)
              END
            END;
            IF statBuf.st_mode # 0 THEN RETURN prog END
          END;
	END;
	IF i < 0 THEN EXIT END;
	start := i + 1
      END;
      OSErrorPosix.Raise0(Uerror.ENOENT)
    ELSE (* pn contains '/' *)
      IF Ustat.stat(M3toC.TtoS(pn), ADR(statBuf)) < 0 THEN
        OSErrorPosix.Raise()
      END
    END;
    RETURN pn
  END GetPathToExec;

PROCEDURE AllocArgs(path, base: TEXT; READONLY args: ARRAY OF TEXT): ArrCStr =
(* Return an array of pointers, say "a", with:
|    a[0] = path
|    a[1] = "sh"
|    a[2] = base
|    a[3+i] = args[i] for i = 0,...,LAST(args)
|    a[n] = NIL for n = NUMBER(args) + 3
*)
  VAR argx := NEW(ArrCStr, NUMBER(args) + 4);
  BEGIN
    argx[0] := M3toC.CopyTtoS(path);
    argx[1] := Sh;
    argx[2] := M3toC.CopyTtoS(base);
    FOR i := 0 TO LAST(args) DO argx[3 + i] := M3toC.CopyTtoS(args[i]) END;
    argx[LAST(argx^)] := NIL;
    RETURN argx
  END AllocArgs;

PROCEDURE FreeArgs(VAR argx: ArrCStr) =
(* Free all elements except "argx[1]", which equals "Sh".  Note that
   "ExecChild" may swap "argx[0]" and "argx[2]". *)
  BEGIN
    FOR i := 0 TO LAST(argx^) - 1 DO
      IF i # 1 THEN M3toC.FreeCopiedS(argx[i]) END
    END;
    <* ASSERT argx[LAST(argx^)] = NIL *>
    DISPOSE(argx)
  END FreeArgs;

PROCEDURE AllocEnv(READONLY env: ARRAY OF TEXT): ArrCStr =
  VAR envx := NEW(ArrCStr, NUMBER(env) + 1);
  BEGIN
    FOR i := 0 TO LAST(env) DO envx[i] := M3toC.CopyTtoS(env[i]) END;
    envx[LAST(envx^)] := NIL;
    RETURN envx
  END AllocEnv;

PROCEDURE FreeEnv(VAR envx: ArrCStr) =
  BEGIN
    FOR i := 0 TO LAST(envx^) - 1 DO
      M3toC.FreeCopiedS(envx[i])
    END;
    <* ASSERT envx[LAST(envx^)] = NIL *>
    DISPOSE(envx)
  END FreeEnv;

VAR (*CONST*)
  BinSh := M3toC.TtoS("/bin/sh");
  Sh := M3toC.TtoS("sh");

PROCEDURE ExecChild(
    argx: ArrCStr; (* see "AllocArgs" for layout *)
    envp: Ctypes.char_star_star;
    wdstr: Ctypes.char_star;
    stdin, stdout, stderr: File.T)
  RAISES {} =
(* Modify Unix state using "stdin", ..., and invoke execve using
   "argx" and "envp".  Do not invoke scheduler, allocator, or
   exceptions.  Return only if a fatal Unix error is encountered, in
   which case Uerror.errno is set. *)
  PROCEDURE SetFd(fd: INTEGER; h: File.T): BOOLEAN =
  (* Make file descriptor "fd" refer to file "h", or set "fd"'s
     close-on-exec flag if "h=NIL".  Return "TRUE" if succesful. *)
    VAR ok : BOOLEAN;
    BEGIN
      IF h # NIL THEN 
        ok := NOT Unix.dup2(h.fd, fd) < 0;
        IF NOT ok THEN
          Msg.D("SetFd failed for h # NIL");
        END;
        RETURN ok;
      END;
      ok := Unix.fcntl(fd, UNIX_F_SETFD, 1) # -1;
      IF NOT ok THEN
        Msg.D("SetFd failed for h = NIL");
      END;
      RETURN ok;
    END SetFd;
  VAR res := 0; t: Ctypes.char_star;
  BEGIN
    IF wdstr # NIL THEN
      IF Unix.chdir(wdstr) < 0 THEN
        Msg.D("chdir failed");
        RETURN;
      END
    END;
    IF NOT (SetFd(0, stdin) AND SetFd(1, stdout) AND SetFd(2, stderr)) THEN
      Msg.D("setting stdin, stdout, stderr failed");
      RETURN
    END;
    FOR fd := 3 TO Unix.getdtablesize() - 1 DO
      EVAL Unix.close(fd) (* ignore errors *)
    END;
    (* Modula-3 ignores SIGPIPE, but most programs don't expect that: *)
    RTSignal.RestoreHandlers();
    res := Unix.execve((*path*)argx[0], ADR(argx[2]), envp);
    Msg.D("execve returned " & Fmt.Int(res));
    <* ASSERT res < 0 *>
    IF Uerror.errno = Uerror.ENOEXEC THEN
      Msg.D("trying again via shell");
      t := argx[0]; argx[0] := argx[2]; argx[2] := t;
      res := Unix.execve(BinSh, ADR(argx[1]), envp);
      Msg.D("execve returned " & Fmt.Int(res));
      <* ASSERT res < 0 *>
    END
  END ExecChild;

EXCEPTION WaitAlreadyCalled;

PROCEDURE Wait(p: T): ExitCode = <* FATAL WaitAlreadyCalled *> 
  VAR result: Ctypes.int;  status: Uexec.w_A;
  CONST Delay = 0.2D0;
  BEGIN
    IF NOT p.waitOk THEN RAISE WaitAlreadyCalled END;
    p.waitOk := FALSE;
    (* By rights, the SchedulerPosix interface should have a WaitPID
       procedure that is integrated with the thread scheduler. *)
    LOOP
      result := Uexec.waitpid(p.pid, ADR(status), Uexec.WNOHANG);
      IF result # 0 THEN EXIT END;
      Thread.Pause(Delay)
    END;
    <* ASSERT result > 0 *>
    IF Word.And(status, LAST(ExitCode)) = status THEN
      RETURN status
    ELSE
      RETURN LAST(ExitCode)
    END
  END Wait;

PROCEDURE Exit(n: ExitCode) =
  BEGIN
    RTProcess.Exit(n)
  END Exit;

PROCEDURE Crash(msg: TEXT) =
  BEGIN
    RTProcess.Crash(msg)
  END Crash;

PROCEDURE RegisterExitor(p: PROCEDURE()) =
  BEGIN
    RTProcess.RegisterExitor(p)
  END RegisterExitor;

PROCEDURE GetID(p: T): ID =
  BEGIN
    RETURN p.pid
  END GetID;

PROCEDURE GetMyID(): ID =
  BEGIN
    RETURN Uprocess.getpid()
  END GetMyID;

PROCEDURE GetStandardFileHandles(VAR stdin, stdout, stderr: File.T) =
  BEGIN
    stdin := stdin_g; stdout := stdout_g; stderr := stderr_g
  END GetStandardFileHandles;

VAR
  wdCacheMutex := NEW(MUTEX);
  wdCache: Pathname.T := NIL; (* NIL => unknown *)
(* The main purpose for this cache is speeding up FS.Iterate when it
   is called with a relative pathname. *)

PROCEDURE GetWorkingDirectory(): Pathname.T RAISES {OSError.E} =
  VAR
    buffer: ARRAY [0..Unix.MaxPathLen] OF Ctypes.char;
    rc: Ctypes.char_star;
  BEGIN
    LOCK wdCacheMutex DO
      IF wdCache = NIL THEN
        rc := Unix.getwd(ADR(buffer[0]));
        IF rc = NIL THEN
          RAISE OSError.E(
            NEW(AtomList.T,
              head := Atom.FromText(M3toC.CopyStoT(ADR(buffer[0]))),
              tail := NIL))
        END;
        wdCache := M3toC.CopyStoT(ADR(buffer[0]))
      END;
      RETURN wdCache
    END
  END GetWorkingDirectory;

PROCEDURE SetWorkingDirectory(pn: Pathname.T) RAISES {OSError.E} =
  BEGIN
    LOCK wdCacheMutex DO
      IF Unix.chdir(M3toC.TtoS(pn)) < 0 THEN OSErrorPosix.Raise() END;
      wdCache := NIL
    END
  END SetWorkingDirectory;


(* Initialization *)

PROCEDURE GetFileHandle(fd: INTEGER; ds: FilePosix.DirectionSet): File.T =
  VAR f: File.T := NIL;
  BEGIN
    TRY f := FilePosix.New(fd, ds);
    EXCEPT 
    | OSError.E => (* not available *)
    END;
    RETURN f
  END GetFileHandle;

VAR stdin_g, stdout_g, stderr_g: File.T;

BEGIN
  stdin_g := GetFileHandle(0, FilePosix.Read);
  stdout_g := GetFileHandle(1, FilePosix.Write);
  stderr_g := GetFileHandle(2, FilePosix.Write)
END ProcessPosixOS2.
