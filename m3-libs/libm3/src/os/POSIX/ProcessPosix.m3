(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Thu Mar 30 14:54:37 PST 1995 by mcjones    *)
(*      modified on Wed Jun 22 16:45:37 PDT 1994 by kalsow     *)
(*      modified on Mon Feb 22 11:41:21 PST 1993 by mjordan    *)

UNSAFE MODULE ProcessPosix EXPORTS Process;

IMPORT Atom, AtomList, Cerrno, Ctypes, Env, File, FilePosix, M3toC, OSError,
  OSErrorPosix, Pathname, RTLinker, RTProcess, RTSignal,
  SchedulerPosix, Text, Thread, Unix, Uerror, Uexec, Uprocess, Ustat,
  Utime, Uugid, Word;
 
REVEAL T = BRANDED REF RECORD
    pid: INTEGER;
    waitOk := TRUE
  END;

CONST
  NoFileDescriptor: INTEGER = -1;
  (* A non-existent file descriptor *)

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
    stdin_fd, stdout_fd, stderr_fd: INTEGER := NoFileDescriptor;
  BEGIN
    VAR path := GetPathToExec(cmd); BEGIN
      (* make sure the result is an absolute pathname if "wd # NIL" *)
      IF wd # NIL AND NOT Text.Empty(wd) AND NOT Pathname.Absolute(path) THEN
        path := Pathname.Join(GetWorkingDirectory(), path, ext := NIL);
        <* ASSERT Pathname.Absolute(path) *>
      END;
      argx := AllocArgs(path, Pathname.Base(cmd), params)
    END;
    IF env # NIL THEN
      envx := AllocEnv(env^);
      envp := ADR(envx[0])
    ELSE
      envx := NIL;
      envp := LOOPHOLE(RTLinker.envp, Ctypes.char_star_star)
    END;
    IF wd # NIL AND NOT Text.Empty(wd) THEN
      wdstr := M3toC.SharedTtoS(wd)
    ELSE
     wdstr := NIL
    END;

    (* grab the file descriptors from inside the traced File.Ts so
       we don't trigger a GC after the vfork() call. *)
    stdin_fd  := NoFileDescriptor;  
    IF (stdin  # NIL) THEN stdin_fd  := stdin.fd;  END;
    stdout_fd := NoFileDescriptor;
    IF (stdout # NIL) THEN stdout_fd := stdout.fd; END;
    stderr_fd := NoFileDescriptor;
    IF (stderr # NIL) THEN stderr_fd := stderr.fd; END;

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
      execResult := ExecChild(argx, envp, wdstr, stdin_fd, stdout_fd,
          stderr_fd);
      (* If ExecChild returns, the execve failed. Let's try to leave
        a note for our parent, in case we're still sharing their
        address space. *)
      execErrno := Cerrno.GetErrno();
      Unix.underscore_exit(99)
    END;

    (* Back in parent. *)

    forkErrno := Cerrno.GetErrno();

    (* Enable scheduler. *)
    SchedulerPosix.EnableSwitching ();

    (* Restore previous virtual timer. *)
    IF Utime.setitimer(Utime.ITIMER_VIRTUAL, oit, nit) < 0 THEN
      <* ASSERT FALSE *>
    END;

    FreeArgs(argx);
    IF envx # NIL THEN FreeEnv(envx) END;
    IF wdstr # NIL THEN M3toC.FreeSharedS(wd, wdstr); END;

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
    path, prog: TEXT;
    start, i, end, result, uid, gid: INTEGER;
    statBuf: Ustat.struct_stat;
    pname: Ctypes.char_star;
  CONST MaskXXX = Ustat.S_IEXEC + Ustat.S_GEXEC + Ustat.S_OEXEC;
  BEGIN
    IF Text.FindChar(pn, '/') < 0 THEN
      path := Env.Get("PATH");
      IF path = NIL THEN path := ":/bin:/usr/bin" END;
      uid := -1; gid := -1;
      start := 0;
      LOOP
        i := Text.FindChar(path, ':', start);
        IF i < 0 THEN end := Text.Length(path) ELSE end := i END;
        prog := Pathname.Join(Text.Sub(path, start, end - start), pn, NIL);
        pname := M3toC.SharedTtoS(prog);
        result := Ustat.stat(pname, ADR(statBuf));
        M3toC.FreeSharedS(prog, pname);
        IF result = 0 AND 
          Word.And(statBuf.st_mode, Ustat.S_IFMT) = Ustat.S_IFREG THEN
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
      pname := M3toC.SharedTtoS(pn);
      IF Ustat.stat(pname, ADR(statBuf)) < 0 THEN
        result := Cerrno.GetErrno();
        M3toC.FreeSharedS(pn, pname);
        OSErrorPosix.Raise0(result)
      END;
      M3toC.FreeSharedS(pn, pname);
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
  BinSh := M3toC.FlatTtoS("/bin/sh");
  Sh := M3toC.FlatTtoS("sh");

PROCEDURE ExecChild(
    argx: ArrCStr; (* see "AllocArgs" for layout *)
    envp: Ctypes.char_star_star;
    wdstr: Ctypes.char_star;
    stdin, stdout, stderr: INTEGER) : INTEGER
  RAISES {} =
(* Modify Unix state using "stdin", ..., and invoke execve using
   "argx" and "envp".  Do not invoke scheduler, allocator, or
   exceptions.  Return only if a fatal Unix error is encountered, in
   which case Cerrno.GetErrno() is set. *)
  VAR res := 0; t: Ctypes.char_star;
  BEGIN
    IF wdstr # NIL THEN
      IF Unix.chdir(wdstr) < 0 THEN RETURN -1; END
    END;
    IF NOT (SetFd(0, stdin) AND SetFd(1, stdout) AND SetFd(2, stderr)) THEN
      RETURN -1;
    END;
    FOR fd := 3 TO Unix.getdtablesize() - 1 DO
      EVAL Unix.close(fd) (* ignore errors *)
    END;
    (* Modula-3 ignores SIGPIPE, but most programs don't expect that: *)
    RTSignal.RestoreHandlers();
    res := Unix.execve((*path*)argx[0], ADR(argx[2]), envp);
    <* ASSERT res < 0 *>
    IF Cerrno.GetErrno() = Uerror.ENOEXEC THEN
      t := argx[0]; argx[0] := argx[2]; argx[2] := t;
      res := Unix.execve(BinSh, ADR(argx[1]), envp);
      <* ASSERT res < 0 *>
    END;
    RETURN res;
  END ExecChild;

PROCEDURE SetFd(fd: INTEGER; h: INTEGER(*File.T*)): BOOLEAN =
  (* Make file descriptor "fd" refer to file "h", or set "fd"'s
     close-on-exec flag if "h=NoFile".  Return "TRUE" if succesful. *)
  BEGIN
    IF h # NoFileDescriptor THEN
      RETURN NOT Unix.dup2(h, fd) < 0
    ELSIF Unix.fcntl(fd, Unix.F_SETFD, 1) >= 0 THEN
      RETURN TRUE;
    ELSE (* EBADF => "fd" was already closed, don't panic *)
      RETURN (Cerrno.GetErrno() = Uerror.EBADF);
    END;
  END SetFd;

EXCEPTION WaitAlreadyCalled;

PROCEDURE Wait(p: T): ExitCode = <* FATAL WaitAlreadyCalled *> 
  VAR
    result: Ctypes.int;
    statusT: Uexec.w_T;
    statusM3: Uexec.w_M3;
  CONST Delay = 0.1D0;
  BEGIN
    IF NOT p.waitOk THEN RAISE WaitAlreadyCalled END;
    p.waitOk := FALSE;
    (* By rights, the SchedulerPosix interface should have a WaitPID
       procedure that is integrated with the thread scheduler. *)
    LOOP
      result := Uexec.waitpid(p.pid, ADR(statusT), Uexec.WNOHANG);
      IF result # 0 THEN EXIT END;
      Thread.Pause(Delay)
    END;
    <* ASSERT result > 0 *>
    statusM3.w_Filler := 0;
    statusM3.w_Coredump := statusT.w_Coredump;
    statusM3.w_Termsig := statusT.w_Termsig;
    statusM3.w_Retcode := statusT.w_Retcode;
    RETURN MIN(LAST(ExitCode),LOOPHOLE(statusM3,Uexec.w_A));
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
        rc := Unix.getcwd(ADR(buffer[0]), Unix.MaxPathLen+1);
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
  VAR fname := M3toC.SharedTtoS(pn);  err: INTEGER;
  BEGIN
    LOCK wdCacheMutex DO
      IF Unix.chdir(fname) < 0 THEN
        err := Cerrno.GetErrno();
        M3toC.FreeSharedS(pn, fname);
        OSErrorPosix.Raise0(err);
      END;
      wdCache := NIL
    END;
    M3toC.FreeSharedS(pn, fname);
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
END ProcessPosix.
