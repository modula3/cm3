(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Thu Mar 30 14:54:37 PST 1995 by mcjones    *)
(*      modified on Wed Jun 22 16:45:37 PDT 1994 by kalsow     *)
(*      modified on Mon Feb 22 11:41:21 PST 1993 by mjordan    *)

UNSAFE MODULE ProcessCygwin EXPORTS Process;

IMPORT Cerrno, Ctypes, File, FilePosix, OSError, OSErrorPosix,
  Pathname, RTLinker, Unix, Uerror, ProcessPosixCommon, Process;
IMPORT Uexec;

FROM ProcessPosixCommon IMPORT ArrCStr, GetPathToExec, AllocArgs,
    AllocEnv, FreeArgs, FreeEnv, stdin_g, stdout_g, stderr_g,
    Create_ForkExec, BinSh;

PROCEDURE Create_Spawn(
    cmd: Pathname.T;
    READONLY params: ARRAY OF TEXT; 
    env: REF ARRAY OF TEXT)
  : Process.T RAISES {OSError.E} =
  VAR
    envx: ArrCStr;
    envp: Ctypes.char_star_star;
    execResult: INTEGER;
    execErrno: Ctypes.int;
    (* lots of internal compiler errors path := Pathname.T; *)
    path := GetPathToExec(cmd);
    argx := AllocArgs(path, Pathname.Base(cmd), params);
  BEGIN
    IF env # NIL THEN
      envx := AllocEnv(env^);
      envp := ADR(envx[0])
    ELSE
      envx := NIL;
      envp := LOOPHOLE(RTLinker.envp, Ctypes.char_star_star)
    END;

    execResult := ExecChild_Spawn(argx, envp);
    execErrno := Cerrno.GetErrno();

    FreeArgs(argx);
    IF envx # NIL THEN FreeEnv(envx) END;

    (* Did the execve succeed? *)
    IF execResult < 0 THEN
      OSErrorPosix.Raise0(execErrno)
    END;

    RETURN NEW(T, pid := execResult)
  END Create_Spawn;

PROCEDURE Create(
    cmd: Pathname.T;
    READONLY params: ARRAY OF TEXT; 
    env: REF ARRAY OF TEXT := NIL;
    wd: Pathname.T := NIL; 
    stdin, stdout, stderr: File.T := NIL)
  : Process.T RAISES {OSError.E} =
  BEGIN
    IF (wd # NIL)
        OR (stdin # NIL AND stdin.fd # stdin_g.fd)
        OR (stdout # NIL AND stdout.fd # stdout_g.fd)
        OR (stderr # NIL AND stderr.fd # stderr_g.fd) THEN
      RETURN Create_ForkExec(cmd, params, env, wd, stdin, stdout, stderr);
    ELSE
      RETURN Create_Spawn(cmd, params, env);
    END;
  END Create;

PROCEDURE ExecChild_Spawn(
    argx: ArrCStr; (* see "AllocArgs" for layout *)
    envp: Ctypes.char_star_star) : INTEGER
  RAISES {} =
  VAR res : INTEGER; t: Ctypes.char_star;
  BEGIN
    res := Uexec.spawnve(Uexec.P_NOWAIT, (*path*)argx[0], ADR(argx[2]), envp);
    IF res < 0 THEN
      IF Cerrno.GetErrno() = Uerror.ENOEXEC THEN
        t := argx[0]; argx[0] := argx[2]; argx[2] := t;
        res := Unix.execve(BinSh, ADR(argx[1]), envp);
      END;
    END;
    RETURN res;
  END ExecChild_Spawn;

BEGIN
END ProcessCygwin.
