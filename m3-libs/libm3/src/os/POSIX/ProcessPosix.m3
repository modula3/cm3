(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Thu Mar 30 14:54:37 PST 1995 by mcjones    *)
(*      modified on Wed Jun 22 16:45:37 PDT 1994 by kalsow     *)
(*      modified on Mon Feb 22 11:41:21 PST 1993 by mjordan    *)

UNSAFE MODULE ProcessPosix EXPORTS Process;

IMPORT OSError, Pathname, Process, File;
FROM ProcessPosixCommon IMPORT Create_ForkExec;

PROCEDURE Create(
    cmd: Pathname.T;
    READONLY params: ARRAY OF TEXT; 
    env: REF ARRAY OF TEXT := NIL;
    wd: Pathname.T := NIL; 
    stdin, stdout, stderr: File.T := NIL)
  : Process.T RAISES {OSError.E} =
  BEGIN
      RETURN Create_ForkExec(cmd, params, env, wd, stdin, stdout, stderr);
  END Create;

BEGIN
END ProcessPosix.
