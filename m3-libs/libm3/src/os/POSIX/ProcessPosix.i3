(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

UNSAFE INTERFACE ProcessPosix;

IMPORT Ctypes, File, OSError, Pathname, Process;
FROM Process IMPORT T;

REVEAL Process.T = BRANDED REF RECORD
    pid: INTEGER;
    waitOk := TRUE
  END;

PROCEDURE Create_ForkExec(
    cmd: Pathname.T;
    READONLY params: ARRAY OF TEXT; 
    env: REF ARRAY OF TEXT := NIL;
    wd: Pathname.T := NIL; 
    stdin, stdout, stderr: File.T := NIL)
  : T RAISES {OSError.E};

PROCEDURE GetPathToExec(pn: Pathname.T): Pathname.T RAISES {OSError.E};

TYPE ArrCStr = UNTRACED REF ARRAY OF Ctypes.char_star;

PROCEDURE AllocArgs(path, base: TEXT; READONLY args: ARRAY OF TEXT): ArrCStr;

PROCEDURE FreeArgs(VAR argx: ArrCStr);

PROCEDURE AllocEnv(READONLY env: ARRAY OF TEXT): ArrCStr;

PROCEDURE FreeEnv(VAR envx: ArrCStr);

PROCEDURE ExecChild(
    argx: ArrCStr; (* see "AllocArgs" for layout *)
    envp: Ctypes.char_star_star;
    wdstr: Ctypes.char_star;
    stdin, stdout, stderr: INTEGER) : INTEGER
  RAISES {};

VAR stdin_g, stdout_g, stderr_g: File.T;
(*CONST*) BinSh : Ctypes.char_star;

END ProcessPosix.
