(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last Modified On Wed Mar 23 08:58:00 PST 1994 by kalsow                   *)
(*      Modified On Sat Feb  6 11:41:23 PST 1993 by mjordan                  *)
(*      Modified On Fri May 29 17:37:42 PDT 1992 by muller                   *)

UNSAFE MODULE LowPerfTool;

IMPORT Ctypes, File, Unix, Text, M3toC, Uexec, Uuio, RTParams;
IMPORT FilePosix, OSError, RTProcess;

CONST 
  readPort = 0;
  writePort = 1;

TYPE
  Pipe = ARRAY [0..1] OF Ctypes.int; 

PROCEDURE ClosePipe (READONLY p: Pipe) =
  BEGIN
    EVAL Unix.close (p[readPort]);
    EVAL Unix.close (p[writePort]);
  END ClosePipe;

PROCEDURE Start (name: Text.T; VAR r, w: File.T): BOOLEAN =
  VAR toTool, fromTool: Pipe;
  BEGIN
    (* open a pipe to send bytes to the performance tool *)
    IF Unix.pipe (toTool) = -1 THEN RETURN FALSE; END;
 
    (* open a pipe to get bytes from the performance tool *)
    IF Unix.pipe (fromTool) = -1 THEN  ClosePipe (toTool); RETURN FALSE; END;
  
    (* Create the tool process *)
    CASE RTProcess.Fork () OF
      
    | -1 => (* fork failed *)
      ClosePipe (fromTool);
      ClosePipe (toTool);
      RETURN FALSE;

    | 0  => (* in the child *)

      (* close the unused ends of the pipes *)
      EVAL Unix.close (toTool [writePort]);
      EVAL Unix.close (fromTool [readPort]);

      (* connect the useful ends to stdin and stdout *)
      IF toTool [readPort] # 0 THEN
        IF Unix.dup2 (toTool [readPort], 0) = -1 THEN RETURN FALSE; END;
        EVAL Unix.close (toTool [readPort]);
      END;
      IF fromTool [writePort] # 1 THEN
        IF Unix.dup2 (fromTool [writePort], 1) = -1 THEN RETURN FALSE; END;
        EVAL Unix.close (fromTool [writePort]);
      END;
      
      (* execute the perf tool *)
      VAR args: ARRAY [0..1] OF Ctypes.char_star; BEGIN
        args [0] := M3toC.SharedTtoS (name);
        args [1] := NIL;
        EVAL Uexec.execvp (args [0], ADR (args [0]));
      END;
      
      RETURN FALSE;
  
    ELSE (* in the parent, after the child has been forked *)

      (* close the unused ends of the pipes *)
      EVAL Unix.close (toTool [readPort]);
      EVAL Unix.close (fromTool [writePort]);

      TRY
        (* update r and w *)
        r := FilePosix.New (fromTool [readPort], FilePosix.Read);
        w := FilePosix.New (toTool [writePort], FilePosix.Write);
      EXCEPT OSError.E =>
        r := NIL;  w := NIL;
        RETURN FALSE;
      END;

    END; (*CASE*)

    RETURN TRUE;
  END Start;

PROCEDURE StartAndWait (name: Text.T; VAR r,w: File.T): BOOLEAN =
  VAR c: Ctypes.char;
  BEGIN
    IF NOT Start (name, r, w) THEN RETURN FALSE; END;
    EVAL Uuio.read (r.fd, ADR (c), 1);
    RETURN TRUE;
  END StartAndWait;
      
PROCEDURE ParamStart (param: Text.T; VAR r,w: File.T): BOOLEAN =
  VAR value := RTParams.Value (param);
  BEGIN
    IF value = NIL THEN
      RETURN FALSE;
    ELSIF Text.Length (value) = 0 THEN
      RETURN Start (param, r, w);
    ELSE 
      RETURN Start (value, r, w);
    END;
  END ParamStart;

PROCEDURE ParamStartAndWait (param: Text.T; VAR r,w: File.T): BOOLEAN =
  VAR value := RTParams.Value (param);
  BEGIN
    IF value = NIL THEN
      RETURN FALSE;
    ELSIF Text.Length (value) = 0 THEN
      RETURN StartAndWait (param, r, w);
    ELSE 
      RETURN StartAndWait (value, r, w);
    END;
  END ParamStartAndWait;


BEGIN
END LowPerfTool.
