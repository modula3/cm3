(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(* Last Modified On Fri Feb 11 14:59:57 PST 1994 by kalsow                   *)
(*      Modified On Sat Feb  6 11:41:23 PST 1993 by mjordan                  *)
(*      Modified On Fri May 29 17:37:42 PDT 1992 by muller                   *)

UNSAFE MODULE RTPerfTool;

IMPORT Ctypes, Unix, Text, M3toC, Uexec, Uuio, RTParams, RTProcess;

PROCEDURE Start (param: TEXT;  VAR w: Handle): BOOLEAN =
  VAR value: TEXT;  c: Ctypes.char;  r: Handle;
  BEGIN
    value := RTParams.Value (param);
    IF value = NIL THEN  RETURN FALSE; END;
    IF Text.Length (value) = 0 THEN  value := param;  END;
    IF NOT StartTool (value, r, w) THEN  RETURN FALSE; END;
    EVAL Uuio.read (r, ADR (c), 1);
    EVAL Unix.close (r);
    RETURN TRUE;
  END Start;

PROCEDURE Close (w: Handle) =
  BEGIN
    EVAL Unix.close (w);
  END Close;

PROCEDURE Send (w: Handle;  at: ADDRESS;  len: CARDINAL): BOOLEAN =
  BEGIN
    RETURN Uuio.write (w, at, len) # -1;
  END Send;

(*-------------------------------------------------------------- internal ---*)

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

PROCEDURE StartTool (name: TEXT; VAR r, w: Handle): BOOLEAN =
  VAR
    toTool   : Pipe;
    fromTool : Pipe;
    args     : ARRAY [0..1] OF Ctypes.char_star;
    execResult : INTEGER := 0;
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
      args [0] := M3toC.SharedTtoS (name);
      args [1] := NIL;
      execResult := Uexec.execvp (args [0], ADR (args [0]));
      Unix.underscore_exit (99);
      RETURN FALSE;
  
    ELSE (* in the parent, after the child has been forked *)
 
      (* close the unused ends of the pipes *)
      EVAL Unix.close (toTool [readPort]);
      EVAL Unix.close (fromTool [writePort]);

      (* update r and w *)
      r := fromTool [readPort];
      w := toTool [writePort];

      RETURN (execResult >= 0);
    END; (*CASE*)

  END StartTool;

BEGIN
END RTPerfTool.
