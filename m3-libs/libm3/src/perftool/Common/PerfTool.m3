(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last Modified On Wed Jul 14 16:37:42 PDT 1993 by kalsow                   *)
(*      Modified On Wed Feb  3 22:59:13 PST 1993 by mjordan                  *)
(*      Modified On Fri May 29 16:48:29 PDT 1992 by muller                   *)

MODULE PerfTool;

IMPORT Rd, Wr, File, FileRd, FileWr, Text, LowPerfTool, RTParams, OSError;

PROCEDURE Start (name: Text.T; VAR rd: Rd.T; VAR wr: Wr.T): BOOLEAN =
  VAR r, w: File.T;
BEGIN
  IF NOT LowPerfTool.Start (name, r, w) THEN 
    RETURN FALSE; END;

  (* create rd and wr *)
  TRY
    rd := NEW(FileRd.T).init (r);
    wr := NEW(FileWr.T).init (w);
  EXCEPT
    | OSError.E => RETURN FALSE; END;
  RETURN TRUE;
END Start;

PROCEDURE StartAndWait (name: Text.T; VAR rd: Rd.T; VAR wr: Wr.T): BOOLEAN =
  VAR r, w: File.T;
BEGIN
  IF NOT LowPerfTool.StartAndWait (name, r, w) THEN
    RETURN FALSE; END;

  TRY
    rd := NEW(FileRd.T).init (r);
    wr := NEW(FileWr.T).init (w);
  EXCEPT 
    | OSError.E => RETURN FALSE; END;
  RETURN TRUE;
END StartAndWait;

PROCEDURE ParamStart (param: Text.T; VAR rd: Rd.T; wr: Wr.T): BOOLEAN =
  VAR value := RTParams.Value (param);
  BEGIN
    IF value = NIL THEN
      RETURN FALSE;
    ELSIF Text.Length (value) = 0 THEN
      RETURN Start (param, rd, wr);
    ELSE 
      RETURN Start (value, rd, wr); END;
  END ParamStart;

PROCEDURE ParamStartAndWait (param: Text.T;
                             VAR rd: Rd.T; VAR wr: Wr.T): BOOLEAN =
  VAR value := RTParams.Value (param);
  BEGIN
    IF value = NIL THEN
      RETURN FALSE;
    ELSIF Text.Length (value) = 0 THEN
      RETURN StartAndWait (param, rd, wr);
    ELSE 
      RETURN StartAndWait (value, rd, wr); END;
  END ParamStartAndWait;

      

BEGIN
END PerfTool.
