(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

TYPE Rec = RECORD i, for: INTEGER;  break := 6;  if: TEXT := NIL END;
TYPE Ref = REF Rec;

PROCEDURE break () =
  PROCEDURE Goto () =
    BEGIN
    END Goto;
  VAR s: Rec := Rec { i := 3, for := 5 };
  BEGIN
    Goto ();
    EVAL s;
  END break;

VAR x := 7;
VAR r := NEW (Ref, i := 3, for := 5 );
VAR s: Rec := Rec { i := 3, for := x };
BEGIN
  break ();
  EVAL s;  EVAL r;
END Main.


