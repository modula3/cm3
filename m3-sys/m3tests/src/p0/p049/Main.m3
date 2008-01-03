(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

FROM Test IMPORT done, checkI;

TYPE
   RecordType = PROCEDURE (xin, yin: INTEGER; first: CHAR);

PROCEDURE ProcessPoint (xin: INTEGER; recordIt: RecordType := NIL;) =
  VAR
    yin: INTEGER := 12;
    first: CHAR;
  BEGIN
    IF recordIt # NIL THEN recordIt (xin, yin, first); END;
  END ProcessPoint;

PROCEDURE Rec (xin, yin: INTEGER; <*UNUSED*>first: CHAR) =
  BEGIN
    checkI (xin, 1);
    checkI (yin, 12);
  END Rec;

BEGIN
  ProcessPoint (0);
  ProcessPoint (1, Rec);

  done ();
END Main.

