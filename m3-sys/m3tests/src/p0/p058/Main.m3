(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

FROM Test IMPORT checkI, done;

PROCEDURE P() RAISES {}=
  VAR
    buffer: ARRAY [0..15] OF CHAR;
  BEGIN
    WITH
        half = SUBARRAY(buffer, 0, 8),
        quarter = SUBARRAY(buffer, 0, 4) DO
      Check(half, 8);
      Check(quarter, 4);
    END;
  END P;

PROCEDURE Check(READONLY a: ARRAY OF CHAR; size: CARDINAL) RAISES {}=
  BEGIN
    checkI (NUMBER (a), size);
  END Check;


BEGIN
  P();
  done ();
END Main.

