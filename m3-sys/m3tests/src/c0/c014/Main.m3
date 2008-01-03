(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: TRY FINALLY & RETURN statements *)

MODULE Main;

VAR b: BOOLEAN;

PROCEDURE P (i: INTEGER) =
  VAR j: INTEGER;
  BEGIN
    j := 1;
    IF b THEN RETURN END;
    i := 3;
  END P;

PROCEDURE Q (i: INTEGER): INTEGER =
  VAR j: INTEGER;
  BEGIN
    j := 1;
    IF b THEN RETURN  j+2 END;
    i := 3;
    RETURN j+3;
  END Q;

PROCEDURE PP (i: INTEGER) =
  VAR j: INTEGER;
  BEGIN
    j := 1;
    TRY
      j := 2;
      IF b THEN RETURN END;
      j := 3;
    FINALLY
      j := 4;
      IF b THEN RETURN END;
      j := 5;
    END;
    IF b THEN RETURN END;
    i := 6;
  END PP;

PROCEDURE QQ (i: INTEGER): INTEGER =
  VAR j: INTEGER;
  BEGIN
    j := 1;
    TRY
      j := 2;
      IF b THEN RETURN j+0 END;
      j := 3;
    FINALLY
      j := 4;
      IF b THEN RETURN j+1 END;
      j := 5;
    END;
    IF b THEN RETURN j+2 END;
    i := 6;
    RETURN j+3;
  END QQ;

VAR ii: [0..9];
BEGIN
  P (3);
  ii := Q (3);
  EVAL PP;
  EVAL QQ;
END Main.
