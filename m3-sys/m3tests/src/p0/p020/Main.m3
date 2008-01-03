(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: TRY FINALLY & RETURN statements *)

MODULE Main;

FROM Test IMPORT checkI, done;

PROCEDURE P (VAR i: INTEGER)=
  VAR j: INTEGER;
  BEGIN
    j := 1;
    RETURN;
    i := 3;
  END P;

PROCEDURE Q (i: INTEGER): INTEGER=
  VAR j: INTEGER;
  BEGIN
    j := 1;
    RETURN  j+2;
    i := 3;
  END Q;

PROCEDURE PP (VAR i: INTEGER)=
  VAR j: INTEGER;
  BEGIN
    j := 1;
    TRY
      j := 2;
      RETURN;
      j := 3;
    FINALLY
      j := 4;
      RETURN;
      j := 5;
    END;
    RETURN;
    i := 6;
  END PP;

PROCEDURE QQ (i: INTEGER): INTEGER=
  VAR j: INTEGER;
  BEGIN
    j := 1;
    TRY
      j := 2;
      RETURN j+10;
      j := 3;
    FINALLY
      j := 4;
      RETURN j+1;
      j := 5;
    END;
    RETURN j+2;
    i := 6;
  END QQ;

VAR k: INTEGER;
BEGIN
  k := 7;
  P (k);
  checkI (k, 7);
  k := Q (10);
  checkI (k, 3);
  PP (k);
  checkI (k, 3);
  k := QQ (10);
  checkI (k, 5);

  done ();
END Main.
