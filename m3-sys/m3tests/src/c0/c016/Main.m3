(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: TRY EXCEPT statements *)

MODULE Main;

EXCEPTION
  e1;
  e2 (INTEGER);

VAR
  i: INTEGER;

BEGIN

  TRY
    i := 1;
  EXCEPT
  | e1 =>  i := 2;
  | e2 =>  i := 3;
  END;

  TRY
    i := 1;
  EXCEPT
  | e1 =>  i := 2;
  | e2 =>  i := 3;
  ELSE     i := 4;
  END;

  TRY
    i := 1;
  EXCEPT
  | e1    =>  i := 2;
  | e2(x) =>  i := x;
  END;

  TRY
    i := 1;
  EXCEPT
  | e1    =>  i := 2;
  | e2(x) =>  i := x;
  ELSE        i := 4;
  END;

END Main.
