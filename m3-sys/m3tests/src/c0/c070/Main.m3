(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: scopes *)
(* BUG: if the content bar is moved to Main, the result is wrong *)

MODULE Main;

BEGIN

  PROCEDURE bar () =
    BEGIN
      VAR x: INTEGER;
      PROCEDURE foo () = BEGIN x := 0; END foo;
      BEGIN foo (); END;

      VAR x: REAL;
      PROCEDURE foo () = BEGIN x := 0.0; END foo;
      BEGIN foo (); END;
    END bar;

  BEGIN
    bar ();
  END;

END Main.

