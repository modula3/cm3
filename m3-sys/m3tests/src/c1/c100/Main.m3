(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;
(* IMPORT Wr, Stdio, Fmt; *)

PROCEDURE app (h: PROCEDURE (i:INTEGER) : INTEGER;
               i:INTEGER) : INTEGER =
  BEGIN
    RETURN h(i)
  END app;

PROCEDURE f(x:INTEGER):INTEGER =

  PROCEDURE g(y:INTEGER):INTEGER =
    BEGIN
      RETURN x+y
    END g;

  BEGIN
    RETURN app(g,1)
  END f;

BEGIN
  EVAL f(7);
END Main.
