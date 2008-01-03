(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: Hello world *)

MODULE Main;
IMPORT Wr, Stdio;

PROCEDURE P () =
  BEGIN
    Wr.PutText (Stdio.stdout, "Hello world!\n");
  END P;

PROCEDURE Q () RAISES { Wr.Failure } =
  BEGIN
    Wr.PutText (Stdio.stdout, "Hello world!\n");
  END Q;

PROCEDURE R () RAISES ANY =
  BEGIN
    Wr.PutText (Stdio.stdout, "Hello world!\n");
  END R;

PROCEDURE S () =
  <* FATAL Wr.Failure *>
  BEGIN
    Wr.PutText (Stdio.stdout, "Hello world!\n");
  END S;

PROCEDURE T () =
  <* FATAL ANY *>
  BEGIN
    Wr.PutText (Stdio.stdout, "Hello world!\n");
  END T;

BEGIN
  EVAL P; EVAL Q; EVAL R; EVAL S; EVAL T;

  TRY
    Wr.PutText (Stdio.stdout, "Hello world!\n");
  EXCEPT Wr.Failure =>
  END;

  Wr.PutText (Stdio.stdout, "Hello world!\n");
END Main.
