(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Thu Jun 17 14:31:03 PDT 1993 by kalsow    *)
(*      modified on Fri Apr 27 04:13:12 1990 by muller        *)

MODULE Main;

IMPORT Thread;
FROM Test IMPORT msg, done;

CONST Second = 0.001d0;

PROCEDURE Task1 (<*UNUSED*> self: Thread.Closure): REFANY RAISES {} =
BEGIN
  FOR i := 1 TO 250000 DO
    msg ("1");
    Thread.Pause (0.2d0 * Second);
  END;
  RETURN NIL;    
END Task1;

PROCEDURE Task2 (<*UNUSED*> self: Thread.Closure): REFANY RAISES {} =
BEGIN
  FOR i := 1 TO 100000 DO
    msg ("2");
    Thread.Pause (0.5d0 * Second);
  END;
  RETURN NIL;
END Task2;

VAR
  t1, t2: Thread.T; 


BEGIN
  (* this test takes a very long time and doesn't work; 
     do not run it.
  t1 := Thread.Fork (NEW (Thread.Closure, apply := Task1));
  t2 := Thread.Fork (NEW (Thread.Closure, apply := Task2));
  EVAL Thread.Join (t1);
  EVAL Thread.Join (t2);
  *)
  EVAL t1;
  EVAL t2;
  EVAL Task1;
  EVAL Task2;

  done ();
END Main.
