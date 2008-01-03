(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

(* T1 and T2 should be the same type *)

CONST r = 1.0;

TYPE T1 = RECORD a := r END;
TYPE T2 = RECORD a := 1.0 END;

PROCEDURE P (VAR t2: T2) =
  BEGIN
    EVAL t2.a;
  END P;

VAR t1: T1;
BEGIN
  P (t1);
END Main.
