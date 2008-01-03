(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: open array parameters *)

MODULE Main;

PROCEDURE P (x: ARRAY OF ARRAY OF INTEGER) RAISES ANY =
  PROCEDURE Z () RAISES ANY =
    BEGIN EVAL (x[3]) END Z;
  BEGIN
    EVAL Z;
  END P;

PROCEDURE Q (VAR x: ARRAY OF ARRAY OF INTEGER) RAISES ANY =
  BEGIN
    EVAL x;
  END Q;

PROCEDURE R (READONLY x: ARRAY OF ARRAY OF INTEGER) RAISES ANY =
  BEGIN
    EVAL x;
  END R;

PROCEDURE foo (a: ARRAY OF ARRAY [0..4] OF INTEGER) RAISES ANY =
  BEGIN
    P (a);
    Q (a);
    R (a);
  END foo;

BEGIN
  EVAL foo;
END Main.
