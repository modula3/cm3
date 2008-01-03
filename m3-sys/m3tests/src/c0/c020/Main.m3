(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: assignment of INTEGER subranges *)

MODULE Main;

TYPE
  t1 = [0..5];
  t2 = [3..4];
  t3 = [3..8];
  t4 = [-3..9];

VAR
  v1: t1;
  v2: t2;
  v3: t3;
  v4: t4;
  i: INTEGER;

BEGIN
  i := v1;
  i := v2;
  i := v3;
  i := v4;

  v1 := 0;
  v1 := i;
  v1 := v1;
  v1 := v2;
  v1 := v3;
  v1 := v4;

  v2 := 3;
  v2 := i;
  v2 := v1;
  v2 := v2;
  v2 := v3;
  v2 := v4;

  v3 := 7;
  v3 := i;
  v3 := v1;
  v3 := v2;
  v3 := v3;
  v3 := v4;

  v4 := 0;
  v4 := i;
  v4 := v1;
  v4 := v2;
  v4 := v3;
  v4 := v4;

END Main.
