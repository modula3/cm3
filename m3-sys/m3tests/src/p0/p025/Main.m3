(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: assignment of INTEGER subranges *)

MODULE Main;

FROM Test IMPORT checkI, done;

TYPE
  t1 = [0..5];
  t2 = [3..4];
  t3 = [3..8];
  t4 = [-3..9];

VAR
  v1: t1;  v2: t2;  v3: t3;  v4: t4; i: INTEGER;

BEGIN
  i := 3;	  checkI (i, 3);
  v1 := i;	  checkI (v1, 3);
  v2 := i;	  checkI (v2, 3);
  v2 := v1;	  checkI (v2, 3);
  v3 := i;	  checkI (v3, 3);
  v3 := v1;	  checkI (v3, 3);
  v3 := v2;	  checkI (v3, 3);
  v4 := i;	  checkI (v4, 3);
  v4 := v1;	  checkI (v4, 3);
  v4 := v2;	  checkI (v4, 3);
  v4 := v3;	  checkI (v4, 3);
  v4 := -3;	  checkI (v4, -3);

  done ();

END Main.

