(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: user & language specified variable initialization *)

MODULE Main;

FROM Test IMPORT msg, msgI, check, checkI, done;

TYPE
  t1 = [0..5];
  t2 = [3..8];

VAR
  a: t1;
  b: t2;
  f, g : INTEGER := j;
  i, j : t1 := 4;
  k, m : t2 := i;
  n, p : INTEGER := k + j;

PROCEDURE Foo () =
  VAR
  a: t1;
  b: t2;
  f, g : INTEGER := j;
  i, j : t1 := 4;
  k, m : t2 := i;
  n, p : INTEGER := k + j;

BEGIN
  msgI (a); msg ("0..5"); check (0 <= a AND a <= 5);
  msgI (b); msg ("3..8"); check (3 <= b AND b <= 8);
  msgI (f); msg ("integer");
  msgI (g); checkI (g, f);
  checkI (i, 4);
  checkI (j, 4);
  checkI (k, 4);
  checkI (m, 4);
  checkI (n, 8);
  checkI (p, 8);
END Foo;

BEGIN
  msgI (a); msg ("0..5"); check (0 <= a AND a <= 5);
  msgI (b); msg ("3..8"); check (3 <= b AND b <= 8);
  msgI (f); msg ("integer");
  msgI (g); checkI (g, f);
  checkI (i, 4);
  checkI (j, 4);
  checkI (k, 4);
  checkI (m, 4);
  checkI (n, 8);
  checkI (p, 8);
  
  Foo ();
  done ();
END Main.
