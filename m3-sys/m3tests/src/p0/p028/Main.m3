(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: fixed ARRAY types, variables, assignments and subscripting *)

MODULE Main;

FROM Test IMPORT checkI, checkC, done;

TYPE
  R1 = [-20..20];
  A1 = ARRAY [0..10] OF R1;
  A2 = ARRAY [0..6] OF CHAR;
  A3 = ARRAY CHAR OF A2;
  A4 = ARRAY BOOLEAN OF A3;
  A5 = ARRAY [1..7] OF CHAR;

VAR
  a, b: A1;
  c, d: A2;
  e, f: A3;
  g, h: A4;
  j, k: A5;
  i: R1;

BEGIN

  a := b;
  c := d;
  e := f;
  g := h;
  j := k;
  c := j;
  k := d;

  i := 7;
  a[4] := 5;
  a[i] := i+4;
  i := -3;
  a[2*i+14] := 6;
  checkI (a[4], 5);
  checkI (a[7], 11);
  checkI (a[8], 6);

  i := 1;
  c[2] := 'A';
  c[i] := c[2];
  c[i*i] := 'B';
  checkC (c[2], 'A');
  checkC (c[1], 'B');

  i := 1;
  e['A'] := c;
  e[c[1]] := d;
  e[c[i]][i] := 'M';
  checkC (e['B'][1], 'M');
  e[c[i],i] := 'N';
  checkC (e['B'][1], 'N');
  checkC (e['A'][1], 'B');
 
  g[TRUE] := e;
  g[NOT TRUE] := g[TRUE];
  checkC (g[TRUE]['B'][1], 'N');
  checkC (g[FALSE]['B'][1], 'N');

  i := 4;
  j[1] := 'A';
  j[3] := 'B';
  j[i] := 'C';
  j[a[i]] := 'D';
  checkC (j[5], 'D');

  done ();

END Main.
