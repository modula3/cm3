(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: fixed ARRAY types, variables, assignments and subscripting *)

MODULE Main;

TYPE
  R1 = [2..20];
  A1 = ARRAY [0..10] OF R1;
  A2 = ARRAY [0..3] OF CHAR;
  A3 = ARRAY CHAR OF A2;
  A4 = ARRAY BOOLEAN OF A3;
  A5 = ARRAY [1..4] OF CHAR;

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

  a[4] := 5;
  a[i] := i+4;
  a[2*i+14] := 6;

  c[2] := 'A';
  c[i] := c[2];
  c[i*i] := 'B';

  e['A'] := c;
  e[c[0]] := d;
  e[c[i]][i] := 'M';
  e[c[i],i] := 'N';
 
  g[TRUE] := e;
  g[NOT FALSE] := g[FALSE];

  j[0] := 'A';
  j[3] := 'B';
  j[i] := 'C';
  j[a[i]] := 'D';

END Main.
