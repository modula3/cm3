(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: type minimization of REF/RECORD *)

MODULE Main;

TYPE
  r1 = REF r1;
  r2 = REF REF r2;
  r3 = REF REF REF r3;
  t  = INTEGER;
  x1 = RECORD i: INTEGER;  r: r1 END;
  x2 = RECORD i: t;        r: r3 END;
  x3 = RECORD i: INTEGER;  r: REF r2 END;

VAR
  a : r1;
  b : r2;
  c : r3;
  d : x1;
  e : x2;
  f : x3;


BEGIN
  a := b;
  b := c;
  c := a;
  d := e;
  e := f;
  f := d;
  d.r := a;
  e.r := a;
  f.r := a;
END Main.
