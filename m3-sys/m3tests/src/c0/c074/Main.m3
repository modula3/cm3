(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: INC and DEC - range checking *)

MODULE Main;

VAR 
  a: INTEGER;
  b: [1..100];
  c: CARDINAL;
  w: INTEGER;
  x: [-10..10];
  y: [-100..0];
  z: [0..100];

BEGIN

INC (a, w);
INC (a, x);
INC (a, y);
INC (a, z);

DEC (a, w);
DEC (a, x);
DEC (a, y);
DEC (a, z);

INC (b, w);
INC (b, x);
INC (b, y);
INC (b, z);

DEC (b, w);
DEC (b, x);
DEC (b, y);
DEC (b, z);

INC (c, w);
INC (c, x);
INC (c, y);
INC (c, z);

DEC (c, w);
DEC (c, x);
DEC (c, y);
DEC (c, z);


END Main.
