(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* File created by Gadow, IBM Rochester. *)

MODULE Main;

FROM Test IMPORT checkI, done;
    
CONST
    (* I could not find a section in the report which disallowed this *)
 
   c = ARRAY OF INTEGER {10, 3};
   b = ARRAY OF ARRAY OF INTEGER {c, c, c, c, c, c}; 
   e = ARRAY OF INTEGER {c[1], b[3][0]};
   d = ARRAY OF ARRAY OF INTEGER {c, e};

VAR
  x1, x2, x3 : INTEGER;
  y,u : ARRAY [10..11] OF INTEGER;
  z   : ARRAY [20..25] OF ARRAY [30..31] OF INTEGER;
  v   : ARRAY [40..41] OF ARRAY [50..51] OF INTEGER;

BEGIN
  x1 := c[1];
  x2 := b[5][0];   
  x3 := d[1][1];
  y := c;
  z := b;
  u := e;
  v := d;

  checkI (x1, 3);
  checkI (x2, 10);
  checkI (x3, 10);

  checkI (y[10], 10);
  checkI (y[11], 3);

  FOR i := 20 TO 25 DO
    checkI (z[i][30], 10);
    checkI (z[i][31], 3); END;

  checkI (u[10], 3);
  checkI (u[11], 10);

  checkI (v[40][50], 10);
  checkI (v[40][51],  3);
  checkI (v[41][50],  3);
  checkI (v[41][51], 10);

  done ();
END Main.


