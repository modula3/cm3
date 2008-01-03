(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

TYPE Int = BITS 3 FOR [0..7];

VAR a :  RECORD
           a : CHAR := '3';
           b : BOOLEAN := TRUE;
           d : INTEGER := 34;
           e : REAL := 3.4;
           f : LONGREAL := 6.5d+3;
           g : EXTENDED := 8.9x-3;
           h : Int := 1;
           i : Int := 2;
           j : Int := 3;
           k : Int := 4;
           l : Int := 5;
           m : Int := 6;
           n : Int := 7;
           o : INTEGER := 99;
           h1 : Int := 1;
           i1 : Int := 2;
           j1 : Int := 3;
           k1 : Int := 4;
           l1 : Int := 5;
           m1 : Int := 6;
           n1 : Int := 7;
         END;

BEGIN
  EVAL a;
END Main.
