(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* File created by Gadow, IBM Rochester. *)

MODULE Main;
    
CONST
    (* I could not find a section in the report which disallowed this *)
 
   c = ARRAY OF INTEGER {10, 3};
   b = ARRAY OF ARRAY OF INTEGER {c, c, c, c, c, c}; 
   e = ARRAY OF INTEGER {c[1], b[3][0]};
   d = ARRAY OF ARRAY OF INTEGER {c, e};

VAR
  x : INTEGER;
  y : ARRAY [10..11] OF INTEGER;
  z : ARRAY [20..25] OF ARRAY [30..31] OF INTEGER;

BEGIN
  
  x := ARRAY OF INTEGER {10, 3} [2];
  x := ARRAY OF ARRAY OF INTEGER {c, c, c, c, c, c} [5][1];
  x := ARRAY OF ARRAY OF INTEGER {
	ARRAY OF INTEGER {10, 3}, 
        ARRAY OF INTEGER {10, 3}, 
        ARRAY OF INTEGER {10, 3}, 
        ARRAY OF INTEGER {10, 3}, 
        ARRAY OF INTEGER {10, 3}, 
        ARRAY OF INTEGER {10, 3}} [5][1];
  x := ARRAY OF ARRAY OF INTEGER {c, e} [2][1];
  x := ARRAY OF ARRAY OF INTEGER {
	ARRAY OF INTEGER {10, 3}, 
	ARRAY OF INTEGER {c[1], b[3][0]}} [2][1];

  EVAL d;
  EVAL y;
  EVAL z;

END Main.



