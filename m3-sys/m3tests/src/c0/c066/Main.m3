(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: Constructor expressions *)

MODULE Main;

TYPE 
  T = ARRAY [1..7] OF INTEGER;
  LS = SET OF [0..244];
  SS = SET OF [0..20];

VAR
  i, j: INTEGER;
  s: LS;
  t: SS;

BEGIN

 i := ARRAY [1..10] OF INTEGER {1, 2, 3, 4, 5, 6, 7, 8, 9, 10} [7];
 i := ARRAY [1..10] OF INTEGER {1, 2, 3, 4, ..} [7];

 i := ARRAY [1..10] OF T { T {1,..}, T{2,..}, T{3,..}, ..} [3] [4]; 

 s := LS { 23, 45, 37, 40..55 };
 s := LS { 23, 45, 37, 40..55, i, j-3, i..j };
 t := SS { 1, 3, 5, 7 };
 t := SS { 1, 3, 5, 7, i, j-3, i..j };

END Main.
