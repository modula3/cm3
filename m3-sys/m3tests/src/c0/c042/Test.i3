(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: SET declarations and constants *)

INTERFACE Test;

TYPE
  E = {a, b, c};
  S = SET OF E;

  F = [0..35];
  T = SET OF F;

CONST
  C = S {E.b, E.a};
  D = T {4, 8, 34};
  G = T {};

VAR
  c := S {E.b, E.a};
  d := T {4, 8, 34};
  g := T {};

END Test.    
