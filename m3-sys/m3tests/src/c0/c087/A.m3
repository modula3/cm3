(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE A EXPORTS Main, A;

IMPORT B;

VAR
  b: INTEGER;
  c: ARRAY [0..3] OF INTEGER;

BEGIN

  b := B.T;
  c := B.U;

END A.
