(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE A EXPORTS Main, A;

IMPORT B;

VAR
  b: B.T;
  r: REFANY;
  same: BOOLEAN;

BEGIN

  TYPECASE r OF
  | NULL =>
  | B.T (r) => same := r^ = B.t;
  ELSE
  END;

  EVAL b;

END A.
