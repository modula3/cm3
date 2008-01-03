(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: DivExpr.i3                                            *)
(* Last Modified On Thu Dec 15 13:46:12 PST 1994 By kalsow     *)

INTERFACE DivExpr;

IMPORT Expr, Target;

PROCEDURE New (a, b: Expr.T): Expr.T;

PROCEDURE SmallPowerOfTwo (READONLY x: Target.Int;  VAR log: INTEGER): BOOLEAN;
(* If 'x' is a small positive power of 2, sets 'log' to the base 2 logorithm
   of 'x' and returns 'TRUE', otherwise returns 'FALSE' *)

END DivExpr.
