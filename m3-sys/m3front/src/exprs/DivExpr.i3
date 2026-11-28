(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: DivExpr.i3                                            *)
(* Last Modified On Thu Dec 15 13:46:12 PST 1994 By kalsow     *)

INTERFACE DivExpr;

IMPORT CG, Expr, Target;

PROCEDURE New (a, b: Expr.T): Expr.T;

PROCEDURE MayOverflow (a, b: Expr.T;  t: CG.Type): BOOLEAN;
(* TRUE if t is INTEGER's or LONGINT's stack type and a DIV b or a MOD b
   could be FIRST (t) DIV -1 or FIRST (t) MOD -1, judging by the bounds of
   a and b.  Hardware and C division trap or are undefined there. *)

PROCEDURE CompileWrapped (t: CG.IType;  sa: CG.Sign;  div: BOOLEAN);
(* s1.t := s1.t DIV s0.t (div) or s1.t MOD s0.t (NOT div);  pop.
   FIRST (t) DIV -1 wraps around to FIRST (t) and FIRST (t) MOD -1 is 0,
   with no trap.  sa is the sign of s1. *)

PROCEDURE SmallPowerOfTwo (READONLY x: Target.Int;  VAR log: INTEGER): BOOLEAN;
(* If 'x' is a small positive power of 2, sets 'log' to the base 2 logorithm
   of 'x' and returns 'TRUE', otherwise returns 'FALSE' *)

END DivExpr.
