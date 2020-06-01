(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: SetExpr.i3                                            *)
(* Last Modified On Thu Sep  9 13:59:20 PDT 1993 By kalsow     *)
(*      Modified On Fri Jul  6 23:46:44 1990 By muller         *)

INTERFACE SetExpr;
(* For set constructors. *) 

IMPORT Type, Expr, CG;

PROCEDURE New (type: Type.T;  args: Expr.List): Expr.T;

PROCEDURE Is (e: Expr.T): BOOLEAN;
(* Purely syntactic. Will not look through a ConsExpr. *)

PROCEDURE Compare       (a, b: Expr.T;  VAR s: INTEGER): BOOLEAN;
PROCEDURE Union         (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN;
PROCEDURE Intersection  (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN;
PROCEDURE Difference    (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN;
PROCEDURE SymDifference (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN;
PROCEDURE Include       (set, elt: Expr.T;  VAR c: Expr.T): BOOLEAN;
PROCEDURE Exclude       (set, elt: Expr.T;  VAR c: Expr.T): BOOLEAN;
PROCEDURE Member        (set, elt: Expr.T;  VAR c: Expr.T): BOOLEAN;
PROCEDURE CheckStaticRTErrEval
  (expr: Expr.T; VAR(*OUT*) Code: CG.RuntimeError; VAR(*OUT*) Msg: TEXT);
(* Set Code and Msg if they are not set and expr is known to produce a
   statically unconditional runtime error when evaluated. *)

PROCEDURE Init ();

END SetExpr.
