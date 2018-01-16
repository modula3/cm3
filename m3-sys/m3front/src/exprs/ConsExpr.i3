(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ConsExpr.i3                                           *)
(* Last Modified On Tue Jun 20 15:50:06 PDT 1995 By kalsow     *)
(*      Modified On Thu Jun 15 14:03:43 PDT 1995 By ericv      *)

INTERFACE ConsExpr;
(* A value constructor. Has an array, record, or set constructor
   as a child. *) 

IMPORT Expr;

PROCEDURE New (typeExpr: Expr.T;  args: Expr.List;  dots: BOOLEAN): Expr.T;

PROCEDURE Is (e: Expr.T): BOOLEAN;

PROCEDURE Base (e: Expr.T): Expr.T;

END ConsExpr.
