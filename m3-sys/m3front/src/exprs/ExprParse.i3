(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ExprParse.i3                                          *)
(* Last Modified On Fri Jun 24 13:36:37 PDT 1994 By kalsow     *)

INTERFACE ExprParse;

IMPORT Expr;

PROCEDURE E0 (types: BOOLEAN): Expr.T;
(* parse an expression, types = TRUE => allow type expressions *)

END ExprParse.
