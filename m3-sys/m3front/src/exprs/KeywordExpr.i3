(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: KeywordExpr.i3                                        *)
(* Last Modified On Fri Jun 24 09:00:06 PDT 1994 By kalsow     *)
(*      Modified On Sat Dec  8 00:17:07 1990 By muller         *)

INTERFACE KeywordExpr;

IMPORT M3ID, Expr;

PROCEDURE New (key: M3ID.T;  arg: Expr.T): Expr.T;

PROCEDURE Is (e: Expr.T): BOOLEAN;
PROCEDURE Split (e: Expr.T;  VAR key: M3ID.T;  VAR value: Expr.T): BOOLEAN;

END KeywordExpr.
