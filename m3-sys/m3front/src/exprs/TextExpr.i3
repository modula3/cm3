(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: TextExpr.i3                                           *)
(* Last Modified On Fri Jun 24 08:48:56 PDT 1994 By kalsow     *)

INTERFACE TextExpr;

IMPORT Expr, M3String, M3WString;

PROCEDURE New8  (value: M3String.T): Expr.T;
PROCEDURE New32 (value: M3WString.T): Expr.T;

PROCEDURE Cat (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN;

PROCEDURE Split8  (e: Expr.T;  VAR value: M3String.T): BOOLEAN;
PROCEDURE Split32 (e: Expr.T;  VAR value: M3WString.T): BOOLEAN;

PROCEDURE Reset ();

END TextExpr.
