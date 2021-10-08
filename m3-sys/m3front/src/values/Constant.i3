(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Constant.i3                                           *)
(* Last Modified On Tue Dec 20 14:57:43 PST 1994 By kalsow     *)
(*      Modified On Thu Dec 13 01:35:56 1990 By muller         *)

INTERFACE Constant;

IMPORT Expr, Decl, Value;

TYPE
  T <: Value.T;

PROCEDURE ParseDecl (READONLY att: Decl.Attributes);

PROCEDURE Declare (name: TEXT;  value: Expr.T;  reserved: BOOLEAN): T;
(* Declare in the current scope. *)

PROCEDURE DeclareGlobal (name: TEXT;  valExpr: Expr.T;  reserved := FALSE): T;
(* Declare in the global scope. *)

END Constant.
