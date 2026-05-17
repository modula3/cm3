(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: QualifyExpr.i3                                        *)
(* Last Modified On Fri Jun 24 08:57:39 PDT 1994 By kalsow     *)
(*      Modified On Sat Aug 18 01:13:38 1990 By muller         *)

INTERFACE QualifyExpr;

IMPORT M3ID, Expr, Value, Type, MSIR;

PROCEDURE New (a: Expr.T;  id: M3ID.T): Expr.T;

PROCEDURE Split (e: Expr.T; VAR v: Value.T): BOOLEAN;

PROCEDURE SplitQID (e: Expr.T; VAR module, item: M3ID.T): BOOLEAN;

PROCEDURE PassObject (e: Expr.T): BOOLEAN;

PROCEDURE MethodType (e: Expr.T): Type.T;

(* If e is a QualifyExpr for an object method call (class=objMethod),
   return the left-hand object expression.  Otherwise return NIL.
   Used by the MSIR path to compile virtual dispatch. *)
PROCEDURE LhsExpr (e: Expr.T): Expr.T;

(* If e is a QualifyExpr whose field is sub-byte, emit a read-modify-write
   bit-insertion of rhs and return TRUE.  Otherwise return FALSE immediately. *)
PROCEDURE SubByteStoreMSIR (e: Expr.T;  rhs: MSIR.Value): BOOLEAN;

END QualifyExpr.
