(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: SubscriptExpr.i3                                      *)
(* Last Modified On Mon Jul 31 13:22:38 1989 By kalsow         *)

INTERFACE SubscriptExpr;

IMPORT Expr, MSIR;

PROCEDURE New (a, b: Expr.T): Expr.T;

PROCEDURE SubByteStoreElemMSIR (e: Expr.T;  rhs: MSIR.Value): BOOLEAN;
(* If e is a SubscriptExpr on a packed-element (sub-byte) fixed array,
   emit a read-modify-write bit insertion and return TRUE.
   Otherwise return FALSE with no side effects. *)

END SubscriptExpr.
