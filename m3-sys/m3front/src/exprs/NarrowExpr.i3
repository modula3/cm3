(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved.  *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE NarrowExpr;

IMPORT Expr, Type;

PROCEDURE New (a: Expr.T;  t: Type.T): Expr.T;

END NarrowExpr.
