(* Copyright 1997, Critical Mass, Inc.  All rights reserved.  *)

INTERFACE NarrowExpr;

IMPORT Expr, Type;

PROCEDURE New (a: Expr.T;  t: Type.T): Expr.T;

END NarrowExpr.
