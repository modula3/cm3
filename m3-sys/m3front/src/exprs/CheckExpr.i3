(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: CheckExpr.i3                                          *)
(* Last Modified On Sun Aug 23 15:26:38 PDT 1992 By kalsow     *)

INTERFACE CheckExpr;

IMPORT Expr, Target, CG;

PROCEDURE New      (a: Expr.T;  READONLY min, max: Target.Int;
                    err: CG.RuntimeError): Expr.T;
PROCEDURE NewLower (a: Expr.T;  READONLY min: Target.Int;
                    err: CG.RuntimeError): Expr.T;
PROCEDURE NewUpper (a: Expr.T;  READONLY max: Target.Int;
                    err: CG.RuntimeError): Expr.T;

PROCEDURE EmitChecks (e: Expr.T;  READONLY min, max: Target.Int;
                      err: CG.RuntimeError);
(* compiles 'e' and ensures that it's contained in [min..max] *)

END CheckExpr.
