(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Trunc.m3                                              *)
(* Last Modified On Mon Sep 26 09:15:50 PDT 1994 By kalsow     *)
(*      Modified On Tue Apr 10 10:57:52 1990 By muller         *)

MODULE Trunc;

IMPORT CG, CallExpr, Expr, Type, Procedure, Ceiling, Int, ReelExpr;

VAR Z: CallExpr.MethodList;

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  BEGIN
    Ceiling.DoCheck ("TRUNC", ce, cs);
  END Check;

PROCEDURE Compile (ce: CallExpr.T) =
  VAR e := ce.args[0];
  BEGIN
    Expr.Compile (e);
    CG.Cvt_int (Type.CGType (Expr.TypeOf (e)), CG.Cvt.Trunc);
  END Compile;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  VAR e, x: Expr.T;
  BEGIN
    e := Expr.ConstValue (ce.args[0]);
    IF    (e = NIL)             THEN RETURN NIL
    ELSIF ReelExpr.Trunc (e, x) THEN RETURN x;
    ELSE (* bogus *)                 RETURN NIL;
    END;
  END Fold;

PROCEDURE Initialize () =
  BEGIN
    Z := CallExpr.NewMethodList (1, 1, TRUE, FALSE, TRUE, Int.T,
                                 NIL,
                                 CallExpr.NotAddressable,
                                 Check,
                                 CallExpr.PrepArgs,
                                 Compile,
                                 CallExpr.NoLValue,
                                 CallExpr.NoLValue,
                                 CallExpr.NotBoolean,
                                 CallExpr.NotBoolean,
                                 Fold,
                                 CallExpr.NoBounds,
                                 CallExpr.IsNever, (* writable *)
                                 CallExpr.IsNever, (* designator *)
                                 CallExpr.NotWritable (* noteWriter *));
    Procedure.Define ("TRUNC", Z, TRUE);
  END Initialize;

BEGIN
END Trunc.
