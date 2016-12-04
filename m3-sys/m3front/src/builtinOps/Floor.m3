(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Floor.m3                                              *)
(* Last Modified On Mon Sep 26 09:15:22 PDT 1994 By kalsow     *)
(*      Modified On Tue Mar 20 04:11:50 1990 By muller         *)

MODULE Floor;

IMPORT CG, CallExpr, Expr, Type, Procedure, Ceiling, Int, ReelExpr, TypeExpr;

VAR Z: CallExpr.MethodList;

PROCEDURE TypeOf (ce: CallExpr.T): Type.T =
  VAR t: Type.T;
  BEGIN
    t := Int.T;
    IF (NUMBER (ce.args^) > 1) THEN
      EVAL TypeExpr.Split (ce.args[1], t);
      t := Type.Base (t);
    END;
    RETURN t;
  END TypeOf;

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  BEGIN
    Ceiling.DoCheck ("FLOOR", ce, cs);
  END Check;

PROCEDURE Prep (ce: CallExpr.T) =
  BEGIN
    Expr.Prep (ce.args[0]);
  END Prep;

PROCEDURE Compile (ce: CallExpr.T) =
  VAR e := ce.args[0];  t := TypeOf (ce);
  BEGIN
    Expr.Compile (e);
    CG.Cvt_int (Type.CGType (Expr.TypeOf (e)), Type.CGType (t), CG.Cvt.Floor);
  END Compile;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  VAR e, x: Expr.T;  t := TypeOf (ce);
  BEGIN
    e := Expr.ConstValue (ce.args[0]);
    IF    (e = NIL)                THEN RETURN NIL
    ELSIF ReelExpr.Floor (e, t, x) THEN RETURN x;
    ELSE  (* bogus *)                   RETURN NIL;
    END;
  END Fold;

PROCEDURE Initialize () =
  BEGIN
    Z := CallExpr.NewMethodList (1, 2, TRUE, FALSE, TRUE, NIL,
                                 TypeOf,
                                 CallExpr.NotAddressable,
                                 Check,
                                 Prep,
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
    Procedure.DefinePredefined ("FLOOR", Z, TRUE);
  END Initialize;

BEGIN
END Floor.
