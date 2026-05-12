(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Round.m3                                              *)
(* Last Modified On Mon Sep 26 09:15:45 PDT 1994 By kalsow     *)
(*      Modified On Tue Mar 20 03:21:34 1990 By muller         *)

MODULE Round;

IMPORT CG, CallExpr, Expr, Type, Procedure, Ceiling, Int, ReelExpr, TypeExpr;
IMPORT MSIR, MSIRBuilder, MSIRType;

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
    Ceiling.DoCheck ("ROUND", ce, cs);
  END Check;

PROCEDURE Prep (ce: CallExpr.T) =
  BEGIN
    Expr.Prep (ce.args[0]);
  END Prep;

PROCEDURE Compile (ce: CallExpr.T) =
  VAR e := ce.args[0];  t := TypeOf (ce);
  BEGIN
    Expr.Compile (e);
    CG.Cvt_int (Type.CGType (Expr.TypeOf (e)), Type.CGType (t), CG.Cvt.Round);
  END Compile;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  VAR e, x: Expr.T;  t := TypeOf (ce);
  BEGIN
    e := Expr.ConstValue (ce.args[0]);
    IF    (e = NIL)                THEN RETURN NIL
    ELSIF ReelExpr.Round (e, t, x) THEN RETURN x;
    ELSE  (* bogus *)                   RETURN NIL;
    END;
  END Fold;

PROCEDURE CompileMSIR (ce: CallExpr.T): MSIR.Value =
  VAR
    e    := ce.args[0];
    dstT := MSIRType.Translate (TypeOf (ce));
    blk  := MSIRBuilder.CurrentBlock ();
    src  : MSIR.Value;
  BEGIN
    src := Expr.CompileMSIR (e);  IF src = NIL THEN RETURN NIL END;
    IF dstT = NIL THEN
      MSIRBuilder.Abandon ("ROUND: unsupported dest type");  RETURN NIL
    END;
    RETURN MSIR.BuildFPToSI (blk, "", MSIR.BuildFPRound (blk, "", src), dstT);
  END CompileMSIR;

PROCEDURE Initialize () =
  BEGIN
    Z := CallExpr.NewMethodList (1, 2, TRUE, FALSE, TRUE, NIL,
                                 TypeOf,
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
    CallExpr.SetMethodMSIR (Z, CompileMSIR);
    Procedure.DefinePredefined ("ROUND", Z, TRUE);
  END Initialize;

BEGIN
END Round.
