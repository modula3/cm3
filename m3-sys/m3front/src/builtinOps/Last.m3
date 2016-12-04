(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Last.m3                                               *)
(* Last Modified On Mon Sep 26 09:15:29 PDT 1994 By kalsow     *)
(*      Modified On Fri Dec 21 01:34:18 1990 By muller         *)

MODULE Last;

IMPORT CG, CallExpr, Expr, Type, Procedure, First, ArrayType;
IMPORT TypeExpr, IntegerExpr, EnumExpr, Int, LInt, ArrayExpr;
IMPORT Reel, LReel, EReel, ReelExpr, Target, TInt;

VAR Z: CallExpr.MethodList;

PROCEDURE Check (ce: CallExpr.T;  <*UNUSED*> VAR cs: Expr.CheckState) =
  BEGIN
    First.DoCheck ("LAST", ce);
  END Check;

PROCEDURE Prep (ce: CallExpr.T) =
  VAR
    e := ce.args[0];
    t, index, element: Type.T;
  BEGIN
    IF NOT TypeExpr.Split (e, t) THEN t := Expr.TypeOf (e) END;
    Type.Compile (t);
    IF ArrayType.Split (t, index, element) THEN t := index END;
 
    IF (t = NIL) THEN (* open array *)
      Expr.Prep (e);
    END;
  END Prep;

PROCEDURE Compile (ce: CallExpr.T) =
  VAR
    e := ce.args[0];
    t, index, element: Type.T;
    min, max: Target.Int;
  BEGIN
    IF NOT TypeExpr.Split (e, t) THEN t := Expr.TypeOf (e) END;
    IF ArrayType.Split (t, index, element) THEN t := index END;
 
    IF (t = NIL) THEN (* open array *)
      Expr.Compile (e);
      CG.Open_size (0);
      CG.Load_integer (Target.Integer.cg_type, TInt.One);
      CG.Subtract (Target.Integer.cg_type);
    ELSIF Type.GetBounds (t, min, max) THEN (* ordinal type *)
      IF Type.IsSubtype (t, LInt.T)
        THEN CG.Load_integer (Target.Longint.cg_type, max);
        ELSE CG.Load_integer (Target.Integer.cg_type, max);
      END;
    ELSIF Type.IsEqual (t, Reel.T, NIL) THEN
      CG.Load_float (Target.Real.max);
    ELSIF Type.IsEqual (t, LReel.T, NIL) THEN
      CG.Load_float (Target.Longreal.max);
    ELSIF Type.IsEqual (t, EReel.T, NIL) THEN
      CG.Load_float (Target.Extended.max);
    ELSE
      <* ASSERT FALSE *>
    END;
  END Compile;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  VAR t, index, elem: Type.T;  e: Expr.T;  min, max: Target.Int;
  BEGIN
    e := ce.args[0];
    IF TypeExpr.Split (e, t) THEN RETURN LastOfType (t) END;
    t := Expr.TypeOf (e);
    IF NOT ArrayType.Split (t, index, elem) THEN RETURN NIL END;
    IF (index = NIL) THEN
      (* LAST (open array value) => try for constant open array *)
      e := Expr.ConstValue (e);
      IF (e = NIL) THEN RETURN NIL END;
      IF ArrayExpr.GetBounds (e, min, max)
        THEN RETURN IntegerExpr.New (Int.T, max);
        ELSE RETURN NIL;
      END;
    END;
    RETURN LastOfType (t);
  END Fold;

PROCEDURE LastOfType (t: Type.T): Expr.T =
  VAR min, max: Target.Int;   elem, t_base: Type.T;
  BEGIN
    IF ArrayType.Split (t, t, elem) AND (t = NIL) THEN
      RETURN NIL;
    END;
    t_base:= Type.Base (t);
    IF Type.GetBounds (t, min, max) THEN
      IF t_base = Int.T OR t_base = LInt.T
        THEN RETURN IntegerExpr.New (t_base, max);
        ELSE RETURN EnumExpr.New (t, max);
      END;
    ELSIF t_base = Reel.T THEN
      RETURN ReelExpr.New (Target.Real.max);
    ELSIF t_base = LReel.T THEN
      RETURN ReelExpr.New (Target.Longreal.max);
    ELSIF t_base = EReel.T THEN
      RETURN ReelExpr.New (Target.Extended.max);
    ELSE
      RETURN NIL;
    END;
  END LastOfType;

PROCEDURE Initialize () =
  BEGIN
    Z := CallExpr.NewMethodList (1, 1, TRUE, FALSE, FALSE, NIL,
                                 First.TypeOf,
                                 CallExpr.NotAddressable,
                                 Check,
                                 Prep,
                                 Compile,
                                 CallExpr.NoLValue,
                                 CallExpr.NoLValue,
                                 CallExpr.PrepNoBranch,
                                 CallExpr.NoBranch,
                                 Fold,
                                 CallExpr.NoBounds,
                                 CallExpr.IsNever, (* writable *)
                                 CallExpr.IsNever, (* designator *)
                                 CallExpr.NotWritable (* noteWriter *));
    Procedure.DefinePredefined ("LAST", Z, TRUE);
  END Initialize;

BEGIN
END Last.
