(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Min.m3                                                *)
(* Last Modified On Mon Sep 26 09:15:35 PDT 1994 By kalsow     *)
(*      Modified On Tue Mar 20 23:03:27 1990 By muller         *)

MODULE Min;

IMPORT CG, CallExpr, Expr, Type, Procedure, Max;
IMPORT IntegerExpr, EnumExpr, ReelExpr, Target, TInt;

VAR Z: CallExpr.MethodList;

PROCEDURE Check (ce: CallExpr.T;  <*UNUSED*> VAR cs: Expr.CheckState) =
  BEGIN
    Max.DoCheck ("MIN", ce);
  END Check;

PROCEDURE Compile (ce: CallExpr.T) =
  BEGIN
    Expr.Compile (ce.args[0]);
    Expr.Compile (ce.args[1]);
    CG.Min (Type.CGType (Type.Base (Expr.TypeOf (ce.args[0]))));
  END Compile;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  VAR e1, e2, e3: Expr.T;  i1, i2: Target.Int;  t: Type.T;
  BEGIN
    e1 := Expr.ConstValue (ce.args[0]);
    IF (e1 = NIL) THEN RETURN NIL END;
    e2 := Expr.ConstValue (ce.args[1]);
    IF (e2 = NIL) THEN RETURN NIL END;
    IF IntegerExpr.Split (e1, i1, t) AND IntegerExpr.Split (e2, i2, t) THEN
      IF TInt.LT (i1, i2)
        THEN RETURN e1;
        ELSE RETURN e2;
      END;
    ELSIF EnumExpr.Split (e1, i1, t) AND EnumExpr.Split (e2, i2, t) THEN
      IF TInt.LT (i1, i2)
        THEN RETURN e1;
        ELSE RETURN e2;
      END;
    ELSIF ReelExpr.Min (e1, e2, e3) THEN
      RETURN e3;
    ELSE
      RETURN NIL;
    END;
  END Fold;

PROCEDURE GetBounds (ce: CallExpr.T;  VAR min, max: Target.Int) =
  VAR min_a, max_a, min_b, max_b : Target.Int;
  BEGIN
    Expr.GetBounds (ce.args[0], min_a, max_a);
    Expr.GetBounds (ce.args[1], min_b, max_b);
    IF TInt.LT (min_a, min_b)
      THEN min := min_a;
      ELSE min := min_b;
    END;
    IF TInt.LT (max_a, max_b)
      THEN max := max_a;
      ELSE max := max_b;
    END;
  END GetBounds;

PROCEDURE Initialize () =
  BEGIN
    Z := CallExpr.NewMethodList (2, 2, TRUE, FALSE, TRUE, NIL,
                                 Max.TypeOf,
                                 CallExpr.NotAddressable,
                                 Check,
                                 CallExpr.PrepArgs,
                                 Compile,
                                 CallExpr.NoLValue,
                                 CallExpr.NoLValue,
                                 CallExpr.PrepNoBranch,
                                 CallExpr.NoBranch,
                                 Fold,
                                 GetBounds,
                                 CallExpr.IsNever, (* writable *)
                                 CallExpr.IsNever, (* designator *)
                                 CallExpr.NotWritable (* noteWriter *));
    Procedure.DefinePredefined ("MIN", Z, TRUE);
  END Initialize;

BEGIN
END Min.
