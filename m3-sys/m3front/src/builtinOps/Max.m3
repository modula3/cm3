(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Max.m3                                                *)
(* Last Modified On Fri Jun 24 09:38:26 PDT 1994 By kalsow     *)
(*      Modified On Fri Dec 21 01:02:24 1990 By muller         *)

MODULE Max;

IMPORT CG, CallExpr, Expr, ExprRep, Type, Procedure, Error, Target, TInt;
IMPORT Int, LInt, Reel, LReel, EReel, IntegerExpr, EnumExpr, ReelExpr;

VAR Z: CallExpr.MethodList;

PROCEDURE TypeOf (ce: CallExpr.T): Type.T =
  BEGIN
    RETURN Type.Base (Expr.TypeOf (ce.args[0]));
  END TypeOf;

PROCEDURE Check (ce: CallExpr.T;  <*UNUSED*> VAR cs: Expr.CheckState) =
  BEGIN
    DoCheck ("MAX", ce);
  END Check;

PROCEDURE DoCheck (name: TEXT;  ce: CallExpr.T) =
  VAR ta, tb: Type.T;
  BEGIN
    ta := Type.Base (Expr.TypeOf (ce.args[0]));
    tb := Type.Base (Expr.TypeOf (ce.args[1]));

    IF (NOT Type.IsEqual (ta, tb, NIL)) THEN
      Error.Txt (name, "incompatible argument types");
    ELSIF (ta = Int.T) OR (ta = LInt.T) OR (Type.IsOrdinal (ta)) THEN
      (* ok *)
    ELSIF (ta = Reel.T) OR (ta = LReel.T) OR (ta = EReel.T) THEN
      (* ok *)
    ELSE
      Error.Txt (name, "wrong argument types");
      ta := Int.T;
    END;
    ce.type := ta;
  END DoCheck;

PROCEDURE Compile (ce: CallExpr.T) =
  BEGIN
    Expr.Compile (ce.args[0]);
    Expr.Compile (ce.args[1]);
    CG.Max (Type.CGType (Type.Base (Expr.TypeOf (ce.args[0]))));
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
        THEN RETURN e2;
        ELSE RETURN e1;
      END;
    ELSIF EnumExpr.Split (e1, i1, t) AND EnumExpr.Split (e2, i2, t) THEN
      IF TInt.LT (i1, i2)
        THEN RETURN e2;
        ELSE RETURN e1;
      END;
    ELSIF ReelExpr.Max (e1, e2, e3) THEN
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
      THEN min := min_b;
      ELSE min := min_a;
    END;
    IF TInt.LT (max_a, max_b)
      THEN max := max_b;
      ELSE max := max_a;
    END;
  END GetBounds;

PROCEDURE Initialize () =
  BEGIN
    Z := CallExpr.NewMethodList (2, 2, TRUE, FALSE, TRUE, NIL,
                                 TypeOf,
                                 TypeOf,
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
    Procedure.DefinePredefined ("MAX", Z, TRUE);
  END Initialize;

BEGIN
END Max.
