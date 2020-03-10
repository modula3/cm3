(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: First.m3                                              *)
(* Last Modified On Fri Jun 24 12:35:52 PDT 1994 By kalsow     *)
(*      Modified On Fri Dec 21 01:35:21 1990 By muller         *)

MODULE First;

IMPORT CG, CallExpr, Expr, ExprRep, Type, Procedure, Error, ArrayType;
IMPORT Int, LInt, EnumType, IntegerExpr, EnumExpr, TypeExpr;
IMPORT Reel, LReel, EReel, ReelExpr, Target, TInt;

VAR Z: CallExpr.MethodList;

PROCEDURE TypeOf (ce: CallExpr.T): Type.T =
  VAR e: Expr.T;  t, index, element: Type.T;
  BEGIN
    e := ce.args[0];
    t := Expr.TypeOf (e);
    index := NIL;
    IF ArrayType.Split (t, index, element) THEN
      IF (index = NIL) THEN index := Int.T END;
    ELSIF TypeExpr.Split (e, t) THEN
      IF NOT ArrayType.Split (t, index, element) THEN index := t END;
    END;
    IF (index = NIL) THEN index := Int.T; END;
    RETURN Type.Base (index);
  END TypeOf;

PROCEDURE Check (ce: CallExpr.T;  <*UNUSED*> VAR cs: Expr.CheckState) =
  BEGIN
    DoCheck ("FIRST", ce);
  END Check;

PROCEDURE DoCheck (name: TEXT;  ce: CallExpr.T) =
  VAR e: Expr.T; t, index, element: Type.T;
  BEGIN
    e := ce.args[0];
    t := Expr.TypeOf (e);
    IF ArrayType.Split (t, index, element) THEN
      IF (index = NIL) THEN index := Int.T END;
    ELSIF TypeExpr.Split (e, t) THEN
      IF ArrayType.Split (t, index, element) THEN
        IF (index = NIL) THEN
          Error.Txt (name, "argument cannot be an open array type");
          index := Int.T;
        END;
      ELSE
        index := t;
      END;
    ELSE
      Error.Txt (name, "argument must be a type or array");
      index := Int.T;
    END;
    IF EnumType.Is (index) THEN
      IF NOT TInt.LT (TInt.Zero, Type.Number (index)) THEN
        Error.Txt (name, "empty enumeration type");
      END;
    ELSIF Type.IsOrdinal (index)             THEN (* ordinal type => OK*)
    ELSIF Type.IsEqual (index, Reel.T, NIL)  THEN (* OK *)
    ELSIF Type.IsEqual (index, LReel.T, NIL) THEN (* OK *)
    ELSIF Type.IsEqual (index, EReel.T, NIL) THEN (* OK *)
    ELSE
      Error.Txt (name, "argument must be an ordinal type, floating type, array type or array");
    END;
    ce.type := Type.Base (index);
  END DoCheck;

PROCEDURE Prep (<*UNUSED*> ce: CallExpr.T) =
  BEGIN
    (* skip *)
  END Prep;

PROCEDURE Compile (ce: CallExpr.T) =
  VAR
    e := ce.args[0];
    min, max: Target.Int;
    t, index, element: Type.T;
  BEGIN
    IF NOT TypeExpr.Split (e, t) THEN t := Expr.TypeOf (e) END;
    Type.Compile (t);
    IF ArrayType.Split (t, index, element) THEN t := index END;

    IF (t = NIL) THEN (* open array *)
      CG.Load_integer (Target.Integer.cg_type, TInt.Zero);
    ELSIF Type.GetBounds (t, min, max) THEN (* ordinal type *)
      IF Type.IsSubtype (t, LInt.T)
        THEN CG.Load_integer (Target.Longint.cg_type, min);
        ELSE CG.Load_integer (Target.Integer.cg_type, min);
      END;
    ELSIF Type.IsEqual (t, Reel.T, NIL) THEN
      CG.Load_float (Target.Real.min);
    ELSIF Type.IsEqual (t, LReel.T, NIL) THEN
      CG.Load_float (Target.Longreal.min);
    ELSIF Type.IsEqual (t, EReel.T, NIL) THEN
      CG.Load_float (Target.Extended.min);
    ELSE
      <* ASSERT FALSE *>
    END;
  END Compile;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  VAR t, index, elem: Type.T;  e: Expr.T;
  BEGIN
    e := ce.args[0];
    IF TypeExpr.Split (e, t) THEN RETURN FirstOfType (t) END;
    t := Expr.TypeOf (e);
    IF NOT ArrayType.Split (t, index, elem) THEN RETURN NIL END;
    RETURN FirstOfType (t);
  END Fold;

PROCEDURE FirstOfType (t: Type.T): Expr.T =
  VAR min, max: Target.Int;  elem, t_base: Type.T;
  BEGIN
    IF ArrayType.Split (t, t, elem) AND (t = NIL) THEN
      RETURN IntegerExpr.New (Int.T, TInt.Zero);
    END;
    t_base := Type.Base (t);
    IF Type.GetBounds (t, min, max) THEN
      IF t_base = Int.T OR t_base = LInt.T
        THEN RETURN IntegerExpr.New (t_base, min);
        ELSE RETURN EnumExpr.New (t, min);
      END;
    ELSIF t_base = Reel.T THEN
      RETURN ReelExpr.New (Target.Real.min);
    ELSIF t_base = LReel.T THEN
      RETURN ReelExpr.New (Target.Longreal.min);
    ELSIF t_base = EReel.T THEN
      RETURN ReelExpr.New (Target.Extended.min);
    ELSE
      RETURN NIL;
    END;
  END FirstOfType;

PROCEDURE Initialize () =
  BEGIN
    Z := CallExpr.NewMethodList (1, 1, TRUE, FALSE, FALSE, NIL,
                                 TypeOf,
                                 TypeOf,
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
    Procedure.DefinePredefined ("FIRST", Z, TRUE);
  END Initialize;

BEGIN
END First.
