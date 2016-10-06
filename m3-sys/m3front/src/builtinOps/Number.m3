(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Number.m3                                             *)
(* Last Modified On Tue May  3 16:32:23 PDT 1994 By kalsow     *)
(*      Modified On Fri Dec 21 01:16:23 1990 By muller         *)

MODULE Number;

IMPORT CG, CallExpr, Expr, ExprRep, Type, Procedure, Card, Error, ArrayExpr;
IMPORT ArrayType, TypeExpr, IntegerExpr, Int, EnumType, Target, TInt;

VAR Z: CallExpr.MethodList;

PROCEDURE Check (ce: CallExpr.T;  <*UNUSED*> VAR cs: Expr.CheckState) =
  VAR e: Expr.T; t, index, element: Type.T;
  BEGIN
    e := ce.args[0];
    t := Expr.TypeOf (e);
    IF ArrayType.Split (t, index, element) THEN
      IF (index = NIL) THEN index := Int.T END;
    ELSIF TypeExpr.Split (e, t) THEN
      IF ArrayType.Split (t, index, element) THEN
        IF (index = NIL) THEN
          Error.Msg ("NUMBER: argument cannot be an open array type");
          index := Int.T;
        END;
      ELSE
        index := t;
      END;
    ELSE
      Error.Msg ("NUMBER: argument must be a type or array");
      index := Int.T;
    END;
    IF EnumType.Is (index) THEN
      IF NOT TInt.LT (TInt.Zero, Type.Number (index)) THEN
        Error.Msg ("NUMBER: empty enumeration type");
      END;
    ELSIF Type.IsOrdinal (index) THEN
      (* ordinal type => OK*)
    ELSE
      Error.Msg ("NUMBER: argument must be an ordinal type, array type or array");
    END;
    ce.type := Card.T;
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
    min, max, tmp, num: Target.Int;
  BEGIN
    IF NOT TypeExpr.Split (e, t) THEN t := Expr.TypeOf (e) END;
    IF ArrayType.Split (t, index, element) THEN t := index END;

    IF (t = NIL) THEN (* open array *)
      Expr.Compile (e);
      CG.Open_size (0);
    ELSE
      EVAL Type.GetBounds (t, min, max);
      IF TInt.LT (max, min) THEN
        CG.Load_integer (Target.Integer.cg_type, TInt.Zero);
      ELSIF TInt.Subtract (max, min, tmp)
        AND TInt.Add (tmp, TInt.One, num)
        AND NOT TInt.LT (Target.Integer.max, num) THEN
        CG.Load_integer (Target.Integer.cg_type, num);
      ELSE
        Error.Warn (2, "result of NUMBER too large");
        CG.Load_integer (Target.Integer.cg_type, Target.Integer.max);
        CG.Check_hi (Target.Integer.cg_type, TInt.Zero,
                     CG.RuntimeError.ValueOutOfRange);
      END;
    END;
  END Compile;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  VAR min, max, tmp, num: Target.Int;  t, index, elem: Type.T;  e: Expr.T;
  BEGIN
    e := ce.args[0];
    IF NOT TypeExpr.Split (e, t) THEN
      t := Expr.TypeOf (e);
      IF NOT ArrayType.Split (t, index, elem) THEN RETURN NIL END;
      IF (index = NIL) THEN
        (* NUMBER (open array value) => try for constant open array *)
        e := Expr.ConstValue (e);
        IF (e = NIL) THEN RETURN NIL END;
        IF ArrayExpr.GetBounds (e, min, max)
          AND TInt.Subtract (max, min, tmp)
          AND TInt.Add (tmp, TInt.One, num)
          AND NOT TInt.LT (num, Target.Integer.min)
          AND NOT TInt.LT (Target.Integer.max, num)
          THEN RETURN IntegerExpr.New (Int.T, num);
          ELSE RETURN NIL;
        END;
      END;
    END;
    IF ArrayType.Split (t, t, elem) AND (t = NIL) THEN RETURN NIL; END;
    IF NOT Type.GetBounds (t, min, max) THEN RETURN NIL; END;

    IF TInt.LT (max, min) THEN
      RETURN IntegerExpr.New (Int.T, TInt.Zero);
    ELSIF TInt.Subtract (max, min, tmp)
      AND TInt.Add (tmp, TInt.One, num)
      AND NOT TInt.LT (Target.Integer.max, num) THEN
      RETURN IntegerExpr.New (Int.T, num);
    ELSE
      RETURN NIL;
    END;
  END Fold;

PROCEDURE Initialize () =
  BEGIN
    Z := CallExpr.NewMethodList (1, 1, TRUE, FALSE, FALSE, Card.T,
                                 NIL,
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
    Procedure.DefinePredefined ("NUMBER", Z, TRUE);
  END Initialize;

BEGIN
END Number.
