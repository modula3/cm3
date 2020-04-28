(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Val.m3                                                *)
(* Last Modified On Tue May  3 16:33:31 PDT 1994 By kalsow     *)
(*      Modified On Fri Dec 21 01:18:57 1990 By muller         *)

MODULE Val;

IMPORT CallExpr, Expr, ExprRep, Type, Procedure, Error, TypeExpr, Int, LInt;
IMPORT IntegerExpr, EnumExpr, EnumType, CheckExpr, Target, TInt, CG;

VAR Z: CallExpr.MethodList;

PROCEDURE TypeOf (ce: CallExpr.T): Type.T =
  VAR t: Type.T;
  BEGIN
    IF TypeExpr.Split (ce.args[1], t)
      THEN RETURN Type.StripPacked (t);
      ELSE RETURN Int.T;
    END;
  END TypeOf;

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  VAR t, u: Type.T;  mint, maxt, minu, maxu: Target.Int;
  BEGIN
    u := Expr.TypeOf (ce.args[0]);
    IF Type.IsSubtype (u, LInt.T) THEN
      t := LInt.T;
    ELSIF Type.IsSubtype (u, Int.T) THEN
      t := Int.T
    ELSE
      Error.Msg ("VAL: first argument must be an integer");
    END;
    IF NOT TypeExpr.Split (ce.args[1], t) THEN
      Error.Msg ("VAL: second argument must be a type");
    ELSIF NOT Type.IsOrdinal (t) THEN
      Error.Msg ("VAL: second argument must be an ordinal type");
    ELSE (* looks ok *)
      Expr.GetBounds (ce.args[0], minu, maxu);
      EVAL Type.GetBounds (t, mint, maxt);
      IF TInt.LT (minu, mint) THEN
        (* we need a lower bound check *)
        IF TInt.LT (maxt, maxu) THEN
          (* we also need an upper bound check *)
          ce.args[0] := CheckExpr.New (ce.args[0], mint, maxt,
                                          CG.RuntimeError.ValueOutOfRange);
          Expr.TypeCheck (ce.args[0], cs);
        ELSE
          ce.args[0] := CheckExpr.NewLower (ce.args[0], mint,
                                          CG.RuntimeError.ValueOutOfRange);
          Expr.TypeCheck (ce.args[0], cs);
        END;
      ELSIF TInt.LT (maxt, maxu) THEN
        (* we need an upper bound check *)
        ce.args[0] := CheckExpr.NewUpper (ce.args[0], maxt,
                                          CG.RuntimeError.ValueOutOfRange);
        Expr.TypeCheck (ce.args[0], cs);
      END;
    END;
    ce.type := t;
  END Check;

PROCEDURE Prep (ce: CallExpr.T) =
  BEGIN
    Expr.Prep (ce.args[0]);
  END Prep;

PROCEDURE Compile (ce: CallExpr.T) =
  VAR t, u: Type.T;
  BEGIN
    u := Expr.TypeOf (ce.args[0]);
    IF TypeExpr.Split (ce.args[1], t) THEN Type.Compile (t) END;
    Expr.Compile (ce.args[0]);
    IF Type.IsSubtype (t, LInt.T) THEN
      (* definitely not an enumeration *)
      IF Type.IsSubtype (u, Int.T) THEN
        CG.Loophole (Target.Integer.cg_type, Target.Longint.cg_type);
      END;
    ELSE
      (* base type Int.T or enumeration *)
      IF Type.IsSubtype (u, LInt.T) THEN
        CG.Loophole (Target.Longint.cg_type, Target.Integer.cg_type);
      END;
    END;
  END Compile;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  VAR t: Type.T;  e: Expr.T;  x, min, max: Target.Int;
  BEGIN
    e := Expr.ConstValue (ce.args[0]);
    IF (e = NIL) OR (NOT IntegerExpr.Split (e, x, t))
      OR (NOT TypeExpr.Split (ce.args[1], t)) THEN
      RETURN NIL;
    END;
    EVAL Type.GetBounds (t, min, max);
    IF TInt.LT (x, min) OR TInt.LT (max, x) THEN
      Error.Msg ("VAL: value out of range");
      RETURN NIL;
    END;
    t := Type.Base (t);
    IF EnumType.Is (t)
      THEN RETURN EnumExpr.New (t, x);
      ELSE RETURN IntegerExpr.New (t, x);
    END;
  END Fold;

PROCEDURE GetBounds (ce: CallExpr.T;  VAR min, max: Target.Int) =
  BEGIN
    Expr.GetBounds (ce.args[0], min, max);
  END GetBounds;

PROCEDURE Initialize () =
  BEGIN
    Z := CallExpr.NewMethodList (2, 2, TRUE, FALSE, TRUE, NIL,
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
                                 GetBounds,
                                 CallExpr.IsNever, (* writable *)
                                 CallExpr.IsNever, (* designator *)
                                 CallExpr.NotWritable (* noteWriter *));
    Procedure.DefinePredefined ("VAL", Z, TRUE);
  END Initialize;

BEGIN
END Val.
