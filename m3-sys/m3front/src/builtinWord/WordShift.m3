(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: WordShift.m3                                          *)
(* Last Modified On Mon Dec  5 15:30:43 PST 1994 By kalsow     *)
(*      Modified On Fri May 18 08:15:52 1990 By muller         *)

MODULE WordShift;

IMPORT CG, CallExpr, Expr, ExprRep, Procedure, SubrangeType, Formal;
IMPORT Int, IntegerExpr, Value, ProcType, CheckExpr, Target, TInt, TWord;

VAR Z, ZL, ZR: CallExpr.MethodList;
VAR formals, formalsL, formalsR: Value.T;

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  BEGIN
    EVAL Formal.CheckArgs (cs, ce.args, formals, ce.proc);
    ce.type := Int.T;
  END Check;

PROCEDURE CheckL (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  BEGIN
    EVAL Formal.CheckArgs (cs, ce.args, formalsL, ce.proc);
    ce.type := Int.T;
  END CheckL;

PROCEDURE CheckR (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  BEGIN
    EVAL Formal.CheckArgs (cs, ce.args, formalsR, ce.proc);
    ce.type := Int.T;
  END CheckR;

PROCEDURE Compile (ce: CallExpr.T) =
  BEGIN
    Expr.Compile (ce.args[0]);
    Expr.Compile (ce.args[1]);
    CG.Shift ();
  END Compile;

PROCEDURE CompileL (ce: CallExpr.T) =
  VAR max: Target.Int;  b := TInt.FromInt (Target.Integer.size -1, max);
  BEGIN
    <* ASSERT b *>
    Expr.Compile (ce.args[0]);
    CheckExpr.EmitChecks (ce.args[1], TInt.Zero, max,
                          CG.RuntimeError.ValueOutOfRange);
    CG.Shift_left ();
  END CompileL;

PROCEDURE CompileR (ce: CallExpr.T) =
  VAR max: Target.Int;  b := TInt.FromInt (Target.Integer.size -1, max);
  BEGIN
    <* ASSERT b *>
    Expr.Compile (ce.args[0]);
    CheckExpr.EmitChecks (ce.args[1], TInt.Zero, max,
                          CG.RuntimeError.ValueOutOfRange);
    CG.Shift_right ();
  END CompileR;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  VAR e0, e1: Expr.T;  w0, i1, result: Target.Int;
  BEGIN
    e0 := Expr.ConstValue (ce.args[0]);
    e1 := Expr.ConstValue (ce.args[1]);
    IF (e0 # NIL) AND IntegerExpr.Split (e0, w0)
      AND (e1 # NIL) AND IntegerExpr.Split (e1, i1)
    THEN
      TWord.Shift (w0, i1, result);
      RETURN IntegerExpr.New (TWord.Trim (result));
    ELSE
      RETURN NIL;
    END;
  END Fold;

PROCEDURE FoldL (ce: CallExpr.T): Expr.T =
  VAR e0, e1: Expr.T;  w0, i1, max, result: Target.Int;
  BEGIN
    e0 := Expr.ConstValue (ce.args[0]);
    e1 := Expr.ConstValue (ce.args[1]);
    IF (e0 # NIL) AND IntegerExpr.Split (e0, w0)
      AND (e1 # NIL) AND IntegerExpr.Split (e1, i1)
      AND TInt.LE (TInt.Zero, i1)
      AND TInt.FromInt (Target.Integer.size, max)
      AND TInt.LT (i1, max)
    THEN
      TWord.Shift (w0, i1, result);
      RETURN IntegerExpr.New (TWord.Trim (result));
    ELSE
      RETURN NIL;
    END;
  END FoldL;

PROCEDURE FoldR (ce: CallExpr.T): Expr.T =
  VAR e0, e1: Expr.T;  w0, i1, max, neg_i1, result: Target.Int;
  BEGIN
    e0 := Expr.ConstValue (ce.args[0]);
    e1 := Expr.ConstValue (ce.args[1]);
    IF (e0 # NIL) AND IntegerExpr.Split (e0, w0)
      AND (e1 # NIL) AND IntegerExpr.Split (e1, i1)
      AND TInt.LE (TInt.Zero, i1)
      AND TInt.FromInt (Target.Integer.size, max)
      AND TInt.LT (i1, max)
      AND TInt.Subtract (TInt.Zero, i1, neg_i1)
    THEN
      TWord.Shift (w0, neg_i1, result);
      RETURN IntegerExpr.New (TWord.Trim (result));
    ELSE
      RETURN NIL;
    END;
  END FoldR;

PROCEDURE Initialize () =
  VAR
    max : Target.Int;
    b   := TInt.FromInt (Target.Integer.size-1, max);
    sub := SubrangeType.New (TInt.Zero, max, Int.T, FALSE);

    f0  := Formal.NewBuiltin ("x", 0, Int.T);
    f1  := Formal.NewBuiltin ("n", 1, Int.T);
    t   := ProcType.New (Int.T, f0, f1);

    Lf0 := Formal.NewBuiltin ("x", 0, Int.T);
    Lf1 := Formal.NewBuiltin ("n", 1, sub);
    Lt  := ProcType.New (Int.T, Lf0, Lf1);

    Rf0 := Formal.NewBuiltin ("x", 0, Int.T);
    Rf1 := Formal.NewBuiltin ("n", 1, sub);
    Rt  := ProcType.New (Int.T, Rf0, Rf1);
  BEGIN
    <*ASSERT b*>
    Z := CallExpr.NewMethodList (2, 2, TRUE, TRUE, TRUE, Int.T,
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
    Procedure.Define ("Shift", Z, FALSE, t);
    formals := ProcType.Formals (t);


    ZL := CallExpr.NewMethodList (2, 2, TRUE, TRUE, TRUE, Int.T,
                                 NIL,
                                 CallExpr.NotAddressable,
                                 CheckL,
                                 CallExpr.PrepArgs,
                                 CompileL,
                                 CallExpr.NoLValue,
                                 CallExpr.NoLValue,
                                 CallExpr.NotBoolean,
                                 CallExpr.NotBoolean,
                                 FoldL,
                                 CallExpr.NoBounds,
                                 CallExpr.IsNever, (* writable *)
                                 CallExpr.IsNever, (* designator *)
                                 CallExpr.NotWritable (* noteWriter *));
    Procedure.Define ("LeftShift", ZL, FALSE, Lt);
    formalsL := ProcType.Formals (Lt);


    ZR := CallExpr.NewMethodList (2, 2, TRUE, TRUE, TRUE, Int.T,
                                 NIL,
                                 CallExpr.NotAddressable,
                                 CheckR,
                                 CallExpr.PrepArgs,
                                 CompileR,
                                 CallExpr.NoLValue,
                                 CallExpr.NoLValue,
                                 CallExpr.NotBoolean,
                                 CallExpr.NotBoolean,
                                 FoldR,
                                 CallExpr.NoBounds,
                                 CallExpr.IsNever, (* writable *)
                                 CallExpr.IsNever, (* designator *)
                                 CallExpr.NotWritable (* noteWriter *));
    Procedure.Define ("RightShift", ZR, FALSE, Rt);
    formalsR := ProcType.Formals (Rt);

  END Initialize;

BEGIN
END WordShift.
