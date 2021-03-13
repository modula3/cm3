(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

GENERIC MODULE Rotate (Rep);

IMPORT CG, CallExpr, Expr, ExprRep, Procedure, Formal, Type, SubrangeType;
IMPORT Int, IntegerExpr, Value, ProcType, CheckExpr, Target, TInt, TWord;
FROM Rep IMPORT T;
FROM TargetMap IMPORT Word_types;

VAR Z, ZL, ZR: CallExpr.MethodList;
VAR formals, formalsL, formalsR: Value.T;
VAR rep: [FIRST (Word_types) .. LAST (Word_types)];
VAR max : Target.Int;

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  BEGIN
    EVAL Formal.CheckArgs (cs, ce.args, formals, ce.proc);
    ce.type := T;
  END Check;

PROCEDURE CheckL (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  BEGIN
    EVAL Formal.CheckArgs (cs, ce.args, formalsL, ce.proc);
    ce.type := T;
  END CheckL;

PROCEDURE CheckR (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  BEGIN
    EVAL Formal.CheckArgs (cs, ce.args, formalsR, ce.proc);
    ce.type := T;
  END CheckR;

PROCEDURE Compile (ce: CallExpr.T) =
  BEGIN
    Expr.Compile (ce.args[0]);
    Expr.Compile (ce.args[1]);
    CG.Rotate (Word_types[rep].cg_type);
  END Compile;

PROCEDURE CompileL (ce: CallExpr.T) =
  BEGIN
    Expr.Compile (ce.args[0]);
    CheckExpr.EmitChecks (ce.args[1], TInt.Zero, max,
                          CG.RuntimeError.ValueOutOfRange);
    CG.Rotate_left (Word_types[rep].cg_type);
  END CompileL;

PROCEDURE CompileR (ce: CallExpr.T) =
  BEGIN
    Expr.Compile (ce.args[0]);
    CheckExpr.EmitChecks (ce.args[1], TInt.Zero, max,
                          CG.RuntimeError.ValueOutOfRange);
    CG.Rotate_right (Word_types[rep].cg_type);
  END CompileR;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  VAR e0, e1: Expr.T;  w0, result: Target.Int;  i1: INTEGER;  t: Type.T;
  BEGIN
    e0 := Expr.ConstValue (ce.args[0]);
    e1 := Expr.ConstValue (ce.args[1]);
    IF (e0 # NIL) AND IntegerExpr.Split (e0, w0, t)
      AND (e1 # NIL) AND IntegerExpr.ToInt (e1, i1)
    THEN
      TWord.And (w0, Word_types[rep].max, w0);
      TWord.Rotate (w0, i1, Word_types[rep].bytes, result);
      EVAL TInt.Extend (result, Word_types[rep].bytes, result);
      RETURN IntegerExpr.New (T, result);
    END;
    RETURN NIL;
  END Fold;

PROCEDURE FoldL (ce: CallExpr.T): Expr.T =
  VAR e0, e1: Expr.T;  w0, result: Target.Int;  i1: INTEGER;  t: Type.T;
  BEGIN
    e0 := Expr.ConstValue (ce.args[0]);
    e1 := Expr.ConstValue (ce.args[1]);
    IF (e0 # NIL) AND IntegerExpr.Split (e0, w0, t)
      AND (e1 # NIL) AND IntegerExpr.ToInt (e1, i1)
      AND 0 <= i1 AND i1 < Word_types[rep].size
    THEN
      TWord.And (w0, Word_types[rep].max, w0);
      TWord.Rotate (w0, i1, Word_types[rep].bytes, result);
      EVAL TInt.Extend (result, Word_types[rep].bytes, result);
      RETURN IntegerExpr.New (T, result);
    END;
    RETURN NIL;
  END FoldL;

PROCEDURE FoldR (ce: CallExpr.T): Expr.T =
  VAR e0, e1: Expr.T;  w0, result: Target.Int;  i1: INTEGER;  t: Type.T;
  BEGIN
    e0 := Expr.ConstValue (ce.args[0]);
    e1 := Expr.ConstValue (ce.args[1]);
    IF (e0 # NIL) AND IntegerExpr.Split (e0, w0, t)
      AND (e1 # NIL) AND IntegerExpr.ToInt (e1, i1)
      AND 0 <= i1 AND i1 < Word_types[rep].size
    THEN
      TWord.And (w0, Word_types[rep].max, w0);
      TWord.Rotate (w0, -i1, Word_types[rep].bytes, result);
      EVAL TInt.Extend (result, Word_types[rep].bytes, result);
      RETURN IntegerExpr.New (T, result);
    END;
    RETURN NIL;
  END FoldR;

PROCEDURE Initialize (r: INTEGER) =
  VAR
    b   := TInt.FromInt (Word_types[r].size-1, max);
    sub := SubrangeType.New (TInt.Zero, max, Int.T, FALSE);

    f0  := Formal.NewBuiltin ("x", 0, T);
    f1  := Formal.NewBuiltin ("n", 1, Int.T);
    t   := ProcType.New (T, f0, f1);

    Lf0 := Formal.NewBuiltin ("x", 0, T);
    Lf1 := Formal.NewBuiltin ("n", 1, sub);
    Lt  := ProcType.New (T, Lf0, Lf1);

    Rf0 := Formal.NewBuiltin ("x", 0, T);
    Rf1 := Formal.NewBuiltin ("n", 1, sub);
    Rt  := ProcType.New (T, Rf0, Rf1);
  BEGIN
    rep := r;
    <*ASSERT b*>
    Z := CallExpr.NewMethodList (2, 2, TRUE, TRUE, TRUE, T,
                                 NIL, NIL,
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
    Procedure.DefinePredefined ("Rotate", Z, FALSE, t, assignable:=TRUE);
    formals := ProcType.Formals (t);

    ZL := CallExpr.NewMethodList (2, 2, TRUE, TRUE, TRUE, T,
                                 NIL, NIL,
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
    Procedure.DefinePredefined ("LeftRotate", ZL, FALSE, Lt, assignable:=TRUE);
    formalsL := ProcType.Formals (Lt);

    ZR := CallExpr.NewMethodList (2, 2, TRUE, TRUE, TRUE,  T,
                                 NIL, NIL,
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
    Procedure.DefinePredefined ("RightRotate", ZR, FALSE, Rt, assignable:=TRUE);
    formalsR := ProcType.Formals (Rt);
  END Initialize;

BEGIN
END Rotate.
