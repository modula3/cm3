(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

GENERIC MODULE Insert (Rep);

IMPORT CG, CallExpr, Expr, ExprRep, Procedure;
IMPORT IntegerExpr, Type, ProcType, CheckExpr, Card;
IMPORT Target, TInt, TWord, Value, Formal, Host;
FROM Rep IMPORT T;
FROM TargetMap IMPORT Word_types;

VAR Z: CallExpr.MethodList;
VAR formals: Value.T;
VAR rep: [FIRST (Word_types) .. LAST (Word_types)];

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  BEGIN
    EVAL Formal.CheckArgs (cs, ce.args, formals, ce.proc);
    ce.type := T;
  END Check;

PROCEDURE Compile (ce: CallExpr.T) =
  VAR t2, t3: CG.Val;  b: BOOLEAN;  max: Target.Int;
  BEGIN
    CheckExpr.EmitChecks (ce.args[2], TInt.Zero, Target.Integer.max,
                          CG.RuntimeError.ValueOutOfRange);
    t2 := CG.Pop ();
    CheckExpr.EmitChecks (ce.args[3], TInt.Zero, Target.Integer.max,
                          CG.RuntimeError.ValueOutOfRange);
    t3 := CG.Pop ();
    IF Host.doRangeChk THEN
      b := TInt.FromInt (Word_types[rep].size, max);  <*ASSERT b*>
      CG.Push (t2);
      CG.Push (t3);
      CG.Add (Target.Integer.cg_type);
      CG.Check_hi (Target.Integer.cg_type, max,
                   CG.RuntimeError.ValueOutOfRange);
      CG.Discard (Target.Integer.cg_type);
    END;
    Expr.Compile (ce.args[0]);
    CG.Force ();
    Expr.Compile (ce.args[1]);
    CG.Force ();
    CG.Push (t2);
    CG.Push (t3);
    CG.Insert (Word_types[rep].cg_type);
    CG.Free (t2);
    CG.Free (t3);
  END Compile;

PROCEDURE GetBitIndex (e: Expr.T;  VAR i: INTEGER): BOOLEAN =
  BEGIN
    e := Expr.ConstValue (e);
    IF (e = NIL) THEN RETURN FALSE END;
    RETURN IntegerExpr.ToInt (e, i)
       AND (0 <= i) AND (i <= Word_types[rep].size);
  END GetBitIndex;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  VAR e0, e1: Expr.T;  w0, w1, result: Target.Int; i2, i3: INTEGER;  t: Type.T;
  BEGIN
    e0 := Expr.ConstValue (ce.args[0]);
    e1 := Expr.ConstValue (ce.args[1]);
    IF   (e0 = NIL) OR (e1 = NIL)
      OR NOT IntegerExpr.Split (e0, w0, t)
      OR NOT IntegerExpr.Split (e1, w1, t)
      OR NOT GetBitIndex (ce.args[2], i2)
      OR NOT GetBitIndex (ce.args[3], i3)
      OR i2 + i3 > Word_types[rep].size
      OR NOT TWord.Insert (w0, w1, i2, i3, result) THEN
      RETURN NIL;
    END;
    TInt.Chop (result, Word_types[rep].bytes);
    RETURN IntegerExpr.New (T, result);
  END Fold;

PROCEDURE Initialize (r: INTEGER) =
  VAR
    f0 := Formal.NewBuiltin ("x", 0, T);
    f1 := Formal.NewBuiltin ("y", 1, T);
    f2 := Formal.NewBuiltin ("i", 2, Card.T);
    f3 := Formal.NewBuiltin ("n", 3, Card.T);
    t  := ProcType.New (T, f0, f1, f2, f3);
  BEGIN
    rep := r;
    Z := CallExpr.NewMethodList (4, 4, TRUE, TRUE, TRUE, T,
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
    Procedure.Define ("Insert", Z, FALSE, t);
    formals := ProcType.Formals (t);
  END Initialize;

BEGIN
END Insert.
