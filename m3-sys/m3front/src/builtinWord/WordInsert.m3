(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: WordInsert.m3                                         *)
(* Last Modified On Mon Dec  5 15:30:50 PST 1994 By kalsow     *)
(*      Modified On Thu Mar  7 20:20:02 1991 By muller         *)

MODULE WordInsert;

IMPORT CG, CallExpr, Expr, ExprRep, Procedure;
IMPORT Int, IntegerExpr, ProcType, CheckExpr, Card;
IMPORT Target, TInt, TWord, Value, Formal, Host;

VAR Z: CallExpr.MethodList;
VAR formals: Value.T;

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  BEGIN
    EVAL Formal.CheckArgs (cs, ce.args, formals, ce.proc);
    ce.type := Int.T;
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
      b := TInt.FromInt (Target.Integer.size, max);  <*ASSERT b*>
      CG.Push (t2);
      CG.Push (t3);
      CG.Add (Target.Integer.cg_type);
      CG.Check_hi (max, CG.RuntimeError.ValueOutOfRange);
      CG.Discard (Target.Integer.cg_type);
    END;
    Expr.Compile (ce.args[0]);
    CG.Force ();
    Expr.Compile (ce.args[1]);
    CG.Force ();
    CG.Push (t2);
    CG.Push (t3);
    CG.Insert ();
    CG.Free (t2);
    CG.Free (t3);
  END Compile;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  VAR e0, e1, e2, e3: Expr.T;  w0, w1, i2, i3, result: Target.Int;
  BEGIN
    e0 := Expr.ConstValue (ce.args[0]);
    e1 := Expr.ConstValue (ce.args[1]);
    e2 := Expr.ConstValue (ce.args[2]);
    e3 := Expr.ConstValue (ce.args[3]);
    IF (e0 = NIL) OR (NOT IntegerExpr.Split (e0, w0)) OR
       (e1 = NIL) OR (NOT IntegerExpr.Split (e1, w1)) OR 
       (e2 = NIL) OR (NOT IntegerExpr.Split (e2, i2)) OR 
       (e3 = NIL) OR (NOT IntegerExpr.Split (e3, i3)) OR
       NOT TWord.Insert (w0, w1, i2, i3, result) THEN
      RETURN NIL;
    END;
    RETURN IntegerExpr.New (result);
  END Fold;

PROCEDURE Initialize () =
  VAR
    f0 := Formal.NewBuiltin ("x", 0, Int.T);
    f1 := Formal.NewBuiltin ("y", 1, Int.T);
    f2 := Formal.NewBuiltin ("i", 2, Card.T);
    f3 := Formal.NewBuiltin ("n", 3, Card.T);
    t  := ProcType.New (Int.T, f0, f1, f2, f3);
  BEGIN
    Z := CallExpr.NewMethodList (4, 4, TRUE, TRUE, TRUE, Int.T,
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
END WordInsert.
