(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: GenExtract.mg                                         *)
(* Last Modified On Mon Dec  5 15:30:52 PST 1994 By kalsow     *)
(*      Modified On Thu Mar  7 20:19:39 1991 By muller         *)

GENERIC MODULE GenExtract (Rep);

IMPORT CG, CallExpr, Expr, ExprRep, Procedure;
IMPORT IntegerExpr, Type, ProcType, Host, Card;
IMPORT Target, TInt, TWord, Value, Formal, CheckExpr, Error;
FROM Rep IMPORT T;

VAR Z: CallExpr.MethodList;
VAR formals: Value.T;

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  BEGIN
    EVAL Formal.CheckArgs (cs, ce.args, formals, ce.proc);
    ce.type := T;
  END Check;

PROCEDURE Compile (ce: CallExpr.T) =
  VAR
    t1, t2: CG.Val;
    max: Target.Int;
    b, x1, x2: BOOLEAN;
    i1, i2: INTEGER;
  BEGIN
    x1 := GetBitIndex (ce.args[1], i1);
    x2 := GetBitIndex (ce.args[2], i2);

    IF x1 AND x2 THEN
      (* we can use the extract_mn operator *)
      IF (i1 + i2 > Rep.Size) THEN
        Error.Warn (2, "Word.Extract: i+n value out of range");
        CG.Load_integer (Target.Integer.cg_type, TInt.One);
        CG.Check_hi (Target.Integer.cg_type, TInt.Zero,
                     CG.RuntimeError.ValueOutOfRange);
      ELSE
        Expr.Compile (ce.args[0]);
        CG.Extract_mn (Rep.Signed, FALSE, i1, i2);
      END;

    ELSIF x2 THEN
      (* we can use the extract_n operator *)
      b := TInt.FromInt (Rep.Size - i2, Target.Integer.bytes, max);
      <*ASSERT b*>
      Expr.Compile (ce.args[0]);
      CheckExpr.EmitChecks (ce.args[1], TInt.Zero, max,
                            CG.RuntimeError.ValueOutOfRange);
      CG.Extract_n (Rep.Signed, FALSE, i2);

    ELSIF x1 THEN
      (* we need the general purpose extract operator, but can simplify
         the range checking code *)
      b := TInt.FromInt (Rep.Size - i1, Target.Integer.bytes, max);
      <*ASSERT b*>
      Expr.Compile (ce.args[0]);
      CG.Force ();
      CG.Load_intt (i1);
      CheckExpr.EmitChecks (ce.args[2], TInt.Zero, max,
                            CG.RuntimeError.ValueOutOfRange);
      CG.Extract (Rep.Signed, sign := FALSE);

    ELSE
      (* we need the general purpose extract operator *)
      CheckExpr.EmitChecks (ce.args[1], TInt.Zero, Target.Integer.max,
                            CG.RuntimeError.ValueOutOfRange);
      t1 := CG.Pop ();
      CheckExpr.EmitChecks (ce.args[2], TInt.Zero, Target.Integer.max,
                            CG.RuntimeError.ValueOutOfRange);
      t2 := CG.Pop ();
      IF Host.doRangeChk THEN
        b := TInt.FromInt (Rep.Size, Target.Integer.bytes, max);
        <*ASSERT b*>
        CG.Push (t1);
        CG.Push (t2);
        CG.Add (Target.Integer.cg_type);
        CG.Check_hi (Target.Integer.cg_type, max,
                     CG.RuntimeError.ValueOutOfRange);
        CG.Discard (Target.Integer.cg_type);
      END;
      Expr.Compile (ce.args[0]);
      CG.Force ();
      CG.Push (t1);
      CG.Push (t2);
      CG.Extract (Rep.Signed, sign := FALSE);
      CG.Free (t1);
      CG.Free (t2);
    END;
  END Compile;

PROCEDURE GetBitIndex (e: Expr.T;  VAR i: INTEGER): BOOLEAN =
  BEGIN
    e := Expr.ConstValue (e);
    IF (e = NIL) THEN RETURN FALSE END;
    RETURN IntegerExpr.ToInt (e, i) AND (0 <= i) AND (i <= Rep.Size);
  END GetBitIndex;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  VAR e0, e1, e2: Expr.T;  w0, result: Target.Int;  i1, i2: INTEGER;  t: Type.T;
  BEGIN
    e0 := Expr.ConstValue (ce.args[0]);
    e1 := Expr.ConstValue (ce.args[1]);
    e2 := Expr.ConstValue (ce.args[2]);
    IF (e0 = NIL) OR (NOT IntegerExpr.Split (e0, w0, t)) OR 
       (e1 = NIL) OR (NOT IntegerExpr.ToInt (e1, i1)) OR 
       (e2 = NIL) OR (NOT IntegerExpr.ToInt (e2, i2)) OR
       NOT TWord.Extract (w0, i1, i2, result) THEN
      RETURN NIL;
    END;
    RETURN IntegerExpr.New (T, result);
  END Fold;

PROCEDURE GetBounds (ce: CallExpr.T;  VAR min, max: Target.Int) =
  VAR min_bits, max_bits: Target.Int;  i: INTEGER;
  BEGIN
    Expr.GetBounds (ce.args[2], min_bits, max_bits);
    IF TInt.ToInt (max_bits, i) AND i < Rep.Size THEN
      IF NOT TWord.Extract (TInt.MOne, 0, i, max) THEN
        EVAL Type.GetBounds (T, min, max);
      END;
      min := TInt.Zero;
    ELSE
      (* possible that we'll preserve all bits *)
      Expr.GetBounds (ce.args[0], min, max);
    END;
  END GetBounds;

PROCEDURE Initialize () =
  VAR
    f0 := Formal.NewBuiltin ("x", 0, T);
    f1 := Formal.NewBuiltin ("i", 1, Card.T);
    f2 := Formal.NewBuiltin ("n", 2, Card.T);
    t  := ProcType.New (T, f0, f1, f2);
  BEGIN
    Z := CallExpr.NewMethodList (3, 3, TRUE, TRUE, TRUE, T,
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
                                 GetBounds,
                                 CallExpr.IsNever, (* writable *)
                                 CallExpr.IsNever, (* designator *)
                                 CallExpr.NotWritable (* noteWriter *));
    Procedure.Define ("Extract", Z, FALSE, t);
    formals := ProcType.Formals (t);
  END Initialize;

BEGIN
END GenExtract.
