(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

GENERIC MODULE Insert (Rep);

IMPORT CG, CallExpr, Expr, ExprRep, Procedure;
IMPORT IntegerExpr, Type, ProcType, Host, Card;
IMPORT Target, TInt, TWord, Value, Formal, CheckExpr, Error;
IMPORT MSIR, MSIRBuilder;
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
  VAR
    t2, t3: CG.Val;
    max: Target.Int;
    b, x2, x3: BOOLEAN;
    i2, i3: INTEGER;
  BEGIN
    x2 := GetBitIndex (ce.args[2], i2);
    x3 := GetBitIndex (ce.args[3], i3);

    IF x2 AND x3 THEN
      (* we can use the insert_mn operator *)
      IF (i2 + i3 > Word_types[rep].size) THEN
        Error.Warn (2, "Word.Insert: i+n value out of range");
        CG.Load_integer (Target.Integer.cg_type, TInt.One);
        CG.Check_hi (Target.Integer.cg_type, TInt.Zero,
                     CG.RuntimeError.ValueOutOfRange);
      ELSE
        Expr.Compile (ce.args[0]);
        Expr.Compile (ce.args[1]);
        CG.Insert_mn (Word_types[rep].cg_type, i2, i3);
      END;

    ELSIF x3 THEN
      (* we can use the insert_n operator *)
      b := TInt.FromInt (Word_types[rep].size - i3, max);  <*ASSERT b*>
      Expr.Compile (ce.args[0]);
      CG.ForceStacked ();
      Expr.Compile (ce.args[1]);
      CheckExpr.EmitChecks (ce.args[2], TInt.Zero, max,
                            CG.RuntimeError.ValueOutOfRange);
      CG.Insert_n (Word_types[rep].cg_type, i3);

    ELSIF x2 THEN
      (* we need the general purpose insert operator, but can simplify
         the range checking code *)
      b := TInt.FromInt (Word_types[rep].size - i2, max);  <*ASSERT b*>
      Expr.Compile (ce.args[0]);
      CG.ForceStacked ();
      Expr.Compile (ce.args[1]);
      CG.ForceStacked ();
      CG.Load_intt (i2);
      CheckExpr.EmitChecks (ce.args[3], TInt.Zero, max,
                            CG.RuntimeError.ValueOutOfRange);
      CG.Insert (Word_types[rep].cg_type);

    ELSE
      (* we need the general purpose insert operator *)
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
      CG.ForceStacked ();
      Expr.Compile (ce.args[1]);
      CG.ForceStacked ();
      CG.Push (t2);
      CG.Push (t3);
      CG.Insert (Word_types[rep].cg_type);
      CG.Free (t2);
      CG.Free (t3);
    END;
  END Compile;

PROCEDURE GetBitIndex (e: Expr.T;  VAR i: INTEGER): BOOLEAN =
  BEGIN
    e := Expr.ConstValue (e);
    IF (e = NIL) THEN RETURN FALSE END;
    RETURN IntegerExpr.ToInt (e, i)
       AND (0 <= i) AND (i <= Word_types[rep].size);
  END GetBitIndex;

PROCEDURE CompileMSIR (ce: CallExpr.T): MSIR.Value =
  (* Insert(x, y, i, n): insert n low-order bits of y into x at position i.
     mask         = lshr(allones, W-n)    -- n-bit mask in LSB
     shifted_mask = shl(mask, i)          -- mask at position i
     result = or(and(x, xor(shifted_mask, allones)), shl(and(y, mask), i))
     All operands coerced to canonical word type wt (same pattern as Shift.mg). *)
  VAR
    x  := Expr.CompileMSIR (ce.args[0]);
    y  := Expr.CompileMSIR (ce.args[1]);
    i  := Expr.CompileMSIR (ce.args[2]);
    n  := Expr.CompileMSIR (ce.args[3]);
    b  := MSIRBuilder.CurrentBlock ();
    W  := Word_types[rep].size;
    wt := MSIR.TI (W);
    ones, wConst, wMinusN, mask, shiftedMask, invMask,
    xCleared, yBits, yShifted: MSIR.Value;
    xb, yb, ib, nb : INTEGER;
  BEGIN
    IF x = NIL OR y = NIL OR i = NIL OR n = NIL THEN RETURN NIL END;
    xb := MSIR.BitWidth (MSIR.ValueType (x));
    yb := MSIR.BitWidth (MSIR.ValueType (y));
    ib := MSIR.BitWidth (MSIR.ValueType (i));
    nb := MSIR.BitWidth (MSIR.ValueType (n));
    IF xb > 0 AND xb # W THEN
      IF xb > W THEN x := MSIR.BuildTrunc (b, "", x, wt) ELSE x := MSIR.BuildZExt (b, "", x, wt) END;
    END;
    IF yb > 0 AND yb # W THEN
      IF yb > W THEN y := MSIR.BuildTrunc (b, "", y, wt) ELSE y := MSIR.BuildZExt (b, "", y, wt) END;
    END;
    IF ib > 0 AND ib # W THEN
      IF ib > W THEN i := MSIR.BuildTrunc (b, "", i, wt) ELSE i := MSIR.BuildZExt (b, "", i, wt) END;
    END;
    IF nb > 0 AND nb # W THEN
      IF nb > W THEN n := MSIR.BuildTrunc (b, "", n, wt) ELSE n := MSIR.BuildZExt (b, "", n, wt) END;
    END;
    ones        := MSIR.ConstInt (wt, -1);
    wConst      := MSIR.ConstInt (wt, W);
    wMinusN     := MSIR.BuildISub  (b, "", wConst, n);
    mask        := MSIR.BuildILShr (b, "", ones, wMinusN);
    (* lshr by W (the n = 0 case) yields a POISON value in LLVM, not 0, so the
       mask would be garbage and Insert(x, y, i, 0) would corrupt x instead of
       returning it unchanged (p227).  Force mask = 0 when n = 0. *)
    mask        := MSIR.BuildSelect (b, "",
                     MSIR.BuildICmp (b, "", MSIR.CmpPred.Eq, n,
                                     MSIR.ConstInt (wt, 0)),
                     MSIR.ConstInt (wt, 0), mask);
    shiftedMask := MSIR.BuildIShl  (b, "", mask, i);
    invMask     := MSIR.BuildIXor  (b, "", shiftedMask, ones);
    xCleared    := MSIR.BuildIAnd  (b, "", x, invMask);
    yBits       := MSIR.BuildIAnd  (b, "", y, mask);
    yShifted    := MSIR.BuildIShl  (b, "", yBits, i);
    RETURN MSIR.BuildIOr (b, "", xCleared, yShifted);
  END CompileMSIR;

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
    EVAL TInt.Extend (result, Word_types[rep].bytes, result);
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
    CallExpr.SetMethodMSIR (Z, CompileMSIR);
    Procedure.DefinePredefined ("Insert", Z, FALSE, t, assignable:=TRUE);
    formals := ProcType.Formals (t);
  END Initialize;

BEGIN
END Insert.
