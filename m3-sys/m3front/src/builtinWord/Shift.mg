(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

GENERIC MODULE Shift (Rep);

IMPORT CG, CallExpr, Expr, ExprRep, Procedure, Type, SubrangeType, Formal;
IMPORT Int, IntegerExpr, Value, ProcType, CheckExpr, Target, TInt, TWord;
IMPORT MSIR, MSIRBuilder;
FROM Rep IMPORT T;
FROM TargetMap IMPORT Word_types;

VAR Z, ZL, ZR: CallExpr.MethodList;
VAR formals, formalsL, formalsR: Value.T;
VAR rep: [FIRST (Word_types) .. LAST (Word_types)];
VAR max: Target.Int;

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
    CG.Shift (Word_types[rep].cg_type);
  END Compile;

PROCEDURE CompileL (ce: CallExpr.T) =
  BEGIN
    Expr.Compile (ce.args[0]);
    CheckExpr.EmitChecks (ce.args[1], TInt.Zero, max,
                          CG.RuntimeError.ValueOutOfRange);
    CG.Shift_left (Word_types[rep].cg_type);
  END CompileL;

PROCEDURE CompileR (ce: CallExpr.T) =
  BEGIN
    Expr.Compile (ce.args[0]);
    CheckExpr.EmitChecks (ce.args[1], TInt.Zero, max,
                          CG.RuntimeError.ValueOutOfRange);
    CG.Shift_right (Word_types[rep].cg_type);
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
      TWord.Shift (w0, i1, result);
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
      TWord.Shift (w0, i1, result);
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
      TWord.Shift (w0, -i1, result);
      EVAL TInt.Extend (result, Word_types[rep].bytes, result);
      RETURN IntegerExpr.New (T, result);
    END;
    RETURN NIL;
  END FoldR;

PROCEDURE ShiftMSIR (ce: CallExpr.T): MSIR.Value =
  (* General shift: n may be any signed integer.
     n > 0 → left shift; n < 0 → logical right shift by |n|; |n| >= W → 0. *)
  VAR
    x      := Expr.CompileMSIR (ce.args[0]);
    n      := Expr.CompileMSIR (ce.args[1]);
    b      := MSIRBuilder.CurrentBlock ();
    wt     := MSIR.TI (Word_types[rep].size);
    xt, nt : MSIR.T;
    W      : INTEGER;
    zero, zeroN, wConst, wm1, isNeg, negN, absN, ltW,
    nMasked, absNMasked, shlV, lshrV, safeShl, safeShr: MSIR.Value;
    xb, nb : INTEGER;
  BEGIN
    IF x = NIL OR n = NIL THEN RETURN NIL END;
    (* Coerce x to word type bit width. *)
    xb := MSIR.BitWidth (MSIR.ValueType (x));
    nb := MSIR.BitWidth (MSIR.ValueType (n));
    W  := Word_types[rep].size;
    IF xb > 0 AND xb # W THEN
      IF xb > W THEN x := MSIR.BuildTrunc (b, "", x, wt)
      ELSE x := MSIR.BuildZExt (b, "", x, wt) END;
    END;
    IF nb > 0 AND nb # W THEN
      IF nb > W THEN n := MSIR.BuildTrunc (b, "", n, wt)
      ELSE n := MSIR.BuildZExt (b, "", n, wt) END;
    END;
    xt     := MSIR.ValueType (x);
    nt     := MSIR.ValueType (n);
    zero   := MSIR.ConstInt (xt, 0);
    zeroN  := MSIR.ConstInt (nt, 0);
    wConst := MSIR.ConstInt (nt, W);
    wm1    := MSIR.ConstInt (nt, W - 1);
    isNeg  := MSIR.BuildICmp   (b, "", MSIR.CmpPred.Slt, n, zeroN);
    negN   := MSIR.BuildISub   (b, "", zeroN, n);
    absN   := MSIR.BuildSelect (b, "", isNeg, negN, n);
    ltW    := MSIR.BuildICmp   (b, "", MSIR.CmpPred.Slt, absN, wConst);
    nMasked    := MSIR.BuildIAnd (b, "", n,    wm1);
    absNMasked := MSIR.BuildIAnd (b, "", absN, wm1);
    shlV   := MSIR.BuildIShl  (b, "", x, nMasked);
    lshrV  := MSIR.BuildILShr (b, "", x, absNMasked);
    safeShl := MSIR.BuildSelect (b, "", ltW, shlV,  zero);
    safeShr := MSIR.BuildSelect (b, "", ltW, lshrV, zero);
    RETURN MSIR.BuildSelect (b, "", isNeg, safeShr, safeShl);
  END ShiftMSIR;

PROCEDURE ShiftLeftMSIR (ce: CallExpr.T): MSIR.Value =
  (* LeftShift: 0 <= n < W guaranteed by type; emit shl with safety mask. *)
  VAR
    x   := Expr.CompileMSIR (ce.args[0]);
    n   := Expr.CompileMSIR (ce.args[1]);
    b   := MSIRBuilder.CurrentBlock ();
    wt  := MSIR.TI (Word_types[rep].size);
    wm1, masked : MSIR.Value;
    xb, nb : INTEGER;
  BEGIN
    IF x = NIL OR n = NIL THEN RETURN NIL END;
    xb := MSIR.BitWidth (MSIR.ValueType (x));
    nb := MSIR.BitWidth (MSIR.ValueType (n));
    IF xb > 0 AND xb # Word_types[rep].size THEN
      IF xb > Word_types[rep].size THEN x := MSIR.BuildTrunc (b, "", x, wt)
      ELSE x := MSIR.BuildZExt (b, "", x, wt) END;
    END;
    IF nb > 0 AND nb # Word_types[rep].size THEN
      IF nb > Word_types[rep].size THEN n := MSIR.BuildTrunc (b, "", n, wt)
      ELSE n := MSIR.BuildZExt (b, "", n, wt) END;
    END;
    wm1    := MSIR.ConstInt (wt, Word_types[rep].size - 1);
    masked := MSIR.BuildIAnd (b, "", n, wm1);
    RETURN MSIR.BuildIShl (b, "", x, masked);
  END ShiftLeftMSIR;

PROCEDURE ShiftRightMSIR (ce: CallExpr.T): MSIR.Value =
  (* RightShift: 0 <= n < W guaranteed by type; emit lshr with safety mask. *)
  VAR
    x   := Expr.CompileMSIR (ce.args[0]);
    n   := Expr.CompileMSIR (ce.args[1]);
    b   := MSIRBuilder.CurrentBlock ();
    wt  := MSIR.TI (Word_types[rep].size);
    wm1, masked : MSIR.Value;
    xb, nb : INTEGER;
  BEGIN
    IF x = NIL OR n = NIL THEN RETURN NIL END;
    xb := MSIR.BitWidth (MSIR.ValueType (x));
    nb := MSIR.BitWidth (MSIR.ValueType (n));
    IF xb > 0 AND xb # Word_types[rep].size THEN
      IF xb > Word_types[rep].size THEN x := MSIR.BuildTrunc (b, "", x, wt)
      ELSE x := MSIR.BuildZExt (b, "", x, wt) END;
    END;
    IF nb > 0 AND nb # Word_types[rep].size THEN
      IF nb > Word_types[rep].size THEN n := MSIR.BuildTrunc (b, "", n, wt)
      ELSE n := MSIR.BuildZExt (b, "", n, wt) END;
    END;
    wm1    := MSIR.ConstInt (wt, Word_types[rep].size - 1);
    masked := MSIR.BuildIAnd (b, "", n, wm1);
    RETURN MSIR.BuildILShr (b, "", x, masked);
  END ShiftRightMSIR;

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
    <*ASSERT b*>
    rep := r;
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
    CallExpr.SetMethodMSIR (Z, ShiftMSIR);
    Procedure.DefinePredefined ("Shift", Z, FALSE, t, assignable:=TRUE);
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
    CallExpr.SetMethodMSIR (ZL, ShiftLeftMSIR);
    Procedure.DefinePredefined ("LeftShift", ZL, FALSE, Lt, assignable:=TRUE);
    formalsL := ProcType.Formals (Lt);


    ZR := CallExpr.NewMethodList (2, 2, TRUE, TRUE, TRUE, T,
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
    CallExpr.SetMethodMSIR (ZR, ShiftRightMSIR);
    Procedure.DefinePredefined ("RightShift", ZR, FALSE, Rt, assignable:=TRUE);
    formalsR := ProcType.Formals (Rt);

  END Initialize;

BEGIN
END Shift.
