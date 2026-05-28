(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Inc.m3                                                *)
(* Last Modified On Tue May 23 15:31:58 PDT 1995 By kalsow     *)
(*      Modified On Tue Apr  2 03:47:06 1991 By muller         *)

MODULE Inc;

IMPORT CG, CallExpr, Expr, Type, Procedure, Dec, Target, TInt;
IMPORT IntegerExpr, Host, Int, LInt, MSIR, MSIRBuilder, MSIRType;
IMPORT CaptureAnalysis;

VAR Z: CallExpr.MethodList;

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  BEGIN
    Dec.DoCheck ("INC", ce, cs);
  END Check;

PROCEDURE Prep (ce: CallExpr.T) =
  BEGIN
    Expr.PrepLValue (ce.args[0], traced := FALSE);
    IF (NUMBER (ce.args^) > 1) THEN Expr.Prep (ce.args[1]); END;
  END Prep;

PROCEDURE Compile (ce: CallExpr.T) =
  VAR
    lhs    := ce.args[0];
    tlhs   := Expr.TypeOf (lhs);
    info   : Type.Info;
    inc    : Expr.T;
    check  : [0..3] := 0;
    lvalue : CG.Val;
    bmin, bmax: Target.Int;
    cg_type: CG.Type;
  BEGIN
    tlhs := Type.CheckInfo (tlhs, info);
    IF Type.IsSubtype (tlhs, LInt.T)
      THEN tlhs := LInt.T; cg_type := Target.Longint.cg_type;
      ELSE tlhs := Int.T;  cg_type := Target.Integer.cg_type;
    END;
    IF (NUMBER (ce.args^) > 1)
      THEN inc := ce.args[1];
    ELSIF tlhs = LInt.T
      THEN inc := IntegerExpr.New (LInt.T, TInt.One);  Expr.Prep (inc);
      ELSE inc := IntegerExpr.New (Int.T,  TInt.One);  Expr.Prep (inc);
    END;
    Expr.GetBounds (lhs, bmin, bmax);

    IF Host.doRangeChk THEN
      IF tlhs = LInt.T THEN
        IF TInt.LT (Target.Longint.min, bmin) THEN INC (check) END;
        IF TInt.LT (bmax, Target.Longint.max) THEN INC (check, 2) END;
      ELSE
        IF TInt.LT (Target.Integer.min, bmin) THEN INC (check) END;
        IF TInt.LT (bmax, Target.Integer.max) THEN INC (check, 2) END;
      END;
    END;

    Expr.CompileLValue (lhs, traced := FALSE);
    lvalue := CG.Pop ();
    CG.Push (lvalue);

    CG.Push (lvalue);
    CG.Load_indirect (info.stk_type, 0, info.size, info.alignment);
    Expr.Compile (inc);

    IF (info.stk_type = CG.Type.Addr)
      THEN CG.Index_bytes (Target.Byte);  check := 0;
      ELSE CG.Add (cg_type);
    END;

    CASE check OF
    | 0 => (* no range checking *)
    | 1 => CG.Check_lo (cg_type, bmin, CG.RuntimeError.ValueOutOfRange);
    | 2 => CG.Check_hi (cg_type, bmax, CG.RuntimeError.ValueOutOfRange);
    | 3 => CG.Check_range (cg_type, bmin, bmax,
                           CG.RuntimeError.ValueOutOfRange);
    END;

    CG.Store_indirect (info.stk_type, 0, info.size);
    CG.Free (lvalue);
    Expr.NoteWrite (lhs);
  END Compile;

PROCEDURE CompileMSIR (ce: CallExpr.T): MSIR.Value =
  VAR
    lhsExpr := ce.args[0];
    addr    := Expr.LValueMSIR (lhsExpr);
    mt      : MSIR.T;
    old, delta, updated : MSIR.Value;
    blk     := MSIRBuilder.CurrentBlock ();
  BEGIN
    IF NOT MSIRBuilder.InProc () THEN RETURN NIL END;
    IF addr = NIL THEN
      MSIRBuilder.Abandon ("INC: cannot get lvalue in MSIR");
      RETURN NIL;
    END;
    mt := MSIRType.Translate (Expr.TypeOf (lhsExpr));
    IF mt = NIL THEN
      MSIRBuilder.Abandon ("INC: unsupported variable type");
      RETURN NIL;
    END;
    old := MSIR.BuildLoad (blk, "", mt, addr);
    IF NUMBER (ce.args^) > 1 THEN
      delta := Expr.CompileMSIR (ce.args[1]);
      IF delta = NIL THEN RETURN NIL END;
      blk := MSIRBuilder.CurrentBlock ();  (* delta may have emitted an invoke *)
      (* Widen delta to match destination type (e.g. IByte arg to INTEGER INC) *)
      IF MSIR.Kind (mt) # MSIR.TypeKind.Ptr THEN
        VAR mtBits := MSIR.BitWidth (mt);
            dBits  := MSIR.BitWidth (MSIR.ValueType (delta));
        BEGIN
          IF mtBits > 0 AND dBits > 0 AND mtBits # dBits THEN
            IF dBits < mtBits
              THEN delta := MSIR.BuildZExt (blk, "", delta, mt)
              ELSE delta := MSIR.BuildTrunc (blk, "", delta, mt)
            END
          END
        END
      END;
    ELSIF MSIR.Kind (mt) = MSIR.TypeKind.Ptr THEN
      delta := MSIR.ConstInt (MSIR.TI (Target.Integer.size), 1);
    ELSE
      delta := MSIR.ConstInt (MSIR.ValueType (old), 1);
    END;
    IF MSIR.Kind (mt) = MSIR.TypeKind.Ptr THEN
      (* ADDRESS arithmetic: getelementptr i8 — delta is byte count *)
      updated := MSIR.BuildGepByte (blk, "", old, delta);
    ELSE
      updated := MSIR.BuildIAdd (blk, "", old, delta);
    END;
    MSIR.BuildStore (blk, updated, addr);
    Expr.NoteWrite (lhsExpr);
    RETURN NIL;
  END CompileMSIR;

PROCEDURE Capture (ce: CallExpr.T;  ca: CaptureAnalysis.T) =
  (* INC writes its first argument; any increment expression is a read. *)
  BEGIN
    Expr.CaptureLV (ce.args[0], ca);
    FOR i := 1 TO LAST (ce.args^) DO Expr.Capture (ce.args[i], ca) END;
  END Capture;

PROCEDURE Initialize () =
  BEGIN
    Z := CallExpr.NewMethodList (1, 2, FALSE, FALSE, TRUE, NIL,
                                 NIL, NIL,
                                 CallExpr.NotAddressable,
                                 Check,
                                 Prep,
                                 Compile,
                                 CallExpr.NoLValue,
                                 CallExpr.NoLValue,
                                 CallExpr.NotBoolean,
                                 CallExpr.NotBoolean,
                                 CallExpr.NoValue,
                                 CallExpr.NoBounds,
                                 CallExpr.IsNever, (* writable *)
                                 CallExpr.IsNever, (* designator *)
                                 CallExpr.NotWritable (* noteWriter *));
    CallExpr.SetMethodMSIR    (Z, CompileMSIR);
    CallExpr.SetMethodCapture (Z, Capture);
    Procedure.DefinePredefined ("INC", Z, TRUE);
  END Initialize;

BEGIN
END Inc.
