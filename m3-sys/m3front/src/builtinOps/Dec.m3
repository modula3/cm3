(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Dec.m3                                                *)
(* Last Modified On Tue May 23 15:31:47 PDT 1995 By kalsow     *)
(*      Modified On Tue Apr  2 03:46:13 1991 By muller         *)

MODULE Dec;

IMPORT CG, CallExpr, Expr, ExprRep, Type, Procedure, Error, Int, LInt, Module;
IMPORT M3ID, Addr, Target, TInt, IntegerExpr, Host, NamedExpr, MSIR, MSIRBuilder, MSIRType;
IMPORT CaptureAnalysis;

VAR Z: CallExpr.MethodList;

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  BEGIN
    DoCheck ("DEC", ce, cs);
  END Check;

PROCEDURE DoCheck (name: TEXT;  ce: CallExpr.T;
                   <*UNUSED*> VAR cs: Expr.CheckState) =
  VAR t: Type.T; e: Expr.T;  nm: M3ID.T;
  BEGIN
    e := ce.args[0];
    t := Type.Base (Expr.TypeOf (e));
    IF NOT Type.IsOrdinal (t) THEN
      IF Type.IsSubtype (t, Addr.T) THEN
        IF Module.IsSafe () THEN Error.Txt (name, "unsafe operation") END;
      ELSE
        Error.Txt (name, "first argument must be of an ordinal type");
      END;
    END;
    IF (NOT Expr.IsDesignator (e)) THEN
      Error.Txt (name, "first argument must be a variable");
    ELSIF (NOT Expr.IsWritable (e, traced := FALSE)) THEN
      Error.Txt (name, "first argument must be writable");
    ELSIF NamedExpr.SplitName (e, nm) THEN
      (* simple scalar => we don't need an explicit address
            -- demanded by Eric Veach 9/17/93 *)
    ELSE
      Expr.NeedsAddress (e);
    END;
    IF (NUMBER (ce.args^) > 1) THEN
      IF Type.IsSubtype (t, LInt.T) THEN
        t := Type.Base (Expr.TypeOf (ce.args[1]));
        IF (t # LInt.T) THEN
          Error.Txt (name, "second argument must be a LONGINT");
        END;
      ELSE
        t := Type.Base (Expr.TypeOf (ce.args[1]));
        IF (t # Int.T) THEN
          Error.Txt (name, "second argument must be an INTEGER");
        END;
      END;
    END;
    ce.type := NIL;
  END DoCheck;

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
    dec    : Expr.T;
    check  : [0..3] := 0;
    lvalue : CG.Val;
    tmin, tmax, bmin, bmax: Target.Int;
    cg_type: CG.Type;
  BEGIN
    tlhs := Type.CheckInfo (tlhs, info);
    IF Type.IsSubtype (tlhs, LInt.T)
      THEN tlhs := LInt.T; cg_type := Target.Longint.cg_type;
      ELSE tlhs := Int.T;  cg_type := Target.Integer.cg_type;
    END;
    EVAL Type.GetBounds (tlhs, tmin, tmax);
    IF (NUMBER (ce.args^) > 1)
      THEN dec := ce.args[1];
      ELSE dec := IntegerExpr.New (tlhs, TInt.One);  Expr.Prep (dec);
    END;
    Expr.GetBounds (lhs, bmin, bmax);

    IF Host.doRangeChk THEN
      IF TInt.LT (tmin, bmin) THEN INC (check) END;
      IF TInt.LT (bmax, tmax) THEN INC (check, 2) END;
    END;

    Expr.CompileLValue (lhs, traced := FALSE);
    lvalue := CG.Pop ();
    CG.Push (lvalue);

    CG.Push (lvalue);
    CG.Load_indirect (info.stk_type, 0, info.size, info.alignment);
    Expr.Compile (dec);

    IF (info.stk_type = CG.Type.Addr)
      THEN CG.Index_bytes (-Target.Byte);  check := 0;
      ELSE CG.Subtract (cg_type);
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
      MSIRBuilder.Abandon ("DEC: cannot get lvalue in MSIR");
      RETURN NIL;
    END;
    mt := MSIRType.Translate (Expr.TypeOf (lhsExpr));
    IF mt = NIL THEN
      MSIRBuilder.Abandon ("DEC: unsupported variable type");
      RETURN NIL;
    END;
    (* AddLocalMSIR widens ordinal allocas to i64 so loop counters don't wrap.
       Use the actual alloca element type for load/store to match. *)
    VAR addrT := MSIR.ValueType (addr);
    BEGIN
      IF MSIR.Kind (addrT) = MSIR.TypeKind.Ptr THEN
        mt := MSIR.EltType (addrT);
      END;
    END;
    old := MSIR.BuildLoad (blk, "", mt, addr);
    IF NUMBER (ce.args^) > 1 THEN
      delta := Expr.CompileMSIR (ce.args[1]);
      IF delta = NIL THEN RETURN NIL END;
      blk := MSIRBuilder.CurrentBlock ();  (* delta may have emitted an invoke *)
      (* Widen delta to match destination type (e.g. IByte arg to INTEGER DEC) *)
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
      (* ADDRESS arithmetic: negate delta, then GepByte *)
      VAR negDelta := MSIR.BuildISub (blk, "", MSIR.ConstInt (MSIR.TI (Target.Integer.size), 0), delta);
      BEGIN
        updated := MSIR.BuildGepByte (blk, "", old, negDelta);
      END;
    ELSE
      updated := MSIR.BuildISub (blk, "", old, delta);
    END;
    MSIR.BuildStore (blk, updated, addr);
    Expr.NoteWrite (lhsExpr);
    RETURN NIL;
  END CompileMSIR;

PROCEDURE Capture (ce: CallExpr.T;  ca: CaptureAnalysis.T) =
  (* DEC writes its first argument; any decrement expression is a read. *)
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
    Procedure.DefinePredefined ("DEC", Z, TRUE);
  END Initialize;

BEGIN
END Dec.
