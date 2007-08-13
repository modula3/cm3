(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Inc.m3                                                *)
(* Last Modified On Tue May 23 15:31:58 PDT 1995 By kalsow     *)
(*      Modified On Tue Apr  2 03:47:06 1991 By muller         *)

MODULE Inc;

IMPORT CG, CallExpr, Expr, Type, Procedure, Dec, Target, TInt;
IMPORT IntegerExpr, Host, LInt;

VAR Z: CallExpr.MethodList;

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  BEGIN
    Dec.DoCheck ("INC", ce, cs);
  END Check;

PROCEDURE Prep (ce: CallExpr.T) =
  BEGIN
    Expr.PrepLValue (ce.args[0], lhs := TRUE);
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
    min, max, bmin, bmax, imin, imax: Target.Int;
  BEGIN
    tlhs := Type.CheckInfo (tlhs, info);
    IF (NUMBER (ce.args^) > 1)
      THEN inc := ce.args[1];
      ELSE
        IF Type.IsSubtype (tlhs, LInt.T)
          THEN inc := IntegerExpr.New (TInt.OneL);
          ELSE inc := IntegerExpr.New (TInt.OneI);
        END;
        Expr.Prep (inc);
    END;
    Expr.GetBounds (lhs, bmin, bmax);
    Expr.GetBounds (inc, imin, imax);

    IF Host.doRangeChk THEN
      IF Type.IsSubtype (tlhs, LInt.T) THEN
        min := Target.Int {Target.Longint.min, Target.Pre.Longint};
        max := Target.Int {Target.Longint.max, Target.Pre.Longint};
      ELSE        
        min := Target.Int {Target.Integer.min, Target.Pre.Integer};
        max := Target.Int {Target.Integer.max, Target.Pre.Integer};
      END;
      IF NOT TInt.EQ (bmin, min) AND TInt.Sig (imin) < 0 THEN INC (check) END;
      IF NOT TInt.EQ (bmax, max) AND TInt.Sig (imax) > 0 THEN INC (check, 2) END;
    END;

    Expr.CompileLValue (lhs);
    lvalue := CG.Pop ();
    CG.Push (lvalue);

    CG.Push (lvalue);
    CG.Load_indirect (info.stk_type, 0, info.size);
    Expr.Compile (inc);

    IF (info.stk_type = CG.Type.Addr)
      THEN CG.Index_bytes (Target.Byte);  check := 0;
      ELSE CG.Add (info.stk_type);
    END;

    CASE check OF
    | 0 => (* no range checking *)
    | 1 => CG.Check_lo (bmin, CG.RuntimeError.ValueOutOfRange);
    | 2 => CG.Check_hi (bmax, CG.RuntimeError.ValueOutOfRange);
    | 3 => CG.Check_range (bmin, bmax, CG.RuntimeError.ValueOutOfRange);
    END;

    CG.Store_indirect (info.stk_type, 0, info.size);
    CG.Free (lvalue);
    Expr.NoteWrite (lhs);
  END Compile;

PROCEDURE Initialize () =
  BEGIN
    Z := CallExpr.NewMethodList (1, 2, FALSE, FALSE, TRUE, NIL,
                                 NIL,
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
    Procedure.Define ("INC", Z, TRUE);
  END Initialize;

BEGIN
END Inc.
