(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: InExpr.m3                                             *)
(* Last modified on Fri Jul  8 09:48:45 PDT 1994 by kalsow     *)
(*      modified on Thu Nov 29 03:31:28 1990 by muller         *)

MODULE InExpr;

IMPORT CG, Expr, ExprRep, Error, Type, SetType, Bool, SetExpr;
IMPORT Target, TInt, Value;
IMPORT MSIR, MSIRBuilder;

TYPE
  P = ExprRep.Tab BRANDED "InExpr.P" OBJECT
        tmp: CG.Val;
      OVERRIDES
        typeOf       := ExprRep.NoType;
        repTypeOf    := ExprRep.NoType;
        check        := Check;
        need_addr    := ExprRep.NotAddressable;
        prep         := Prep;
        compile      := Compile;
        prepLV       := ExprRep.NotLValue;
        compileLV    := ExprRep.NotLValueBool;
        prepBR       := ExprRep.PrepNoBranch;
        compileBR    := ExprRep.NoBranch;
        evaluate     := Fold;
        isEqual      := ExprRep.EqCheckAB;
        getBounds    := ExprRep.NoBounds;
        isWritable   := ExprRep.IsNever;
        isDesignator := ExprRep.IsNever;
        isZeroes     := ExprRep.IsNever;
        genFPLiteral := ExprRep.NoFPLiteral;
        prepLiteral  := ExprRep.NoPrepLiteral;
        genLiteral   := ExprRep.NoLiteral;
        note_write   := ExprRep.NotWritable;
        exprAlign    := ExprRep.ExprBoolAlign;
        compileMSIR  := CompileMSIR;
      END;

PROCEDURE New (a, b: Expr.T): Expr.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    ExprRep.Init (p);
    p.a       := a;
    p.b       := b;
    p.type    := Bool.T;
    p.repType := Bool.T;
    p.tmp     := NIL;
    RETURN p;
  END New;

PROCEDURE Check (p: P;  VAR cs: Expr.CheckState) =
  VAR ta, tb, tc: Type.T;
  BEGIN
    Expr.TypeCheck (p.a, cs);
    Expr.TypeCheck (p.b, cs);
    ta := Type.Base (Expr.TypeOf (p.a));
    tb := Type.Base (Expr.TypeOf (p.b));
    IF SetType.Split (tb, tc) AND Type.IsSubtype (ta, Type.Base (tc)) THEN
      (*ok *)
    ELSE
      p.type := Expr.BadOperands ("IN", ta, tb);
    END;
  END Check;

PROCEDURE Prep (p: P) =
  VAR
    set, range: Type.T;
    b: BOOLEAN;
    min, max, emin, emax, n_elts: Target.Int;
    skip: CG.Label;
    index: CG.Val;
    info: Type.Info;
    cg_type: CG.Type;
  BEGIN
    set := Type.Base (Type.CheckInfo (Expr.TypeOf (p.b), info));
    b := SetType.Split (set, range);  <*ASSERT b*>
    b := Type.GetBounds (range, min, max);  <*ASSERT b*>
    Expr.GetBounds (p.a, emin, emax);
    cg_type := Type.CGType (range);

    Expr.Prep (p.a);
    Expr.Prep (p.b);

    IF TInt.LT (emin, min) OR TInt.LT (max, emax) THEN
      (* we need to range check a *)
      IF NOT TInt.Subtract (max, min, n_elts)
        OR TInt.LT (Target.Integer.max, n_elts) THEN
        Error.Msg ("set too large");
      END;
      Expr.Compile (p.a);
      IF NOT TInt.EQ (min, TInt.Zero) THEN
        CG.Load_integer (cg_type, min);
        CG.Subtract (cg_type);
      END;
      index := CG.Pop ();
      Value.Load (Bool.False);
      p.tmp := CG.Pop_temp ();
      CG.Push (index);
      CG.Loophole (cg_type, Target.Word.cg_type);
      CG.Load_integer (Target.Word.cg_type, n_elts);
      skip := CG.Next_label ();
      CG.If_compare (Target.Word.cg_type, CG.Cmp.GT, skip, CG.Never);
      Expr.Compile (p.b);
      CG.Push (index);
      CG.Set_member (info.size);
      CG.Store_temp (p.tmp);
      CG.Set_label (skip);
      CG.Free (index);
    END;
  END Prep;

PROCEDURE Compile (p: P; StaticOnly: BOOLEAN) =
  VAR
    set, range: Type.T;
    b: BOOLEAN;
    min, max, emin, emax: Target.Int;
    info: Type.Info;
    cg_type: CG.Type;
  BEGIN
    <* ASSERT NOT StaticOnly *>
    set := Type.Base (Type.CheckInfo (Expr.TypeOf (p.b), info));
    b := SetType.Split (set, range);  <*ASSERT b*>
    b := Type.GetBounds (range, min, max);  <*ASSERT b*>
    Expr.GetBounds (p.a, emin, emax);

    IF TInt.LT (emin, min) OR TInt.LT (max, emax) THEN
      (* we need to range check a *)
      CG.Push (p.tmp);
      CG.Free (p.tmp);
      p.tmp := NIL;
    ELSE
      (* no range checking is needed *)
      Expr.Compile (p.b);
      Expr.Compile (p.a);
      cg_type := Type.CGType (range);
      IF NOT TInt.EQ (min, TInt.Zero) THEN
        CG.Load_integer (cg_type, min);
        CG.Subtract (cg_type);
      END;
      CG.Loophole (cg_type, Target.Integer.cg_type);
      CG.Set_member (info.size);
    END;
  END Compile;

PROCEDURE CompileMSIR (p: P): MSIR.Value =
  VAR
    set, range            : Type.T;
    b                     : BOOLEAN;
    min, max, emin, emax  : Target.Int;
    info                  : Type.Info;
    mask                  : LONGINT;
    minOrd                : INTEGER;
    elt, maskVal, shifted, bit : MSIR.Value;
    blk                        : MSIR.Block;
    ti64                       : MSIR.T;
  BEGIN
    set := Type.Base (Type.CheckInfo (Expr.TypeOf (p.b), info));
    IF info.size > Target.Word.size THEN
      MSIRBuilder.Abandon ("InExpr: multi-word set not supported");
      RETURN NIL;
    END;
    b := SetType.Split (set, range);
    IF NOT b THEN MSIRBuilder.Abandon ("InExpr: SetType.Split failed"); RETURN NIL END;
    b := Type.GetBounds (range, min, max);
    IF NOT b THEN MSIRBuilder.Abandon ("InExpr: GetBounds failed"); RETURN NIL END;
    Expr.GetBounds (p.a, emin, emax);

    IF NOT SetExpr.GetWordBitMask (p.b, minOrd, mask) THEN
      MSIRBuilder.Abandon ("InExpr: non-constant or large set");
      RETURN NIL;
    END;
    IF TInt.LT (emin, min) OR TInt.LT (max, emax) THEN
      MSIRBuilder.Abandon ("InExpr: element needs range check");
      RETURN NIL;
    END;

    blk  := MSIRBuilder.CurrentBlock ();
    elt  := Expr.CompileMSIR (p.a);
    IF elt = NIL THEN RETURN NIL END;
    blk  := MSIRBuilder.CurrentBlock ();
    ti64 := MSIR.TI (64);

    (* Zero-extend element to i64 if narrower. *)
    IF MSIR.BitWidth (MSIR.ValueType (elt)) < 64 THEN
      elt := MSIR.BuildZExt (blk, "in.ext", elt, ti64);
    END;

    maskVal := MSIR.ConstInt (ti64, mask);

    IF minOrd # 0 THEN
      elt := MSIR.BuildISub (blk, "in.adj", elt,
                             MSIR.ConstInt (ti64, VAL (minOrd, LONGINT)));
    END;

    shifted := MSIR.BuildILShr (blk, "in.shr", maskVal, elt);
    bit     := MSIR.BuildIAnd  (blk, "in.bit", shifted, MSIR.ConstInt (ti64, 1L));
    RETURN MSIR.BuildICmp (blk, "in.cmp", MSIR.CmpPred.Ne,
                           bit, MSIR.ConstZero (ti64));
  END CompileMSIR;

PROCEDURE Fold (p: P): Expr.T =
  VAR e1, e2, e3: Expr.T;
  BEGIN
    e1 := Expr.ConstValue (p.b);
    e2 := Expr.ConstValue (p.a);
    e3 := NIL;
    IF (e1 = NIL) OR (e2 = NIL) THEN
    ELSIF SetExpr.Member (e1, e2, e3) THEN
    END;
    RETURN e3;
  END Fold;

BEGIN
END InExpr.
