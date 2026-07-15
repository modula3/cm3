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
  (* IN yields a BOOLEAN — widen the i1 membership result to ZType (i64). *)
  VAR r := CompileMSIRRaw (p);
  BEGIN
    IF r # NIL AND MSIR.Kind (MSIR.ValueType (r)) = MSIR.TypeKind.I1 THEN
      r := MSIR.BuildZExt (MSIRBuilder.CurrentBlock (), "", r,
                           MSIR.TI (Target.Integer.size));
    END;
    RETURN r;
  END CompileMSIR;

PROCEDURE CompileMSIRRaw (p: P): MSIR.Value =
  VAR
    set, range              : Type.T;
    b                       : BOOLEAN;
    min, max, emin, emax    : Target.Int;
    info                    : Type.Info;
    minI, maxI              : INTEGER;
    minOrd, maxOrd          : INTEGER;
    needRangeCheck          : BOOLEAN;
    elt, setVal, shifted, bit : MSIR.Value;
    blk                       : MSIR.Block;
    ti                        : MSIR.T;
  BEGIN
    set := Type.Base (Type.CheckInfo (Expr.TypeOf (p.b), info));
    b := SetType.Split (set, range);
    IF NOT b THEN MSIRBuilder.Abandon ("InExpr: SetType.Split failed"); RETURN NIL END;
    b := Type.GetBounds (range, min, max);
    IF NOT b THEN MSIRBuilder.Abandon ("InExpr: GetBounds failed"); RETURN NIL END;
    Expr.GetBounds (p.a, emin, emax);
    needRangeCheck := TInt.LT (emin, min) OR TInt.LT (max, emax);

    IF NOT TInt.ToInt (min, minI) THEN
      MSIRBuilder.Abandon ("InExpr: set domain min out of INTEGER range");
      RETURN NIL;
    END;
    minOrd := minI;
    maxOrd := 0;
    IF needRangeCheck THEN
      IF NOT TInt.ToInt (max, maxI) THEN
        MSIRBuilder.Abandon ("InExpr: set domain max out of INTEGER range");
        RETURN NIL;
      END;
      maxOrd := maxI;
    END;

    ti  := MSIR.TI (info.size);
    blk := MSIRBuilder.CurrentBlock ();
    setVal := Expr.CompileMSIR (p.b);
    IF setVal = NIL THEN RETURN NIL END;
    blk := MSIRBuilder.CurrentBlock ();
    elt := Expr.CompileMSIR (p.a);
    IF elt = NIL THEN RETURN NIL END;
    blk := MSIRBuilder.CurrentBlock ();
    (* Enum-typed element values are integers in representation; retype to TI(N)
       so that arithmetic operations receive consistently-kinded operands.
       RetypeValue emits no instruction — TEnum and TI(N) both lower to iN. *)
    IF MSIR.Kind (MSIR.ValueType (elt)) = MSIR.TypeKind.Enum THEN
      elt := MSIR.RetypeValue (elt, MSIR.TI (MSIR.BitWidth (MSIR.ValueType (elt))));
    END;

    (* Work in the wider of the set type and the element type so that all
       operands to binary instructions have the same type.  setVal is
       zero-extended (set bits stay in the low bits); elt is sign-extended
       so that negative ordinals become large unsigned values and correctly
       fail the range check. *)
    VAR wt := ti;
        wBW := info.size;
    BEGIN
      IF MSIR.BitWidth (MSIR.ValueType (elt)) > info.size THEN
        wt  := MSIR.ValueType (elt);
        wBW := MSIR.BitWidth (wt);
      END;
      IF MSIR.BitWidth (MSIR.ValueType (setVal)) < wBW THEN
        setVal := MSIR.BuildZExt (blk, "", setVal, wt);
      END;
      IF MSIR.BitWidth (MSIR.ValueType (elt)) < wBW THEN
        elt := MSIR.BuildSExt (blk, "", elt, wt);
      END;

      (* adj = elt - minOrd; negative/large elt → large unsigned → range check fails *)
      VAR adj := elt;
      BEGIN
        IF minOrd # 0 THEN
          adj := MSIR.BuildISub (blk, "", elt,
                                 MSIR.ConstInt (wt, minOrd));
        END;

        (* Clamp the shift count so lshr is always defined (a shift amount
           >= the bit width is poison in LLVM).  A bitmask (adj AND wBW-1)
           only works when wBW is a power of two; multi-word sets have
           non-power-of-two widths (e.g. SET OF [0..191] is i192), where
           masking would corrupt valid shift amounts (98 AND 191 = 34).  Use a
           select: an out-of-range adj (unsigned compare, so negative adj maps
           to a huge value too) shifts by 0, and for those the inRange guard
           below zeroes the membership result. *)
        VAR isValid   := MSIR.BuildICmp (blk, "", MSIR.CmpPred.Ult, adj,
                           MSIR.ConstInt (wt, wBW));
            adjMasked := MSIR.BuildSelect (blk, "", isValid, adj,
                           MSIR.ConstZero (wt));
        BEGIN
          shifted := MSIR.BuildILShr (blk, "", setVal, adjMasked);
          bit     := MSIR.BuildIAnd  (blk, "", shifted, MSIR.ConstInt (wt, 1));
          VAR membership := MSIR.BuildICmp (blk, "", MSIR.CmpPred.Ne,
                                            bit, MSIR.ConstZero (wt));
          BEGIN
            IF needRangeCheck THEN
              (* inRange: (adj as unsigned) <= (maxOrd - minOrd) *)
              VAR cardinality := maxOrd - minOrd;
                  inRange     := MSIR.BuildICmp (blk, "", MSIR.CmpPred.Ule, adj,
                                                 MSIR.ConstInt (wt, cardinality));
              BEGIN
                RETURN MSIR.BuildIAnd (blk, "", inRange, membership);
              END;
            END;
            RETURN membership;
          END;
        END;
      END;
    END;
  END CompileMSIRRaw;

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
