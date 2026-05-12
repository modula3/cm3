(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: CompareExpr.m3                                        *)
(* Last modified on Fri Feb 24 16:41:37 PST 1995 by kalsow     *)
(*      modified on Thu Nov 29 03:32:44 1990 by muller         *)

MODULE CompareExpr;

IMPORT M3, CG, Expr, ExprRep, Type, Int, LInt, Reel, LReel, EReel;
IMPORT EnumType, SetType, Bool, Module, Addr, Target, TInt;
IMPORT IntegerExpr, EnumExpr, ReelExpr, AddressExpr;
IMPORT SetExpr, Error;
IMPORT MSIR, MSIRBuilder;

CONST
  cINT   = 0;
  cLINT  = 1;
  cREAL  = 2;
  cLONG  = 3;
  cEXTND = 4;
  cADDR  = 5;
  cENUM  = 6;
  cSET   = 7;

CONST
  CGType = ARRAY [cREAL..cADDR] OF CG.Type {
             CG.Type.Reel, CG.Type.LReel, CG.Type.XReel,  CG.Type.Addr };

TYPE
  Op = [ CG.Cmp.GT .. CG.Cmp.LE ];

TYPE
  OpDesc = RECORD signA, signB : INTEGER;  name: TEXT END;

CONST
  Ops = ARRAY Op OF OpDesc {
    (*GT*) OpDesc {  1,  1, "\'>\'" },
    (*GE*) OpDesc {  1,  0, "\'>=\'" },
    (*LT*) OpDesc { -1, -1, "\'<\'" },
    (*LE*) OpDesc { -1,  0, "\'<=\'" }
  };

TYPE
  P = ExprRep.Tabc BRANDED "CompareExpr.P" OBJECT
        op      : Op;
        bad_set : BOOLEAN;
      OVERRIDES
        typeOf       := ExprRep.NoType;
        repTypeOf    := ExprRep.NoType;
        check        := Check;
        need_addr    := ExprRep.NotAddressable;
        prep         := Prep;
        compile      := Compile;
        prepLV       := ExprRep.NotLValue;
        compileLV    := ExprRep.NotLValueBool;
        prepBR       := PrepBR;
        compileBR    := ExprRep.NoBranch;
        evaluate     := Fold;
        isEqual      := EqCheck;
        getBounds    := ExprRep.NoBounds;
        isWritable   := ExprRep.IsNever;
        isDesignator := ExprRep.IsNever;
        isZeroes     := ExprRep.IsNever;
        genFPLiteral := ExprRep.NoFPLiteral;
        prepLiteral  := ExprRep.NoPrepLiteral;
        genLiteral   := ExprRep.NoLiteral;
        note_write   := ExprRep.NotWritable;
        compileMSIR  := CompileMSIR;
      END;

PROCEDURE New (a, b: Expr.T;  op: Op): Expr.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    ExprRep.Init (p);
    p.a       := a;
    p.b       := b;
    p.op      := op;
    p.type    := Bool.T;
    p.repType := Bool.T;
    p.bad_set := FALSE;
    RETURN p;
  END New;

PROCEDURE Check (p: P;  VAR cs: Expr.CheckState) =
  VAR ta, tb, range: Type.T;  info: Type.Info;
  BEGIN
    Expr.TypeCheck (p.a, cs);
    Expr.TypeCheck (p.b, cs);
    ta := Type.Base (Expr.TypeOf (p.a));
    tb := Type.Base (Expr.TypeOf (p.b));
    p.class := cINT;
    IF    (ta = Int.T)   AND (tb = Int.T)    THEN  p.class := cINT;
    ELSIF (ta = LInt.T)  AND (tb = LInt.T)   THEN  p.class := cLINT;
    ELSIF (ta = Reel.T)  AND (tb = Reel.T)   THEN  p.class := cREAL;
    ELSIF (ta = LReel.T) AND (tb = LReel.T)  THEN  p.class := cLONG;
    ELSIF (ta = EReel.T) AND (tb = EReel.T)  THEN  p.class := cEXTND;
    ELSIF (Type.IsSubtype (ta, Addr.T)) AND (Type.IsSubtype (tb, Addr.T)) THEN
      IF Module.IsSafe () THEN Error.Msg ("unsafe operation") END;
      p.class := cADDR;
    ELSIF  NOT Type.IsEqual (ta, tb, NIL)    THEN  Err (p, ta, tb);
    ELSIF EnumType.Is (ta)                   THEN  p.class := cENUM;
    ELSIF SetType.Split (ta, range)          THEN
      p.class := cSET;
      ta := Type.CheckInfo (ta, info);
    ELSE Err (p, ta, tb);
    END;

    IF (p.class = cSET)
      AND ((p.op = CG.Cmp.LT) OR (p.op = CG.Cmp.GT))
      AND (info.size <= Target.Integer.size) THEN
      p.bad_set := TRUE;
    END;
  END Check;

PROCEDURE Err (p: P;  a, b: Type.T) =
  BEGIN
    p.type := Expr.BadOperands (Ops[p.op].name, a, b);
  END Err;

PROCEDURE EqCheck (a: P;  e: Expr.T;  x: M3.EqAssumption): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(b) => RETURN (a.class = b.class)
                 AND (a.op = b.op)
                 AND Expr.IsEqual (a.a, b.a, x)
                 AND Expr.IsEqual (a.b, b.b, x);
    ELSE RETURN FALSE;
    END;
  END EqCheck;

PROCEDURE Prep (p: P) =
  BEGIN
    Expr.Prep (p.a);
    Expr.Prep (p.b);
  END Prep;

PROCEDURE Compile (p: P; <*UNUSED*> StaticOnly: BOOLEAN) =
  VAR type: CG.Type;  ta, tb, tmp: CG.Val;  info: Type.Info;
  BEGIN
    IF (p.class # cSET) THEN
      Expr.Compile (p.a);
      Expr.Compile (p.b);
      IF (p.class = cINT) OR (p.class = cENUM) THEN
        type := Target.Integer.cg_type;
      ELSIF (p.class = cLINT) THEN
        type := Target.Longint.cg_type;
      ELSE
        type := CGType [p.class];
      END;
      CG.Compare (type, p.op);

    ELSIF (p.bad_set) THEN
      Expr.Compile (p.a);  ta := CG.Pop ();
      Expr.Compile (p.b);  tb := CG.Pop ();
      IF (p.op = CG.Cmp.GT) THEN tmp := ta;  ta := tb;  tb := tmp END;
      CG.Push (ta);
      CG.Push (tb);
      CG.Compare (Target.Word.cg_type, CG.Cmp.NE);
      CG.Push (ta);
      CG.Push (tb);
      CG.Or (Target.Word.cg_type);
      CG.Push (tb);
      CG.Compare (Target.Word.cg_type, CG.Cmp.EQ);
      CG.And (Target.Word.cg_type);
      CG.Free (ta);
      CG.Free (tb);

    ELSE (* simple set ops *)
      Expr.Compile (p.a);
      Expr.Compile (p.b);
      EVAL Type.CheckInfo (Expr.TypeOf (p.a), info);
      CG.Set_compare (info.size, p.op);

    END;
  END Compile;

PROCEDURE PrepBR (p: P;  true, false: CG.Label;  freq: CG.Frequency) =
  VAR type: CG.Type;  ta, tb, tmp: CG.Val;
      info: Type.Info;
  BEGIN
    Expr.Prep (p.a);
    Expr.Prep (p.b);
    IF (p.class # cSET) THEN
      Expr.Compile (p.a);
      Expr.Compile (p.b);
      IF (p.class = cINT) OR (p.class = cENUM) THEN
        type := Target.Integer.cg_type;
      ELSIF (p.class = cLINT) THEN
        type := Target.Longint.cg_type;
      ELSE
        type := CGType [p.class];
      END;
      CG.If_then (type, p.op, true, false, freq);

    ELSIF (p.bad_set) THEN
      Expr.Compile (p.a);  ta := CG.Pop ();
      Expr.Compile (p.b);  tb := CG.Pop ();
      IF (p.op = CG.Cmp.GT) THEN tmp := ta;  ta := tb;  tb := tmp END;
      CG.Push (ta);
      CG.Push (tb);
      CG.Compare (Target.Word.cg_type, CG.Cmp.NE);
      CG.Push (ta);
      CG.Push (tb);
      CG.Not (Target.Word.cg_type);
      CG.And (Target.Word.cg_type);
      CG.Load_integer (Target.Word.cg_type, TInt.Zero);
      CG.Compare (Target.Word.cg_type, CG.Cmp.EQ);
      CG.And (Target.Word.cg_type);
      IF (true # CG.No_label)
        THEN CG.If_true  (true,  freq);
        ELSE CG.If_false (false, freq);
      END;
      CG.Free (ta);
      CG.Free (tb);

    ELSE (* simple set ops *)
      Expr.Compile (p.a);
      Expr.Compile (p.b);
      EVAL Type.CheckInfo (Expr.TypeOf (p.a), info);
      CG.Set_compare (info.size, p.op);
      IF (true # CG.No_label)
        THEN CG.If_true  (true, freq);
        ELSE CG.If_false (false, freq);
      END;
    END;
  END PrepBR;

PROCEDURE Fold (p: P): Expr.T =
  VAR e1, e2: Expr.T;  s: INTEGER;
  BEGIN
    e1 := Expr.ConstValue (p.a);
    IF (e1 = NIL) THEN RETURN NIL END;
    e2 := Expr.ConstValue (p.b);
    IF (e2 = NIL) THEN RETURN NIL END;
    IF   IntegerExpr.Compare (e1, e2, s)
      OR EnumExpr.Compare (e1, e2, s)
      OR ReelExpr.Compare (e1, e2, s)
      OR AddressExpr.Compare (e1, e2, s)
      OR SetExpr.Compare (e1, e2, s)
      THEN
      RETURN Bool.Map[(s = Ops[p.op].signA) OR (s = Ops[p.op].signB)];
    END;
    RETURN NIL;
  END Fold;

PROCEDURE CompileMSIR (p: P): MSIR.Value =
  VAR
    lv, rv:   MSIR.Value;
    pred:     MSIR.CmpPred;
    fpred:    MSIR.FCmpPred;
    isFloat   := (p.class = cREAL) OR (p.class = cLONG) OR (p.class = cEXTND);
  BEGIN
    IF isFloat THEN
      CASE p.op OF
      | CG.Cmp.GT => fpred := MSIR.FCmpPred.OGt;
      | CG.Cmp.GE => fpred := MSIR.FCmpPred.OGe;
      | CG.Cmp.LT => fpred := MSIR.FCmpPred.OLt;
      | CG.Cmp.LE => fpred := MSIR.FCmpPred.OLe;
      END;
      lv := Expr.CompileMSIR (p.a);  IF lv = NIL THEN RETURN NIL END;
      rv := Expr.CompileMSIR (p.b);  IF rv = NIL THEN RETURN NIL END;
      RETURN MSIR.BuildFCmp (MSIRBuilder.CurrentBlock (), "", fpred, lv, rv);
    ELSIF (p.class = cINT) OR (p.class = cLINT)
       OR (p.class = cADDR) OR (p.class = cENUM) THEN
      CASE p.op OF
      | CG.Cmp.GT => pred := MSIR.CmpPred.Sgt;
      | CG.Cmp.GE => pred := MSIR.CmpPred.Sge;
      | CG.Cmp.LT => pred := MSIR.CmpPred.Slt;
      | CG.Cmp.LE => pred := MSIR.CmpPred.Sle;
      END;
      lv := Expr.CompileMSIR (p.a);  IF lv = NIL THEN RETURN NIL END;
      rv := Expr.CompileMSIR (p.b);  IF rv = NIL THEN RETURN NIL END;
      RETURN MSIR.BuildICmp (MSIRBuilder.CurrentBlock (), "", pred, lv, rv);
    ELSE
      MSIRBuilder.Abandon ("unsupported comparison class in MSIR");
      RETURN NIL;
    END;
  END CompileMSIR;

BEGIN
END CompareExpr.
