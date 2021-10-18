(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: SubtractExpr.m3                                       *)
(* Last modified on Fri Feb 24 11:45:21 PST 1995 by kalsow     *)
(*      modified on Thu Nov 29 23:25:28 1990 by muller         *)

MODULE SubtractExpr;

IMPORT CG, Expr, ExprRep, Type, Error, LInt, Int, Reel, EnumType;
IMPORT SetType, Addr, Module, AddressExpr, Target, EnumExpr;
IMPORT IntegerExpr, ReelExpr, SetExpr, LReel, EReel, TInt, ErrType;

TYPE
  Class = { cINT, cLINT, cREAL, cLONG, cEXTND, cADDR, cSET, cENUM };

CONST
  FPType = ARRAY [Class.cREAL .. Class.cEXTND] OF CG.Type {
             CG.Type.Reel,  CG.Type.LReel, CG.Type.XReel };

TYPE
  P = ExprRep.Tab BRANDED "SubtractExpr.P" OBJECT
        extended : BOOLEAN; (* => can subtract enum values *)
        class    : Class;
        tmp      : CG.Var;
      OVERRIDES
        typeOf       := TypeOf;
        repTypeOf    := RepTypeOf;
        check        := Check;
        need_addr    := ExprRep.NotAddressable;
        prep         := Prep;
        compile      := Compile;
        prepLV       := ExprRep.NotLValue;
        compileLV    := ExprRep.NotLValue;
        prepBR       := ExprRep.PrepNoBranch;
        compileBR    := ExprRep.NoBranch;
        evaluate     := Fold;
        isEqual      := ExprRep.EqCheckAB;
        getBounds    := GetBounds;
        isWritable   := ExprRep.IsNever;
        isDesignator := ExprRep.IsNever;
        isZeroes     := ExprRep.IsNever;
        genFPLiteral := ExprRep.NoFPLiteral;
        prepLiteral  := ExprRep.NoPrepLiteral;
        genLiteral   := ExprRep.NoLiteral;
        note_write   := ExprRep.NotWritable;
      END;

PROCEDURE New (a, b: Expr.T; extended := FALSE): Expr.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    ExprRep.Init (p);
    p.a := a;
    p.b := b;
    p.extended := extended;
    p.tmp := NIL;
    RETURN p;
  END New;

PROCEDURE TypeOf (p: P): Type.T =
  VAR ta: Type.T;
  BEGIN
    ta := Type.Base (Expr.TypeOf (p.a));
    IF (p.extended) AND EnumType.Is (ta) THEN
      ta := Int.T;
    ELSIF Type.IsSubtype (ta, Addr.T) AND
      Type.IsSubtype (Type.Base (Expr.TypeOf (p.b)), Addr.T) THEN
      ta := Int.T;
    END;
    RETURN ta;
  END TypeOf;

PROCEDURE RepTypeOf (p: P): Type.T =
  VAR ta: Type.T;
  BEGIN
    ta := Type.Base (Expr.RepTypeOf (p.a));
    IF (p.extended) AND EnumType.Is (ta) THEN
      ta := Int.T;
    ELSIF Type.IsSubtype (ta, Addr.T) AND
      Type.IsSubtype (Type.Base (Expr.RepTypeOf (p.b)), Addr.T) THEN
      ta := Int.T;
    END;
    RETURN ta;
  END RepTypeOf;

PROCEDURE Check (p: P;  VAR cs: Expr.CheckState) =
  VAR ta, tb, range: Type.T;
  BEGIN
    Expr.TypeCheck (p.a, cs);
    Expr.TypeCheck (p.b, cs);
    ta := Type.Base (Expr.TypeOf (p.a));
    tb := Type.Base (Expr.TypeOf (p.b));
    IF    (ta = Int.T)   AND (tb = Int.T)   THEN
      p.class := Class.cINT;
    ELSIF (ta = LInt.T)  AND (tb = LInt.T)  THEN
      p.class := Class.cLINT;
    ELSIF (ta = Reel.T)  AND (tb = Reel.T)  THEN
      p.class := Class.cREAL;
    ELSIF (ta = LReel.T) AND (tb = LReel.T) THEN
      p.class := Class.cLONG;
    ELSIF (ta = EReel.T) AND (tb = EReel.T) THEN
      p.class := Class.cEXTND;
    ELSIF (ta = ErrType.T) OR (tb = ErrType.T) THEN
      p.class := Class.cINT; (* there's already an error *)
      ta := ErrType.T;
    ELSIF SetType.Split (ta, range) THEN
      p.class := Class.cSET;
      IF NOT Type.IsEqual (ta, tb, NIL) THEN
        ta := Expr.BadOperands ("\'-\'", ta, tb);
      END;
    ELSIF EnumType.Is (ta) THEN
      p.class := Class.cENUM;
      IF (p.extended) AND (tb = Int.T)
        THEN ta := Int.T;
        ELSE ta := Expr.BadOperands ("\'-\'", ta, tb);
      END;
    ELSIF Type.IsSubtype (ta, Addr.T) THEN
      p.class := Class.cADDR;
      IF    (tb = Int.T)                THEN ta := Addr.T;
      ELSIF Type.IsSubtype (tb, Addr.T) THEN ta := Int.T;
      ELSE  ta := Expr.BadOperands ("\'-\'", ta, tb);
      END;
      IF Module.IsSafe () THEN Error.Msg ("unsafe \'-\'") END;
    ELSE
      ta := Expr.BadOperands ("\'-\'", ta, tb);
      p.class := Class.cINT;
    END;
    p.type := ta;
  END Check;

PROCEDURE Prep (p: P) =
  VAR info: Type.Info;
  BEGIN
    Expr.Prep (p.a);
    Expr.Prep (p.b);
    IF (p.class = Class.cSET) THEN
      EVAL Type.CheckInfo (p.type, info);
      IF (info.size > Target.Integer.size) THEN
        p.tmp := CG.Declare_temp (info.size, Target.Integer.align,
                                  CG.Type.Struct, in_memory := TRUE);
        CG.Load_addr_of (p.tmp, 0, Target.Integer.align);
        CG.ForceStacked ();
        Expr.Compile (p.a);
        Expr.Compile (p.b);
        CG.Set_difference (info.size);
      END;
    END;
  END Prep;

PROCEDURE Compile (p: P) =
  VAR size: INTEGER;  info: Type.Info;
  BEGIN
    CASE p.class OF
    | Class.cINT, Class.cENUM =>
        Expr.Compile (p.a);
        Expr.Compile (p.b);
        CG.Subtract (Target.Integer.cg_type);
    | Class.cLINT =>
        Expr.Compile (p.a);
        Expr.Compile (p.b);
        CG.Subtract (Target.Longint.cg_type);
    | Class.cREAL, Class.cLONG, Class.cEXTND =>
        Expr.Compile (p.a);
        Expr.Compile (p.b);
        CG.Subtract (FPType [p.class]);
    | Class.cSET  =>
        EVAL Type.CheckInfo (p.type, info);
        size := info.size;
        IF (size > Target.Integer.size) THEN
          (* Prep did most of the work *)
          CG.Load_addr_of_temp (p.tmp, 0, Target.Integer.align);
          p.tmp := NIL;
        ELSE
          Expr.Compile (p.a);
          Expr.Compile (p.b);
          CG.Set_difference (size);
        END;
    | Class.cADDR =>
        IF (p.type = Addr.T) THEN (* Addr - Int *)
          Expr.Compile (p.a);
          Expr.Compile (p.b);
          CG.Index_bytes (-Target.Byte);
        ELSE (* Addr - Addr *)
          Expr.Compile (p.a);
          CG.Loophole (CG.Type.Addr, Target.Word.cg_type);
          Expr.Compile (p.b);
          CG.Loophole (CG.Type.Addr, Target.Word.cg_type);
          CG.Subtract (Target.Word.cg_type);
        END;
    END;
  END Compile;

PROCEDURE Fold (p: P): Expr.T =
  VAR e1, e2, e3: Expr.T;  x1, x2, x3: Target.Int;  t1: Type.T;
  BEGIN
    e1 := Expr.ConstValue (p.a);
    e2 := Expr.ConstValue (p.b);
    e3 := NIL;
    IF    (e1 = NIL) OR (e2 = NIL)          THEN
    ELSIF IntegerExpr.Subtract (e1, e2, e3) THEN
    ELSIF (p.extended)
      AND EnumExpr.Split (e1, x1, t1)
      AND IntegerExpr.Split (e2, x2, t1)
      AND TInt.Subtract (x1, x2, x3)
      AND NOT TInt.LT (x3, Target.Integer.min)
      AND NOT TInt.LT (Target.Integer.max, x3) THEN
      e3 := IntegerExpr.New (t1, x3);
    ELSIF ReelExpr.Subtract    (e1, e2, e3) THEN
    ELSIF AddressExpr.Subtract (e1, e2, e3) THEN
    ELSIF SetExpr.Difference   (e1, e2, e3) THEN
    END;
    RETURN e3;
  END Fold;

PROCEDURE GetBounds (p: P;  VAR min, max: Target.Int) =
  VAR min_a, max_a, min_b, max_b, smin, smax: Target.Int;
  BEGIN
    EVAL Type.GetBounds (p.type, min, max);
    Expr.GetBounds (p.a, min_a, max_a);
    Expr.GetBounds (p.b, min_b, max_b);
    IF TInt.Subtract (min_a, max_b, smin)
      AND NOT TInt.LT (smin, min) AND NOT TInt.LT (max, smin)
      AND TInt.Subtract (max_a, min_b, smax)
      AND NOT TInt.LT (smax, min) AND NOT TInt.LT (max, smax) THEN
      IF TInt.LT (min, smin) THEN min := smin END;
      IF TInt.LT (smax, max) THEN max := smax END;
    END;
  END GetBounds;

BEGIN
END SubtractExpr.
