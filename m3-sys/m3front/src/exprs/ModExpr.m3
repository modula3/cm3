(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: ModExpr.m3                                            *)
(* Last modified on Thu Dec 15 14:01:22 PST 1994 by kalsow     *)
(*      modified on Thu Nov 29 03:31:49 1990 by muller         *)

MODULE ModExpr;

IMPORT CG, Expr, ExprRep, Type, Int, LInt, IntegerExpr, Target;
IMPORT Reel, LReel, EReel, ReelExpr, DivExpr, TInt;

TYPE
  Class = { cINT, cLINT, cREAL, cLONG, cEXTND, cERR };

CONST
  CGType = ARRAY [Class.cREAL .. Class.cEXTND] OF CG.Type {
             CG.Type.Reel, CG.Type.LReel, CG.Type.XReel};

TYPE
  P = ExprRep.Tab BRANDED "ModExpr.P" OBJECT
        class: Class;
        tmp1 : CG.Var;
        tmp2 : CG.Var;
      OVERRIDES
        typeOf       := ExprRep.NoType;
        repTypeOf    := ExprRep.NoType;
        check        := Check;
        need_addr    := ExprRep.NotAddressable;
        prep         := Prep;
        compile      := Compile;
        prepLV       := ExprRep.NotLValue;
        compileLV    := ExprRep.NotLValue;
        prepBR       := ExprRep.NotBoolean;
        compileBR    := ExprRep.NotBoolean;
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

PROCEDURE New (a, b: Expr.T): Expr.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    ExprRep.Init (p);
    p.a       := a;
    p.b       := b;
    p.type    := Int.T (*May be changed by Check.*);
    p.repType := Int.T (*May be changed by Check.*);
    p.tmp1    := NIL;
    p.tmp2    := NIL;
    RETURN p;
  END New;

PROCEDURE Check (p: P;  VAR cs: Expr.CheckState) =
  VAR ta, tb: Type.T;
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
    ELSE p.class := Class.cERR;  ta := Int.T;
      ta := Expr.BadOperands ("MOD", ta, tb);
    END;
    p.type := ta;
    p.repType := ta;
  END Check;

PROCEDURE Prep (p: P) =
  VAR cg_type: CG.Type;  sz, align: INTEGER;  info: Type.Info;
  BEGIN
    Expr.Prep (p.a);
    Expr.Prep (p.b);
    IF (p.class # Class.cINT) AND (p.class # Class.cLINT) THEN
      (* floating point: x MOD y == x - y * FLOOR (x / y)  *)
      EVAL Type.CheckInfo (p.a.type, info);
      sz := info.size;
      align := info.alignment;
      cg_type := CGType [p.class];

      Expr.Compile (p.a);
      p.tmp1 := CG.Declare_temp (sz, align, cg_type, in_memory := FALSE);
      CG.Store (p.tmp1, 0, sz, align, cg_type);

      Expr.Compile (p.b);
      p.tmp2 := CG.Declare_temp (sz, align, cg_type, in_memory := FALSE);
      CG.Store (p.tmp2, 0, sz, align, cg_type);
    END;
  END Prep;

PROCEDURE Compile (p: P) =
  VAR
    cg_type: CG.Type;
    info: Type.Info;
    sz, align: INTEGER;
    e1, e2, e3: Expr.T;
    divisor, mask: Target.Int;
    log: INTEGER;
    t: Type.T;
  BEGIN
    IF (p.class = Class.cINT) OR (p.class = Class.cLINT) THEN
      cg_type := Type.CGType (p.type);
      e1 := Expr.ConstValue (p.a);
      e2 := Expr.ConstValue (p.b);
      e3 := NIL;
      IF (e1 # NIL) AND (e2 # NIL) AND IntegerExpr.Mod (e1, e2, e3) THEN
        Expr.Compile (e3);
      ELSIF (e2 # NIL)
        AND IntegerExpr.Split (e2, divisor, t)
        AND DivExpr.SmallPowerOfTwo (divisor, log) THEN
        IF (e1 = NIL) THEN e1 := p.a; END;
        IF (log = 0) THEN
          (* mod 1 => zero *)
          Expr.Compile (e1);
          CG.Discard (cg_type);
          CG.Load_integer (cg_type, TInt.Zero);
        ELSE
          EVAL TInt.Subtract (divisor, TInt.One, mask);
          Expr.Compile (e1);
          CG.Load_integer (cg_type, mask);
          CG.And (cg_type);
        END;
      ELSE
        IF (e1 = NIL) THEN e1 := p.a; END;
        IF (e2 = NIL) THEN e2 := p.b; END;
        Expr.Compile (e1);
        Expr.Compile (e2);
        CG.Mod (cg_type, Expr.GetSign (e1), Expr.GetSign (e2));
      END;
    ELSE
      (* floating point: x MOD y == x - y * FLOOR (x / y)  *)
      EVAL Type.CheckInfo (p.a.type, info);
      sz := info.size;
      align := info.alignment;
      cg_type := CGType [p.class];

      CG.Load (p.tmp1, 0, sz, align, info.addr_align, cg_type);
      CG.Load (p.tmp2, 0, sz, align, info.addr_align, cg_type);
      CG.Divide (cg_type);
      CG.Cvt_int (cg_type, Target.Integer.cg_type, CG.Cvt.Floor);
      CG.Cvt_float (Target.Integer.cg_type, cg_type);
      CG.Load (p.tmp2, 0, sz, align, info.addr_align, cg_type);
      CG.Multiply (cg_type);
      CG.Load (p.tmp1, 0, sz, align, info.addr_align, cg_type);
      CG.Swap ();
      CG.Subtract (cg_type);

      CG.Free_temp (p.tmp1);
      CG.Free_temp (p.tmp2);
      p.tmp1 := NIL;
      p.tmp2 := NIL;
    END;
  END Compile;

PROCEDURE Fold (p: P): Expr.T =
  VAR e1, e2, e3: Expr.T;
  BEGIN
    e1 := Expr.ConstValue (p.a);
    e2 := Expr.ConstValue (p.b);
    e3 := NIL;
    IF    (e1 = NIL) OR (e2 = NIL)     THEN
    ELSIF IntegerExpr.Mod (e1, e2, e3) THEN
    ELSIF ReelExpr.Mod    (e1, e2, e3) THEN
    END;
    RETURN e3;
  END Fold;

PROCEDURE GetBounds (p: P;  VAR min, max: Target.Int) =
  VAR tmin, tmax, min_b, max_b: Target.Int;
  BEGIN
    IF (p.class = Class.cINT) OR (p.class = Class.cLINT) THEN
      EVAL Type.GetBounds (p.type, tmin, tmax);
      Expr.GetBounds (p.b, min_b, max_b);
      IF TInt.LT (min_b, TInt.Zero) OR TInt.LT (max_b, TInt.Zero) THEN
        ExprRep.NoBounds (p, min, max);
      ELSE
        min := TInt.Zero;
        IF NOT TInt.Subtract (max_b, TInt.One, max)
          OR TInt.LT (max, tmin) OR TInt.LT (tmax, max) THEN
          max := tmax;
        END;
      END;
    ELSE
      ExprRep.NoBounds (p, min, max);
    END;
  END GetBounds;

BEGIN
END ModExpr.

