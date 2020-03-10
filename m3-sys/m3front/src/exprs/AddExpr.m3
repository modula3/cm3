(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: AddExpr.m3                                            *)
(* Last modified on Fri Feb 24 11:49:03 PST 1995 by kalsow     *)
(*      modified on Thu Nov 29 03:30:20 1990 by muller         *)

MODULE AddExpr;

IMPORT CG, Expr, ExprRep, Type, Int, LInt, Reel, LReel, EReel;
IMPORT SetType, Addr, Module, Error, Target, ErrType, TInt;
IMPORT AddressExpr, SetExpr, IntegerExpr, ReelExpr;

TYPE
  Class = { cINT, cLINT, cREAL, cLONG, cEXTND, cADDR, cSET };

CONST
  FPType = ARRAY [Class.cREAL .. Class.cEXTND] OF CG.AType {
             CG.Type.Reel, CG.Type.LReel, CG.Type.XReel };

TYPE
  P = ExprRep.Tab BRANDED "AddExpr.P" OBJECT
        class   : Class;
        tmp     : CG.Var;
      OVERRIDES
        typeOf       := TypeOf;
        repTypeOf    := RepTypeOf;
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
    p.a := a;
    p.b := b;
    p.tmp := NIL;
    RETURN p;
  END New;

PROCEDURE TypeOf (p: P): Type.T =
  VAR ta: Type.T;
  BEGIN
    ta := Expr.TypeOf (p.a);
    ta := Type.Check (ta);
    IF Type.IsSubtype (ta, Addr.T) THEN ta := Addr.T END;
    RETURN Type.Base (ta);
  END TypeOf;

PROCEDURE RepTypeOf (p: P): Type.T =
  VAR ta: Type.T;
  BEGIN
    ta := Expr.RepTypeOf (p.a);
    ta := Type.Check (ta);
    IF Type.IsSubtype (ta, Addr.T) THEN ta := Addr.T END;
    RETURN Type.Base (ta);
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
    ELSIF (ta = LInt.T)  AND (tb = LInt.T) THEN
      p.class := Class.cLINT
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
      IF  NOT Type.IsEqual (ta, tb, NIL) THEN
        ta := Expr.BadOperands ("\'+\'", ta, tb);
      END;
    ELSIF Type.IsSubtype (ta, Addr.T) AND (tb = Int.T) THEN
      p.class := Class.cADDR;  ta := Addr.T;
      IF Module.IsSafe () THEN Error.Msg ("unsafe \'+\'"); END;
    ELSE
      ta := Expr.BadOperands ("\'+\'", ta, tb);
    END;
    p.type := ta;
    p.repType := ta;
  END Check;

PROCEDURE Prep (p: P) =
  VAR info: Type.Info;
  BEGIN
    Expr.Prep (p.a);
    Expr.Prep (p.b);
    IF (p.class = Class.cSET) THEN
      EVAL Type.CheckInfo (p.type, info);
      IF (info.size > Target.Word.size) THEN
        p.tmp := CG.Declare_temp (info.size, Target.Word.align,
                                  CG.Type.Struct, in_memory := TRUE);
        CG.Load_addr_of (p.tmp, 0, Target.Word.align);
        CG.ForceStacked ();
        Expr.Compile (p.a);
        Expr.Compile (p.b);
        CG.Set_union (info.size);
      END;
    END;
  END Prep;

PROCEDURE Compile (p: P) =
  VAR info: Type.Info;
  BEGIN
    CASE p.class OF
    | Class.cSET =>
        EVAL Type.CheckInfo (p.type, info);
        IF (info.size > Target.Word.size) THEN
          CG.Load_addr_of_temp (p.tmp, 0, Target.Word.align);
          p.tmp := NIL;
        ELSE
          Expr.Compile (p.a);
          Expr.Compile (p.b);
          CG.Set_union (info.size);
        END;
    | Class.cADDR =>
        Expr.Compile (p.a);
        Expr.Compile (p.b);
        CG.Index_bytes (Target.Byte);
    | Class.cINT =>
        Expr.Compile (p.a);
        Expr.Compile (p.b);
        CG.Add (Target.Integer.cg_type);
    | Class.cLINT =>
        Expr.Compile (p.a);
        Expr.Compile (p.b);
        CG.Add (Target.Longint.cg_type);
    | Class.cREAL, Class.cLONG, Class.cEXTND =>
        Expr.Compile (p.a);
        Expr.Compile (p.b);
        CG.Add (FPType[p.class]);
    END;
  END Compile;

PROCEDURE Fold (p: P): Expr.T =
  VAR e1, e2, e3: Expr.T;
  BEGIN
    e1 := Expr.ConstValue (p.a);
    e2 := Expr.ConstValue (p.b);
    e3 := NIL;
    IF    (e1 = NIL) OR (e2 = NIL)     THEN
    ELSIF IntegerExpr.Add (e1, e2, e3) THEN
    ELSIF ReelExpr.Add    (e1, e2, e3) THEN
    ELSIF AddressExpr.Add (e1, e2, e3) THEN
    ELSIF SetExpr.Union   (e1, e2, e3) THEN
    END;
    RETURN e3;
  END Fold;

PROCEDURE GetBounds (p: P;  VAR min, max: Target.Int) =
  VAR min_a, max_a, min_b, max_b, smin, smax: Target.Int;
  BEGIN
    EVAL Type.GetBounds (p.type, min, max);
    Expr.GetBounds (p.a, min_a, max_a);
    Expr.GetBounds (p.b, min_b, max_b);
    IF TInt.Add (min_a, min_b, smin)
      AND NOT TInt.LT (smin, min) AND NOT TInt.LT (max, smin)
      AND TInt.Add (max_a, max_b, smax)
      AND NOT TInt.LT (smax, min) AND NOT TInt.LT (max, smax) THEN
      IF TInt.LT (min, smin) THEN min := smin END;
      IF TInt.LT (smax, max) THEN max := smax END;
    END;
  END GetBounds;

BEGIN
END AddExpr.
