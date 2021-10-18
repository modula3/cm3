(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: NegateExpr.m3                                         *)
(* Last modified on Fri Jul  8 09:48:43 PDT 1994 by kalsow     *)
(*      modified on Thu Nov 29 03:29:51 1990 by muller         *)

MODULE NegateExpr;

IMPORT CG, Expr, ExprRep, Type, Int, LInt, Reel, LReel, EReel;
IMPORT IntegerExpr, ReelExpr;

TYPE
  P = ExprRep.Ta BRANDED "NegateExpr.P" OBJECT
        folded : Expr.T;
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
        isEqual      := ExprRep.EqCheckA;
        getBounds    := ExprRep.NoBounds;
        isWritable   := ExprRep.IsNever;
        isDesignator := ExprRep.IsNever;
        isZeroes     := ExprRep.IsNever;
        genFPLiteral := ExprRep.NoFPLiteral;
        prepLiteral  := ExprRep.NoPrepLiteral;
        genLiteral   := ExprRep.NoLiteral;
        note_write   := ExprRep.NotWritable;
      END;

PROCEDURE New (a: Expr.T): Expr.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    ExprRep.Init (p);
    p.a := a;
    p.folded := NIL;
    RETURN p;
  END New;

PROCEDURE TypeOf (p: P): Type.T =
  BEGIN
    RETURN Type.Base (Expr.TypeOf (p.a));
  END TypeOf;

PROCEDURE RepTypeOf (p: P): Type.T =
  BEGIN
    RETURN Type.Base (Expr.RepTypeOf (p.a));
  END RepTypeOf;

PROCEDURE Check (p: P;  VAR cs: Expr.CheckState) =
  VAR t: Type.T;
  BEGIN
    Expr.TypeCheck (p.a, cs);
    t := Type.Base (Expr.TypeOf (p.a));
    IF (t = Int.T) OR (t = LInt.T)
      OR (t = Reel.T) OR (t = LReel.T) OR (t = EReel.T) THEN
      (* ok *)
(*    ELSIF SetType.split (t, range) THEN (* OK *) *)
    ELSE
      t := Expr.BadOperands ("unary \'-\'", t);
    END;
    p.type := t;
  END Check;

PROCEDURE Prep (p: P) =
  VAR e := Fold (p);
  BEGIN
    IF (e = NIL)
      THEN Expr.Prep (p.a);
      ELSE Expr.Prep (e);
    END;
  END Prep;

PROCEDURE Compile (p: P) =
  VAR e := p.folded;
  BEGIN
    IF (e = NIL) THEN
      Expr.Compile (p.a);
      CG.Negate (Type.CGType (p.type));
    ELSE
      Expr.Compile (e); (* e is folded p, not p.a, thus already negated. *)
    END;
  END Compile;

PROCEDURE Fold (p: P): Expr.T =
  VAR e1, e3: Expr.T;
  BEGIN
    IF (p.folded # NIL) THEN RETURN p.folded END;
    e1 := Expr.ConstValue (p.a);
    e3 := NIL;
    IF    (e1 = NIL)                  THEN
    ELSIF IntegerExpr.Negate (e1, e3) THEN
    ELSIF ReelExpr.Negate    (e1, e3) THEN
    END;
    p.folded := e3;
    RETURN e3;
  END Fold;

BEGIN
END NegateExpr.
