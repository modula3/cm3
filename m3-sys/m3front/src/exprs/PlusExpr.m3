(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: PlusExpr.m3                                           *)
(* Last modified on Fri Feb 24 16:45:09 PST 1995 by kalsow     *)
(*      modified on Thu Nov 29 03:29:06 1990 by muller         *)

(* Unary "+" operator. *)

MODULE PlusExpr;

IMPORT M3, Expr, ExprRep, Type, Int, Reel, LReel, EReel;

TYPE
  P = ExprRep.Ta BRANDED "PlusExpr.P" OBJECT
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
        isEqual      := EqCheck;
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
    p.type := Expr.TypeOf (p.a);
    t := Type.Base (p.type);
    IF (t # Int.T) AND (t # Reel.T) AND (t # LReel.T) AND (t # EReel.T) THEN
      p.type := Expr.BadOperands ("unary \'+\'", t);
    END;
  END Check;

PROCEDURE EqCheck (a: P;  e: Expr.T;  x: M3.EqAssumption): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(b) => RETURN Expr.IsEqual (a.a, b.a, x);
    ELSE      RETURN Expr.IsEqual (a.a, e, x);
    END;
  END EqCheck;

PROCEDURE Prep (p: P) =
  BEGIN
    Expr.Prep (p.a);
  END Prep;

PROCEDURE Compile (p: P) =
  BEGIN
    Expr.Compile (p.a);
  END Compile;

PROCEDURE Fold (p: P): Expr.T =
  BEGIN
    RETURN Expr.ConstValue (p.a);
  END Fold;

BEGIN
END PlusExpr.
