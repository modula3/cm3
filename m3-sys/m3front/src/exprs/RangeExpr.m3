(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: RangeExpr.m3                                          *)
(* Last modified on Wed Aug 11 08:51:57 PDT 1993 by kalsow     *)
(*      modified on Fri Dec 21 01:22:54 1990 by muller         *)

MODULE RangeExpr;

IMPORT Expr, ExprRep, Type, Int, EnumType;
IMPORT EnumExpr, IntegerExpr, Error, Target, M3Buf;

TYPE
  P = ExprRep.Tab BRANDED "RangeExpr.P" OBJECT
      OVERRIDES
        typeOf       := TypeOf;
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
        getBounds    := Bounder;
        isWritable   := ExprRep.IsNever;
        isDesignator := ExprRep.IsNever;
	isZeroes     := IsZeroes;
	genFPLiteral := GenFPLiteral;
	prepLiteral  := ExprRep.NoPrepLiteral;
	genLiteral   := ExprRep.NoLiteral;
        note_write   := ExprRep.NotWritable;
      END;

PROCEDURE New (a, b: Expr.T): Expr.T =
  VAR p := NEW (P);
  BEGIN
    ExprRep.Init (p);
    p.a    := a;
    p.b    := b;
    RETURN p;
  END New;

PROCEDURE Split (e: Expr.T;  VAR min, max: Expr.T): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(p) => min := p.a;  max := p.b;  RETURN TRUE;
    ELSE      RETURN FALSE;
    END;
  END Split;

PROCEDURE TypeOf (p: P): Type.T =
  BEGIN
    RETURN Type.Base (Expr.TypeOf (p.a));
  END TypeOf;

PROCEDURE Check (p: P;  VAR cs: Expr.CheckState) =
  VAR ta, tb: Type.T;
  BEGIN
    Expr.TypeCheck (p.a, cs);
    Expr.TypeCheck (p.b, cs);
    ta := Type.Base (Expr.TypeOf (p.a));
    tb := Type.Base (Expr.TypeOf (p.b));
    IF (ta = Int.T) AND (tb = Int.T) THEN (* ok *)
    ELSIF EnumType.Is (ta) AND Type.IsEqual (ta, tb, NIL) THEN (* ok *)
    ELSE  Error.Msg ("incompatible types for \'..\'");
    END;
    p.type := ta;
  END Check;

PROCEDURE Prep (p: P) =
  BEGIN
    Expr.Prep (p.a);
    Expr.Prep (p.b);
  END Prep;

PROCEDURE Compile (<*UNUSED*> p: P) =
  BEGIN
    Error.Msg ("INTERNAL ERROR: cannot compile a range expression");
    <* ASSERT FALSE *>
  END Compile;

PROCEDURE Bounder (p: P;  VAR min, max: Target.Int) =
  VAR e: Expr.T;  i: Target.Int;  t: Type.T;
  BEGIN
    EVAL Type.GetBounds (p.type, min, max);
    e := Expr.ConstValue (p.a);
    IF (e = NIL) THEN (* can't improve lower bound *)
    ELSIF IntegerExpr.Split (e, i) THEN  min := i;
    ELSIF EnumExpr.Split (e, i, t) THEN  min := i;
    END;
    e := Expr.ConstValue (p.a);
    IF (e = NIL) THEN (* can't improve upper bound *)
    ELSIF IntegerExpr.Split (e, i) THEN  max := i;
    ELSIF EnumExpr.Split (e, i, t) THEN  max := i;
    END;
  END Bounder;

PROCEDURE Fold (p: P): Expr.T =
  VAR e1, e2: Expr.T;
  BEGIN
    e1 := Expr.ConstValue (p.a);
    IF (e1 = NIL) THEN RETURN NIL END;
    e2 := Expr.ConstValue (p.b);
    IF (e2 = NIL) THEN RETURN NIL END;
    IF (e1 = p.a) AND (e2 = p.b)
      THEN RETURN p;
      ELSE RETURN New (e1, e2);
    END;
  END Fold;

PROCEDURE GenFPLiteral (p: P;  buf: M3Buf.T) =
  BEGIN
    M3Buf.PutText (buf, "RANGE<");
    Expr.GenFPLiteral (p.a, buf);
    M3Buf.PutChar (buf, ',');
    Expr.GenFPLiteral (p.b, buf);
    M3Buf.PutChar (buf, '>');
  END GenFPLiteral;

PROCEDURE IsZeroes (p: P): BOOLEAN =
  BEGIN
    RETURN Expr.IsZeroes (p.a) AND Expr.IsZeroes (p.b);
  END IsZeroes;

BEGIN
END RangeExpr.
