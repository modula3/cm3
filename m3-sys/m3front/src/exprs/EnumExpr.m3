(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: EnumExpr.m3                                           *)
(* Last modified on Fri Feb 24 16:42:47 PST 1995 by kalsow     *)
(*      modified on Fri Nov 30 21:02:21 1990 by muller         *)

MODULE EnumExpr;

IMPORT M3, CG, Expr, ExprRep, Type, M3Buf, Target, TInt;

TYPE
  P = Expr.T BRANDED "EnumExpr.T" OBJECT
        value: Target.Int;
      OVERRIDES
        typeOf       := ExprRep.NoType;
        repTypeOf    := ExprRep.NoType;
        check        := ExprRep.NoCheck;
        need_addr    := ExprRep.NotAddressable;
        prep         := ExprRep.NoPrep;
        compile      := Compile;
        prepLV       := ExprRep.NotLValue;
        compileLV    := ExprRep.NotLValue;
        prepBR       := ExprRep.PrepNoBranch;
        compileBR    := ExprRep.NoBranch;
        evaluate     := ExprRep.Self;
        isEqual      := EqCheck;
        getBounds    := Bounder;
        isWritable   := ExprRep.IsNever;
        isDesignator := ExprRep.IsNever;
        isZeroes     := IsZeroes;
        genFPLiteral := GenFPLiteral;
        prepLiteral  := ExprRep.NoPrepLiteral;
        genLiteral   := GenLiteral;
        note_write   := ExprRep.NotWritable;
      END;

PROCEDURE New (type: Type.T;  READONLY value: Target.Int): Expr.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    ExprRep.Init (p);
    p.value      := value;
    p.type       := type;
    p.repType    := type;
    p.checked    := TRUE;
    RETURN p;
  END New;

PROCEDURE EqCheck (a: P;  e: Expr.T;  <*UNUSED*> x: M3.EqAssumption): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(b) => RETURN (a.value = b.value);
    ELSE      RETURN FALSE;
    END;
  END EqCheck;

PROCEDURE Compile (p: P) =
  BEGIN
    CG.Load_integer (Target.Integer.cg_type, p.value);
  END Compile;

PROCEDURE Bounder (p: P;  VAR min, max: Target.Int) =
  BEGIN
    min := p.value;
    max := p.value;
  END Bounder;

PROCEDURE Compare (a, b: Expr.T;  VAR sign: INTEGER): BOOLEAN =
  VAR x, y: Target.Int;
  BEGIN
    IF NOT SplitPair (a, b, x, y) THEN RETURN FALSE END;
    IF    TInt.LT (x, y) THEN  sign := -1
    ELSIF TInt.LT (y, x) THEN  sign := 1
    ELSE                           sign := 0
    END;
    RETURN TRUE;
  END Compare;

PROCEDURE Split (e: Expr.T;  VAR i: Target.Int;  VAR t: Type.T): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(p) => i := p.value;  t := Type.Base (p.type);  RETURN TRUE;
    ELSE      RETURN FALSE;
    END;
  END Split;

PROCEDURE SplitPair (a, b: Expr.T;  VAR x, y: Target.Int): BOOLEAN =
  BEGIN
    TYPECASE a OF
    | NULL => RETURN FALSE;
    | P(p) => x := p.value;
    ELSE      RETURN FALSE;
    END;
    TYPECASE b OF
    | NULL => RETURN FALSE;
    | P(p) => y := p.value;
    ELSE      RETURN FALSE;
    END;
    RETURN TRUE;
  END SplitPair;

PROCEDURE IsZeroes (p: P;  <*UNUSED*> lhs: BOOLEAN): BOOLEAN =
  BEGIN
    RETURN TInt.EQ (p.value, TInt.Zero);
  END IsZeroes;

PROCEDURE GenFPLiteral (p: P;  buf: M3Buf.T) =
  BEGIN
    M3Buf.PutText (buf, "ENUM<");
    M3Buf.PutIntt (buf, p.value);
    M3Buf.PutChar (buf, '>');
  END GenFPLiteral;

PROCEDURE GenLiteral (p: P;  offset: INTEGER;  type: Type.T;  is_const: BOOLEAN) =
  VAR info: Type.Info;
  BEGIN
    EVAL Type.CheckInfo (type, info);
    CG.Init_int (offset, info.size, p.value, is_const);
  END GenLiteral;

BEGIN
END EnumExpr.
