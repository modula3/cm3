(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: AddressExpr.m3                                        *)
(* Last modified on Fri Feb 24 16:39:49 PST 1995 by kalsow     *)
(*      modified on Fri Apr 27 07:34:00 1990 by muller         *)

MODULE AddressExpr;
(* ADDRESS *Constants* only. *)

IMPORT M3, CG, Expr, ExprRep, Type, Addr, Null, IntegerExpr;
IMPORT Target, TInt, TWord, M3Buf;

TYPE
  P = Expr.T OBJECT
        value        : Target.Int;
      OVERRIDES
        typeOf       := ExprRep.NoType;
        check        := ExprRep.NoCheck;
        need_addr    := ExprRep.NotAddressable;
        prep         := ExprRep.NoPrep;
        compile      := Compile;
        prepLV       := ExprRep.NotLValue;
        compileLV    := ExprRep.NotLValue;
        prepBR       := ExprRep.NotBoolean;
        compileBR    := ExprRep.NotBoolean;
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

PROCEDURE New (READONLY value: Target.Int): Expr.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    ExprRep.Init (p);
    p.value   := value;
    p.checked := TRUE;
    IF TInt.EQ (value, TInt.Zero)
      THEN p.type := Null.T;
      ELSE p.type := Addr.T;
    END;
    RETURN p;
  END New;

PROCEDURE Split (e: Expr.T;  VAR value: Target.Int): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(p) => value := p.value;  RETURN TRUE;
    ELSE      RETURN FALSE;
    END;
  END Split;

PROCEDURE Add (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR i, j: Target.Int;  t: Type.T;
  BEGIN
    IF NOT IntegerExpr.Split (b, i, t) THEN RETURN FALSE END;
    TYPECASE a OF
    | NULL => RETURN FALSE;
    | P(p) =>
      TWord.Add (p.value, i, j);
      EVAL TInt.Extend (j, Target.Address.bytes, j);
      c := New (j);
      RETURN TRUE;
    ELSE      RETURN FALSE;
    END;
  END Add;

PROCEDURE Subtract (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR i, j, k: Target.Int;  t: Type.T;
  BEGIN
    TYPECASE a OF
    | NULL => RETURN FALSE;
    | P(p) => i := p.value;
    ELSE      RETURN FALSE;
    END;

    IF IntegerExpr.Split (b, j, t) THEN
      TWord.Subtract (i, j, k);
      EVAL TInt.Extend (k, Target.Address.bytes, k);
      c := New (k);
    ELSE (* address - address *)
      TYPECASE b OF
      | NULL => RETURN FALSE;
      | P(p) =>
        TWord.Subtract (i, p.value, k);
        EVAL TInt.Extend (k, Target.Address.bytes, k);
        c := IntegerExpr.New (t, k);
      ELSE      RETURN FALSE;
      END;
    END;
    RETURN TRUE;
  END Subtract;

PROCEDURE Compare (a, b: Expr.T;  VAR sign: INTEGER): BOOLEAN =
  VAR x, y: Target.Int;
  BEGIN
    IF  NOT SplitPair (a, b, x, y) THEN RETURN FALSE END;
    TWord.And (x, Target.Address.max, x);
    TWord.And (y, Target.Address.max, y);
    IF TWord.LT (x, y) THEN
      sign := -1
    ELSIF TWord.LT (y, x) THEN
      sign := 1
    ELSE
      sign := 0
    END;
    RETURN TRUE;
  END Compare;

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

PROCEDURE EqCheck (a: P;  e: Expr.T;  <*UNUSED*> x: M3.EqAssumption): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(b) => RETURN (a.value = b.value);
    ELSE      RETURN FALSE;
    END;
  END EqCheck;

PROCEDURE Compile (p: P) =
  VAR val: INTEGER;  b := TInt.ToInt (p.value, val);
  BEGIN
    <*ASSERT b*>
    CG.Load_nil ();
    IF (val # 0) THEN CG.Add_offset (Target.Byte * val) END;
  END Compile;

PROCEDURE Bounder (p: P;   VAR min, max: Target.Int) =
  BEGIN
    min := p.value;
    max := p.value;
  END Bounder;

PROCEDURE IsZeroes (p: P;  <*UNUSED*> lhs: BOOLEAN): BOOLEAN =
  BEGIN
    RETURN p = Null.Nil OR TInt.EQ (p.value, TInt.Zero);
  END IsZeroes;

PROCEDURE GenFPLiteral (p: P;  buf: M3Buf.T) =
  BEGIN
    M3Buf.PutText (buf, "ADDR<");
    M3Buf.PutIntt (buf, p.value);
    M3Buf.PutChar (buf, '>');
  END GenFPLiteral;

PROCEDURE GenLiteral (p: P;  offset: INTEGER;  <*UNUSED*> type: Type.T;
                      is_const: BOOLEAN) =
  BEGIN
    IF NOT TInt.EQ (p.value, TInt.Zero) THEN
      CG.Init_int (offset, MIN (Target.Integer.size, Target.Address.size),
                   p.value, is_const);
    END;
  END GenLiteral;

BEGIN
END AddressExpr.
