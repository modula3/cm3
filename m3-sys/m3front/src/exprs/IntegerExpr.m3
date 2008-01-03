(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: IntegerExpr.m3                                        *)
(* Last modified on Fri Feb 24 16:43:29 PST 1995 by kalsow     *)
(*      modified on Tue Apr 10 22:35:24 1990 by muller         *)

MODULE IntegerExpr;

IMPORT M3, CG, Expr, ExprRep, Type, Int, LInt, Error, M3Buf, Target, TInt;

TYPE
  P = Expr.T BRANDED "IntegerExpr.T" OBJECT
        value: Target.Int;
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

VAR cache := ARRAY BOOLEAN, [-7 .. 64] OF P {ARRAY [-7 .. 64] OF P{NIL, ..},..};

PROCEDURE New (type: Type.T;  READONLY value: Target.Int): Expr.T =
  VAR p: P;  n: INTEGER;  v: Target.Int;  t := type = LInt.T;
  BEGIN
    IF TInt.ToInt (value, n)
      AND (FIRST (cache[t]) <= n) AND (n <= LAST (cache[t])) THEN
      p := cache[t][n];
      IF (p # NIL) THEN RETURN p; END;
    END;
    IF type = Int.T THEN
      IF NOT TInt.IntI (value, Target.Integer.bytes, v) THEN RETURN NIL END;
    ELSIF type = LInt.T THEN
      IF NOT TInt.IntI (value, Target.Longint.bytes, v) THEN RETURN NIL END;
    ELSE RETURN NIL END;
    p := NEW (P);
    ExprRep.Init (p);
    p.value   := v;
    p.type    := type;
    p.checked := TRUE;
    IF TInt.ToInt (value, n)
      AND (FIRST (cache[t]) <= n) AND (n <= LAST (cache[t])) THEN
      cache[t][n] := p;
    END;
    RETURN p;
  END New;

PROCEDURE EqCheck (a: P;  e: Expr.T;  <*UNUSED*> x: M3.EqAssumption): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(b) => RETURN TInt.EQ (a.value, b.value);
    ELSE      RETURN FALSE;
    END;
  END EqCheck;

PROCEDURE Compile (p: P) =
  BEGIN
    CG.Load_integer (Type.CGType (p.type), p.value);
  END Compile;

PROCEDURE Bounder (p: P;  VAR min, max: Target.Int) =
  BEGIN
    min := p.value;
    max := p.value;
  END Bounder;

PROCEDURE Compare (a, b: Expr.T;  VAR sign: INTEGER): BOOLEAN =
  VAR x, y: Target.Int; t: Type.T;
  BEGIN
    IF NOT SplitPair (a, b, x, y, t) THEN RETURN FALSE END;
    IF    TInt.LT (x, y) THEN  sign := -1
    ELSIF TInt.LT (y, x) THEN  sign := +1
    ELSE                       sign :=  0
    END;
    RETURN TRUE;
  END Compare;

PROCEDURE Add (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR x, y, res: Target.Int;  t: Type.T;
  BEGIN
    IF NOT SplitPair (a, b, x, y, t) THEN RETURN FALSE END;
    IF NOT TInt.Add (x, y, res) THEN RETURN FALSE END;
    c := New (t, res);
    RETURN TRUE;
  END Add;

PROCEDURE Subtract (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR x, y, res: Target.Int;  t: Type.T;
  BEGIN
    IF NOT SplitPair (a, b, x, y, t) THEN RETURN FALSE END;
    IF NOT TInt.Subtract (x, y, res) THEN RETURN FALSE END;
    c := New (t, res);
    RETURN TRUE;
  END Subtract;

PROCEDURE Multiply (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR x, y, res: Target.Int;  t: Type.T;
  BEGIN
    IF NOT SplitPair (a, b, x, y, t) THEN RETURN FALSE END;
    IF NOT TInt.Multiply (x, y, res) THEN RETURN FALSE END;
    c := New (t, res);
    RETURN TRUE;
  END Multiply;

PROCEDURE Div (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR x, y, res: Target.Int;  t: Type.T;
  BEGIN
    IF NOT SplitPair (a, b, x, y, t) THEN RETURN FALSE END;
    IF TInt.EQ (y, TInt.Zero) THEN
      Error.Msg ("attempt to DIV by 0");
      RETURN FALSE;
    END;
    IF NOT TInt.Div (x, y, res) THEN RETURN FALSE END;
    c := New (t, res);
    RETURN TRUE;
  END Div;

PROCEDURE Mod (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR x, y, res: Target.Int;  t: Type.T;
  BEGIN
    IF NOT SplitPair (a, b, x, y, t) THEN RETURN FALSE END;
    IF TInt.EQ (y, TInt.Zero) THEN
      Error.Msg ("attempt to MOD by 0");
      RETURN FALSE;
    END;
    IF NOT TInt.Mod (x, y, res) THEN RETURN FALSE END;
    c := New (t, res);
    RETURN TRUE;
  END Mod;

PROCEDURE Negate (a: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR res: Target.Int;
  BEGIN
    TYPECASE a OF
    | NULL => RETURN FALSE;
    | P(p) => IF NOT TInt.Subtract (TInt.Zero, p.value, res) THEN
                RETURN FALSE;
              END;
              c := New (p.type, res);  RETURN TRUE;
    ELSE      RETURN FALSE;
    END;
  END Negate;

PROCEDURE Abs (a: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR res: Target.Int;
  BEGIN
    TYPECASE a OF
    | NULL => RETURN FALSE;
    | P(p) => IF TInt.LE (TInt.Zero, p.value) THEN
                c := a;  RETURN TRUE;
              END;
              IF NOT TInt.Subtract (TInt.Zero, p.value, res) THEN
                RETURN FALSE;
              END;
              c := New (p.type, res);  RETURN TRUE;
    ELSE      RETURN FALSE;
    END;
  END Abs;

PROCEDURE ToInt (a: Expr.T;  VAR i: INTEGER): BOOLEAN =
  BEGIN
    TYPECASE a OF
    | NULL => RETURN FALSE;
    | P(p) => IF NOT TInt.ToInt (p.value, i) THEN
                RETURN FALSE;
              END;
              RETURN TRUE;
    ELSE      RETURN FALSE;
    END;
  END ToInt;

PROCEDURE SplitPair (a, b: Expr.T;  VAR x, y: Target.Int;  VAR t: Type.T):
  BOOLEAN =
  BEGIN
    TYPECASE a OF
    | NULL => RETURN FALSE;
    | P(p) => x := p.value; t := p.type;
    ELSE      RETURN FALSE;
    END;
    TYPECASE b OF
    | NULL => RETURN FALSE;
    | P(p) => y := p.value; RETURN t = p.type;
    ELSE      RETURN FALSE;
    END;
  END SplitPair;

PROCEDURE Split (e: Expr.T;  VAR value: Target.Int;  VAR t: Type.T): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(p) => value := p.value; t := p.type;  RETURN TRUE;
    ELSE      RETURN FALSE;
    END;
  END Split;

PROCEDURE IsZeroes (p: P;  <*UNUSED*> lhs: BOOLEAN): BOOLEAN =
  BEGIN
    RETURN TInt.EQ (p.value, TInt.Zero);
  END IsZeroes;

PROCEDURE GenFPLiteral (p: P;  buf: M3Buf.T) =
  BEGIN
    IF    p.type = Int.T  THEN M3Buf.PutText (buf, "INT<");
    ELSIF p.type = LInt.T THEN M3Buf.PutText (buf, "LINT<");
    ELSE <*ASSERT FALSE*> END;
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
END IntegerExpr.
