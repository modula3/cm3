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
        pre : Precision;
        val : Target.Int;
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

VAR cache: ARRAY Precision OF ARRAY [-7 .. 64] OF P;

PROCEDURE New (READONLY value: Target.Int): Expr.T =
  VAR p: P;  n: INTEGER;  pre := TInt.Prec (value);
  BEGIN
    IF TInt.ToInt (value, n)
      AND (FIRST (cache[pre]) <= n) AND (n <= LAST (cache[pre])) THEN
      p := cache[pre, n];
      IF (p # NIL) THEN RETURN p; END;
    END;
    p := NEW (P);
    ExprRep.Init (p);
    p.val := value;
    CASE pre OF
    | Precision.Integer => p.type := Int.T;
    | Precision.Longint => p.type := LInt.T;
    END;
    p.checked := TRUE;
    IF TInt.ToInt (value, n)
      AND (FIRST (cache[pre]) <= n) AND (n <= LAST (cache[pre])) THEN
      cache[pre][n] := p;
    END;
    RETURN p;
  END New;

PROCEDURE EqCheck (a: P;  e: Expr.T;  <*UNUSED*> x: M3.EqAssumption): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(b) => RETURN TInt.EQ (a.val, b.val);
    ELSE      RETURN FALSE;
    END;
  END EqCheck;

PROCEDURE Compile (p: P) =
  BEGIN
    CG.Load_integer (p.val);
  END Compile;

PROCEDURE Bounder (p: P;  VAR min, max: Target.Int) =
  BEGIN
    min := p.val;
    max := p.val;
  END Bounder;

PROCEDURE Compare (a, b: Expr.T;  VAR sign: INTEGER): BOOLEAN =
  VAR x, y: Target.Int;
  BEGIN
    IF NOT SplitPair (a, b, x, y) THEN RETURN FALSE END;
    IF    TInt.LT (x, y) THEN  sign := -1
    ELSIF TInt.LT (y, x) THEN  sign := +1
    ELSE                       sign :=  0
    END;
    RETURN TRUE;
  END Compare;

PROCEDURE Add (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR x, y, res: Target.Int;
  BEGIN
    IF NOT SplitPair (a, b, x, y) THEN RETURN FALSE END;
    IF NOT TInt.Add (x, y, res) THEN RETURN FALSE END;
    c := New (res);
    RETURN TRUE;
  END Add;

PROCEDURE Subtract (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR x, y, res: Target.Int;
  BEGIN
    IF NOT SplitPair (a, b, x, y) THEN RETURN FALSE END;
    IF NOT TInt.Subtract (x, y, res) THEN RETURN FALSE END;
    c := New (res);
    RETURN TRUE;
  END Subtract;

PROCEDURE Multiply (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR x, y, res: Target.Int;
  BEGIN
    IF NOT SplitPair (a, b, x, y) THEN RETURN FALSE END;
    IF NOT TInt.Multiply (x, y, res) THEN RETURN FALSE END;
    c := New (res);
    RETURN TRUE;
  END Multiply;

PROCEDURE Div (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR x, y, res: Target.Int;
  BEGIN
    IF NOT SplitPair (a, b, x, y) THEN RETURN FALSE END;
    IF TInt.Sig (y) = 0 THEN
      Error.Msg ("attempt to DIV by 0");
      RETURN FALSE;
    END;
    IF NOT TInt.Div (x, y, res) THEN RETURN FALSE END;
    c := New (res);
    RETURN TRUE;
  END Div;

PROCEDURE Mod (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR x, y, res: Target.Int;
  BEGIN
    IF NOT SplitPair (a, b, x, y) THEN RETURN FALSE END;
    IF TInt.Sig (y) = 0 THEN
      Error.Msg ("attempt to MOD by 0");
      RETURN FALSE;
    END;
    IF NOT TInt.Mod (x, y, res) THEN RETURN FALSE END;
    c := New (res);
    RETURN TRUE;
  END Mod;

PROCEDURE Negate (a: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR res: Target.Int;
  BEGIN
    TYPECASE a OF
    | NULL => RETURN FALSE;
    | P(p) => IF NOT TInt.Negate (p.val, res) THEN
                RETURN FALSE;
              END;
              c := New (res);  RETURN TRUE;
    ELSE      RETURN FALSE;
    END;
  END Negate;

PROCEDURE SplitPair (a, b: Expr.T;  VAR x, y: Target.Int): BOOLEAN =
  BEGIN
    TYPECASE a OF
    | NULL => RETURN FALSE;
    | P(p) => x := p.val;
    ELSE      RETURN FALSE;
    END;
    TYPECASE b OF
    | NULL => RETURN FALSE;
    | P(p) => y := p.val; RETURN TRUE;
    ELSE      RETURN FALSE;
    END;
  END SplitPair;

PROCEDURE Split (e: Expr.T;  VAR value: Target.Int): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(p) => value := p.val; RETURN TRUE;
    ELSE      RETURN FALSE;
    END;
  END Split;

PROCEDURE IsZeroes (p: P;  <*UNUSED*> lhs: BOOLEAN): BOOLEAN =
  BEGIN
    RETURN TInt.Sig (p.val) = 0;
  END IsZeroes;

PROCEDURE GenFPLiteral (p: P;  buf: M3Buf.T) =
  CONST mark = ARRAY Precision OF TEXT { "INT<", "LONGINT<" };
  BEGIN
    M3Buf.PutText (buf, mark [p.pre]);
    M3Buf.PutIntt (buf, p.val);
    M3Buf.PutChar (buf, '>');
  END GenFPLiteral;

PROCEDURE GenLiteral (p: P;  offset: INTEGER;  type: Type.T;  is_const: BOOLEAN) =
  VAR info: Type.Info;
  BEGIN
    EVAL Type.CheckInfo (type, info);
    CG.Init_int (offset, info.size, p.val, is_const);
  END GenLiteral;

BEGIN
END IntegerExpr.
