(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ReelExpr.m3                                           *)
(* Last modified on Fri Feb 24 16:46:45 PST 1995 by kalsow     *)
(*      modified on Tue Apr 10 22:38:17 1990 by muller         *)

MODULE ReelExpr;

IMPORT M3, CG, Expr, ExprRep, Type, Target, TFloat;
IMPORT M3Buf, Reel, LReel, EReel, IntegerExpr;

TYPE
  P = Expr.T OBJECT
        pre : Precision;
        val : Target.Float;
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
        getBounds    := ExprRep.NoBounds;
        isWritable   := ExprRep.IsNever;
        isDesignator := ExprRep.IsNever;
	isZeroes     := IsZeroes;
	genFPLiteral := GenFPLiteral;
	prepLiteral  := ExprRep.NoPrepLiteral;
	genLiteral   := GenLiteral;
        note_write   := ExprRep.NotWritable;
      END;

PROCEDURE New (READONLY value: Target.Float;  pre: Precision): Expr.T =
  VAR p := NEW (P);
  BEGIN
    ExprRep.Init (p);
    p.pre     := pre;
    p.val     := value;
    p.checked := TRUE;
    CASE pre OF
    | Precision.Short    => p.type := Reel.T;
    | Precision.Long     => p.type := LReel.T;
    | Precision.Extended => p.type := EReel.T;
    END;
    RETURN p;
  END New;

PROCEDURE EqCheck (a: P;  e: Expr.T;  <*UNUSED*> x: M3.EqAssumption): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(b) => RETURN (a.pre = b.pre) AND TFloat.EQ (a.val, b.val);
    ELSE      RETURN FALSE;
    END;
  END EqCheck;

PROCEDURE Compile (p: P) =
  BEGIN
    CG.Load_float (p.val);
  END Compile;

PROCEDURE Compare (a, b: Expr.T;  VAR sign: INTEGER): BOOLEAN =
  VAR x, y: Target.Float;
  BEGIN
    IF NOT SplitPair (a, b, x, y) THEN RETURN FALSE END;
    IF    TFloat.LT (x, y) THEN sign := -1
    ELSIF TFloat.LT (y, x) THEN sign := +1
    ELSE                          sign :=  0
    END;
    RETURN TRUE;
  END Compare;

PROCEDURE Add (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR x, y, res: Target.Float;
  BEGIN
    IF NOT SplitPair (a, b, x, y) THEN RETURN FALSE END;
    IF NOT TFloat.Add (x, y, res) THEN RETURN FALSE END;
    c := New (res, res.pre);
    RETURN TRUE;
  END Add;

PROCEDURE Subtract (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR x, y, res: Target.Float;
  BEGIN
    IF NOT SplitPair (a, b, x, y) THEN RETURN FALSE END;
    IF NOT TFloat.Subtract (x, y, res) THEN RETURN FALSE END;
    c := New (res, res.pre);
    RETURN TRUE;
  END Subtract;

PROCEDURE Multiply (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR x, y, res: Target.Float;
  BEGIN
    IF NOT SplitPair (a, b, x, y) THEN RETURN FALSE END;
    IF NOT TFloat.Multiply (x, y, res) THEN RETURN FALSE END;
    c := New (res, res.pre);
    RETURN TRUE;
  END Multiply;

PROCEDURE Divide (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR x, y, res: Target.Float;
  BEGIN
    IF NOT SplitPair (a, b, x, y) THEN RETURN FALSE END;
    IF NOT TFloat.Divide (x, y, res) THEN RETURN FALSE END;
    c := New (res, res.pre);
    RETURN TRUE;
  END Divide;

PROCEDURE Mod (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR x, y, res: Target.Float;
  BEGIN
    IF NOT SplitPair (a, b, x, y) THEN RETURN FALSE END;
    IF NOT TFloat.Mod (x, y, res) THEN RETURN FALSE END;
    c := New (res, res.pre);
    RETURN TRUE;
  END Mod;

PROCEDURE Min (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR x, y: Target.Float;
  BEGIN
    IF NOT SplitPair (a, b, x, y) THEN RETURN FALSE END;
    IF TFloat.LT (x, y)
      THEN c := a;
      ELSE c := b;
    END;
    RETURN TRUE;
  END Min;

PROCEDURE Max (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR x, y: Target.Float;
  BEGIN
    IF NOT SplitPair (a, b, x, y) THEN RETURN FALSE END;
    IF TFloat.LT (x, y)
      THEN c := b;
      ELSE c := a;
    END;
    RETURN TRUE;
  END Max;

PROCEDURE Negate (a: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR x, zero, res: Target.Float;
  BEGIN
    IF NOT Split (a, x) THEN RETURN FALSE END;
    IF    (x.pre = Precision.Short) THEN zero := TFloat.ZeroR;
    ELSIF (x.pre = Precision.Long) THEN  zero := TFloat.ZeroL;
    ELSE                                 zero := TFloat.ZeroX;
    END;
    IF NOT TFloat.Subtract (zero, x, res) THEN RETURN FALSE END;
    c := New (res, res.pre);
    RETURN TRUE;
  END Negate;

PROCEDURE Abs (a: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR  x, zero, res: Target.Float;
  BEGIN
    IF NOT Split (a, x) THEN RETURN FALSE END;
    IF    (x.pre = Precision.Short) THEN zero := TFloat.ZeroR;
    ELSIF (x.pre = Precision.Long) THEN  zero := TFloat.ZeroL;
    ELSE                                 zero := TFloat.ZeroX;
    END;
    IF NOT TFloat.LT (x, zero) THEN  c := a; RETURN TRUE  END;
    IF NOT TFloat.Subtract (zero, x, res) THEN RETURN FALSE END;
    c := New (res, res.pre);
    RETURN TRUE;
  END Abs;

PROCEDURE Floor (a: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR x: Target.Float;  res: Target.Int;
  BEGIN
    IF NOT Split (a, x) THEN RETURN FALSE END;
    IF NOT TFloat.Floor (x, res) THEN RETURN FALSE END;
    c := IntegerExpr.New (res);
    RETURN TRUE;
  END Floor;

PROCEDURE Ceiling (a: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR x: Target.Float;  res: Target.Int;
  BEGIN
    IF NOT Split (a, x) THEN RETURN FALSE END;
    IF NOT TFloat.Ceiling (x, res) THEN RETURN FALSE END;
    c := IntegerExpr.New (res);
    RETURN TRUE;
  END Ceiling;

PROCEDURE Trunc (a: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR x: Target.Float;  res: Target.Int;
  BEGIN
    IF NOT Split (a, x) THEN RETURN FALSE END;
    IF NOT TFloat.Trunc (x, res) THEN RETURN FALSE END;
    c := IntegerExpr.New (res);
    RETURN TRUE;
  END Trunc;

PROCEDURE Round (a: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR x: Target.Float;  res: Target.Int;
  BEGIN
    IF NOT Split (a, x) THEN RETURN FALSE END;
    IF NOT TFloat.Round (x, res) THEN RETURN FALSE END;
    c := IntegerExpr.New (res);
    RETURN TRUE;
  END Round;

PROCEDURE Float (a: Expr.T;  t: Type.T;  VAR c: Expr.T): BOOLEAN =
  VAR i: Target.Int;  x, res: Target.Float;  new_pre: Precision;
  BEGIN
    t := Type.Base (t);
    IF    (t = Reel.T)  THEN new_pre := Precision.Short;
    ELSIF (t = LReel.T) THEN new_pre := Precision.Long;
    ELSIF (t = EReel.T) THEN new_pre := Precision.Extended;
    ELSE  RETURN FALSE;
    END;

    IF Split (a, x) THEN
      IF NOT TFloat.FloatF (x, new_pre, res) THEN RETURN FALSE END;
    ELSIF IntegerExpr.Split (a, i) THEN
      IF NOT TFloat.FloatI (i, new_pre, res) THEN RETURN FALSE END;
    ELSE
      RETURN FALSE;
    END;
    c := New (res, new_pre);
    RETURN TRUE;
  END Float;

PROCEDURE SplitPair (a, b: Expr.T;  VAR x, y: Target.Float): BOOLEAN =
  BEGIN
    IF NOT Split (a, x) THEN RETURN FALSE END;
    IF NOT Split (b, y) THEN RETURN FALSE END;
    IF (x.pre # y.pre) THEN RETURN FALSE END;
    RETURN TRUE;
  END SplitPair;

PROCEDURE Split (e: Expr.T;  VAR x: Target.Float): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(p) => x := p.val;  RETURN TRUE;
    ELSE      RETURN FALSE;
    END;
  END Split;

PROCEDURE IsZeroes (p: P): BOOLEAN =
  BEGIN
    RETURN TFloat.EQ (TFloat.ZeroR, p.val)
        OR TFloat.EQ (TFloat.ZeroL, p.val)
        OR TFloat.EQ (TFloat.ZeroX, p.val);
  END IsZeroes;

PROCEDURE GenFPLiteral (p: P;  buf: M3Buf.T) =
  CONST mark = ARRAY Precision OF TEXT { "REAL<", "LREAL<", "EREAL<" };
  BEGIN
    M3Buf.PutText  (buf, mark [p.pre]);
    M3Buf.PutFloat (buf, p.val);
    M3Buf.PutChar  (buf, '>');
  END GenFPLiteral;

PROCEDURE GenLiteral (p: P;  offset: INTEGER;  <*UNUSED*>type: Type.T;
                      is_const: BOOLEAN) =
  BEGIN
    CG.Init_float (offset, p.val, is_const);
  END GenLiteral;

BEGIN
END ReelExpr.
