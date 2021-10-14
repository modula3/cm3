(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ReelExpr.m3                                           *)
(* Last modified on Fri Feb 24 16:46:45 PST 1995 by kalsow     *)
(*      modified on Tue Apr 10 22:38:17 1990 by muller         *)

MODULE ReelExpr;

IMPORT M3, CG, Expr, ExprRep, Type, Target, TInt, TFloat;
IMPORT M3Buf, Reel, LReel, EReel, IntegerExpr;

TYPE
  P = Expr.T OBJECT
        val : Target.Float;
      OVERRIDES
        typeOf       := ExprRep.NoType;
        repTypeOf    := ExprRep.NoType;
        check        := ExprRep.NoCheck;
        need_addr    := ExprRep.NotAddressable;
        prep         := ExprRep.NoPrep;
        compile      := Compile;
        prepLV       := ExprRep.NotLValue;
        compileLV    := ExprRep.NotLValueBool;
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

PROCEDURE New (READONLY value: Target.Float): Expr.T =
  VAR p := NEW (P);
  BEGIN
    ExprRep.Init (p);
    p.val     := value;
    p.checked := TRUE;
    CASE TFloat.Prec (value) OF
    | Target.Precision.Short    => p.type := Reel.T;
    | Target.Precision.Long     => p.type := LReel.T;
    | Target.Precision.Extended => p.type := EReel.T;
    END;
    p.repType := p.type;
    RETURN p;
  END New;

PROCEDURE EqCheck (a: P;  e: Expr.T;  <*UNUSED*> x: M3.EqAssumption): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(b) => RETURN TFloat.EQ (a.val, b.val);
    ELSE      RETURN FALSE;
    END;
  END EqCheck;

PROCEDURE Compile (p: P; StaticOnly: BOOLEAN) =
  BEGIN
    <* ASSERT NOT StaticOnly *>
    CG.Load_float (p.val);
  END Compile;

PROCEDURE Compare (a, b: Expr.T;  VAR sign: INTEGER): BOOLEAN =
  VAR x, y: Target.Float;
  BEGIN
    IF NOT SplitPair (a, b, x, y) THEN RETURN FALSE END;
    IF    TFloat.LT (x, y) THEN sign := -1
    ELSIF TFloat.LT (y, x) THEN sign := +1
    ELSE                        sign :=  0
    END;
    RETURN TRUE;
  END Compare;

PROCEDURE Add (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR x, y, res: Target.Float;
  BEGIN
    IF NOT SplitPair (a, b, x, y) THEN RETURN FALSE END;
    IF NOT TFloat.Add (x, y, res) THEN RETURN FALSE END;
    c := New (res);
    RETURN TRUE;
  END Add;

PROCEDURE Subtract (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR x, y, res: Target.Float;
  BEGIN
    IF NOT SplitPair (a, b, x, y) THEN RETURN FALSE END;
    IF NOT TFloat.Subtract (x, y, res) THEN RETURN FALSE END;
    c := New (res);
    RETURN TRUE;
  END Subtract;

PROCEDURE Multiply (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR x, y, res: Target.Float;
  BEGIN
    IF NOT SplitPair (a, b, x, y) THEN RETURN FALSE END;
    IF NOT TFloat.Multiply (x, y, res) THEN RETURN FALSE END;
    c := New (res);
    RETURN TRUE;
  END Multiply;

PROCEDURE Divide (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR x, y, res: Target.Float;
  BEGIN
    IF NOT SplitPair (a, b, x, y) THEN RETURN FALSE END;
    IF NOT TFloat.Divide (x, y, res) THEN RETURN FALSE END;
    c := New (res);
    RETURN TRUE;
  END Divide;

PROCEDURE Mod (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR x, y, res: Target.Float;
  BEGIN
    IF NOT SplitPair (a, b, x, y) THEN RETURN FALSE END;
    IF NOT TFloat.Mod (x, y, res) THEN RETURN FALSE END;
    c := New (res);
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
    CASE TFloat.Prec (x) OF
    | Target.Precision.Short    => zero := TFloat.ZeroR;
    | Target.Precision.Long     => zero := TFloat.ZeroL;
    | Target.Precision.Extended => zero := TFloat.ZeroX;
    END;
    IF NOT TFloat.Subtract (zero, x, res) THEN RETURN FALSE END;
    c := New (res);
    RETURN TRUE;
  END Negate;

PROCEDURE Abs (a: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR  x, zero, res: Target.Float;
  BEGIN
    IF NOT Split (a, x) THEN RETURN FALSE END;
    CASE TFloat.Prec (x) OF
    | Target.Precision.Short    => zero := TFloat.ZeroR;
    | Target.Precision.Long     => zero := TFloat.ZeroL;
    | Target.Precision.Extended => zero := TFloat.ZeroX;
    END;
    IF NOT TFloat.LT (x, zero) THEN  c := a; RETURN TRUE  END;
    IF NOT TFloat.Subtract (zero, x, res) THEN RETURN FALSE END;
    c := New (res);
    RETURN TRUE;
  END Abs;

PROCEDURE Floor (a: Expr.T;  t: Type.T;  VAR c: Expr.T): BOOLEAN =
  VAR x: Target.Float;  min, max, res: Target.Int;
  BEGIN
    IF NOT Split (a, x) THEN RETURN FALSE END;
    IF NOT TInt.FromInt (TFloat.Floor (x), res) THEN RETURN FALSE END;
    IF NOT Type.GetBounds (t, min, max) THEN RETURN FALSE END;
    IF TInt.LT (res, min) OR TInt.LT (max, res) THEN RETURN FALSE END;
    c := IntegerExpr.New (t, res);
    RETURN TRUE;
  END Floor;

PROCEDURE Ceiling (a: Expr.T;  t: Type.T;  VAR c: Expr.T): BOOLEAN =
  VAR x: Target.Float;  min, max, res: Target.Int;
  BEGIN
    IF NOT Split (a, x) THEN RETURN FALSE END;
    IF NOT TInt.FromInt (TFloat.Ceiling (x), res) THEN RETURN FALSE END;
    IF NOT Type.GetBounds (t, min, max) THEN RETURN FALSE END;
    IF TInt.LT (res, min) OR TInt.LT (max, res) THEN RETURN FALSE END;
    c := IntegerExpr.New (t, res);
    RETURN TRUE;
  END Ceiling;

PROCEDURE Trunc (a: Expr.T;  t: Type.T;  VAR c: Expr.T): BOOLEAN =
  VAR x: Target.Float;  min, max, res: Target.Int;
  BEGIN
    IF NOT Split (a, x) THEN RETURN FALSE END;
    IF NOT TInt.FromInt (TFloat.Trunc (x), res) THEN RETURN FALSE END;
    IF NOT Type.GetBounds (t, min, max) THEN RETURN FALSE END;
    IF TInt.LT (res, min) OR TInt.LT (max, res) THEN RETURN FALSE END;
    c := IntegerExpr.New (t, res);
    RETURN TRUE;
  END Trunc;

PROCEDURE Round (a: Expr.T;  t: Type.T;  VAR c: Expr.T): BOOLEAN =
  VAR x: Target.Float;  min, max, res: Target.Int;
  BEGIN
    IF NOT Split (a, x) THEN RETURN FALSE END;
    IF NOT TInt.FromInt (TFloat.Round (x), res) THEN RETURN FALSE END;
    IF NOT Type.GetBounds (t, min, max) THEN RETURN FALSE END;
    IF TInt.LT (res, min) OR TInt.LT (max, res) THEN RETURN FALSE END;
    c := IntegerExpr.New (t, res);
    RETURN TRUE;
  END Round;

PROCEDURE Float (a: Expr.T;  t: Type.T;  VAR c: Expr.T): BOOLEAN =
  VAR i: INTEGER;  x, res: Target.Float;  new_pre: Target.Precision;
  BEGIN
    IF    (t = Reel.T)  THEN new_pre := Target.Precision.Short;
    ELSIF (t = LReel.T) THEN new_pre := Target.Precision.Long;
    ELSIF (t = EReel.T) THEN new_pre := Target.Precision.Extended;
    ELSE RETURN FALSE END;

    IF Split (a, x) THEN
      IF NOT TFloat.FloatF (x, new_pre, res) THEN RETURN FALSE END;
    ELSIF IntegerExpr.ToInt (a, i) THEN
      IF NOT TFloat.FloatI (i, new_pre, res) THEN RETURN FALSE END;
    ELSE
      RETURN FALSE;
    END;
    c := New (res);
    RETURN TRUE;
  END Float;

PROCEDURE SplitPair (a, b: Expr.T;  VAR x, y: Target.Float): BOOLEAN =
  VAR t: Type.T;
  BEGIN
    TYPECASE a OF
    | NULL => RETURN FALSE;
    | P(p) => x := p.val;  t := p.type;
    ELSE      RETURN FALSE;
    END;
    TYPECASE b OF
    | NULL => RETURN FALSE;
    | P(p) => y := p.val;  RETURN t = p.type;
    ELSE      RETURN FALSE;
    END;
  END SplitPair;

PROCEDURE Split (e: Expr.T;  VAR x: Target.Float): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(p) => x := p.val;  RETURN TRUE;
    ELSE      RETURN FALSE;
    END;
  END Split;

PROCEDURE IsZeroes (p: P;  <*UNUSED*> lhs: BOOLEAN): BOOLEAN =
  BEGIN
    RETURN TFloat.EQ (TFloat.ZeroR, p.val)
        OR TFloat.EQ (TFloat.ZeroL, p.val)
        OR TFloat.EQ (TFloat.ZeroX, p.val);
  END IsZeroes;

PROCEDURE GenFPLiteral (p: P;  buf: M3Buf.T) =
  BEGIN
    IF    p.type = Reel.T  THEN M3Buf.PutText  (buf, "REAL<");
    ELSIF p.type = LReel.T THEN M3Buf.PutText  (buf, "LREAL<");
    ELSIF p.type = EReel.T THEN M3Buf.PutText  (buf, "EREAL<");
    ELSE <*ASSERT FALSE*> END;
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
