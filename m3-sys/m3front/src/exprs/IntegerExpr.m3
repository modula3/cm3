(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: IntegerExpr.m3                                        *)
(* Last modified on Fri Feb 24 16:43:29 PST 1995 by kalsow     *)
(*      modified on Tue Apr 10 22:35:24 1990 by muller         *)

MODULE IntegerExpr;
(* INTEGER and LONGINT *Literals* only. *)

IMPORT M3, CG, Expr, ExprRep, Type, Int, LInt, Error, M3Buf, Target, TInt, TWord;
IMPORT Host;

TYPE
  P = Expr.T BRANDED "IntegerExpr.T" OBJECT
        value: Target.Int;
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
(* PRE: Type.IsSubtype (type, Int.T) OR Type.IsSubtype (type, LInt.T) *)
  VAR p: P;  n: INTEGER;  t := Type.IsSubtype (type, LInt.T);
      min, max: Target.Int;
  BEGIN
    IF TInt.ToInt (value, n)
      AND (FIRST (cache[t]) <= n) AND (n <= LAST (cache[t])) THEN
      p := cache[t][n];
      IF (p # NIL) THEN RETURN p; END;
    END;
    IF NOT Type.GetBounds (type, min, max)
      OR TInt.LT (value, min)
      OR TInt.LT (max, value) THEN
      <*ASSERT FALSE*>
    END;
    p := NEW (P);
    ExprRep.Init (p);
    p.value   := value;
    p.type    := type;
    p.repType := type;
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

PROCEDURE Compile (p: P; StaticOnly: BOOLEAN) =
  BEGIN
    <* ASSERT NOT StaticOnly *>
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
  VAR overflow: BOOLEAN;
  BEGIN
    RETURN Arith (Op.Add, a, b, c, overflow);
  END Add;

PROCEDURE Subtract (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR overflow: BOOLEAN;
  BEGIN
    RETURN Arith (Op.Subtract, a, b, c, overflow);
  END Subtract;

PROCEDURE Multiply (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR overflow: BOOLEAN;
  BEGIN
    RETURN Arith (Op.Multiply, a, b, c, overflow);
  END Multiply;

PROCEDURE IsMinusOne (e: Expr.T): BOOLEAN =
  VAR x: Target.Int;  t: Type.T;
  BEGIN
    e := Expr.ConstValue (e);
    RETURN (e # NIL) AND Split (e, x, t) AND TInt.EQ (x, TInt.MOne);
  END IsMinusOne;

PROCEDURE Div (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR x, y, min, max, res: Target.Int;  t: Type.T;
  BEGIN
    IF NOT SplitPair (a, b, x, y, t) THEN RETURN FALSE END;
    IF TInt.EQ (y, TInt.Zero) THEN
      Error.Msg ("attempt to DIV by 0");
      RETURN FALSE;
    END;
    IF TInt.EQ (y, TInt.MOne) THEN
      (* x DIV -1 = -x, which wraps around for FIRST, as at run time *)
      RETURN Negate (a, c);
    END;
    IF NOT TInt.Div (x, y, res) THEN RETURN FALSE END;
    IF NOT Type.GetBounds (t, min, max) THEN RETURN FALSE END;
    IF TInt.LT (res, min) OR TInt.LT (max, res) THEN RETURN FALSE END;
    c := New (t, res);
    RETURN TRUE;
  END Div;

PROCEDURE Mod (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR x, y, min, max, res: Target.Int;  t: Type.T;
  BEGIN
    IF NOT SplitPair (a, b, x, y, t) THEN RETURN FALSE END;
    IF TInt.EQ (y, TInt.Zero) THEN
      Error.Msg ("attempt to MOD by 0");
      RETURN FALSE;
    END;
    IF TInt.EQ (y, TInt.MOne) THEN
      c := New (t, TInt.Zero);  (* even for FIRST, whose DIV -1 overflows *)
      RETURN TRUE;
    END;
    IF NOT TInt.Mod (x, y, res) THEN RETURN FALSE END;
    IF NOT Type.GetBounds (t, min, max) THEN RETURN FALSE END;
    IF TInt.LT (res, min) OR TInt.LT (max, res) THEN RETURN FALSE END;
    c := New (t, res);
    RETURN TRUE;
  END Mod;

PROCEDURE Negate (a: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR overflow: BOOLEAN;
  BEGIN
    RETURN Arith (Op.Negate, a, NIL, c, overflow);
  END Negate;

(* Integer +, -, * and negation of constants.  An overflow wraps around:
   in CM3 the result is the two's-complement wraparound of the exact
   result (FloatMode.i3), so the fold computes it, and "overflow" says it
   happened, for the operator's Check to warn (Overflows).  The type is
   INTEGER or LONGINT (subranges are converted to their base). *)
PROCEDURE Arith (op: Op;  a, b: Expr.T;  VAR c: Expr.T;
                 VAR overflow: BOOLEAN): BOOLEAN =
  VAR x, y, min, max, res, w: Target.Int;  t: Type.T;  ok: BOOLEAN;
      bytes: CARDINAL;
  BEGIN
    overflow := FALSE;
    IF op = Op.Negate THEN
      IF NOT Split (a, x, t) THEN RETURN FALSE END;
      y := x;  x := TInt.Zero;
    ELSIF NOT SplitPair (a, b, x, y, t) THEN
      RETURN FALSE;
    END;
    CASE op OF
    | Op.Add                => ok := TInt.Add (x, y, res);
    | Op.Subtract, Op.Negate => ok := TInt.Subtract (x, y, res);
    | Op.Multiply           => ok := TInt.Multiply (x, y, res);
    END;
    IF NOT Type.GetBounds (t, min, max) THEN RETURN FALSE END;
    IF ok AND NOT (TInt.LT (res, min) OR TInt.LT (max, res)) THEN
      c := New (t, res);
      RETURN TRUE;
    END;
    (* the exact result does not fit: wrap it to the type's width *)
    IF    Type.Base (t) = Int.T  THEN bytes := Target.Integer.bytes;
    ELSIF Type.Base (t) = LInt.T THEN bytes := Target.Longint.bytes;
    ELSE  RETURN FALSE;
    END;
    CASE op OF
    | Op.Add                => TWord.Add (x, y, w);
    | Op.Subtract, Op.Negate => TWord.Subtract (x, y, w);
    | Op.Multiply           => TWord.Multiply (x, y, w);
    END;
    EVAL TWord.Truncate (w, bytes, w);
    EVAL TInt.Extend (w, bytes, w);
    overflow := TRUE;
    c := New (Type.Base (t), w);
    RETURN TRUE;
  END Arith;

PROCEDURE Overflows (op: Op;  a, b: Expr.T): BOOLEAN =
  VAR c: Expr.T;  overflow: BOOLEAN;
  BEGIN
    a := Expr.ConstValue (a);
    IF op # Op.Negate THEN b := Expr.ConstValue (b) END;
    RETURN Arith (op, a, b, c, overflow) AND overflow;
  END Overflows;

PROCEDURE CheckOverflow (op: Op;  a, b: Expr.T) =
  BEGIN
    IF NOT Overflows (op, a, b) THEN
      (* fine *)
    ELSIF Host.doOverflowChk THEN
      Error.Msg ("integer overflow in a constant expression");
    ELSE
      Error.Warn (2, "integer overflow in a constant expression: the result wraps around");
    END;
  END CheckOverflow;

PROCEDURE Abs (a: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR x, min, max, res: Target.Int;  t: Type.T;
  BEGIN
    IF NOT Split (a, x, t) THEN RETURN FALSE END;
    IF TInt.LT (x, TInt.Zero) THEN
      IF NOT TInt.Subtract (TInt.Zero, x, res) THEN RETURN FALSE END;
      IF NOT Type.GetBounds (t, min, max) THEN RETURN FALSE END;
      IF TInt.LT (res, min) OR TInt.LT (max, res) THEN RETURN FALSE END;
      c := New (t, res);
    ELSE
      c := a;
    END;
    RETURN TRUE;
  END Abs;

PROCEDURE ToInt (a: Expr.T;  VAR i: INTEGER): BOOLEAN =
  VAR x: Target.Int;  t: Type.T;
  BEGIN
    IF NOT Split (a, x, t) THEN RETURN FALSE END;
    IF NOT TInt.ToInt (x, i) THEN RETURN FALSE END;
    RETURN TRUE;
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
