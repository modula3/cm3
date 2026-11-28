(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: DivExpr.m3                                            *)
(* Last modified on Wed Feb 22 09:01:01 PST 1995 by kalsow     *)
(*      modified on Thu Nov 29 03:31:09 1990 by muller         *)

MODULE DivExpr;

IMPORT CG, Expr, ExprRep, Type, Int, LInt, IntegerExpr, TInt, Target;
IMPORT TargetMap;
IMPORT Host, OverflowChk;

TYPE
  P = ExprRep.Tab BRANDED "DivExpr.P" OBJECT
      OVERRIDES
        typeOf       := TypeOf;
        repTypeOf    := RepTypeOf;
        check        := Check;
        need_addr    := ExprRep.NotAddressable;
        prep         := Prep;
        compile      := Compile;
        prepLV       := ExprRep.NotLValue;
        compileLV    := ExprRep.NotLValueBool;
        prepBR       := ExprRep.NotBoolean;
        compileBR    := ExprRep.NotBoolean;
        evaluate     := Fold;
        isEqual      := ExprRep.EqCheckAB;
        getBounds    := ExprRep.NoBounds;
        isWritable   := ExprRep.IsNever;
        isDesignator := ExprRep.IsNever;
        isZeroes     := ExprRep.IsNever;
        genFPLiteral := ExprRep.NoFPLiteral;
        prepLiteral  := ExprRep.NoPrepLiteral;
        genLiteral   := ExprRep.NoLiteral;
        note_write   := ExprRep.NotWritable;
      END;

PROCEDURE New (a, b: Expr.T): Expr.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    ExprRep.Init (p);
    p.a := a;
    p.b := b;
    p.type := NIL;
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
  VAR ta, tb: Type.T;
  BEGIN
    Expr.TypeCheck (p.a, cs);
    Expr.TypeCheck (p.b, cs);
    ta := Type.Base (Expr.TypeOf (p.a));
    tb := Type.Base (Expr.TypeOf (p.b));
    IF (ta = Int.T) AND (tb = Int.T) THEN
      p.type := Int.T;
    ELSIF (ta = LInt.T) AND (tb = LInt.T) THEN
      p.type := LInt.T;
    ELSE
      p.type := Expr.BadOperands ("DIV", ta, tb);
    END;
    p.repType := p.type;
    IF IntegerExpr.IsMinusOne (p.b) THEN
      (* x DIV -1 = -x *)
      IntegerExpr.CheckOverflow (IntegerExpr.Op.Negate, p.a, NIL);
    END;
  END Check;

PROCEDURE Prep (p: P) =
  BEGIN
    Expr.Prep (p.a);
    Expr.Prep (p.b);
  END Prep;

PROCEDURE Compile (p: P; StaticOnly: BOOLEAN) =
  VAR e1, e2, e3: Expr.T;  divisor: Target.Int;  log: INTEGER;
      t: Type.T;  cg_type: CG.Type;
  BEGIN
    <* ASSERT NOT StaticOnly *>
    e1 := Expr.ConstValue (p.a);
    e2 := Expr.ConstValue (p.b);
    e3 := NIL;
    IF (e1 # NIL) AND (e2 # NIL) AND IntegerExpr.Div (e1, e2, e3) THEN
      Expr.Compile (e3);
    ELSIF (e2 # NIL)
      AND IntegerExpr.Split (e2, divisor, t)
      AND SmallPowerOfTwo (divisor, log) THEN
      IF (e1 = NIL) THEN e1 := p.a; END;
      IF (log = 0) THEN
        (* divide by one *)
        Expr.Compile (e1);
      ELSE
        Expr.Compile (e1);
        cg_type := Type.CGType (p.type);
        CG.Extract_mn (cg_type, TRUE, log, TargetMap.CG_Size[cg_type] - log);
      END;
    ELSE
      IF (e1 = NIL) THEN e1 := p.a; END;
      IF (e2 = NIL) THEN e2 := p.b; END;
      Expr.Compile (e1);
      Expr.Compile (e2);
      cg_type := Type.CGType (p.type);
      IF Host.doOverflowChk AND OverflowChk.Signed (cg_type) THEN
        (* FIRST DIV -1 aborts *)
        OverflowChk.Div (cg_type, Expr.GetSign (e1), Expr.GetSign (e2));
      ELSIF MayOverflow (e1, e2, cg_type) THEN
        (* FIRST DIV -1 wraps around *)
        CompileWrapped (cg_type, Expr.GetSign (e1), div := TRUE);
      ELSE
        CG.Div (cg_type, Expr.GetSign (e1), Expr.GetSign (e2));
      END;
    END;
  END Compile;

PROCEDURE Fold (p: P): Expr.T =
  VAR e1, e2, e3: Expr.T;
  BEGIN
    e1 := Expr.ConstValue (p.a);
    e2 := Expr.ConstValue (p.b);
    e3 := NIL;
    IF IntegerExpr.Div (e1, e2, e3) THEN END;
    RETURN e3;
  END Fold;

VAR(*CONST*)
  powers    : ARRAY [0..63] OF Target.Int;
  max_power := 0;

PROCEDURE MayOverflow (a, b: Expr.T;  t: CG.Type): BOOLEAN =
  VAR first, amin, amax, bmin, bmax: Target.Int;
  BEGIN
    IF    t = Target.Integer.cg_type THEN first := Target.Integer.min;
    ELSIF t = Target.Longint.cg_type THEN first := Target.Longint.min;
    ELSE  RETURN FALSE;
    END;
    Expr.GetBounds (a, amin, amax);
    Expr.GetBounds (b, bmin, bmax);
    RETURN TInt.LE (amin, first)
       AND TInt.LE (bmin, TInt.MOne) AND TInt.LE (TInt.MOne, bmax);
  END MayOverflow;

PROCEDURE CompileWrapped (t: CG.IType;  sa: CG.Sign;  div: BOOLEAN) =
  (* With m := ORD (b = -1), divide by b + 2*m, which is 1 when b = -1:
     a MOD 1 = 0 is then right, and a DIV 1 = a needs negating, wrapping
     around, which is (q XOR -m) + m.  No jumps, so this is safe in the
     middle of an expression. *)
  VAR b, m: CG.Val;
  BEGIN
    b := CG.Pop_temp ();
    CG.Push (b);
    CG.Load_integer (t, TInt.MOne);
    CG.Compare (t, CG.Cmp.EQ);
    IF t # Target.Integer.cg_type THEN
      CG.Loophole (Target.Integer.cg_type, t);
    END;
    m := CG.Pop_temp ();
    CG.Push (b);  CG.Push (m);  CG.Add (t);  CG.Push (m);  CG.Add (t);
    IF div THEN
      CG.Div (t, sa, CG.Sign.Unknown);
      CG.Push (m);  CG.Negate (t);  CG.Xor (t);
      CG.Push (m);  CG.Add (t);
    ELSE
      CG.Mod (t, sa, CG.Sign.Unknown);
    END;
    CG.Free (b);  CG.Free (m);
  END CompileWrapped;

PROCEDURE SmallPowerOfTwo (READONLY x: Target.Int;  VAR log: INTEGER): BOOLEAN=
  VAR lo, hi, mid: INTEGER;
  BEGIN
    IF max_power <= 0 THEN
      powers[0] := TInt.One;
      FOR i := 1 TO MIN (Target.Longint.size - 2, LAST (powers)) DO
        IF TInt.Add (powers[i-1], powers[i-1], powers[i]) THEN
          max_power := i;
        END;
      END;
    END;
    lo := 0;
    hi := max_power+1;
    WHILE (lo < hi) DO
      mid := (lo + hi) DIV 2;
      IF TInt.EQ (x, powers[mid]) THEN
        log := mid;
        RETURN TRUE;
      ELSIF TInt.LT (x, powers[mid]) THEN
        hi := mid;
      ELSE
        lo := mid + 1;
      END;
    END;
    log := -1;
    RETURN FALSE;
  END SmallPowerOfTwo;

BEGIN
END DivExpr.
