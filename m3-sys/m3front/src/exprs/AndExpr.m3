(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: AndExpr.m3                                            *)
(* Last modified on Fri Jul  8 09:48:48 PDT 1994 by kalsow     *)
(*      modified on Thu Nov 29 03:30:36 1990 by muller         *)

MODULE AndExpr;

IMPORT CG, Expr, ExprRep, Type, Bool, EnumExpr, Target, TInt;

TYPE
  P = ExprRep.Tab BRANDED "AndExpr.P" OBJECT
        tmp: CG.Val;
      OVERRIDES
        typeOf       := ExprRep.NoType;
        check        := Check;
        need_addr    := ExprRep.NotAddressable;
        prep         := Prep;
        compile      := Compile;
        prepLV       := ExprRep.NotLValue;
        compileLV    := ExprRep.NotLValue;
        prepBR       := PrepBR;
        compileBR    := ExprRep.NoBranch;
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
    p.type := Bool.T;
    p.tmp := NIL;
    RETURN p;
  END New;

PROCEDURE Check (p: P;  VAR cs: Expr.CheckState) =
  VAR ta, tb: Type.T;
  BEGIN
    Expr.TypeCheck (p.a, cs);
    Expr.TypeCheck (p.b, cs);
    ta := Type.Base (Expr.TypeOf (p.a));
    tb := Type.Base (Expr.TypeOf (p.b));
    IF (ta # Bool.T) OR (tb # Bool.T) THEN
      p.type := Expr.BadOperands ("AND", ta, tb);
    END;
  END Check;

PROCEDURE Prep (p: P) =
  VAR false := CG.Next_label ();
  BEGIN
    CG.Load_integer (TInt.Zero);
    p.tmp := CG.Pop_temp ();
    PrepBR (p, CG.No_label, false, CG.Maybe);
    CG.Load_integer (TInt.One);
    CG.Store_temp (p.tmp);
    CG.Set_label (false);
  END Prep;

PROCEDURE Compile (p: P) =
  BEGIN
    CG.Push (p.tmp);
    CG.Free (p.tmp);
    p.tmp := NIL;
  END Compile;

PROCEDURE PrepBR (p: P;  true, false: CG.Label;  freq: CG.Frequency) =
  VAR skip: CG.Label;
  BEGIN
    IF (true = CG.No_label) THEN
      Expr.PrepBranch (p.a, true, false, freq);
      Expr.CompileBranch (p.a, true, false, freq);
      Expr.PrepBranch (p.b, true, false, freq);
      Expr.CompileBranch (p.b, true, false, freq);
    ELSE (* false = NoLabel => fall through *)
      skip := CG.Next_label ();
      Expr.PrepBranch (p.a, CG.No_label, skip, CG.Always - freq);
      Expr.CompileBranch (p.a, CG.No_label, skip, CG.Always - freq);
      Expr.PrepBranch (p.b, true, CG.No_label, freq);
      Expr.CompileBranch (p.b, true, CG.No_label, freq);
      CG.Set_label (skip);
    END;
  END PrepBR;

PROCEDURE Fold (p: P): Expr.T =
  VAR e1, e2: Expr.T;  i1, i2: Target.Int;  t1, t2: Type.T;  z1, z2: INTEGER;
  BEGIN
    e1 := Expr.ConstValue (p.a);
    e2 := Expr.ConstValue (p.b);
    IF (e1 = NIL) OR (e2 = NIL)
      OR (NOT EnumExpr.Split (e1, i1, t1))
      OR (NOT EnumExpr.Split (e2, i2, t2))
      OR (Type.Base (t1) # Bool.T)
      OR (Type.Base (t2) # Bool.T)
      OR NOT TInt.ToInt (i1, z1)
      OR NOT TInt.ToInt (i2, z2) THEN
      RETURN NIL;
    ELSE
      RETURN Bool.Map[(ORD (TRUE) = z1) AND (ORD (TRUE) = z2)];
    END;
  END Fold;

BEGIN
END AndExpr.
