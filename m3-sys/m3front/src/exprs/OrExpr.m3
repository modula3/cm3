(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: OrExpr.m3                                             *)
(* Last modified on Fri Jul  8 09:48:43 PDT 1994 by kalsow     *)
(*      modified on Thu Nov 29 03:32:03 1990 by muller         *)

MODULE OrExpr;

IMPORT CG, Expr, ExprRep, Type, Bool, EnumExpr, Target, TInt;

TYPE
  P = ExprRep.Tab BRANDED "OrExpr.P" OBJECT
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
    p.a    := a;
    p.b    := b;
    p.type := Bool.T;
    p.tmp  := NIL;
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
      p.type := Expr.BadOperands ("OR", ta, tb);
    END;
  END Check;

PROCEDURE Prep (p: P) =
  VAR true := CG.Next_label ();
  BEGIN
    CG.Load_integer (TInt.One);
    p.tmp := CG.Pop_temp ();
    PrepBR (p, true, CG.No_label, CG.Maybe);
    CG.Load_integer (TInt.Zero);
    CG.Store_temp (p.tmp);
    CG.Set_label (true);
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
    IF (false = CG.No_label) THEN
      Expr.PrepBranch (p.a, true, false, freq);
      Expr.CompileBranch (p.a, true, false, freq);
      Expr.PrepBranch (p.b, true, false, freq);
      Expr.CompileBranch (p.b, true, false, freq);
    ELSE (* true = NoLabel => fall through *)
      skip := CG.Next_label ();
      Expr.PrepBranch (p.a, skip, CG.No_label, CG.Always - freq);
      Expr.CompileBranch (p.a, skip, CG.No_label, CG.Always - freq);
      Expr.PrepBranch (p.b, CG.No_label, false, freq);
      Expr.CompileBranch (p.b, CG.No_label, false, freq);
      CG.Set_label (skip);
    END;
  END PrepBR;

PROCEDURE Fold (p: P): Expr.T =
  VAR e1, e2: Expr.T; i1, i2: Target.Int; t1, t2: Type.T;  z1, z2: INTEGER;
  BEGIN
    e1 := Expr.ConstValue (p.a);
    e2 := Expr.ConstValue (p.b);
    IF (e1 = NIL) OR (e2 = NIL)
      OR (NOT EnumExpr.Split (e1, i1, t1))
      OR (NOT EnumExpr.Split (e2, i2, t2))
      OR (Type.Base (t1) # Bool.T)
      OR (Type.Base (t2) # Bool.T)
      OR (NOT TInt.ToInt (i1, z1))
      OR (NOT TInt.ToInt (i2, z2)) THEN
      RETURN NIL;
    ELSE
      RETURN Bool.Map[(ORD (TRUE) = z1) OR (ORD (TRUE) = z2)];
    END;
  END Fold;

BEGIN
END OrExpr.
