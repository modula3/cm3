(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: NotExpr.m3                                            *)
(* Last modified on Fri Jul  8 09:42:35 PDT 1994 by kalsow     *)
(*      modified on Thu Nov 29 03:28:44 1990 by muller         *)

MODULE NotExpr;

IMPORT CG, Expr, ExprRep, Type, Bool, EnumExpr, Target, TInt, Value;

TYPE
  P = ExprRep.Ta BRANDED "NotExpr.P" OBJECT
      OVERRIDES
        typeOf       := ExprRep.NoType;
        repTypeOf    := ExprRep.NoType;
        check        := Check;
        need_addr    := ExprRep.NotAddressable;
        prep         := Prep;
        compile      := Compile;
        prepLV       := ExprRep.NotLValue;
        compileLV    := ExprRep.NotLValue;
        prepBR       := PrepBR;
        compileBR    := ExprRep.NoBranch;
        evaluate     := Fold;
        isEqual      := ExprRep.EqCheckA;
        getBounds    := ExprRep.NoBounds;
        isWritable   := ExprRep.IsNever;
        isDesignator := ExprRep.IsNever;
        isZeroes     := ExprRep.IsNever;
        genFPLiteral := ExprRep.NoFPLiteral;
        prepLiteral  := ExprRep.NoPrepLiteral;
        genLiteral   := ExprRep.NoLiteral;
        note_write   := ExprRep.NotWritable;
        exprAlign    := ExprRep.ExprBoolAlign; 
      END;

PROCEDURE New (a: Expr.T): Expr.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    ExprRep.Init (p);
    p.a       := a;
    p.type    := Bool.T;
    p.repType := Bool.T;
    RETURN p;
  END New;

PROCEDURE Check (p: P;  VAR cs: Expr.CheckState) =
  VAR ta: Type.T;
  BEGIN
    Expr.TypeCheck (p.a, cs);
    ta := Type.Base (Expr.TypeOf (p.a));
    IF (ta # Bool.T) THEN
      p.type := Expr.BadOperands ("NOT", ta);
    END;
  END Check;

PROCEDURE Prep (p: P) =
  BEGIN
    Expr.Prep (p.a);
  END Prep;

PROCEDURE Compile (p: P) =
  BEGIN
    Expr.Compile (p.a);
    Value.Load (Bool.False);
    CG.Compare (Target.Integer.cg_type, CG.Cmp.EQ);
  END Compile;

PROCEDURE PrepBR (p: P;  true, false: CG.Label;  freq: CG.Frequency) =
  BEGIN
    Expr.PrepBranch (p.a, false, true, CG.Always - freq);
    Expr.CompileBranch (p.a, false, true, CG.Always - freq);
  END PrepBR;

PROCEDURE Fold (p: P): Expr.T =
  VAR e1, e3: Expr.T;  ii: Target.Int; i: INTEGER; t: Type.T;
  BEGIN
    e1 := Expr.ConstValue (p.a);
    e3 := NIL;
    IF (e1 # NIL)
      AND (EnumExpr.Split (e1, ii, t))
      AND (Type.Base (t) = Bool.T)
      AND TInt.ToInt (ii, i) THEN
      e3 := Bool.Map[ORD (FALSE) = i]
    END;
    RETURN e3;
  END Fold;

BEGIN
END NotExpr.
