(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ConcatExpr.m3                                         *)
(* Last modified on Tue Dec 20 14:25:23 PST 1994 by kalsow     *)
(*      modified on Thu Nov 29 03:30:53 1990 by muller         *)

MODULE ConcatExpr;

IMPORT CG, Expr, ExprRep, Type, Textt, Procedure, Target;
IMPORT TextExpr, AssignStmt, Host, NarrowExpr, RunTyme, Error;

TYPE
  P = ExprRep.Tab BRANDED "ConcatExpr.P" OBJECT
        folded : Expr.T;
        tmp    : CG.Val;
      OVERRIDES
        typeOf       := ExprRep.NoType;
        repTypeOf    := ExprRep.NoType;
        check        := Check;
        need_addr    := ExprRep.NotAddressable;
        prep         := Prep;
        compile      := Compile;
        prepLV       := ExprRep.NotLValue;
        compileLV    := ExprRep.NotLValue;
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
        exprAlign    := ExprRep.ExprAddrAlign; 
      END;

PROCEDURE New (a, b: Expr.T): Expr.T =
  VAR p := NEW (P);
  BEGIN
    ExprRep.Init (p);
    p.a := a;
    p.b := b;
    p.folded := NIL;
    p.type := Textt.T;
    p.repType := Textt.T;
    p.tmp := NIL;
    RETURN p;
  END New;

PROCEDURE Check (p: P;  VAR cs: Expr.CheckState) =
  VAR ta, tb: Type.T;  a, b: INTEGER;
  BEGIN
    Expr.TypeCheck (p.a, cs);
    Expr.TypeCheck (p.b, cs);
    ta := Type.Base (Expr.TypeOf (p.a));
    tb := Type.Base (Expr.TypeOf (p.b));

    IF Type.IsAssignable (Textt.T, ta) AND Type.IsAssignable (Textt.T, tb) THEN
      p.a := CheckArg (Textt.T, ta, p.a, cs);
      p.b := CheckArg (Textt.T, tb, p.b, cs);
    ELSE
      p.type := Expr.BadOperands ("\'&\'", ta, tb);
    END;

    Error.Count (a, b);
    IF (a = 0) THEN
      EVAL Fold (p); (* try folding all concatenations *)
    END;

    IF (p.folded # NIL) THEN
      Expr.TypeCheck (p.folded, cs);
    END;
  END Check;

PROCEDURE CheckArg (tlhs, trhs: Type.T;  e: Expr.T;
                    VAR cs: Expr.CheckState): Expr.T =
  BEGIN
    AssignStmt.Check (tlhs, e, cs);
    IF Host.doNarrowChk AND NOT Type.IsSubtype (trhs, tlhs) THEN
      e := NarrowExpr.New (e, tlhs);
      Expr.TypeCheck (e, cs);
    END;
    RETURN e;
  END CheckArg;

PROCEDURE Prep (p: P) =
  VAR proc: Procedure.T;
  BEGIN
    IF (p.folded # NIL) THEN
      Expr.Prep (p.folded);
    ELSE
      Expr.Prep (p.a);
      Expr.Prep (p.b);
      proc := RunTyme.LookUpProc (RunTyme.Hook.Concat);
      Procedure.StartCall (proc);
      IF Target.DefaultCall.args_left_to_right THEN
        Expr.Compile (p.a);
        CG.Pop_param (CG.Type.Addr);
        Expr.Compile (p.b);
        CG.Pop_param (CG.Type.Addr);
      ELSE
        Expr.Compile (p.b);
        CG.Pop_param (CG.Type.Addr);
        Expr.Compile (p.a);
        CG.Pop_param (CG.Type.Addr);
      END;
      p.tmp := Procedure.EmitValueCall (proc);
    END;
  END Prep;

PROCEDURE Compile (p: P) =
  BEGIN
    IF (p.folded # NIL) THEN
      Expr.Compile (p.folded);
    ELSE
      CG.Push (p.tmp);
      CG.Boost_addr_alignment (Target.Address.align);
      CG.Free (p.tmp);
      p.tmp := NIL;
    END;
  END Compile;

PROCEDURE Fold (p: P): Expr.T =
  VAR e1, e2, e3: Expr.T;
  BEGIN
    IF (p.folded # NIL) THEN RETURN p.folded END;
    e1 := Expr.ConstValue (p.a);
    IF (e1 = NIL) THEN RETURN NIL END;
    p.a := e1;
    e2 := Expr.ConstValue (p.b);
    IF (e2 = NIL) THEN RETURN NIL END;
    p.b := e2;
    e3 := NIL;
    IF TextExpr.Cat (e1, e2, e3) THEN p.folded := e3 END;
    RETURN e3;
  END Fold;

BEGIN
END ConcatExpr.
