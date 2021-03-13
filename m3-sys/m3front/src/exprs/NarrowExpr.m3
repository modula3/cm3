(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE NarrowExpr;

IMPORT M3, Expr, ExprRep, Type, Narrow, CG, Host, Target;

TYPE
  P = Expr.T BRANDED "NarrowExpr" OBJECT
        expr : Expr.T;
        tipe : Type.T;
        tmp  : CG.Val;
      OVERRIDES
        typeOf       := ExprRep.NoType;
        repTypeOf    := ExprRep.NoType;
        check        := Check;
        need_addr    := NeedsAddress;
        prep         := Prep;
        compile      := Compile;
        prepLV       := ExprRep.NotLValue;
        compileLV    := ExprRep.NotLValue;
        prepBR       := ExprRep.PrepNoBranch;
        compileBR    := ExprRep.NoBranch;
        evaluate     := Fold;
        isEqual      := EqCheck;
        getBounds    := ExprRep.NoBounds;
        isWritable   := ExprRep.IsNever;
        isDesignator := ExprRep.IsNever;
        isZeroes     := ExprRep.IsNever;
        genFPLiteral := ExprRep.NoFPLiteral;
        prepLiteral  := ExprRep.NoPrepLiteral;
        genLiteral   := ExprRep.NoLiteral;
        note_write   := ExprRep.NotWritable;
        exprAlign    := NarrowExprAlign;
      END;

PROCEDURE New (a: Expr.T;  t: Type.T): Expr.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    ExprRep.Init (p);
    p.origin  := a.origin;
    p.expr    := a;
    p.tipe    := t;
    p.type    := t;
    p.repType := Type.StripPacked (t);
    p.tmp     := NIL;
    RETURN p;
  END New;

PROCEDURE Check (p: P;  VAR cs: Expr.CheckState) =
  VAR info: Type.Info;
  BEGIN
    Expr.TypeCheck (p.expr, cs);
    p.tipe := Type.CheckInfo (p.tipe, info);
  END Check;

PROCEDURE EqCheck (a: P;  e: Expr.T;  x: M3.EqAssumption): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(b) => RETURN Type.IsEqual (a.tipe, b.tipe, x)
                 AND Expr.IsEqual (a.expr, b.expr, x);
    ELSE      RETURN FALSE;
    END;
  END EqCheck;

PROCEDURE NeedsAddress (p: P) =
  BEGIN
    Expr.NeedsAddress (p.expr);
  END NeedsAddress;

PROCEDURE NarrowExprAlign (p: P): Type.BitAlignT =
  (* Just inherit from operand expression. *)
  BEGIN
    RETURN Expr.Alignment(p.expr);
  END NarrowExprAlign;

PROCEDURE Prep (p: P) =
  BEGIN
    Expr.Prep (p.expr);
    Expr.Compile (p.expr);
    IF Host.doNarrowChk THEN Narrow.Emit (p.tipe, Expr.TypeOf (p.expr)); END;
    p.tmp := CG.Pop ();
  END Prep;

PROCEDURE Compile (p: P) =
  (* all the work was done by "Prep" *)
  BEGIN
    CG.Push (p.tmp);
    CG.Boost_addr_alignment (Target.Address.align);
    CG.Free (p.tmp);
    p.tmp := NIL;
  END Compile;

PROCEDURE Fold (p: P): Expr.T =
  VAR e: Expr.T;
  BEGIN
    e := Expr.ConstValue (p.expr);
    IF (e = NIL) THEN RETURN NIL END;
    p.expr := e;
    IF NOT Host.doNarrowChk OR Type.IsSubtype (Expr.TypeOf (e), p.tipe) THEN
      RETURN e;
    END;
    RETURN p;
  END Fold;

BEGIN
END NarrowExpr.
