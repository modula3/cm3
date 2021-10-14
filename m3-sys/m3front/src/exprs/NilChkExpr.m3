(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: NilChkExpr.m3                                         *)
(* Last Modified On Thu Jun 30 08:50:41 PDT 1994 By kalsow     *)
(*      Modified On Thu Nov 29 03:28:30 1990 By muller         *)

MODULE NilChkExpr;

IMPORT CG, Expr, ExprRep, Type, AddressExpr;
IMPORT Target, TInt, RefType, Host, OpenArrayType, M3RT;

TYPE
  P = ExprRep.Ta BRANDED "NilChkExpr.P" OBJECT
        offset       : INTEGER;
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
        isEqual      := ExprRep.EqCheckA;
        getBounds    := Bounder;
        isWritable   := ExprRep.IsNever;
        isDesignator := ExprRep.IsNever;
        isZeroes     := ExprRep.IsNever;
        genFPLiteral := ExprRep.NoFPLiteral;
        prepLiteral  := ExprRep.NoPrepLiteral;
        genLiteral   := ExprRep.NoLiteral;
        note_write   := NoteWrites;
      END;

PROCEDURE New (a: Expr.T): Expr.T =
  VAR p: P;
  BEGIN
    IF (NOT Host.doNilChk) OR (TYPECODE (a) = TYPECODE (P)) THEN RETURN a END;
    p := NEW (P);
    ExprRep.Init (p);
    p.origin := a.origin;
    p.a := a;
    p.offset := LAST (INTEGER);
    RETURN p;
  END New;

PROCEDURE SetOffset (a: Expr.T; offset: INTEGER) =
  BEGIN
    TYPECASE a OF
    | NULL => (* skip *)
    | P(p) => p.offset := MIN (p.offset, offset);
    ELSE      (* skip *)
    END;
  END SetOffset;

PROCEDURE TypeOf (p: P): Type.T =
  BEGIN
    RETURN Expr.TypeOf (p.a)
  END TypeOf;

PROCEDURE RepTypeOf (p: P): Type.T =
  BEGIN
    RETURN Expr.RepTypeOf (p.a)
  END RepTypeOf;

PROCEDURE Check (p: P;  VAR cs: Expr.CheckState) =
  VAR target: Type.T;  x: INTEGER;  info: Type.Info;
  BEGIN
    Expr.TypeCheck (p.a, cs);
    p.type := Type.Check (Expr.TypeOf (p.a));
    IF RefType.Split (p.type, target) THEN 
      IF OpenArrayType.Is (target)
        THEN x := MIN (M3RT.OA_elt_ptr, M3RT.OA_size_0);
        ELSE EVAL Type.CheckInfo (target, info);  x := info.size;
      END;
      p.offset := MIN (p.offset, x);
    END;
  END Check;

PROCEDURE Prep (p: P) =
  BEGIN
    Expr.Prep (p.a);
  END Prep;

PROCEDURE Compile (p: P; StaticOnly: BOOLEAN) =
  VAR EasyCheck := Target.First_readable_addr * Target.Char.size;
  BEGIN
    <* ASSERT NOT StaticOnly *>
    Expr.Compile (p.a);
    IF (p.offset < 0) OR (EasyCheck <= p.offset) THEN
      CG.Check_nil (CG.RuntimeError.BadMemoryReference);
    END;
  END Compile;

PROCEDURE Fold (p: P): Expr.T =
  VAR e: Expr.T;  i: Target.Int;
  BEGIN
    e := Expr.ConstValue (p.a);
    IF (e = NIL) THEN RETURN NIL END;
    IF  NOT AddressExpr.Split (e, i) THEN RETURN NIL END;
    IF TInt.EQ (i, TInt.Zero) THEN RETURN NIL END;
    RETURN e;
  END Fold;

PROCEDURE Bounder (p: P;  VAR min, max: Target.Int) =
  BEGIN
    Expr.GetBounds (p.a, min, max);
  END Bounder;

PROCEDURE NoteWrites (p: P) =
  BEGIN
    Expr.NoteWrite (p.a);
  END NoteWrites;

BEGIN
END NilChkExpr.
