(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: DerefExpr.m3                                          *)
(* Last modified on Fri Feb 24 07:43:48 PST 1995 by kalsow     *)
(*      modified on Thu Nov 29 06:04:10 1990 by muller         *)

MODULE DerefExpr;

IMPORT Expr, ExprRep, RefType, Error, Type;
IMPORT NilChkExpr, CG, ErrType;

TYPE
  P = ExprRep.Ta BRANDED "DerefExpr.P" OBJECT
      OVERRIDES
        typeOf       := TypeOf;
        check        := Check;
        need_addr    := NeedsAddress;
        prep         := Prep;
        compile      := Compile;
        prepLV       := Prep;
        compileLV    := CompileLV;
        prepBR       := ExprRep.PrepNoBranch;
        compileBR    := ExprRep.NoBranch;
        evaluate     := ExprRep.NoValue;
        isEqual      := ExprRep.EqCheckA;
        getBounds    := ExprRep.NoBounds;
        isWritable   := ExprRep.IsAlways;
        isDesignator := ExprRep.IsAlways;
	isZeroes     := ExprRep.IsNever;
	genFPLiteral := ExprRep.NoFPLiteral;
	prepLiteral  := ExprRep.NoPrepLiteral;
	genLiteral   := ExprRep.NoLiteral;
        note_write   := NoteWrites;
      END;

PROCEDURE New (a: Expr.T): Expr.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    ExprRep.Init (p);
    p.a := NilChkExpr.New (a);
    p.origin := p.a.origin;
    RETURN p;
  END New;

PROCEDURE SetOffset (e: Expr.T; n: INTEGER) =
  BEGIN
    TYPECASE e OF
    | NULL => (* nothing *)
    | P(p) => NilChkExpr.SetOffset (p.a, n);
    ELSE      (* nothing *)
    END;
  END SetOffset;

PROCEDURE TypeOf (p: P): Type.T =
  VAR ta, target: Type.T;
  BEGIN
    ta := Expr.TypeOf (p.a);
    IF RefType.Split (ta, target)
      THEN RETURN target;
      ELSE RETURN ErrType.T;
    END;
  END TypeOf;

PROCEDURE Check (p: P;  VAR cs: Expr.CheckState) =
  VAR tx, ta, target: Type.T;  err0, err1, warn: INTEGER;
  BEGIN
    Error.Count (err0, warn);
    Expr.TypeCheck (p.a, cs);
    tx := Expr.TypeOf (p.a);
    Error.Count (err1, warn);
    ta := Type.Base (tx);
    target := NIL;
    IF ((tx = NIL) OR (tx = ErrType.T)) AND (err0 # err1) THEN
      (* already an error, don't generate any more *)
      target := ErrType.T;
    ELSIF NOT RefType.Split (ta, target) THEN
      Error.Msg ("cannot dereference a non-REF value");
      target := ErrType.T;
    ELSIF (target = NIL) THEN
      Error.Msg ("cannot dereference REFANY, ADDRESS, or NULL");
      target := ErrType.T;
    END;
    p.type := target;
  END Check;

PROCEDURE NeedsAddress (<*UNUSED*> p: P) =
  BEGIN
    (* ok *)
  END NeedsAddress;

PROCEDURE Prep (p: P) =
  BEGIN
    Expr.Prep (p.a);
  END Prep;

PROCEDURE Compile (p: P) =
  VAR t := p.type;  info: Type.Info;
  BEGIN
    Expr.Compile (p.a);
    EVAL Type.CheckInfo (t, info);
    CG.Force ();  (*'cause alignment applies to the referent, not the pointer*)
    CG.Boost_alignment (info.alignment);
    Type.LoadScalar (t);
  END Compile;

PROCEDURE CompileLV (p: P) =
  VAR info: Type.Info;
  BEGIN
    Expr.Compile (p.a);
    EVAL Type.CheckInfo (p.type, info);
    CG.Force ();  (*'cause alignment applies to the referent, not the pointer*)
    CG.Boost_alignment (info.alignment);
  END CompileLV;

PROCEDURE NoteWrites (p: P) =
  BEGIN
    Expr.NoteWrite (p.a);
  END NoteWrites;

BEGIN
END DerefExpr.
