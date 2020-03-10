(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: DerefExpr.m3                                          *)
(* Last modified on Fri Feb 24 07:43:48 PST 1995 by kalsow     *)
(*      modified on Thu Nov 29 06:04:10 1990 by muller         *)

MODULE DerefExpr;

IMPORT Expr, ExprRep, RefType, Error, Type, RunTyme;
IMPORT NilChkExpr, CG, ErrType, Host;

TYPE
  P = ExprRep.Ta BRANDED "DerefExpr.P" OBJECT
        tmp: CG.Val;
      OVERRIDES
        typeOf       := TypeOf;
        repTypeOf    := RepTypeOf;
        check        := Check;
        need_addr    := NeedsAddress;
        prep         := Prep;
        compile      := Compile;
        prepLV       := PrepLV;
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
    p.tmp := NIL;
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

PROCEDURE RepTypeOf (p: P): Type.T =
  VAR ta, target: Type.T;
  BEGIN
    ta := Expr.RepTypeOf (p.a);
    IF RefType.Split (ta, target)
      THEN RETURN target;
      ELSE RETURN ErrType.T;
    END;
  END RepTypeOf;

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
  VAR info: Type.Info;
  BEGIN
    Expr.Prep (p.a);
    IF Host.doIncGC THEN
      EVAL Type.CheckInfo (p.type, info);
      IF info.isTraced THEN
        CASE info.class OF
        | Type.Class.Object, Type.Class.Opaque, Type.Class.Ref =>
          Compile (p);
          RunTyme.EmitCheckLoadTracedRef ();
          p.tmp := CG.Pop ();
        ELSE
          (* no check *)
        END
      END
    END
  END Prep;

PROCEDURE Compile (p: P) =
  VAR t := p.type;  info: Type.Info;
  BEGIN
    IF p.tmp # NIL THEN
      CG.Push (p.tmp);
      CG.Free (p.tmp);
      p.tmp := NIL;
      RETURN;
    END;
    Expr.Compile (p.a);
    EVAL Type.CheckInfo (t, info);
    CG.ForceStacked ();
    (* ^'cause alignment applies to the referent, not the pointer*)
    CG.Boost_addr_alignment (info.alignment);
    Type.LoadScalar (t);
  END Compile;

PROCEDURE PrepLV (p: P; traced: BOOLEAN) =
  VAR info: Type.Info;
  BEGIN
    Expr.Prep (p.a);
    IF traced AND Host.doGenGC THEN
      EVAL Type.CheckInfo (p.type, info);
      IF NOT info.isTraced THEN RETURN END;
      EVAL Type.CheckInfo (Expr.TypeOf (p.a), info);
      IF NOT info.isTraced THEN RETURN END;
      Expr.Compile (p.a);
      RunTyme.EmitCheckStoreTraced ();
      p.tmp := CG.Pop ();
    END
  END PrepLV;

PROCEDURE CompileLV (p: P; traced: BOOLEAN) =
  VAR info: Type.Info;
  BEGIN
    IF p.tmp # NIL THEN
      <*ASSERT traced*>
      CG.Push (p.tmp);
      CG.Free (p.tmp);
      p.tmp := NIL;
    ELSE
      Expr.Compile (p.a);
    END;
    EVAL Type.CheckInfo (p.type, info);
    CG.ForceStacked ();
    (*^ Because alignment applies to the referent, not the pointer*)
    CG.Boost_addr_alignment (info.alignment);
  END CompileLV;

PROCEDURE NoteWrites (p: P) =
  BEGIN
    Expr.NoteWrite (p.a);
  END NoteWrites;

BEGIN
END DerefExpr.
