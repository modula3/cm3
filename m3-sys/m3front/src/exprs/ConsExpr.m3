(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ConsExpr.m3                                           *)
(* Last modified on Tue Jun 20 15:46:56 PDT 1995 by kalsow     *)
(*      modified on Thu Jun 15 14:17:19 PDT 1995 by ericv      *)
(*      modified on Fri Dec 14 21:41:11 1990 by muller         *)

MODULE ConsExpr;
(* A value constructor. Has an array, record, or set constructor
   as a child. *) 

IMPORT M3, Expr, ExprRep, Error, Type;
IMPORT TypeExpr, SetExpr, RecordExpr, ArrayExpr;

TYPE
  Kind = { Unknown, Record, Set, Array };

TYPE
  P = Expr.T BRANDED "ConsExpr.P" OBJECT
        typeExpr : Expr.T; (* An *expression* for the type being constructed. *)
        args     : Expr.List;
        dots     : BOOLEAN;
        base     : Expr.T;
        kind     : Kind;
      OVERRIDES
        typeOf       := TypeOf;
        check        := Check;
        need_addr    := NeedsAddress;
        prep         := Prep;
        compile      := Compile;
        prepLV       := ExprRep.NotLValue;
        compileLV    := ExprRep.NotLValue;
        prepBR       := ExprRep.NotBoolean;
        compileBR    := ExprRep.NotBoolean;
        evaluate     := Fold;
        isEqual      := EqCheck;
        getBounds    := ExprRep.NoBounds;
        isWritable   := ExprRep.IsNever;
        isDesignator := ExprRep.IsNever;
        isZeroes     := IsZeroes;
        genFPLiteral := ExprRep.NoFPLiteral;
        prepLiteral  := ExprRep.NoPrepLiteral;
        genLiteral   := ExprRep.NoLiteral;
        note_write   := ExprRep.NotWritable;
      END;

PROCEDURE New (typeExpr: Expr.T;  args: Expr.List;  dots: BOOLEAN): Expr.T =
  VAR p := NEW (P);
  BEGIN
    ExprRep.Init (p);
    p.typeExpr  := typeExpr;
    p.args      := args;
    p.dots      := dots;
    p.base      := NIL;
    p.kind      := Kind.Unknown;
    p.direct_ok := TRUE;
    RETURN p;
  END New;

PROCEDURE Is (e: Expr.T): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P    => RETURN TRUE;
    ELSE      RETURN FALSE;
    END;   
  END Is;

PROCEDURE Base (e: Expr.T): Expr.T =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN NIL;
    | P(p) => RETURN p.base;
    ELSE      RETURN NIL;
    END;   
  END Base;

PROCEDURE TypeOf (p: P): Type.T =
  VAR ta: Type.T;
  BEGIN
    IF TypeExpr.Split (p.typeExpr, ta)
      THEN RETURN ta;
      ELSE RETURN Expr.TypeOf (p.typeExpr);
    END;
  END TypeOf;

PROCEDURE Seal (p: P) =
  VAR ta: Type.T;  info: Type.Info;
  BEGIN
    IF (p.base # NIL) THEN RETURN END;
    IF NOT TypeExpr.Split (p.typeExpr, ta) THEN RETURN END;
    ta := Type.Base (ta);  (* strip any BITS FOR packing *)
    ta := Type.CheckInfo (ta, info);
    IF (ta = NIL) THEN
      (* error *)
    ELSIF (info.class = Type.Class.Record) THEN
      p.base := RecordExpr.New (ta, p.args);
      p.kind := Kind.Record;
    ELSIF (info.class = Type.Class.Set) THEN
      p.base := SetExpr.New (ta, p.args);
      p.kind := Kind.Set;
    ELSIF (info.class = Type.Class.Array)
       OR (info.class = Type.Class.OpenArray) THEN
      p.base := ArrayExpr.New (ta, p.args, p.dots);
      p.kind := Kind.Array;
    END;
  END Seal;

PROCEDURE Check (p: P;  VAR cs: Expr.CheckState) =
  BEGIN
    Seal (p);
    Expr.TypeCheck (p.typeExpr, cs);
    p.type := TypeOf (p);
    IF (p.kind = Kind.Unknown) THEN
      Error.Msg ("constructor type must be array, record, or set type");
    ELSIF (p.dots) AND (p.kind # Kind.Array) THEN
      Error.Msg ("trailing \'..\' in constructor, ignored");
    END;
    FOR i := 0 TO LAST (p.args^) DO Expr.TypeCheck (p.args[i], cs) END;
    Expr.TypeCheck (p.base, cs);
  END Check;

PROCEDURE EqCheck (a: P;  e: Expr.T;  x: M3.EqAssumption): BOOLEAN =
  BEGIN
    Seal (a);
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(b) => Seal (b);  RETURN Expr.IsEqual (a.base, b.base, x);
    ELSE      RETURN Expr.IsEqual (a.base, e, x);
    END;
  END EqCheck;

PROCEDURE NeedsAddress (p: P) =
  BEGIN
    Seal (p);
    Expr.NeedsAddress (p.base);
  END NeedsAddress;

PROCEDURE Prep (p: P) =
  VAR t: Type.T;
  BEGIN
    Seal (p);
    IF TypeExpr.Split (p.typeExpr, t) THEN Type.Compile (t) END;
    Expr.Prep (p.base);
  END Prep;

PROCEDURE Compile (p: P) =
  BEGIN
    Expr.Compile (p.base);
  END Compile;

PROCEDURE Fold (p: P): Expr.T =
  BEGIN
    Seal (p);
    RETURN Expr.ConstValue (p.base);
  END Fold;

PROCEDURE IsZeroes (p: P;  <*UNUSED*> lhs: BOOLEAN): BOOLEAN =
  BEGIN
    Seal (p);
    RETURN Expr.IsZeroes (p.base);
  END IsZeroes;

BEGIN
END ConsExpr.
