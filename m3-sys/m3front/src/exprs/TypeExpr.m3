(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: TypeExpr.m3                                           *)
(* Last modified on Fri Feb 24 16:47:54 PST 1995 by kalsow     *)
(*      modified on Tue Aug 28 06:03:04 1990 by muller         *)

MODULE TypeExpr;

IMPORT M3, M3ID, Expr, ExprRep, Type, Value, NamedExpr, QualifyExpr;

TYPE
  P = Expr.T BRANDED "TypeExpr.P"OBJECT
        t: Type.T;
      OVERRIDES
        typeOf       := TypeOf;
        check        := Check;
        need_addr    := ExprRep.NotAddressable;
        prep         := Compile;
        compile      := Compile;
        prepLV       := ExprRep.NotLValue;
        compileLV    := ExprRep.NotLValue;
        prepBR       := ExprRep.NotBoolean;
        compileBR    := ExprRep.NotBoolean;
        evaluate     := ExprRep.Self;
        isEqual      := EqCheck;
        getBounds    := ExprRep.NoBounds;
        isWritable   := ExprRep.IsNever;
        isDesignator := ExprRep.IsNever;
	isZeroes     := ExprRep.IsNever;
	genFPLiteral := ExprRep.NoFPLiteral;
	prepLiteral  := ExprRep.NoPrepLiteral;
	genLiteral   := ExprRep.NoLiteral;
        note_write   := ExprRep.NotWritable;
      END;

PROCEDURE New (t: Type.T): Expr.T =
  VAR p := NEW (P);
  BEGIN
    ExprRep.Init (p);
    p.t := t;
    p.type := NIL;
    RETURN p;
  END New;

PROCEDURE Split (e: Expr.T;  VAR t: Type.T): BOOLEAN =
  VAR obj: Value.T;  name: M3ID.T;
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(p) => t := p.t;  RETURN TRUE;
    ELSE
      IF (NamedExpr.Split (e, name, obj) OR QualifyExpr.Split (e, obj))
        AND (Value.ClassOf (obj) = Value.Class.Type)
        THEN  t := Value.ToType (obj);  RETURN TRUE;
        ELSE  RETURN FALSE;
      END;
    END;
  END Split;

PROCEDURE TypeOf (<*UNUSED*> p: P): Type.T =
  BEGIN
    RETURN NIL;
  END TypeOf;

PROCEDURE Check (p: P;  <*UNUSED*> VAR cs: Expr.CheckState) =
  BEGIN
    p.t := Type.Check (p.t);
  END Check;

PROCEDURE EqCheck (a: P;  e: Expr.T;  x: M3.EqAssumption): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(b) => RETURN Type.IsEqual (a.t, b.t, x);
    ELSE      RETURN FALSE;
    END;
  END EqCheck;

PROCEDURE Compile (<*UNUSED*> p: P) =
  BEGIN
    <*ASSERT FALSE*>
  END Compile;

BEGIN
END TypeExpr.
