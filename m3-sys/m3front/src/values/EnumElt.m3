(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: EnumElt.m3                                            *)
(* Last modified on Wed Mar  1 08:41:03 PST 1995 by kalsow     *)
(*      modified on Fri Apr 20 06:45:16 1990 by muller         *)

MODULE EnumElt;

IMPORT M3, M3ID, CG, Type, Value, ValueRep, Expr, EnumExpr, Target, TInt;

TYPE
  T = Value.T BRANDED OBJECT
	value  : Target.Int;
	parent : Type.T;
      OVERRIDES
        typeCheck   := Check;
        set_globals := ValueRep.NoInit;
        load        := Load;
        declare     := ValueRep.Never;
        const_init  := ValueRep.NoInit;
        need_init   := ValueRep.Never;
        lang_init   := ValueRep.NoInit;
        user_init   := ValueRep.NoInit;
	toExpr      := ToExpr;
	toType      := ValueRep.NoType;
        typeOf      := TypeOf;
        base        := ValueRep.Self;
        add_fp_tag  := AddFPTag;
        fp_type     := FPType;
      END;

PROCEDURE New (name: M3ID.T;  READONLY value: Target.Int;
                                             parent: Type.T): Value.T =
  VAR t := NEW (T);
  BEGIN
    ValueRep.Init (t, name, Value.Class.Expr);
    t.readonly := TRUE;
    t.value    := value;
    t.parent   := parent;
    RETURN t;
  END New;

PROCEDURE IsEqual (va, vb: Value.T): BOOLEAN =
  VAR a: T := va;  b: T := vb;
  BEGIN
    RETURN (a.name = b.name) AND TInt.EQ (a.value, b.value);
  END IsEqual;

PROCEDURE Check (<*UNUSED*> t: T;  <*UNUSED*> VAR cs: Value.CheckState) =
  BEGIN
    (* no checking needed *)
  END Check;

PROCEDURE Load (t: T) =
  BEGIN
    CG.Load_integer (t.value);
  END Load;

PROCEDURE ToExpr (t: T): Expr.T =
  BEGIN
    RETURN EnumExpr.New (t.parent, t.value);
  END ToExpr;

PROCEDURE TypeOf (t: T): Type.T =
  BEGIN
    RETURN t.parent;
  END TypeOf;

PROCEDURE AddFPTag (t: T;  VAR x: M3.FPInfo): CARDINAL =
  BEGIN
    ValueRep.FPStart (t, x, "ENUM-ELT ", 0, global := FALSE);
    RETURN 0;
  END AddFPTag;

PROCEDURE FPType (<*UNUSED*> t: T): Type.T =
  BEGIN
    RETURN NIL;
  END FPType;

BEGIN
END EnumElt.
