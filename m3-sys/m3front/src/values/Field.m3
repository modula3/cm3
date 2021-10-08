(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Field.m3                                              *)
(* Last modified on Wed Mar  1 08:43:31 PST 1995 by kalsow     *)
(*      modified on Fri Apr 20 06:47:07 1990 by muller         *)

MODULE Field;

IMPORT M3, CG, Value, ValueRep, Type, Expr, Error;
IMPORT AssignStmt, Module;

REVEAL
  T = Value.T BRANDED OBJECT
        index   : INTEGER;
        offset  : INTEGER;
        tipe    : Type.T;
        dfault  : Expr.T;
      OVERRIDES
        typeCheck   := TypeCheck;
        set_globals := SetGlobals;
        load        := ValueRep.NoLoader;
        declare     := ValueRep.Never;
        const_init  := ValueRep.NoInit;
        need_init   := ValueRep.Never;
        lang_init   := Compile;
        user_init   := ValueRep.NoInit;
        toExpr      := ValueRep.NoExpr;
        toType      := ValueRep.NoType;
        typeOf      := TypeOf;
        repTypeOf   := RepTypeOf;
        base        := ValueRep.Self;
        add_fp_tag  := AddFPTag;
        fp_type     := TypeOf;
      END;

(*EXPORTED:*)
PROCEDURE New (READONLY info: Info): Value.T =
  VAR t := NEW (T);
  BEGIN
    ValueRep.Init (t, info.name, Value.Class.Field);
    t.index  := info.index;
    t.offset := info.offset;
    t.tipe   := info.type;
    t.dfault := info.dfault;
    RETURN t;
  END New;

(*EXPORTED:*)
PROCEDURE Is (v: Value.T): BOOLEAN =
  BEGIN
    TYPECASE v OF
    | NULL =>  RETURN FALSE;
    | T    =>  RETURN TRUE;
    ELSE       RETURN FALSE;
    END;
  END Is;

(*EXPORTED:*)
PROCEDURE Split (field: Value.T;  VAR info: Info) =
  VAR t: T := field;
  BEGIN
    info.name   := t.name;
    info.index  := t.index;
    info.offset := t.offset;
    info.type   := t.tipe;
    info.dfault := t.dfault;
  END Split;

(*EXPORTED:*)
PROCEDURE SetOffset (field: Value.T;  newOffset: INTEGER) =
  VAR t: T := field;
  BEGIN
    t.offset := newOffset;
  END SetOffset;

(*EXPORTED:*)
PROCEDURE EmitDeclaration (field: Value.T) =
  VAR
    t: T := field;
    info : Type.Info;
  BEGIN
    EVAL Type.CheckInfo (t.tipe, info);
    Type.Compile (t.tipe);
    CG.Declare_field (t.name, t.offset, info.size, Type.GlobalUID (t.tipe));
  END EmitDeclaration;

(*EXPORTED:*)
PROCEDURE IsEqualList (a, b: Value.T;  x: Type.Assumption;
                       types: BOOLEAN): BOOLEAN =
  BEGIN
    WHILE (a # NIL) AND (b # NIL) DO
      IF NOT IsEqual (a, b, x, types) THEN RETURN FALSE END;
      a := a.next;  b := b.next;
    END;
    RETURN (a = NIL) AND (b = NIL);
  END IsEqualList;

(*EXPORTED:*)
PROCEDURE IsEqual (va, vb: Value.T;  x: Type.Assumption;
                   types: BOOLEAN): BOOLEAN =
  VAR a: T := va;  b: T := vb;
  BEGIN
    IF (a = NIL) OR (b = NIL) OR (a.name # b.name) OR (a.index # b.index) THEN
      RETURN FALSE;
    END;
    IF NOT types THEN RETURN TRUE; END;

    (* now, we'll do the harder type-based checks... *)
    RETURN Type.IsEqual (TypeOf (a), TypeOf (b), x)
       (* AND Expr.IsEqual (Expr.ConstValue (a.dfault),
                         Expr.ConstValue (b.dfault), x)*);
  END IsEqual;

(*Externally dispatched-to:*)
PROCEDURE TypeOf (t: T): Type.T =
  BEGIN
    IF (t.tipe = NIL) THEN t.tipe := Expr.TypeOf (t.dfault) END;
    RETURN t.tipe;
  END TypeOf;

(*Externally dispatched-to:*)
PROCEDURE RepTypeOf (t: T): Type.T =
  BEGIN
    IF t.tipe # NIL THEN RETURN t.tipe; END;
    RETURN Expr.RepTypeOf (t.dfault);
  END RepTypeOf;

(*Externally dispatched-to:*)
PROCEDURE TypeCheck (t: T;  VAR cs: Value.CheckState) =
  VAR info: Type.Info;
  BEGIN
    t.tipe := Type.CheckInfo (TypeOf (t), info);
    IF (info.isEmpty) THEN
      Error.ID (t.name, "A field's type may not be empty (2.2.4).");
    END;
    IF (info.class = Type.Class.OpenArray) THEN
      Error.ID (t.name, "A field's type may not be open array (2.2.3).");
    END;
    t.checked := TRUE;

    IF (t.dfault # NIL) THEN
      (* check for assignability!! *)
      AssignStmt.Check (t.tipe, t.dfault, cs);
      Expr.TypeCheck (t.dfault, cs);
      IF (Expr.ConstValue (t.dfault) = NIL) THEN
        Error.ID (t.name, "Default value of a field must be constant (2.2.4).");
      END;
      (* NOTE: we don't save the constant-folded version of the default,
         otherwise we'd lose references to large named constants. *)
    END;
  END TypeCheck;

(*EXPORTED:*)
PROCEDURE NameAnonConstr (VAR (*IN OUT*) o: Value.T; VAR cs: Value.CheckState) =
  VAR field: T := o;
  BEGIN
    IF field.dfault # NIL AND Module.IsInterface () THEN
      Expr.NameAnonConstr
        (field.dfault, Module.Name (NIL (*current*)), field.name, cs);
    END;
  END NameAnonConstr;

(*EXPORTED, Externally dispatched-to:*)
PROCEDURE Compile (t: T) =
  BEGIN
    Type.Compile (t.tipe);
 (* Expr.Compile (t.dfault); *)
  END Compile;

(*Externally dispatched-to:*)
PROCEDURE SetGlobals (<*UNUSED*> t: T) =
  BEGIN
    (* Type.SetGlobals (t.tipe); *)
    (* IF (t.dfault # NIL) THEN Type.SetGlobals (Expr.TypeOf (t.dfault)) END; *)
  END SetGlobals;

(*Externally dispatched-to:*)
PROCEDURE AddFPTag  (t: T;  VAR x: M3.FPInfo): CARDINAL =
  BEGIN
    ValueRep.FPStart (t, x, "FIELD ", 0, global := FALSE);
 (* No, a default value does not contribute to the type.
    IF (t.dfault # NIL) THEN
      M3Buf.PutText (x.buf, " := ");
      Expr.GenFPLiteral (t.dfault, x.buf);
    END;
 *)
    RETURN 1;
  END AddFPTag;

BEGIN
END Field.
