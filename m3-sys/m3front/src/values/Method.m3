(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Method.m3                                             *)
(* Last modified on Wed Mar  1 08:44:03 PST 1995 by kalsow     *)
(*      modified on Fri Mar 22 08:34:06 1991 by muller         *)

MODULE Method;

IMPORT M3, Value, ValueRep, Type, Scope, Expr, UserProc;
IMPORT Error, ProcType, Procedure, Null, M3Buf;
IMPORT Module, Formal;

TYPE
  T = Value.T BRANDED OBJECT
        offset    : INTEGER;
        override  : BOOLEAN;
        parent    : Type.T;
        signature : Type.T;
        dfaultE   : Expr.T;
        dfault    : Value.T;
      OVERRIDES
        typeCheck   := Check;
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
        repTypeOf   := TypeOf;
        base        := ValueRep.Self;
        add_fp_tag  := AddFPTag;
        fp_type     := FPType;
      END;

PROCEDURE New (READONLY info: Info): Value.T =
  VAR t := NEW (T);
  BEGIN
    ValueRep.Init (t, info.name, Value.Class.Method);
    t.readonly   := TRUE;
    t.offset     := info.offset;
    t.override   := info.override;
    t.parent     := info.parent;
    t.signature  := info.signature;
    t.dfaultE    := info.dfault;
    t.dfault     := NIL;
    Scope.Insert (t);
    RETURN t;
  END New;

PROCEDURE Split (method: Value.T;  VAR info: Info): BOOLEAN =
  BEGIN
    TYPECASE method OF
    | NULL => RETURN FALSE;

    | T(t) =>
              info.name      := t.name;
              info.offset    := t.offset;
              info.parent    := t.parent;
              info.signature := t.signature;
              info.dfault    := t.dfaultE;
              info.override  := t.override;
              RETURN TRUE;

    ELSE RETURN FALSE;
    END;
  END Split;

PROCEDURE SplitX (method: Value.T;  VAR info: Info) =
  VAR b := Split (method, info);
  BEGIN
    <* ASSERT b *>
  END SplitX;

PROCEDURE NoteOverride (newV, oldV: Value.T) =
  VAR new: T := newV;  old: T := oldV;
  BEGIN
    <* ASSERT new.override *>
    <* ASSERT old.signature # NIL *>
    new.signature := old.signature;
    new.offset    := old.offset;
  END NoteOverride;

PROCEDURE ResolveDefault (t: T) =
  VAR default_type: Type.T;
  BEGIN
    IF (t.dfault # NIL) THEN RETURN END;
    IF (t.dfaultE = NIL) THEN RETURN END;
    IF UserProc.IsProcedureLiteral (t.dfaultE, t.dfault) THEN RETURN END;
    default_type := Expr.TypeOf (t.dfaultE); 
    IF Type.IsEqual (default_type, Null.T, NIL) THEN
      RETURN; (* ok *)
    ELSIF NOT ProcType.Is (default_type) THEN
      Error.ID (t.name, "Method default must be a procedure (2.2.9).");
    ELSE
      Error.ID (t.name, "Method default must be a procedure constant (2.2.9).");
    END;
  END ResolveDefault;

PROCEDURE IsEqualList (a, b: Value.T;  x: Type.Assumption;
                       types: BOOLEAN): BOOLEAN =
  BEGIN
    WHILE (a # NIL) AND (b # NIL) DO
      IF NOT IsEqual (a, b, x, types) THEN RETURN FALSE END;
      a := a.next;  b := b.next;
    END;
    RETURN (a = NIL) AND (b = NIL);
  END IsEqualList;

PROCEDURE IsEqual (va, vb: Value.T;  x: Type.Assumption; types: BOOLEAN): BOOLEAN =
  VAR a: T := va;  b: T := vb;
  BEGIN
    IF (a = NIL) OR (b = NIL) OR (a.name # b.name) OR (a.override # b.override) THEN
      RETURN FALSE;
    END;
    IF NOT types THEN RETURN TRUE; END;

    (* now, we'll do the harder type-based checks... *)
    ResolveDefault (a);
    ResolveDefault (b);
    RETURN Type.IsEqual (a.signature, b.signature, x)
       AND (Value.Base (a.dfault) = Value.Base (b.dfault)) (*CHEAT, BUG!*);
  END IsEqual;

PROCEDURE Check (t: T;  VAR cs: Value.CheckState) =
  VAR proc: Value.T;  procType: Type.T;
  BEGIN
    IF (t.signature # NIL) THEN
      t.signature := Type.Check (t.signature);
      IF Module.IsInterface () THEN
        Formal.NameDefaultConstructors (t.signature, t.name, cs);
      END;
    END;

    IF (t.dfaultE # NIL) THEN
      Expr.TypeCheck (t.dfaultE, cs);
      ResolveDefault (t);
    END;

    proc := t.dfault;
    IF (proc # NIL) THEN
      Value.TypeCheck (proc, cs);
      procType := Value.TypeOf (proc);
      IF (procType = Null.T) THEN
        t.dfault := NIL;
      ELSIF (Value.ClassOf (proc) # Value.Class.Procedure) THEN
        Error.ID (t.name, "Method default must be a procedure (2.2.9).");
      ELSIF Procedure.IsNested (proc) THEN
        Error.ID
          (t.name, "Method default must not be a nested procedure (2.2.9).");
      ELSIF NOT ProcType.IsCompatible (procType, t.parent, t.signature) THEN
        Error.ID
          (t.name, "Method default must be compatible with method signature  (2.2.9).");
      END;
    END;
  END Check;

PROCEDURE TypeOf (t: T): Type.T =
  BEGIN
    RETURN t.signature;
  END TypeOf;

PROCEDURE Compile (t: T) =
  BEGIN
    Type.Compile (t.signature);
  END Compile;

PROCEDURE SetGlobals (<*UNUSED*> t: T) =
  BEGIN
    (* Type.SetGlobals (t.signature); *)
    (* IF (t.dfaultE # NIL) THEN Type.SetGlobals (Expr.TypeOf (t.dfaultE)) END;*)
  END SetGlobals;

PROCEDURE AddFPTag  (t: T;  VAR x: M3.FPInfo): CARDINAL =
  CONST Tags = ARRAY BOOLEAN OF TEXT { "METHOD ", "OVERRIDE " };
  CONST Cnt = ARRAY BOOLEAN OF INTEGER { 1, 0 };
  BEGIN
    ValueRep.FPStart (t, x, Tags[t.override], 0, global := FALSE);
    IF (t.dfault # NIL) THEN
      M3Buf.PutText (x.buf, " := ");
      Expr.GenFPLiteral (t.dfaultE, x.buf);
    END;
    RETURN Cnt [t.override];
  END AddFPTag;

PROCEDURE FPType (t: T): Type.T =
  BEGIN
    IF (t.override)
      THEN RETURN NIL;
      ELSE RETURN t.signature;
    END;
  END FPType;

BEGIN
END Method.
