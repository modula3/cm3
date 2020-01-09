(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: NamedExpr.m3                                          *)
(* Last modified on Fri Feb 24 16:44:46 PST 1995 by kalsow     *)
(*      modified on Fri Dec 21 01:22:10 1990 by muller         *)

MODULE NamedExpr;

IMPORT M3, M3ID, Expr, ExprRep, Value, Target;
IMPORT Type, Variable, VarExpr, ProcExpr, Scanner;
IMPORT Scope, Error, ErrType, TInt, CG, Host, RunTyme;

TYPE
  P = Expr.T BRANDED "Named Expr" OBJECT
        scope       : Scope.T;
        value       : Value.T;
        name        : M3ID.T;
        inFold      : BOOLEAN;
        inIsZeroes  : BOOLEAN;
        inGetBounds : BOOLEAN;
        inTypeOf    : BOOLEAN;
        tmp         : CG.Val;
      OVERRIDES
        typeOf       := TypeOf;
        check        := Check;
        need_addr    := NeedsAddress;
        prep         := Prep;
        compile      := Compile;
        prepLV       := PrepLV;
        compileLV    := CompileLV;
        prepBR       := ExprRep.PrepNoBranch;
        compileBR    := ExprRep.NoBranch;
        evaluate     := Fold;
        isEqual      := EqCheck;
        getBounds    := Bounder;
        isWritable   := IsWritable;
        isDesignator := IsDesignator;
        isZeroes     := IsZeroes;
        genFPLiteral := ExprRep.NoFPLiteral;
        prepLiteral  := ExprRep.NoPrepLiteral;
        genLiteral   := ExprRep.NoLiteral;
        note_write   := NoteWrites;
        use          := Use;
      END;

VAR cache := ARRAY [0..31] OF P { NIL, .. };

PROCEDURE New (name: M3ID.T;  value: Value.T): Expr.T =
  VAR p: P;  cur_scope := Scope.Top ();    hash := name MOD NUMBER (cache);
  BEGIN
(*
    (* check for a cache hit... *)
    p := cache[hash];
    IF (p # NIL) AND (p.name = name)
      AND (p.scope = cur_scope)
      AND (p.value = value) THEN
      RETURN p;
    END;
*)

    (* build a new node *)
    p := NEW (P);
    ExprRep.Init (p);
    p.scope       := cur_scope;
    p.name        := name;
    p.value       := value;
    p.inFold      := FALSE;
    p.inIsZeroes  := FALSE;
    p.inGetBounds := FALSE;
    p.inTypeOf    := FALSE;
    p.tmp         := NIL;
    cache[hash] := p;

    RETURN p;
  END New;

PROCEDURE FromValue (value: Value.T): Expr.T =
  VAR p := NEW (P);
  BEGIN
    ExprRep.Init (p);
    p.scope       := NIL;
    p.name        := Value.CName (value);
    p.value       := value;
    p.inFold      := FALSE;
    p.inIsZeroes  := FALSE;
    p.inGetBounds := FALSE;
    p.inTypeOf    := FALSE;
    p.tmp         := NIL;
    RETURN p;
  END FromValue;

PROCEDURE Split (e: Expr.T;  VAR name: M3ID.T;  VAR obj: Value.T): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL =>
        RETURN FALSE;
    | P(p) =>
        IF (p.value = NIL) THEN Resolve (p) END;
        name := p.name;  obj := p.value;
        RETURN TRUE;
    ELSE
        RETURN FALSE;
    END;
  END Split;

PROCEDURE SplitName (e: Expr.T;  VAR name: M3ID.T): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(p) => name := p.name;  RETURN TRUE;
    ELSE      RETURN FALSE;
    END;
  END SplitName;

PROCEDURE Resolve (p: P) =
  VAR save: INTEGER;
  BEGIN
    IF (p.value = NIL) THEN
      p.value := Scope.LookUp (p.scope, p.name, FALSE);
      IF (p.value = NIL) THEN
        save := Scanner.offset;
        Scanner.offset := p.origin;
        Error.ID (p.name, "undefined");
        p.value := VarExpr.Obj (VarExpr.New (ErrType.T, p.name));
        Scanner.offset := save;
      END;
    END;
  END Resolve;

PROCEDURE TypeOf (p: P): Type.T =
  VAR t: Type.T;
  BEGIN
    IF (p.value = NIL) THEN Resolve (p) END;
    IF (p.inTypeOf) THEN
      Value.IllegalRecursion (p.value);
      RETURN ErrType.T;
    END;
    p.inTypeOf := TRUE;
    t := Value.TypeOf (p.value);
    p.inTypeOf := FALSE;
    RETURN t;
  END TypeOf;

PROCEDURE Check (p: P;  VAR cs: Expr.CheckState) =
  BEGIN
    IF (p.value = NIL) THEN Resolve (p) END;
    Value.TypeCheck (p.value, cs);
    p.type := Value.TypeOf (p.value);
    p.value := Value.Base (p.value);
  END Check;

PROCEDURE EqCheck (a: P;  e: Expr.T;  <*UNUSED*> x: M3.EqAssumption): BOOLEAN =
  BEGIN
    IF (a.value = NIL) THEN Resolve (a) END;
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(b) => IF (b.value = NIL) THEN Resolve (b) END;
              RETURN (Value.Base (a.value) = Value.Base (b.value));
    ELSE      RETURN FALSE;
    END;
  END EqCheck;

PROCEDURE NeedsAddress (p: P) =
  BEGIN
    IF (p.value = NIL) THEN Resolve (p) END;
    CASE Value.ClassOf (p.value) OF
    | Value.Class.Expr => Expr.NeedsAddress (Value.ToExpr (p.value));
    | Value.Class.Var  => Variable.NeedsAddress (p.value);
    ELSE                  <*ASSERT FALSE*>
    END;
  END NeedsAddress;

PROCEDURE Prep (p: P) =
  VAR
    t: Type.T; info: Type.Info;
    global, indirect, lhs: BOOLEAN;
  BEGIN
    IF (p.value = NIL) THEN Resolve (p) END;
    IF Host.doIncGC AND Value.ClassOf (p.value) = Value.Class.Var THEN
      Variable.Split (p.value, t, global, indirect, lhs);
      EVAL Type.CheckInfo (t, info);
      IF info.isTraced AND (global OR indirect) THEN
        CASE info.class OF 
        | Type.Class.Object, Type.Class.Opaque, Type.Class.Ref =>
          Variable.Load (p.value);
          RunTyme.EmitCheckLoadTracedRef ();
          p.tmp := CG.Pop ();
        ELSE
          (* no check *)
        END
      END
    END
  END Prep;

PROCEDURE Compile (p: P) =
  BEGIN
    IF p.tmp = NIL THEN
      Value.Load (p.value);
    ELSE
      CG.Push (p.tmp);
      CG.Free (p.tmp);
      p.tmp := NIL;
    END
  END Compile;

PROCEDURE PrepLV (p: P; <*UNUSED*> traced: BOOLEAN) =
  BEGIN
    IF (p.value = NIL) THEN Resolve (p) END;
  END PrepLV;

PROCEDURE CompileLV (p: P; <*UNUSED*> traced: BOOLEAN) =
  BEGIN
    IF p.tmp = NIL THEN
      CASE Value.ClassOf (p.value) OF
      | Value.Class.Expr => Value.Load (p.value);
      | Value.Class.Var  => Variable.LoadLValue (p.value);
      ELSE <*ASSERT FALSE*>
      END;
    ELSE
      CG.Push (p.tmp);
      CG.Free (p.tmp);
      p.tmp := NIL;
    END
  END CompileLV;

PROCEDURE Bounder (p: P;  VAR min, max: Target.Int) =
  BEGIN
    IF (p.value = NIL) THEN Resolve (p) END;
    IF (p.inGetBounds) THEN
      Value.IllegalRecursion (p.value);
      min := TInt.Zero;
      max := TInt.One;
      RETURN;
    END;
    p.inGetBounds := TRUE;
    CASE Value.ClassOf (p.value) OF
    | Value.Class.Expr => Expr.GetBounds (Value.ToExpr (p.value), min, max);
    | Value.Class.Var  => Variable.GetBounds (p.value, min, max);
    ELSE                  EVAL Type.GetBounds (p.type, min, max);
    END;
    p.inGetBounds := FALSE;
  END Bounder;

PROCEDURE Fold (p: P): Expr.T =
  VAR e: Expr.T;
  BEGIN
    IF (p.value = NIL) THEN Resolve (p) END;
    IF (p.inFold) THEN Value.IllegalRecursion (p.value);  RETURN NIL END;
    p.inFold := TRUE;
    CASE Value.ClassOf (p.value) OF
    | Value.Class.Expr      => e := Expr.ConstValue (Value.ToExpr (p.value));
    | Value.Class.Procedure => e := ProcExpr.New (p.value);
    | Value.Class.Type      => e := NIL; (*TypeExpr.New (Value.ToType (p.value));*)
    ELSE                       e := NIL;
    END;
    p.inFold := FALSE;
    RETURN e;
  END Fold;

PROCEDURE IsDesignator (p: P;  <*UNUSED*> lhs: BOOLEAN): BOOLEAN =
  BEGIN
    IF (p.value = NIL) THEN Resolve (p) END;
    RETURN (Value.ClassOf (p.value) = Value.Class.Var);
  END IsDesignator;

PROCEDURE IsWritable (p: P;  lhs: BOOLEAN): BOOLEAN =
  BEGIN
    IF (p.value = NIL) THEN Resolve (p) END;
    RETURN Value.IsWritable (p.value, lhs);
  END IsWritable;

PROCEDURE IsZeroes (p: P;  <*UNUSED*> lhs: BOOLEAN): BOOLEAN =
  VAR b: BOOLEAN;
  BEGIN
    IF (p.value = NIL) THEN Resolve (p) END;
    IF (p.inIsZeroes) THEN Value.IllegalRecursion (p.value); RETURN TRUE END;
    p.inIsZeroes := TRUE;
    b := (Value.ClassOf (p.value) = Value.Class.Expr) 
         AND Expr.IsZeroes (Value.ToExpr (p.value));
    p.inIsZeroes := FALSE;
    RETURN b;
  END IsZeroes;

PROCEDURE NoteWrites (p: P) =
  BEGIN
    IF (p.value = NIL) THEN Resolve (p) END;
    IF (Value.ClassOf (p.value) = Value.Class.Var) THEN
      Variable.ScheduleTrace (p.value);
    END;
  END NoteWrites;

(* Externally dispatched-to: *)
PROCEDURE Use (p: P): BOOLEAN =
  VAR base: Expr.T;
  BEGIN
    <* ASSERT p.checked *>
    base := Value.ToExpr (p.value);
    RETURN Expr.Use (base);
  END Use;

BEGIN
END NamedExpr.
