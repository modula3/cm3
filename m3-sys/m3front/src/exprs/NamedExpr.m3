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
IMPORT MSIR, MSIRBuilder, CaptureAnalysis;
IMPORT ArrayExpr;

TYPE
  P = Expr.T BRANDED "Named Expr" OBJECT
        scope       : Scope.T := NIL;
        value       : Value.T := NIL;
        name        : M3ID.T  := M3ID.NoID;
        inFold      : BOOLEAN := FALSE;
        inIsZeroes  : BOOLEAN := FALSE;
        inGetBounds : BOOLEAN := FALSE;
        inTypeOf    : BOOLEAN := FALSE;
        tmp         : CG.Val  := NIL;
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
        checkUseFailure := CheckUseFailure;
        capture  := Capture;
        captureLV := CaptureLV;
        compileMSIR       := CompileMSIR;
        compileLValueMSIR := LValueMSIR;
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
    p := NEW (P, name := name, value := value, scope := cur_scope);
    ExprRep.Init (p);
    cache[hash] := p;

    RETURN p;
  END New;

PROCEDURE FromValue (value: Value.T): Expr.T =
  VAR p := NEW (P, value := value, name := Value.CName (value));
  BEGIN
    ExprRep.Init (p);
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
  BEGIN
    IF p.value = NIL THEN Resolve (p) END;
    IF p.type = NIL THEN
      IF p.inTypeOf THEN
        Value.IllegalRecursion (p.value);
        p.type := ErrType.T;
        p.repType := ErrType.T;
      END;
      p.inTypeOf := TRUE;
      p.type := Value.TypeOf (p.value);
      p.inTypeOf := FALSE;
    END;
    RETURN p.type;
  END TypeOf;

PROCEDURE RepTypeOf (p: P): Type.T =
  BEGIN
    IF p.value = NIL THEN Resolve (p) END;
    IF p.repType = NIL THEN
      IF p.inTypeOf THEN
        Value.IllegalRecursion (p.value);
        p.type := ErrType.T;
        p.repType := ErrType.T;
      END;
      p.inTypeOf := TRUE;
      p.repType := Value.RepTypeOf (p.value);
      p.inTypeOf := FALSE;
    END;
    RETURN p.repType;
  END RepTypeOf;

PROCEDURE Check (p: P;  VAR cs: Expr.CheckState) =
  BEGIN
    IF (p.value = NIL) THEN Resolve (p) END;
    Value.TypeCheck (p.value, cs);
    p.type := Value.TypeOf (p.value);
    p.repType := Value.RepTypeOf (p.value);
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

PROCEDURE Compile (p: P; StaticOnly: BOOLEAN) =
  BEGIN
    IF NOT StaticOnly THEN
      IF p.tmp = NIL THEN
        Value.Load (p.value);
      ELSE
        CG.Push (p.tmp);
        CG.Free (p.tmp);
        p.tmp := NIL;
      END
    END
  END Compile;

PROCEDURE CompileMSIR (p: P): MSIR.Value =
  VAR constExpr: Expr.T;  folded: Expr.T;
  BEGIN
    IF p.value = NIL THEN Resolve (p) END;
    TYPECASE p.value OF
    | Variable.T(vv) =>
        VAR v := MSIRBuilder.LookupVar (vv);
        BEGIN
          IF v = NIL THEN
            Variable.RegisterExternMSIR (vv);
            v := MSIRBuilder.LookupVar (vv);
          END;
          IF v = NIL THEN
            MSIRBuilder.Abandon ("unbound variable reference: "
                                 & Value.GlobalName (vv));
            RETURN NIL;
          END;
          RETURN v;
        END;
    ELSE
      CASE Value.ClassOf (p.value) OF
      | Value.Class.Expr =>
          constExpr := Value.ToExpr (p.value);
          IF constExpr # NIL THEN
            (* For CONST arrays, try LValueMSIR (→ MaterializeConstArray) first so
               open-typed nested array constants (b = ARRAY OF ARRAY OF INTEGER {c,...})
               produce const globals rather than abandoning.  p081. *)
            IF ArrayExpr.ArrayConstrExpr (constExpr) # NIL THEN
              VAR lv := LValueMSIR (p);
              BEGIN
                IF lv # NIL THEN
                  RETURN MSIR.BuildLoad (MSIRBuilder.CurrentBlock (), "",
                                        MSIR.EltType (MSIR.ValueType (lv)), lv);
                END;
              END;
              IF MSIRBuilder.IsAbandoned () THEN RETURN NIL END;
            END;
            RETURN Expr.CompileMSIR (constExpr);
          END;
      | Value.Class.Procedure =>
          folded := Fold (p);
          IF folded # NIL THEN RETURN Expr.CompileMSIR (folded) END;
      ELSE (* skip *)
      END;
      MSIRBuilder.Abandon ("named-expr value is not a Variable");
      RETURN NIL;
    END;
  END CompileMSIR;

PROCEDURE LValueMSIR (p: P): MSIR.Value =
  BEGIN
    IF p.value = NIL THEN Resolve (p) END;
    TYPECASE p.value OF
    | Variable.T(vv) =>
        (* Bit-field WITH alias has no plain lvalue; return NIL so AssignStmt
           routes the write through MSIRBuilder.TryBitFieldStore (InsertBitField)
           instead of Abandoning. *)
        IF MSIRBuilder.IsBitFieldVar (vv) THEN RETURN NIL END;
        VAR addr := MSIRBuilder.LookupVarAddr (vv);
        BEGIN
          IF addr = NIL THEN
            Variable.RegisterExternMSIR (vv);
            addr := MSIRBuilder.LookupVarAddr (vv);
          END;
          IF addr = NIL THEN
            MSIRBuilder.Abandon ("named lvalue: unbound variable reference: "
                                 & Value.GlobalName (vv));
            RETURN NIL;
          END;
          RETURN addr;
        END;
    ELSE
      IF Value.ClassOf (p.value) = Value.Class.Expr THEN
        VAR constExpr := Value.ToExpr (p.value);
        BEGIN
          IF constExpr # NIL THEN
            (* CONST ARRAY OF T — materialise as a private constant global. *)
            IF ArrayExpr.ArrayConstrExpr (constExpr) # NIL THEN
              (* Using a const array with a statically out-of-range element is a
                 CT-warned RT error; MaterializeConstArray skips the fault the
                 inline path emits, so raise it here at the use site (p270). *)
              ArrayExpr.EmitUseFailureMSIR (constExpr);
              RETURN MSIRBuilder.MaterializeConstArray (p.value, constExpr);
            END;
            (* Other named constants (record, set, scalar) referenced where an
               lvalue is needed (e.g. an aggregate passed by reference, or a
               record-field default): use the constructor's own lvalue when it
               has one (RecordExpr does), otherwise compile the value and spill
               it to a temp.  Returning a real address here — rather than
               abandoning — keeps the enclosing procedure intact. *)
            VAR lv := Expr.LValueMSIR (constExpr);
            BEGIN
              IF lv # NIL THEN RETURN lv END;
            END;
            VAR v := Expr.CompileMSIR (constExpr);
            BEGIN
              IF v # NIL THEN
                VAR b    := MSIRBuilder.CurrentBlock ();
                    slot := MSIR.BuildAlloca (b, "", MSIR.ValueType (v));
                BEGIN
                  MSIR.BuildStore (b, v, slot);
                  RETURN slot;
                END;
              END;
            END;
          END;
        END;
      END;
      MSIRBuilder.Abandon ("named lvalue is not a Variable or CONST array");
      RETURN NIL;
    END;
  END LValueMSIR;

(* If p names a bit-field WITH alias, write rhs through it (InsertBitField) and
   return TRUE; else FALSE.  Called by AssignStmt when the LHS has no lvalue. *)
PROCEDURE BitFieldStoreMSIR (e: Expr.T;  rhs: MSIR.Value): BOOLEAN =
  VAR p: P;
  BEGIN
    TYPECASE e OF
    | P (pp) => p := pp;
    ELSE        RETURN FALSE;
    END;
    IF p.value = NIL THEN Resolve (p) END;
    TYPECASE p.value OF
    | Variable.T (vv) => RETURN MSIRBuilder.TryBitFieldStore (vv, rhs);
    ELSE                 RETURN FALSE;
    END;
  END BitFieldStoreMSIR;

PROCEDURE PrepLV (p: P; <*UNUSED*> traced: BOOLEAN) =
  BEGIN
    IF (p.value = NIL) THEN Resolve (p) END;
  END PrepLV;

PROCEDURE CompileLV (p: P; <*UNUSED*> traced: BOOLEAN; StaticOnly: BOOLEAN) =
  BEGIN
    IF StaticOnly THEN RETURN END;
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
PROCEDURE CheckUseFailure (p: P): BOOLEAN =
  VAR base: Expr.T;
  BEGIN
    <* ASSERT p.checked *>
    base := Value.ToExpr (p.value);
    RETURN Expr.CheckUseFailure (base);
  END CheckUseFailure;

(* EXPORTED: *)
PROCEDURE Is (e: Expr.T): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P => RETURN TRUE;
    ELSE RETURN FALSE;
    END;
  END Is;

PROCEDURE NoteCapture (p: P;  ca: CaptureAnalysis.T;  written: BOOLEAN) =
  BEGIN
    IF p.value = NIL THEN Resolve (p) END;
    TYPECASE p.value OF
    | Variable.T(v) =>
        IF Variable.IsUpLevel (v) THEN CaptureAnalysis.Note (ca, v, written) END;
    ELSE
        (* Nested proc used as a value or called as a sibling: propagate its
           captures transitively so BuildClosureValue / EmitNestedCall can find
           them in the enclosing proc's varMap.
           Skip module-scope globals (t.global=TRUE): those are always accessible
           through LookupVar/LookupVarAddr via globalMap without lambda-lifting.
           Adding them as value captures would shadow globalMap lookups and break
           LookupVarAddr for callers that need write addresses (VAR params). *)
        VAR procCaps := MSIRBuilder.GetProcCaptures (p.value); BEGIN
          IF procCaps # NIL THEN
            FOR k := 0 TO NUMBER (procCaps^) - 1 DO
              VAR sv := procCaps[k].var;
                  svT: Type.T;  svG, svI, svL: BOOLEAN;
              BEGIN
                Variable.Split (sv, svT, svG, svI, svL);
                IF NOT svG THEN
                  CaptureAnalysis.Note (ca, sv, procCaps[k].written)
                END
              END
            END
          END
        END
    END;
  END NoteCapture;

PROCEDURE Capture (p: P;  ca: CaptureAnalysis.T) =
  BEGIN
    NoteCapture (p, ca, FALSE);
  END Capture;

PROCEDURE CaptureLV (p: P;  ca: CaptureAnalysis.T) =
  BEGIN
    NoteCapture (p, ca, TRUE);
  END CaptureLV;

BEGIN
END NamedExpr.
