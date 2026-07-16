(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: VarExpr.m3                                            *)
(* Last modified on Fri Feb 24 16:48:13 PST 1995 by kalsow     *)
(*      modified on Sun Jan 21 10:57:47 1990 by muller         *)

MODULE VarExpr;

IMPORT M3, M3ID, Expr, ExprRep, Type, Value, Variable;
IMPORT MSIR, MSIRBuilder, MSIRType, CaptureAnalysis;

TYPE
  P = Expr.T OBJECT
        v : Variable.T;
      OVERRIDES
        typeOf       := ExprRep.NoType;
        repTypeOf    := ExprRep.NoType;
        check        := Check;
        need_addr    := NeedsAddress;
        prep         := ExprRep.NoPrep;
        compile      := Compile;
        prepLV       := ExprRep.NotLValue;
        compileLV    := CompileLV;
        prepBR       := ExprRep.PrepNoBranch;
        compileBR    := ExprRep.NoBranch;
        evaluate     := ExprRep.NoValue;
        isEqual      := EqCheck;
        getBounds    := ExprRep.NoBounds;
        isWritable   := ExprRep.IsAlways;
        isDesignator := ExprRep.IsAlways;
        isZeroes     := ExprRep.IsNever;
        genFPLiteral := ExprRep.NoFPLiteral;
        prepLiteral  := ExprRep.NoPrepLiteral;
        genLiteral   := ExprRep.NoLiteral;
        note_write   := NoteWrites;
        capture  := Capture;
        captureLV := CaptureLV;
        compileMSIR  := CompileMSIR;
      END;

PROCEDURE New (t: Type.T;  name: M3ID.T): Expr.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    ExprRep.Init (p);
    p.v := Variable.New (name, TRUE);
    p.type := Type.Base (t);
    Variable.BindType (p.v, p.type, indirect := FALSE, readonly := FALSE,
                       open_array_ok := FALSE,  needs_init := TRUE);
    p.repType := p.type;
    RETURN p;
  END New;

PROCEDURE Obj (e: Expr.T): Variable.T =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN NIL;
    | P(p) => RETURN p.v;
    ELSE      RETURN NIL;
    END;
  END Obj;

PROCEDURE Check (p: P;  VAR cs: Expr.CheckState) =
  BEGIN
    Value.TypeCheck (p.v, cs);
  END Check;

PROCEDURE EqCheck (a: P;  e: Expr.T;  <*UNUSED*> x: M3.EqAssumption): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(b) => RETURN (a.v = b.v);
    ELSE      RETURN FALSE;
    END;
  END EqCheck;

PROCEDURE NeedsAddress (p: P) =
  BEGIN
    Variable.NeedsAddress (p.v);
  END NeedsAddress;

PROCEDURE Compile (p: P; StaticOnly: BOOLEAN) =
  BEGIN
    <* ASSERT NOT StaticOnly *>
    Variable.Load (p.v);
  END Compile;

PROCEDURE CompileLV (p: P; <*UNUSED*> traced: BOOLEAN; StaticOnly: BOOLEAN) =
  BEGIN
    <* ASSERT NOT StaticOnly *>
    Variable.LoadLValue (p.v);
  END CompileLV;

PROCEDURE NoteWrites (p: P) =
  BEGIN
    Variable.ScheduleTrace (p.v);
  END NoteWrites;

PROCEDURE CompileMSIR (p: P): MSIR.Value =
  VAR v := MSIRBuilder.LookupVar (p.v);
  BEGIN
    IF v = NIL THEN
      MSIRBuilder.Abandon ("unbound variable reference: " & Value.GlobalName (p.v));
      RETURN NIL;
    END;
    (* Normalize an ordinal variable read to ZType (machine width).  Local reads
       already land at i64 (their allocas are widened), but global reads return
       the narrow MType storage width; widen so every variable read is uniform
       width.  No-op for non-scalars (BitWidth < 0) and already-i64 locals. *)
    VAR t := Value.TypeOf (p.v);  zt: MSIR.T;
    BEGIN
      IF t # NIL THEN
        zt := MSIRType.ComputeType (t);
        IF zt # NIL AND MSIR.BitWidth (zt) > 0
           AND MSIR.BitWidth (MSIR.ValueType (v)) > 0
           AND NOT MSIR.Equal (zt, MSIR.ValueType (v)) THEN
          v := MSIRBuilder.CoerceToMSIR (MSIRBuilder.CurrentBlock (), v, zt);
        END;
      END;
    END;
    RETURN v;
  END CompileMSIR;

(* If p is a bit-field WITH alias, write rhs through it (InsertBitField) and
   return TRUE; else FALSE.  Called by AssignStmt for a no-lvalue var LHS. *)
PROCEDURE BitFieldStoreMSIR (e: Expr.T;  rhs: MSIR.Value): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | P (p) => RETURN MSIRBuilder.TryBitFieldStore (p.v, rhs);
    ELSE       RETURN FALSE;
    END;
  END BitFieldStoreMSIR;

PROCEDURE Capture (p: P;  ca: CaptureAnalysis.T) =
  BEGIN
    IF Variable.IsUpLevel (p.v) THEN CaptureAnalysis.Note (ca, p.v, FALSE) END;
  END Capture;

PROCEDURE CaptureLV (p: P;  ca: CaptureAnalysis.T) =
  BEGIN
    IF Variable.IsUpLevel (p.v) THEN CaptureAnalysis.Note (ca, p.v, TRUE) END;
  END CaptureLV;

BEGIN
END VarExpr.
