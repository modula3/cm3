(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: CheckExpr.m3                                          *)
(* Last Modified On Fri Feb 24 16:41:16 PST 1995 By kalsow     *)
(*      Modified On Fri Feb 15 04:03:38 1991 By muller         *)

MODULE CheckExpr;

IMPORT M3, CG, Expr, ExprRep, Type, IntegerExpr, EnumExpr, Host;
IMPORT Target, TInt, Error, LInt, Word;
IMPORT MSIR, MSIRBuilder, CaptureAnalysis;

TYPE
  Class = { cLOWER, cUPPER, cBOTH };

TYPE
(* CLEAN ME UP: Use ExprRep.Ta as parent type, then replace uses of
                field 'expr' by field 'a' from Ta.
                Could then use general-purpose ExprRep.ExprAlignArg0
                instead of CheckExpr-specific CheckExprAlign.
                There are several *Expr.P types that could use the
                same. *)
  P = Expr.T OBJECT
        expr  : Expr.T;
        min   : Target.Int;
        max   : Target.Int;
        class : Class;
        err   : CG.RuntimeError;
      OVERRIDES
        typeOf       := TypeOf;
        repTypeOf    := RepTypeOf;
        check        := Check;
        need_addr    := ExprRep.NotAddressable;
        prep         := Prep;
        compile      := Compile;
        prepLV       := ExprRep.NotLValue;
        compileLV    := ExprRep.NotLValueBool;
        prepBR       := ExprRep.PrepNoBranch;
        compileBR    := ExprRep.NoBranch;
        evaluate     := Fold;
        isEqual      := EqCheck;
        getBounds    := Bounder;
        isWritable   := ExprRep.IsNever;
        isDesignator := ExprRep.IsNever;
        isZeroes     := ExprRep.IsNever;
        genFPLiteral := ExprRep.NoFPLiteral;
        prepLiteral  := ExprRep.NoPrepLiteral;
        genLiteral   := ExprRep.NoLiteral;
        note_write   := ExprRep.NotWritable;
        exprAlign    := CheckExprAlign;
        capture  := Capture;
        compileMSIR  := CompileMSIR;
      END;

PROCEDURE New (a: Expr.T;  READONLY min, max: Target.Int;
               err: CG.RuntimeError): Expr.T =
  BEGIN
    RETURN Create (a, min, max, Class.cBOTH, err);
  END New;

PROCEDURE NewLower (a: Expr.T;  READONLY min: Target.Int;
                    err: CG.RuntimeError): Expr.T =
  BEGIN
    RETURN Create (a, min, TInt.Zero, Class.cLOWER, err);
  END NewLower;

PROCEDURE NewUpper (a: Expr.T;  READONLY max: Target.Int;
                    err: CG.RuntimeError): Expr.T =
  BEGIN
    RETURN Create (a, TInt.Zero, max, Class.cUPPER, err);
  END NewUpper;

PROCEDURE Create (a: Expr.T; READONLY min, max: Target.Int; c: Class;
                  err: CG.RuntimeError): Expr.T =
  VAR p: P;
  BEGIN
    IF (NOT Host.doRangeChk) THEN RETURN a END;
    p := NEW (P);
    ExprRep.Init (p);
    p.expr   := a;
    p.min    := min;
    p.max    := max;
    p.class  := c;
    p.err    := err;
    p.origin := a.origin;
    RETURN p;
  END Create;

PROCEDURE TypeOf (p: P): Type.T =
  BEGIN
    RETURN Expr.TypeOf (p.expr);
  END TypeOf;

PROCEDURE RepTypeOf (p: P): Type.T =
  BEGIN
    RETURN Expr.RepTypeOf (p.expr);
  END RepTypeOf;

PROCEDURE Check (p: P;  VAR cs: Expr.CheckState) =
  BEGIN
    Expr.TypeCheck (p.expr, cs);
  END Check;

PROCEDURE EqCheck (a: P;  e: Expr.T;  x: M3.EqAssumption): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(b) => RETURN (a.class = b.class)
                 AND (a.min = b.min)
                 AND (a.max = b.max)
                 AND Expr.IsEqual (a.expr, b.expr, x);
    ELSE RETURN FALSE;
    END;
  END EqCheck;

PROCEDURE CheckExprAlign (p: P): Type.BitAlignT =
  BEGIN
    RETURN Expr.Alignment(p.expr);
  END CheckExprAlign;

PROCEDURE Prep (p: P) =
  BEGIN
    Expr.Prep (p.expr);
  END Prep;

PROCEDURE Compile (p: P; StaticOnly: BOOLEAN) =
  VAR t := Expr.TypeOf (p.expr);  cg_type: CG.Type;
  BEGIN
    <* ASSERT NOT StaticOnly *>
    IF Type.IsSubtype (t, LInt.T)
      THEN cg_type := Target.Longint.cg_type;
      ELSE cg_type := Target.Integer.cg_type;
    END;
    Expr.Compile (p.expr, StaticOnly);
    CASE p.class OF
    | Class.cLOWER => CG.Check_lo (cg_type, p.min, p.err);
    | Class.cUPPER => CG.Check_hi (cg_type, p.max, p.err);
    | Class.cBOTH  => CG.Check_range (cg_type, p.min, p.max, p.err);
    END;
  END Compile;

PROCEDURE EmitChecks (e: Expr.T;  READONLY min, max: Target.Int;
                      err: CG.RuntimeError) =
(* Compiles 'e' and ensures that it's contained in [min..max].
   Does not construct an expression node. *)
  VAR minE, maxE: Target.Int;  x: Expr.T;
      t := Expr.TypeOf (e);  cg_type: CG.Type;
  BEGIN
    IF Type.IsSubtype (t, LInt.T)
      THEN cg_type := Target.Longint.cg_type;
      ELSE cg_type := Target.Integer.cg_type;
    END;
    x := Expr.ConstValue (e);
    IF (x # NIL) THEN e := x;  END;
    Expr.Compile (e);
    IF Host.doRangeChk THEN
      Expr.GetBounds (e, minE, maxE);
      IF TInt.LT (minE, min) AND TInt.LT (max, maxE) THEN
        CG.Check_range (cg_type, min, max, err);
      ELSIF TInt.LT (minE, min) THEN
        IF TInt.LT (maxE, min) THEN
          Error.Warn (2, "Value out of range--low.");
        END;
        CG.Check_lo (cg_type, min, err);
      ELSIF TInt.LT (max, maxE) THEN
        IF TInt.LT (max, minE) THEN
          Error.Warn (2, "Value out of range--high.");
        END;
        CG.Check_hi (cg_type, max, err);
      END;
    END;
  END EmitChecks;

PROCEDURE Fold (p: P): Expr.T =
  VAR e: Expr.T;  i: Target.Int;  t: Type.T;
  BEGIN
    e := Expr.ConstValue (p.expr);
    IF (e = NIL) THEN RETURN NIL END;
    IF (NOT IntegerExpr.Split (e, i, t))
      AND (NOT EnumExpr.Split (e, i, t)) THEN
      RETURN NIL;
    END;
    CASE p.class OF
    | Class.cLOWER => IF TInt.LT (i, p.min) THEN RETURN NIL END;
    | Class.cUPPER => IF TInt.LT (p.max, i) THEN RETURN NIL END;
    | Class.cBOTH  => IF TInt.LT (i, p.min)
                      OR TInt.LT (p.max, i) THEN RETURN NIL END;
    END;
    RETURN e;
  END Fold;

PROCEDURE Bounder (p: P;  VAR min, max: Target.Int) =
  BEGIN
    Expr.GetBounds (p.expr, min, max);
    CASE p.class OF
    | Class.cLOWER => IF TInt.LT (min, p.min) THEN min := p.min END;
    | Class.cUPPER => IF TInt.LT (p.max, max) THEN max := p.max END;
    | Class.cBOTH  => IF TInt.LT (min, p.min) THEN min := p.min END;
                      IF TInt.LT (p.max, max) THEN max := p.max END;
    END;
  END Bounder;

(* Shared MSIR range-check emitter.  Emits a range_check (D32) of the lower
   and/or upper bound of an already-compiled integer value v of expr e,
   signed or unsigned by e's declared type. *)
PROCEDURE DoCheckMSIR (v: MSIR.Value;  e: Expr.T;
                       READONLY min, max: Target.Int;
                       doLo, doHi: BOOLEAN;  err: CG.RuntimeError) =
  VAR lo, hi: Target.Int;
  BEGIN
    Guard (v, min, max, doLo, doHi, err,
           unsigned := Type.GetBounds (Type.StripPacked (Expr.TypeOf (e)), lo, hi)
                       AND NOT TInt.LT (lo, TInt.Zero));
  END DoCheckMSIR;

PROCEDURE EmitBoundsMSIR (v: MSIR.Value;  READONLY min, max: Target.Int;
                          doLo, doHi: BOOLEAN;  err: CG.RuntimeError) =
  BEGIN
    IF Checkable (v) THEN
      Guard (v, min, max, doLo, doHi, err, unsigned := FALSE);
    END;
  END EmitBoundsMSIR;

PROCEDURE Guard (v: MSIR.Value;  READONLY min, max: Target.Int;
                 doLo, doHi: BOOLEAN;  err: CG.RuntimeError;
                 unsigned: BOOLEAN) =
  (* One range_check; a side not checked gets the type's extreme, which
     the check's expansion omits. *)
  VAR
    vt := MSIR.ValueType (v);
    w  := MSIR.BitWidth (vt);
    loI, hiI: INTEGER;
  BEGIN
    doLo := doLo AND TInt.ToInt (min, loI);
    doHi := doHi AND TInt.ToInt (max, hiI);
    IF NOT (doLo OR doHi) THEN RETURN END;
    IF NOT doLo THEN
      IF unsigned THEN loI := 0
      ELSIF w >= BITSIZE (INTEGER) THEN loI := FIRST (INTEGER)
      ELSE loI := -Word.LeftShift (1, w - 1)
      END;
    END;
    IF NOT doHi THEN
      IF w >= BITSIZE (INTEGER) THEN
        IF unsigned THEN hiI := -1 ELSE hiI := LAST (INTEGER) END;
      ELSIF unsigned THEN hiI := Word.LeftShift (1, w) - 1
      ELSE hiI := Word.LeftShift (1, w - 1) - 1
      END;
    END;
    MSIRBuilder.EmitRangeCheck (v, MSIR.ConstInt (vt, loI),
                                MSIR.ConstInt (vt, hiI), unsigned, ORD (err));
  END Guard;

PROCEDURE Checkable (v: MSIR.Value): BOOLEAN =
(* v is an integer-kind value inside a proc, so a ReportFault guard applies. *)
  BEGIN
    IF v = NIL OR NOT MSIRBuilder.InProc () THEN RETURN FALSE END;
    RETURN MSIR.Kind (MSIR.ValueType (v)) >= MSIR.TypeKind.I1
       AND MSIR.Kind (MSIR.ValueType (v)) <= MSIR.TypeKind.W64;
  END Checkable;

PROCEDURE CompileMSIR (p: P): MSIR.Value =
  VAR v := Expr.CompileMSIR (p.expr);
  BEGIN
    IF NOT Checkable (v) THEN RETURN v END;
    DoCheckMSIR (v, p.expr, p.min, p.max,
                 doLo := (p.class = Class.cLOWER OR p.class = Class.cBOTH),
                 doHi := (p.class = Class.cUPPER OR p.class = Class.cBOTH),
                 err  := p.err);
    RETURN v;
  END CompileMSIR;

PROCEDURE EmitChecksMSIR (v: MSIR.Value;  e: Expr.T;
                          READONLY min, max: Target.Int;
                          err: CG.RuntimeError): MSIR.Value =
  VAR minE, maxE: Target.Int;  doLo, doHi: BOOLEAN;  x: Expr.T;
  BEGIN
    IF NOT Host.doRangeChk OR NOT Checkable (v) THEN RETURN v END;
    Expr.GetBounds (e, minE, maxE);
    doLo := TInt.LT (minE, min);   (* e's range can fall below min *)
    doHi := TInt.LT (max, maxE);   (* e's range can rise above max *)
    IF doLo OR doHi THEN
      (* As EmitChecks: a constant's bounds are its value (RETURN -1 needs no
         check against [-1..1]).  Folded only when a check is otherwise
         needed: folding a name looks it up again, which repeats its
         <*OBSOLETE*> warning (p104). *)
      x := Expr.ConstValue (e);
      IF (x # NIL) THEN
        Expr.GetBounds (x, minE, maxE);
        doLo := TInt.LT (minE, min);
        doHi := TInt.LT (max, maxE);
      END;
    END;
    IF doLo OR doHi THEN
      DoCheckMSIR (v, e, min, max, doLo, doHi, err);
    END;
    RETURN v;
  END EmitChecksMSIR;

PROCEDURE Capture (p: P;  ca: CaptureAnalysis.T) =
  BEGIN
    Expr.Capture (p.expr, ca);
  END Capture;

BEGIN
END CheckExpr.
