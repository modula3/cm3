(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: CallExpr.m3                                           *)
(* Last modified on Tue Jun 20 15:09:15 PDT 1995 by kalsow     *)
(*      modified on Thu Jun 15 11:11:20 PDT 1995 by ericv      *)
(*      modified on Wed Nov  7 01:30:54 1990 by muller         *)

MODULE CallExpr;

(* NOTE: Notwithstanding its name, this module handles only
         calls on builtin procedures.
*)

IMPORT CG, Expr, ExprRep, Error, ProcType, Type, UserProc;
IMPORT KeywordExpr, ESet, QualifyExpr, ErrType, Value, Target;
IMPORT MSIR, MSIRBuilder, MSIRType, Method;
IMPORT Formal, CaptureAnalysis;

REVEAL
  MethodList = BRANDED "CallExpr.MethodList" REF RECORD
                 minArgs      : INTEGER;
                 maxArgs      : INTEGER;
                 functional   : BOOLEAN;
                 keywords     : BOOLEAN;
                 strict       : BOOLEAN;
                 fixedType    : Type.T;
                 typeOf       : Typer;
                 repTypeOf    : Typer;
                 need_addr    : Visitor;
                 checker      : TypeChecker;
                 prep         : Compiler;
                 compiler     : Compiler;
                 prepLV       : CompilerLV;
                 compilerLV   : CompilerLV;
                 prepBR       : CompilerBR;
                 compilerBR   : CompilerBR;
                 evaluator    : Evaluator;
                 bounder      : Bounder;
                 isWritable   : Predicate;
                 isDesignator : Predicate;
                 noteWriter   : NoteWriter;
                 isIndirect   : Predicate;
                 builtinAlign : BuiltinAlign;
                 compileMSIR  : MSIRCompiler := NIL;
                 writesArg0   : BOOLEAN := FALSE;
                 (* TRUE only for INC and DEC: their first argument is written.
                    Used by Scan to decide scanLV vs scan for capture analysis. *)
               END;

REVEAL
  T = T_ BRANDED "CallExpr.P" OBJECT
        methods  : MethodList;
        proc_type: Type.T;
      OVERRIDES
        typeOf       := TypeOf;
        repTypeOf    := RepTypeOf;
        check        := Check;
        need_addr    := NeedsAddress;
        prep         := Prep;
        compile      := Compile;
        prepLV       := PrepLV;
        compileLV    := CompileLV;
        prepBR       := PrepBR;
        compileBR    := CompileBR;
        evaluate     := Fold;
        isEqual      := ExprRep.NeverEq;
        getBounds    := GetBounds;
        isWritable   := IsWritable;
        isDesignator := IsDesignator;
        isZeroes     := ExprRep.IsNever;
        genFPLiteral := ExprRep.NoFPLiteral;
        prepLiteral  := ExprRep.NoPrepLiteral;
        genLiteral   := ExprRep.NoLiteral;
        note_write   := NoteWrites;
        exprAlign    := CallExprAlign;
        usesAssignProtocol := UsesAssignProtocol;
        scan               := Scan;
        compileMSIR        := CompileMSIR;
      END;

PROCEDURE New (proc: Expr.T;  args: Expr.List): Expr.T =
  VAR p := NEW (T);
  BEGIN
    ExprRep.Init (p);
    p.proc      := proc;
    p.args      := args;
    p.tmp       := NIL;
    p.methods   := NIL;
    p.proc_type := NIL;
    p.directAssignableType := TRUE;
    RETURN p;
  END New;

PROCEDURE Is (e: Expr.T): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | T    => RETURN TRUE;
    ELSE      RETURN FALSE;
    END;   
  END Is;

PROCEDURE IsUserProc (e: Expr.T): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | T(t) => Resolve (t);  RETURN (t.methods = UserProc.Methods);
    ELSE      RETURN FALSE;
    END;   
  END IsUserProc;

PROCEDURE NewMethodList (minArgs, maxArgs: INTEGER;
                         functional   : BOOLEAN;
                         keywords     : BOOLEAN;
                         strict       : BOOLEAN;
                         fixedType    : Type.T;
                         typeOf       : Typer;
                         repTypeOf    : Typer;
                         need_addr    : Visitor;
                         checker      : TypeChecker;
                         prep         : Compiler;
                         compiler     : Compiler;
                         prepLV       : CompilerLV;
                         compilerLV   : CompilerLV;
                         prepBR       : CompilerBR;
                         compilerBR   : CompilerBR;
                         evaluator    : Evaluator;
                         bounder      : Bounder;
                         isWritable   : Predicate;
                         isDesignator : Predicate;
                         noteWriter   : NoteWriter;
                         builtinAlign : BuiltinAlign := BuiltinAlignDefault;
                   (* usesAssignProtocol : Predicate; *)
                        ): MethodList =
  VAR m: MethodList;
  BEGIN
    m := NEW (MethodList);
    m.minArgs      := minArgs;
    m.maxArgs      := maxArgs;
    m.functional   := functional;
    m.keywords     := keywords;
    m.strict       := strict;
    m.fixedType    := fixedType;
    m.typeOf       := typeOf;
    m.repTypeOf    := repTypeOf;
    m.need_addr    := need_addr;
    m.checker      := checker;
    m.prep         := prep;
    m.compiler     := compiler;
    m.prepLV       := prepLV;
    m.compilerLV   := compilerLV;
    m.prepBR       := prepBR;
    m.compilerBR   := compilerBR;
    m.evaluator    := evaluator;
    m.bounder      := bounder;
    m.isWritable   := isWritable;
    m.isDesignator := isDesignator;
    m.noteWriter   := noteWriter;
    m.builtinAlign := builtinAlign;
    RETURN m;
  END NewMethodList;

PROCEDURE IsNever (<*UNUSED*> t: T;  <*UNUSED*> lhs: BOOLEAN): BOOLEAN =
  BEGIN
    RETURN FALSE;
  END IsNever;

PROCEDURE IsAlways (<*UNUSED*> t: T): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END IsAlways;

PROCEDURE NoValue (<*UNUSED*> t: T): Expr.T =
  BEGIN
    RETURN NIL;
  END NoValue;

PROCEDURE NoBounds (t: T;  VAR min, max: Target.Int) =
  BEGIN
    ExprRep.NoBounds (t, min, max);
  END NoBounds;

PROCEDURE NotAddressable (<*UNUSED*> t: T) =
  BEGIN
    Error.Msg ("Internal compiler error CallExpr.NotAddressable");
    <* ASSERT FALSE *>
  END NotAddressable;

PROCEDURE PrepArgs (t: T) =
  BEGIN
    FOR i := 0 TO LAST (t.args^) DO
      Expr.Prep (t.args[i]);
    END;
  END PrepArgs;

PROCEDURE NoLValue (<*UNUSED*> t: T; <*UNUSED*> traced: BOOLEAN) =
  BEGIN
    Error.Msg ("Internal compiler error CallExpr.NoLValue");
    <*ASSERT FALSE*>
  END NoLValue;

PROCEDURE NotBoolean (<*UNUSED*> t: T;
                      <*UNUSED*> true, false: CG.Label;
                      <*UNUSED*> freq: CG.Frequency) =
  BEGIN
    Error.Msg ("Internal compiler error CallExpr.NotBoolean");
    <*ASSERT FALSE*>
  END NotBoolean;

PROCEDURE PrepNoBranch (t: T;  true, false: CG.Label;  freq: CG.Frequency) =
  BEGIN
    t.prep ();
    t.compile (StaticOnly := FALSE);
    IF (true # CG.No_label)
      THEN CG.If_true (true, freq);
      ELSE CG.If_false (false, freq);
    END;
  END PrepNoBranch;

PROCEDURE NoBranch (<*UNUSED*> t: T;
                    <*UNUSED*> true, false: CG.Label;
                    <*UNUSED*> freq: CG.Frequency) =
  BEGIN
    (* all the work was done by prep *)
  END NoBranch;

PROCEDURE NotWritable (<*UNUSED*> t: T)=
  BEGIN
    (* skip *)
  END NotWritable;

(***********************************************************************)

PROCEDURE Resolve (p: T) =
  VAR t: Type.T;
  BEGIN
    IF (p.methods # NIL) THEN RETURN END;
    t := Expr.TypeOf (p.proc);
    IF (t = NIL) THEN
      t := QualifyExpr.MethodType (p.proc);
      (* we need this hack because "TypeOf(obj.method)" returns NIL
         so that you can't use it as a vanilla procedure value. *)
    END;
    p.methods := ProcType.Methods (t);
    p.proc_type := t;
  END Resolve;

PROCEDURE ComputeTypes (p: T) =
  BEGIN
    Resolve (p);
    IF p.methods = NIL THEN
      p.type := ErrType.T;
      p.repType := ErrType.T;
    ELSIF (p.methods.fixedType # NIL) OR (p.methods.typeOf = NIL) THEN
      p.type := p.methods.fixedType;
      p.repType := p.type;
    ELSE
      FixArgs (p);
      p.type := p.methods.typeOf (p);
      p.repType := p.methods.repTypeOf (p);
    END;
  END ComputeTypes;

PROCEDURE TypeOf (p: T): Type.T =
  BEGIN
    ComputeTypes (p);
    RETURN p.type;
  END TypeOf;

PROCEDURE RepTypeOf (p: T): Type.T =
  BEGIN
    ComputeTypes (p);
    RETURN p.repType;
  END RepTypeOf;

PROCEDURE Check (p: T;  VAR cs: Expr.CheckState) =
  VAR
    nErrs0, nErrs1, nWarns: INTEGER;
    arg: Expr.T;
    keywords: BOOLEAN;
  BEGIN
    (* check the procedure *)
    Error.Count (nErrs0, nWarns);
    Expr.TypeCheck (p.proc, cs);
    Resolve (p);
    Error.Count (nErrs1, nWarns);
    IF (p.methods = NIL) THEN
      IF (nErrs0 = nErrs1) AND (Expr.TypeOf (p.proc) # ErrType.T) THEN
        Error.Msg ("attempting to call a non-procedure" & ProcName (p));
      END;
      p.type := ErrType.T;
    END;

    (* check its args *)
    keywords := (p.methods = NIL) OR (p.methods.keywords);
    FOR i := 0 TO LAST (p.args^) DO
      arg := p.args[i];
      Expr.TypeCheck (arg, cs);
      IF (Expr.TypeOf (arg) = ErrType.T) THEN
        p.type := ErrType.T;
      ELSIF (NOT keywords) AND KeywordExpr.Is (arg) THEN
        Error.Msg ("keyword parameters not allowed on builtin operations" &
                   ProcName (p));
      END;
    END;

    (* finally, do the procedure specific checking *)
    IF (p.type # ErrType.T) AND (p.methods # NIL) THEN
      FixArgs (p);
      p.methods.checker (p, cs);
    END;

    (* check the exceptions *)
    ESet.NoteExceptions (cs, ProcType.Raises (p.proc_type));
  END Check;

PROCEDURE FixArgs (p: T) =
  VAR z: Expr.List;
  BEGIN
    IF (NUMBER (p.args^) < p.methods.minArgs) THEN
      Error.Msg ("too few arguments" & ProcName (p));
      z := NEW (Expr.List, p.methods.minArgs);
      FOR i := 0 TO LAST (p.args^) DO z[i] := p.args[i] END;
      p.args := z;
    ELSIF (NUMBER (p.args^) > p.methods.maxArgs) THEN
      Error.Msg ("too many arguments" & ProcName (p));
      z := NEW (Expr.List, p.methods.maxArgs);
      FOR i := 0 TO p.methods.maxArgs - 1 DO z[i] := p.args[i] END;
      p.args := z;
    END;
  END FixArgs;

PROCEDURE ProcName (p: T): TEXT =
  VAR v: Value.T;
  BEGIN
    IF (p.proc # NIL) AND UserProc.IsProcedureLiteral (p.proc, v) THEN
      RETURN ": " & Value.GlobalName (v);
    ELSE
      RETURN "";
    END;
  END ProcName;

PROCEDURE NeedsAddress (p: T) =
  BEGIN
    IF (p.methods # NIL) THEN
      p.methods.need_addr (p);
    END;
  END NeedsAddress;

PROCEDURE CallExprAlign (p: T): Type.BitAlignT =
  VAR resultType : Type.T;
  VAR typeInfo: Type.Info;
  BEGIN
    IF p.methods = NIL THEN (* User proc, has non-nil proc_type. *) 
      resultType := ProcType.Result (p.proc_type);
      resultType := Type.StripPacked (resultType);
      EVAL Type.CheckInfo (resultType, typeInfo);
      RETURN typeInfo.alignment;
    ELSE
      RETURN p.methods.builtinAlign (p);
    END;
  END CallExprAlign;

PROCEDURE SetMethodMSIR (ml: MethodList;  c: MSIRCompiler) =
  BEGIN
    ml.compileMSIR := c;
  END SetMethodMSIR;

PROCEDURE SetWritesArg0 (ml: MethodList) =
  BEGIN
    ml.writesArg0 := TRUE;
  END SetWritesArg0;

PROCEDURE BuiltinAlignDefault (p: T): Type.BitAlignT =
  VAR
    resultType : Type.T;
    resultInfo : Type.Info;
  BEGIN
    resultType := Type.CheckInfo (TypeOf (p), resultInfo);
    RETURN resultInfo.alignment; 
  END BuiltinAlignDefault; 

PROCEDURE UsesAssignProtocol (<*UNUSED*>p: T): BOOLEAN =
  BEGIN
    RETURN FALSE
  END UsesAssignProtocol;

PROCEDURE Prep (p: T) =
  BEGIN
    p.methods.prep (p);
  END Prep;

PROCEDURE Compile (p: T; <*UNUSED*> StaticOnly: BOOLEAN) =
  BEGIN
    p.methods.compiler (p);
  END Compile;

PROCEDURE PrepLV (p: T; traced: BOOLEAN) =
  BEGIN
    p.methods.prepLV (p, traced);
  END PrepLV;

PROCEDURE CompileLV (p: T; traced: BOOLEAN; <*UNUSED*> StaticOnly: BOOLEAN) =
  BEGIN
    p.methods.compilerLV (p, traced);
  END CompileLV;

PROCEDURE PrepBR (p: T;  true, false: CG.Label;  freq: CG.Frequency) =
  BEGIN
    p.methods.prepBR (p, true, false, freq);
  END PrepBR;

PROCEDURE CompileBR (p: T;  true, false: CG.Label;  freq: CG.Frequency) =
  BEGIN
    p.methods.compilerBR (p, true, false, freq);
  END CompileBR;

PROCEDURE NoteWrites (p: T) =
  BEGIN
    IF p.methods # NIL THEN
      p.methods.noteWriter (p);
    END;
  END NoteWrites;

PROCEDURE Fold (p: T): Expr.T =
  BEGIN
    Resolve (p);
    IF p.type = ErrType.T THEN RETURN NIL END; 
    IF (p.methods = NIL) THEN RETURN NIL END;
    RETURN p.methods.evaluator (p);
  END Fold;

PROCEDURE GetBounds (p: T;  VAR min, max: Target.Int) =
  VAR e := Fold (p);
  BEGIN
    IF (e # NIL) AND (e # p) THEN
      Expr.GetBounds (e, min, max);
    ELSIF p.type = ErrType.T OR p.methods = NIL THEN
      ExprRep.NoBounds (p, min, max);
    ELSE
      p.methods.bounder (p, min, max);
    END;
  END GetBounds;

PROCEDURE IsDesignator (p: T;  <*UNUSED*> lhs: BOOLEAN): BOOLEAN =
  BEGIN
    Resolve (p);
    IF p.methods = NIL THEN RETURN FALSE END;
    RETURN p.methods.isDesignator (p);
  END IsDesignator;

PROCEDURE IsWritable (p: T;  lhs: BOOLEAN): BOOLEAN =
  BEGIN
    Resolve (p);
    IF p.methods = NIL THEN RETURN FALSE END;
    RETURN p.methods.isWritable (p, lhs);
  END IsWritable;

PROCEDURE CompileMSIR (p: T): MSIR.Value =
  VAR
    v:          Value.T;
    msirCallee: MSIR.Proc;
    argVals:    REF ARRAY OF MSIR.Value;
    n:          INTEGER;
    argVal:     MSIR.Value;
    isNested:   BOOLEAN;
    pBase:      INTEGER;
  BEGIN
    IF NOT MSIRBuilder.InProc() THEN RETURN NIL END;
    IF NOT IsUserProc(p) THEN
      (* IsUserProc called Resolve; p.methods is already set. *)
      IF p.methods # NIL AND p.methods.compileMSIR # NIL THEN
        RETURN p.methods.compileMSIR(p);
      END;
      MSIRBuilder.Abandon("builtin call not supported in MSIR v0");
      RETURN NIL;
    END;
    IF NOT UserProc.IsProcedureLiteral(p.proc, v) THEN
      (* Check for virtual method dispatch: obj.method(args) *)
      VAR
        methodVal : Value.T;
        methodInfo: Method.Info;
        objExpr   : Expr.T;
        objVal    : MSIR.Value;
        rtype     : MSIR.T;
        dispArgs  : REF ARRAY OF MSIR.Value;
        midx      : LONGINT;
      BEGIN
        IF QualifyExpr.Split(p.proc, methodVal) AND
           Value.ClassOf(methodVal) = Value.Class.Method THEN
          objExpr := QualifyExpr.LhsExpr(p.proc);
          IF objExpr = NIL THEN
            MSIRBuilder.Abandon("method call: cannot get receiver");
            RETURN NIL;
          END;
          EVAL Method.Split(methodVal, methodInfo);
          (* Vtable index = bit offset / address size in bits *)
          midx := VAL(methodInfo.offset, LONGINT)
                    DIV VAL(Target.Address.size, LONGINT);

          objVal := Expr.CompileMSIR(objExpr);
          IF objVal = NIL THEN RETURN NIL END;

          n       := NUMBER(p.args^);
          rtype   := MSIRType.TranslateResult(ProcType.Result(p.proc_type));
          dispArgs := NEW(REF ARRAY OF MSIR.Value, n);
          FOR i := 0 TO n - 1 DO
            dispArgs[i] := Expr.CompileMSIR(p.args[i]);
            IF dispArgs[i] = NIL THEN RETURN NIL END;
          END;
          RETURN MSIRBuilder.EmitMethodCall("", objVal, midx, rtype, dispArgs^);
        END;
      END;
      MSIRBuilder.Abandon("indirect/closure call not supported in MSIR v0");
      RETURN NIL;
    END;
    Resolve(p);
    msirCallee := MSIRBuilder.LookupOrCreateProc(v, p.proc_type);
    IF msirCallee = NIL THEN RETURN NIL END;
    isNested := MSIRBuilder.IsNestedProc(v);
    IF isNested THEN
      VAR caps := MSIRBuilder.GetProcCaptures(v);
      BEGIN
        IF caps = NIL THEN pBase := 0 ELSE pBase := NUMBER(caps^) END;
      END;
    ELSE
      pBase := 0;
    END;
    n       := NUMBER(p.args^);
    argVals := NEW(REF ARRAY OF MSIR.Value, n);
    FOR i := 0 TO n - 1 DO
      IF MSIR.Kind(MSIR.ValueType(MSIR.ProcParam(msirCallee, i + pBase)))
           = MSIR.TypeKind.Ptr THEN
        argVal := Expr.LValueMSIR(p.args[i]);
      ELSE
        argVal := Expr.CompileMSIR(p.args[i]);
      END;
      IF argVal = NIL THEN RETURN NIL END;
      argVals[i] := argVal;
    END;
    IF isNested THEN
      RETURN MSIRBuilder.EmitNestedCall("", msirCallee, v, argVals^);
    ELSE
      RETURN MSIRBuilder.EmitCall("", msirCallee, argVals^);
    END;
  END CompileMSIR;

PROCEDURE Scan (ce: T;  ca: CaptureAnalysis.T) =
  VAR formal: Value.T;  finfo: Formal.Info;
  BEGIN
    Expr.Scan (ce.proc, ca);
    IF ce.args = NIL THEN RETURN END;
    IF IsUserProc (ce) THEN
      (* User procedure call: derive mode from formal parameter list.
         IsUserProc called Resolve, so ce.proc_type is set. *)
      formal := ProcType.Formals (ce.proc_type);
      FOR i := 0 TO LAST (ce.args^) DO
        IF formal # NIL THEN
          Formal.Split (formal, finfo);
          formal := formal.next;
        ELSE
          finfo.mode := Formal.Mode.mVALUE;  (* extra args beyond formals *)
        END;
        IF finfo.mode = Formal.Mode.mVAR THEN
          Expr.ScanLV (ce.args[i], ca);
        ELSE
          Expr.Scan (ce.args[i], ca);
        END;
      END;
    ELSE
      (* Builtin call: only INC and DEC write their first argument;
         all other builtins treat every argument as a read. *)
      IF ce.methods # NIL AND ce.methods.writesArg0
           AND NUMBER (ce.args^) > 0 THEN
        Expr.ScanLV (ce.args[0], ca);
        FOR i := 1 TO LAST (ce.args^) DO Expr.Scan (ce.args[i], ca) END;
      ELSE
        FOR i := 0 TO LAST (ce.args^) DO Expr.Scan (ce.args[i], ca) END;
      END;
    END;
  END Scan;

BEGIN
END CallExpr.
