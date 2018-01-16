(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: CallExpr.m3                                           *)
(* Last modified on Tue Jun 20 15:09:15 PDT 1995 by kalsow     *)
(*      modified on Thu Jun 15 11:11:20 PDT 1995 by ericv      *)
(*      modified on Wed Nov  7 01:30:54 1990 by muller         *)

MODULE CallExpr;

IMPORT CG, Expr, ExprRep, Error, ProcType, Type, UserProc;
IMPORT KeywordExpr, ESet, QualifyExpr, ErrType, Value, Target;

REVEAL
  MethodList = BRANDED "CallExpr.MethodList" REF RECORD
                 minArgs      : INTEGER;
                 maxArgs      : INTEGER;
                 functional   : BOOLEAN;
                 keywords     : BOOLEAN;
                 strict       : BOOLEAN;
                 fixedType    : Type.T;
                 typeOf       : Typer;
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
               END;

REVEAL
  T = T_ BRANDED "CallExpr.P" OBJECT
        methods  : MethodList;
        proc_type: Type.T;
      OVERRIDES
        typeOf       := TypeOf;
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
    p.direct_ok := TRUE;
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
                         noteWriter   : NoteWriter): MethodList =
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
    <*ASSERT FALSE*>
  END NoLValue;

PROCEDURE NotBoolean (<*UNUSED*> t: T;
                      <*UNUSED*> true, false: CG.Label;
                      <*UNUSED*> freq: CG.Frequency) =
  BEGIN
    <*ASSERT FALSE*>
  END NotBoolean;

PROCEDURE PrepNoBranch (t: T;  true, false: CG.Label;  freq: CG.Frequency) =
  BEGIN
    t.prep ();
    t.compile ();
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

PROCEDURE TypeOf (p: T): Type.T =
  BEGIN
    Resolve (p);
    IF (p.methods = NIL) THEN
      p.type := ErrType.T;
    ELSIF (p.methods.fixedType # NIL) OR (p.methods.typeOf = NIL) THEN
      p.type := p.methods.fixedType;
    ELSE
      FixArgs (p);
      p.type := p.methods.typeOf (p);
    END;
    RETURN p.type;
  END TypeOf;

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
      RETURN ": " & Value.GlobalName (v, dots := TRUE, with_module := TRUE);
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
    resultType := ProcType.Result (p.proc_type);
    resultType := Type.StripPacked (resultType);
    EVAL Type.CheckInfo (resultType, typeInfo);
    RETURN typeInfo.alignment;
  END CallExprAlign;

PROCEDURE Prep (p: T) =
  BEGIN
    p.methods.prep (p);
  END Prep;

PROCEDURE Compile (p: T) =
  BEGIN
    p.methods.compiler (p);
  END Compile;

PROCEDURE PrepLV (p: T; traced: BOOLEAN) =
  BEGIN
    p.methods.prepLV (p, traced);
  END PrepLV;

PROCEDURE CompileLV (p: T; traced: BOOLEAN) =
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

BEGIN
END CallExpr.
