MODULE M3CTypeCheck;

(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)

(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

IMPORT Text, Fmt;

IMPORT AST, M3AST_AS, M3AST_SM;
IMPORT ASTWalk, M3ASTNext;

IMPORT M3AST_AS_F, M3AST_SM_F, M3AST_TM_F;

IMPORT SeqM3AST_AS_EXP, SeqM3AST_AS_M3TYPE, SeqM3AST_AS_Qual_used_id;

IMPORT M3Error, M3Assert, M3CTypeRelation, M3CTypesMisc, M3CStdProcs;
IMPORT M3CStdTypes, M3CTypeChkUtil, M3CExpsMisc, M3COrdinal, M3CConcTypeSpec;
IMPORT M3CNEWActualS, M3CStdActualS, M3CProcActualS, M3CConsActualS;
IMPORT M3CBackEnd;


REVEAL
  Handle = BRANDED OBJECT
    procStack: ProcStack := NIL;
    safe: BOOLEAN;
  END;


(* utilities *)

PROCEDURE BaseType(exp: M3AST_AS.EXP): M3AST_SM.TYPE_SPEC_UNSET RAISES {}=
  VAR
    expType, base: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    IF M3CTypeChkUtil.IsNormalEXP(exp) THEN
      expType := M3CTypesMisc.CheckedUnpack(exp.sm_exp_type_spec);
      TYPECASE expType OF
      | NULL =>
          RETURN NIL;
      | M3AST_AS.Subrange_type(subrangeType) =>
          base := subrangeType.sm_base_type_spec;
          RETURN base;
      ELSE
        RETURN expType;
      END; (* if *)
    ELSE
      RETURN NIL;
    END; (* if *)
  END BaseType;


PROCEDURE IsException(
    q: M3AST_AS.Qual_used_id;
    VAR id: M3AST_AS.Exc_id)
    : BOOLEAN
    RAISES {}=
  BEGIN
    TYPECASE q.as_id.sm_def OF
    | NULL =>
        id := NIL;
        RETURN TRUE;
    | M3AST_AS.Exc_id(excId) =>
        id := excId;
        RETURN TRUE;
    ELSE
      RETURN FALSE;
    END; (* if *)
  END IsException;


(* procedures called by tree walker *)

PROCEDURE Unary(u: M3AST_AS.UNARY) RAISES {}=
  VAR
    type: M3AST_SM.TYPE_SPEC_UNSET;
    ok: BOOLEAN;
  BEGIN
    type := BaseType(u.as_exp);
    IF type = NIL THEN (* previous error *) RETURN END;
    TYPECASE u OF <*NOWARN*>
    | M3AST_AS.Not =>
        ok := M3CTypeChkUtil.IsSubTypeOfBoolean(type);
    | M3AST_AS.Unaryplus, M3AST_AS.Unaryminus =>
        ok := (ISTYPE(type, M3AST_AS.FLOAT_TYPE)) OR
            (ISTYPE(type, M3AST_AS.Integer_type));
    | M3AST_AS.Deref =>
        ok := TRUE; (* already checked *)
    END; (* case *)
    IF NOT ok THEN
      M3Error.Report(u, "type error in argument to unary operator");
    END; (* if *)
  END Unary;


PROCEDURE Textcat(b: M3AST_AS.BINARY) RAISES {}=
  BEGIN
    IF M3CTypeChkUtil.IsNormalEXP(b.as_exp1) AND
        M3CTypeChkUtil.IsNormalEXP(b.as_exp2) THEN
      IF M3CTypeChkUtil.IsSubTypeOfText(b.as_exp1.sm_exp_type_spec) AND
          M3CTypeChkUtil.IsSubTypeOfText(b.as_exp2.sm_exp_type_spec) THEN
        (* no problem *)
      ELSE
        M3Error.Report(b,
            "type error in arguments to text concatentation operator");
      END; (* if *)
    END;
  END Textcat;


PROCEDURE Binary(h: Handle; b: M3AST_AS.BINARY) RAISES {}=
  VAR
    type1, type2: M3AST_SM.TYPE_SPEC_UNSET;
    ok, safe: BOOLEAN;
    set: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    (* textcat is special deal with it separately *)
    IF ISTYPE(b, M3AST_AS.Textcat) THEN Textcat(b); RETURN END;

    (* assert: "b" is not selection or textcat *)
    type1 := BaseType(b.as_exp1);
    type2 := BaseType(b.as_exp2);
    IF type1 = NIL OR type2 = NIL THEN RETURN END;

    (* assert: op is not selection or textcat; neither type is unset *)
    safe := h.safe;
    ok := FALSE;
    TYPECASE b OF <*NOWARN*>
    | M3AST_AS.Plus, M3AST_AS.Minus,
      M3AST_AS.Times, M3AST_AS.Rdiv =>
        TYPECASE type1 OF
        | M3AST_AS.Integer_type, M3AST_AS.FLOAT_TYPE  =>
            ok := (TYPECODE(type1) = TYPECODE(type2));
        | M3AST_AS.Set_type =>
            ok := (TYPECODE(type1) = TYPECODE(type2)) AND
                (M3CTypeRelation.SubType(type1, type2) OR
                M3CTypeRelation.SubType(type2, type1))
        ELSE
          IF (NOT safe) AND
                (ISTYPE(b, M3AST_AS.Plus) OR ISTYPE(b, M3AST_AS.Minus)) AND
                M3CTypeChkUtil.IsSubTypeOfAddress(type1) THEN
              ok := ISTYPE(type2, M3AST_AS.Integer_type) OR
                  (ISTYPE(b, M3AST_AS.Minus) AND
                      M3CTypeChkUtil.IsSubTypeOfAddress(type2));
            END; (* if *)
        END; (* case *)
    | M3AST_AS.Div, M3AST_AS.Mod => 
        ok := (ISTYPE(type1, M3AST_AS.Integer_type) OR
                   (ISTYPE(b, M3AST_AS.Mod) AND
                    ISTYPE(type1, M3AST_AS.FLOAT_TYPE))) AND
            (TYPECODE(type1) = TYPECODE(type2));
    | M3AST_AS.Eq, M3AST_AS.Ne, M3AST_AS.Gt, M3AST_AS.Lt,
      M3AST_AS.Ge, M3AST_AS.Le =>
        IF ISTYPE(b, M3AST_AS.Eq) OR ISTYPE(b, M3AST_AS.Ne) THEN
          ok := TRUE;
        ELSE
          ok := (ISTYPE(type1, M3AST_AS.Integer_type)) OR
              (ISTYPE(type1, M3AST_AS.Enumeration_type)) OR
              (ISTYPE(type1, M3AST_AS.FLOAT_TYPE)) OR
              (ISTYPE(type1, M3AST_AS.Set_type)) OR
              (M3CTypeChkUtil.IsSubTypeOfAddress(type1) AND
                  M3CTypeChkUtil.IsSubTypeOfAddress(type2));
        END; (* if *)
        ok := ok AND M3CTypeRelation.Assignable(type1, type2, safe) OR
            M3CTypeRelation.Assignable(type2, type1, safe);
    | M3AST_AS.And, M3AST_AS.Or =>
        ok := M3CTypeChkUtil.IsSubTypeOfBoolean(type1) AND
             M3CTypeChkUtil.IsSubTypeOfBoolean(type2);
    | M3AST_AS.In =>
        IF ISTYPE(type2, M3AST_AS.Set_type) THEN
          M3CTypesMisc.GetTYPE_SPECFromM3TYPE(
              NARROW(type2, M3AST_AS.Set_type).as_type, set);
          ok := M3CTypeRelation.Assignable(set, type1, safe);
        END; (* if *)
    END; (* case *)
    IF NOT ok THEN
      M3Error.Report(b, "type error in arguments to binary operator");
    END; (* if *)
  END Binary;


PROCEDURE Index(i: M3AST_AS.Index; safe: BOOLEAN) RAISES {}=
  VAR
    type, expType, indexType, expBaseType: M3AST_SM.TYPE_SPEC_UNSET;
    arrayType: M3AST_AS.Array_type;
    iter: SeqM3AST_AS_EXP.Iter;
    exp: M3AST_AS.EXP;
    ok: BOOLEAN;
  BEGIN
    (* First get the array type; note that the type of 'i.as_array' may
     validly be a reference to an array type *)
    IF NOT M3CTypesMisc.Indexable(BaseType(i.as_array), arrayType) THEN
      RETURN;
    END;

    (* Loop through the index expressions, typechecking them as we go *)
    iter := SeqM3AST_AS_EXP.NewIter(i.as_exp_s);
    IF NOT SeqM3AST_AS_EXP.Next(iter, exp) THEN RETURN END;

    LOOP

      (* Check expression is normal and then type check if possible *)
      IF M3CTypeChkUtil.IsNormalEXP(exp) AND arrayType # NIL THEN
        expType := exp.sm_exp_type_spec;
        CASE M3CTypesMisc.Index(arrayType, indexType) OF
        | M3CTypesMisc.Ix.Unknown =>
            ok := M3COrdinal.Is(expType, expBaseType);
        | M3CTypesMisc.Ix.Ordinal =>
            ok := M3CTypeChkUtil.EXPAssignable(indexType, exp, safe);
        | M3CTypesMisc.Ix.Open =>
            ok := M3COrdinal.Is(expType, expBaseType) AND
               (expBaseType = NIL OR
                   NOT ISTYPE(expBaseType, M3AST_AS.Enumeration_type));
        | M3CTypesMisc.Ix.Bad =>
            ok := TRUE; (* cock up elsewhere *)
        END; (* case *)
        IF NOT ok THEN
          M3Error.Report(i, "index expression not assignable to index type");
        END;
      ELSE
        (* 'exp' is not normal, or we do not have an array type to check *)
      END; (* if *)

      (* Move on to next index expression and array type *)
      IF NOT SeqM3AST_AS_EXP.Next(iter, exp) THEN RETURN END;
      IF arrayType # NIL THEN
        M3CTypesMisc.GetTYPE_SPECFromM3TYPE(
            arrayType.sm_norm_type.as_elementtype, type);
        IF NOT M3CTypesMisc.Indexable(type, arrayType) THEN
          arrayType := NIL;
        END;
      END;

    END; (* loop *)
  END Index;


PROCEDURE Assign(a: M3AST_AS.Assign_st; safe: BOOLEAN) RAISES {}=
  VAR
    lhs := a.as_lhs_exp;
    rhs := a.as_rhs_exp;
    writeable: BOOLEAN;
  BEGIN
    IF M3CTypeChkUtil.IsNormalEXP(lhs) AND M3CTypeChkUtil.IsNormalEXP(rhs) THEN
      IF NOT (M3CExpsMisc.IsDesignator(lhs, writeable) AND writeable) THEN
        M3Error.Report(a, "lhs of assignment is not a writeable designator");
      END; (* if *)
      IF NOT M3CTypeChkUtil.EXPAssignable(lhs.sm_exp_type_spec, rhs, safe) THEN
        M3Error.Report(a, "rhs of assignment not assignable to lhs");
      END; (* if *)
    END; (* if *)
  END Assign;


PROCEDURE ProcedureDeclaration(p: M3AST_AS.Proc_decl) RAISES {}=
  VAR
    defId: M3AST_AS.DEF_ID;
  BEGIN
    IF p.as_id.vREDEF_ID.sm_int_def # NIL THEN
      defId := p.as_id.vREDEF_ID.sm_int_def;
      TYPECASE defId OF
      | NULL =>
      | M3AST_AS.Proc_id(procId) =>
          TYPECASE procId.sm_type_spec OF
          | NULL =>
          | M3AST_AS.Procedure_type(procType) =>
              IF NOT M3CTypeRelation.Covered(p.as_type, procType) THEN
                (* Assert that 'p.as_id.lx_symrep' must be non NIL in order
                 for the 'sm_int_def' field to be set up *)
                M3Error.ReportWithId(p.as_id,
                 "procedure \'%s\' is not covered by declaration in interface",
                     p.as_id.lx_symrep);
              END;
          ELSE
          END; (* typecase *)
      ELSE
      END; (* typecase *)
    END; (* if *)
  END ProcedureDeclaration;


TYPE
  ProcStack = REF RECORD
    next: ProcStack;
    declaration: M3AST_AS.Proc_decl;
    function: BOOLEAN;
    resultType: M3AST_SM.TYPE_SPEC_UNSET;
  END; (* record *)


PROCEDURE PushProc(h: Handle; p: M3AST_AS.Proc_decl) RAISES {}=
  VAR
    new := NEW(ProcStack);
    m3TypeOrVoid: M3AST_AS.M3TYPE_NULL;
  BEGIN
    new.next := h.procStack;
    h.procStack := new;
    new.declaration := p;
    m3TypeOrVoid := p.as_type.as_result_type;
    new.function := (m3TypeOrVoid # NIL);
    IF new.function THEN
      M3CTypesMisc.GetTYPE_SPECFromM3TYPE(m3TypeOrVoid, new.resultType);
    END; (* if *)
  END PushProc;


PROCEDURE PopProc(h: Handle; p: M3AST_AS.Proc_decl) RAISES {}=
  BEGIN
    M3Assert.Check(h.procStack # NIL AND h.procStack.declaration = p);
    h.procStack := h.procStack.next;
  END PopProc;


PROCEDURE Return(h: Handle; r: M3AST_AS.Return_st) RAISES {}=
  VAR
    isFunctionReturn: BOOLEAN;
    msg: Text.T;
  BEGIN
    IF h.procStack = NIL THEN
      msg := "return statement only allowed in a procedure";
    ELSE
      isFunctionReturn := (r.as_exp # NIL);
      IF h.procStack.function = isFunctionReturn THEN
        IF isFunctionReturn AND M3CTypeChkUtil.IsNormalEXP(r.as_exp) AND
            (NOT M3CTypeChkUtil.EXPAssignable(
                h.procStack.resultType, r.as_exp, h.safe)) THEN
          msg := "return expression not assignable to procedure result type";
        ELSE
          (* procedure, bad exp or correct function return - no problem *)
          msg := NIL;
        END; (* if *)
      ELSE
        IF isFunctionReturn THEN
          msg := "expression returned in proper procedure";
        ELSE
          msg := "return in function not followed by expression";
        END; (* if *)
      END; (* if *)
    END; (* if *)
    IF msg # NIL THEN M3Error.Report(r, msg) END;
  END Return;


PROCEDURE MustBeBoolean(exp: M3AST_AS.EXP; text: Text.T) RAISES {}=
  BEGIN
    IF M3CTypeChkUtil.IsNormalEXP(exp) AND
        (NOT M3CTypeChkUtil.IsBoolean(exp.sm_exp_type_spec)) THEN
      M3Error.Report(exp, Fmt.F("expression after %s is not BOOLEAN", text));
    END; (* if *)
  END MustBeBoolean;


PROCEDURE For(f: M3AST_AS.For_st) RAISES {}=
  VAR
    byType: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    IF M3CTypeChkUtil.IsNormalEXP(f.as_from) AND
        M3CTypeChkUtil.IsNormalEXP(f.as_to) THEN
      IF NOT M3CTypeRelation.SameOrdinalSupertype(
          f.as_from.sm_exp_type_spec,
          f.as_to.sm_exp_type_spec) THEN
        M3Error.Report(f,
            "low and high bound of FOR loop are not ordinals with common supertype");
      END; (* if *)
    ELSE
      (* from or to expression bogus *)
    END; (* if *)
    IF f.as_by # NIL THEN
      byType := BaseType(f.as_by.as_exp);
      IF byType # NIL AND NOT ISTYPE(byType, M3AST_AS.Integer_type) THEN
        M3Error.Report(f.as_by.as_exp,
            "For loop BY expression is not subtype of INTEGER");
      END; (* if *)
    END; (* if *)
  END For;


PROCEDURE CaseLabel(
    ordType: M3AST_SM.TYPE_SPEC_UNSET;
    exp: M3AST_AS.EXP;
    safe: BOOLEAN)
    RAISES {}=
  BEGIN
    IF M3CTypeChkUtil.IsNormalEXP(exp) AND
        (NOT M3CTypeChkUtil.EXPAssignable(ordType, exp, safe)) THEN
      M3Error.Report(exp,
          "CASE label not assignable to type of CASE expression");
    END; (* if *)
  END CaseLabel;


PROCEDURE Case(t: M3AST_AS.Case_st; safe: BOOLEAN) RAISES {}=
  VAR
    ordType, baseType: M3AST_SM.TYPE_SPEC_UNSET;
    iter: M3ASTNext.IterCaseLabel;
    case: M3AST_AS.Case;
    label: M3AST_AS.RANGE_EXP;
  BEGIN
    IF M3CTypeChkUtil.IsNormalEXP(t.as_exp) THEN
      ordType := t.as_exp.sm_exp_type_spec;
      IF NOT M3COrdinal.Is(ordType, baseType) THEN
        ordType := NIL;
        M3Error.Report(t.as_exp, "CASE expression is not ordinal");
      END; (* if *)
    ELSE
      ordType := NIL;
    END; (* if *)
    iter := M3ASTNext.NewIterCaseLabel(t.as_case_s);
    WHILE M3ASTNext.CaseLabel(iter, case, label) DO
      TYPECASE label OF <*NOWARN*>
      | M3AST_AS.Range(range) =>
          CaseLabel(ordType, range.as_exp1, safe);
          CaseLabel(ordType, range.as_exp2, safe);
      | M3AST_AS.Range_EXP(rangeExp) =>
          CaseLabel(ordType, rangeExp.as_exp, safe);
      END; (* if *)
    END; (* while *)
  END Case;


PROCEDURE Typecase(t: M3AST_AS.Typecase_st) RAISES {}=
  VAR
    refType, labelType: M3AST_SM.TYPE_SPEC_UNSET;
    iter: M3ASTNext.IterTypeCaseLabel;
    tcase: M3AST_AS.Tcase;
    m3type: M3AST_AS.M3TYPE;
  BEGIN
    IF M3CTypeChkUtil.IsNormalEXP(t.as_exp) THEN
      refType := t.as_exp.sm_exp_type_spec;
      IF refType # NIL AND
          (NOT M3CTypesMisc.IsRef(refType) OR
              ISTYPE(refType, M3AST_AS.Address_type)) THEN
        refType := NIL;
        M3Error.Report(t.as_exp,
            "TYPECASE expression is not of valid reference type");
      END; (* if *)
    ELSE
      refType := NIL;
    END; (* if *)
    iter := M3ASTNext.NewIterTypeCaseLabel(t.as_tcase_s);
    WHILE M3ASTNext.TypeCaseLabel(iter, tcase, m3type) DO
      M3CTypesMisc.GetTYPE_SPECFromM3TYPE(m3type, labelType);
      IF NOT M3CTypeRelation.SubType(labelType, refType) THEN
        M3Error.Report(m3type,
            "label type is not subtype of TYPECASE expression type");
      END; (* if *)
    END; (* while *)
  END Typecase;


PROCEDURE Lock(l: M3AST_AS.Lock_st) RAISES {}=
  BEGIN
    IF M3CTypeChkUtil.IsNormalEXP(l.as_exp) THEN
      VAR
        type := l.as_exp.sm_exp_type_spec;
      BEGIN
        IF type # NIL AND
            (NOT M3CTypeRelation.SubType(type, M3CStdTypes.Mutex())) THEN
          M3Error.Report(l.as_exp, "LOCK expression is not a MUTEX");
        END;
      END;
    END; (* if *)
  END Lock;


PROCEDURE Raise(r: M3AST_AS.Raise_st; safe: BOOLEAN) RAISES {}=
  VAR
    error: Text.T;
    excId: M3AST_AS.Exc_id;
    type: M3AST_SM.TYPE_SPEC_UNSET;
    noExp: BOOLEAN;
  BEGIN
    error := NIL;
    IF IsException(r.as_qual_id, excId) THEN
      IF excId # NIL THEN
        type := excId.sm_type_spec;
        noExp := r.as_exp_void = NIL;
        IF type # NIL AND ISTYPE(type, M3AST_SM.Void_type) THEN
          IF NOT noExp THEN
            error := "Unexpected argument to RAISE";
          END;
        ELSE
          IF noExp THEN
            error := "Missing argument to RAISE";
          ELSE
            IF NOT M3CTypeChkUtil.EXPAssignable(type, r.as_exp_void, safe) THEN
              error := "Argument to RAISE is wrong type";
            END;
          END;
        END;
      END; (* if *)
    ELSE
      error := "RAISE must be followed by exception";
    END; (* if *)
    IF error # NIL THEN
      M3Error.Report(r, error);
    END;
  END Raise;


PROCEDURE Call(c: M3AST_AS.Call; safe: BOOLEAN) RAISES {}=
  CONST
    NormalOrMethod = M3CExpsMisc.ClassSet{
        M3CExpsMisc.Class.Normal, M3CExpsMisc.Class.Method};
  VAR
    pf: M3CStdProcs.T;
  BEGIN
    IF M3CTypeChkUtil.IsExpectedClass(c.as_callexp, NormalOrMethod) THEN END;
    IF M3CStdProcs.IsStandardCall(c, pf) THEN
      IF pf = M3CStdProcs.T.New THEN
        M3CNEWActualS.SetAndTypeCheck(c, safe);
      ELSE
        M3CStdActualS.TypeCheck(c, pf, safe);
      END; (* if *)
    ELSE
      M3CProcActualS.TypeCheck(c, safe);
    END; (* if *)
  END Call;


PROCEDURE Const(c: M3AST_AS.Const_decl; safe: BOOLEAN) RAISES {}=
  BEGIN
    IF M3CTypeChkUtil.IsNormalEXP(c.as_exp) THEN
      IF (c.as_type # NIL) AND
          (NOT M3CTypeChkUtil.EXPAssignable(
              c.as_id.sm_type_spec, c.as_exp, safe)) THEN
        M3Error.Report(c.as_exp,
            "CONST expression not member of declared type");
      END; (* if *)
    END; (* if *)
  END Const;


TYPE
  TypeAndDefaultError = {None, OpenArray, Empty, NotAssignable};


PROCEDURE TypeAndDefault(
    type: M3AST_AS.M3TYPE_NULL;
    default: M3AST_AS.EXP_NULL;
    param, safe: BOOLEAN)
    : TypeAndDefaultError
    RAISES {}=
  VAR
    normalExp: BOOLEAN;
    ts: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    IF default # NIL THEN
      normalExp := M3CTypeChkUtil.IsNormalEXP(default);
    ELSE
      normalExp := FALSE;
    END;
    IF type # NIL THEN
      M3CTypesMisc.GetTYPE_SPECFromM3TYPE(type, ts);
      IF normalExp AND NOT M3CTypeChkUtil.EXPAssignable(ts, default, safe) THEN
        RETURN TypeAndDefaultError.NotAssignable;
      END; (* if *)
    ELSIF normalExp THEN
      (* we still need to do some checks on the type *)
      ts := default.sm_exp_type_spec;
    ELSE
      RETURN TypeAndDefaultError.None; (* we cannot do any further checking *)
    END; (* if *)
    IF NOT param THEN
      IF M3CTypesMisc.IsEmpty(ts) THEN RETURN TypeAndDefaultError.Empty END;
      IF M3CTypesMisc.IsOpenArray(ts) THEN
        RETURN TypeAndDefaultError.OpenArray;
      END;
    END;
    RETURN TypeAndDefaultError.None;
  END TypeAndDefault;


PROCEDURE Var(v: M3AST_AS.Var_decl; safe: BOOLEAN) RAISES {}=
  VAR
    error := TypeAndDefault(v.as_type, v.as_default, FALSE, safe);
    errorText: Text.T;
  BEGIN
    IF error # TypeAndDefaultError.None THEN
      CASE error OF <*NOWARN*>
      | TypeAndDefaultError.OpenArray =>
          errorText := "variable cannot be of open array type";
      | TypeAndDefaultError.Empty =>
          errorText := "variable cannot be of empty type";
      | TypeAndDefaultError.NotAssignable =>
          errorText := "VAR default not assignable to variable" ;
      END;
      M3Error.Report(v, errorText);
    END;
  END Var;


PROCEDURE Exception(e: M3AST_AS.Exc_decl) RAISES {}=
  VAR
    excArgType: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    IF e.as_type # NIL THEN
      M3CTypesMisc.GetTYPE_SPECFromM3TYPE(e.as_type, excArgType);
      IF M3CTypesMisc.IsOpenArray(excArgType) THEN
        M3Error.Report(e.as_type,
            "exception argument cannot be of open array type");
      END; (* if *)
    END; (* if *)
  END Exception;


PROCEDURE Revelation(i: M3AST_AS.Concrete_reveal) RAISES {}=
  VAR
    type, revealed: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    TYPECASE i.as_qual_id.as_id.sm_def OF
    | NULL =>
    | M3AST_AS.Type_id(typeId) =>
        TYPECASE typeId.sm_type_spec OF
        | NULL =>
        | M3AST_AS.Opaque_type(opaqueType) =>
            M3CTypesMisc.GetTYPE_SPECFromM3TYPE(opaqueType.as_type, type);
            M3CTypesMisc.GetTYPE_SPECFromM3TYPE(i.as_type, revealed);
            IF NOT (M3CTypeRelation.SubType(revealed, type) OR
                M3CTypeRelation.SubType(type, revealed)) THEN
              M3Error.Report(i.as_type,
                  "revealed type is not appropriate for opaque type");
            END; (* if *)
        ELSE
        END; (* typecase *)
    ELSE
    END; (* typecase *)
  END Revelation;


PROCEDURE Formal(f: M3AST_AS.Formal_param; safe: BOOLEAN) RAISES {}=
  VAR
  BEGIN
    IF TypeAndDefault(f.as_formal_type, f.as_default, TRUE, safe) #
        TypeAndDefaultError.None THEN
      (* can only be not assignable *)
      M3Error.Report(f, "default not member of type of parameter");
    END; (* if *)
  END Formal;


PROCEDURE Field(f: M3AST_AS.Fields; safe: BOOLEAN) RAISES {}=
  VAR
    error := TypeAndDefault(f.as_type, f.as_default, FALSE, safe);
    errorText: Text.T;
  BEGIN
    IF error # TypeAndDefaultError.None THEN
      CASE error OF <*NOWARN*>
      | TypeAndDefaultError.OpenArray =>
          errorText := "field cannot be of open array type";
      | TypeAndDefaultError.Empty =>
          errorText := "field cannot be of empty type";
      | TypeAndDefaultError.NotAssignable =>
          errorText := "default not member of type of field" ;
      END;
      M3Error.Report(f, errorText);
    END;
  END Field;


PROCEDURE MethodOverride(m: M3AST_AS.METHOD_OVERRIDE) RAISES {}=
  VAR
    id := m.as_id;
    defaultType: M3AST_SM.TYPE_SPEC_UNSET;
    proc: M3CTypeChkUtil.Proc;
  CONST
    OkDefault = M3CTypeChkUtil.ProcSet{M3CTypeChkUtil.Proc.TopLevel,
                                 M3CTypeChkUtil.Proc.Method};
  BEGIN
    IF id.vINIT_ID.sm_init_exp # NIL  AND id.sm_type_spec # NIL THEN
      defaultType := id.vINIT_ID.sm_init_exp.sm_exp_type_spec;
      IF defaultType # NIL THEN
        proc := M3CTypeChkUtil.ClassifyProc(id.vINIT_ID.sm_init_exp);
        IF NOT(proc IN OkDefault) THEN
          M3Error.Report(m.as_default,
              "default for method is not a top level procedure constant");
        ELSIF NOT M3CTypeRelation.Satisfies(defaultType,
                m.tmp_type, id.sm_type_spec) THEN
          M3Error.Report(m.as_default,
              "default does not satisfy signature of method");
        END; (* if *)
      END; (* if *)
    ELSE
      (* no default to check or method type unset *)
    END; (* if *)
  END MethodOverride;


PROCEDURE Subrange(s: M3AST_AS.Subrange_type) RAISES {}=
  VAR
    range := s.as_range;
    exp1 := range.as_exp1;
    exp2 := range.as_exp2;
  BEGIN
    IF M3CTypeChkUtil.IsNormalEXP(exp1) AND
        M3CTypeChkUtil.IsNormalEXP(exp2) THEN
      IF NOT M3CTypeRelation.SameOrdinalSupertype(
          exp1.sm_exp_type_spec, exp2.sm_exp_type_spec) THEN
        M3Error.Report(
            s, "subrange bounds are not ordinal or are incompatible");
      END;
    END; (* if *)
  END Subrange;


PROCEDURE Set(s: M3AST_AS.Set_type) RAISES {}=
  VAR
    base, baseBase: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    M3CTypesMisc.GetTYPE_SPECFromM3TYPE(s.as_type, base);
    IF NOT M3COrdinal.Is(base, baseBase) THEN
      M3Error.Report(s.as_type, "set base type must be ordinal");
    END; (* if *)
  END Set;


PROCEDURE Array(a: M3AST_AS.Array_type) RAISES {}=
  VAR
    arrayType, indexType, elementType, indexBase: M3AST_SM.TYPE_SPEC_UNSET;
    m3Type: M3AST_AS.M3TYPE;
    iter: SeqM3AST_AS_M3TYPE.Iter;
  BEGIN
    arrayType := a;
    IF NOT M3CTypesMisc.IsOpenArray(arrayType) THEN
      iter := SeqM3AST_AS_M3TYPE.NewIter(a.as_indextype_s);
      WHILE SeqM3AST_AS_M3TYPE.Next(iter, m3Type) DO
        M3CTypesMisc.GetTYPE_SPECFromM3TYPE(m3Type, indexType);
        IF NOT M3COrdinal.Is(indexType, indexBase) THEN
          M3Error.Report(m3Type, "index type must be ordinal");
        END; (* if *)
      END; (* while *)
      M3CTypesMisc.GetTYPE_SPECFromM3TYPE(a.as_elementtype, elementType);
      IF M3CTypesMisc.IsOpenArray(elementType) THEN
        M3Error.Report(a.as_elementtype,
            "fixed array element type cannot be open array");
      END; (* if *)
    END; (* if *)
  END Array;


PROCEDURE Procedure(p: M3AST_AS.Procedure_type) RAISES {}=
  BEGIN
    IF p.as_result_type # NIL THEN
      VAR
        resultType: M3AST_SM.TYPE_SPEC_UNSET;
      BEGIN
        M3CTypesMisc.GetTYPE_SPECFromM3TYPE(p.as_result_type, resultType);
        IF M3CTypesMisc.IsOpenArray(resultType) THEN
          M3Error.Report(p.as_result_type,
              "procedure result type cannot be open array");
        END; (* if *)
      END;
    END; (* if *)
    TYPECASE p.as_raises OF <*NOWARN*>
    | NULL => (* RAISES {} *)
    | M3AST_AS.Raisees_any =>
    | M3AST_AS.Raisees_some(raises) =>
        VAR
          iter := SeqM3AST_AS_Qual_used_id.NewIter(raises.as_raisees_s);
          qualId: M3AST_AS.Qual_used_id;
        BEGIN
          WHILE SeqM3AST_AS_Qual_used_id.Next(iter, qualId) DO
            TYPECASE qualId.as_id.sm_def OF
            | NULL =>
            | M3AST_AS.Exc_id =>
            ELSE
              M3Error.ReportWithId(qualId.as_id,
                  "\'%s\' is not an exception", qualId.as_id.lx_symrep);
            END;
          END;
        END;
    END;
  END Procedure;


PROCEDURE Ref(h: Handle; r: M3AST_AS.Ref_type) RAISES {}=
  VAR
    referent: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    IF r.as_trace_mode # NIL AND h.safe THEN
      M3CTypesMisc.GetTYPE_SPECFromM3TYPE(r.as_type, referent);
      IF M3CTypesMisc.IsTraced(referent) THEN
        M3Error.Report(r, "untraced reference must not have traced referent");
      END; (* if *)
    END; (* if *)
  END Ref;


PROCEDURE Object(h: Handle; o: M3AST_AS.Object_type) RAISES {}=
  VAR
    super: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    IF (M3CTypesMisc.IsTracedObject(o) = M3CTypesMisc.Ref.Untraced) AND
        h.safe AND M3CTypesMisc.ContainsTracedFields(o.as_fields_s) THEN
      M3Error.Report(o, "untraced object must not have any traced fields");
    END; (* if *)
    IF M3ASTNext.SimpleSuperType(o, super) THEN
      LOOP
        IF super # NIL AND ISTYPE(super, M3AST_AS.Opaque_type) THEN
          super := M3CConcTypeSpec.CurrentReveal(super);
        ELSE
          EXIT;
        END;
      END;
      IF super = NIL OR ISTYPE(super, M3AST_AS.Object_type) OR
          ISTYPE(super, M3AST_AS.Root_type) THEN
        (* all is well *)
      ELSE
        M3Error.Report(o.as_ancestor,
            "supertype of object type must be another object type");
      END; (* if *)
    END; (* if *)
  END Object;


PROCEDURE IsNormalIntegerExpression(exp: M3AST_AS.EXP): BOOLEAN RAISES {}=
  BEGIN
    IF M3CTypeChkUtil.IsNormalEXP(exp) THEN
      IF M3CTypeChkUtil.IsSubTypeOfInteger(exp.sm_exp_type_spec) THEN
        RETURN TRUE;
      ELSE
        M3Error.Report(exp, "expression must be integer");
      END;
    END;
    RETURN FALSE;
  END IsNormalIntegerExpression;


PROCEDURE Packed(p: M3AST_AS.Packed_type) RAISES {}=
  VAR
    packedType: M3AST_SM.TYPE_SPEC_UNSET;
    exp: M3AST_AS.EXP;
  BEGIN
    M3CTypesMisc.GetTYPE_SPECFromM3TYPE(p.as_type, packedType);
    IF M3CTypesMisc.IsOpenArray(packedType) THEN
      M3Error.Report(p.as_type, "cannot pack open array");
    ELSE
      exp := p.as_exp;
      IF IsNormalIntegerExpression(exp) THEN
        IF (exp.sm_exp_value # NIL) AND
           packedType # NIL AND NOT M3CBackEnd.BitsOK(exp, packedType) THEN
          M3Error.Report(exp, "cannot pack type in given number of bits");
        END; (* if *)
      END;
    END; (* if *)
  END Packed;


PROCEDURE Node(h: Handle; any: AST.NODE; v: ASTWalk.VisitMode) RAISES {}=
  BEGIN
    IF v = ASTWalk.VisitMode.Entry THEN
      IF ISTYPE(any, M3AST_AS.Proc_decl) THEN
        PushProc(h, any);
      END; (* if *)
    ELSE
      TYPECASE any OF
      | M3AST_AS.UNARY(t) =>
          Unary(t);
      | M3AST_AS.BINARY(t) =>
          Binary(h, t);
      | M3AST_AS.Index(t) =>
          Index(t, h.safe);
      | M3AST_AS.Assign_st(t) =>
          Assign(t, h.safe);
      | M3AST_AS.Proc_decl(proc_decl) =>
          ProcedureDeclaration(proc_decl);
          PopProc(h, proc_decl);
      | M3AST_AS.Return_st(return_st) =>
          Return(h, return_st);
      | M3AST_AS.If_st(if_st) =>
          MustBeBoolean(if_st.as_exp, "IF");
      | M3AST_AS.Elsif(elsif) =>
          MustBeBoolean(elsif.as_exp, "ELSIF");
      | M3AST_AS.Repeat_st(repeat_st) =>
          MustBeBoolean(repeat_st.as_exp, "UNTIL");
      | M3AST_AS.While_st(while_st) =>
          MustBeBoolean(while_st.as_exp, "WHILE");
      | M3AST_AS.For_st(for_st) =>
          For(for_st);
      | M3AST_AS.Case_st(case_st) =>
          Case(case_st, h.safe);
      | M3AST_AS.Typecase_st(t) =>
          Typecase(t);
      | M3AST_AS.Lock_st(t) =>
          Lock(t);
      | M3AST_AS.Raise_st(t) =>
          Raise(t, h.safe);
      | M3AST_AS.Call(t) =>
          Call(t, h.safe);
      | M3AST_AS.Constructor(t) =>
          M3CConsActualS.TypeCheck(t, h.safe);
      | M3AST_AS.Const_decl(t) =>
          Const(t, h.safe);
      | M3AST_AS.Var_decl(t) =>
          Var(t, h.safe);
      | M3AST_AS.Exc_decl(t) =>
          Exception(t);
      | M3AST_AS.Concrete_reveal(t) =>
          Revelation(t);
      | M3AST_AS.Formal_param(t) =>
          Formal(t, h.safe);
      | M3AST_AS.Fields(t) =>
          Field(t, h.safe);
      | M3AST_AS.METHOD_OVERRIDE(t) =>
          MethodOverride(t);
      | M3AST_AS.Subrange_type(t) =>
          Subrange(t);
      | M3AST_AS.Set_type(t) =>
          Set(t);
      | M3AST_AS.Array_type(t) =>
          Array(t);
      | M3AST_AS.Procedure_type(t) =>
          Procedure(t);
      | M3AST_AS.Ref_type(t) =>
          Ref(h, t);
      | M3AST_AS.Object_type(t) =>
          Object(h, t);
      | M3AST_AS.Packed_type(t) =>
          Packed(t);
      ELSE
        (* no action *)
      END; (* case *)
    END; (* if *)
  END Node;


PROCEDURE NewHandle(safe: BOOLEAN; in: M3AST_AS.Proc_decl): Handle RAISES {}=
  VAR
    new := NEW(Handle, safe := safe);
  BEGIN
    IF in # NIL THEN PushProc(new, in) END;
    RETURN new;
  END NewHandle;


BEGIN
END M3CTypeCheck.
