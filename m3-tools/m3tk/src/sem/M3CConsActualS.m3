MODULE M3CConsActualS;

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

IMPORT M3AST_AS, M3AST_SM, M3ASTNext;

IMPORT M3AST_LX_F, M3AST_AS_F, M3AST_SM_F;

IMPORT SeqM3AST_AS_RANGE_EXP, SeqM3AST_AS_CONS_ELEM;

IMPORT M3Error, M3CTypesMisc, M3CTypeChkUtil, M3CActualUtil,
    M3CExpValue, M3CBackEnd;


<*INLINE*> PROCEDURE AddConstructorElement(
    constructor: M3AST_AS.Constructor;
    rangeExp: M3AST_AS.RANGE_EXP)
    RAISES {}=
  BEGIN
    SeqM3AST_AS_RANGE_EXP.AddRear(constructor.sm_actual_s, rangeExp);
  END AddConstructorElement;


<*INLINE*> PROCEDURE AddNewRangeExp(
    constructor: M3AST_AS.Constructor;
    exp: M3AST_AS.EXP)
    RAISES {}=
  VAR
    r: M3AST_AS.Range_EXP := NIL;
  BEGIN
    IF exp # NIL THEN
      r := NEW(M3AST_AS.Range_EXP).init();
      r.lx_srcpos := exp.lx_srcpos;
      r.as_exp := exp;
    END;
    AddConstructorElement(constructor, r);
  END AddNewRangeExp;


PROCEDURE RecordBuild(
    cons: M3AST_AS.Constructor;
    recordType: M3AST_AS.Record_type)
    RAISES {}=
  VAR
    elements := M3CActualUtil.ElementList(cons);
    iterFields := M3ASTNext.NewIterField(recordType.as_fields_s);
    fieldId: M3AST_AS.Field_id;
    exp: M3AST_AS.EXP;
  BEGIN

    FOR pos := 0 TO M3CActualUtil.PositionalActuals(elements) - 1 DO
      IF M3ASTNext.Field(iterFields, fieldId) THEN
        AddNewRangeExp(cons,
            M3CActualUtil.ActualAt(elements, pos, fieldId.lx_symrep));
      ELSE
        M3Error.Report(cons, "too many elements in record constructor");
        M3CActualUtil.FindUnmatched(elements);
        RETURN;
      END;
    END;

    (* For the remaining fields, see if there is a keyword element.
     If there is, use its expression.  If not, use the default. *)
    WHILE M3ASTNext.Field(iterFields, fieldId) DO
      IF NOT M3CActualUtil.ActualByKeyword(elements, fieldId, exp) THEN
        exp := fieldId.vINIT_ID.sm_init_exp;
        IF exp # NIL THEN
          (* Should be a default *)
          IF exp = M3AST_SM.UNSET_EXP() THEN
            (* Error elsewhere - set 'exp' to NIL *)
            exp := NIL;
          ELSE
            (* Default has been correctly set up *)
          END;
        ELSE
          IF fieldId.lx_symrep # NIL THEN
            M3Error.ReportWithId(cons,
                "no value for field \'%s\'", fieldId.lx_symrep);
          END;
        END;
      END;
      (* Always append 'exp' to list, even if it is NIL, so that the later
       typechecking phase can compare the correct expression against the
       correct field *)
      AddNewRangeExp(cons, exp);
    END;

    IF cons.as_propagate # NIL THEN
      M3Error.Report(cons, "propagation not allowed in record constructor");
    END; (* if *)

    M3CActualUtil.FindUnmatched(elements);
  END RecordBuild;


PROCEDURE CheckClass(exp: M3AST_SM.EXP_UNSET): BOOLEAN RAISES {}=
  BEGIN
    IF exp = NIL THEN
      RETURN FALSE;
    ELSE
      RETURN M3CTypeChkUtil.IsNormalEXP(exp);
    END;
  END CheckClass;


<*INLINE*> PROCEDURE CheckClassAndAddConstructorElement(
    cons: M3AST_AS.Constructor;
    rangeExp: M3AST_AS.Range_EXP)
    RAISES {}=
  BEGIN
    (* In array and set constructors we are not worried by the order of
     the 'sm_actual_s' list so we do not bother to add NIL elements *)
    IF CheckClass(rangeExp.as_exp) THEN
      AddConstructorElement(cons, rangeExp);
    END;
  END CheckClassAndAddConstructorElement;


PROCEDURE ArrayBuild(cons: M3AST_AS.Constructor) RAISES {}=
  VAR
    iter := SeqM3AST_AS_CONS_ELEM.NewIter(cons.as_element_s);
    elem: M3AST_AS.CONS_ELEM;
  BEGIN
    WHILE SeqM3AST_AS_CONS_ELEM.Next(iter, elem) DO
      TYPECASE elem OF <*NOWARN*>
      | M3AST_AS.Actual_elem =>
          M3Error.Report(elem,
              "keyword bindings not allowed in array constructor");
      | M3AST_AS.RANGE_EXP_elem(rangeExpElem) =>
          TYPECASE rangeExpElem.as_range_exp OF <*NOWARN*>
          | M3AST_AS.Range =>
              M3Error.Report(elem, "range not allowed in array constructor");
          | M3AST_AS.Range_EXP(rangeExp) =>
              CheckClassAndAddConstructorElement(cons, rangeExp);
          END;
      END; (* typecase *)
    END;
  END ArrayBuild;


PROCEDURE SetBuild(cons: M3AST_AS.Constructor) RAISES {}=
  VAR
    iter := SeqM3AST_AS_CONS_ELEM.NewIter(cons.as_element_s);
    elem: M3AST_AS.CONS_ELEM;
  BEGIN
    WHILE SeqM3AST_AS_CONS_ELEM.Next(iter, elem) DO
      TYPECASE elem OF <*NOWARN*>
      | M3AST_AS.Actual_elem =>
          M3Error.Report(elem,
               "keyword bindings not allowed in set constructor");
      | M3AST_AS.RANGE_EXP_elem(rangeExpElem) =>
          TYPECASE rangeExpElem.as_range_exp OF <*NOWARN*>
          | M3AST_AS.Range(range) =>
              IF CheckClass(range.as_exp1) AND CheckClass(range.as_exp2) THEN
                AddConstructorElement(cons, range);
              END;
          | M3AST_AS.Range_EXP(rangeExp) =>
              CheckClassAndAddConstructorElement(cons, rangeExp);
          END;
      END;
    END;
    IF cons.as_propagate # NIL THEN
      M3Error.Report(cons, "propagation not allowed in set constructor");
    END; (* if *)
  END SetBuild;


PROCEDURE Set(constructor: M3AST_AS.Constructor) RAISES {}=
  BEGIN
    TYPECASE M3CTypesMisc.CheckedUnpack(constructor.sm_exp_type_spec) OF
    | NULL =>
        (* ignore it *)
    | M3AST_AS.Array_type =>
        ArrayBuild(constructor);
    | M3AST_AS.Record_type(recordType) =>
        RecordBuild(constructor, recordType);
    | M3AST_AS.Set_type =>
        SetBuild(constructor);
    ELSE
      M3Error.Report(constructor, "bad type for constructor");
    END;
  END Set;


<*INLINE*> PROCEDURE ExpCheck(
    type: M3AST_SM.TYPE_SPEC_UNSET;
    exp: M3AST_AS.EXP;
    safe: BOOLEAN)
    RAISES {}=
  BEGIN
    IF exp # NIL AND NOT M3CTypeChkUtil.EXPAssignable(type, exp, safe) THEN
      M3Error.Report(exp,
          "expression in constructor not assignable to element type");
    END; (* if *)
  END ExpCheck;


PROCEDURE ElementCheck(
    type: M3AST_SM.TYPE_SPEC_UNSET;
    rangeExp: M3AST_AS.RANGE_EXP;
    safe: BOOLEAN)
    RAISES {}=
  BEGIN
    TYPECASE rangeExp OF <*NOWARN*>
    | NULL =>
    | M3AST_AS.Range_EXP(rExp) =>
        ExpCheck(type, rExp.as_exp, safe);
    | M3AST_AS.Range(range) =>
        ExpCheck(type, range.as_exp1, safe);
        ExpCheck(type, range.as_exp2, safe);
    END;
  END ElementCheck;


PROCEDURE NumberCheck(
    cons: M3AST_AS.Constructor;
    arrayType: M3AST_AS.Array_type;
    count: INTEGER)
    RAISES {}=
  VAR
    propagate := cons.as_propagate # NIL;
    indexType: M3AST_SM.TYPE_SPEC_UNSET;
    number: M3AST_SM.Exp_value;
    intNumber: INTEGER;
  BEGIN
    CASE M3CTypesMisc.Index(arrayType, indexType) OF
    | M3CTypesMisc.Ix.Open =>
        IF propagate THEN
          M3Error.Report(cons,
              "propagation not allowed in open array constructor");
        END; (* if *)
    | M3CTypesMisc.Ix.Ordinal =>
        IF M3CExpValue.Number(indexType, number) =
                M3CBackEnd.NumStatus.Valid AND
            M3CBackEnd.Ord(number, intNumber) = M3CBackEnd.NumStatus.Valid THEN
          IF intNumber < count THEN
            M3Error.Report(cons, "too many elements in array constructor");
          ELSIF intNumber > count AND NOT propagate THEN
            M3Error.Report(cons, "too few elements in array constructor");
          END; (* if *)
        END; (* if *)
    ELSE
      (* bad or unset index type; can't do any checking *)
    END; (* case *)
  END NumberCheck;


PROCEDURE RecordCheck(
    recordType: M3AST_AS.Record_type;
    rangeExps: SeqM3AST_AS_RANGE_EXP.T;
    safe: BOOLEAN)
    RAISES {}=
  VAR
    iterFields := M3ASTNext.NewIterField(recordType.as_fields_s);
    iterRangeExps := SeqM3AST_AS_RANGE_EXP.NewIter(rangeExps);
    fieldId: M3AST_AS.Field_id;
    rangeExp: M3AST_AS.RANGE_EXP;
  BEGIN
    WHILE M3ASTNext.Field(iterFields, fieldId) AND
        SeqM3AST_AS_RANGE_EXP.Next(iterRangeExps, rangeExp) DO
      ElementCheck(fieldId.sm_type_spec, rangeExp, safe);
    END; (* while *)
  END RecordCheck;


PROCEDURE ElementsCheck(
    type: M3AST_SM.TYPE_SPEC_UNSET;
    rangeExps: SeqM3AST_AS_RANGE_EXP.T;
    safe: BOOLEAN)
    : INTEGER
    RAISES {}=
  VAR
    iter := SeqM3AST_AS_RANGE_EXP.NewIter(rangeExps);
    rangeExp: M3AST_AS.RANGE_EXP;
    count := 0;
  BEGIN
    WHILE SeqM3AST_AS_RANGE_EXP.Next(iter, rangeExp) DO
      ElementCheck(type, rangeExp, safe);
      INC(count);
    END; (* while *)
    RETURN count;
  END ElementsCheck;


PROCEDURE TypeCheck(
    constructor: M3AST_AS.Constructor;
    safe: BOOLEAN)
    RAISES {}=
  VAR
    rangeExps := constructor.sm_actual_s;
    elementType: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    TYPECASE M3CTypesMisc.CheckedUnpack(constructor.sm_exp_type_spec) OF
    | NULL =>
    | M3AST_AS.Array_type(arrayType) =>
        M3CTypesMisc.GetTYPE_SPECFromM3TYPE(
          arrayType.sm_norm_type.as_elementtype, elementType);
        NumberCheck(constructor, arrayType,
            ElementsCheck(elementType, rangeExps, safe));
    | M3AST_AS.Record_type(recordType) =>
        RecordCheck(recordType, rangeExps, safe);
    | M3AST_AS.Set_type(setType) =>
        M3CTypesMisc.GetTYPE_SPECFromM3TYPE(setType.as_type, elementType);
        EVAL ElementsCheck(elementType, rangeExps, safe);
    ELSE
     (* nothing to do *)
    END;
  END TypeCheck;


BEGIN
END M3CConsActualS.
