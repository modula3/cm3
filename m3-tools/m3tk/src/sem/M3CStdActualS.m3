MODULE M3CStdActualS;

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

IMPORT Text;

IMPORT M3AST_AS, M3AST_SM, M3ASTNext;

IMPORT M3AST_AS_F, M3AST_SM_F;

IMPORT SeqM3AST_AS_EXP;

IMPORT M3CStdProcs, M3CTypesMisc, M3CTypeChkUtil, M3CActualUtil, M3CExpsMisc,
    M3CTypeRelation, M3Error, M3CStdTypes,
    M3COrdinal;


(* First 'Set' and the routines it uses to build the sm_actual_s list, check
the number and class of actuals etc. *)

PROCEDURE IsCorrectClass(
    pf: M3CStdProcs.T;
    pos: INTEGER;
    class: M3CExpsMisc.Class)
    : BOOLEAN
    RAISES {}=
  BEGIN
    CASE class OF
    | M3CExpsMisc.Class.Type =>
        IF pos = 0 THEN
          RETURN pf IN M3CStdProcs.FirstParameterCanBeType;
        ELSE
          RETURN pos = 1 AND pf IN M3CStdProcs.SecondParameterCanBeType;
        END; (* if *)
    | M3CExpsMisc.Class.Normal =>
        IF pos = 0 THEN
          RETURN pf IN M3CStdProcs.FirstParameterCanBeNormal;
        ELSE
          RETURN pos > 1 OR pf IN M3CStdProcs.SecondParameterCanBeNormal;
        END; (* if *)
    ELSE
      RETURN FALSE;
    END; (* case *)
  END IsCorrectClass;


<*INLINE*> PROCEDURE CheckClass(
    call: M3AST_AS.Call;
    pf: M3CStdProcs.T;
    pos: INTEGER;
    exp: M3AST_SM.EXP_UNSET)
    : M3AST_SM.EXP_UNSET
    RAISES {}=
  VAR
    class: M3CExpsMisc.Class;
  BEGIN
    IF exp = NIL THEN RETURN NIL END;
    IF ISTYPE(exp, M3AST_SM.TypeActual) THEN
      class := M3CExpsMisc.Class.Type;
    ELSE
      class := M3CExpsMisc.Class.Normal;
    END;
    IF IsCorrectClass(pf, pos, class) THEN
      RETURN exp;
    ELSE
      (* We need original actual because 'exp' may be type actual, which is
       not an original part of the tree and will not be walked by the error
       detection pass *)
      M3CExpsMisc.WrongClass(M3CActualUtil.OriginalActual(call, pos), class);
      RETURN NIL;
    END;
  END CheckClass;


PROCEDURE Set(call: M3AST_AS.Call; pf: M3CStdProcs.T) RAISES {}=
  VAR
    actuals := M3CActualUtil.ActualList(call, TRUE);
    count := M3CActualUtil.PositionalActuals(actuals);
    iter: M3ASTNext.IterFormal;
    pos := 0;
    formal: M3AST_AS.Formal_param;
    formalId: M3AST_AS.FORMAL_ID;
    exp: M3AST_SM.EXP_UNSET;
  BEGIN
    WITH procType =
        NARROW(call.as_callexp.sm_exp_type_spec, M3AST_AS.Procedure_type) DO
      iter := M3ASTNext.NewIterFormal(procType.as_formal_param_s);
    END;
    pos := 0;
    LOOP
      IF NOT M3ASTNext.Formal(iter, formal, formalId) THEN EXIT END;
      IF pos >= count THEN
        IF pf = M3CStdProcs.T.Float THEN
          (* cant represent a default formal of Type_type, so fix up here. *)
          VAR ta: M3AST_SM.TypeActual := NEW(M3AST_SM.TypeActual).init();
          BEGIN
            ta.sm_exp_type_spec := M3CStdTypes.Real();
            SeqM3AST_AS_EXP.AddRear(call.sm_actual_s, ta);
          END;
        ELSIF NOT M3CActualUtil.AddDefault(call, formal) THEN
          M3CActualUtil.TooFewArguments(call);
          EXIT;
        END; (* if *)
      ELSE
        exp := CheckClass(call, pf, pos,
            M3CActualUtil.ActualAt(actuals, pos, NIL));
        SeqM3AST_AS_EXP.AddRear(call.sm_actual_s, exp);
      END; (* if *)
      INC(pos);
    END; (* loop *)
    IF pos < count THEN M3CActualUtil.TooManyArguments(call) END;
    M3CActualUtil.FindUnmatched(actuals);
  END Set;


(* type errors *)

CONST
  WrongType = "argument is wrong type";
  NotWriteableDesignator = "argument is not writeable designator";
  BadTypeConversion = "cannot convert to given type";
  IllegalOpenArray = "argument is open array";
  NotDesignator = "argument is not designator";
  BoundOfEmptyEnumeration = "cannot find bound of empty enumeration type";


PROCEDURE Error(
    call: M3AST_AS.Call;
    pos: INTEGER;
    exp: M3AST_AS.EXP;
    msg: Text.T)
    RAISES {}=
  VAR
    actual: M3AST_AS.Actual;
    en: M3Error.ERROR_NODE;
  BEGIN
    IF ISTYPE(exp, M3AST_SM.TypeActual) THEN
      (* can't hang error from it; it's not part of the syntactic tree *)
      actual := M3CActualUtil.OriginalActual(call, pos);
      en := actual.as_exp_type;
    ELSE
      en := exp;
    END; (* if *)
    M3Error.Report(en, msg);
  END Error;


PROCEDURE UnsafeCall(call: M3AST_AS.Call) RAISES {}=
  VAR
  BEGIN
    M3Error.Report(call, "unsafe call not permitted in safe module");
  END UnsafeCall;


(* utility routines for simple type and expression checking *)

PROCEDURE LoopholePossible(
    type: M3AST_SM.TYPE_SPEC_UNSET;
    exp: M3AST_SM.EXP_UNSET)
    : BOOLEAN
    RAISES {}=
  BEGIN
    IF exp = NIL OR exp.sm_exp_type_spec = NIL OR type = NIL THEN
      RETURN TRUE; (* something else is already wrong *)
    ELSE
      WITH ets = exp.sm_exp_type_spec DO
        IF M3CTypesMisc.IsOpenArray(type) THEN
          (* element type cannot be open array and size must divide exactly *)
          VAR at := NARROW(type, M3AST_AS.Array_type);
              ats: M3AST_AS.TYPE_SPEC;
          BEGIN
            M3CTypesMisc.GetTYPE_SPECFromM3TYPE(at.as_elementtype, ats);
            IF M3CTypesMisc.IsOpenArray(ats) THEN RETURN FALSE
            ELSE
              IF ets.sm_bitsize <= 0 OR  ats.sm_bitsize <= 0 THEN
                RETURN TRUE (* something else is already wrong *)
              ELSE
                RETURN ets.sm_bitsize MOD ats.sm_bitsize = 0;
              END
            END
          END;
        ELSE
          RETURN type.sm_bitsize = ets.sm_bitsize;
        END;
      END;
    END
  END LoopholePossible;


PROCEDURE IsTracedOrIsObject(
    type: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {}=
  VAR
    reveal: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    IF M3CTypesMisc.IsTracedRef(type) IN M3CTypesMisc.ProbablyTraced THEN
      (* this arm catches 'NIL' *)
      RETURN TRUE;
    ELSE
      reveal := M3CTypesMisc.Reveal(type);
      RETURN reveal = NIL OR ISTYPE(reveal, M3AST_AS.Object_type) OR
          ISTYPE(reveal, M3AST_AS.Root_type);
    END;
  END IsTracedOrIsObject;


TYPE
  Array = {Not, Open, Errant, Ordinal};


PROCEDURE IsArray(
    type: M3AST_SM.TYPE_SPEC_UNSET;
    VAR index: M3AST_SM.TYPE_SPEC_UNSET)
    : Array
    RAISES {}=
  BEGIN
    TYPECASE type OF
    | NULL =>
        RETURN Array.Errant;
    | M3AST_AS.Array_type(at) =>
        CASE M3CTypesMisc.Index(at, index) OF
        | M3CTypesMisc.Ix.Unknown, M3CTypesMisc.Ix.Bad =>
            RETURN Array.Errant;
        | M3CTypesMisc.Ix.Open =>
            RETURN Array.Open;
        | M3CTypesMisc.Ix.Ordinal =>
            RETURN Array.Ordinal;
        END;
    ELSE
      RETURN Array.Not;
    END; (* if *)
  END IsArray;


PROCEDURE SameBaseType(
    type1, type2: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {}=
  BEGIN
    RETURN M3CTypeRelation.SameOrdinalSupertype(type1, type2) OR
        M3CTypeRelation.Identical(type1, type2);
  END SameBaseType;


PROCEDURE IsReal(type: M3AST_SM.TYPE_SPEC_UNSET): BOOLEAN RAISES {}=
  BEGIN
    RETURN type = NIL OR ISTYPE(type, M3AST_AS.FLOAT_TYPE);
  END IsReal;


PROCEDURE Disposable(type: M3AST_SM.TYPE_SPEC_UNSET): BOOLEAN RAISES {}=
  BEGIN
    TYPECASE type OF
    | M3AST_AS.Ref_type, M3AST_AS.Object_type =>
        (* Catches NULL *)
        RETURN TRUE;
    | M3AST_AS.Opaque_type =>
        RETURN M3CTypesMisc.IsConcrete(type, FALSE);
    ELSE
      RETURN FALSE;
    END;
  END Disposable;


PROCEDURE IsDesignator(
    exp: M3AST_SM.EXP_UNSET;
    VAR writeable: BOOLEAN)
    : BOOLEAN
    RAISES {}=
  BEGIN
    IF exp # NIL THEN
      RETURN M3CExpsMisc.IsDesignator(exp, writeable);
    ELSE
      writeable := TRUE;
      RETURN TRUE;
    END; (* if *)
  END IsDesignator;


PROCEDURE IsWriteableDesignator(exp: M3AST_SM.EXP_UNSET): BOOLEAN RAISES {}=
  VAR
    writeable: BOOLEAN;
  BEGIN
    RETURN IsDesignator(exp, writeable) AND writeable;
  END IsWriteableDesignator;


PROCEDURE ExprAssignable(
    type: M3AST_SM.TYPE_SPEC_UNSET;
    exp: M3AST_SM.EXP_UNSET;
    safe: BOOLEAN)
    : BOOLEAN
    RAISES {}=
  BEGIN
    IF exp # NIL THEN
      RETURN M3CTypeChkUtil.EXPAssignable(type, exp, safe);
    ELSE
      RETURN TRUE;
    END; (* if *)
  END ExprAssignable;


PROCEDURE ExprAssignableToCardinal(
    exp: M3AST_SM.EXP_UNSET;
    safe: BOOLEAN)
    : BOOLEAN
    RAISES {}=
  BEGIN
    RETURN ExprAssignable(M3CStdTypes.Cardinal(), exp, safe);
  END ExprAssignableToCardinal;


PROCEDURE CheckOrdinalOrFloat(
    type: M3AST_SM.TYPE_SPEC_UNSET;
    number: BOOLEAN; floatOk: BOOLEAN)
    : Text.T
    RAISES {}=
  VAR
    baseType: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    IF floatOk AND NOT number AND
        ISTYPE(type, M3AST_AS.FLOAT_TYPE) THEN RETURN NIL END;

    IF NOT M3COrdinal.Is(type, baseType) THEN RETURN WrongType END;
    IF NOT number AND baseType # NIL AND
        ISTYPE(baseType, M3AST_AS.Enumeration_type) AND
        M3CTypesMisc.IsEmpty(baseType) THEN
      RETURN BoundOfEmptyEnumeration;
    ELSE
      RETURN NIL;
    END; (* if *)
  END CheckOrdinalOrFloat;


(* the main type checking routine *)

PROCEDURE TypeCheck(
    call: M3AST_AS.Call;
    pf: M3CStdProcs.T;
    safe: BOOLEAN)
    RAISES {}=
  VAR
    actuals := call.sm_actual_s;
    iter := SeqM3AST_AS_EXP.NewIter(actuals);
    pos := 0;
    wrongClass, isType, writeable: BOOLEAN;
    arg, prevArg: M3AST_SM.EXP_UNSET;
    type, prevType, baseType, index: M3AST_SM.TYPE_SPEC_UNSET;
    error: Text.T;
  BEGIN
    IF pf IN M3CStdProcs.Unsafe AND safe THEN
      UnsafeCall(call)
    END; (* if *)
    WHILE SeqM3AST_AS_EXP.Next(iter, arg) DO
      wrongClass := arg = NIL;
      isType := NOT wrongClass AND ISTYPE(arg, M3AST_SM.TypeActual);
      IF NOT wrongClass THEN
        type := arg.sm_exp_type_spec;
        error := NIL;
        CASE pf OF <*NOWARN*>
        | M3CStdProcs.T.Inc, M3CStdProcs.T.Dec =>
            IF pos = 0 THEN
              IF NOT M3COrdinal.Is(type, baseType) AND
                  (safe OR NOT M3CTypeChkUtil.IsSubTypeOfAddress(type)) THEN
                error := WrongType;
              ELSIF NOT IsWriteableDesignator(arg) THEN
                error := NotWriteableDesignator
              END; (* if *)
            ELSE
              IF NOT M3CTypeChkUtil.IsSubTypeOfInteger(type) THEN
                error := WrongType;
              END;
            END; (* if *)
        | M3CStdProcs.T.Dispose =>
            IF NOT Disposable(type) THEN
              error := WrongType;
            ELSIF NOT IsWriteableDesignator(arg) THEN
              error := NotWriteableDesignator;
            END; (* if *)
        | M3CStdProcs.T.Abs =>
            IF NOT
                (M3CTypeChkUtil.IsSubTypeOfInteger(type) OR IsReal(type)) THEN
              error := WrongType;
            END; (* if *)
        | M3CStdProcs.T.Float =>
            IF pos = 0 THEN
              IF NOT
                 (M3CTypeChkUtil.IsSubTypeOfInteger(type) OR IsReal(type)) THEN
                error := WrongType;
              END; (* if *)
            ELSIF pos = 1 THEN
              IF NOT IsReal(type) THEN
                error := WrongType;
              END; (* if *)
            END; (* if *)
        | M3CStdProcs.T.Floor, M3CStdProcs.T.Ceiling,
          M3CStdProcs.T.Round, M3CStdProcs.T.Trunc =>
            IF NOT IsReal(type) THEN error := WrongType END;
        | M3CStdProcs.T.Max, M3CStdProcs.T.Min =>
            IF (NOT (M3COrdinal.Is(type, baseType) OR IsReal(type))) OR
                ((pos = 1) AND (NOT SameBaseType(prevType, type))) THEN
              error := WrongType;
            END; (* if *)
        | M3CStdProcs.T.Ord =>
            IF NOT M3COrdinal.Is(type, baseType) THEN error := WrongType END;
        | M3CStdProcs.T.Val =>
            IF pos = 0 THEN
              IF NOT M3CTypeChkUtil.IsSubTypeOfInteger(type) THEN
                error := WrongType;
              END;
            ELSE
              IF NOT M3COrdinal.Is(type, baseType) THEN error := WrongType END;
            END; (* if *)
        | M3CStdProcs.T.Number, M3CStdProcs.T.First, M3CStdProcs.T.Last =>
            CASE IsArray(type, index) OF
            | Array.Not =>
                IF isType THEN
                  error := CheckOrdinalOrFloat(type,
                      pf = M3CStdProcs.T.Number, TRUE);
                ELSE
                  error := WrongType;
                END;
            | Array.Ordinal =>
                error := CheckOrdinalOrFloat(index,
                    pf = M3CStdProcs.T.Number, FALSE);
            | Array.Open =>
                IF isType THEN error := WrongType END;
            | Array.Errant =>
            END;
        | M3CStdProcs.T.TypeCode =>
            IF isType THEN
              IF type # NIL AND
                  (NOT IsTracedOrIsObject(type) OR
                  ISTYPE(type, M3AST_AS.RefAny_type)) THEN
                error := WrongType;
              END;
            ELSE
              IF NOT IsTracedOrIsObject(type) THEN
                error := WrongType;
              END;
            END; (* if *)
        | M3CStdProcs.T.Narrow, M3CStdProcs.T.IsType =>
            IF pos = 0 THEN
              IF NOT IsTracedOrIsObject(type) THEN error := WrongType END;
            ELSE
              IF NOT IsTracedOrIsObject(type) THEN
                error := WrongType;
              ELSIF NOT ExprAssignable(type, prevArg, safe) THEN
                error := BadTypeConversion;
              END; (* if *)
            END; (* if *)
        | M3CStdProcs.T.BitSize, M3CStdProcs.T.ByteSize,
          M3CStdProcs.T.AdrSize =>
            IF isType THEN
              IF M3CTypesMisc.IsOpenArray(type) THEN
                error := IllegalOpenArray;
              END; (* if *)
            ELSE
              IF NOT IsDesignator(arg, writeable) THEN
                error := NotDesignator;
              END; (* if *)
            END; (* if *)
        | M3CStdProcs.T.Loophole =>
            IF pos = 0 THEN
              IF M3CTypesMisc.IsOpenArray(type) THEN
                error := IllegalOpenArray;
              END; (* if *)
            ELSE
              IF NOT LoopholePossible(type, prevArg) THEN
                error := BadTypeConversion;
              END; (* if *)
            END; (* if *)
        | M3CStdProcs.T.Adr =>
            IF NOT IsDesignator(arg, writeable) THEN
              error := NotDesignator;
            END; (* if *)
        | M3CStdProcs.T.Subarray =>
            IF pos = 0 THEN
              IF IsArray(type, index) = Array.Not THEN error := WrongType END;
            ELSE
              IF NOT ExprAssignableToCardinal(arg, safe) THEN
                 error := WrongType;
              END;
            END; (* if *)
        END; (* case *)
        IF error # NIL THEN Error(call, pos, arg, error) END;
      ELSE
        (* actual expression was wrong class - message already given by Set *)
      END; (* if *)
      INC(pos);
      IF (NOT wrongClass) AND (error = NIL) THEN
        prevArg := arg; prevType := type;
      ELSE
        prevArg := NIL;
        prevType := NIL;
      END; (* if *) 
    END; (* while *)
  END TypeCheck;


BEGIN
END M3CStdActualS.
