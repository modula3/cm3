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
(**)
(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)


MODULE M3CExpValue;

IMPORT Text, Fmt;
IMPORT AST, M3AST_AS, M3AST_SM;

IMPORT M3AST_AS_F, M3AST_SM_F, M3AST_TM_F;

IMPORT SeqM3AST_AS_M3TYPE, SeqM3AST_AS_RANGE_EXP, SeqM3AST_AS_EXP,
    SeqM3AST_AS_Actual, SeqM3AST_AS_CONS_ELEM;
IMPORT M3ASTNext;
IMPORT M3CLiteral;

IMPORT ASTWalk;
IMPORT M3Error, M3Assert;
IMPORT M3CStdProcs, M3CWordProcs, M3CStdTypes;
IMPORT M3CTypesMisc;
IMPORT M3COrdinal;
IMPORT M3CExpsMisc;
IMPORT M3CBackEnd, M3CBitSize;

(* We try to evaluate all expressions which can be evaluated at compile time.
This code handles both items which must be constant (e.g. the expression in
a constant declaration) and also normal expressions which happen to be
constant. Note that there are some differences between constant folding in
normal expressions and evaluating constant expressions. e.g.
CONST C = BYTESIZE(v^);
is illegal and must not be evaluated due to potential recursion. On the other
hand in:
FOR i := 0 TO BYTESIZE(v^) DO
It is ok to try and evaluate BYTESIZE *)


(* Useful constants *)

CONST
  ExpOrType = 
      M3CExpsMisc.ClassSet{M3CExpsMisc.Class.Normal, M3CExpsMisc.Class.Type};
  (* Useful when checking actuals to built in functions; these are the only
   classes we are ever interested in *)


(* Simple utility routines *)

PROCEDURE BackEndFailure(e: M3AST_AS.EXP; ns: M3CBackEnd.NumStatus) RAISES {}=
  BEGIN
    IF ns = M3CBackEnd.NumStatus.Unknown THEN
      M3Error.Report(e, "expression cannot be evaluated");
    ELSIF ns = M3CBackEnd.NumStatus.Overflow THEN
      M3Error.Report(e, "expression overflow");
    END; (* if *)
  END BackEndFailure;


<*INLINE*> PROCEDURE ChkVal(e: M3AST_AS.EXP; ns: M3CBackEnd.NumStatus) RAISES {}=
  BEGIN
    IF ns # M3CBackEnd.NumStatus.Valid THEN BackEndFailure(e, ns) END;
  END ChkVal;


<*INLINE*> PROCEDURE ValIsOK(
    e: M3AST_AS.EXP;
    ns: M3CBackEnd.NumStatus)
    : BOOLEAN
    RAISES {}=
  BEGIN
    IF ns = M3CBackEnd.NumStatus.Valid THEN
      RETURN TRUE;
    ELSE
      BackEndFailure(e, ns);
      RETURN TRUE;
    END;
  END ValIsOK;


PROCEDURE ConvertToInt(
    e: M3AST_SM.Exp_value;
    VAR er: M3AST_SM.Exp_value)
    : M3CBackEnd.NumStatus
    RAISES {}=
  BEGIN
    RETURN M3CBackEnd.ConvertOrdinal(e, M3CStdTypes.Integer(), er);
  END ConvertToInt;


TYPE
  Mode = {Walk, WalkConst, Recursive};
  ModeSet = SET OF Mode;


<*INLINE*> PROCEDURE EvalComponent(
    e: M3AST_AS.EXP;
    mode: ModeSet)
    : M3AST_SM.Exp_value
    RAISES {}=
  BEGIN
    IF Mode.Recursive IN mode THEN
      RETURN Eval(e, mode);
    ELSE
      RETURN e.sm_exp_value;
    END;
  END EvalComponent;


<*INLINE*> PROCEDURE LiteralLastChar(l: M3CLiteral.T): CHAR RAISES {}=
  VAR
    t := M3CLiteral.ToText(l);
  BEGIN
    (* Assert: lexer guarantees length of literal is never 0 *)
    RETURN Text.GetChar(t, Text.Length(t) - 1);
  END LiteralLastChar;


PROCEDURE NotConstant(e: M3AST_AS.EXP) RAISES {}=
  BEGIN
    M3Error.Report(e, "expression is not constant");
  END NotConstant;


(* Evaluating constant expressions recursively; needs to be done to handle
forward references *)

PROCEDURE GetValueForUsedId(
    id: M3AST_AS.Exp_used_id;
    ccv_id: M3AST_SM.CCV_ID)
    : M3AST_SM.Exp_value
    RAISES {}=
(* 'ccv_id' must always be the CCV_ID field of the 'sm_def' of 'id' and the def
of 'id' must always be a constant or enumeration id *)
  VAR
    er: M3AST_SM.Exp_value;
  BEGIN
    (* This sets id.sm_exp_value and returns the same value. *)
    er := ccv_id.sm_exp_value;
    IF er = NIL THEN
      TYPECASE id.vUSED_ID.sm_def OF <*NOWARN*>
      | M3AST_AS.Const_id(constId) =>
          (* Const_ids can be (recursively) computed through the sm_init_exp
             attribute, but watch for illegal recursive declarations. *)
          IF NOT constId.tmp_recursive THEN 
            er := Eval(constId.vINIT_ID.sm_init_exp, ModeSet{Mode.Recursive});
            ccv_id.sm_exp_value := er;
          END;
      | M3AST_AS.Enum_id =>
          (* Should have been precomputed in M3CTypeSpec pass1; if we get here
           there has been an error; leave 'er' at NIL *)
      END; (* typecase *)
    END; (* if *)
    id.sm_exp_value := er;
    RETURN er;
  END GetValueForUsedId;


(* Type utilities *)
 
PROCEDURE IsOrdinal(
    VAR ts: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {}=
  VAR
    void: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    LOOP
      TYPECASE ts OF
      | NULL =>
          RETURN FALSE;
      | M3AST_AS.Packed_type(packed) =>
          (* We handle this rather than leaving it to 'M3COrdinal' because we
           want to return FALSE if 'ts' is NIL *)
          ts := M3CTypesMisc.Unpack(packed);
      ELSE
        RETURN M3COrdinal.Is(ts, void);
      END;
    END;
  END IsOrdinal;


PROCEDURE IsOrdinalFloatOrArrayType(
    VAR (*inout*) ts: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {}=
  BEGIN
    ts := M3CTypesMisc.CheckedUnpack(ts);
    TYPECASE ts OF
    | NULL =>
        RETURN FALSE;
    | M3AST_AS.Array_type(arrayType) =>
        VAR
          iter := SeqM3AST_AS_M3TYPE.NewIter(arrayType.as_indextype_s);
          it: M3AST_AS.M3TYPE;
        BEGIN
          IF SeqM3AST_AS_M3TYPE.Next(iter, it) THEN
            M3CTypesMisc.GetTYPE_SPECFromM3TYPE(it, ts);
          ELSE
            (* open array/error *)
            ts := NIL;
            RETURN FALSE;
          END; (* if *)
        END;
    | M3AST_AS.FLOAT_TYPE => RETURN TRUE;
    ELSE
    END; (* if *)
    RETURN IsOrdinal(ts);
  END IsOrdinalFloatOrArrayType;


(* Useful "constants". They are not initialized immediately to avoid problems
with complicated intialization order *)

VAR
  minus_g: M3AST_AS.Minus := NIL;
  plus_g: M3AST_AS.Plus := NIL;
  zero_g, one_g: M3AST_SM.Exp_value := NIL;


PROCEDURE Minus(): M3AST_AS.Minus RAISES {}=
  BEGIN
    IF minus_g = NIL THEN minus_g := NEW(M3AST_AS.Minus).init() END;
    RETURN minus_g;
  END Minus;


PROCEDURE Plus(): M3AST_AS.Plus RAISES {}=
  BEGIN
    IF plus_g = NIL THEN plus_g := NEW(M3AST_AS.Plus).init() END;
    RETURN plus_g;
  END Plus;


PROCEDURE Zero(): M3AST_SM.Exp_value RAISES {}=
  BEGIN
    IF zero_g = NIL THEN
      EVAL M3CBackEnd.Val(0, M3CStdTypes.Integer(), zero_g);
    END;
    RETURN zero_g;
  END Zero;


PROCEDURE One(): M3AST_SM.Exp_value RAISES {}=
  BEGIN
    IF one_g = NIL THEN
      EVAL M3CBackEnd.Val(1, M3CStdTypes.Integer(), one_g);
    END;
    RETURN one_g;
  END One;


(* Constructing boolean results *)

PROCEDURE NewBoolean(b: BOOLEAN): M3AST_SM.Exp_value RAISES {}=
  VAR
    er: M3AST_SM.Exp_value;
  BEGIN
    M3Assert.Check(M3CBackEnd.Val(ORD(b), M3CStdTypes.Boolean(), er) =
        M3CBackEnd.NumStatus.Valid);
    RETURN er;
  END NewBoolean;


(* Exported utility routines *)

PROCEDURE GetBounds(
    ts: M3AST_SM.TYPE_SPEC_UNSET;
    VAR low, high: M3AST_SM.Exp_value)
    : M3CBackEnd.NumStatus
    RAISES {}=
  BEGIN 
    TYPECASE M3CTypesMisc.CheckedUnpack(ts) OF
    | NULL =>
        RETURN M3CBackEnd.NumStatus.Unknown

    | M3AST_AS.Integer_type(integerType) =>
        EVAL M3CBackEnd.StdUnaryTypeOp(M3CStdProcs.T.First, integerType, low);
        EVAL M3CBackEnd.StdUnaryTypeOp(M3CStdProcs.T.Last, integerType, high);
        RETURN M3CBackEnd.NumStatus.Valid;

    | M3AST_AS.Enumeration_type(enumType) =>
        VAR
          ord := enumType.sm_num_elements;
        BEGIN
          IF ord = 0 THEN RETURN M3CBackEnd.NumStatus.Unknown END;
          M3Assert.Check(ord > 0 AND
              M3CBackEnd.ConvertOrdinal(Zero(), enumType, low) =
                  M3CBackEnd.NumStatus.Valid AND
              M3CBackEnd.Val(ord-1, enumType, high) =
                  M3CBackEnd.NumStatus.Valid);
          RETURN M3CBackEnd.NumStatus.Valid;
        END;

    | M3AST_AS.Subrange_type(subrangeType) =>
        (* Must eval the expressions in case forward refs *)
        VAR
          e1 := Eval(subrangeType.as_range.as_exp1, ModeSet{Mode.Recursive});
          e2 := Eval(subrangeType.as_range.as_exp2, ModeSet{Mode.Recursive});
        BEGIN
          IF M3COrdinal.ValidBounds(subrangeType, e1, e2) THEN
            low := e1; high := e2;
            RETURN M3CBackEnd.NumStatus.Valid;
          ELSE
            (* Bounds have not both been evaluated or are not compatible *)
            RETURN M3CBackEnd.NumStatus.Unknown;
          END;
        END;

    ELSE
      RETURN M3CBackEnd.NumStatus.Unknown
    END;
  END GetBounds;


PROCEDURE Number(
    ts: M3AST_SM.TYPE_SPEC_UNSET;
    VAR (*out*) number: M3AST_SM.Exp_value)
    : M3CBackEnd.NumStatus
    RAISES {}=
  BEGIN
    TYPECASE M3CTypesMisc.CheckedUnpack(ts) OF
    | NULL =>
        RETURN M3CBackEnd.NumStatus.Unknown;
    | M3AST_AS.Integer_type =>
        RETURN M3CBackEnd.NumStatus.Overflow
    | M3AST_AS.Enumeration_type(enumType) =>
        RETURN M3CBackEnd.Val(enumType.sm_num_elements, enumType, number);
    | M3AST_AS.Subrange_type =>
        VAR
          low, high, diff: M3AST_SM.Exp_value;
        BEGIN
          IF GetBounds(ts, low, high) # M3CBackEnd.NumStatus.Valid OR
              ConvertToInt(low, low) # M3CBackEnd.NumStatus.Valid OR
              ConvertToInt(high, high) # M3CBackEnd.NumStatus.Valid THEN
            RETURN M3CBackEnd.NumStatus.Unknown;
          END;
          WITH compare = M3CBackEnd.Compare(low, high) DO
            IF compare = 0 THEN
              number := One();
              RETURN M3CBackEnd.NumStatus.Valid;
            ELSIF compare > 0 THEN
              number := Zero();
              RETURN M3CBackEnd.NumStatus.Valid;
            END;
          END;
          WITH valid = M3CBackEnd.BinaryOp(Minus(), high, low, diff) DO
            IF valid # M3CBackEnd.NumStatus.Valid THEN RETURN valid END;
          END;
          RETURN M3CBackEnd.BinaryOp(Plus(), diff, One(), number);
        END;
    ELSE
      RETURN M3CBackEnd.NumStatus.Unknown;
    END;
  END Number;


<*INLINE*> PROCEDURE Ordinal(
    e: M3AST_AS.EXP; 
    VAR (*out*) i: INTEGER)
    : M3CBackEnd.NumStatus
    RAISES {}=
  BEGIN
    IF M3CBackEnd.IsOrdinal(e.sm_exp_value) THEN
      RETURN M3CBackEnd.Ord(e.sm_exp_value, i);
    ELSE
      RETURN M3CBackEnd.NumStatus.Unknown;
    END;
  END Ordinal;


(* Constructor operations. Operations on constructors can usually be handled
portably so they are done here rather than in 'M3CBackEnd' *)

PROCEDURE Selection(
    lhs: M3AST_AS.EXP;
    fieldId: M3AST_AS.Field_id)
    : M3AST_SM.Exp_value
    RAISES {}=
(* Only called if 'lhs' is known to be constant *)
  BEGIN
    TYPECASE M3CTypesMisc.CheckedUnpack(lhs.sm_exp_type_spec) OF
    | M3AST_AS.Record_type(recordType) =>
        VAR
          iterFields := M3ASTNext.NewIterField(recordType.as_fields_s);
          search: M3AST_AS.Field_id;
          iterRangeExps := SeqM3AST_AS_RANGE_EXP.NewIter(
              M3CBackEnd.ConstructorOriginal(lhs.sm_exp_value).sm_actual_s);
          rangeExp: M3AST_AS.RANGE_EXP;
        BEGIN
          WHILE M3ASTNext.Field(iterFields, search) AND
              SeqM3AST_AS_RANGE_EXP.Next(iterRangeExps, rangeExp) DO
            IF search = fieldId THEN
              TYPECASE rangeExp OF
              | M3AST_AS.Range_EXP(rExp) =>
                  RETURN rExp.as_exp.sm_exp_value;
              ELSE
              END;
              RETURN NIL;
            END;
          END;
          RETURN NIL;
        END;
    ELSE
      (* Ref record or object - result of dereferencing it is not constant *)
      RETURN NIL;
    END;
  END Selection;


PROCEDURE Index(
    index: M3AST_AS.Index;
    mode: ModeSet)
    : M3AST_SM.Exp_value
    RAISES {}=
(* Called only if 'index.as_array' is known to be constant (so, obviously,
'Eval' must have been called on 'index.as_array). The index expressions have
not yet been evaluated, unless 'componentsKnown' is TRUE. No check has been
done on the type of 'index.as_array'. *)
  VAR
    er: M3AST_SM.Exp_value := NIL;
    arrayExp := index.as_array;
    arrayExpType := arrayExp.sm_exp_type_spec;
    iter := SeqM3AST_AS_EXP.NewIter(index.as_exp_s);
    exp: M3AST_AS.EXP;
  BEGIN
    WHILE SeqM3AST_AS_EXP.Next(iter, exp) DO
      TYPECASE M3CTypesMisc.CheckedUnpack(arrayExpType) OF
      | NULL =>
          RETURN NIL;
      | M3AST_AS.Array_type(arrayType) =>
          VAR
            ix := EvalComponent(exp, mode);
            ixType: M3AST_SM.TYPE_SPEC_UNSET;
            low, high, diff: M3AST_SM.Exp_value;
            ixInt: INTEGER;
          BEGIN
            IF NOT M3CBackEnd.IsOrdinal(ix) THEN RETURN NIL END;
            CASE M3CTypesMisc.Index(arrayType, ixType) OF
            | M3CTypesMisc.Ix.Ordinal =>
                TYPECASE ixType OF
                | M3AST_AS.Subrange_type =>
                    IF NOT ValIsOK(index, GetBounds(ixType, low, high)) THEN
                      RETURN NIL;
                    END;
                ELSE
                  low := Zero();
                END;
            | M3CTypesMisc.Ix.Open =>
                low := Zero();
            ELSE
              RETURN NIL;
            END;
            IF NOT (M3CBackEnd.Compare(low, ix) <= 0 AND
                ValIsOK(index, ConvertToInt(ix, ix)) AND
                ValIsOK(index, ConvertToInt(low, low)) AND
                ValIsOK(index, M3CBackEnd.BinaryOp(Minus(), ix, low, diff)) AND
                ValIsOK(index, M3CBackEnd.Ord(diff, ixInt))) THEN
              RETURN NIL;
            END;
            VAR
              constructor :=
                  M3CBackEnd.ConstructorOriginal(arrayExp.sm_exp_value);
              iter := SeqM3AST_AS_RANGE_EXP.NewIter(constructor.sm_actual_s);
              rangeExp: M3AST_AS.RANGE_EXP;
              count := -1;
            BEGIN
              REPEAT
                IF SeqM3AST_AS_RANGE_EXP.Next(iter, rangeExp) THEN
                  INC(count);
                ELSE
                  IF count < 0 OR constructor.as_propagate = NIL THEN
                    RETURN NIL;
                  ELSE
                    EXIT;
                  END;
                END;
              UNTIL count = ixInt;
              TYPECASE rangeExp OF
              | M3AST_AS.Range_EXP(rExp) =>
                  (* Note constructor cannot contain NIL elements or its
                   'sm_exp_value' field would not have been set up *)
                  arrayExp := rExp.as_exp;
                  M3CTypesMisc.GetTYPE_SPECFromM3TYPE(
                      arrayType.sm_norm_type.as_elementtype, arrayExpType);
                  er := arrayExp.sm_exp_value;
              ELSE
                RETURN NIL;
              END;
            END;
          END;
      ELSE
        (* 'arrayExp' does not have array type; if it's a ref array we can't
         index it in a constant expression; if it's anything else we can't
         index it at all *)
        RETURN NIL;
      END;
    END; (* while *)
    RETURN er;
  END Index;


PROCEDURE EqualConstructors(e1, e2: M3AST_AS.EXP): BOOLEAN RAISES {}=
(* Only called if both 'e1' and 'e2' are known to be constant and either both
arrays or both records *)
  VAR
    c1 := M3CBackEnd.ConstructorOriginal(e1.sm_exp_value);
    c2 := M3CBackEnd.ConstructorOriginal(e2.sm_exp_value);
    iter1 := SeqM3AST_AS_RANGE_EXP.NewIter(c1.sm_actual_s);
    iter2 := SeqM3AST_AS_RANGE_EXP.NewIter(c2.sm_actual_s);
    re1, re2: M3AST_AS.RANGE_EXP;
    exp1, exp2: M3AST_AS.EXP;
    b1, b2: BOOLEAN;
  BEGIN
    LOOP
      b1 := SeqM3AST_AS_RANGE_EXP.Next(iter1, re1);
      b2 := SeqM3AST_AS_RANGE_EXP.Next(iter2, re2);
      IF b1 # b2 THEN EXIT END;
      IF NOT b1 THEN RETURN TRUE END;
      IF re1 = NIL OR re2 = NIL THEN RETURN FALSE END;
      TYPECASE re1 OF
      | M3AST_AS.Range_EXP(rangeExp1) =>
          exp1 := rangeExp1.as_exp;
          IF exp1 = NIL THEN RETURN FALSE END;
          TYPECASE re2 OF
          | M3AST_AS.Range_EXP(rangeExp2) =>
              exp2 := rangeExp2.as_exp;
              IF exp2 = NIL OR NOT Equal(exp1, exp2) THEN RETURN FALSE END;
          ELSE
            RETURN FALSE;
          END;
      ELSE
        RETURN FALSE;
      END;
    END;

    VAR
      iter: SeqM3AST_AS_RANGE_EXP.Iter;
      re: M3AST_AS.RANGE_EXP;
      mustPropagate: M3AST_AS.Constructor;
      propagated: M3AST_AS.EXP;
    BEGIN
      IF b1 THEN
        mustPropagate := c2;
        propagated := exp2;
        iter := iter1;
        re := re1;
      ELSE
        mustPropagate := c1;
        propagated := exp1;
        iter := iter2;
        re := re2;
      END;
      IF mustPropagate.as_propagate = NIL THEN RETURN FALSE END;
      REPEAT
        TYPECASE re OF
        | NULL =>
            RETURN FALSE;
        | M3AST_AS.Range_EXP(rangeExp) =>
            IF rangeExp.as_exp = NIL OR
                NOT Equal(rangeExp.as_exp, propagated) THEN
              RETURN FALSE;
            END;
        ELSE
          RETURN FALSE;
        END;
      UNTIL NOT SeqM3AST_AS_RANGE_EXP.Next(iter, re);
      RETURN TRUE;
    END;
  END EqualConstructors;


PROCEDURE Equal(e1, e2: M3AST_AS.EXP): BOOLEAN RAISES {}=
  BEGIN
    IF e1.sm_exp_value # NIL AND e2.sm_exp_value # NIL THEN
      TYPECASE M3CTypesMisc.CheckedUnpack(e1.sm_exp_type_spec) OF
      | M3AST_AS.Array_type =>
          TYPECASE M3CTypesMisc.CheckedUnpack(e2.sm_exp_type_spec) OF
          | M3AST_AS.Array_type =>
              RETURN EqualConstructors(e1, e2);
          ELSE
            RETURN FALSE;
          END;
      | M3AST_AS.Record_type =>
          TYPECASE M3CTypesMisc.CheckedUnpack(e2.sm_exp_type_spec) OF
          | M3AST_AS.Record_type =>
              RETURN EqualConstructors(e1, e2);
          ELSE
            RETURN FALSE;
          END;
      ELSE
        RETURN M3CBackEnd.Compare(e1.sm_exp_value, e2.sm_exp_value) = 0;
      END;
    ELSE
      RETURN FALSE;
    END;
  END Equal;


(* Handling calls of the built in functions (i.e. the operations which look
like function calls) and the 'Word' functions *)

PROCEDURE EvalActual(
    actual: M3AST_AS.Actual;
    mode: ModeSet)
    : M3AST_SM.Exp_value
    RAISES {}=
  BEGIN
    TYPECASE actual.as_exp_type OF
    | M3AST_AS.EXP(exp) =>
        RETURN EvalComponent(exp, mode);
    ELSE
      RETURN NIL;
    END;
  END EvalActual;


PROCEDURE CheckActual(
    a: M3AST_AS.Actual;
    VAR ts: M3AST_AS.TYPE_SPEC)
    : M3CExpsMisc.Class
    RAISES {}=
  BEGIN
    TYPECASE a.as_exp_type OF <*NOWARN*>
    | M3AST_AS.EXP(exp) =>
        ts := exp.sm_exp_type_spec;
        RETURN M3CExpsMisc.Classify(exp);
    | M3AST_AS.Bad_M3TYPE =>
        ts := NIL;
        RETURN M3CExpsMisc.Class.Type;
    | M3AST_AS.TYPE_SPEC(typeSpec) =>
        ts := typeSpec;
        RETURN M3CExpsMisc.Class.Type;
    END;
  END CheckActual;


<*INLINE*> PROCEDURE IsTypeActual(
    a: M3AST_AS.Actual;
    VAR ts: M3AST_AS.TYPE_SPEC)
    : BOOLEAN
    RAISES {}=
  VAR
    typeSpec: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    IF CheckActual(a, typeSpec) = M3CExpsMisc.Class.Type THEN
      ts := typeSpec;
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END IsTypeActual;


PROCEDURE NotInBounds(val, low, high: M3AST_SM.Exp_value): BOOLEAN RAISES {}=
  BEGIN
    RETURN NOT
        (M3CBackEnd.IsOrdinal(val) AND
        M3CBackEnd.Compare(low, val) <= 0 AND
        M3CBackEnd.Compare(val, high) <= 0);
  END NotInBounds;



CONST
  SpecialCall = M3CStdProcs.ProcFuncSet{
      M3CStdProcs.T.First, M3CStdProcs.T.Last, M3CStdProcs.T.Number,
      M3CStdProcs.T.BitSize, M3CStdProcs.T.ByteSize, M3CStdProcs.T.AdrSize};
  (* These calls are special because their arguments can be variables, even
   in a constant expression. Much code follows to deal with this *)


TYPE
  SpecialCallClosure =
    ASTWalk.Closure OBJECT
      error := FALSE;
    OVERRIDES
      callback := CheckSpecialCallActual;
    END; (* object *)


<*INLINE*> PROCEDURE IsNonNilRefType(
    ts: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {}=
  BEGIN
    RETURN ts # NIL AND M3CTypesMisc.IsRef(ts);
  END IsNonNilRefType;


PROCEDURE CheckSpecialCallActual(
    cl: SpecialCallClosure;
    n: AST.NODE;
    <*UNUSED*> vm: ASTWalk.VisitMode)
    RAISES {}=
  VAR
    error := FALSE;
  BEGIN
    TYPECASE n OF
    | M3AST_AS.Deref =>
        error := TRUE;
    | M3AST_AS.Index(index) =>
        error := IsNonNilRefType(index.as_array.sm_exp_type_spec);
    | M3AST_AS.Select(select) =>
        error := 
            M3CExpsMisc.Classify(select) = M3CExpsMisc.Class.Normal AND
            IsNonNilRefType(select.as_exp.sm_exp_type_spec);
    | M3AST_AS.Call(call) =>
        VAR
          pf: M3CStdProcs.T;
          w: M3CWordProcs.T;
        BEGIN
          error := NOT(
              M3CStdProcs.IsStandardCall(call, pf) AND
                  pf IN M3CStdProcs.AllowedInConstantExpressions OR
              M3CWordProcs.IsWordCall(call, w))
        END;
    ELSE
    END;
    IF error THEN
      cl.error := TRUE;
      M3Error.Report(n, "operation not allowed in constant expression");
    END;
  END CheckSpecialCallActual;


PROCEDURE EvalSpecialCall(
    call: M3AST_AS.Call;
    pf: M3CStdProcs.T;
    ts: M3AST_AS.TYPE_SPEC)
    : M3AST_SM.Exp_value
    RAISES {}=
  VAR
    er: M3AST_SM.Exp_value := NIL;
  BEGIN
    CASE pf OF  <*NOWARN*>
    | M3CStdProcs.T.Number, M3CStdProcs.T.First, M3CStdProcs.T.Last =>
        IF IsOrdinalFloatOrArrayType(ts) THEN
          (* 'IsOrdinalFloatOrArrayType' ensures 'ts' is non NIL *)
          TYPECASE ts OF <*NOWARN*>
          | M3AST_AS.Integer_type, M3AST_AS.FLOAT_TYPE =>
              IF pf = M3CStdProcs.T.Number THEN
                BackEndFailure(call, M3CBackEnd.NumStatus.Overflow);
              ELSE
                ChkVal(call, M3CBackEnd.StdUnaryTypeOp(pf, ts, er));
              END;

          | M3AST_AS.Subrange_type,
            M3AST_AS.Enumeration_type =>
              IF pf = M3CStdProcs.T.Number THEN
                ChkVal(call, Number(ts, er));
              ELSE
                VAR
                  first, last: M3AST_SM.Exp_value;
                BEGIN
                  IF ValIsOK(call, GetBounds(ts, first, last)) THEN
                    IF pf = M3CStdProcs.T.First THEN
                      er := first;
                    ELSE
                      er := last;
                    END;
                  END;
                END;
              END;
          END; (* case *)
        END;
    | M3CStdProcs.T.BitSize, M3CStdProcs.T.ByteSize,
      M3CStdProcs.T.AdrSize =>
        IF ISTYPE(ts, M3AST_AS.Subrange_type) THEN
          (* just make sure subranges are evaluated *)
          VAR
            first, last: M3AST_SM.Exp_value;
          BEGIN
            EVAL GetBounds(ts, first, last);
          END;
        END;
        M3CBitSize.Set(ts);
        IF ts.sm_bitsize > 0 THEN
          ChkVal(call, M3CBackEnd.StdUnaryTypeOp(pf, ts, er));
        END;
    END;
    RETURN er;
  END EvalSpecialCall;


PROCEDURE CheckSpecialCall(
    call: M3AST_AS.Call;
    pf: M3CStdProcs.T;
    cl: Closure)
    : M3AST_SM.Exp_value
    RAISES {}=
  VAR
    actual := SeqM3AST_AS_Actual.First(call.as_param_s);
    typeSpec: M3AST_SM.TYPE_SPEC_UNSET;
    class := CheckActual(actual, typeSpec);
  BEGIN
    IF typeSpec = NIL OR NOT class IN ExpOrType THEN
      (* Error elsewhere *)
      RETURN NIL;
    END;
    IF IsTrulyOpenArray(actual, typeSpec) THEN
      IF cl # NIL AND cl.node # NIL THEN
        (* We are tree walking an expression which should be constant; we
         cannot find the bounds or size of an open array. Ignore children and
         complain *)
        ASTWalk.IgnoreChildren(cl);
        NotConstant(call);
      ELSE
        (* Either we have been called from a recursive 'Eval' or when tree
         walking a non constant expression. In either case we do nothing *)
      END;
      (* Value of expression cannot be determined at compile time *)
      RETURN NIL;
    ELSE
      IF class = M3CExpsMisc.Class.Normal THEN
        (* Expression does not need to be evaluated so if we are in a tree walk
         we ignore children. *)
        IF cl # NIL THEN ASTWalk.IgnoreChildren(cl) END;
        (* If we are being called from a recursive 'Eval' or from a tree walk
         of a constant expression we must check that this is a valid constant
         expression. To do this we have to check the argument to the call to
         make sure it contains no operations which are illegal in constants *)
        IF cl = NIL OR cl.node # NIL THEN
          VAR
            scc := NEW(SpecialCallClosure);
          BEGIN
            <*FATAL ANY*> BEGIN
              ASTWalk.VisitNodes(actual, scc);
            END;
            IF scc.error THEN RETURN NIL END;
          END;
        END;
      ELSE
        (* Argument is a type; any tree walk continues in case type contains
         nested constant expressions *)
      END;
      RETURN EvalSpecialCall(call, pf, typeSpec);
    END;
  END CheckSpecialCall;

PROCEDURE IsTrulyOpenArray(
    actual: M3AST_AS.Actual;
    VAR (*inout *)ts: M3AST_AS.TYPE_SPEC)
    : BOOLEAN
    RAISES {}=
  BEGIN
    IF M3CTypesMisc.IsOpenArray(ts) THEN
      (* permit constants of type open array with fixed length constructors *)
      TYPECASE actual.as_exp_type OF
      | NULL =>
      | M3AST_AS.Exp_used_id(id) =>
          TYPECASE id.vUSED_ID.sm_def OF
          | NULL =>
          | M3AST_AS.Const_id(const_id) =>
              VAR cons := M3CBackEnd.ConstructorOriginal(
                  const_id.vCCV_ID.sm_exp_value);
                  cons_l := SeqM3AST_AS_CONS_ELEM.Length(cons.as_element_s);
              BEGIN
                IF cons.as_propagate = NIL AND cons_l > 0 THEN
                  (* fake a non-open array type *)
                  VAR
                    nts: M3AST_AS.Array_type :=
                      NEW(M3AST_AS.Array_type).init();
                    sr: M3AST_AS.Subrange_type :=
                        NEW(M3AST_AS.Subrange_type).init();
                    r: M3AST_AS.Range := NEW(M3AST_AS.Range).init();
                  BEGIN
                    nts.as_elementtype :=
                        NARROW(ts, M3AST_AS.Array_type).as_elementtype;
                    nts.sm_norm_type := nts;
                    SeqM3AST_AS_M3TYPE.AddFront(nts.as_indextype_s, sr);
                    sr.as_range := r;
                    sr.sm_base_type_spec := M3CStdTypes.Integer();
                    r.as_exp1 := NewInteger_literal(0);
                    r.as_exp2 := NewInteger_literal(cons_l-1);
                    ts := nts;
                  END; (* with *)
                  RETURN FALSE;
                END; (* if *)
              END;
          ELSE
          END;
      ELSE   
      END; (* typecase *)
      RETURN TRUE;
    ELSE
      (* not an open array *)
      RETURN FALSE;
    END; (* if *)
  END IsTrulyOpenArray;

PROCEDURE NewInteger_literal(n: INTEGER): M3AST_AS.Integer_literal=
  VAR lit: M3AST_AS.Integer_literal := NEW(M3AST_AS.Integer_literal).init();
  BEGIN
    lit.lx_litrep := M3CLiteral.Enter(Fmt.Int(n));
    lit.sm_exp_type_spec := M3CStdTypes.Integer();
    RETURN lit;
  END NewInteger_literal;

PROCEDURE StandardCall(
    call: M3AST_AS.Call;
    pf: M3CStdProcs.T;
    mode: ModeSet)
    : M3AST_SM.Exp_value
    RAISES {}=
  VAR
    er: M3AST_SM.Exp_value := NIL;
    iterActuals := SeqM3AST_AS_Actual.NewIter(call.as_param_s);
    actual1, actual2: M3AST_AS.Actual;
    ev1, ev2: M3AST_SM.Exp_value;
    ts: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    IF NOT SeqM3AST_AS_Actual.Next(iterActuals, actual1) THEN
      RETURN NIL;
    END;

    CASE pf OF <*NOWARN*>

    | M3CStdProcs.T.Abs .. M3CStdProcs.T.Trunc =>
        ev1 := EvalActual(actual1, mode);
        IF ev1 # NIL THEN
          IF pf = M3CStdProcs.T.Float THEN
            VAR ft: M3AST_AS.FLOAT_TYPE := NIL;
            BEGIN
              IF SeqM3AST_AS_Actual.Next(iterActuals, actual2) THEN
                TYPECASE actual2.as_exp_type OF
                | NULL =>
                | M3AST_AS.FLOAT_TYPE(x) =>
                    ft := x;
                ELSE    
                END; (* typecase *)
              ELSE
                ft := M3CStdTypes.Real();
              END;
              IF ft # NIL THEN
                ChkVal(call, M3CBackEnd.StdUnaryOp(pf, ev1, er, ft));
              END;
            END;
          ELSE
            ChkVal(call, M3CBackEnd.StdUnaryOp(pf, ev1, er));
          END; (* if *)
        END; (* if *)

    | M3CStdProcs.T.Max, M3CStdProcs.T.Min =>
        IF SeqM3AST_AS_Actual.Next(iterActuals, actual2) THEN
          ev1 := EvalActual(actual1, mode);
          ev2 := EvalActual(actual2, mode);
          IF ev1 # NIL AND ev2 # NIL THEN
            ChkVal(call, M3CBackEnd.StdBinaryOp(pf, ev1, ev2, er));
          END; (* if *)
        END;
          
    | M3CStdProcs.T.Ord =>
        (* input may be int, enum or char *)
        ev1 := EvalActual(actual1, mode);
        IF M3CBackEnd.IsOrdinal(ev1) THEN
          ChkVal(call, ConvertToInt(ev1, er));
        END; (* if *)

    | M3CStdProcs.T.Val =>
        IF SeqM3AST_AS_Actual.Next(iterActuals, actual2) THEN
          ev1 := EvalActual(actual1, mode);
          IF M3CBackEnd.IsOrdinal(ev1) AND
              IsTypeActual(actual2, ts) AND IsOrdinal(ts) THEN
            (* 'IsOrdinal' ensures 'ts' is non NIL *)
            VAR
              boundsOK := FALSE;
            BEGIN
              IF ISTYPE(ts, M3AST_AS.Integer_type) THEN
                boundsOK := TRUE;
              ELSE
                VAR
                  low, high: M3AST_SM.Exp_value;
                BEGIN
                  IF GetBounds(ts, low, high) = M3CBackEnd.NumStatus.Valid THEN
                    IF NotInBounds(ev1, low, high) THEN
                      M3Error.Report(actual1, 
                          "VAL expression out of range for type");
                    ELSE
                      boundsOK := TRUE;
                    END;
                  END;
                END;
              END;
              IF boundsOK THEN
                ChkVal(call, M3CBackEnd.ConvertOrdinal(ev1, ts, er));
              END;
            END;
          ELSE
            (* Errant; nothing we can do *)
          END; (* if *)
        END;

    | M3CStdProcs.T.Number, M3CStdProcs.T.First, M3CStdProcs.T.Last,
      M3CStdProcs.T.BitSize, M3CStdProcs.T.ByteSize,
      M3CStdProcs.T.AdrSize =>
        er := CheckSpecialCall(call, pf, NIL);

    END; (* case *)

    RETURN er;
  END StandardCall;


PROCEDURE WordCall(
    call: M3AST_AS.Call;
    w: M3CWordProcs.T;
    mode: ModeSet)
    : M3AST_SM.Exp_value
    RAISES {}=
  VAR
    er: M3AST_SM.Exp_value := NIL;
    iter := SeqM3AST_AS_EXP.NewIter(call.sm_actual_s);
    exp: M3AST_AS.EXP;
    values: ARRAY [0..M3CWordProcs.MaxArgCount-1] OF M3AST_SM.Exp_value;
    baseType: M3AST_SM.TYPE_SPEC_UNSET;
    pos: CARDINAL := 0;
  BEGIN
    WHILE SeqM3AST_AS_EXP.Next(iter, exp) DO
      IF pos > LAST(values) OR exp = NIL THEN RETURN NIL END;
      WITH ev = values[pos] DO
        ev := EvalComponent(exp, mode);
        IF NOT (ev # NIL AND 
            M3COrdinal.Is(exp.sm_exp_type_spec, baseType) AND
            baseType # NIL AND ISTYPE(baseType, M3AST_AS.Integer_type)) THEN
          RETURN NIL;
        END;
      END;
      INC(pos);
    END;
    IF pos # M3CWordProcs.ArgCount(w) THEN RETURN NIL END;
    ChkVal(call, M3CBackEnd.WordOp(w, SUBARRAY(values, 0, pos), er));
    RETURN er;
  END WordCall;


PROCEDURE CanAppearInConst(defId: M3AST_AS.DEF_ID): BOOLEAN RAISES {}=
  BEGIN
    TYPECASE defId OF
    | M3AST_AS.Var_id, M3AST_AS.Exc_id, M3AST_AS.FORMAL_ID,
      M3AST_AS.For_id, M3AST_AS.Handler_id,
      M3AST_AS.Tcase_id, M3AST_AS.With_id =>
        RETURN FALSE;
    ELSE
      RETURN TRUE;
    END;
  END CanAppearInConst;


(* The main routine for evaluating expressions *)        

PROCEDURE Eval(
    e: M3AST_AS.EXP;
    mode: ModeSet)
    : M3AST_SM.Exp_value
    RAISES {}=
  VAR
    er: M3AST_SM.Exp_value := e.sm_exp_value;
    error := FALSE;
  BEGIN
    (* If already evaluated or type unknown (indicating previous error) we
     return immediately *)
    IF er # NIL THEN RETURN er END;
    IF e.sm_exp_type_spec = NIL THEN RETURN NIL END;

    TYPECASE e OF
    | M3AST_AS.NUMERIC_LITERAL(numeric_literal) =>
        VAR
          literal := numeric_literal.lx_litrep;
        CONST HexDigit = SET OF CHAR{'a'..'f', 'A'..'F', '0'..'9'};
        BEGIN
          IF LiteralLastChar(literal) IN HexDigit THEN
            ChkVal(e, M3CBackEnd.LiteralValue(e, er));
          ELSE
            M3Error.Report(e, "bad numeric literal");
          END;
        END;

    | M3AST_AS.Nil_literal =>
        ChkVal(e, M3CBackEnd.LiteralValue(e, er));

    | M3AST_AS.Text_literal(text_literal) =>
        VAR
          literal := text_literal.lx_litrep;
        BEGIN
          IF LiteralLastChar(literal) = '\"' THEN
            ChkVal(e, M3CBackEnd.LiteralValue(e, er));
          ELSE
            M3Error.Report(e, "bad text literal");
          END;
        END;

    | M3AST_AS.Char_literal(char_literal) =>
        VAR
          literal := char_literal.lx_litrep;
        BEGIN
          IF LiteralLastChar(literal) = '\'' THEN
            ChkVal(e, M3CBackEnd.LiteralValue(e, er));
          ELSE
            M3Error.Report(e, "bad character literal");
          END;
        END;
        
    | M3AST_AS.Exp_used_id(exp_used_id) =>
        (* has to be a Const_id  *)
        TYPECASE exp_used_id.vUSED_ID.sm_def OF
        | NULL =>
        | M3AST_AS.Const_id(constId) =>
            er := GetValueForUsedId(e, constId.vCCV_ID);
        | M3AST_AS.Proc_id =>
            ChkVal(e, M3CBackEnd.LiteralValue(e, er));
        | M3AST_AS.DEF_ID(defId) =>
            IF Mode.WalkConst IN mode AND NOT CanAppearInConst(defId) THEN
              error := TRUE;
            END;
        ELSE
          (* error reported elsewhere *)
        END; (* case *)

    | M3AST_AS.Select(b) =>
          (* Validity of selection already established. *)
            TYPECASE b.as_id.vUSED_ID.sm_def OF
            | NULL =>
            | M3AST_AS.Const_id(constId) =>
                er := GetValueForUsedId(b.as_id, constId.vCCV_ID);
            | M3AST_AS.Enum_id(enumId) =>
                er := GetValueForUsedId(b.as_id, enumId.vCCV_ID);
            | M3AST_AS.Field_id(fieldId) =>
                IF EvalComponent(b.as_exp, mode) # NIL THEN
                  er := Selection(b.as_exp, fieldId);
                END;
            | M3AST_AS.Proc_id =>
                TYPECASE b.as_exp OF
                | M3AST_AS.Exp_used_id(used_id) =>
                    WITH def = used_id.vUSED_ID.sm_def DO 
                      IF ISTYPE(def, M3AST_AS.Interface_id) OR
                         ISTYPE(def, M3AST_AS.Interface_AS_id) THEN
                        ChkVal(e, M3CBackEnd.LiteralValue(e, er));
                      END;
                    END;
                ELSE
                END;
            | M3AST_AS.DEF_ID(defId) =>
                IF Mode.WalkConst IN mode AND NOT CanAppearInConst(defId) THEN
                  error := TRUE;
                END;
            END; (* case *)

    | M3AST_AS.BINARY(b) =>
            VAR
              e1 := b.as_exp1;
              e2 := b.as_exp2;
              ev1 := EvalComponent(e1, mode);
              ev2 := EvalComponent(e2, mode);
            BEGIN
              IF ev1 # NIL AND ev2 # NIL THEN
                (* for all but IN, value types must be identical *)
                IF ISTYPE(b, M3AST_AS.In) THEN
                  (* As 'ev2' is not NIL we assume its 'sm_exp_type_spec' must
                   be set up *)
                  VAR
                    low, high: M3AST_SM.Exp_value;
                  BEGIN
                    IF ISTYPE(e2.sm_exp_type_spec, M3AST_AS.Set_type) AND
                       M3CBackEnd.IsOrdinal(ev1) AND
                       GetBounds(e2.sm_exp_type_spec, low, high) =
                          M3CBackEnd.NumStatus.Valid THEN
                      IF M3CBackEnd.Compare(low, ev1) <= 0 AND
                          M3CBackEnd.Compare(ev2, high) <= 0 THEN
                        ChkVal(e, M3CBackEnd.InOp(ev1, ev2, er));
                      ELSE
                        er := NewBoolean(FALSE);
                      END;
                    END; (* if *)
                  END;
                ELSIF ISTYPE(b, M3AST_AS.Eq) OR ISTYPE(b, M3AST_AS.Ne) THEN
                  er := NewBoolean((ISTYPE(b, M3AST_AS.Eq)) = Equal(e1, e2));
                ELSE
                  ChkVal(e, M3CBackEnd.BinaryOp(b, ev1, ev2, er));
                END; (* if *)
              END; (* if *)
            END; (* with *)

    | M3AST_AS.UNARY(unary) =>
        VAR
          ev := EvalComponent(unary.as_exp, mode);
        BEGIN
          IF ev # NIL AND NOT ISTYPE(unary, M3AST_AS.Deref) THEN
            ChkVal(e, M3CBackEnd.UnaryOp(unary, ev, er));
          END; (* if *)
        END;

    | M3AST_AS.Constructor(constructor) =>
        (* nothing doing if constructor badly typed. *)
        VAR
          low, high: M3AST_SM.Exp_value := NIL;
        BEGIN
          TYPECASE M3CTypesMisc.CheckedUnpack(e.sm_exp_type_spec) OF
          | NULL =>
              RETURN NIL;
          | M3AST_AS.Record_type, M3AST_AS.Array_type =>
              (* ok *)
          | M3AST_AS.Set_type(st) =>
              VAR
                baseType: M3AST_SM.TYPE_SPEC_UNSET;
              BEGIN
                M3CTypesMisc.GetTYPE_SPECFromM3TYPE(st.as_type, baseType);
                IF NOT IsOrdinal(baseType) THEN RETURN NIL END;
                IF GetBounds(baseType, low, high) #
                    M3CBackEnd.NumStatus.Valid THEN
                  RETURN NIL;
                END;
              END;
          ELSE
            RETURN NIL;
          END;
          (* sadly there is no guarantee that the actuals have had
             their values computed yet, since they can arise from
             default values of types not yet visited. So we evaluate
             recursively.
          *)
          VAR
            iterRangeExps :=
                SeqM3AST_AS_RANGE_EXP.NewIter(constructor.sm_actual_s);
            rangeExp: M3AST_AS.RANGE_EXP;
            rmode := mode + ModeSet{Mode.Recursive};
          BEGIN
            WHILE SeqM3AST_AS_RANGE_EXP.Next(iterRangeExps, rangeExp) DO
              TYPECASE rangeExp OF  <*NOWARN*>
              | NULL =>
                  RETURN NIL;
              | M3AST_AS.Range(range) =>
                  VAR
                    e1 := EvalComponent(range.as_exp1, rmode);
                    e2 := EvalComponent(range.as_exp2, rmode);
                  BEGIN
                    IF e1 = NIL OR e2 = NIL OR
                        (low # NIL AND
                            (NotInBounds(e1, low, high) OR
                            NotInBounds(e2, low, high))) THEN
                      RETURN NIL;
                    END;
                  END;
              | M3AST_AS.Range_EXP(rExp) =>
                  VAR
                    e1 := EvalComponent(rExp.as_exp, rmode);
                  BEGIN
                    IF e1 = NIL OR
                        (low # NIL AND NotInBounds(e1, low, high)) THEN
                      RETURN NIL;
                    END;
                  END;
               END;
            END; (* while *)
            M3CBitSize.Set(e.sm_exp_type_spec);
            (* size=0 permits open array constructors *)
            IF e.sm_exp_type_spec.sm_bitsize >= 0 THEN
              ChkVal(e, M3CBackEnd.ConstructorValue(e, er));
            END;
          END;
        END;
        
    | M3AST_AS.Call(call) =>
        VAR
          pf: M3CStdProcs.T;
          w: M3CWordProcs.T;
        BEGIN
          IF M3CStdProcs.IsStandardCall(call, pf) THEN
            IF pf IN M3CStdProcs.AllowedInConstantExpressions THEN
              er := StandardCall(call, pf, mode);
            ELSE
              IF Mode.WalkConst IN mode THEN error := TRUE END;
            END;
          ELSIF M3CWordProcs.IsWordCall(call, w) THEN
            er := WordCall(call, w, mode);
          ELSE
            IF Mode.WalkConst IN mode THEN error := TRUE END;
          END;
        END;

    | M3AST_AS.Index(index) =>
          IF EvalComponent(index.as_array, mode) # NIL THEN
            (* Note 'e1' non NIL implies that type of 'as_array' is non NIL *)
            er := Index(index, mode);
          END;

    ELSE
      (* Bad expression *)
    END; (* case *)
    IF error THEN
      NotConstant(e);
    ELSE
      e.sm_exp_value := er;
    END;
    RETURN er;
  END Eval;


REVEAL
  Closure =
    ASTWalk.Closure BRANDED OBJECT
      interface: BOOLEAN;
      node: AST.NODE;
      exp: M3AST_AS.EXP := NIL;
    OVERRIDES
      callback := Set;
    END;


PROCEDURE NewClosure(
    interface: BOOLEAN;
    node: AST.NODE := NIL)
    : Closure
    RAISES {}=
  BEGIN
    RETURN NEW(Closure, interface := interface, node := node);
  END NewClosure;


PROCEDURE Set(
    cl: Closure;
    an: AST.NODE;
    vm: ASTWalk.VisitMode)
    RAISES {}=
  BEGIN
    IF vm = ASTWalk.VisitMode.Entry THEN
      IF cl.node = NIL THEN
        TYPECASE an OF
        | M3AST_AS.Var_decl =>
            IF cl.interface THEN cl.node := an END;
            RETURN;
        | M3AST_AS.TYPE_SPEC, M3AST_AS.Const_decl, M3AST_AS.Case =>
            cl.node := an;
            RETURN;
        ELSE
        END;
      ELSE
        TYPECASE an OF
        | M3AST_AS.STM =>
            cl.node := NIL;
            RETURN;
        ELSE
        END;
      END;
      TYPECASE an OF
      | M3AST_AS.EXP(exp) =>
          IF cl.node # NIL AND cl.exp = NIL THEN cl.exp := exp END;
          TYPECASE exp OF
          | M3AST_AS.Call(call) =>
              VAR
                pf: M3CStdProcs.T;
              BEGIN
                IF M3CStdProcs.IsStandardCall(call, pf) AND
                    pf IN SpecialCall AND
                    NOT SeqM3AST_AS_Actual.Empty(call.as_param_s) THEN
                  call.sm_exp_value := CheckSpecialCall(call, pf, cl);
                END;
              END;
          ELSE
          END;
      ELSE
      END;
    ELSE
      IF an = cl.node THEN
        cl.node := NIL;
        TYPECASE an OF
        | M3AST_AS.Const_decl(constDecl) =>
            constDecl.as_id.vCCV_ID.sm_exp_value :=
                constDecl.as_exp.sm_exp_value;
        ELSE
        END;
        RETURN;
      END;
      TYPECASE an OF
      | M3AST_AS.EXP(exp) =>
          VAR
            mode: ModeSet;
          BEGIN
            IF cl.exp # NIL THEN
              mode := ModeSet{Mode.WalkConst};
            ELSE
              mode := ModeSet{Mode.Walk};
            END;
            EVAL Eval(exp, mode);
            IF exp = cl.exp THEN
              cl.exp := NIL;
            END;
          END;
      ELSE
      END;
    END;
  END Set;


BEGIN
END M3CExpValue.
