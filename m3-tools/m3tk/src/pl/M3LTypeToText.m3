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

MODULE M3LTypeToText EXPORTS M3LTypeToText, M3LTypeSpecToText;

IMPORT Wr, Thread, Text;
IMPORT M3AST_AS, M3AST_SM, M3ASTNext;

IMPORT M3AST_AS_F, M3AST_TM_F, M3AST_SM_F;

IMPORT SeqM3AST_AS_RANGE_EXP, SeqM3AST_AS_Enum_id, SeqM3AST_AS_M3TYPE,
    SeqM3AST_AS_Fields, SeqM3AST_AS_Formal_param, SeqM3AST_AS_Qual_used_id,
    SeqM3AST_AS_Method, SeqM3AST_AS_Override;

IMPORT M3CId;
IMPORT M3CTypesMisc, M3CStdTypes, M3CBackEnd, M3CExpValue;
IMPORT M3CBackEnd_C; (* for representation of brands *)

VAR
  char_g, boolean_g, cardinal_g, text_g: INTEGER;

PROCEDURE Initialize() RAISES {}=
  BEGIN
    (* these guys can't be initialized as global variables because 
       M3CStdTypes.Cardinal returns NIL until after M3 has been parsed
       and that doesn't occur until much later. 
    *)
    char_g := M3CStdTypes.Char().tmp_type_code;
    boolean_g := M3CStdTypes.Boolean().tmp_type_code;
    cardinal_g := M3CStdTypes.Cardinal().tmp_type_code;
    text_g := M3CStdTypes.Text().tmp_type_code;
  END Initialize;

PROCEDURE SmallNumberDigits(s: Wr.T; n: CARDINAL)
    RAISES {Wr.Failure, Thread.Alerted}=
  VAR  
    base: CARDINAL;
    baseCh: CHAR;
  BEGIN
    IF n >= FirstBigNumber THEN
      SmallNumberDigits(s, n DIV FirstBigNumber);
      n := n MOD FirstBigNumber;
    END;
    CASE n OF <*NOWARN*>
    | FirstDigitValue..LastDigitValue =>
        base := FirstDigitValue;
        baseCh := FirstDigitCh;
    | FirstLowerValue..LastLowerValue =>
        base := FirstLowerValue;
        baseCh := FirstLowerCh;
    | FirstUpperValue..LastUpperValue =>
        base := FirstUpperValue;
        baseCh := FirstUpperCh;
    END; (* case *)
    Wr.PutChar(s, VAL(ORD(baseCh) + n - base, CHAR));
  END SmallNumberDigits;


PROCEDURE SmallNumber(s: Wr.T; n: CARDINAL)
    RAISES {Wr.Failure, Thread.Alerted}=
  BEGIN
    IF n >= FirstBigNumber THEN
      Wr.PutChar(s, BigNumberBraCh);
      SmallNumberDigits(s, n);
      Wr.PutChar(s, BigNumberKetCh);
    ELSE
      SmallNumberDigits(s, n);
    END;
  END SmallNumber;


<*INLINE*> PROCEDURE TypeIndexDigits(
    s: Wr.T;
    n: CARDINAL)
    RAISES {Wr.Failure, Thread.Alerted}=
  VAR
    div := n DIV TypeIndexBase;
    mod := n MOD TypeIndexBase;
  BEGIN
    IF div # 0 THEN TypeIndexDigits(s, div) END;
    Wr.PutChar(s, VAL(mod + ORD(TypeIndexFirstDigitCh), CHAR));
  END TypeIndexDigits;


PROCEDURE TypeIndex(s: Wr.T; n: CARDINAL) RAISES {Wr.Failure, Thread.Alerted}=
  CONST
    MaxOneDigit = TypeIndexBase - 1;
    MaxTwoDigit = TypeIndexBase * TypeIndexBase - 1;
    MaxThreeDigit = TypeIndexBase * TypeIndexBase * TypeIndexBase - 1;
  VAR
    ch: CHAR;
  BEGIN
    IF n <= MaxOneDigit THEN
      ch := TypeIndexOneCh;
    ELSIF n <= MaxTwoDigit THEN
      ch := TypeIndexTwoCh;
    ELSIF n <= MaxThreeDigit THEN
      ch := TypeIndexThreeCh;
    ELSE
      ch := TypeIndexManyCh;
    END;
    Wr.PutChar(s, ch);
    TypeIndexDigits(s, n);
    IF ch = TypeIndexManyCh THEN Wr.PutChar(s, ch) END;
  END TypeIndex;


PROCEDURE Txt(s: Wr.T; t: TEXT) RAISES {Wr.Failure, Thread.Alerted}=
  BEGIN
    SmallNumber(s, Text.Length(t));
    Wr.PutText(s, t);
  END Txt;


PROCEDURE Id(s: Wr.T; id: M3CId.T) RAISES {Wr.Failure, Thread.Alerted}=
  BEGIN
    Txt(s, M3CId.ToText(id));
  END Id;


PROCEDURE Exp(s: Wr.T; exp: M3AST_AS.EXP) RAISES {Wr.Failure, Thread.Alerted}=
  BEGIN
    TYPECASE exp.sm_exp_type_spec OF
    | M3AST_AS.RefAny_type, M3AST_AS.Root_type,
      M3AST_AS.Address_type,
      M3AST_AS.Ref_type, M3AST_AS.Object_type,
      M3AST_AS.Opaque_type, M3AST_AS.Null_type,
      M3AST_AS.Procedure_type =>
        (* excepts for texts can only be NIL *)
        IF exp.sm_exp_type_spec.tmp_type_code = text_g THEN
          Txt(s, M3CBackEnd.ExpValueToText(
              exp.sm_exp_value));
        ELSE
          SmallNumber(s, 0);
        END;
    | M3AST_AS.Record_type, M3AST_AS.Array_type =>
        VAR
          constructor := M3CBackEnd.ConstructorOriginal(exp.sm_exp_value);
          iter := SeqM3AST_AS_RANGE_EXP.NewIter(constructor.sm_actual_s);
          r: M3AST_AS.RANGE_EXP;
        BEGIN
          WHILE SeqM3AST_AS_RANGE_EXP.Next(iter, r) DO
            Exp(s, NARROW(r, M3AST_AS.Range_EXP).as_exp);
          END; (* while *)
          IF constructor.as_propagate # NIL THEN
            Wr.PutChar(s, PropagateCh);
          END;
          Wr.PutChar(s, EndSeqCh);
        END;
    ELSE
      VAR
        val: INTEGER;
      BEGIN
        IF M3CExpValue.Ordinal(exp, val) = M3CBackEnd.NumStatus.Valid AND
           val = 0 THEN
          SmallNumber(s, 0);
        ELSE
          Txt(s, M3CBackEnd.ExpValueToText(exp.sm_exp_value));
        END;
      END;
    END;
  END Exp;


PROCEDURE QualId(s: Wr.T; q: M3AST_AS.Qual_used_id) RAISES {Wr.Failure, Thread.Alerted}=
  VAR
    defId := q.as_id.sm_def;
  BEGIN
    Id(s, defId.tmp_unit_id.lx_symrep);
    Id(s, defId.lx_symrep);
  END QualId;

PROCEDURE Enumeration(
    s: Wr.T;
    e: M3AST_AS.Enumeration_type)
    RAISES {Wr.Failure, Thread.Alerted}=
  BEGIN
    IF e.tmp_type_code = char_g THEN
      Wr.PutChar(s, CharCh);
    ELSIF e.tmp_type_code = boolean_g THEN
      Wr.PutChar(s, BooleanCh);
    ELSE
      VAR
        i := SeqM3AST_AS_Enum_id.NewIter(e.as_id_s);
        id: M3AST_AS.Enum_id;
      BEGIN
        Wr.PutChar(s, EnumerationCh);
        WHILE SeqM3AST_AS_Enum_id.Next(i, id) DO
          Id(s, id.lx_symrep)
        END;
        Wr.PutChar(s, EndSeqCh);
      END;
    END;
  END Enumeration;


PROCEDURE Subrange(
    s: Wr.T;
    sub: M3AST_AS.Subrange_type)
    RAISES {Wr.Failure, Thread.Alerted}=
  BEGIN
    IF sub.tmp_type_code = cardinal_g THEN
      Wr.PutChar(s, CardinalCh);
    ELSE
      Wr.PutChar(s, SubrangeCh);
      ComponentType(s, sub.sm_base_type_spec);
      Exp(s, sub.as_range.as_exp1);
      Exp(s, sub.as_range.as_exp2);
    END;
  END Subrange;


PROCEDURE Array(s: Wr.T; a: M3AST_AS.Array_type) RAISES {Wr.Failure, Thread.Alerted}=
  VAR
    iter := SeqM3AST_AS_M3TYPE.NewIter(a.as_indextype_s);
    m3Type: M3AST_AS.M3TYPE;
  BEGIN
    Wr.PutChar(s, ArrayCh);
    IF SeqM3AST_AS_M3TYPE.Next(iter, m3Type) THEN
      ComponentType(s, m3Type);
    ELSE
      Wr.PutChar(s, VoidCh);
    END;
    ComponentType(s, a.sm_norm_type.as_elementtype);
  END Array;


PROCEDURE Fields(
    s: Wr.T;
    fields: SeqM3AST_AS_Fields.T)
    RAISES {Wr.Failure, Thread.Alerted}=
  VAR
    iter := M3ASTNext.NewIterField(fields);
    id: M3AST_AS.Field_id;
  BEGIN
    WHILE M3ASTNext.Field(iter, id) DO
      Id(s, id.lx_symrep);
      ComponentType(s, id.sm_type_spec);
      IF id.vINIT_ID.sm_init_exp # NIL THEN
        Wr.PutChar(s, DefaultCh);
        Exp(s, id.vINIT_ID.sm_init_exp);
      END; (* if *)
    END; (* while *)
    Wr.PutChar(s, EndSeqCh);
  END Fields;


PROCEDURE Record(
    s: Wr.T;
    r: M3AST_AS.Record_type)
    RAISES {Wr.Failure, Thread.Alerted}=
  BEGIN
    Wr.PutChar(s, RecordCh);
    Fields(s, r.as_fields_s);
  END Record;


PROCEDURE Packed(
    s: Wr.T;
    b: M3AST_AS.Packed_type)
    RAISES {Wr.Failure, Thread.Alerted}=
  BEGIN
    Wr.PutChar(s, BitsCh);
    Exp(s, b.as_exp);
    ComponentType(s, b.as_type);
  END Packed;


PROCEDURE Set(s: Wr.T; set: M3AST_AS.Set_type) RAISES {Wr.Failure, Thread.Alerted}=
  BEGIN
    Wr.PutChar(s, SetCh);
    ComponentType(s, set.as_type);
  END Set;


PROCEDURE Brand(s: Wr.T; b: M3AST_AS.Brand_NULL) RAISES {Wr.Failure, Thread.Alerted}=
  BEGIN
    IF b # NIL THEN
      IF b.as_exp = NIL THEN
        Wr.PutChar(s, CompilerBrandCh);
      ELSE
        Wr.PutChar(s, UserBrandCh);
      END; (* if *)
      Txt(s, NARROW(b.sm_brand, M3CBackEnd_C.Text_value).sm_value);
    END; (* if *)
  END Brand;


PROCEDURE Ref(s: Wr.T; r: M3AST_AS.Ref_type) RAISES {Wr.Failure, Thread.Alerted}=
  BEGIN
    IF r.as_trace_mode = NIL THEN
      Wr.PutChar(s, RefCh);
    ELSE
      Wr.PutChar(s, UntracedRefCh);
    END; (* if *)
    Brand(s, r.as_brand);
    ComponentType(s, r.as_type);
  END Ref;


PROCEDURE Formals(
    s: Wr.T;
    formals: SeqM3AST_AS_Formal_param.T)
    RAISES {Wr.Failure, Thread.Alerted}=
  VAR
    iter := M3ASTNext.NewIterFormal(formals);
    f: M3AST_AS.Formal_param;
    id: M3AST_AS.FORMAL_ID;
  BEGIN
    WHILE M3ASTNext.Formal(iter, f, id) DO
      TYPECASE id OF
      | M3AST_AS.F_Var_id => Wr.PutChar(s, VarCh);
      | M3AST_AS.F_Readonly_id => Wr.PutChar(s, ReadonlyCh);
      ELSE
      END;
      Id(s, id.lx_symrep);
      ComponentType(s, id.sm_type_spec);
      IF f.as_default # NIL THEN
        Wr.PutChar(s, DefaultCh);
        Exp(s, f.as_default);
      END; (* if *)
    END; (* while *)
    Wr.PutChar(s, EndSeqCh);
  END Formals;


PROCEDURE Procedure(
    s: Wr.T;
    p: M3AST_AS.Procedure_type)
    RAISES {Wr.Failure, Thread.Alerted}=
  BEGIN
    Wr.PutChar(s, ProcedureCh);
    WITH smDefId = p.sm_def_id DO
      IF (smDefId # NIL) AND (ISTYPE(smDefId, M3AST_AS.Method_id)) THEN
        WITH methodId = NARROW(smDefId, M3AST_AS.Method_id) DO
          Wr.PutChar(s, MethodCh);
          TypeIndex(s, methodId.vRECOBJ_ID.sm_enc_type_spec.tmp_type_code);
        END;
      END;
    END;

    Formals(s, p.as_formal_param_s);
    IF p.as_result_type = NIL THEN
      Wr.PutChar(s, VoidCh);
    ELSE
      ComponentType(s, p.as_result_type);
    END; (* if *)
    VAR
      r := p.as_raises;
    BEGIN
      TYPECASE r OF <*NOWARN*>
      | NULL => 
          Wr.PutChar(s, EndSeqCh); (* RAISES {} *)
      | M3AST_AS.Raisees_any =>
          Wr.PutChar(s, RaisesAnyCh);
      | M3AST_AS.Raisees_some(r) => 
          VAR
            i := SeqM3AST_AS_Qual_used_id.NewIter(r.as_raisees_s);
            q: M3AST_AS.Qual_used_id;
          BEGIN
            WHILE SeqM3AST_AS_Qual_used_id.Next(i, q) DO
              QualId(s, q);
            END; (* while *)
          END;
          Wr.PutChar(s, EndSeqCh);
      END;
    END;
  END Procedure;


PROCEDURE Object(
    s: Wr.T;
    o: M3AST_AS.Object_type)
    RAISES {Wr.Failure, Thread.Alerted}=
  BEGIN
    Wr.PutChar(s, ObjectCh);
    Brand(s, o.as_brand);
    IF o.as_ancestor = NIL THEN
      Wr.PutChar(s, RootCh);
    ELSE
      ComponentType(s, o.as_ancestor);
    END; (* if *)
    Fields(s, o.as_fields_s);
    VAR
      i := SeqM3AST_AS_Method.NewIter(o.as_method_s);
      m: M3AST_AS.Method;
    BEGIN
      WHILE SeqM3AST_AS_Method.Next(i, m) DO
        Id(s, m.as_id.lx_symrep);
        ComponentType(s, m.as_type);
        IF m.as_default # NIL THEN
          Wr.PutChar(s, DefaultCh);
          Exp(s, m.as_id.vINIT_ID.sm_init_exp);
        END; (* if *)
      END; (* while *)
    END;
    Wr.PutChar(s, EndSeqCh);
    VAR
      i := SeqM3AST_AS_Override.NewIter(o.as_override_s);
      m: M3AST_AS.Override;
    BEGIN
      WHILE SeqM3AST_AS_Override.Next(i, m) DO
        Id(s, m.as_id.lx_symrep);
        ComponentType(s, m.as_id.sm_type_spec);
        Wr.PutChar(s, DefaultCh);
        Exp(s, m.as_id.vINIT_ID.sm_init_exp);
      END; (* while *)
    END;
    Wr.PutChar(s, EndSeqCh);
  END Object;


PROCEDURE Opaque(s: Wr.T; o: M3AST_AS.Opaque_type) RAISES {Wr.Failure, Thread.Alerted}=
  BEGIN
    Wr.PutChar(s, OpaqueCh);
    IF o.sm_concrete_type_spec = NIL THEN
      TypeIndex(s, o.tmp_type_code);
      Wr.PutChar(s, VoidCh);
    ELSE
      TypeSpec(s, o.sm_concrete_type_spec);
    END;
  END Opaque;


PROCEDURE ComponentType(s: Wr.T; t: M3AST_AS.M3TYPE) RAISES {Wr.Failure, Thread.Alerted}=
  VAR
    ts: M3AST_SM.TYPE_SPEC_UNSET;
    tc: INTEGER;
  BEGIN
    M3CTypesMisc.GetTYPE_SPECFromM3TYPE(t, ts);
    TYPECASE ts OF
    | M3AST_AS.Integer_type, M3AST_AS.FLOAT_TYPE,
      M3AST_AS.RefAny_type,
      M3AST_AS.Address_type, M3AST_AS.Null_type,
      M3AST_AS.Root_type =>
        TypeSpec(s, ts);
    ELSE
      tc := ts.tmp_type_code;
      IF tc = boolean_g THEN
        Wr.PutChar(s, BooleanCh);
      ELSIF tc = char_g THEN
        Wr.PutChar(s, CharCh);
      ELSIF tc = cardinal_g THEN
        Wr.PutChar(s, CardinalCh);
      ELSE 
        TypeIndex(s, tc);
      END;
    END;
  END ComponentType;


PROCEDURE TypeSpec(
    s: Wr.T;
    t: M3AST_AS.TYPE_SPEC)
    RAISES {Wr.Failure, Thread.Alerted}=
  BEGIN
    TYPECASE t OF <*NOWARN*>
    | M3AST_AS.Integer_type =>
        Wr.PutChar(s, IntegerCh);
    | M3AST_AS.Real_type =>
        Wr.PutChar(s, RealCh);
    | M3AST_AS.LongReal_type =>
        Wr.PutChar(s, LongRealCh);
    | M3AST_AS.Extended_type =>
        Wr.PutChar(s, ExtendedCh);
    | M3AST_AS.RefAny_type =>
        Wr.PutChar(s, RefAnyCh);
    | M3AST_AS.Address_type =>
        Wr.PutChar(s, AddressCh);
    | M3AST_AS.Null_type =>
        Wr.PutChar(s, NullCh);
    | M3AST_AS.Root_type(root_type) =>
        IF root_type.as_trace_mode = NIL THEN
          Wr.PutChar(s, RootCh);
        ELSE
          Wr.PutChar(s, UntracedRootCh);
        END;
    | M3AST_AS.Enumeration_type =>
        Enumeration(s, t);
    | M3AST_AS.Subrange_type =>
        Subrange(s, t);
    | M3AST_AS.Array_type =>
        Array(s, t);
    | M3AST_AS.Record_type =>
        Record(s, t);
    | M3AST_AS.Packed_type =>
        Packed(s, t);
    | M3AST_AS.Set_type =>
        Set(s, t);
    | M3AST_AS.Ref_type =>
        Ref(s, t);
    | M3AST_AS.Procedure_type =>
        Procedure(s, t);
    | M3AST_AS.Object_type =>
        Object(s, t);
    | M3AST_AS.Opaque_type =>
        Opaque(s, t);
    END; (* case *)
  END TypeSpec;


BEGIN
END M3LTypeToText.
