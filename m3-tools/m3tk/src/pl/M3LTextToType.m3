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

MODULE M3LTextToType;

IMPORT Fmt, Text;

IMPORT M3AST_AS;

IMPORT M3AST_LX_F, M3AST_AS_F, M3AST_SM_F, M3AST_TM_F;

IMPORT SeqM3AST_AS_RANGE_EXP, SeqM3AST_AS_M3TYPE, SeqM3AST_AS_Fields,
    SeqM3AST_AS_Field_id, SeqM3AST_AS_Formal_param, SeqM3AST_AS_FORMAL_ID,
    SeqM3AST_AS_Enum_id, SeqM3AST_AS_Qual_used_id, SeqM3AST_AS_Method,
    SeqM3AST_AS_Override;
IMPORT M3Assert, M3ASTNext;

IMPORT M3CId, M3CLiteral;
IMPORT M3CStdTypes, M3CBackEnd, M3CBackEnd_C;
IMPORT M3LTypeToText;

TYPE
  TextIndex = CARDINAL;

PROCEDURE Default(text: Text.T; VAR i: TextIndex): BOOLEAN RAISES {}=
  BEGIN
    RETURN FindChar(text, i, M3LTypeToText.DefaultCh);
  END Default;

PROCEDURE EndSeq(text: Text.T; VAR i: TextIndex): BOOLEAN RAISES {}=
  BEGIN
    RETURN FindChar(text, i, M3LTypeToText.EndSeqCh);
  END EndSeq;

PROCEDURE Void(text: Text.T; VAR i: TextIndex): BOOLEAN RAISES {}=
  BEGIN
    RETURN FindChar(text, i, M3LTypeToText.VoidCh);
  END Void;

PROCEDURE FindChar(text: Text.T; VAR i: TextIndex; ch: CHAR): BOOLEAN RAISES {}=
  BEGIN
    IF Text.GetChar(text, i) = ch THEN
      INC(i);
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END FindChar;

PROCEDURE NumberDigit(ch: CHAR): CARDINAL RAISES {}=
  BEGIN
    CASE ch OF <*NOWARN*>
    | M3LTypeToText.FirstDigitCh..M3LTypeToText.LastDigitCh =>
        RETURN ORD(ch) - ORD(M3LTypeToText.FirstDigitCh);
    | M3LTypeToText.FirstLowerCh..M3LTypeToText.LastLowerCh =>
        RETURN ORD(ch) - ORD(M3LTypeToText.FirstLowerCh) + 
               M3LTypeToText.FirstLowerValue;
    | M3LTypeToText.FirstUpperCh..M3LTypeToText.LastUpperCh =>
        RETURN ORD(ch) - ORD(M3LTypeToText.FirstUpperCh) + 
               M3LTypeToText.FirstUpperValue;
    END;
  END NumberDigit;

PROCEDURE Number(text: Text.T; VAR chIndex: TextIndex): CARDINAL RAISES {}=
  VAR 
    ch := Text.GetChar(text, chIndex);
    result := 0;
  BEGIN
    INC(chIndex);
    IF ch = M3LTypeToText.BigNumberBraCh THEN
      ch := Text.GetChar(text, chIndex);
      WHILE ch # M3LTypeToText.BigNumberKetCh DO
        INC(chIndex);
        result := result * M3LTypeToText.FirstBigNumber +
         NumberDigit(ch);
      END;
      INC(chIndex);
      RETURN(result);
    ELSE
      RETURN(NumberDigit(ch));
    END;
  END Number;

PROCEDURE TypeNumberDigits(text: Text.T; 
                           VAR chIndex: TextIndex;
                           digits: CARDINAL): CARDINAL RAISES {}=
  VAR  
    result := 0;
  BEGIN
    FOR i := 1 TO digits DO
      result := result * M3LTypeToText.TypeIndexBase + 
       ORD(Text.GetChar(text, chIndex)) - 
       ORD(M3LTypeToText.TypeIndexFirstDigitCh);
      INC(chIndex);
    END;
    RETURN(result);
  END TypeNumberDigits;

PROCEDURE TypeNumber(text: Text.T; VAR chIndex: TextIndex): CARDINAL
  RAISES {}=
  VAR 
    ch := Text.GetChar(text, chIndex);
    i: CARDINAL;
  BEGIN
    INC(chIndex);
    CASE ch OF <*NOWARN*>
    | M3LTypeToText.TypeIndexOneCh =>
        RETURN(TypeNumberDigits(text, chIndex, 1));
    | M3LTypeToText.TypeIndexTwoCh =>
        RETURN(TypeNumberDigits(text, chIndex, 2));
    | M3LTypeToText.TypeIndexThreeCh =>
        RETURN(TypeNumberDigits(text, chIndex, 3));
    | M3LTypeToText.TypeIndexManyCh =>
        (* search for end of number character *)
        i := 4; (* chIndex has already be incremented once *)
        WHILE Text.GetChar(text, chIndex + i) # 
              M3LTypeToText.TypeIndexManyCh DO
          INC(i);
        END;
        i := TypeNumberDigits(text, chIndex, i);
        INC(chIndex);
        RETURN i;
    END;
  END TypeNumber;

PROCEDURE TypeSpec(type: M3AST_AS.M3TYPE): M3AST_AS.TYPE_SPEC RAISES {}=
  BEGIN
    TYPECASE type OF
    | M3AST_AS.Named_type(named) => RETURN named.sm_type_spec;
    ELSE RETURN type;
    END;
  END TypeSpec;

PROCEDURE TypeIndex(t: T;
                    text: Text.T;
                    VAR chIndex: TextIndex): M3AST_AS.Named_type
  RAISES {}=
  VAR tempForBE: CARDINAL; (* extra range check causes unwanted side effects *)
  BEGIN
    tempForBE := TypeNumber(text, chIndex);
    RETURN t[tempForBE].named;
  END TypeIndex;


PROCEDURE Txt(text: Text.T; VAR chIndex: TextIndex): Text.T RAISES {}=
  VAR
    length: CARDINAL;
    txt: Text.T;
  BEGIN
    length := Number(text, chIndex);
    txt := Text.Sub(text, chIndex, length);
    INC(chIndex, length);
    RETURN txt;
  END Txt;


PROCEDURE Id(text: Text.T; VAR chIndex: TextIndex): M3CId.T RAISES {}=
  BEGIN
    RETURN M3CId.Enter(Txt(text, chIndex));
  END Id;

PROCEDURE Exp(t: T;
              expTS: M3AST_AS.TYPE_SPEC;
              text: Text.T;
              VAR chIndex: TextIndex): M3AST_AS.EXP
  RAISES {}=
  VAR
    exp: M3AST_AS.EXP;
    textExp: Text.T;
    oldChIndex: TextIndex := chIndex;
    value: CARDINAL;
  BEGIN
    ForceParseTypeSpec(t, expTS);
    TYPECASE expTS OF
    | M3AST_AS.RefAny_type, M3AST_AS.Root_type,
      M3AST_AS.Address_type,
      M3AST_AS.Ref_type, M3AST_AS.Object_type,
      M3AST_AS.Opaque_type, M3AST_AS.Null_type,
      M3AST_AS.Procedure_type =>
        value := Number(text, chIndex);
        IF value = 0 THEN
          (* can't be a text *)
          exp := NEW(M3AST_AS.Nil_literal).init();
          exp.sm_exp_value := M3CBackEnd.TextToExpValue("0");
          (* cheat to get type spec right *)
          expTS := M3CStdTypes.Null();
        ELSE
          chIndex := oldChIndex;
          (* excepts for texts can only be NIL, so must be a Text *)
          exp := NEW(M3AST_AS.Text_literal).init();
          textExp := Txt(text, chIndex);
          NARROW(exp, M3AST_AS.Text_literal).lx_litrep := 
            M3CLiteral.Enter(textExp);
          exp.sm_exp_value := M3CBackEnd.TextToExpValue(textExp);
          (* cheat to get type spec right in all cases: opaque, refany, etc. *)
          expTS := M3CStdTypes.Text(); 
        END;
    | M3AST_AS.Record_type(record) =>
        VAR
          constructor: M3AST_AS.Constructor :=
              NEW(M3AST_AS.Constructor).init();
          cons_value: M3CBackEnd_C.Array_or_record_constructor_value;
          rExp: M3AST_AS.Range_EXP;
          iterFields := M3ASTNext.NewIterField(record.as_fields_s);
          fid: M3AST_AS.Field_id;
        BEGIN
          ForceParseTypeSpec(t, record);
          exp := constructor;
          constructor.as_type := expTS;
          cons_value := NEW(M3CBackEnd_C.Array_or_record_constructor_value);
          cons_value.sm_constructor := constructor;
          exp.sm_exp_value := cons_value;
          WHILE NOT EndSeq(text, chIndex) DO
            rExp := NEW(M3AST_AS.Range_EXP).init();
            SeqM3AST_AS_RANGE_EXP.AddRear(constructor.sm_actual_s, rExp);
            M3Assert.Check(M3ASTNext.Field(iterFields, fid));
            rExp.as_exp := Exp(t, fid.sm_type_spec, text, chIndex);
          END;
        END;
    | M3AST_AS.Array_type(array) =>
        VAR
          constructor: M3AST_AS.Constructor :=
              NEW(M3AST_AS.Constructor).init();
          cons_value: M3CBackEnd_C.Array_or_record_constructor_value;
          rExp: M3AST_AS.Range_EXP;
          elemType := TypeSpec(array.sm_norm_type.as_elementtype);
        BEGIN
          ForceParseTypeSpec(t, array);
          exp := constructor;
          constructor.as_type := expTS;
          cons_value := NEW(M3CBackEnd_C.Array_or_record_constructor_value);
          cons_value.sm_constructor := constructor;
          exp.sm_exp_value := cons_value;
          WHILE NOT EndSeq(text, chIndex) DO
            IF FindChar(text, chIndex, M3LTypeToText.PropagateCh) THEN
              constructor.as_propagate := NEW(M3AST_AS.Propagate).init();
            ELSE
              rExp := NEW(M3AST_AS.Range_EXP).init();
              SeqM3AST_AS_RANGE_EXP.AddRear(constructor.sm_actual_s, rExp);
              rExp.as_exp := Exp(t, elemType, text, chIndex);
            END;
          END;
        END;
    ELSE
      exp := NEW(M3AST_AS.Integer_literal).init();
      value := Number(text, chIndex);
      IF value = 0 THEN
        (* can't be a text *)
        exp.sm_exp_value := M3CBackEnd.TextToExpValue("0");
        NARROW(exp, M3AST_AS.Integer_literal).lx_litrep :=
          M3CLiteral.Enter("0");
      ELSE
        chIndex := oldChIndex;
        textExp := Txt(text, chIndex);
        exp.sm_exp_value := M3CBackEnd.TextToExpValue(textExp);
        NARROW(exp, M3AST_AS.Integer_literal).lx_litrep :=
          M3CLiteral.Enter(textExp);
      END;
    END;
    exp.sm_exp_type_spec := expTS;
    RETURN exp;
  END Exp;

PROCEDURE QualId(text: Text.T;
                 VAR chIndex: TextIndex): M3AST_AS.Qual_used_id
  RAISES {}=
  VAR
    qUID: M3AST_AS.Qual_used_id := NEW(M3AST_AS.Qual_used_id).init();
    intfId: M3AST_AS.Interface_id := NEW(M3AST_AS.Interface_id).init();
    defId: M3AST_AS.Interface_id := NEW(M3AST_AS.Interface_id).init(); 
            (* random DEF_ID  will it matter? *)
  BEGIN
    qUID.as_intf_id := NEW(M3AST_AS.Used_interface_id).init();
    intfId.lx_symrep := Id(text, chIndex);
    qUID.as_intf_id.lx_symrep := intfId.lx_symrep;
    defId.tmp_unit_id := intfId;
    defId.lx_symrep := Id(text, chIndex);
    qUID.as_id := NEW(M3AST_AS.Used_def_id).init();
    qUID.as_id.lx_symrep := defId.lx_symrep;
    qUID.as_id.sm_def := defId;
    RETURN qUID;
  END QualId;

PROCEDURE Enumeration(e: M3AST_AS.Enumeration_type;
                      text: Text.T;
                      VAR chIndex: TextIndex)
    RAISES {}=
  VAR
    id: M3AST_AS.Enum_id;
  BEGIN
    WHILE NOT EndSeq(text, chIndex) DO
      id := NEW(M3AST_AS.Enum_id).init();
      SeqM3AST_AS_Enum_id.AddRear(e.as_id_s, id);
      id.lx_symrep := Id(text, chIndex);
    END;
  END Enumeration;

PROCEDURE Subrange(t: T;
                   sub: M3AST_AS.Subrange_type;
                   text: Text.T;
                   VAR chIndex: TextIndex)
    RAISES {}=
  BEGIN
    sub.sm_base_type_spec := TypeSpec(M3Type(t, text, chIndex));
    sub.as_range := NEW(M3AST_AS.Range).init();
    sub.as_range.as_exp1 := Exp(t, sub.sm_base_type_spec, text, chIndex);
    sub.as_range.as_exp2 := Exp(t, sub.sm_base_type_spec, text, chIndex);
  END Subrange;

PROCEDURE Array(t: T;
                a: M3AST_AS.Array_type;
                text: Text.T;
                VAR chIndex: TextIndex) RAISES {}=
  BEGIN
    a.sm_norm_type := NEW(M3AST_AS.Array_type).init();
    IF NOT Void(text, chIndex) THEN
      SeqM3AST_AS_M3TYPE.AddRear(a.as_indextype_s, 
                                  M3Type(t, text, chIndex));
      a.sm_norm_type.as_indextype_s := a.as_indextype_s;
    END;
    a.as_elementtype := M3Type(t, text, chIndex);
    a.sm_norm_type.as_elementtype := a.as_elementtype;
  END Array;

PROCEDURE Fields(t: T;
                 text: Text.T;
                 VAR chIndex: TextIndex): SeqM3AST_AS_Fields.T
    RAISES {}=
  VAR
    fs := SeqM3AST_AS_Fields.Null;
    f: M3AST_AS.Fields;
    id: M3AST_AS.Field_id;
  BEGIN
    WHILE NOT EndSeq(text, chIndex) DO
      f := NEW(M3AST_AS.Fields).init();
      SeqM3AST_AS_Fields.AddRear(fs, f);
      id := NEW(M3AST_AS.Field_id).init();
      SeqM3AST_AS_Field_id.AddRear(f.as_id_s, id);
      id.lx_symrep := Id(text, chIndex);
      WITH type = M3Type(t, text, chIndex) DO
        f.as_type := type;
        id.sm_type_spec := TypeSpec(type);
      END;

      IF Default(text, chIndex) THEN
        f.as_default := Exp(t, id.sm_type_spec, text, chIndex);
        id.vINIT_ID.sm_init_exp := f.as_default;
      END;
    END;
    RETURN fs;
  END Fields;

PROCEDURE Record(t: T;
                 r: M3AST_AS.Record_type;
                 text: Text.T;
                 VAR chIndex: TextIndex)
    RAISES {}=
  BEGIN
    r.as_fields_s := Fields(t, text, chIndex);
  END Record;

PROCEDURE Packed(t: T;
                 packed: M3AST_AS.Packed_type;
                 text: Text.T;
                 VAR chIndex: TextIndex)
    RAISES {}=
  BEGIN
    packed.as_exp := Exp(t, M3CStdTypes.Integer(), text, chIndex);
    packed.as_type := M3Type(t, text, chIndex);
  END Packed;

PROCEDURE Set(t: T;
              set: M3AST_AS.Set_type;
              text: Text.T;
              VAR chIndex: TextIndex) RAISES {}=
  BEGIN
    set.as_type := M3Type(t, text, chIndex);
  END Set;

PROCEDURE Brand(text: Text.T;
                VAR chIndex: TextIndex): M3AST_AS.Brand_NULL RAISES {}=
  VAR 
    ch := Text.GetChar(text, chIndex);
    b: M3AST_AS.Brand_NULL := NIL;
    text_value := NEW(M3CBackEnd_C.Text_value);
  BEGIN
    IF ch = M3LTypeToText.CompilerBrandCh THEN
      b := NEW(M3AST_AS.Brand).init();
      INC(chIndex);
    ELSIF ch = M3LTypeToText.UserBrandCh THEN
      b := NEW(M3AST_AS.Brand).init();
      b.as_exp := NEW(M3AST_AS.Text_literal).init();
      INC(chIndex);
    END;
    text_value.sm_value := Txt(text, chIndex);
    b.sm_brand := text_value;
    RETURN b;
  END Brand;

PROCEDURE Ref(t: T;
              r: M3AST_AS.Ref_type;
              text: Text.T;
              VAR chIndex: TextIndex;
              untraced: BOOLEAN) RAISES {}=
  BEGIN
    IF untraced THEN
      r.as_trace_mode := NEW(M3AST_AS.Untraced).init();
    END;
    r.as_brand := Brand(text, chIndex);
    r.as_type := M3Type(t, text, chIndex);
  END Ref;

PROCEDURE Formals(
    t: T;
    text: Text.T;
    VAR chIndex: TextIndex): SeqM3AST_AS_Formal_param.T
    RAISES {}=
  VAR
    fs := SeqM3AST_AS_Formal_param.Null;
    f: M3AST_AS.Formal_param;
    id: M3AST_AS.FORMAL_ID;
  BEGIN
    WHILE NOT EndSeq(text, chIndex) DO
      f := NEW(M3AST_AS.Formal_param).init();
      CASE Text.GetChar(text, chIndex) OF
      | M3LTypeToText.VarCh =>
          INC(chIndex);
          id := NEW(M3AST_AS.F_Var_id).init();
      | M3LTypeToText.ReadonlyCh =>
          INC(chIndex);
          id := NEW(M3AST_AS.F_Readonly_id).init();
      ELSE
        id := NEW(M3AST_AS.F_Value_id).init();
      END;
      id.lx_symrep := Id(text, chIndex);
      WITH type = M3Type(t, text, chIndex) DO
        f.as_formal_type := type;
        id.sm_type_spec := TypeSpec(type);
      END;
      IF Default(text, chIndex) THEN
        f.as_default := Exp(t, id.sm_type_spec, text, chIndex);
      END;
      SeqM3AST_AS_FORMAL_ID.AddRear(f.as_id_s, id);
      SeqM3AST_AS_Formal_param.AddRear(fs, f);
    END;
    RETURN(fs);
  END Formals;

PROCEDURE Procedure(
    t: T;
    p: M3AST_AS.Procedure_type;
    text: Text.T;
    VAR chIndex: TextIndex)
    RAISES {}=
  VAR methodId: M3AST_AS.Method_id;
  BEGIN
    IF FindChar(text, chIndex, M3LTypeToText.MethodCh) THEN
      methodId := NEW(M3AST_AS.Method_id).init();
      p.sm_def_id := methodId;
      methodId.vRECOBJ_ID.sm_enc_type_spec := 
       TypeIndex(t, text, chIndex).sm_type_spec;
    END;

    p.as_formal_param_s := Formals(t, text, chIndex);
    IF NOT Void(text, chIndex) THEN
      p.as_result_type := M3Type(t, text, chIndex);
    END; (* if *)

    IF FindChar(text, chIndex, M3LTypeToText.RaisesAnyCh) THEN
      p.as_raises := NEW(M3AST_AS.Raisees_any).init();
    ELSE
     VAR r: M3AST_AS.Raisees_some := NEW(M3AST_AS.Raisees_some).init();
     BEGIN
      p.as_raises := r;
      WHILE NOT EndSeq(text, chIndex) DO
        SeqM3AST_AS_Qual_used_id.AddRear(r.as_raisees_s,
                                          QualId(text, chIndex));
      END;
     END;
    END;
  END Procedure;

PROCEDURE Object(
    t: T;
    o: M3AST_AS.Object_type;
    text: Text.T;
    VAR chIndex: TextIndex)
    RAISES {}=
  BEGIN
    o.as_brand := Brand(text, chIndex);
    o.as_ancestor := M3Type(t, text, chIndex);
    o.as_fields_s := Fields(t, text, chIndex);

    VAR
      method: M3AST_AS.Method;
    BEGIN
      WHILE NOT EndSeq(text, chIndex) DO
        method := NEW(M3AST_AS.Method).init();
        SeqM3AST_AS_Method.AddRear(o.as_method_s, method);
        method.as_id := NEW(M3AST_AS.Method_id).init();
        method.as_id.lx_symrep := Id(text, chIndex);
        WITH type = M3Type(t, text, chIndex) DO
          method.as_type := type;
          method.as_id.sm_type_spec := TypeSpec(type);
        END;
        IF Default(text, chIndex) THEN
          method.as_default := Exp(t, method.as_id.sm_type_spec, text, chIndex);
          method.as_id.vINIT_ID.sm_init_exp := method.as_default;
        END;
      END; (* while *)
    END; (* begin *)

    VAR
      override: M3AST_AS.Override;
    BEGIN
      WHILE NOT EndSeq(text, chIndex) DO
        override := NEW(M3AST_AS.Override).init();
        SeqM3AST_AS_Override.AddRear(o.as_override_s, override);
        override.as_id := NEW(M3AST_AS.Method_id).init();
        override.as_id.lx_symrep := Id(text, chIndex);
        override.as_id.sm_type_spec := TypeSpec(M3Type(t, text, chIndex));
        EVAL Default(text, chIndex);
          override.as_default := Exp(t, override.as_id.sm_type_spec, text, chIndex);
      END; (* while *)
    END; (* begin *)
  END Object;

PROCEDURE Opaque(t: T;
                 o: M3AST_AS.Opaque_type;
                 text: Text.T;
                 VAR chIndex: TextIndex) RAISES {}=
  BEGIN
    o.as_type := M3Type(t, text, chIndex);
    IF NOT Void(text, chIndex) THEN
      o.sm_concrete_type_spec := TypeSpec(M3Type(t, text, chIndex));
    END;
  END Opaque;

PROCEDURE M3Type(t: T;
                 text: Text.T;
                 VAR chIndex: TextIndex): M3AST_AS.M3TYPE RAISES {}=
  VAR 
    type: M3AST_AS.M3TYPE;
    ch: CHAR := Text.GetChar(text, chIndex);
  BEGIN
    CASE ch OF
    | M3LTypeToText.TypeIndexOneCh,
      M3LTypeToText.TypeIndexTwoCh,
      M3LTypeToText.TypeIndexThreeCh,
      M3LTypeToText.TypeIndexManyCh =>
        type := TypeIndex(t, text, chIndex);
    ELSE
      WITH ts = NewTS(ch) DO
        FillinTypeSpec(t, ts, text, chIndex);
        type := ts;
      END;
    END;
    RETURN type;
  END M3Type;

PROCEDURE FillinTypeSpec(t: T;
                         ts: M3AST_AS.TYPE_SPEC;
                         text: Text.T;
                         VAR chIndex: TextIndex)
  RAISES {}=
  VAR ch := Text.GetChar(text, chIndex);
  BEGIN
    INC(chIndex);
    CASE ch OF <*NOWARN*>
    | M3LTypeToText.BooleanCh =>
    | M3LTypeToText.CharCh =>
    | M3LTypeToText.CardinalCh =>
    | M3LTypeToText.IntegerCh =>
    | M3LTypeToText.RealCh =>
    | M3LTypeToText.LongRealCh =>
    | M3LTypeToText.ExtendedCh =>
    | M3LTypeToText.RefAnyCh =>
    | M3LTypeToText.AddressCh =>
    | M3LTypeToText.NullCh =>
    | M3LTypeToText.RootCh =>
    | M3LTypeToText.UntracedRootCh =>
    | M3LTypeToText.EnumerationCh =>
        Enumeration(ts, text, chIndex);
    | M3LTypeToText.SubrangeCh =>
        Subrange(t, ts, text, chIndex);
    | M3LTypeToText.ArrayCh =>
        Array(t, ts, text, chIndex);
    | M3LTypeToText.RecordCh =>
        Record(t, ts, text, chIndex);
    | M3LTypeToText.BitsCh =>
        Packed(t, ts, text, chIndex);
    | M3LTypeToText.SetCh =>
        Set(t, ts, text, chIndex);
    | M3LTypeToText.RefCh,
      M3LTypeToText.UntracedRefCh =>
        Ref(t, ts, text, chIndex, ch = M3LTypeToText.UntracedRefCh);
    | M3LTypeToText.ProcedureCh =>
        Procedure(t, ts, text, chIndex);
    | M3LTypeToText.ObjectCh =>
        Object(t, ts, text, chIndex);
    | M3LTypeToText.OpaqueCh =>
        Opaque(t, ts, text, chIndex);
    END; (* case *)
  END FillinTypeSpec;

(* initialize each type so that other types sharing the type can use the
   type allocated there and get the sharing automatically
*)
PROCEDURE NewTS(ch: CHAR): M3AST_AS.TYPE_SPEC RAISES {}=
  BEGIN
    CASE ch OF <*NOWARN*>
    | M3LTypeToText.BooleanCh =>
        RETURN M3CStdTypes.Boolean();
    | M3LTypeToText.CharCh =>
        RETURN M3CStdTypes.Char();
    | M3LTypeToText.CardinalCh =>
        RETURN M3CStdTypes.Cardinal();
    | M3LTypeToText.IntegerCh =>
        RETURN M3CStdTypes.Integer();
    | M3LTypeToText.RealCh =>
        RETURN M3CStdTypes.Real();
    | M3LTypeToText.LongRealCh =>
        RETURN M3CStdTypes.LongReal();
    | M3LTypeToText.ExtendedCh =>
        RETURN M3CStdTypes.Extended();
    | M3LTypeToText.RefAnyCh =>
        RETURN M3CStdTypes.RefAny();
    | M3LTypeToText.AddressCh =>
        RETURN M3CStdTypes.Address();
    | M3LTypeToText.NullCh =>
        RETURN M3CStdTypes.Null();
    | M3LTypeToText.RootCh =>
        RETURN M3CStdTypes.Root();
    | M3LTypeToText.UntracedRootCh =>
        RETURN M3CStdTypes.Untraced_Root();
    | M3LTypeToText.EnumerationCh =>
        RETURN NEW(M3AST_AS.Enumeration_type).init();
    | M3LTypeToText.SubrangeCh =>
        RETURN NEW(M3AST_AS.Subrange_type).init();
    | M3LTypeToText.ArrayCh =>
        RETURN NEW(M3AST_AS.Array_type).init();
    | M3LTypeToText.RecordCh =>
        RETURN NEW(M3AST_AS.Record_type).init();
    | M3LTypeToText.BitsCh =>
        RETURN NEW(M3AST_AS.Packed_type).init();
    | M3LTypeToText.SetCh =>
        RETURN NEW(M3AST_AS.Set_type).init();
    | M3LTypeToText.RefCh,
      M3LTypeToText.UntracedRefCh =>
        RETURN NEW(M3AST_AS.Ref_type).init();
    | M3LTypeToText.ProcedureCh =>
        RETURN NEW(M3AST_AS.Procedure_type).init();
    | M3LTypeToText.ObjectCh =>
        RETURN NEW(M3AST_AS.Object_type).init();
    | M3LTypeToText.OpaqueCh =>
        RETURN NEW(M3AST_AS.Opaque_type).init();
    END; (* case *)
  END NewTS;

PROCEDURE ForceParseTypeSpec(t: T; ts: M3AST_AS.TYPE_SPEC) RAISES {}=
  BEGIN
    FOR i := 0 TO LAST(t^) DO
      IF ts = t[i].named.sm_type_spec THEN
        ParseTypeSpec(t, i);
      END;
    END;
  END ForceParseTypeSpec;

PROCEDURE ParseTypeSpec(t: T; i: CARDINAL) RAISES {}=
  VAR
    chIndex: CARDINAL := 0;
  BEGIN
    CASE t[i].status OF
    | Status.Unbegun =>
        t[i].status := Status.Started;
        FillinTypeSpec(t, t[i].named.sm_type_spec, t[i].text, chIndex);
        t[i].named.sm_type_spec.tmp_type_code := i;
        t[i].status := Status.Completed;
    | Status.Started =>
        (* recursion to ref types is ok *)
        CASE Text.GetChar(t[i].text, 0) OF
        | M3LTypeToText.RefCh,
          M3LTypeToText.UntracedRefCh,
          M3LTypeToText.ProcedureCh,
          M3LTypeToText.ObjectCh,
          M3LTypeToText.OpaqueCh => (* ok *)
        ELSE DieRecursive();
        END;
    | Status.Completed =>
    END;
  END ParseTypeSpec;

EXCEPTION
  Recursive;

PROCEDURE DieRecursive () RAISES {} =
  <*FATAL ANY*>
  BEGIN
    RAISE Recursive;
  END DieRecursive;

PROCEDURE TypeSpecs(t: T) RAISES {}=
  BEGIN
    FOR i := 0 TO LAST(t^) DO
      t[i].status := Status.Unbegun;
      (* used named types to break the type recursion... *)
      VAR named: M3AST_AS.Named_type := NEW(M3AST_AS.Named_type).init();
         id: M3AST_AS.Qual_used_id := NEW(M3AST_AS.Qual_used_id).init();
      BEGIN
        t[i].named := named;
        id.as_intf_id := NEW(M3AST_AS.Used_interface_id).init();
        id.as_intf_id.lx_symrep := M3CId.Enter("TestTypes");
        id.as_id := NEW(M3AST_AS.Used_def_id).init();
        id.as_id.lx_symrep := M3CId.Enter("Type" & Fmt.Int(i));
        named.as_qual_id := id;
        named.sm_type_spec := NewTS(Text.GetChar(t[i].text, 0));
      END;
    END;

    FOR i := 0 TO LAST(t^) DO
      ParseTypeSpec(t, i);
    END;
  END TypeSpecs;

BEGIN
END M3LTextToType.
