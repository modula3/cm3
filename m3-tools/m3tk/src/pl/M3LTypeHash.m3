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

UNSAFE MODULE M3LTypeHash;

IMPORT M3AST_AS, M3AST_SM;

IMPORT M3AST_AS_F, M3AST_SM_F, M3AST_TM_F;

IMPORT SeqM3AST_AS_Fields, SeqM3AST_AS_Enum_id, SeqM3AST_AS_Method,
    SeqM3AST_AS_M3TYPE;

IMPORT M3ASTNext, M3CId, M3CExpValue, M3CTypesMisc, M3CBackEnd;
IMPORT FingerPrint;


(* type classification *)

PROCEDURE Classify(t: M3AST_AS.TYPE_SPEC): Class RAISES {}=
  VAR
    index_type_spec: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    TYPECASE t OF <*NOWARN*>
    | M3AST_AS.Enumeration_type =>
        RETURN Class.Enumeration;
    | M3AST_AS.Subrange_type(subrange_type) =>
          IF ISTYPE(subrange_type.sm_base_type_spec,
	         M3AST_AS.Enumeration_type) THEN
            RETURN Class.EnumerationSubrange;
          ELSE
            RETURN Class.IntegerSubrange;
          END; (* if *)
    | M3AST_AS.Array_type =>
        IF M3CTypesMisc.Index(t, index_type_spec) = M3CTypesMisc.Ix.Open THEN
          RETURN Class.OpenArray;
        ELSE
          RETURN Class.Array;
        END; (* if *)
    | M3AST_AS.Record_type =>
        RETURN Class.Record;
    | M3AST_AS.Object_type(object) =>
          IF object.as_brand # NIL THEN
            RETURN Class.Branded;
          ELSIF object.as_ancestor = NIL THEN
            RETURN Class.Object;
          ELSE
            VAR
              ts: M3AST_AS.TYPE_SPEC;
            BEGIN
              M3CTypesMisc.GetTYPE_SPECFromM3TYPE(object.as_ancestor, ts);
              TYPECASE ts OF
              | M3AST_AS.Root_type =>
                  RETURN Class.Object;
              ELSE
                RETURN Class.SubClassObject;
              END;
            END;
          END; (* case *)
    | M3AST_AS.Root_type(root_type) =>
        IF root_type.as_trace_mode = NIL THEN
          RETURN Class.Root;
        ELSE
          RETURN Class.UntracedRoot;
        END;
    | M3AST_AS.Set_type =>
        RETURN Class.Set;
    | M3AST_AS.Procedure_type(p) =>
        IF p.as_result_type = NIL THEN
          RETURN Class.Procedure;
        ELSE
          RETURN Class.Function;
        END; (* if *)
    | M3AST_AS.Ref_type(ref) =>
          IF ref.as_brand # NIL THEN
            RETURN Class.Branded;
          ELSIF ref.as_trace_mode = NIL THEN
            RETURN Class.Ref;
          ELSE
            RETURN Class.UntracedRef;
          END; (* if *)
    | M3AST_AS.RefAny_type =>
        RETURN Class.RefAny;
    | M3AST_AS.Address_type =>
        RETURN Class.Address;
    | M3AST_AS.Packed_type =>
        RETURN Class.Packed;
    | M3AST_AS.Integer_type =>
        RETURN Class.Integer;
    | M3AST_AS.Real_type =>
        RETURN Class.Real;
    | M3AST_AS.LongReal_type =>
        RETURN Class.LongReal;
    | M3AST_AS.Extended_type =>
        RETURN Class.Extended;
    | M3AST_AS.Null_type =>
        RETURN Class.Null;
    | M3AST_AS.Opaque_type =>
        RETURN Class.Branded;
    | M3AST_SM.Any_type =>
        RETURN Class.Any;
    | M3AST_SM.Type_type =>
        RETURN Class.Type;
    END; (* case *)
  END Classify;


PROCEDURE HashId(id: M3AST_AS.ID; VAR hash: INTEGER) RAISES {}=
  BEGIN
    FingerPrint.TextIncrementalSingle(M3CId.ToText(id.lx_symrep), hash);
  END HashId;


PROCEDURE HashExp(exp: M3AST_AS.EXP; VAR hash: INTEGER) RAISES {}=
  VAR
    value: INTEGER;
  BEGIN
    CASE M3CExpValue.Ordinal(exp, value) OF
    | M3CBackEnd.NumStatus.Valid =>
    | M3CBackEnd.NumStatus.Overflow =>
        value := LAST(INTEGER);
    ELSE
      RETURN;
    END; (* if *)
    FingerPrint.DataIncrementalSingle(ADR(value), BYTESIZE(value), hash);
  END HashExp;

PROCEDURE HashComponentTypeSpec(
    class: Class;
    comp: M3AST_AS.TYPE_SPEC;
    VAR hash: INTEGER)
    RAISES {}=
  VAR
    compClass := Classify(comp);
  BEGIN
    IF compClass < class THEN
      (* Type code has been set to something which will not change while we
       are hashing all the other members of 'class' *)
      WITH code = comp.tmp_type_code DO
        IF hash = 0 THEN
          hash := code;
        ELSE
          FingerPrint.DataIncrementalSingle(ADR(code), BYTESIZE(code), hash);
        END;
      END;
    ELSE
      FingerPrint.IncrementalSingle(ORD(compClass), hash);
    END;
  END HashComponentTypeSpec;


PROCEDURE HashComponentM3Type(
    class: Class;
    comp: M3AST_AS.M3TYPE;
    VAR hash: INTEGER)
    RAISES {}=
  VAR
    ts: M3AST_AS.TYPE_SPEC;
  BEGIN
    TYPECASE comp OF <*NOWARN*>
    | M3AST_AS.Named_type(namedType) =>
        ts := namedType.sm_type_spec;
    | M3AST_AS.TYPE_SPEC(typeSpec) =>
        ts := typeSpec;
    END;
    HashComponentTypeSpec(class, ts, hash);
  END HashComponentM3Type;


PROCEDURE HashFields(
    class: Class;
    seq: SeqM3AST_AS_Fields.T;
    VAR hash: INTEGER)
    RAISES {}=
  VAR
    iter := M3ASTNext.NewIterField(seq);
    id: M3AST_AS.Field_id;
  BEGIN
    WHILE M3ASTNext.Field(iter, id) DO
      HashId(id, hash);
      HashComponentTypeSpec(class, id.sm_type_spec, hash);
      IF id.vINIT_ID.sm_init_exp # NIL THEN
        HashExp(id.vINIT_ID.sm_init_exp, hash);
      END;
    END;
  END HashFields;


PROCEDURE Value(
    t: M3AST_AS.TYPE_SPEC;
    class: ConstructorType)
    : INTEGER
    RAISES {}=
  VAR
    hash := 0;
  BEGIN
    CASE class OF
    | Class.Enumeration =>
        VAR
          enum := NARROW(t, M3AST_AS.Enumeration_type);
          iter := SeqM3AST_AS_Enum_id.NewIter(enum.as_id_s);
          id: M3AST_AS.Enum_id;
          first := TRUE;
        BEGIN
          WHILE SeqM3AST_AS_Enum_id.Next(iter, id) DO
            IF first THEN
              first := FALSE;
            ELSE
              FingerPrint.IncrementalSingle(0, hash);
            END;
            HashId(id, hash);
          END;
        END;
    | Class.IntegerSubrange, Class.EnumerationSubrange =>
        WITH range = NARROW(t, M3AST_AS.Subrange_type).as_range DO
          HashExp(range.as_exp1, hash);
          HashExp(range.as_exp2, hash);
        END;
    | Class.Set =>
        HashComponentM3Type(class, NARROW(t, M3AST_AS.Set_type).as_type, hash);
    | Class.Packed =>
        VAR
          packed := NARROW(t, M3AST_AS.Packed_type);
        BEGIN
          HashExp(packed.as_exp, hash);
          HashComponentM3Type(class, packed.as_type, hash);
        END;
    | Class.Array, Class.OpenArray =>
        VAR
          array := NARROW(t, M3AST_AS.Array_type).sm_norm_type;
        BEGIN
          IF class # Class.OpenArray THEN
            HashComponentM3Type(
                class, SeqM3AST_AS_M3TYPE.First(array.as_indextype_s), hash);
            (* unexpected is empty *)
          END;
          HashComponentM3Type(class, array.as_elementtype, hash);
        END;
    | Class.Procedure, Class.Function =>
        VAR
          proc := NARROW(t, M3AST_AS.Procedure_type);
          iter := M3ASTNext.NewIterFormal(proc.as_formal_param_s);
          f: M3AST_AS.Formal_param;
          id: M3AST_AS.FORMAL_ID;
        BEGIN
          WHILE M3ASTNext.Formal(iter, f, id) DO
            HashId(id, hash);
            VAR
              code: FingerPrint.Byte;
            BEGIN
              TYPECASE id OF <*NOWARN*>
              | M3AST_AS.F_Value_id => code := 0;
              | M3AST_AS.F_Var_id => code := 1;
              | M3AST_AS.F_Readonly_id => code := 2;
              END;
              FingerPrint.IncrementalSingle(code, hash);
            END;
            HashComponentTypeSpec(class, id.sm_type_spec, hash);
            IF f.as_default # NIL THEN HashExp(f.as_default, hash) END;
          END;
          IF proc.as_result_type # NIL THEN
            HashComponentM3Type(class, proc.as_result_type, hash);
          END;
        END;
    | Class.Record =>
        HashFields(class, NARROW(t, M3AST_AS.Record_type).as_fields_s, hash);
    | Class.Object, Class.SubClassObject =>
        VAR
          object := NARROW(t, M3AST_AS.Object_type);
          iter := SeqM3AST_AS_Method.NewIter(object.as_method_s);
          m: M3AST_AS.Method;
        BEGIN
          HashFields(class, object.as_fields_s, hash);
          WHILE SeqM3AST_AS_Method.Next(iter, m) DO
            HashId(m.as_id, hash);
            IF m.as_type # NIL THEN
              HashComponentM3Type(class, m.as_type, hash);
            END;
          END;
        END;
    | Class.Ref, Class.UntracedRef =>
        HashComponentM3Type(class, NARROW(t, M3AST_AS.Ref_type).as_type, hash);
    END;
    RETURN hash DIV 2;
  END Value;


BEGIN
END M3LTypeHash.
