MODULE M3CBitSize;

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


IMPORT AST, M3AST_AS, M3AST_SM;

IMPORT M3AST_AS_F, M3AST_SM_F;


IMPORT M3ASTNext, M3CTypesMisc, M3COrdinal;
IMPORT M3CBackEnd;

(* Only the back-end knows how to compute these attributes.
   What we do here is ensure that the attributes of dependent
   types are computed before asking it to compute a size.
*)

EXCEPTION UnsetComponent;

VAR
  proto_object_type_g: M3AST_AS.Object_type := NIL;

PROCEDURE Set(an: AST.NODE) RAISES {}=
  BEGIN
    TYPECASE an OF
    | M3AST_AS.TYPE_SPEC(ts) =>
        TRY
          Eval(ts);
        EXCEPT
        | UnsetComponent =>
            (* leave unset *)
        END;
    ELSE
    END; (* if *)
  END Set;


PROCEDURE Eval(ts: M3AST_SM.TYPE_SPEC_UNSET) RAISES {UnsetComponent}=
  VAR 
    componentType: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    IF ts = NIL THEN RAISE UnsetComponent END;

    IF ts.sm_bitsize > 0 THEN RETURN END;

    TYPECASE ts OF
    | M3AST_AS.Record_type(recordType) =>
        VAR
          iter := M3ASTNext.NewIterField(recordType.as_fields_s);
          fieldId: M3AST_AS.Field_id;
        BEGIN
          WHILE M3ASTNext.Field(iter, fieldId) DO
            Eval(fieldId.sm_type_spec);
          END; (* while *)
        END;

    | M3AST_AS.Object_type(objectType) =>
        (* a reference type, but we compute the referent size/align also;
           but we need to prepare for legal recursion, so we set the
           size on the object first (using an empty prototype object). *)
        VAR
          iter := M3ASTNext.NewIterField(objectType.as_fields_s);
          fieldId: M3AST_AS.Field_id;
        BEGIN
          IF proto_object_type_g = NIL THEN
            proto_object_type_g := NEW(M3AST_AS.Object_type).init();
            M3CBackEnd.BitSizeAndAlign(proto_object_type_g);
          END;
          objectType.sm_bitsize := proto_object_type_g.sm_bitsize;
          objectType.sm_align := proto_object_type_g.sm_align;
          WHILE M3ASTNext.Field(iter, fieldId) DO
            Eval(fieldId.sm_type_spec);
          END; (* while *)
        END;

    | M3AST_AS.Array_type(arrayType) =>
        VAR
          element, index: M3AST_SM.TYPE_SPEC_UNSET;
          isopen: BOOLEAN;
        BEGIN
          IF M3ASTNext.Array(arrayType, element, isopen, index) THEN END;
          IF NOT isopen THEN Eval(index) END;
          Eval(element);
        END;

    | M3AST_AS.Packed_type(packedType) =>
        CheckExp_value(packedType.as_exp);
        CheckOrdinal(packedType.as_exp.sm_exp_type_spec, FALSE);
        M3CTypesMisc.GetTYPE_SPECFromM3TYPE(
            packedType.as_type, componentType);
        Eval(componentType);

    | M3AST_AS.Set_type(setType) =>
        M3CTypesMisc.GetTYPE_SPECFromM3TYPE(
            setType.as_type, componentType);
        Eval(componentType);

    | M3AST_AS.Subrange_type(subrangeType) =>
        VAR
          range := subrangeType.as_range;
        BEGIN
          CheckExp_value(range.as_exp1);
          CheckOrdinal(range.as_exp1.sm_exp_type_spec, TRUE);
          CheckExp_value(range.as_exp2);
          CheckOrdinal(range.as_exp2.sm_exp_type_spec, TRUE);
        END;

    | M3AST_SM.Any_type, M3AST_SM.Type_type =>
        RAISE UnsetComponent;

    ELSE (* other primitive types *)
    END; (* case *)

    M3CBackEnd.BitSizeAndAlign(ts);
  END Eval;


<*INLINE*> PROCEDURE CheckExp_value(e: M3AST_AS.EXP) RAISES {UnsetComponent}=
  BEGIN
    IF e.sm_exp_value = NIL THEN RAISE UnsetComponent END;
  END CheckExp_value;


PROCEDURE CheckOrdinal(
    ts: M3AST_SM.TYPE_SPEC_UNSET;
    enumOk: BOOLEAN)
    RAISES {UnsetComponent}=
  VAR
    baseType: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    IF M3COrdinal.Is(ts, baseType) THEN
      TYPECASE baseType OF
      | NULL =>
      | M3AST_AS.Integer_type =>
          RETURN;
      | M3AST_AS.Enumeration_type =>
          IF enumOk THEN RETURN END;
      ELSE
      END;
    END;
    RAISE UnsetComponent;
  END CheckOrdinal;


BEGIN
END M3CBitSize.
