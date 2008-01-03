(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3ASTOp_SM;

IMPORT M3AST, M3AST_AS, M3AST_SM;
IMPORT M3AST_SM_F;

REVEAL CheckSetClosure = CheckSetClosure_public BRANDED OBJECT END;

PROCEDURE CheckSet(an: NODE; cl: CheckSetClosure)=
  VAR
    initId: M3AST_SM.INIT_ID;
    redefId: M3AST_SM.REDEF_ID;
    recobjId: M3AST_SM.RECOBJ_ID;
    usedId: M3AST_AS.USED_ID;
    ccvId: M3AST_SM.CCV_ID;
  BEGIN
    TYPECASE an OF
    | M3AST_AS.EXP(exp) =>
        C(cl, an, exp.sm_exp_type_spec);
    | M3AST_AS.TYPED_ID(typed_id) =>
        C(cl, an, typed_id.sm_type_spec);
    | M3AST_AS.UNIT_ID(unit_id) =>
        C(cl, an, unit_id.sm_spec);
    | M3AST_AS.UNIT(unit) =>
        C(cl, an, unit.sm_comp_unit);
    ELSE
    END;

    IF an.IsA_INIT_ID(initId) THEN
      B(cl, an, initId.sm_init_exp = M3AST_SM.UNSET_EXP());
    END; 

    IF an.IsA_REDEF_ID(redefId) THEN
      B(cl, an, redefId.sm_int_def = M3AST_SM.UNSET_DEF_ID());
    END;
      
    IF an.IsA_RECOBJ_ID(recobjId) THEN
      C(cl, an, recobjId.sm_enc_type_spec);
    END;
      
    IF an.IsA_USED_ID(usedId) THEN
      C(cl, an, usedId.sm_def);
    END; (* if *)

    IF an.IsA_CCV_ID(ccvId) THEN
      B(cl, an, ccvId.sm_exp_value = NIL);
    END; (* if *)

    TYPECASE an OF
    | M3AST_AS.Named_type(named_type) =>
        C(cl, an, named_type.sm_type_spec);
    | M3AST_AS.Subrange_type(subrange_type) =>
        C(cl, an, subrange_type.sm_base_type_spec);
    | M3AST_AS.Enumeration_type(enumeration_type) =>
        B(cl, an, enumeration_type.sm_num_elements < 0);
    | M3AST_AS.Array_type(array_type) =>
        C(cl, an, array_type.sm_norm_type);
    | M3AST_AS.Object_type(object_type) =>
        B(cl, an, object_type.sm_rf_bitsize < 0 OR
                  object_type.sm_rf_align < 0);
    | M3AST_AS.Procedure_type(procedure_type) =>
        B(cl, an, procedure_type.sm_def_id =
                M3AST_SM.UNSET_DEF_ID());
    | M3AST_AS.Proc_id(proc_id) =>
        B(cl, an, proc_id.sm_concrete_proc_id =
                M3AST_SM.UNSET_DEF_ID());
    ELSE
    END; (* case *)

    TYPECASE an OF
    | M3AST_SM.Any_type, M3AST_SM.Type_type =>
        (* Ignore unset attributes in these bogus types *)
    | M3AST_AS.TYPE_SPEC(typeSpec) =>
        B(cl, an, typeSpec.sm_bitsize < 0 OR typeSpec.sm_align < 0);
    ELSE
    END; (* if *)
  END CheckSet;

PROCEDURE B(cl: CheckSetClosure; an: M3AST.NODE; b: BOOLEAN) RAISES {}=
  BEGIN
    IF b THEN cl.callback(an) END;
  END B;

PROCEDURE C(cl: CheckSetClosure; an: M3AST.NODE; tn: M3AST.NODE) RAISES {}=
  BEGIN
    IF tn = NIL THEN
      cl.callback(an);
    END; (* if *)
  END C;

BEGIN
END M3ASTOp_SM.
