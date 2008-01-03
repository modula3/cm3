(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3AST_SM_F;

IMPORT AST, M3AST_AS, M3AST_PG, M3AST_SM;

PROCEDURE NotA_INIT_ID(<*UNUSED*> n: NODE;
                       <*UNUSED*> VAR(*out*) init_id: M3AST_SM.INIT_ID): BOOLEAN=
  BEGIN RETURN FALSE END NotA_INIT_ID;

PROCEDURE NotA_CCV_ID(<*UNUSED*> n: NODE;
                      <*UNUSED*> VAR(*out*) ccv_id: M3AST_SM.CCV_ID): BOOLEAN=
  BEGIN RETURN FALSE END NotA_CCV_ID;

PROCEDURE NotA_RECOBJ_ID(
    <*UNUSED*> n: NODE;
    <*UNUSED*> VAR(*out*) recobj_id: M3AST_SM.RECOBJ_ID): BOOLEAN=
  BEGIN RETURN FALSE END NotA_RECOBJ_ID;

PROCEDURE NotA_REDEF_ID(
    <*UNUSED*> n: NODE;
    <*UNUSED*> VAR(*out*) init_id: M3AST_SM.REDEF_ID): BOOLEAN=
  BEGIN RETURN FALSE END NotA_REDEF_ID;

PROCEDURE NotA_SCOPE(<*UNUSED*> n: NODE;
                     <*UNUSED*> VAR(*out*) scope: M3AST_SM.SCOPE): BOOLEAN=
  BEGIN RETURN FALSE END NotA_SCOPE;

PROCEDURE A_INIT_ID(n: NODE;
                    VAR(*out*) init_id: M3AST_SM.INIT_ID): BOOLEAN=
  BEGIN
    TYPECASE n OF
    | M3AST_AS.Const_id(t) => init_id := t.vINIT_ID; RETURN TRUE
    | M3AST_AS.Var_id(t) => init_id := t.vINIT_ID; RETURN TRUE
    | M3AST_AS.F_Value_id(t) => init_id := t.vINIT_ID; RETURN TRUE 
    | M3AST_AS.F_Readonly_id(t) => init_id := t.vINIT_ID; RETURN TRUE
    | M3AST_AS.Field_id(t) => init_id := t.vINIT_ID; RETURN TRUE
    | M3AST_AS.For_id(t) => init_id := t.vINIT_ID; RETURN TRUE 
    | M3AST_AS.With_id(t) => init_id := t.vINIT_ID; RETURN TRUE
    | M3AST_AS.METHOD_OVERRIDE_ID(t) => init_id := t.vINIT_ID; RETURN TRUE;
    ELSE RETURN FALSE
    END
  END A_INIT_ID;

PROCEDURE A_CCV_ID(n: NODE;
                      VAR(*out*) ccv_id: M3AST_SM.CCV_ID): BOOLEAN=
  BEGIN
    TYPECASE n OF
    | M3AST_AS.Const_id(t) => ccv_id := t.vCCV_ID; RETURN TRUE
    | M3AST_AS.Enum_id(t) => ccv_id := t.vCCV_ID; RETURN TRUE
    ELSE RETURN FALSE
    END;
  END A_CCV_ID;

PROCEDURE A_RECOBJ_ID(n: NODE;
                         VAR(*out*) recobj_id: M3AST_SM.RECOBJ_ID): BOOLEAN=
  BEGIN
    TYPECASE n OF
    | M3AST_AS.METHOD_OVERRIDE_ID(t) => recobj_id := t.vRECOBJ_ID; RETURN TRUE
    | M3AST_AS.Field_id(t) => recobj_id := t.vRECOBJ_ID; RETURN TRUE
    ELSE RETURN FALSE
    END;
  END A_RECOBJ_ID;

PROCEDURE A_REDEF_ID(n: NODE;
                        VAR(*out*) redef_id: M3AST_SM.REDEF_ID): BOOLEAN=
  BEGIN
    TYPECASE n OF
    | M3AST_AS.Proc_id(t) => redef_id := t.vREDEF_ID; RETURN TRUE
    | M3AST_AS.METHOD_OVERRIDE_ID(t) => redef_id := t.vREDEF_ID; RETURN TRUE
    ELSE RETURN FALSE
    END;
  END A_REDEF_ID;

PROCEDURE A_SCOPE(n: NODE;
                     VAR(*out*) scope: M3AST_SM.SCOPE): BOOLEAN=
  BEGIN
    TYPECASE n OF
    | M3AST_AS.UNIT_ID(t) => scope := t.vSCOPE; RETURN TRUE;
    | M3AST_AS.Block(t) => scope := t.vSCOPE; RETURN TRUE;
    | M3AST_AS.Proc_id(t) => scope := t.vSCOPE; RETURN TRUE;
    | M3AST_AS.Method_id(t) => scope := t.vSCOPE; RETURN TRUE;
    | M3AST_AS.With_id(t) => scope := t.vSCOPE; RETURN TRUE;
    | M3AST_AS.For_id(t) => scope := t.vSCOPE; RETURN TRUE;
    | M3AST_AS.Tcase_id(t) => scope := t.vSCOPE; RETURN TRUE;
    | M3AST_AS.Handler_id(t) => scope := t.vSCOPE; RETURN TRUE;
    ELSE
      RETURN FALSE
    END
  END A_SCOPE;

PROCEDURE Init_UNIT_ID(n: UNIT_ID): AST.NODE=
  BEGIN
    n.vSCOPE := NEW(M3AST_SM.SCOPE).init();
    n.vSCOPE.sm_mi_node := n;
    RETURN n;
  END Init_UNIT_ID;

PROCEDURE Init_Interface_id(n: Interface_id): AST.NODE RAISES {}=
  BEGIN
    n.vEXTERNAL_ID := NEW(M3AST_PG.EXTERNAL_ID).init();
    RETURN Init_UNIT_ID(n);
  END Init_Interface_id;


PROCEDURE Init_Module_id(n: Module_id): AST.NODE RAISES {}=
  BEGIN
    RETURN Init_UNIT_ID(n);
  END Init_Module_id;


PROCEDURE Init_Type_id(n: Type_id): AST.NODE RAISES {}=
  BEGIN
    n.vEXTERNAL_ID := NEW(M3AST_PG.EXTERNAL_ID).init();
    RETURN n;
  END Init_Type_id;


PROCEDURE Init_F_Value_id(n: F_Value_id): AST.NODE RAISES {}=
  BEGIN
    n.vINIT_ID := NEW(M3AST_SM.INIT_ID).init();
    RETURN n;
  END Init_F_Value_id;


PROCEDURE Init_F_Readonly_id(n: F_Readonly_id): AST.NODE RAISES {}=
  BEGIN
    n.vINIT_ID := NEW(M3AST_SM.INIT_ID).init();
    RETURN n;
  END Init_F_Readonly_id;


PROCEDURE Init_For_id(n: For_id): AST.NODE RAISES {}=
  BEGIN
    n.vINIT_ID := NEW(M3AST_SM.INIT_ID).init();
    n.vSCOPE := NEW(M3AST_SM.SCOPE).init();
    n.vSCOPE.sm_mi_node := n;
    RETURN n;
  END Init_For_id;


PROCEDURE Init_Const_id(n: Const_id): AST.NODE RAISES {}=
  BEGIN
    n.vINIT_ID := NEW(M3AST_SM.INIT_ID).init();
    n.vCCV_ID := NEW(M3AST_SM.CCV_ID).init();
    n.vEXTERNAL_ID := NEW(M3AST_PG.EXTERNAL_ID).init();
    RETURN n;
  END Init_Const_id;


PROCEDURE Init_Var_id(n: Var_id): AST.NODE RAISES {}=
  BEGIN
    n.vINIT_ID := NEW(M3AST_SM.INIT_ID).init();
    n.vEXTERNAL_ID := NEW(M3AST_PG.EXTERNAL_ID).init();
    RETURN n;
  END Init_Var_id;


PROCEDURE Init_Proc_id(n: Proc_id): AST.NODE RAISES {}=
  BEGIN
    n.vREDEF_ID := NEW(M3AST_SM.REDEF_ID).init();
    n.vEXTERNAL_ID := NEW(M3AST_PG.EXTERNAL_ID).init();
    n.vSCOPE := NEW(M3AST_SM.SCOPE).init();
    n.vSCOPE.sm_mi_node := n;
    n.sm_concrete_proc_id := M3AST_SM.UNSET_DEF_ID();
    RETURN n;
  END Init_Proc_id;


PROCEDURE Init_Enum_id(n: Enum_id): AST.NODE RAISES {}=
  BEGIN
    n.vCCV_ID := NEW(M3AST_SM.CCV_ID).init();
    RETURN n;
  END Init_Enum_id;


PROCEDURE Init_Field_id(n: Field_id): AST.NODE RAISES {}=
  BEGIN
    n.vRECOBJ_ID := NEW(M3AST_SM.RECOBJ_ID).init();
    n.vINIT_ID := NEW(M3AST_SM.INIT_ID).init();
    RETURN n;
  END Init_Field_id;


PROCEDURE Init_METHOD_OVERRIDE_ID(n: METHOD_OVERRIDE_ID): AST.NODE RAISES {}=
  BEGIN
    n.vINIT_ID := NEW(M3AST_SM.INIT_ID).init();
    n.vREDEF_ID := NEW(M3AST_SM.REDEF_ID).init();
    n.vRECOBJ_ID := NEW(M3AST_SM.RECOBJ_ID).init();
    RETURN n;
  END Init_METHOD_OVERRIDE_ID;


PROCEDURE Init_Method_id(n: Method_id): AST.NODE RAISES {}=
  BEGIN
    n.vSCOPE := NEW(M3AST_SM.SCOPE).init();
    n.vSCOPE.sm_mi_node := n;
    RETURN Init_METHOD_OVERRIDE_ID(n);
  END Init_Method_id;


PROCEDURE Init_With_id(n: With_id): AST.NODE RAISES {}=
  BEGIN
    n.vINIT_ID := NEW(M3AST_SM.INIT_ID).init();
    n.vSCOPE := NEW(M3AST_SM.SCOPE).init();
    n.vSCOPE.sm_mi_node := n;
    RETURN n;
  END Init_With_id;


PROCEDURE Init_Exc_id(n: Exc_id): AST.NODE RAISES {}=
  BEGIN
    n.vEXTERNAL_ID := NEW(M3AST_PG.EXTERNAL_ID).init();
    RETURN n;
  END Init_Exc_id;

PROCEDURE Init_Tcase_id(n: Tcase_id): AST.NODE RAISES {}=
  BEGIN
    n.vSCOPE := NEW(M3AST_SM.SCOPE).init();
    n.vSCOPE.sm_mi_node := n;
    RETURN n;
  END Init_Tcase_id;

PROCEDURE Init_Handler_id(n: Handler_id): AST.NODE RAISES {}=
  BEGIN
    n.vSCOPE := NEW(M3AST_SM.SCOPE).init();
    n.vSCOPE.sm_mi_node := n;
    RETURN n;
  END Init_Handler_id;

PROCEDURE Init_Block(n: Block): AST.NODE RAISES {}=
  BEGIN
    n.vSCOPE := NEW(M3AST_SM.SCOPE).init();
    n.vSCOPE.sm_mi_node := n;
    RETURN n;
  END Init_Block;

PROCEDURE Init_INIT_ID(n: INIT_ID): AST.NODE RAISES {}=
  BEGIN
    n.sm_init_exp := M3AST_SM.UNSET_EXP();
    RETURN n;
  END Init_INIT_ID;

PROCEDURE Init_REDEF_ID(n: REDEF_ID): AST.NODE RAISES {}=
  BEGIN
    n.sm_int_def := M3AST_SM.UNSET_DEF_ID();
    RETURN n;
  END Init_REDEF_ID;

PROCEDURE Init_Procedure_type(n: Procedure_type): AST.NODE RAISES {}=
  BEGIN
    n.sm_def_id := M3AST_SM.UNSET_DEF_ID();
    RETURN n;
  END Init_Procedure_type;

BEGIN
END M3AST_SM_F.
