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
(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE M3AST_SM_F;

(*--------------------------------------------------------------------------
  This interface is a companion to "M3AST_SM", in which the attributes
  are declared to be object fields.
--------------------------------------------------------------------------*)

IMPORT AST, AST_WalkRep, AST_Iter;
IMPORT M3AST, M3AST_AS, M3AST_SM, M3AST_PG;
IMPORT SeqM3AST_SM_Opaque_type_Revln, SeqM3AST_AS_Used_interface_id,
    SeqM3AST_AS_TYPE_SPEC, SeqM3AST_AS_EXP, SeqM3AST_AS_RANGE_EXP,
    SeqM3AST_AS_Actual, SeqM3AST_AS_DEF_ID;
IMPORT M3ASTOp_AS AS Previous_View;

(*--------------------------------------------------------------------------
                 Refinements of M3AST, M3AST_AS nodes
--------------------------------------------------------------------------*)

TYPE
  NODE = Previous_View.NODE OBJECT
  METHODS
    IsA_INIT_ID(VAR(*out*) init_id: M3AST_SM.INIT_ID): BOOLEAN :=
        NotA_INIT_ID;
    IsA_CCV_ID(VAR(*out*) ccv_id: M3AST_SM.CCV_ID): BOOLEAN :=
        NotA_CCV_ID;
    IsA_RECOBJ_ID(VAR(*out*) recobj_id: M3AST_SM.RECOBJ_ID): BOOLEAN :=
        NotA_RECOBJ_ID;
    IsA_REDEF_ID(VAR(*out*) redef_id: M3AST_SM.REDEF_ID): BOOLEAN :=
        NotA_REDEF_ID;
    IsA_SCOPE(VAR(*out*) scope: M3AST_SM.SCOPE): BOOLEAN :=
        NotA_SCOPE;
  END;

  UNIT = Previous_View.UNIT OBJECT
    sm_comp_unit: M3AST_AS.Compilation_Unit := NIL;
  END;

  UNIT_WITH_BODY = Previous_View.UNIT_WITH_BODY OBJECT
    sm_import_s := SeqM3AST_AS_Used_interface_id.Null;
    sm_type_spec_s := SeqM3AST_AS_TYPE_SPEC.Null;
    sm_reveal_s := SeqM3AST_SM_Opaque_type_Revln.Null;
  END;

  UNIT_GEN_INS = Previous_View.UNIT_GEN_INS OBJECT
    sm_ins_comp_unit: M3AST_AS.Compilation_Unit := NIL;  
  END;

  UNIT_ID = Previous_View.UNIT_ID OBJECT
    sm_spec: M3AST_AS.UNIT := NIL;
    vSCOPE: M3AST_SM.SCOPE := NIL;
  OVERRIDES
    IsA_SCOPE := A_SCOPE;
  END;

  Module = Previous_View.Module OBJECT
    sm_export_s := SeqM3AST_AS_Used_interface_id.Null;
  END;

  Interface_id = Previous_View.Interface_id OBJECT
    vEXTERNAL_ID: M3AST_PG.EXTERNAL_ID := NIL;
  OVERRIDES
    init := Init_Interface_id;
  END;

  Module_id = Previous_View.Module_id OBJECT
  OVERRIDES
    init := Init_Module_id;
  END;

  Type_id = Previous_View.Type_id OBJECT
    vEXTERNAL_ID: M3AST_PG.EXTERNAL_ID := NIL;
  OVERRIDES
    init := Init_Type_id;
  END;

  Exc_id = Previous_View.Exc_id OBJECT
    vEXTERNAL_ID: M3AST_PG.EXTERNAL_ID := NIL;
  OVERRIDES
    init := Init_Exc_id;
  END;

  Proc_id = Previous_View.Proc_id OBJECT
    sm_spec: M3AST_SM.Proc_decl_UNSET := NIL;
    sm_concrete_proc_id: M3AST_SM.DEF_ID_NULL_UNSET := NIL;
    vREDEF_ID: M3AST_SM.REDEF_ID := NIL;
    vEXTERNAL_ID: M3AST_PG.EXTERNAL_ID := NIL;
    vSCOPE: M3AST_SM.SCOPE := NIL;
  OVERRIDES
    init := Init_Proc_id;
    IsA_REDEF_ID := A_REDEF_ID;
    IsA_SCOPE := A_SCOPE;
  END;

(* These all inherit INIT_ID and/or CCV_ID *)

  METHOD_OVERRIDE_ID = Previous_View.METHOD_OVERRIDE_ID OBJECT
    sm_spec: M3AST_SM.METHOD_OVERRIDE_UNSET := NIL;
    vINIT_ID: M3AST_SM.INIT_ID := NIL;
    vREDEF_ID: M3AST_SM.REDEF_ID := NIL;
    vRECOBJ_ID: M3AST_SM.RECOBJ_ID := NIL;
  OVERRIDES
    init := Init_METHOD_OVERRIDE_ID;
    IsA_INIT_ID := A_INIT_ID;
    IsA_REDEF_ID := A_REDEF_ID;
    IsA_RECOBJ_ID := A_RECOBJ_ID;
  END;

  Method_id = Previous_View.Method_id OBJECT
    vSCOPE: M3AST_SM.SCOPE := NIL;
  OVERRIDES
    init := Init_Method_id;
    IsA_SCOPE := A_SCOPE;
  END;

  Field_id = Previous_View.Field_id OBJECT
    vRECOBJ_ID: M3AST_SM.RECOBJ_ID := NIL;
    vINIT_ID: M3AST_SM.INIT_ID := NIL;
  OVERRIDES
    init := Init_Field_id;
    IsA_INIT_ID := A_INIT_ID;
    IsA_REDEF_ID := A_REDEF_ID;
    IsA_RECOBJ_ID := A_RECOBJ_ID;
  END;

  Const_id = Previous_View.Const_id OBJECT
    vINIT_ID: M3AST_SM.INIT_ID := NIL;
    vCCV_ID: M3AST_SM.CCV_ID := NIL;
    vEXTERNAL_ID: M3AST_PG.EXTERNAL_ID := NIL;
  OVERRIDES
    init := Init_Const_id;
    IsA_INIT_ID := A_INIT_ID;
    IsA_CCV_ID := A_CCV_ID;
  END;

  Enum_id = Previous_View.Enum_id OBJECT
    vCCV_ID: M3AST_SM.CCV_ID := NIL;
  OVERRIDES
    init := Init_Enum_id;
    IsA_INIT_ID := A_INIT_ID;
  END;

  Var_id = Previous_View.Var_id OBJECT
    vINIT_ID: M3AST_SM.INIT_ID := NIL;
    vEXTERNAL_ID: M3AST_PG.EXTERNAL_ID := NIL;
  OVERRIDES
    init := Init_Var_id;
    IsA_INIT_ID := A_INIT_ID;
  END;

  F_Value_id = Previous_View.F_Value_id OBJECT
    vINIT_ID: M3AST_SM.INIT_ID := NIL;
  OVERRIDES
    init := Init_F_Value_id;
    IsA_INIT_ID := A_INIT_ID;
  END;

  F_Readonly_id = Previous_View.F_Readonly_id OBJECT
    vINIT_ID: M3AST_SM.INIT_ID := NIL;
  OVERRIDES
    init := Init_F_Readonly_id;
    IsA_INIT_ID := A_INIT_ID;
  END;

  For_id = Previous_View.For_id OBJECT
    vINIT_ID: M3AST_SM.INIT_ID := NIL;
    vSCOPE: M3AST_SM.SCOPE := NIL;
  OVERRIDES
    init := Init_For_id;
    IsA_INIT_ID := A_INIT_ID;
    IsA_SCOPE := A_SCOPE;
  END;

  With_id = Previous_View.With_id OBJECT
    vINIT_ID: M3AST_SM.INIT_ID := NIL;
    vSCOPE: M3AST_SM.SCOPE := NIL;
  OVERRIDES
    init := Init_With_id;
    IsA_INIT_ID := A_INIT_ID;
    IsA_SCOPE := A_SCOPE;
  END;

  Tcase_id = Previous_View.Tcase_id OBJECT
    vSCOPE: M3AST_SM.SCOPE := NIL;
  OVERRIDES
    init := Init_Tcase_id;
    IsA_SCOPE := A_SCOPE;
  END;
 
  Handler_id = Previous_View.Handler_id OBJECT
    vSCOPE: M3AST_SM.SCOPE := NIL;
  OVERRIDES
    init := Init_Handler_id;
    IsA_SCOPE := A_SCOPE;
  END;
 
  USED_ID = Previous_View.USED_ID OBJECT
    sm_def: M3AST_SM.DEF_ID_UNSET := NIL;
  END;

  TYPED_ID = Previous_View.TYPED_ID OBJECT
    sm_type_spec: M3AST_SM.TYPE_SPEC_UNSET := NIL;
  END;

  Enumeration_type =  Previous_View.Enumeration_type OBJECT
    sm_num_elements := -1;
  END;

  Array_type = Previous_View.Array_type OBJECT
    sm_norm_type: M3AST_AS.Array_type := NIL;
  END;

  Opaque_type = Previous_View.Opaque_type OBJECT
    sm_concrete_type_spec: M3AST_SM.TYPE_SPEC_UNSET := NIL;
    sm_type_spec_s := SeqM3AST_AS_TYPE_SPEC.Null;
  END;

  Named_type = Previous_View.Named_type OBJECT
    sm_type_spec: M3AST_SM.TYPE_SPEC_UNSET := NIL;
  END;

   Subrange_type = Previous_View.Subrange_type OBJECT
     sm_base_type_spec: M3AST_SM.TYPE_SPEC_UNSET := NIL;
   END;

   Procedure_type = Previous_View.Procedure_type OBJECT
     sm_def_id: M3AST_SM.DEF_ID_NULL_UNSET := NIL;
   OVERRIDES
     init := Init_Procedure_type;
   END;

  TYPE_SPEC = Previous_View.TYPE_SPEC OBJECT
     sm_bitsize := -1;
     sm_align := -1;
  END;

  Brand = Previous_View.Brand OBJECT
    sm_brand: M3AST_SM.Exp_value := NIL;
  END;

  Object_type = Previous_View.Object_type OBJECT
    sm_rf_bitsize := -1;
    sm_rf_align := -1;
  END;

  EXP = Previous_View.EXP OBJECT
    sm_exp_type_spec: M3AST_SM.TYPE_SPEC_UNSET := NIL;
    sm_exp_value: M3AST_SM.Exp_value := NIL;
  END; 

  Call = Previous_View.Call OBJECT
    sm_actual_s := SeqM3AST_AS_EXP.Null;
  END;

  NEWCall = Previous_View.NEWCall OBJECT
    sm_norm_actual_s := SeqM3AST_AS_Actual.Null;
  OVERRIDES
    walk := Walk_NEWCall;
    newIter := NewIter_NEWCall;
  END;

  Constructor = Previous_View.Constructor OBJECT
    sm_actual_s := SeqM3AST_AS_RANGE_EXP.Null;
  END;

  Block = Previous_View.Block OBJECT
    vSCOPE: M3AST_SM.SCOPE := NIL;
  OVERRIDES
    init := Init_Block;
    IsA_SCOPE := A_SCOPE;
  END;

REVEAL
  M3AST.NODE <: NODE;
  M3AST_AS.UNIT <: UNIT;
  M3AST_AS.UNIT_WITH_BODY <: UNIT_WITH_BODY;
  M3AST_AS.UNIT_GEN_INS <: UNIT_GEN_INS;
  M3AST_AS.UNIT_ID <: UNIT_ID;
  M3AST_AS.Module <: Module;
  M3AST_AS.Module_id <: Module_id;
  M3AST_AS.Interface_id <: Interface_id;
  M3AST_AS.Proc_id <: Proc_id;
  M3AST_AS.Type_id <: Type_id;
  M3AST_AS.Exc_id <: Exc_id;
  M3AST_AS.METHOD_OVERRIDE_ID <: METHOD_OVERRIDE_ID;
  M3AST_AS.Method_id <: Method_id;
  M3AST_AS.Field_id <: Field_id;
  M3AST_AS.Const_id <: Const_id;
  M3AST_AS.Enum_id <: Enum_id;
  M3AST_AS.Var_id <: Var_id;
  M3AST_AS.F_Value_id <: F_Value_id;
  M3AST_AS.F_Readonly_id <: F_Readonly_id;
  M3AST_AS.For_id <: For_id;
  M3AST_AS.With_id <: With_id;
  M3AST_AS.Tcase_id <: Tcase_id;
  M3AST_AS.Handler_id <: Handler_id;
  M3AST_AS.USED_ID <: USED_ID;
  M3AST_AS.TYPED_ID <: TYPED_ID;
  M3AST_AS.Enumeration_type <: Enumeration_type;
  M3AST_AS.Array_type <: Array_type;
  M3AST_AS.Opaque_type <: Opaque_type;
  M3AST_AS.Named_type <: Named_type;
  M3AST_AS.Subrange_type <: Subrange_type;
  M3AST_AS.Procedure_type <: Procedure_type;
  M3AST_AS.Object_type <: Object_type;
  M3AST_AS.TYPE_SPEC <: TYPE_SPEC;
  M3AST_AS.Brand <: Brand;
  M3AST_AS.EXP <: EXP;
  M3AST_AS.Call <: Call;
  M3AST_AS.NEWCall <: NEWCall;
  M3AST_AS.Constructor <: Constructor;
  M3AST_AS.Block <: Block;

(*--------------------------------------------------------------------------
                         M3AST_SM nodes
--------------------------------------------------------------------------*)

TYPE

  REDEF_ID = M3AST.NODE OBJECT    (* MULTIPLY INHERITED *)
    sm_int_def: M3AST_SM.DEF_ID_NULL_UNSET := NIL;
  OVERRIDES
    init := Init_REDEF_ID;
  END;

  RECOBJ_ID = M3AST.NODE OBJECT  (* MULTIPLY INHERITED *)
    sm_enc_type_spec: M3AST_SM.TYPE_SPEC_UNSET := NIL; 
  END;

   
  INIT_ID = M3AST.NODE OBJECT      (* MULTIPLY INHERITED *)
    sm_init_exp: M3AST_SM.EXP_NULL_UNSET := NIL;
  OVERRIDES
    init := Init_INIT_ID;
  END;

  CCV_ID = M3AST.NODE OBJECT        (* MULTIPLY INHERITED *)
    sm_exp_value: M3AST_SM.Exp_value := NIL;
  END;

  Type_type = M3AST_AS.TYPE_SPEC OBJECT END;
  Any_type = M3AST_AS.TYPE_SPEC OBJECT END;
  Void_type = M3AST_AS.TYPE_SPEC OBJECT END;

  Opaque_type_Revln = M3AST.NODE OBJECT
    sm_type_id: M3AST_SM.DEF_ID_UNSET := NIL;
    sm_concrete_rev: M3AST_AS.TYPE_SPEC := NIL;
    sm_opaque_rev_s := SeqM3AST_AS_TYPE_SPEC.Null;
  END;

  TypeActual = M3AST_AS.EXP OBJECT END;
  
  SCOPE = M3AST.NODE OBJECT        (* MULTIPLY INHERITED *)
    sm_def_id_s := SeqM3AST_AS_DEF_ID.Null;
    sm_enc_scope: M3AST_SM.SCOPE := NIL;
    sm_level := -1;
    sm_mi_node: M3AST_AS.SRC_NODE := NIL;
  END;

REVEAL
  M3AST_SM.Type_type <: Type_type;
  M3AST_SM.Any_type <: Any_type;
  M3AST_SM.Void_type <: Void_type;
  M3AST_SM.Opaque_type_Revln <: Opaque_type_Revln; 
  M3AST_SM.TypeActual <: TypeActual; 
  M3AST_SM.CCV_ID <: CCV_ID;
  M3AST_SM.RECOBJ_ID <: RECOBJ_ID;
  M3AST_SM.REDEF_ID <: REDEF_ID;
  M3AST_SM.INIT_ID <: INIT_ID;
  M3AST_SM.SCOPE <: SCOPE;

(* These are to implement the multiple inheritance strategy *)

PROCEDURE Init_Interface_id(n: Interface_id): AST.NODE RAISES {};
PROCEDURE Init_Module_id(n: Module_id): AST.NODE RAISES {};
PROCEDURE Init_Type_id(n: Type_id): AST.NODE RAISES {};
PROCEDURE Init_F_Value_id(n: F_Value_id): AST.NODE RAISES {};
PROCEDURE Init_F_Readonly_id(n: F_Readonly_id): AST.NODE RAISES {};
PROCEDURE Init_For_id(n: For_id): AST.NODE RAISES {};
PROCEDURE Init_Const_id(n: Const_id): AST.NODE RAISES {};
PROCEDURE Init_Var_id(n: Var_id): AST.NODE RAISES {};
PROCEDURE Init_Proc_id(n: Proc_id): AST.NODE RAISES {};
PROCEDURE Init_Enum_id(n: Enum_id): AST.NODE RAISES {};
PROCEDURE Init_Field_id(n: Field_id): AST.NODE RAISES {};
PROCEDURE Init_METHOD_OVERRIDE_ID(n: METHOD_OVERRIDE_ID): AST.NODE RAISES {};
PROCEDURE Init_Method_id(n: Method_id): AST.NODE RAISES {};
PROCEDURE Init_With_id(n: With_id): AST.NODE RAISES {};
PROCEDURE Init_Exc_id(n: Exc_id): AST.NODE RAISES {};
PROCEDURE Init_Tcase_id(n: Tcase_id): AST.NODE RAISES {};
PROCEDURE Init_Handler_id(n: Handler_id): AST.NODE RAISES {};
PROCEDURE Init_Block(n: Block): AST.NODE RAISES {};

(* These are to ensure that the attributes that can legally
   be NIL, are initialised to the distinguished "UNSET" values *)

PROCEDURE Init_INIT_ID(n: INIT_ID): AST.NODE RAISES {};
PROCEDURE Init_REDEF_ID(n: REDEF_ID): AST.NODE RAISES {};
PROCEDURE Init_Procedure_type(n: Procedure_type): AST.NODE RAISES {};

PROCEDURE NotA_INIT_ID(n: NODE;
                       VAR(*out*) init_id: M3AST_SM.INIT_ID): BOOLEAN;
PROCEDURE NotA_CCV_ID(n: NODE;
                      VAR(*out*) ccv_id: M3AST_SM.CCV_ID): BOOLEAN;
PROCEDURE NotA_RECOBJ_ID(n: NODE;
                         VAR(*out*) recobj_id: M3AST_SM.RECOBJ_ID): BOOLEAN;
PROCEDURE NotA_REDEF_ID(n: NODE;
                        VAR(*out*) init_id: M3AST_SM.REDEF_ID): BOOLEAN;
PROCEDURE NotA_SCOPE(n: NODE;
                     VAR(*out*) scope: M3AST_SM.SCOPE): BOOLEAN;

PROCEDURE A_INIT_ID(n: NODE;
                       VAR(*out*) init_id: M3AST_SM.INIT_ID): BOOLEAN;
PROCEDURE A_CCV_ID(n: NODE;
                      VAR(*out*) ccv_id: M3AST_SM.CCV_ID): BOOLEAN;
PROCEDURE A_RECOBJ_ID(n: NODE;
                         VAR(*out*) recobj_id: M3AST_SM.RECOBJ_ID): BOOLEAN;
PROCEDURE A_REDEF_ID(n: NODE;
                        VAR(*out*) init_id: M3AST_SM.REDEF_ID): BOOLEAN;
PROCEDURE A_SCOPE(n: NODE;
                     VAR(*out*) scope: M3AST_SM.SCOPE): BOOLEAN;


PROCEDURE Walk_NEWCall(n: NEWCall; h: AST_WalkRep.Handle) RAISES ANY;
(* This method implementation walks the "sm_norm_actual_s" attribute instead
of the "as_param_s" attribute. *)

PROCEDURE NewIter_NEWCall(n: NEWCall): AST_Iter.T;
(* As per "walk" for the node iterators *)


(* pass through other node names *)
TYPE
  SRC_NODE = Previous_View.SRC_NODE;
  SRC_NODE_C = Previous_View.SRC_NODE_C;
  LITERAL = Previous_View.LITERAL;
  ID = Previous_View.ID;
  Whitespace = Previous_View.Whitespace;
  Comment = Previous_View.Comment;
  Pragma = Previous_View.Pragma;
  BadChar = Previous_View.BadChar;
  Token = Previous_View.Token;
  
  DEF_ID = Previous_View.DEF_ID;
(*UNIT_ID = Previous_View.UNIT_ID;*)
(*Module_id = Previous_View.Module_id;*)
(*Interface_id = Previous_View.Interface_id;*)
  Interface_AS_id = Previous_View.Interface_AS_id;
  F_Interface_id = Previous_View.F_Interface_id;
(*TYPED_ID = Previous_View.TYPED_ID;*)
  FORMAL_ID = Previous_View.FORMAL_ID;
(*F_Value_id = Previous_View.F_Value_id;*)
  F_Var_id = Previous_View.F_Var_id;
(*F_Readonly_id = Previous_View.F_Readonly_id;*)
(*Type_id = Previous_View.Type_id;*)
(*Const_id = Previous_View.Const_id;*)
(*Var_id = Previous_View.Var_id;*)
(*Proc_id = Previous_View.Proc_id;*)
(*Enum_id = Previous_View.Enum_id;*)
(*METHOD_OVERRIDE_ID = Previous_View.METHOD_OVERRIDE_ID;*)
  Override_id = Previous_View.Override_id;
(*Field_id = Previous_View.Field_id;*)
(*For_id = Previous_View.For_id;*)
(*Handler_id = Previous_View.Handler_id;*)
(*Tcase_id = Previous_View.Tcase_id;*)
(*With_id = Previous_View.With_id;*)
(*Exc_id = Previous_View.Exc_id;*)
(*USED_ID = Previous_View.USED_ID;*)
  Used_interface_id = Previous_View.Used_interface_id;
  Used_def_id = Previous_View.Used_def_id;
  Qual_used_id = Previous_View.Qual_used_id;
  Compilation_Unit = Previous_View.Compilation_Unit;
(*UNIT = Previous_View.UNIT;*)
(*UNIT_WITH_BODY = Previous_View.UNIT_WITH_BODY;*)
  UNIT_GEN_DEF = Previous_View.UNIT_GEN_DEF;
  Interface_gen_def = Previous_View.Interface_gen_def;
  Module_gen_def = Previous_View.Module_gen_def;
  UNIT_NORMAL = Previous_View.UNIT_NORMAL;
  Interface = Previous_View.Interface;
(*Module = Previous_View.Module;*)
(*UNIT_GEN_INS = Previous_View.UNIT_GEN_INS;*)
  Interface_gen_ins = Previous_View.Interface_gen_ins;
  Module_gen_ins = Previous_View.Module_gen_ins;
  Unsafe = Previous_View.Unsafe;
  IMPORTED = Previous_View.IMPORTED;
  Simple_import = Previous_View.Simple_import;
  Import_item = Previous_View.Import_item;
  From_import = Previous_View.From_import;
  DECL_REVL = Previous_View.DECL_REVL;
  DECL = Previous_View.DECL;
  Const_decl_s = Previous_View.Const_decl_s;
  Type_decl_s = Previous_View.Type_decl_s;
  Var_decl_s = Previous_View.Var_decl_s;
  Exc_decl_s = Previous_View.Exc_decl_s;
  Proc_decl = Previous_View.Proc_decl;
  Const_decl = Previous_View.Const_decl;
  TYPE_DECL = Previous_View.TYPE_DECL;
  Subtype_decl = Previous_View.Subtype_decl;
  Concrete_decl = Previous_View.Concrete_decl;
  Var_decl = Previous_View.Var_decl;
  Exc_decl = Previous_View.Exc_decl;
  Revelation_s = Previous_View.Revelation_s;
  REVELATION = Previous_View.REVELATION;
  Subtype_reveal = Previous_View.Subtype_reveal;
  Concrete_reveal = Previous_View.Concrete_reveal;
  EXP_TYPE = Previous_View.EXP_TYPE;
  M3TYPE = Previous_View.M3TYPE;
(*Named_type = Previous_View.Named_type;*)
(*TYPE_SPEC = Previous_View.TYPE_SPEC;*)
  FLOAT_TYPE = Previous_View.FLOAT_TYPE;
  Real_type = Previous_View.Real_type;
  LongReal_type = Previous_View.LongReal_type;
  Extended_type = Previous_View.Extended_type;
  Integer_type = Previous_View.Integer_type;
  Null_type = Previous_View.Null_type;
  RefAny_type = Previous_View.RefAny_type;
  Address_type = Previous_View.Address_type;
  Root_type = Previous_View.Root_type;
  Untraced = Previous_View.Untraced;
  Packed_type = Previous_View.Packed_type;
(*Array_type = Previous_View.Array_type;*)
(*Enumeration_type = Previous_View.Enumeration_type;*)
  Set_type = Previous_View.Set_type;
(*Subrange_type = Previous_View.Subrange_type;*)
  RANGE_EXP = Previous_View.RANGE_EXP;
  Range_EXP = Previous_View.Range_EXP;
  Range = Previous_View.Range;
  Record_type = Previous_View.Record_type;
  Fields = Previous_View.Fields;
  BRANDED_TYPE = Previous_View.BRANDED_TYPE;
(*Brand = Previous_View.Brand;*)
  Ref_type = Previous_View.Ref_type;
(*Object_type = Previous_View.Object_type;*)
  METHOD_OVERRIDE = Previous_View.METHOD_OVERRIDE;
  Method = Previous_View.Method;
  Override = Previous_View.Override;
(*Procedure_type = Previous_View.Procedure_type;*)
  Formal_param = Previous_View.Formal_param;
  RAISEES = Previous_View.RAISEES;
  Raisees_some = Previous_View.Raisees_some;
  Raisees_any = Previous_View.Raisees_any;
(*Opaque_type = Previous_View.Opaque_type;*)
(*EXP = Previous_View.EXP;*)
  NUMERIC_LITERAL = Previous_View.NUMERIC_LITERAL;
  Integer_literal = Previous_View.Integer_literal;
  Real_literal = Previous_View.Real_literal;
  LongReal_literal = Previous_View.LongReal_literal;
  Extended_literal = Previous_View.Extended_literal;
  Char_literal = Previous_View.Char_literal;
  Text_literal = Previous_View.Text_literal;
  Nil_literal = Previous_View.Nil_literal;
  Exp_used_id = Previous_View.Exp_used_id;
(*Call = Previous_View.Call;*)
(*NEWCall = Previous_View.NEWCall;*)
  Actual = Previous_View.Actual;
  Index = Previous_View.Index;
(*Constructor = Previous_View.Constructor;*)
  Propagate = Previous_View.Propagate;
  CONS_ELEM = Previous_View.CONS_ELEM;
  RANGE_EXP_elem = Previous_View.RANGE_EXP_elem;
  Actual_elem = Previous_View.Actual_elem;
  BINARY = Previous_View.BINARY;
  Plus = Previous_View.Plus;
  Minus = Previous_View.Minus;
  Times = Previous_View.Times;
  Rdiv = Previous_View.Rdiv;
  Textcat = Previous_View.Textcat;
  Div = Previous_View.Div;
  Mod = Previous_View.Mod;
  Eq = Previous_View.Eq;
  Ne = Previous_View.Ne;
  Gt = Previous_View.Gt;
  Lt = Previous_View.Lt;
  Ge = Previous_View.Ge;
  Le = Previous_View.Le;
  And = Previous_View.And;
  Or = Previous_View.Or;
  In = Previous_View.In;
  Select = Previous_View.Select;
  UNARY = Previous_View.UNARY;
  Not = Previous_View.Not;
  Unaryplus = Previous_View.Unaryplus;
  Unaryminus = Previous_View.Unaryminus;
  Deref = Previous_View.Deref;
  STM = Previous_View.STM;
  STM_WSS = Previous_View.STM_WSS;
  SUBSTM_WSS = Previous_View.SUBSTM_WSS;
  Assign_st = Previous_View.Assign_st;
  Call_st = Previous_View.Call_st;
  Case_st = Previous_View.Case_st;
  Case = Previous_View.Case;
  Else_stm = Previous_View.Else_stm;
  Eval_st = Previous_View.Eval_st;
  Exit_st = Previous_View.Exit_st;
  Raise_st = Previous_View.Raise_st;
  Typecase_st = Previous_View.Typecase_st;
  Tcase = Previous_View.Tcase;
  Handler = Previous_View.Handler;
  Return_st = Previous_View.Return_st;
  For_st = Previous_View.For_st;
  By = Previous_View.By;
  If_st = Previous_View.If_st;
  Elsif = Previous_View.Elsif;
  Lock_st = Previous_View.Lock_st;
  Loop_st = Previous_View.Loop_st;
  Repeat_st = Previous_View.Repeat_st;
  Try_st = Previous_View.Try_st;
  TRY_TAIL = Previous_View.TRY_TAIL;
  Try_except = Previous_View.Try_except;
  Try_finally = Previous_View.Try_finally;
  While_st = Previous_View.While_st;
  With_st = Previous_View.With_st;
  Binding = Previous_View.Binding;
(*Block = Previous_View.Block;*)
  Bad_EXP = Previous_View.Bad_EXP;
  Bad_M3TYPE = Previous_View.Bad_M3TYPE;
  Bad_STM = Previous_View.Bad_STM;

END M3AST_SM_F.

