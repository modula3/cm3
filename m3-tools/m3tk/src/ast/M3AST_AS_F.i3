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

INTERFACE M3AST_AS_F;

IMPORT AST_LAST, M3AST, M3AST_LX, M3AST_AS, M3AST_PG,
       M3CPragma, M3CComment;
IMPORT
    SeqM3AST_AS_IMPORTED, SeqM3AST_AS_Import_item, 
    SeqM3AST_AS_Used_interface_id, SeqM3AST_AS_Used_def_id,
    SeqM3AST_AS_REVELATION, SeqM3AST_AS_DECL_REVL,
    SeqM3AST_AS_Const_decl, SeqM3AST_AS_TYPE_DECL,
    SeqM3AST_AS_Var_decl, SeqM3AST_AS_Exc_decl,
    SeqM3AST_AS_F_Interface_id,
    SeqM3AST_AS_Var_id, 
    SeqM3AST_AS_Enum_id, SeqM3AST_AS_Field_id,
    SeqM3AST_AS_FORMAL_ID, SeqM3AST_AS_Qual_used_id,
    SeqM3AST_AS_Fields, SeqM3AST_AS_Method, SeqM3AST_AS_Override,
    SeqM3AST_AS_M3TYPE,
    SeqM3AST_AS_Formal_param, SeqM3AST_AS_CONS_ELEM,
    SeqM3AST_AS_EXP, SeqM3AST_AS_Actual,
    SeqM3AST_AS_Case, SeqM3AST_AS_STM,
    SeqM3AST_AS_Elsif, SeqM3AST_AS_Tcase,
    SeqM3AST_AS_Handler, SeqM3AST_AS_Binding,
    SeqM3AST_AS_RANGE_EXP;

IMPORT M3AST_LX_F AS Previous_View;

(* In this view, syntactic nodes are revealed to be object types with
fields whose values are either lexical entities or references to other
nodes in the AST.  *)

TYPE
  NODE = AST_LAST.NODE OBJECT
  METHODS
    IsA_USED_ID(VAR (*out*) used_id: M3AST_AS.USED_ID
               ): BOOLEAN := NotA_USED_ID;
    IsA_ID(VAR (*out*) id: M3AST_AS.ID): BOOLEAN := NotA_ID;
  END;

REVEAL
  M3AST.NODE <: NODE;

TYPE
  ID = Previous_View.ID OBJECT
  OVERRIDES
    IsA_ID := A_ID;
  END;

  DEF_ID = M3AST_AS.ID OBJECT END;
  UNIT_ID = M3AST_AS.DEF_ID OBJECT END;
  Module_id =  M3AST_AS.UNIT_ID OBJECT END;
  Interface_id = M3AST_AS.UNIT_ID OBJECT END;
  Interface_AS_id = M3AST_AS.DEF_ID OBJECT END;
  F_Interface_id = M3AST_AS.DEF_ID OBJECT END;

  TYPED_ID = M3AST_AS.DEF_ID OBJECT END;
  FORMAL_ID = M3AST_AS.TYPED_ID OBJECT END;
  F_Value_id = M3AST_AS.FORMAL_ID OBJECT END;
  F_Var_id = M3AST_AS.FORMAL_ID OBJECT END;
  F_Readonly_id = M3AST_AS.FORMAL_ID OBJECT END;

  Type_id = M3AST_AS.TYPED_ID OBJECT END;
  Const_id = M3AST_AS.TYPED_ID OBJECT END;
  Exc_id = M3AST_AS.TYPED_ID OBJECT END;
  Var_id = M3AST_AS.TYPED_ID OBJECT END;
  Proc_id = M3AST_AS.TYPED_ID OBJECT END;
  Enum_id = M3AST_AS.TYPED_ID OBJECT END;
  METHOD_OVERRIDE_ID = M3AST_AS.TYPED_ID OBJECT END;
  Method_id = M3AST_AS.METHOD_OVERRIDE_ID OBJECT END;
  Override_id = M3AST_AS.METHOD_OVERRIDE_ID OBJECT END;
  Field_id = M3AST_AS.TYPED_ID OBJECT END;
  For_id = M3AST_AS.TYPED_ID OBJECT END;
  Handler_id = M3AST_AS.TYPED_ID OBJECT END;
  Tcase_id = M3AST_AS.TYPED_ID OBJECT END;
  With_id = M3AST_AS.TYPED_ID OBJECT END;

  USED_ID = M3AST_AS.ID OBJECT
  OVERRIDES
    IsA_USED_ID := A_USED_ID;
  END;
  Used_interface_id =  M3AST_AS.USED_ID OBJECT END;
  Used_def_id =  M3AST_AS.USED_ID OBJECT END;

  Qual_used_id = M3AST_AS.SRC_NODE_C OBJECT
    as_intf_id: M3AST_AS.Used_interface_id_NULL := NIL;
    as_id: M3AST_AS.Used_def_id := NIL;
  END;

REVEAL
  M3AST_AS.ID <: ID;
  M3AST_AS.DEF_ID <: DEF_ID;
  M3AST_AS.UNIT_ID <: UNIT_ID;
  M3AST_AS.Module_id <: Module_id;
  M3AST_AS.Interface_id <: Interface_id;
  M3AST_AS.Interface_AS_id <: Interface_AS_id;
  M3AST_AS.F_Interface_id <: F_Interface_id;

  M3AST_AS.FORMAL_ID <: FORMAL_ID;
  M3AST_AS.F_Value_id <: F_Value_id;
  M3AST_AS.F_Var_id <: F_Var_id;
  M3AST_AS.F_Readonly_id <: F_Readonly_id;

  M3AST_AS.Type_id <: Type_id;
  M3AST_AS.Const_id <: Const_id;
  M3AST_AS.Exc_id <: Exc_id;
  M3AST_AS.Var_id <: Var_id;
  M3AST_AS.Proc_id <: Proc_id;
  M3AST_AS.Enum_id <: Enum_id;
  M3AST_AS.METHOD_OVERRIDE_ID <: METHOD_OVERRIDE_ID;
  M3AST_AS.Method_id <: Method_id;
  M3AST_AS.Override_id <: Override_id;
  M3AST_AS.Field_id <: Field_id;
  M3AST_AS.For_id <: For_id;
  M3AST_AS.Handler_id <: Handler_id;
  M3AST_AS.Tcase_id <: Tcase_id;
  M3AST_AS.With_id <: With_id;

  M3AST_AS.USED_ID <: USED_ID;
  M3AST_AS.Used_interface_id <: Used_interface_id;
  M3AST_AS.Used_def_id <: Used_def_id;

  M3AST_AS.Qual_used_id <: Qual_used_id;

TYPE
  
  Compilation_Unit =  M3AST_AS.SRC_NODE_C OBJECT
    as_root: M3AST_AS.UNIT := NIL;
    lx_pragmas: M3CPragma.Store := NIL;
    lx_comments: M3CComment.Store := NIL;
  END;
  
  Unsafe = M3AST_AS.SRC_NODE_C OBJECT END;

  UNIT = M3AST_AS.SRC_NODE_C OBJECT
    as_id: M3AST_AS.UNIT_ID := NIL;
  END;

  UNIT_WITH_BODY = M3AST_AS.UNIT OBJECT
    as_import_s := SeqM3AST_AS_IMPORTED.Null;
    as_block: M3AST_AS.Block := NIL;
  END;

  UNIT_GEN_DEF = M3AST_AS.UNIT_WITH_BODY OBJECT
    as_id_s := SeqM3AST_AS_F_Interface_id.Null;
  END;

  Interface_gen_def = M3AST_AS.UNIT_GEN_DEF OBJECT
    vEXTERNAL_DECL: M3AST_PG.EXTERNAL_DECL := NIL;
  END;

 Module_gen_def = M3AST_AS.UNIT_GEN_DEF OBJECT END;

  UNIT_NORMAL = M3AST_AS.UNIT_WITH_BODY OBJECT
    as_unsafe: M3AST_AS.Unsafe_NULL := NIL;
  END;
  
  Interface =  M3AST_AS.UNIT_NORMAL OBJECT 
    vEXTERNAL_DECL: M3AST_PG.EXTERNAL_DECL := NIL;
  END;

  Module =  M3AST_AS.UNIT_NORMAL OBJECT
    as_export_s := SeqM3AST_AS_Used_interface_id.Null;
  END;
  
  UNIT_GEN_INS = M3AST_AS.UNIT OBJECT
    as_unsafe: M3AST_AS.Unsafe_NULL := NIL;
    as_gen_id: M3AST_AS.Used_interface_id := NIL;
    as_id_s := SeqM3AST_AS_Used_interface_id.Null    
  END;
  
  Interface_gen_ins = M3AST_AS.UNIT_GEN_INS OBJECT END;

  Module_gen_ins = M3AST_AS.UNIT_GEN_INS OBJECT
    as_export_s := SeqM3AST_AS_Used_interface_id.Null;
  END;

  IMPORTED =  M3AST_AS.SRC_NODE_C OBJECT END;

  Simple_import =   M3AST_AS.IMPORTED OBJECT 
    as_import_item_s := SeqM3AST_AS_Import_item.Null;
  END;

  Import_item = M3AST_AS.SRC_NODE_C OBJECT
    as_intf_id: M3AST_AS.Used_interface_id := NIL;
    as_id: M3AST_AS.Interface_AS_id := NIL;    
  END;

  From_import =    M3AST_AS.IMPORTED OBJECT
    as_intf_id: M3AST_AS.Used_interface_id := NIL;
    as_id_s := SeqM3AST_AS_Used_def_id.Null;
  END;

  DECL_REVL = M3AST_AS.SRC_NODE_C OBJECT END;
  DECL = M3AST_AS.DECL_REVL OBJECT 
    vEXTERNAL_DECL: M3AST_PG.EXTERNAL_DECL := NIL;
  END;

  Revelation_s = M3AST_AS.DECL_REVL OBJECT
    as_reveal_s := SeqM3AST_AS_REVELATION.Null;
  END;

  Const_decl_s = M3AST_AS.DECL OBJECT
    as_const_decl_s := SeqM3AST_AS_Const_decl.Null;
  END;

  Type_decl_s = M3AST_AS.DECL OBJECT
    as_type_decl_s := SeqM3AST_AS_TYPE_DECL.Null;
  END;

  Var_decl_s = M3AST_AS.DECL OBJECT
    as_var_decl_s := SeqM3AST_AS_Var_decl.Null;
  END;

  Exc_decl_s = M3AST_AS.DECL OBJECT
    as_exc_decl_s := SeqM3AST_AS_Exc_decl.Null;
  END;

  Const_decl = M3AST_AS.SRC_NODE_C OBJECT
    as_id: M3AST_AS.Const_id := NIL;
    as_type: M3AST_AS.M3TYPE_NULL := NIL;
    as_exp: M3AST_AS.EXP := NIL;
  END;

  TYPE_DECL = M3AST_AS.SRC_NODE_C OBJECT
    as_id: M3AST_AS.Type_id := NIL;
    as_type: M3AST_AS.M3TYPE := NIL;
  END;
 
  Subtype_decl = M3AST_AS.TYPE_DECL OBJECT END;  
  Concrete_decl = M3AST_AS.TYPE_DECL OBJECT END;  

  Var_decl = M3AST_AS.SRC_NODE_C OBJECT
    as_id_s := SeqM3AST_AS_Var_id.Null;
    as_type: M3AST_AS.M3TYPE_NULL := NIL;
    as_default: M3AST_AS.EXP_NULL := NIL;
  END;

  Exc_decl = M3AST_AS.SRC_NODE_C OBJECT
    as_id: M3AST_AS.Exc_id := NIL;
    as_type: M3AST_AS.M3TYPE_NULL := NIL;
  END;

  Proc_decl = M3AST_AS.DECL OBJECT
    as_id: M3AST_AS.Proc_id := NIL;
    pg_inline: M3AST_PG.Inline_NULL := NIL;
    as_type: M3AST_AS.Procedure_type := NIL;
    as_body: M3AST_AS.Block_NULL := NIL;
  END;

  REVELATION = M3AST_AS.SRC_NODE_C OBJECT 
    as_qual_id: M3AST_AS.Qual_used_id := NIL;
    as_type: M3AST_AS.M3TYPE := NIL;
  END;
  Subtype_reveal = M3AST_AS.REVELATION OBJECT END;
  Concrete_reveal = M3AST_AS.REVELATION OBJECT END;

REVEAL
  M3AST_AS.Compilation_Unit <: Compilation_Unit;
  M3AST_AS.UNIT <: UNIT;
  M3AST_AS.UNIT_WITH_BODY <: UNIT_WITH_BODY;
  M3AST_AS.UNIT_GEN_DEF <: UNIT_GEN_DEF;
  M3AST_AS.UNIT_NORMAL <: UNIT_NORMAL;
  M3AST_AS.UNIT_GEN_INS <: UNIT_GEN_INS;
  M3AST_AS.Interface_gen_def <: Interface_gen_def;
  M3AST_AS.Module_gen_def <: Module_gen_def;
  M3AST_AS.Interface <: Interface;
  M3AST_AS.Module <: Module;
  M3AST_AS.Interface_gen_ins <: Interface_gen_ins;
  M3AST_AS.Module_gen_ins <: Module_gen_ins;
  M3AST_AS.Unsafe <: Unsafe;
  M3AST_AS.IMPORTED <: IMPORTED;
  M3AST_AS.Import_item <: Import_item;
  M3AST_AS.Simple_import <: Simple_import;
  M3AST_AS.From_import <: From_import;
  M3AST_AS.DECL_REVL <: DECL_REVL;
  M3AST_AS.DECL <: DECL;
  M3AST_AS.Revelation_s <: Revelation_s;
  M3AST_AS.Const_decl_s <: Const_decl_s;
  M3AST_AS.Type_decl_s <: Type_decl_s;
  M3AST_AS.Var_decl_s <: Var_decl_s;
  M3AST_AS.Exc_decl_s <: Exc_decl_s;
  M3AST_AS.Const_decl <: Const_decl;
  M3AST_AS.Var_decl <: Var_decl;
  M3AST_AS.Exc_decl <: Exc_decl;
  M3AST_AS.TYPE_DECL <: TYPE_DECL;
  M3AST_AS.Subtype_decl <: Subtype_decl;
  M3AST_AS.Concrete_decl <: Concrete_decl;
  M3AST_AS.Proc_decl <: Proc_decl;
  M3AST_AS.REVELATION <: REVELATION;
  M3AST_AS.Subtype_reveal <: Subtype_reveal;
  M3AST_AS.Concrete_reveal <: Concrete_reveal;

TYPE
  M3TYPE = M3AST_AS.EXP_TYPE OBJECT END;
  Named_type = M3AST_AS.M3TYPE OBJECT
    as_qual_id: M3AST_AS.Qual_used_id := NIL;
  END;
  
  TYPE_SPEC = M3AST_AS.M3TYPE OBJECT END;

  Integer_type = M3AST_AS.TYPE_SPEC OBJECT END;
  FLOAT_TYPE = M3AST_AS.TYPE_SPEC OBJECT END;
  Real_type = M3AST_AS.FLOAT_TYPE OBJECT END;
  LongReal_type = M3AST_AS.FLOAT_TYPE OBJECT END;
  Extended_type = M3AST_AS.FLOAT_TYPE OBJECT END;
  Null_type = M3AST_AS.TYPE_SPEC OBJECT END;
  RefAny_type = M3AST_AS.TYPE_SPEC OBJECT END;
  Address_type = M3AST_AS.TYPE_SPEC OBJECT END;
  Root_type = M3AST_AS.TYPE_SPEC OBJECT 
    as_trace_mode: M3AST_AS.Untraced_NULL := NIL;
  END;

  Array_type = M3AST_AS.TYPE_SPEC OBJECT
    as_indextype_s := SeqM3AST_AS_M3TYPE.Null;
    as_elementtype: M3AST_AS.M3TYPE := NIL;
  END;

  Enumeration_type = M3AST_AS.TYPE_SPEC OBJECT
    as_id_s := SeqM3AST_AS_Enum_id.Null;
  END;

  Set_type = M3AST_AS.TYPE_SPEC OBJECT
    as_type: M3AST_AS.M3TYPE := NIL;
  END;

  Subrange_type  = M3AST_AS.TYPE_SPEC OBJECT
    as_range: M3AST_AS.Range := NIL;
  END;

  Packed_type = M3AST_AS.TYPE_SPEC OBJECT
    as_exp: M3AST_AS.EXP := NIL;
    as_type: M3AST_AS.M3TYPE := NIL;
  END;

  Record_type = M3AST_AS.TYPE_SPEC OBJECT
    as_fields_s := SeqM3AST_AS_Fields.Null;
  END;

  BRANDED_TYPE = M3AST_AS.TYPE_SPEC OBJECT
    as_brand: M3AST_AS.Brand_NULL := NIL;
  END;

  Brand = M3AST_AS.SRC_NODE_C OBJECT 
    as_exp: M3AST_AS.EXP_NULL := NIL;
  END;
  
  Untraced = M3AST_AS.SRC_NODE_C OBJECT END;

  Ref_type = M3AST_AS.BRANDED_TYPE OBJECT
   as_trace_mode: M3AST_AS.Untraced_NULL := NIL;
   as_type: M3AST_AS.M3TYPE := NIL; 
  END;

  Object_type = M3AST_AS.BRANDED_TYPE OBJECT
    as_ancestor: M3AST_AS.M3TYPE_NULL := NIL; (* will be void if Untraced *)
    as_fields_s := SeqM3AST_AS_Fields.Null;
    as_method_s := SeqM3AST_AS_Method.Null;
    as_override_s := SeqM3AST_AS_Override.Null;
  END;
  
  Fields = M3AST_AS.SRC_NODE_C OBJECT
    as_id_s := SeqM3AST_AS_Field_id.Null;
    as_type: M3AST_AS.M3TYPE_NULL := NIL;
    as_default: M3AST_AS.EXP_NULL := NIL;
  END;

  METHOD_OVERRIDE = M3AST_AS.SRC_NODE_C OBJECT
    as_id: M3AST_AS.METHOD_OVERRIDE_ID := NIL;
    as_default: M3AST_AS.EXP_NULL := NIL; (* not NIL for Override *)
  END;

  Method = M3AST_AS.METHOD_OVERRIDE OBJECT
    as_type: M3AST_AS.Procedure_type := NIL;
  END;

  Override = M3AST_AS.METHOD_OVERRIDE OBJECT
  END;

  Procedure_type = M3AST_AS.TYPE_SPEC OBJECT
    as_formal_param_s := SeqM3AST_AS_Formal_param.Null;
    as_result_type: M3AST_AS.M3TYPE_NULL := NIL;
    as_raises: M3AST_AS.RAISEES_NULL := NIL;
  END;

  Formal_param = M3AST_AS.SRC_NODE_C OBJECT
    as_id_s := SeqM3AST_AS_FORMAL_ID.Null;
    as_formal_type: M3AST_AS.M3TYPE_NULL := NIL;
    as_default: M3AST_AS.EXP_NULL := NIL;
  END;

  RAISEES = M3AST_AS.SRC_NODE_C OBJECT END;

  Raisees_some = M3AST_AS.RAISEES OBJECT
    as_raisees_s := SeqM3AST_AS_Qual_used_id.Null; 
  END;

  Raisees_any = M3AST_AS.RAISEES OBJECT END;

  RANGE_EXP = M3AST_AS.SRC_NODE_C OBJECT END;
 
  Range_EXP = M3AST_AS.RANGE_EXP OBJECT
    as_exp: M3AST_AS.EXP := NIL;
  END;

  Range = M3AST_AS.RANGE_EXP OBJECT
    as_exp1, as_exp2: M3AST_AS.EXP := NIL;
  END;

  Opaque_type = M3AST_AS.TYPE_SPEC OBJECT
    as_type: M3AST_AS.M3TYPE := NIL;
  END;

REVEAL
  M3AST_AS.M3TYPE <: M3TYPE;
  M3AST_AS.Named_type <: Named_type;
  M3AST_AS.TYPE_SPEC <: TYPE_SPEC;
  M3AST_AS.Integer_type <: Integer_type;
  M3AST_AS.FLOAT_TYPE <: FLOAT_TYPE;
  M3AST_AS.Real_type <: Real_type;
  M3AST_AS.LongReal_type <: LongReal_type;
  M3AST_AS.Extended_type <: Extended_type;
  M3AST_AS.Null_type <: Null_type;
  M3AST_AS.RefAny_type <: RefAny_type;
  M3AST_AS.Address_type <: Address_type;
  M3AST_AS.Root_type <: Root_type;
  M3AST_AS.Array_type <: Array_type;
  M3AST_AS.Enumeration_type <: Enumeration_type;
  M3AST_AS.Set_type <: Set_type;
  M3AST_AS.Subrange_type <: Subrange_type;
  M3AST_AS.Packed_type <: Packed_type;
  M3AST_AS.Record_type <: Record_type;
  M3AST_AS.BRANDED_TYPE <: BRANDED_TYPE;
  M3AST_AS.Brand <: Brand;
  M3AST_AS.Untraced <: Untraced;
  M3AST_AS.Ref_type <: Ref_type;
  M3AST_AS.Object_type <: Object_type;
  M3AST_AS.Fields <: Fields;
  M3AST_AS.METHOD_OVERRIDE <: METHOD_OVERRIDE;
  M3AST_AS.Method <: Method;
  M3AST_AS.Override <: Override;
  M3AST_AS.Procedure_type <: Procedure_type;
  M3AST_AS.Formal_param <: Formal_param;
  M3AST_AS.RAISEES <: RAISEES;
  M3AST_AS.Raisees_some <: Raisees_some;
  M3AST_AS.Raisees_any <: Raisees_any;
  M3AST_AS.RANGE_EXP <: RANGE_EXP;
  M3AST_AS.Range_EXP <: Range_EXP;
  M3AST_AS.Range <: Range;
  M3AST_AS.Opaque_type <: Opaque_type;

TYPE
  EXP_TYPE = M3AST_AS.SRC_NODE_C OBJECT END;

  EXP = M3AST_AS.EXP_TYPE OBJECT END;  

  LITERAL = M3AST_AS.EXP OBJECT
    lx_litrep: M3AST_LX.Literal_rep := NIL;
  END;

  NUMERIC_LITERAL = M3AST_LX.LITERAL OBJECT
  END;

  Integer_literal = M3AST_AS.NUMERIC_LITERAL OBJECT END;
  Real_literal = M3AST_AS.NUMERIC_LITERAL OBJECT END;
  LongReal_literal = M3AST_AS.NUMERIC_LITERAL OBJECT END;
  Extended_literal = M3AST_AS.NUMERIC_LITERAL OBJECT END;

  Nil_literal = M3AST_LX.LITERAL OBJECT END;

  Char_literal = M3AST_LX.LITERAL OBJECT
  END;

  Text_literal = M3AST_LX.LITERAL OBJECT
  END;

  Exp_used_id = M3AST_AS.EXP OBJECT
    vUSED_ID: M3AST_AS.USED_ID := NIL  (* MULTIPLE INHERITANCE *)
  OVERRIDES
    IsA_ID := A_Exp_ID;
    IsA_USED_ID := A_Exp_USED_ID;
  END;

  Constructor = M3AST_AS.EXP OBJECT
    as_type: M3AST_AS.M3TYPE := NIL;
    as_element_s := SeqM3AST_AS_CONS_ELEM.Null;
    as_propagate: M3AST_AS.Propagate_NULL := NIL;
  END;

  CONS_ELEM = M3AST_AS.SRC_NODE_C OBJECT END;

  RANGE_EXP_elem = M3AST_AS.CONS_ELEM OBJECT
    as_range_exp: M3AST_AS.RANGE_EXP := NIL; 
  END;
  Actual_elem = M3AST_AS.CONS_ELEM OBJECT
    as_actual: M3AST_AS.Actual := NIL;
  END;

  Propagate = M3AST_AS.SRC_NODE_C OBJECT END;

  BINARY = M3AST_AS.EXP OBJECT
    as_exp1: M3AST_AS.EXP := NIL;
    as_exp2: M3AST_AS.EXP := NIL;
  END;

  Plus = M3AST_AS.BINARY OBJECT END;
  Minus = M3AST_AS.BINARY OBJECT END;
  Times = M3AST_AS.BINARY OBJECT END;
  Rdiv = M3AST_AS.BINARY OBJECT END;
  Textcat = M3AST_AS.BINARY OBJECT END;
  Div = M3AST_AS.BINARY OBJECT END;
  Mod = M3AST_AS.BINARY OBJECT END;
  Eq = M3AST_AS.BINARY OBJECT END;
  Ne = M3AST_AS.BINARY OBJECT END;
  Gt = M3AST_AS.BINARY OBJECT END;
  Lt = M3AST_AS.BINARY OBJECT END;
  Ge = M3AST_AS.BINARY OBJECT END;
  Le = M3AST_AS.BINARY OBJECT END;
  And = M3AST_AS.BINARY OBJECT END;
  Or = M3AST_AS.BINARY OBJECT END;
  In = M3AST_AS.BINARY OBJECT END;

  UNARY = M3AST_AS.EXP OBJECT
    as_exp: M3AST_AS.EXP := NIL;
  END;

  Not = M3AST_AS.UNARY OBJECT END;
  Unaryplus = M3AST_AS.UNARY OBJECT END;
  Unaryminus = M3AST_AS.UNARY OBJECT END;
  Deref = M3AST_AS.UNARY OBJECT END;

  Select = M3AST_AS.EXP OBJECT
    as_exp: M3AST_AS.EXP;
    as_id: M3AST_AS.Exp_used_id;
  END;

  Call = M3AST_AS.EXP OBJECT 
    as_callexp: M3AST_AS.EXP := NIL;
    as_param_s := SeqM3AST_AS_Actual.Null;
  END;

  NEWCall = M3AST_AS.Call OBJECT END;
  (* syntactically identical to a Call, but separated to allow the
     desugaring of method bindings to be represented later as an
     attribute. *)

  Actual = M3AST_AS.SRC_NODE_C OBJECT 
    as_id: M3AST_AS.EXP_NULL := NIL;
    as_exp_type: M3AST_AS.EXP_TYPE := NIL;
  END;

  Index = M3AST_AS.EXP OBJECT
    as_array: M3AST_AS.EXP := NIL;
    as_exp_s := SeqM3AST_AS_EXP.Null;
  END;

REVEAL
  M3AST_AS.EXP_TYPE <: EXP_TYPE;
  M3AST_AS.EXP <: EXP;
  M3AST_AS.Exp_used_id <: Exp_used_id;
  M3AST_LX.LITERAL <: LITERAL;
  M3AST_AS.NUMERIC_LITERAL <: NUMERIC_LITERAL;
  M3AST_AS.Integer_literal <: Integer_literal;
  M3AST_AS.Real_literal <: Real_literal;
  M3AST_AS.LongReal_literal <: LongReal_literal;
  M3AST_AS.Extended_literal <: Extended_literal;
  M3AST_AS.Nil_literal <: Nil_literal; 
  M3AST_AS.Char_literal <: Char_literal;
  M3AST_AS.Text_literal <: Text_literal;
  M3AST_AS.Constructor <: Constructor;
  M3AST_AS.CONS_ELEM <: CONS_ELEM;
  M3AST_AS.RANGE_EXP_elem <: RANGE_EXP_elem;
  M3AST_AS.Actual_elem <: Actual_elem;
  M3AST_AS.Propagate <: Propagate;
  M3AST_AS.BINARY <: BINARY;
  M3AST_AS.Plus <: Plus;
  M3AST_AS.Minus <: Minus;
  M3AST_AS.Times <: Times;
  M3AST_AS.Rdiv <: Rdiv;
  M3AST_AS.Textcat <: Textcat;
  M3AST_AS.Div <: Div;
  M3AST_AS.Mod <: Mod;
  M3AST_AS.Eq <: Eq;
  M3AST_AS.Ne <: Ne;
  M3AST_AS.Gt <: Gt;
  M3AST_AS.Lt <: Lt;
  M3AST_AS.Ge <: Ge;
  M3AST_AS.And <: And;
  M3AST_AS.Or <: Or;
  M3AST_AS.In <: In;
  M3AST_AS.Select <: Select;
  M3AST_AS.UNARY <: UNARY;
  M3AST_AS.Not <: Not;
  M3AST_AS.Unaryplus <: Unaryplus;
  M3AST_AS.Unaryminus <: Unaryminus;
  M3AST_AS.Deref <: Deref;
  M3AST_AS.Call <: Call; 
  M3AST_AS.NEWCall <: NEWCall; 
  M3AST_AS.Actual <: Actual; 
  M3AST_AS.Index <: Index;
  
TYPE
  STM = M3AST_AS.SRC_NODE_C OBJECT END;
  STM_WSS = M3AST_AS.STM OBJECT
    as_stm_s := SeqM3AST_AS_STM.Null;
  END;
  SUBSTM_WSS = M3AST_AS.SRC_NODE_C OBJECT
    as_stm_s := SeqM3AST_AS_STM.Null;
  END;

  Assign_st = M3AST_AS.STM OBJECT 
    as_lhs_exp: M3AST_AS.EXP := NIL;
    as_rhs_exp: M3AST_AS.EXP := NIL;
  END;                                                                       

  Call_st = M3AST_AS.STM OBJECT
    as_call: M3AST_AS.Call := NIL;
  END;

  Case_st = M3AST_AS.STM OBJECT
    as_exp: M3AST_AS.EXP := NIL;
    as_case_s := SeqM3AST_AS_Case.Null;
    as_else: M3AST_AS.Else_stm_NULL := NIL;
  END;

  Case = M3AST_AS.SUBSTM_WSS OBJECT
    as_case_label_s := SeqM3AST_AS_RANGE_EXP.Null;
  END;

  Else_stm = M3AST_AS.SUBSTM_WSS OBJECT END;

  Exit_st = M3AST_AS.STM OBJECT END;

  Eval_st = M3AST_AS.STM OBJECT
    as_exp: M3AST_AS.EXP := NIL;
  END;

  For_st = M3AST_AS.STM_WSS OBJECT
    as_id: M3AST_AS.For_id := NIL;
    as_from: M3AST_AS.EXP := NIL;
    as_to: M3AST_AS.EXP := NIL;
    as_by: M3AST_AS.By_NULL := NIL;
  END;

  By = M3AST_AS.SRC_NODE_C OBJECT
    as_exp: M3AST_AS.EXP := NIL;
  END;

  If_st = M3AST_AS.STM_WSS OBJECT
    as_exp: M3AST_AS.EXP := NIL;
    as_elsif_s := SeqM3AST_AS_Elsif.Null;
    as_else: M3AST_AS.Else_stm_NULL := NIL;
  END;

  Elsif = M3AST_AS.SUBSTM_WSS OBJECT
    as_exp: M3AST_AS.EXP := NIL;
  END;    

  Lock_st = M3AST_AS.STM_WSS OBJECT
    as_exp: M3AST_AS.EXP := NIL;
  END;

  Loop_st = M3AST_AS.STM_WSS OBJECT
  END;

  Return_st = M3AST_AS.STM OBJECT
    as_exp: M3AST_AS.EXP_NULL := NIL;
  END;

  Repeat_st = M3AST_AS.STM_WSS OBJECT
    as_exp: M3AST_AS.EXP := NIL;
  END;

  Raise_st = M3AST_AS.STM OBJECT
    as_qual_id: M3AST_AS.Qual_used_id := NIL;
    as_exp_void: M3AST_AS.EXP_NULL := NIL;
  END;

  Try_st = M3AST_AS.STM_WSS OBJECT
    as_try_tail: M3AST_AS.TRY_TAIL := NIL;
  END;

  TRY_TAIL = M3AST_AS.SUBSTM_WSS OBJECT END;
  Try_finally = M3AST_AS.TRY_TAIL OBJECT END;

  Try_except =  M3AST_AS.TRY_TAIL OBJECT
    as_handler_s := SeqM3AST_AS_Handler.Null;
    as_else: M3AST_AS.Else_stm_NULL := NIL;
  END;

  Handler = M3AST_AS.SUBSTM_WSS OBJECT
    as_qual_id_s := SeqM3AST_AS_Qual_used_id.Null;
    as_id: M3AST_AS.Handler_id_NULL := NIL;  
  END;

  Typecase_st = M3AST_AS.STM OBJECT
    as_exp: M3AST_AS.EXP := NIL;
    as_tcase_s := SeqM3AST_AS_Tcase.Null;
    as_else: M3AST_AS.Else_stm_NULL := NIL;    
  END;

  Tcase = M3AST_AS.SUBSTM_WSS OBJECT
    as_type_s := SeqM3AST_AS_M3TYPE.Null;
    as_id: M3AST_AS.Tcase_id_NULL := NIL;
  END;

  While_st = M3AST_AS.STM_WSS OBJECT
    as_exp: M3AST_AS.EXP := NIL;
  END;

  With_st = M3AST_AS.STM_WSS OBJECT
    as_binding_s := SeqM3AST_AS_Binding.Null;
  END;

  Binding = M3AST_AS.SRC_NODE_C OBJECT
    as_id: M3AST_AS.With_id := NIL;
    as_exp: M3AST_AS.EXP := NIL;    
  END;

  Block = M3AST_AS.STM_WSS OBJECT
    as_decl_s := SeqM3AST_AS_DECL_REVL.Null;
  END;

REVEAL
  M3AST_AS.STM <: STM;
  M3AST_AS.STM_WSS <: STM_WSS;
  M3AST_AS.SUBSTM_WSS <: SUBSTM_WSS;
  M3AST_AS.Assign_st <: Assign_st; 
  M3AST_AS.Call_st <: Call_st;
  M3AST_AS.Case_st <: Case_st;
  M3AST_AS.Case <: Case;
  M3AST_AS.Else_stm <: Else_stm;
  M3AST_AS.Exit_st <: Exit_st;
  M3AST_AS.Eval_st <: Eval_st;
  M3AST_AS.For_st <: For_st;
  M3AST_AS.By <: By;
  M3AST_AS.If_st <: If_st;
  M3AST_AS.Elsif <: Elsif;
  M3AST_AS.Lock_st <: Lock_st;
  M3AST_AS.Loop_st <: Loop_st;
  M3AST_AS.Return_st <: Return_st;
  M3AST_AS.Repeat_st <: Repeat_st;
  M3AST_AS.Raise_st <: Raise_st;
  M3AST_AS.Try_st <: Try_st;
  M3AST_AS.TRY_TAIL <: TRY_TAIL;
  M3AST_AS.Try_finally <: Try_finally;
  M3AST_AS.Try_except <: Try_except;
  M3AST_AS.Handler <: Handler;
  M3AST_AS.Typecase_st <: Typecase_st;
  M3AST_AS.Tcase <: Tcase;
  M3AST_AS.While_st <: While_st;
  M3AST_AS.With_st <: With_st;
  M3AST_AS.Binding <: Binding;
  M3AST_AS.Block <: Block;

TYPE
  Bad_EXP = M3AST_AS.EXP OBJECT END;
  Bad_M3TYPE = M3AST_AS.M3TYPE OBJECT END;
  Bad_STM = M3AST_AS.STM OBJECT END;

REVEAL
  M3AST_AS.Bad_EXP <: Bad_EXP;
  M3AST_AS.Bad_M3TYPE <: Bad_M3TYPE;
  M3AST_AS.Bad_STM <: Bad_STM;

(* Pass throughs; everything from "M3AST_LX" except for "LITERAL"
which was actually defined here. *)

TYPE 
  SRC_NODE = Previous_View.SRC_NODE;
  SRC_NODE_C = Previous_View.SRC_NODE_C;
  Whitespace = Previous_View.Whitespace;
  Comment = Previous_View.Comment;
  Pragma = Previous_View.Pragma;
  BadChar = Previous_View.BadChar;
  Token = Previous_View.Token;

PROCEDURE NotA_USED_ID(n: NODE;
                    VAR (*out*) used_id: M3AST_AS.USED_ID): BOOLEAN;
PROCEDURE A_USED_ID(n: NODE;
                    VAR (*out*) used_id: M3AST_AS.USED_ID): BOOLEAN;
PROCEDURE A_Exp_USED_ID(n: NODE;
                    VAR (*out*) used_id: M3AST_AS.USED_ID): BOOLEAN;
PROCEDURE NotA_ID(n: NODE;
                    VAR (*out*) id: M3AST_AS.ID): BOOLEAN;
PROCEDURE A_ID(n: NODE;
                    VAR (*out*) id: M3AST_AS.ID): BOOLEAN;
PROCEDURE A_Exp_ID(n: NODE;
                    VAR (*out*) id: M3AST_AS.ID): BOOLEAN;
END M3AST_AS_F.

