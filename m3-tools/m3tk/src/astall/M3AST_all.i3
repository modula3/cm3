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


INTERFACE M3AST_all;

IMPORT AST;
IMPORT M3AST_AS;
IMPORT M3AST_LX;
IMPORT M3AST_SM;
IMPORT M3AST_PG;
IMPORT M3AST_SC;

IMPORT M3AST_SC_F AS Last_View;
IMPORT M3AST_PG_F, M3AST_PG_M;

(* The node revelations are ordered by the interface in which they
   were declared. The ordering of the views thus far is:

    M3AST_AS, M3ASTOp_AS, M3AST_SM, M3AST_TM, M3ASTOp_SM,
    M3AST_FE, M3AST_FE_priv, M3AST_TL, M3AST_PL 

  The nodes declared in M3AST_PG have no added attributes and
  are dealt with directly.
*)

REVEAL
(* Nodes declared in AST. *)

  AST.NODE = Last_View.NODE BRANDED OBJECT END;

(* Nodes declared in M3AST_LX. *)

  M3AST_LX.SRC_NODE = Last_View.SRC_NODE BRANDED OBJECT END;
  M3AST_LX.SRC_NODE_C = Last_View.SRC_NODE_C BRANDED OBJECT END;
  M3AST_LX.Whitespace = Last_View.Whitespace BRANDED OBJECT END;
  M3AST_LX.Comment = Last_View.Comment BRANDED OBJECT END;
  M3AST_LX.Pragma = Last_View.Pragma BRANDED OBJECT END;
  M3AST_LX.BadChar = Last_View.BadChar BRANDED OBJECT END;
  M3AST_LX.Token = Last_View.Token BRANDED OBJECT END;
  M3AST_LX.ID = Last_View.ID BRANDED OBJECT END;
  M3AST_LX.LITERAL = Last_View.LITERAL BRANDED OBJECT END;

  M3AST_LX.Pragma_rep = BRANDED OBJECT END;
  M3AST_LX.Comment_rep = BRANDED OBJECT END;
  M3AST_LX.BadChar_rep = BRANDED OBJECT END;

(* Nodes declared in M3AST_AS. *)

  M3AST_AS.DEF_ID = Last_View.DEF_ID BRANDED OBJECT END;
  M3AST_AS.UNIT_ID = Last_View.UNIT_ID BRANDED OBJECT END;
  M3AST_AS.Module_id = Last_View.Module_id BRANDED OBJECT END;
  M3AST_AS.Interface_id = Last_View.Interface_id BRANDED OBJECT END;
  M3AST_AS.Interface_AS_id = Last_View.Interface_AS_id BRANDED OBJECT END;
  M3AST_AS.F_Interface_id = Last_View.F_Interface_id BRANDED OBJECT END;
  M3AST_AS.TYPED_ID = Last_View.TYPED_ID BRANDED OBJECT END;
  M3AST_AS.FORMAL_ID = Last_View.FORMAL_ID BRANDED OBJECT END;
  M3AST_AS.F_Value_id = Last_View.F_Value_id BRANDED OBJECT END;
  M3AST_AS.F_Var_id = Last_View.F_Var_id BRANDED OBJECT END;
  M3AST_AS.F_Readonly_id = Last_View.F_Readonly_id BRANDED OBJECT END;
  M3AST_AS.Type_id = Last_View.Type_id BRANDED OBJECT END;
  M3AST_AS.Const_id = Last_View.Const_id BRANDED OBJECT END;
  M3AST_AS.Var_id = Last_View.Var_id BRANDED OBJECT END;
  M3AST_AS.Proc_id = Last_View.Proc_id BRANDED OBJECT END;
  M3AST_AS.Enum_id = Last_View.Enum_id BRANDED OBJECT END;
  M3AST_AS.METHOD_OVERRIDE_ID = Last_View.METHOD_OVERRIDE_ID BRANDED OBJECT END;
  M3AST_AS.Method_id = Last_View.Method_id BRANDED OBJECT END;
  M3AST_AS.Override_id = Last_View.Override_id BRANDED OBJECT END;
  M3AST_AS.Field_id = Last_View.Field_id BRANDED OBJECT END;
  M3AST_AS.For_id = Last_View.For_id BRANDED OBJECT END;
  M3AST_AS.Handler_id = Last_View.Handler_id BRANDED OBJECT END;
  M3AST_AS.Tcase_id = Last_View.Tcase_id BRANDED OBJECT END;
  M3AST_AS.With_id = Last_View.With_id BRANDED OBJECT END;
  M3AST_AS.Exc_id = Last_View.Exc_id BRANDED OBJECT END;
  M3AST_AS.USED_ID = Last_View.USED_ID BRANDED OBJECT END;
  M3AST_AS.Used_interface_id = Last_View.Used_interface_id BRANDED OBJECT END;
  M3AST_AS.Used_def_id = Last_View.Used_def_id BRANDED OBJECT END;
  M3AST_AS.Qual_used_id = Last_View.Qual_used_id BRANDED OBJECT END;
  M3AST_AS.Compilation_Unit = Last_View.Compilation_Unit BRANDED OBJECT END;
  M3AST_AS.UNIT = Last_View.UNIT BRANDED OBJECT END;
  M3AST_AS.UNIT_WITH_BODY = Last_View.UNIT_WITH_BODY BRANDED OBJECT END;
  M3AST_AS.UNIT_GEN_DEF = Last_View.UNIT_GEN_DEF BRANDED OBJECT END;
  M3AST_AS.Interface_gen_def = Last_View.Interface_gen_def BRANDED OBJECT END;
  M3AST_AS.Module_gen_def = Last_View.Module_gen_def BRANDED OBJECT END;
  M3AST_AS.UNIT_NORMAL = Last_View.UNIT_NORMAL BRANDED OBJECT END;
  M3AST_AS.Interface = Last_View.Interface BRANDED OBJECT END;
  M3AST_AS.Module = Last_View.Module BRANDED OBJECT END;
  M3AST_AS.UNIT_GEN_INS = Last_View.UNIT_GEN_INS BRANDED OBJECT END;
  M3AST_AS.Interface_gen_ins = Last_View.Interface_gen_ins BRANDED OBJECT END;
  M3AST_AS.Module_gen_ins = Last_View.Module_gen_ins BRANDED OBJECT END;
  M3AST_AS.Unsafe = Last_View.Unsafe BRANDED OBJECT END;
  M3AST_AS.IMPORTED = Last_View.IMPORTED BRANDED OBJECT END;
  M3AST_AS.Simple_import = Last_View.Simple_import BRANDED OBJECT END;
  M3AST_AS.Import_item = Last_View.Import_item BRANDED OBJECT END;
  M3AST_AS.From_import = Last_View.From_import BRANDED OBJECT END;
  M3AST_AS.DECL_REVL = Last_View.DECL_REVL BRANDED OBJECT END;
  M3AST_AS.DECL = Last_View.DECL BRANDED OBJECT END;
  M3AST_AS.Const_decl_s = Last_View.Const_decl_s BRANDED OBJECT END;
  M3AST_AS.Type_decl_s = Last_View.Type_decl_s BRANDED OBJECT END;
  M3AST_AS.Var_decl_s = Last_View.Var_decl_s BRANDED OBJECT END;
  M3AST_AS.Exc_decl_s = Last_View.Exc_decl_s BRANDED OBJECT END;
  M3AST_AS.Proc_decl = Last_View.Proc_decl BRANDED OBJECT END;
  M3AST_AS.Const_decl = Last_View.Const_decl BRANDED OBJECT END;
  M3AST_AS.TYPE_DECL = Last_View.TYPE_DECL BRANDED OBJECT END;
  M3AST_AS.Subtype_decl = Last_View.Subtype_decl BRANDED OBJECT END;
  M3AST_AS.Concrete_decl = Last_View.Concrete_decl BRANDED OBJECT END;
  M3AST_AS.Var_decl = Last_View.Var_decl BRANDED OBJECT END;
  M3AST_AS.Exc_decl = Last_View.Exc_decl BRANDED OBJECT END;
  M3AST_AS.Revelation_s = Last_View.Revelation_s BRANDED OBJECT END;
  M3AST_AS.REVELATION = Last_View.REVELATION BRANDED OBJECT END;
  M3AST_AS.Subtype_reveal = Last_View.Subtype_reveal BRANDED OBJECT END;
  M3AST_AS.Concrete_reveal = Last_View.Concrete_reveal BRANDED OBJECT END;
  M3AST_AS.EXP_TYPE = Last_View.EXP_TYPE BRANDED OBJECT END;
  M3AST_AS.M3TYPE = Last_View.M3TYPE BRANDED OBJECT END;
  M3AST_AS.Named_type = Last_View.Named_type BRANDED OBJECT END;
  M3AST_AS.TYPE_SPEC = Last_View.TYPE_SPEC BRANDED OBJECT END;
  M3AST_AS.FLOAT_TYPE = Last_View.FLOAT_TYPE BRANDED OBJECT END;
  M3AST_AS.Real_type = Last_View.Real_type BRANDED OBJECT END;
  M3AST_AS.LongReal_type = Last_View.LongReal_type BRANDED OBJECT END;
  M3AST_AS.Extended_type = Last_View.Extended_type BRANDED OBJECT END;
  M3AST_AS.Integer_type = Last_View.Integer_type BRANDED OBJECT END;
  M3AST_AS.Null_type = Last_View.Null_type BRANDED OBJECT END;
  M3AST_AS.RefAny_type = Last_View.RefAny_type BRANDED OBJECT END;
  M3AST_AS.Address_type = Last_View.Address_type BRANDED OBJECT END;
  M3AST_AS.Root_type = Last_View.Root_type BRANDED OBJECT END;
  M3AST_AS.Untraced = Last_View.Untraced BRANDED OBJECT END;
  M3AST_AS.Packed_type = Last_View.Packed_type BRANDED OBJECT END;
  M3AST_AS.Array_type = Last_View.Array_type BRANDED OBJECT END;
  M3AST_AS.Enumeration_type = Last_View.Enumeration_type BRANDED OBJECT END;
  M3AST_AS.Set_type = Last_View.Set_type BRANDED OBJECT END;
  M3AST_AS.Subrange_type = Last_View.Subrange_type BRANDED OBJECT END;
  M3AST_AS.RANGE_EXP = Last_View.RANGE_EXP BRANDED OBJECT END;
  M3AST_AS.Range_EXP = Last_View.Range_EXP BRANDED OBJECT END;
  M3AST_AS.Range = Last_View.Range BRANDED OBJECT END;
  M3AST_AS.Record_type = Last_View.Record_type BRANDED OBJECT END;
  M3AST_AS.Fields = Last_View.Fields BRANDED OBJECT END;
  M3AST_AS.BRANDED_TYPE = Last_View.BRANDED_TYPE BRANDED OBJECT END;
  M3AST_AS.Brand = Last_View.Brand BRANDED OBJECT END;
  M3AST_AS.Ref_type = Last_View.Ref_type BRANDED OBJECT END;
  M3AST_AS.Object_type = Last_View.Object_type BRANDED OBJECT END;
  M3AST_AS.METHOD_OVERRIDE = Last_View.METHOD_OVERRIDE BRANDED OBJECT END;
  M3AST_AS.Method = Last_View.Method BRANDED OBJECT END;
  M3AST_AS.Override = Last_View.Override BRANDED OBJECT END;
  M3AST_AS.Procedure_type = Last_View.Procedure_type BRANDED OBJECT END;
  M3AST_AS.Formal_param = Last_View.Formal_param BRANDED OBJECT END;
  M3AST_AS.RAISEES = Last_View.RAISEES BRANDED OBJECT END;
  M3AST_AS.Raisees_some = Last_View.Raisees_some BRANDED OBJECT END;
  M3AST_AS.Raisees_any = Last_View.Raisees_any BRANDED OBJECT END;
  M3AST_AS.Opaque_type = Last_View.Opaque_type BRANDED OBJECT END;
  M3AST_AS.EXP = Last_View.EXP BRANDED OBJECT END;
  M3AST_AS.NUMERIC_LITERAL = Last_View.NUMERIC_LITERAL BRANDED OBJECT END;
  M3AST_AS.Integer_literal = Last_View.Integer_literal BRANDED OBJECT END;
  M3AST_AS.Real_literal = Last_View.Real_literal BRANDED OBJECT END;
  M3AST_AS.LongReal_literal = Last_View.LongReal_literal BRANDED OBJECT END;
  M3AST_AS.Extended_literal = Last_View.Extended_literal BRANDED OBJECT END;
  M3AST_AS.Char_literal = Last_View.Char_literal BRANDED OBJECT END;
  M3AST_AS.Text_literal = Last_View.Text_literal BRANDED OBJECT END;
  M3AST_AS.Nil_literal = Last_View.Nil_literal BRANDED OBJECT END;
  M3AST_AS.Exp_used_id = Last_View.Exp_used_id BRANDED OBJECT END;
  M3AST_AS.Call = Last_View.Call BRANDED OBJECT END;
  M3AST_AS.NEWCall = Last_View.NEWCall BRANDED OBJECT END;
  M3AST_AS.Actual = Last_View.Actual BRANDED OBJECT END;
  M3AST_AS.Index = Last_View.Index BRANDED OBJECT END;
  M3AST_AS.Constructor = Last_View.Constructor BRANDED OBJECT END;
  M3AST_AS.Propagate = Last_View.Propagate BRANDED OBJECT END;
  M3AST_AS.CONS_ELEM = Last_View.CONS_ELEM BRANDED OBJECT END;
  M3AST_AS.RANGE_EXP_elem = Last_View.RANGE_EXP_elem BRANDED OBJECT END;
  M3AST_AS.Actual_elem = Last_View.Actual_elem BRANDED OBJECT END;
  M3AST_AS.BINARY = Last_View.BINARY BRANDED OBJECT END;
  M3AST_AS.Plus = Last_View.Plus BRANDED OBJECT END;
  M3AST_AS.Minus = Last_View.Minus BRANDED OBJECT END;
  M3AST_AS.Times = Last_View.Times BRANDED OBJECT END;
  M3AST_AS.Rdiv = Last_View.Rdiv BRANDED OBJECT END;
  M3AST_AS.Textcat = Last_View.Textcat BRANDED OBJECT END;
  M3AST_AS.Div = Last_View.Div BRANDED OBJECT END;
  M3AST_AS.Mod = Last_View.Mod BRANDED OBJECT END;
  M3AST_AS.Eq = Last_View.Eq BRANDED OBJECT END;
  M3AST_AS.Ne = Last_View.Ne BRANDED OBJECT END;
  M3AST_AS.Gt = Last_View.Gt BRANDED OBJECT END;
  M3AST_AS.Lt = Last_View.Lt BRANDED OBJECT END;
  M3AST_AS.Ge = Last_View.Ge BRANDED OBJECT END;
  M3AST_AS.Le = Last_View.Le BRANDED OBJECT END;
  M3AST_AS.And = Last_View.And BRANDED OBJECT END;
  M3AST_AS.Or = Last_View.Or BRANDED OBJECT END;
  M3AST_AS.In = Last_View.In BRANDED OBJECT END;
  M3AST_AS.Select = Last_View.Select BRANDED OBJECT END;
  M3AST_AS.UNARY = Last_View.UNARY BRANDED OBJECT END;
  M3AST_AS.Not = Last_View.Not BRANDED OBJECT END;
  M3AST_AS.Unaryplus = Last_View.Unaryplus BRANDED OBJECT END;
  M3AST_AS.Unaryminus = Last_View.Unaryminus BRANDED OBJECT END;
  M3AST_AS.Deref = Last_View.Deref BRANDED OBJECT END;
  M3AST_AS.STM = Last_View.STM BRANDED OBJECT END;
  M3AST_AS.STM_WSS = Last_View.STM_WSS BRANDED OBJECT END;
  M3AST_AS.SUBSTM_WSS = Last_View.SUBSTM_WSS BRANDED OBJECT END;
  M3AST_AS.Assign_st = Last_View.Assign_st BRANDED OBJECT END;
  M3AST_AS.Call_st = Last_View.Call_st BRANDED OBJECT END;
  M3AST_AS.Case_st = Last_View.Case_st BRANDED OBJECT END;
  M3AST_AS.Case = Last_View.Case BRANDED OBJECT END;
  M3AST_AS.Else_stm = Last_View.Else_stm BRANDED OBJECT END;
  M3AST_AS.Eval_st = Last_View.Eval_st BRANDED OBJECT END;
  M3AST_AS.Exit_st = Last_View.Exit_st BRANDED OBJECT END;
  M3AST_AS.Raise_st = Last_View.Raise_st BRANDED OBJECT END;
  M3AST_AS.Typecase_st = Last_View.Typecase_st BRANDED OBJECT END;
  M3AST_AS.Tcase = Last_View.Tcase BRANDED OBJECT END;
  M3AST_AS.Handler = Last_View.Handler BRANDED OBJECT END;
  M3AST_AS.Return_st = Last_View.Return_st BRANDED OBJECT END;
  M3AST_AS.For_st = Last_View.For_st BRANDED OBJECT END;
  M3AST_AS.By = Last_View.By BRANDED OBJECT END;
  M3AST_AS.If_st = Last_View.If_st BRANDED OBJECT END;
  M3AST_AS.Elsif = Last_View.Elsif BRANDED OBJECT END;
  M3AST_AS.Lock_st = Last_View.Lock_st BRANDED OBJECT END;
  M3AST_AS.Loop_st = Last_View.Loop_st BRANDED OBJECT END;
  M3AST_AS.Repeat_st = Last_View.Repeat_st BRANDED OBJECT END;
  M3AST_AS.Try_st = Last_View.Try_st BRANDED OBJECT END;
  M3AST_AS.TRY_TAIL = Last_View.TRY_TAIL BRANDED OBJECT END;
  M3AST_AS.Try_except = Last_View.Try_except BRANDED OBJECT END;
  M3AST_AS.Try_finally = Last_View.Try_finally BRANDED OBJECT END;
  M3AST_AS.While_st = Last_View.While_st BRANDED OBJECT END;
  M3AST_AS.With_st = Last_View.With_st BRANDED OBJECT END;
  M3AST_AS.Binding = Last_View.Binding BRANDED OBJECT END;
  M3AST_AS.Block = Last_View.Block BRANDED OBJECT END;
  M3AST_AS.Bad_EXP = Last_View.Bad_EXP BRANDED OBJECT END;
  M3AST_AS.Bad_M3TYPE = Last_View.Bad_M3TYPE BRANDED OBJECT END;
  M3AST_AS.Bad_STM = Last_View.Bad_STM BRANDED OBJECT END;

(* Nodes declared in M3AST_PG. *)

  M3AST_PG.External = M3AST_PG_M.External BRANDED OBJECT END;
  M3AST_PG.EXTERNAL_DECL = M3AST_PG_F.EXTERNAL_DECL BRANDED OBJECT END;
  M3AST_PG.Inline = M3AST_PG_M.Inline BRANDED OBJECT END;
  M3AST_PG.EXTERNAL_ID = M3AST_PG_F.EXTERNAL_ID BRANDED OBJECT END;

(* Nodes declared in M3AST_SM. *)

  M3AST_SM.Type_type = Last_View.Type_type BRANDED OBJECT END;
  M3AST_SM.Any_type = Last_View.Any_type BRANDED OBJECT END;
  M3AST_SM.Void_type = Last_View.Void_type BRANDED OBJECT END;
  M3AST_SM.TypeActual = Last_View.TypeActual BRANDED OBJECT END; 
  M3AST_SM.CCV_ID = Last_View.CCV_ID BRANDED OBJECT END;
  M3AST_SM.RECOBJ_ID = Last_View.RECOBJ_ID BRANDED OBJECT END;
  M3AST_SM.REDEF_ID = Last_View.REDEF_ID BRANDED OBJECT END;
  M3AST_SM.INIT_ID = Last_View.INIT_ID BRANDED OBJECT END;
  M3AST_SM.Opaque_type_Revln = Last_View.Opaque_type_Revln BRANDED OBJECT END;
  M3AST_SM.SCOPE = Last_View.SCOPE BRANDED OBJECT END;
 
(* Nodes declared in M3AST_SC. *)

  M3AST_SC.IMPORTED_NODE = Last_View.IMPORTED_NODE BRANDED OBJECT END;
  M3AST_SC.Unit_stub = Last_View.Unit_stub BRANDED OBJECT END;
  M3AST_SC.Imported_id = Last_View.Imported_id BRANDED OBJECT END;
  M3AST_SC.Imported_type = Last_View.Imported_type BRANDED OBJECT END;
  M3AST_SC.Exported_node = Last_View.Exported_node BRANDED OBJECT END;

END M3AST_all.
