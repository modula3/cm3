(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE M3AST_AS_Iter;

IMPORT M3AST_AS_F;
IMPORT AST_Iter;

PROCEDURE Qual_used_id_newIter(
    n: M3AST_AS_F.Qual_used_id): AST_Iter.T RAISES {};

PROCEDURE Compilation_Unit_newIter(
    n: M3AST_AS_F.Compilation_Unit): AST_Iter.T RAISES {};

PROCEDURE Interface_gen_def_newIter(
    n: M3AST_AS_F.Interface_gen_def): AST_Iter.T RAISES {};

PROCEDURE Module_gen_def_newIter(
    n: M3AST_AS_F.Module_gen_def): AST_Iter.T RAISES {};

PROCEDURE Interface_gen_ins_newIter(
    n: M3AST_AS_F.Interface_gen_ins): AST_Iter.T RAISES {};

PROCEDURE Module_gen_ins_newIter(
    n: M3AST_AS_F.Module_gen_ins): AST_Iter.T RAISES {};

PROCEDURE Interface_newIter(
    n: M3AST_AS_F.Interface): AST_Iter.T RAISES {};

PROCEDURE Module_newIter(
    n: M3AST_AS_F.Module): AST_Iter.T RAISES {};

PROCEDURE Import_item_newIter(
    n: M3AST_AS_F.Import_item): AST_Iter.T RAISES {};

PROCEDURE Simple_import_newIter(
    n: M3AST_AS_F.Simple_import): AST_Iter.T RAISES {};

PROCEDURE From_import_newIter(
    n: M3AST_AS_F.From_import): AST_Iter.T RAISES {};

PROCEDURE Revelation_s_newIter(
    n: M3AST_AS_F.Revelation_s): AST_Iter.T RAISES {};

PROCEDURE Const_decl_s_newIter(
    n: M3AST_AS_F.Const_decl_s): AST_Iter.T RAISES {};

PROCEDURE Type_decl_s_newIter(
    n: M3AST_AS_F.Type_decl_s): AST_Iter.T RAISES {};

PROCEDURE Var_decl_s_newIter(
    n: M3AST_AS_F.Var_decl_s): AST_Iter.T RAISES {};

PROCEDURE Exc_decl_s_newIter(
    n: M3AST_AS_F.Exc_decl_s): AST_Iter.T RAISES {};

PROCEDURE Proc_decl_newIter(
    n: M3AST_AS_F.Proc_decl): AST_Iter.T RAISES {};

PROCEDURE Const_decl_newIter(
    n: M3AST_AS_F.Const_decl): AST_Iter.T RAISES {};

PROCEDURE Var_decl_newIter(
    n: M3AST_AS_F.Var_decl): AST_Iter.T RAISES {};

PROCEDURE Exc_decl_newIter(
    n: M3AST_AS_F.Exc_decl): AST_Iter.T RAISES {};

PROCEDURE Subtype_decl_newIter(
    n: M3AST_AS_F.Subtype_decl): AST_Iter.T RAISES {};

PROCEDURE Concrete_decl_newIter(
    n: M3AST_AS_F.Concrete_decl): AST_Iter.T RAISES {};

PROCEDURE Subtype_reveal_newIter(
    n: M3AST_AS_F.Subtype_reveal): AST_Iter.T RAISES {};

PROCEDURE Concrete_reveal_newIter(
    n: M3AST_AS_F.Concrete_reveal): AST_Iter.T RAISES {};

PROCEDURE Named_type_newIter(
    n: M3AST_AS_F.Named_type): AST_Iter.T RAISES {};

PROCEDURE Root_type_newIter(
    n: M3AST_AS_F.Root_type): AST_Iter.T RAISES {};

PROCEDURE Enumeration_type_newIter(
    n: M3AST_AS_F.Enumeration_type): AST_Iter.T RAISES {};

PROCEDURE Subrange_type_newIter(
    n: M3AST_AS_F.Subrange_type): AST_Iter.T RAISES {};

PROCEDURE Array_type_newIter(
    n: M3AST_AS_F.Array_type): AST_Iter.T RAISES {};

PROCEDURE Record_type_newIter(
    n: M3AST_AS_F.Record_type): AST_Iter.T RAISES {};

PROCEDURE Object_type_newIter(
    n: M3AST_AS_F.Object_type): AST_Iter.T RAISES {};

PROCEDURE Set_type_newIter(
    n: M3AST_AS_F.Set_type): AST_Iter.T RAISES {};

PROCEDURE Procedure_type_newIter(
    n: M3AST_AS_F.Procedure_type): AST_Iter.T RAISES {};

PROCEDURE Ref_type_newIter(
    n: M3AST_AS_F.Ref_type): AST_Iter.T RAISES {};

PROCEDURE Packed_type_newIter(
    n: M3AST_AS_F.Packed_type): AST_Iter.T RAISES {};

PROCEDURE Opaque_type_newIter(
    n: M3AST_AS_F.Opaque_type): AST_Iter.T RAISES {};

PROCEDURE Brand_newIter(
    n: M3AST_AS_F.Brand): AST_Iter.T RAISES {};

PROCEDURE Fields_newIter(
    n: M3AST_AS_F.Fields): AST_Iter.T RAISES {};

PROCEDURE Method_newIter(
    n: M3AST_AS_F.Method): AST_Iter.T RAISES {};

PROCEDURE Override_newIter(
    n: M3AST_AS_F.Override): AST_Iter.T RAISES {};

PROCEDURE Formal_param_newIter(
    n: M3AST_AS_F.Formal_param): AST_Iter.T RAISES {};

PROCEDURE Raisees_some_newIter(
    n: M3AST_AS_F.Raisees_some): AST_Iter.T RAISES {};

PROCEDURE Range_EXP_newIter(
    n: M3AST_AS_F.Range_EXP): AST_Iter.T RAISES {};

PROCEDURE Range_newIter(
    n: M3AST_AS_F.Range): AST_Iter.T RAISES {};

PROCEDURE Constructor_newIter(
    n: M3AST_AS_F.Constructor): AST_Iter.T RAISES {};

PROCEDURE RANGE_EXP_elem_newIter(
    n: M3AST_AS_F.RANGE_EXP_elem): AST_Iter.T RAISES {};

PROCEDURE Actual_elem_newIter(
    n: M3AST_AS_F.Actual_elem): AST_Iter.T RAISES {};

PROCEDURE Propagate_newIter(
    n: M3AST_AS_F.Propagate): AST_Iter.T RAISES {};

PROCEDURE BINARY_newIter(
    n: M3AST_AS_F.BINARY): AST_Iter.T RAISES {};

PROCEDURE UNARY_newIter(
    n: M3AST_AS_F.UNARY): AST_Iter.T RAISES {};

PROCEDURE Select_newIter(
    n: M3AST_AS_F.Select): AST_Iter.T RAISES {};

PROCEDURE Call_newIter(
    n: M3AST_AS_F.Call): AST_Iter.T RAISES {};

PROCEDURE Index_newIter(
    n: M3AST_AS_F.Index): AST_Iter.T RAISES {};

PROCEDURE Actual_newIter(
    n: M3AST_AS_F.Actual): AST_Iter.T RAISES {};

PROCEDURE Assign_st_newIter(
    n: M3AST_AS_F.Assign_st): AST_Iter.T RAISES {};

PROCEDURE Call_st_newIter(
    n: M3AST_AS_F.Call_st): AST_Iter.T RAISES {};

PROCEDURE Case_st_newIter(
    n: M3AST_AS_F.Case_st): AST_Iter.T RAISES {};

PROCEDURE Eval_st_newIter(
    n: M3AST_AS_F.Eval_st): AST_Iter.T RAISES {};

PROCEDURE Exit_st_newIter(
    n: M3AST_AS_F.Exit_st): AST_Iter.T RAISES {};

PROCEDURE For_st_newIter(
    n: M3AST_AS_F.For_st): AST_Iter.T RAISES {};

PROCEDURE If_st_newIter(
    n: M3AST_AS_F.If_st): AST_Iter.T RAISES {};

PROCEDURE Lock_st_newIter(
    n: M3AST_AS_F.Lock_st): AST_Iter.T RAISES {};

PROCEDURE Loop_st_newIter(
    n: M3AST_AS_F.Loop_st): AST_Iter.T RAISES {};

PROCEDURE Raise_st_newIter(
    n: M3AST_AS_F.Raise_st): AST_Iter.T RAISES {};

PROCEDURE Repeat_st_newIter(
    n: M3AST_AS_F.Repeat_st): AST_Iter.T RAISES {};

PROCEDURE Return_st_newIter(
    n: M3AST_AS_F.Return_st): AST_Iter.T RAISES {};

PROCEDURE Try_st_newIter(
    n: M3AST_AS_F.Try_st): AST_Iter.T RAISES {};

PROCEDURE Typecase_st_newIter(
    n: M3AST_AS_F.Typecase_st): AST_Iter.T RAISES {};

PROCEDURE While_st_newIter(
    n: M3AST_AS_F.While_st): AST_Iter.T RAISES {};

PROCEDURE With_st_newIter(
    n: M3AST_AS_F.With_st): AST_Iter.T RAISES {};

PROCEDURE Block_newIter(
    n: M3AST_AS_F.Block): AST_Iter.T RAISES {};

PROCEDURE Case_newIter(
    n: M3AST_AS_F.Case): AST_Iter.T RAISES {};

PROCEDURE Else_stm_newIter(
    n: M3AST_AS_F.Else_stm): AST_Iter.T RAISES {};

PROCEDURE Elsif_newIter(
    n: M3AST_AS_F.Elsif): AST_Iter.T RAISES {};

PROCEDURE Try_except_newIter(
    n: M3AST_AS_F.Try_except): AST_Iter.T RAISES {};

PROCEDURE Try_finally_newIter(
    n: M3AST_AS_F.Try_finally): AST_Iter.T RAISES {};

PROCEDURE Tcase_newIter(
    n: M3AST_AS_F.Tcase): AST_Iter.T RAISES {};

PROCEDURE Handler_newIter(
    n: M3AST_AS_F.Handler): AST_Iter.T RAISES {};

PROCEDURE Binding_newIter(
    n: M3AST_AS_F.Binding): AST_Iter.T RAISES {};

PROCEDURE By_newIter(
    n: M3AST_AS_F.By): AST_Iter.T RAISES {};

END M3AST_AS_Iter.
