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

INTERFACE M3AST_AS_Copy;

(* This interface contains the definitions of the default tree copy procedures
for the syntactic nodes. *)

IMPORT AST;
IMPORT M3AST_AS_F;
IMPORT AST_CopyRep;

PROCEDURE Module_id(
    n: M3AST_AS_F.Module_id; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Interface_id(
    n: M3AST_AS_F.Interface_id; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE F_Interface_id(
    n: M3AST_AS_F.F_Interface_id; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Interface_AS_id(
    n: M3AST_AS_F.Interface_AS_id; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE F_Value_id(
    n: M3AST_AS_F.F_Value_id; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE F_Var_id(
    n: M3AST_AS_F.F_Var_id; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE F_Readonly_id(
    n: M3AST_AS_F.F_Readonly_id; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Type_id(
    n: M3AST_AS_F.Type_id; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Const_id(
    n: M3AST_AS_F.Const_id; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Var_id(
    n: M3AST_AS_F.Var_id; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Proc_id(
    n: M3AST_AS_F.Proc_id; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Enum_id(
    n: M3AST_AS_F.Enum_id; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Method_id(
    n: M3AST_AS_F.Method_id; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Override_id(
    n: M3AST_AS_F.Override_id; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Field_id(
    n: M3AST_AS_F.Field_id; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE For_id(
    n: M3AST_AS_F.For_id; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Handler_id(
    n: M3AST_AS_F.Handler_id; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Tcase_id(
    n: M3AST_AS_F.Tcase_id; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE With_id(
    n: M3AST_AS_F.With_id; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Exc_id(
    n: M3AST_AS_F.Exc_id; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Used_interface_id(
    n: M3AST_AS_F.Used_interface_id; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Used_def_id(
    n: M3AST_AS_F.Used_def_id; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Qual_used_id(
    n: M3AST_AS_F.Qual_used_id; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Compilation_Unit(
    n: M3AST_AS_F.Compilation_Unit; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Interface_gen_def(
    n: M3AST_AS_F.Interface_gen_def; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Module_gen_def(
    n: M3AST_AS_F.Module_gen_def; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Interface_gen_ins(
    n: M3AST_AS_F.Interface_gen_ins; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Module_gen_ins(
    n: M3AST_AS_F.Module_gen_ins; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Interface(
    n: M3AST_AS_F.Interface; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Module(
    n: M3AST_AS_F.Module; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Unsafe(
    n: M3AST_AS_F.Unsafe; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Import_item(
    n: M3AST_AS_F.Import_item; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Simple_import(
    n: M3AST_AS_F.Simple_import; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE From_import(
    n: M3AST_AS_F.From_import; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Revelation_s(
    n: M3AST_AS_F.Revelation_s; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Const_decl_s(
    n: M3AST_AS_F.Const_decl_s; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Type_decl_s(
    n: M3AST_AS_F.Type_decl_s; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Var_decl_s(
    n: M3AST_AS_F.Var_decl_s; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Exc_decl_s(
    n: M3AST_AS_F.Exc_decl_s; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Proc_decl(
    n: M3AST_AS_F.Proc_decl; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Const_decl(
    n: M3AST_AS_F.Const_decl; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Var_decl(
    n: M3AST_AS_F.Var_decl; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Exc_decl(
    n: M3AST_AS_F.Exc_decl; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Subtype_decl(
    n: M3AST_AS_F.Subtype_decl; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Concrete_decl(
    n: M3AST_AS_F.Concrete_decl; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Subtype_reveal(
    n: M3AST_AS_F.Subtype_reveal; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Concrete_reveal(
    n: M3AST_AS_F.Concrete_reveal; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Named_type(
    n: M3AST_AS_F.Named_type; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Integer_type(
    n: M3AST_AS_F.Integer_type; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Real_type(
    n: M3AST_AS_F.Real_type; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE LongReal_type(
    n: M3AST_AS_F.LongReal_type; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Extended_type(
    n: M3AST_AS_F.Extended_type; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Null_type(
    n: M3AST_AS_F.Null_type; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE RefAny_type(
    n: M3AST_AS_F.RefAny_type; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Address_type(
    n: M3AST_AS_F.Address_type; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Root_type(
    n: M3AST_AS_F.Root_type; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Enumeration_type(
    n: M3AST_AS_F.Enumeration_type; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Subrange_type(
    n: M3AST_AS_F.Subrange_type; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Array_type(
    n: M3AST_AS_F.Array_type; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Record_type(
    n: M3AST_AS_F.Record_type; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Object_type(
    n: M3AST_AS_F.Object_type; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Set_type(
    n: M3AST_AS_F.Set_type; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Procedure_type(
    n: M3AST_AS_F.Procedure_type; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Ref_type(
    n: M3AST_AS_F.Ref_type; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Packed_type(
    n: M3AST_AS_F.Packed_type; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Opaque_type(
    n: M3AST_AS_F.Opaque_type; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Brand(
    n: M3AST_AS_F.Brand; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Untraced(
    n: M3AST_AS_F.Untraced; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Fields(
    n: M3AST_AS_F.Fields; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Method(
    n: M3AST_AS_F.Method; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Override(
    n: M3AST_AS_F.Override; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Formal_param(
    n: M3AST_AS_F.Formal_param; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Raisees_some(
    n: M3AST_AS_F.Raisees_some; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Raisees_any(
    n: M3AST_AS_F.Raisees_any; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Range_EXP(
    n: M3AST_AS_F.Range_EXP; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Range(
    n: M3AST_AS_F.Range; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Integer_literal(
    n: M3AST_AS_F.Integer_literal; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Real_literal(
    n: M3AST_AS_F.Real_literal; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE LongReal_literal(
    n: M3AST_AS_F.LongReal_literal; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Extended_literal(
    n: M3AST_AS_F.Extended_literal; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Nil_literal(
    n: M3AST_AS_F.Nil_literal; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Text_literal(
    n: M3AST_AS_F.Text_literal; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Char_literal(
    n: M3AST_AS_F.Char_literal; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Exp_used_id(
    n: M3AST_AS_F.Exp_used_id; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Constructor(
    n: M3AST_AS_F.Constructor; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE RANGE_EXP_elem(
    n: M3AST_AS_F.RANGE_EXP_elem; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Actual_elem(
    n: M3AST_AS_F.Actual_elem; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Propagate(
    n: M3AST_AS_F.Propagate; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Plus(
    n: M3AST_AS_F.Plus; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Minus(
    n: M3AST_AS_F.Minus; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Times(
    n: M3AST_AS_F.Times; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Rdiv(
    n: M3AST_AS_F.Rdiv; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Textcat(
    n: M3AST_AS_F.Textcat; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Div(
    n: M3AST_AS_F.Div; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Mod(
    n: M3AST_AS_F.Mod; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Eq(
    n: M3AST_AS_F.Eq; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Ne(
    n: M3AST_AS_F.Ne; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Gt(
    n: M3AST_AS_F.Gt; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Lt(
    n: M3AST_AS_F.Lt; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Ge(
    n: M3AST_AS_F.Ge; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Le(
    n: M3AST_AS_F.Le; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE And(
    n: M3AST_AS_F.And; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Or(
    n: M3AST_AS_F.Or; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE In(
    n: M3AST_AS_F.In; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Select(
    n: M3AST_AS_F.Select; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Not(
    n: M3AST_AS_F.Not; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Unaryplus(
    n: M3AST_AS_F.Unaryplus; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Unaryminus(
    n: M3AST_AS_F.Unaryminus; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Deref(
    n: M3AST_AS_F.Deref; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Call(
    n: M3AST_AS_F.Call; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE NEWCall(
    n: M3AST_AS_F.NEWCall; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Index(
    n: M3AST_AS_F.Index; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Actual(
    n: M3AST_AS_F.Actual; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Assign_st(
    n: M3AST_AS_F.Assign_st; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Call_st(
    n: M3AST_AS_F.Call_st; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Case_st(
    n: M3AST_AS_F.Case_st; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Eval_st(
    n: M3AST_AS_F.Eval_st; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Exit_st(
    n: M3AST_AS_F.Exit_st; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE For_st(
    n: M3AST_AS_F.For_st; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE If_st(
    n: M3AST_AS_F.If_st; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Lock_st(
    n: M3AST_AS_F.Lock_st; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Loop_st(
    n: M3AST_AS_F.Loop_st; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Raise_st(
    n: M3AST_AS_F.Raise_st; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Repeat_st(
    n: M3AST_AS_F.Repeat_st; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Return_st(
    n: M3AST_AS_F.Return_st; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Try_st(
    n: M3AST_AS_F.Try_st; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Typecase_st(
    n: M3AST_AS_F.Typecase_st; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE While_st(
    n: M3AST_AS_F.While_st; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE With_st(
    n: M3AST_AS_F.With_st; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Block(
    n: M3AST_AS_F.Block; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Case(
    n: M3AST_AS_F.Case; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Else_stm(
    n: M3AST_AS_F.Else_stm; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Elsif(
    n: M3AST_AS_F.Elsif; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Try_except(
    n: M3AST_AS_F.Try_except; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Try_finally(
    n: M3AST_AS_F.Try_finally; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Tcase(
    n: M3AST_AS_F.Tcase; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Handler(
    n: M3AST_AS_F.Handler; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Binding(
    n: M3AST_AS_F.Binding; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE By(
    n: M3AST_AS_F.By; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Bad_EXP(
    n: M3AST_AS_F.Bad_EXP; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Bad_M3TYPE(
    n: M3AST_AS_F.Bad_M3TYPE; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;
PROCEDURE Bad_STM(
    n: M3AST_AS_F.Bad_STM; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY;

END M3AST_AS_Copy.
