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

(* This interface contains the definitions of the default tree walk procedures
for the syntactic nodes.  Leaf nodes are not present. *)

INTERFACE M3AST_AS_Walk;

IMPORT M3AST_AS_F;
IMPORT AST_WalkRep;

PROCEDURE Qual_used_id(
    n: M3AST_AS_F.Qual_used_id; h: AST_WalkRep.Handle
    ) RAISES ANY;         
PROCEDURE Compilation_Unit(
    n: M3AST_AS_F.Compilation_Unit; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Interface_gen_def(
    n: M3AST_AS_F.Interface_gen_def; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Module_gen_def(
    n: M3AST_AS_F.Module_gen_def; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Interface_gen_ins(
    n: M3AST_AS_F.Interface_gen_ins; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Module_gen_ins(
    n: M3AST_AS_F.Module_gen_ins; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Interface(
    n: M3AST_AS_F.Interface; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Module(
    n: M3AST_AS_F.Module; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Import_item(
    n: M3AST_AS_F.Import_item; h: AST_WalkRep.Handle
    ) RAISES ANY;       
PROCEDURE Simple_import(
    n: M3AST_AS_F.Simple_import; h: AST_WalkRep.Handle
    ) RAISES ANY;       
PROCEDURE From_import(
    n: M3AST_AS_F.From_import; h: AST_WalkRep.Handle
    ) RAISES ANY;         
PROCEDURE Revelation_s(
    n: M3AST_AS_F.Revelation_s; h: AST_WalkRep.Handle
    ) RAISES ANY;       
PROCEDURE Const_decl_s(
    n: M3AST_AS_F.Const_decl_s; h: AST_WalkRep.Handle
    ) RAISES ANY;            
PROCEDURE Type_decl_s(
    n: M3AST_AS_F.Type_decl_s; h: AST_WalkRep.Handle
    ) RAISES ANY;             
PROCEDURE Var_decl_s(
    n: M3AST_AS_F.Var_decl_s; h: AST_WalkRep.Handle
    ) RAISES ANY;              
PROCEDURE Exc_decl_s(
    n: M3AST_AS_F.Exc_decl_s; h: AST_WalkRep.Handle
    ) RAISES ANY;              
PROCEDURE Proc_decl(
    n: M3AST_AS_F.Proc_decl; h: AST_WalkRep.Handle
    ) RAISES ANY;               
PROCEDURE Const_decl(
    n: M3AST_AS_F.Const_decl; h: AST_WalkRep.Handle
    ) RAISES ANY;          
PROCEDURE Var_decl(
    n: M3AST_AS_F.Var_decl; h: AST_WalkRep.Handle
    ) RAISES ANY;            
PROCEDURE Exc_decl(
    n: M3AST_AS_F.Exc_decl; h: AST_WalkRep.Handle
    ) RAISES ANY;            
PROCEDURE Subtype_decl(
    n: M3AST_AS_F.Subtype_decl; h: AST_WalkRep.Handle
    ) RAISES ANY;       
PROCEDURE Concrete_decl(
    n: M3AST_AS_F.Concrete_decl; h: AST_WalkRep.Handle
    ) RAISES ANY;      
PROCEDURE Subtype_reveal(
    n: M3AST_AS_F.Subtype_reveal; h: AST_WalkRep.Handle
    ) RAISES ANY;    
PROCEDURE Concrete_reveal(
    n: M3AST_AS_F.Concrete_reveal; h: AST_WalkRep.Handle
    ) RAISES ANY;   
PROCEDURE Named_type(
    n: M3AST_AS_F.Named_type; h: AST_WalkRep.Handle
    ) RAISES ANY;            
PROCEDURE Root_type(
    n: M3AST_AS_F.Root_type; h: AST_WalkRep.Handle;
    ) RAISES ANY ;
PROCEDURE Enumeration_type(
    n: M3AST_AS_F.Enumeration_type; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Subrange_type(
    n: M3AST_AS_F.Subrange_type; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Array_type(
    n: M3AST_AS_F.Array_type; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Record_type(
    n: M3AST_AS_F.Record_type; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Object_type(
    n: M3AST_AS_F.Object_type; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Set_type(
    n: M3AST_AS_F.Set_type; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Procedure_type(
    n: M3AST_AS_F.Procedure_type; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Ref_type(
    n: M3AST_AS_F.Ref_type; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Packed_type(
    n: M3AST_AS_F.Packed_type; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Opaque_type(
    n: M3AST_AS_F.Opaque_type; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Brand(
    n: M3AST_AS_F.Brand; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Fields(
    n: M3AST_AS_F.Fields; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Method(
    n: M3AST_AS_F.Method; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Override(
    n: M3AST_AS_F.Override; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Formal_param(
    n: M3AST_AS_F.Formal_param; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Raisees_some(
    n: M3AST_AS_F.Raisees_some; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Range(
    n: M3AST_AS_F.Range; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Range_EXP(
    n: M3AST_AS_F.Range_EXP; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Constructor(
    n: M3AST_AS_F.Constructor; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE RANGE_EXP_elem(
    n: M3AST_AS_F.RANGE_EXP_elem; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Actual_elem(
    n: M3AST_AS_F.Actual_elem; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE BINARY(
    n: M3AST_AS_F.BINARY; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE UNARY(
    n: M3AST_AS_F.UNARY; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Select(
    n: M3AST_AS_F.Select; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Call(
    n: M3AST_AS_F.Call; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Index(
    n: M3AST_AS_F.Index; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Actual(
    n: M3AST_AS_F.Actual; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Assign_st(
    n: M3AST_AS_F.Assign_st; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Call_st(
    n: M3AST_AS_F.Call_st; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Case_st(
    n: M3AST_AS_F.Case_st; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Eval_st(
    n: M3AST_AS_F.Eval_st; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE For_st(
    n: M3AST_AS_F.For_st; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE If_st(
    n: M3AST_AS_F.If_st; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Lock_st(
    n: M3AST_AS_F.Lock_st; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Loop_st(
    n: M3AST_AS_F.Loop_st; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Raise_st(
    n: M3AST_AS_F.Raise_st; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Repeat_st(
    n: M3AST_AS_F.Repeat_st; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Return_st(
    n: M3AST_AS_F.Return_st; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Try_st(
    n: M3AST_AS_F.Try_st; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Typecase_st(
    n: M3AST_AS_F.Typecase_st; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE While_st(
    n: M3AST_AS_F.While_st; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE With_st(
    n: M3AST_AS_F.With_st; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Block(
    n: M3AST_AS_F.Block; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Case(
    n: M3AST_AS_F.Case; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Else_stm(
    n: M3AST_AS_F.Else_stm; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE By(
    n: M3AST_AS_F.By; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Elsif(
    n: M3AST_AS_F.Elsif; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Try_except(
    n: M3AST_AS_F.Try_except; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Try_finally(
    n: M3AST_AS_F.Try_finally; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Tcase(
    n: M3AST_AS_F.Tcase; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Handler(
    n: M3AST_AS_F.Handler; h: AST_WalkRep.Handle
    ) RAISES ANY;
PROCEDURE Binding(
    n: M3AST_AS_F.Binding; h: AST_WalkRep.Handle
    ) RAISES ANY;

END M3AST_AS_Walk.
