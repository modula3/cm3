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

MODULE M3AST_AS_Walk;

IMPORT M3AST_AS, M3AST_PG;
IMPORT M3AST_AS_F, M3AST_PG_F;

IMPORT AST_WalkRep;
IMPORT
    SeqM3AST_AS_IMPORTED,
    SeqM3AST_AS_Import_item, SeqM3AST_AS_F_Interface_id,
    SeqM3AST_AS_Used_interface_id, SeqM3AST_AS_Used_def_id,
    SeqM3AST_AS_REVELATION, SeqM3AST_AS_DECL_REVL,
    SeqM3AST_AS_Const_decl, SeqM3AST_AS_TYPE_DECL,
    SeqM3AST_AS_Var_decl, SeqM3AST_AS_Exc_decl,
    SeqM3AST_AS_Var_id,
    SeqM3AST_AS_Enum_id, SeqM3AST_AS_Field_id,
    SeqM3AST_AS_FORMAL_ID, SeqM3AST_AS_Qual_used_id,
    SeqM3AST_AS_Fields, SeqM3AST_AS_Method,
    SeqM3AST_AS_M3TYPE,
    SeqM3AST_AS_Formal_param, SeqM3AST_AS_CONS_ELEM,
    SeqM3AST_AS_EXP, SeqM3AST_AS_Actual,
    SeqM3AST_AS_Case, SeqM3AST_AS_STM,
    SeqM3AST_AS_Elsif, SeqM3AST_AS_Tcase,
    SeqM3AST_AS_Handler, SeqM3AST_AS_Binding,
    SeqM3AST_AS_RANGE_EXP, SeqM3AST_AS_Override;

(*PRIVATE*)
PROCEDURE VisitSeqStm(seqStm: SeqM3AST_AS_STM.T; h: AST_WalkRep.Handle)
     RAISES ANY =
  VAR
    m: M3AST_AS.STM;
    iter := SeqM3AST_AS_STM.NewIter(seqStm);
  BEGIN
    WHILE SeqM3AST_AS_STM.Next(iter, m) DO h.Visit(m); END;
  END VisitSeqStm;

(*PRIVATE*)
PROCEDURE VisitEXTERNAL_DECL(n: M3AST_PG.External_NULL;
    h: AST_WalkRep.Handle) RAISES ANY=
  BEGIN
    IF n # NIL THEN h.Visit(n) END;
  END VisitEXTERNAL_DECL;

PROCEDURE Qual_used_id(n: M3AST_AS_F.Qual_used_id; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    IF n.as_intf_id # NIL THEN h.Visit(n.as_intf_id); END;
    h.Visit(n.as_id);
  END Qual_used_id;


PROCEDURE Compilation_Unit(n: M3AST_AS_F.Compilation_Unit; 
      h: AST_WalkRep.Handle) RAISES ANY =
  BEGIN
    h.Visit(n.as_root);
  END Compilation_Unit;


(*PRIVATE*)
PROCEDURE VisitUNIT_WITH_BODY(n: M3AST_AS_F.UNIT_WITH_BODY; 
    h: AST_WalkRep.Handle) RAISES ANY=
  VAR
    m2: M3AST_AS.IMPORTED;
    iter2 := SeqM3AST_AS_IMPORTED.NewIter(n.as_import_s);
  BEGIN
    WHILE SeqM3AST_AS_IMPORTED.Next(iter2, m2) DO h.Visit(m2);
    END;
    h.Visit(n.as_block);    
  END VisitUNIT_WITH_BODY;

PROCEDURE VisitUNIT_GEN_DEF(n: M3AST_AS_F.UNIT_GEN_DEF; h: AST_WalkRep.Handle)
     RAISES ANY=
  VAR iter := SeqM3AST_AS_F_Interface_id.NewIter(n.as_id_s);
    m: M3AST_AS.F_Interface_id;
  BEGIN
    WHILE SeqM3AST_AS_F_Interface_id.Next(iter, m) DO h.Visit(m); END;
  END VisitUNIT_GEN_DEF;


PROCEDURE Interface(n: M3AST_AS_F.Interface; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    VisitEXTERNAL_DECL(n.vEXTERNAL_DECL.pg_external, h); 
    IF n.as_unsafe # NIL THEN h.Visit(n.as_unsafe); END;
    h.Visit(n.as_id);
    VisitUNIT_WITH_BODY(n, h);
  END Interface;


PROCEDURE Module(n: M3AST_AS_F.Module; h: AST_WalkRep.Handle)
     RAISES ANY =
  VAR
    m: M3AST_AS.Used_interface_id;
    iter := SeqM3AST_AS_Used_interface_id.NewIter(n.as_export_s);
  BEGIN
    IF n.as_unsafe # NIL THEN h.Visit(n.as_unsafe); END;
    h.Visit(n.as_id);
    WHILE SeqM3AST_AS_Used_interface_id.Next(iter, m) DO h.Visit(m); END;
    VisitUNIT_WITH_BODY(n, h);
  END Module;


PROCEDURE Interface_gen_def(n: M3AST_AS_F.Interface_gen_def; 
    h: AST_WalkRep.Handle) RAISES ANY =
  BEGIN
    VisitEXTERNAL_DECL(n.vEXTERNAL_DECL.pg_external, h); 
    h.Visit(n.as_id);
    VisitUNIT_GEN_DEF(n, h);
    VisitUNIT_WITH_BODY(n, h);
  END Interface_gen_def;


PROCEDURE Module_gen_def(n: M3AST_AS_F.Module_gen_def; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    h.Visit(n.as_id);
    VisitUNIT_GEN_DEF(n, h);
    VisitUNIT_WITH_BODY(n, h);
  END Module_gen_def;


PROCEDURE Interface_gen_ins(n: M3AST_AS_F.Interface_gen_ins; 
    h: AST_WalkRep.Handle) RAISES ANY =
  VAR
    m: M3AST_AS.Used_interface_id;
    iter := SeqM3AST_AS_Used_interface_id.NewIter(n.as_id_s);
  BEGIN
    IF n.as_unsafe # NIL THEN h.Visit(n.as_unsafe); END;
    h.Visit(n.as_id);
    h.Visit(n.as_gen_id);
    WHILE SeqM3AST_AS_Used_interface_id.Next(iter, m) DO h.Visit(m); END;
  END Interface_gen_ins;


PROCEDURE Module_gen_ins(n: M3AST_AS_F.Module_gen_ins; h: AST_WalkRep.Handle)
     RAISES ANY =
  VAR
    m: M3AST_AS.Used_interface_id;
    iter := SeqM3AST_AS_Used_interface_id.NewIter(n.as_export_s);
    iter1 := SeqM3AST_AS_Used_interface_id.NewIter(n.as_id_s);
  BEGIN
    IF n.as_unsafe # NIL THEN h.Visit(n.as_unsafe); END;
    h.Visit(n.as_id);
    WHILE SeqM3AST_AS_Used_interface_id.Next(iter, m) DO h.Visit(m); END;
    h.Visit(n.as_gen_id);
    WHILE SeqM3AST_AS_Used_interface_id.Next(iter1, m) DO h.Visit(m); END;
  END Module_gen_ins;


PROCEDURE Simple_import(n: M3AST_AS_F.Simple_import; h: AST_WalkRep.Handle)
     RAISES ANY =
  VAR
    m: M3AST_AS.Import_item;
    iter := SeqM3AST_AS_Import_item.NewIter(n.as_import_item_s);
  BEGIN
    WHILE SeqM3AST_AS_Import_item.Next(iter, m) DO h.Visit(m); END;
  END Simple_import;

PROCEDURE Import_item(n: M3AST_AS_F.Import_item; h: AST_WalkRep.Handle)
     RAISES ANY=
  BEGIN
    h.Visit(n.as_intf_id);
    IF n.as_id # NIL THEN h.Visit(n.as_id) END;
  END Import_item;


PROCEDURE From_import(n: M3AST_AS_F.From_import; h: AST_WalkRep.Handle)
     RAISES ANY =
  VAR
    m: M3AST_AS.Used_def_id;
    iter := SeqM3AST_AS_Used_def_id.NewIter(n.as_id_s);
  BEGIN
    h.Visit(n.as_intf_id);
    WHILE SeqM3AST_AS_Used_def_id.Next(iter, m) DO h.Visit(m); END;
  END From_import;


PROCEDURE Revelation_s(n: M3AST_AS_F.Revelation_s; h: AST_WalkRep.Handle)
     RAISES ANY =
  VAR
    m: M3AST_AS.REVELATION;
    iter := SeqM3AST_AS_REVELATION.NewIter(n.as_reveal_s);
  BEGIN
    WHILE SeqM3AST_AS_REVELATION.Next(iter, m) DO h.Visit(m); END;
  END Revelation_s;


PROCEDURE Const_decl_s(n: M3AST_AS_F.Const_decl_s; h: AST_WalkRep.Handle)
     RAISES ANY =
  VAR
    m: M3AST_AS.Const_decl;
    iter := SeqM3AST_AS_Const_decl.NewIter(n.as_const_decl_s);
  BEGIN
    VisitEXTERNAL_DECL(n.vEXTERNAL_DECL.pg_external, h);
    WHILE SeqM3AST_AS_Const_decl.Next(iter, m) DO h.Visit(m); END;
  END Const_decl_s;


PROCEDURE Type_decl_s(n: M3AST_AS_F.Type_decl_s; h: AST_WalkRep.Handle)
     RAISES ANY =
  VAR
    m: M3AST_AS.TYPE_DECL;
    iter := SeqM3AST_AS_TYPE_DECL.NewIter(n.as_type_decl_s);
  BEGIN
    VisitEXTERNAL_DECL(n.vEXTERNAL_DECL.pg_external, h);
    WHILE SeqM3AST_AS_TYPE_DECL.Next(iter, m) DO h.Visit(m); END;
  END Type_decl_s;


PROCEDURE Var_decl_s(n: M3AST_AS_F.Var_decl_s; h: AST_WalkRep.Handle)
     RAISES ANY =
  VAR
    m: M3AST_AS.Var_decl;
    iter := SeqM3AST_AS_Var_decl.NewIter(n.as_var_decl_s);
  BEGIN
    VisitEXTERNAL_DECL(n.vEXTERNAL_DECL.pg_external, h);
    WHILE SeqM3AST_AS_Var_decl.Next(iter, m) DO h.Visit(m); END;
  END Var_decl_s;


PROCEDURE Exc_decl_s(n: M3AST_AS_F.Exc_decl_s; h: AST_WalkRep.Handle)
     RAISES ANY =
  VAR
    m: M3AST_AS.Exc_decl;
    iter := SeqM3AST_AS_Exc_decl.NewIter(n.as_exc_decl_s);
  BEGIN
    VisitEXTERNAL_DECL(n.vEXTERNAL_DECL.pg_external, h);
    WHILE SeqM3AST_AS_Exc_decl.Next(iter, m) DO h.Visit(m); END;
  END Exc_decl_s;


PROCEDURE Proc_decl(n: M3AST_AS_F.Proc_decl; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    VisitEXTERNAL_DECL(n.vEXTERNAL_DECL.pg_external, h);
    IF n.pg_inline # NIL THEN h.Visit(n.pg_inline); END;
    h.Visit(n.as_id);
    h.Visit(n.as_type);
    IF n.as_body # NIL THEN h.Visit(n.as_body); END;
  END Proc_decl;


PROCEDURE Const_decl(n: M3AST_AS_F.Const_decl; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    h.Visit(n.as_id);
    IF n.as_type # NIL THEN h.Visit(n.as_type); END;
    h.Visit(n.as_exp);
  END Const_decl;


PROCEDURE Var_decl(n: M3AST_AS_F.Var_decl; h: AST_WalkRep.Handle)
     RAISES ANY =
  VAR
    m: M3AST_AS.Var_id;
    iter := SeqM3AST_AS_Var_id.NewIter(n.as_id_s);
  BEGIN
    WHILE SeqM3AST_AS_Var_id.Next(iter, m) DO h.Visit(m); END;
    IF n.as_type # NIL THEN h.Visit(n.as_type); END;
    IF n.as_default # NIL THEN h.Visit(n.as_default); END;
  END Var_decl;


PROCEDURE Exc_decl(n: M3AST_AS_F.Exc_decl; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    h.Visit(n.as_id);
    IF n.as_type # NIL THEN h.Visit(n.as_type); END;
  END Exc_decl;


PROCEDURE Subtype_decl(n: M3AST_AS_F.Subtype_decl; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    h.Visit(n.as_id);
    h.Visit(n.as_type);
  END Subtype_decl;


PROCEDURE Concrete_decl(n: M3AST_AS_F.Concrete_decl; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    h.Visit(n.as_id);
    h.Visit(n.as_type);
  END Concrete_decl;


PROCEDURE Subtype_reveal(
    n: M3AST_AS_F.Subtype_reveal;
    h: AST_WalkRep.Handle) RAISES ANY =
  BEGIN
    h.Visit(n.as_qual_id);
    h.Visit(n.as_type);
  END Subtype_reveal;


PROCEDURE Concrete_reveal(n: M3AST_AS_F.Concrete_reveal;
    h: AST_WalkRep.Handle) RAISES ANY =
  BEGIN
    h.Visit(n.as_qual_id);
    h.Visit(n.as_type);
  END Concrete_reveal;


PROCEDURE Named_type(n: M3AST_AS_F.Named_type; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    h.Visit(n.as_qual_id);
  END Named_type;


PROCEDURE Root_type(n: M3AST_AS_F.Root_type; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    IF n.as_trace_mode # NIL THEN h.Visit(n.as_trace_mode); END;
  END Root_type;


PROCEDURE Enumeration_type(n: M3AST_AS_F.Enumeration_type;
    h: AST_WalkRep.Handle) RAISES ANY =
  VAR
    m: M3AST_AS.Enum_id;
    iter := SeqM3AST_AS_Enum_id.NewIter(n.as_id_s);
  BEGIN
    WHILE SeqM3AST_AS_Enum_id.Next(iter, m) DO h.Visit(m); END;
  END Enumeration_type;


PROCEDURE Subrange_type(n: M3AST_AS_F.Subrange_type; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    h.Visit(n.as_range);
  END Subrange_type;


PROCEDURE Array_type(n: M3AST_AS_F.Array_type; h: AST_WalkRep.Handle)
     RAISES ANY =
  VAR
    m: M3AST_AS.M3TYPE;
    iter := SeqM3AST_AS_M3TYPE.NewIter(n.as_indextype_s);
  BEGIN
    WHILE SeqM3AST_AS_M3TYPE.Next(iter, m) DO h.Visit(m); END;
    h.Visit(n.as_elementtype);
  END Array_type;


PROCEDURE Record_type(n: M3AST_AS_F.Record_type; h: AST_WalkRep.Handle)
     RAISES ANY =
  VAR
    m: M3AST_AS.Fields;
    iter := SeqM3AST_AS_Fields.NewIter(n.as_fields_s);
  BEGIN
    WHILE SeqM3AST_AS_Fields.Next(iter, m) DO h.Visit(m); END;
  END Record_type;


PROCEDURE Object_type(n: M3AST_AS_F.Object_type; h: AST_WalkRep.Handle)
     RAISES ANY =
  VAR
    m: M3AST_AS.Fields;
    iter := SeqM3AST_AS_Fields.NewIter(n.as_fields_s);
    m2: M3AST_AS.Method;
    iter2 := SeqM3AST_AS_Method.NewIter(n.as_method_s);
    m3: M3AST_AS.Override;
    iter3 := SeqM3AST_AS_Override.NewIter(n.as_override_s);
  BEGIN
    IF n.as_ancestor # NIL THEN h.Visit(n.as_ancestor); END;
    IF n.as_brand # NIL THEN h.Visit(n.as_brand); END;
    WHILE SeqM3AST_AS_Fields.Next(iter, m) DO h.Visit(m); END;
    WHILE SeqM3AST_AS_Method.Next(iter2, m2) DO h.Visit(m2); END;
    WHILE SeqM3AST_AS_Override.Next(iter3, m3) DO h.Visit(m3); END;
  END Object_type;


PROCEDURE Set_type(n: M3AST_AS_F.Set_type; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    h.Visit(n.as_type);
  END Set_type;


PROCEDURE Procedure_type(n: M3AST_AS_F.Procedure_type; 
    h: AST_WalkRep.Handle) RAISES ANY =
  VAR
    m: M3AST_AS.Formal_param;
    iter := SeqM3AST_AS_Formal_param.NewIter(n.as_formal_param_s);
  BEGIN
    WHILE SeqM3AST_AS_Formal_param.Next(iter, m) DO h.Visit(m); END;
    IF n.as_result_type # NIL THEN h.Visit(n.as_result_type); END;
    IF n.as_raises # NIL THEN h.Visit(n.as_raises); END;
  END Procedure_type;


PROCEDURE Ref_type(n: M3AST_AS_F.Ref_type; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    IF n.as_trace_mode # NIL THEN h.Visit(n.as_trace_mode); END;
    IF n.as_brand # NIL THEN h.Visit(n.as_brand) END;
    h.Visit(n.as_type);
  END Ref_type;


PROCEDURE Packed_type(n: M3AST_AS_F.Packed_type; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    h.Visit(n.as_exp);
    h.Visit(n.as_type);
  END Packed_type;


PROCEDURE Opaque_type(n: M3AST_AS_F.Opaque_type; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    h.Visit(n.as_type);
  END Opaque_type;


PROCEDURE Brand(
    n: M3AST_AS_F.Brand; h: AST_WalkRep.Handle
    ) RAISES ANY=
  BEGIN
    IF n.as_exp # NIL THEN h.Visit(n.as_exp) END;
  END Brand;


PROCEDURE Fields(n: M3AST_AS_F.Fields; h: AST_WalkRep.Handle)
     RAISES ANY =
  VAR
    m: M3AST_AS.Field_id;
    iter := SeqM3AST_AS_Field_id.NewIter(n.as_id_s);
  BEGIN
    WHILE SeqM3AST_AS_Field_id.Next(iter, m) DO h.Visit(m); END;
    IF n.as_type # NIL THEN h.Visit(n.as_type); END;
    IF n.as_default # NIL THEN h.Visit(n.as_default); END;
  END Fields;


PROCEDURE Method(n: M3AST_AS_F.Method; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    h.Visit(n.as_id);
    h.Visit(n.as_type);
    IF n.as_default # NIL THEN h.Visit(n.as_default); END;
  END Method;


PROCEDURE Override(n: M3AST_AS_F.Override; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    h.Visit(n.as_id);
    h.Visit(n.as_default);
  END Override;


PROCEDURE Formal_param(n: M3AST_AS_F.Formal_param; h: AST_WalkRep.Handle)
     RAISES ANY =
  VAR
    m: M3AST_AS.FORMAL_ID;
    iter := SeqM3AST_AS_FORMAL_ID.NewIter(n.as_id_s);
  BEGIN
    WHILE SeqM3AST_AS_FORMAL_ID.Next(iter, m) DO h.Visit(m); END;
    IF n.as_formal_type # NIL THEN h.Visit(n.as_formal_type); END;
    IF n.as_default # NIL THEN h.Visit(n.as_default); END;
  END Formal_param;


PROCEDURE Raisees_some(n: M3AST_AS_F.Raisees_some; h: AST_WalkRep.Handle)
     RAISES ANY =
  VAR
    m: M3AST_AS.Qual_used_id;
    iter := SeqM3AST_AS_Qual_used_id.NewIter(n.as_raisees_s);
  BEGIN
    WHILE SeqM3AST_AS_Qual_used_id.Next(iter, m) DO h.Visit(m); END;
  END Raisees_some;


PROCEDURE Range(n: M3AST_AS_F.Range; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    h.Visit(n.as_exp1);
    h.Visit(n.as_exp2);
  END Range;


PROCEDURE Range_EXP(n: M3AST_AS_F.Range_EXP; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    h.Visit(n.as_exp);
  END Range_EXP;


PROCEDURE Constructor(n: M3AST_AS_F.Constructor; h: AST_WalkRep.Handle)
     RAISES ANY =
  VAR
    m: M3AST_AS.CONS_ELEM;
    iter := SeqM3AST_AS_CONS_ELEM.NewIter(n.as_element_s);
  BEGIN
    h.Visit(n.as_type);
    WHILE SeqM3AST_AS_CONS_ELEM.Next(iter, m) DO h.Visit(m); END;
    IF n.as_propagate # NIL THEN h.Visit(n.as_propagate); END;
  END Constructor;


PROCEDURE RANGE_EXP_elem(n: M3AST_AS_F.RANGE_EXP_elem; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    h.Visit(n.as_range_exp);
  END RANGE_EXP_elem;


PROCEDURE Actual_elem(n: M3AST_AS_F.Actual_elem; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    h.Visit(n.as_actual);
  END Actual_elem;


PROCEDURE BINARY(n: M3AST_AS_F.BINARY; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    h.Visit(n.as_exp1);
    h.Visit(n.as_exp2);
  END BINARY;


PROCEDURE UNARY(n: M3AST_AS_F.UNARY; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    h.Visit(n.as_exp);
  END UNARY;


PROCEDURE Select(
    n: M3AST_AS_F.Select; h: AST_WalkRep.Handle
    ) RAISES ANY =
  BEGIN
    h.Visit(n.as_exp);
    h.Visit(n.as_id);
  END Select;

PROCEDURE Call(n: M3AST_AS_F.Call; h: AST_WalkRep.Handle)
     RAISES ANY =
  VAR
    m: M3AST_AS.Actual;
    iter := SeqM3AST_AS_Actual.NewIter(n.as_param_s);
  BEGIN
    h.Visit(n.as_callexp);
    WHILE SeqM3AST_AS_Actual.Next(iter, m) DO h.Visit(m); END;
  END Call;


PROCEDURE Index(n: M3AST_AS_F.Index; h: AST_WalkRep.Handle)
     RAISES ANY =
  VAR
    m: M3AST_AS.EXP;
    iter := SeqM3AST_AS_EXP.NewIter(n.as_exp_s);
  BEGIN
    h.Visit(n.as_array);
    WHILE SeqM3AST_AS_EXP.Next(iter, m) DO h.Visit(m); END;
  END Index;


PROCEDURE Actual(n: M3AST_AS_F.Actual; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    IF n.as_id # NIL THEN h.Visit(n.as_id); END;
    h.Visit(n.as_exp_type);
  END Actual;


PROCEDURE Assign_st(n: M3AST_AS_F.Assign_st; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    h.Visit(n.as_lhs_exp);
    h.Visit(n.as_rhs_exp);
  END Assign_st;


PROCEDURE Call_st(n: M3AST_AS_F.Call_st; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    h.Visit(n.as_call);
  END Call_st;


PROCEDURE Case_st(n: M3AST_AS_F.Case_st; h: AST_WalkRep.Handle)
     RAISES ANY =
  VAR
    m: M3AST_AS.Case;
    iter := SeqM3AST_AS_Case.NewIter(n.as_case_s);
  BEGIN
    h.Visit(n.as_exp);
    WHILE SeqM3AST_AS_Case.Next(iter, m) DO h.Visit(m); END;
    IF n.as_else # NIL THEN h.Visit(n.as_else); END;
  END Case_st;


PROCEDURE Eval_st(n: M3AST_AS_F.Eval_st; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    h.Visit(n.as_exp);
  END Eval_st;


PROCEDURE For_st(n: M3AST_AS_F.For_st; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    h.Visit(n.as_id);
    h.Visit(n.as_from);
    h.Visit(n.as_to);
    IF n.as_by # NIL THEN h.Visit(n.as_by); END;
    VisitSeqStm(n.as_stm_s, h);
  END For_st;


PROCEDURE If_st(n: M3AST_AS_F.If_st; h: AST_WalkRep.Handle)
     RAISES ANY =
  VAR
    m: M3AST_AS.Elsif;
    iter := SeqM3AST_AS_Elsif.NewIter(n.as_elsif_s);
  BEGIN
    h.Visit(n.as_exp);
    VisitSeqStm(n.as_stm_s, h);
    WHILE SeqM3AST_AS_Elsif.Next(iter, m) DO h.Visit(m); END;
    IF n.as_else # NIL THEN h.Visit(n.as_else); END;
  END If_st;


PROCEDURE Lock_st(n: M3AST_AS_F.Lock_st; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    h.Visit(n.as_exp);
    VisitSeqStm(n.as_stm_s, h);
  END Lock_st;


PROCEDURE Loop_st(n: M3AST_AS_F.Loop_st; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    VisitSeqStm(n.as_stm_s, h);
  END Loop_st;


PROCEDURE Raise_st(n: M3AST_AS_F.Raise_st; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    h.Visit(n.as_qual_id);
    IF n.as_exp_void # NIL THEN h.Visit(n.as_exp_void); END;
  END Raise_st;


PROCEDURE Repeat_st(n: M3AST_AS_F.Repeat_st; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    VisitSeqStm(n.as_stm_s, h);
    h.Visit(n.as_exp);
  END Repeat_st;


PROCEDURE Return_st(n: M3AST_AS_F.Return_st; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    IF n.as_exp # NIL THEN h.Visit(n.as_exp); END;
  END Return_st;


PROCEDURE Try_st(n: M3AST_AS_F.Try_st; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    VisitSeqStm(n.as_stm_s, h);
    h.Visit(n.as_try_tail);
  END Try_st;


PROCEDURE Typecase_st(n: M3AST_AS_F.Typecase_st; h: AST_WalkRep.Handle)
     RAISES ANY =
  VAR
    m: M3AST_AS.Tcase;
    iter := SeqM3AST_AS_Tcase.NewIter(n.as_tcase_s);
  BEGIN
    h.Visit(n.as_exp);
    WHILE SeqM3AST_AS_Tcase.Next(iter, m) DO h.Visit(m); END;
    IF n.as_else # NIL THEN h.Visit(n.as_else); END;
  END Typecase_st;


PROCEDURE While_st(n: M3AST_AS_F.While_st; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    h.Visit(n.as_exp);
    VisitSeqStm(n.as_stm_s, h);
  END While_st;


PROCEDURE With_st(n: M3AST_AS_F.With_st; h: AST_WalkRep.Handle)
     RAISES ANY =
  VAR
    m: M3AST_AS.Binding;
    iter := SeqM3AST_AS_Binding.NewIter(n.as_binding_s);
  BEGIN
    WHILE SeqM3AST_AS_Binding.Next(iter, m) DO h.Visit(m); END;
    VisitSeqStm(n.as_stm_s, h);
  END With_st;


PROCEDURE Case(
    n: M3AST_AS_F.Case; h: AST_WalkRep.Handle
    ) RAISES ANY=
  VAR
    m: M3AST_AS.RANGE_EXP;
    iter := SeqM3AST_AS_RANGE_EXP.NewIter(n.as_case_label_s);
  BEGIN
    WHILE SeqM3AST_AS_RANGE_EXP.Next(iter, m) DO h.Visit(m); END;
    VisitSeqStm(n.as_stm_s, h);
  END Case;

PROCEDURE Block(n: M3AST_AS_F.Block; h: AST_WalkRep.Handle)
     RAISES ANY =
  VAR
    m: M3AST_AS.DECL_REVL;
    iter := SeqM3AST_AS_DECL_REVL.NewIter(n.as_decl_s);
  BEGIN
    WHILE SeqM3AST_AS_DECL_REVL.Next(iter, m) DO h.Visit(m); END;
    VisitSeqStm(n.as_stm_s, h);
  END Block;


PROCEDURE Else_stm(n: M3AST_AS_F.Else_stm; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    VisitSeqStm(n.as_stm_s, h);
  END Else_stm;


PROCEDURE By(n: M3AST_AS_F.By; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    h.Visit(n.as_exp);
  END By;


PROCEDURE Elsif(n: M3AST_AS_F.Elsif; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    h.Visit(n.as_exp);
    VisitSeqStm(n.as_stm_s, h);
  END Elsif;


PROCEDURE Try_except(n: M3AST_AS_F.Try_except; h: AST_WalkRep.Handle)
     RAISES ANY =
  VAR
    m: M3AST_AS.Handler;
    iter := SeqM3AST_AS_Handler.NewIter(n.as_handler_s);
  BEGIN
    WHILE SeqM3AST_AS_Handler.Next(iter, m) DO h.Visit(m); END;
    IF n.as_else # NIL THEN h.Visit(n.as_else); END;
  END Try_except;


PROCEDURE Try_finally(n: M3AST_AS_F.Try_finally; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    VisitSeqStm(n.as_stm_s, h);
  END Try_finally;


PROCEDURE Tcase(n: M3AST_AS_F.Tcase; h: AST_WalkRep.Handle)
     RAISES ANY =
  VAR
    m: M3AST_AS.M3TYPE;
    iter := SeqM3AST_AS_M3TYPE.NewIter(n.as_type_s);
  BEGIN
    WHILE SeqM3AST_AS_M3TYPE.Next(iter, m) DO h.Visit(m); END;
    IF n.as_id # NIL THEN h.Visit(n.as_id) END;
    VisitSeqStm(n.as_stm_s, h);
  END Tcase;


PROCEDURE Handler(n: M3AST_AS_F.Handler; h: AST_WalkRep.Handle)
     RAISES ANY =
  VAR
    m: M3AST_AS.Qual_used_id;
    iter := SeqM3AST_AS_Qual_used_id.NewIter(n.as_qual_id_s);
  BEGIN
    WHILE SeqM3AST_AS_Qual_used_id.Next(iter, m) DO h.Visit(m); END;
    IF n.as_id # NIL THEN h.Visit(n.as_id); END;
    VisitSeqStm(n.as_stm_s, h);
  END Handler;


PROCEDURE Binding(n: M3AST_AS_F.Binding; h: AST_WalkRep.Handle)
     RAISES ANY =
  BEGIN
    h.Visit(n.as_id);
    h.Visit(n.as_exp);
  END Binding;

BEGIN
END M3AST_AS_Walk.
