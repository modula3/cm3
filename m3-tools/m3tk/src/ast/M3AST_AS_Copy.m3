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

MODULE M3AST_AS_Copy EXPORTS M3AST_AS_Copy, M3AST_PG_Copy;

IMPORT AST, M3AST_LX, M3AST_AS, M3AST_PG;
IMPORT M3AST_LX_F, M3AST_AS_F, M3AST_PG_F;

IMPORT AST_CopyRep;
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
PROCEDURE CopySeqStm(seqStm: SeqM3AST_AS_STM.T; h: AST_CopyRep.Handle
    ): SeqM3AST_AS_STM.T
    RAISES ANY =
  VAR
    m, n_m: M3AST_AS.STM;
    iter := SeqM3AST_AS_STM.NewIter(seqStm);
    n_seqStm := SeqM3AST_AS_STM.Null;
  BEGIN
    WHILE SeqM3AST_AS_STM.Next(iter, m) DO 
      n_m := h.Copy(m); 
      SeqM3AST_AS_STM.AddRear(n_seqStm, n_m);
    END;
    RETURN n_seqStm;
  END CopySeqStm;

(*PRIVATE*)
PROCEDURE CopyEXTERNAL_DECL(n: M3AST_PG.External_NULL;
    h: AST_CopyRep.Handle): M3AST_PG.External_NULL RAISES ANY=
  VAR cn: M3AST_PG.External_NULL := NIL;
  BEGIN
    IF n # NIL THEN 
      cn := h.Copy(n) 
    END;
    RETURN cn;
  END CopyEXTERNAL_DECL;

(*PRIVATE*)
PROCEDURE CopyUNIT_WITH_BODY(n, cn: M3AST_AS_F.UNIT_WITH_BODY; 
    h: AST_CopyRep.Handle) RAISES ANY=
  VAR
    m2: M3AST_AS.IMPORTED;
    iter2 := SeqM3AST_AS_IMPORTED.NewIter(n.as_import_s);
  BEGIN
    WHILE SeqM3AST_AS_IMPORTED.Next(iter2, m2) DO
      SeqM3AST_AS_IMPORTED.AddRear(cn.as_import_s, h.Copy(m2));
    END;
    cn.as_block := h.Copy(n.as_block);
  END CopyUNIT_WITH_BODY;


(*PRIVATE*)
PROCEDURE CopyUNIT_GEN_DEF(n, cn: M3AST_AS_F.UNIT_GEN_DEF; 
    h: AST_CopyRep.Handle) RAISES ANY=
  VAR iter := SeqM3AST_AS_F_Interface_id.NewIter(n.as_id_s);
    m: M3AST_AS.F_Interface_id;
  BEGIN
    WHILE SeqM3AST_AS_F_Interface_id.Next(iter, m) DO
      SeqM3AST_AS_F_Interface_id.AddRear(cn.as_id_s, h.Copy(m)); 
    END;
  END CopyUNIT_GEN_DEF;

PROCEDURE CopySeqUsed_interface_id(s: SeqM3AST_AS_Used_interface_id.T;
    h: AST_CopyRep.Handle): SeqM3AST_AS_Used_interface_id.T RAISES ANY=
  VAR
    m, n_m: M3AST_AS.Used_interface_id;
    iter := SeqM3AST_AS_Used_interface_id.NewIter(s);
    n_s := SeqM3AST_AS_Used_interface_id.Null;
  BEGIN
    WHILE SeqM3AST_AS_Used_interface_id.Next(iter, m) DO 
      n_m := h.Copy(m); 
      SeqM3AST_AS_Used_interface_id.AddRear(n_s, n_m);
    END;
    RETURN n_s;
  END CopySeqUsed_interface_id;


<*INLINE*> PROCEDURE ID(n, cn: M3AST_AS.ID): M3AST_AS.ID RAISES {}=
  BEGIN
    cn.lx_symrep := n.lx_symrep;
    RETURN cn;
  END ID;

PROCEDURE SRC_NODE(n, cn: M3AST_AS.SRC_NODE): M3AST_AS.SRC_NODE RAISES {}=
  BEGIN
    cn.lx_srcpos := n.lx_srcpos;
    RETURN cn;
  END SRC_NODE;

PROCEDURE Module_id(
    n: M3AST_AS_F.Module_id; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN ID(n, SRC_NODE(n, NEW(M3AST_AS.Module_id).init()));
  END Module_id;

PROCEDURE Interface_id(
    n: M3AST_AS_F.Interface_id; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN ID(n, SRC_NODE(n, NEW(M3AST_AS.Interface_id).init()));
  END Interface_id;

PROCEDURE F_Interface_id(
    n: M3AST_AS_F.F_Interface_id; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN ID(n, SRC_NODE(n, NEW(M3AST_AS.F_Interface_id).init()));
  END F_Interface_id;

PROCEDURE Interface_AS_id(
    n: M3AST_AS_F.Interface_AS_id; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN ID(n, SRC_NODE(n, NEW(M3AST_AS.Interface_AS_id).init()));
  END Interface_AS_id;

PROCEDURE F_Value_id(
    n: M3AST_AS_F.F_Value_id; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN ID(n, SRC_NODE(n, NEW(M3AST_AS.F_Value_id).init()));
  END F_Value_id;

PROCEDURE F_Var_id(
    n: M3AST_AS_F.F_Var_id; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN ID(n, SRC_NODE(n, NEW(M3AST_AS.F_Var_id).init()));
  END F_Var_id;

PROCEDURE F_Readonly_id(
    n: M3AST_AS_F.F_Readonly_id; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN ID(n, SRC_NODE(n, NEW(M3AST_AS.F_Readonly_id).init()));
  END F_Readonly_id;

PROCEDURE Type_id(
    n: M3AST_AS_F.Type_id; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN ID(n, SRC_NODE(n, NEW(M3AST_AS.Type_id).init()));
  END Type_id;

PROCEDURE Const_id(
    n: M3AST_AS_F.Const_id; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN ID(n, SRC_NODE(n, NEW(M3AST_AS.Const_id).init()));
  END Const_id;

PROCEDURE Var_id(
    n: M3AST_AS_F.Var_id; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN ID(n, SRC_NODE(n, NEW(M3AST_AS.Var_id).init()));
  END Var_id;

PROCEDURE Proc_id(
    n: M3AST_AS_F.Proc_id; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN ID(n, SRC_NODE(n, NEW(M3AST_AS.Proc_id).init()));
  END Proc_id;

PROCEDURE Enum_id(
    n: M3AST_AS_F.Enum_id; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN ID(n, SRC_NODE(n, NEW(M3AST_AS.Enum_id).init()));
  END Enum_id;

PROCEDURE Method_id(
    n: M3AST_AS_F.Method_id; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN ID(n, SRC_NODE(n, NEW(M3AST_AS.Method_id).init()));
  END Method_id;

PROCEDURE Override_id(
    n: M3AST_AS_F.Override_id; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN ID(n, SRC_NODE(n, NEW(M3AST_AS.Override_id).init()));
  END Override_id;

PROCEDURE Field_id(
    n: M3AST_AS_F.Field_id; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN ID(n, SRC_NODE(n, NEW(M3AST_AS.Field_id).init()));
  END Field_id;

PROCEDURE For_id(
    n: M3AST_AS_F.For_id; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN ID(n, SRC_NODE(n, NEW(M3AST_AS.For_id).init()));
  END For_id;

PROCEDURE Handler_id(
    n: M3AST_AS_F.Handler_id; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN ID(n, SRC_NODE(n, NEW(M3AST_AS.Handler_id).init()));
  END Handler_id;

PROCEDURE Tcase_id(
    n: M3AST_AS_F.Tcase_id; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN ID(n, SRC_NODE(n, NEW(M3AST_AS.Tcase_id).init()));
  END Tcase_id;

PROCEDURE With_id(
    n: M3AST_AS_F.With_id; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN ID(n, SRC_NODE(n, NEW(M3AST_AS.With_id).init()));
  END With_id;

PROCEDURE Exc_id(
    n: M3AST_AS_F.Exc_id; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN ID(n, SRC_NODE(n, NEW(M3AST_AS.Exc_id).init()));
  END Exc_id;

PROCEDURE Used_interface_id(
    n: M3AST_AS_F.Used_interface_id; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN ID(n, SRC_NODE(n, NEW(M3AST_AS.Used_interface_id).init()));
  END Used_interface_id;

PROCEDURE Used_def_id(
    n: M3AST_AS_F.Used_def_id; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN ID(n, SRC_NODE(n, NEW(M3AST_AS.Used_def_id).init()));
  END Used_def_id;

PROCEDURE Qual_used_id(n: M3AST_AS_F.Qual_used_id; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Qual_used_id := SRC_NODE(n, NEW(M3AST_AS.Qual_used_id).init());
  BEGIN
    IF n.as_intf_id # NIL THEN cn.as_intf_id := h.Copy(n.as_intf_id); END;
    cn.as_id := h.Copy(n.as_id);
    RETURN cn;
  END Qual_used_id;


PROCEDURE Compilation_Unit(n: M3AST_AS_F.Compilation_Unit; 
      h: AST_CopyRep.Handle): AST.NODE RAISES ANY =
  VAR cn: M3AST_AS.Compilation_Unit := NEW(M3AST_AS.Compilation_Unit).init();
  BEGIN
    cn.as_root := h.Copy(n.as_root);
    RETURN cn;
  END Compilation_Unit;


PROCEDURE Interface(n: M3AST_AS_F.Interface; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Interface := SRC_NODE(n, NEW(M3AST_AS.Interface).init());
  BEGIN
    cn.vEXTERNAL_DECL.pg_external := 
      CopyEXTERNAL_DECL(n.vEXTERNAL_DECL.pg_external, h); 
    IF n.as_unsafe # NIL THEN cn.as_unsafe := h.Copy(n.as_unsafe); END;
    cn.as_id := h.Copy(n.as_id);
    CopyUNIT_WITH_BODY(n, cn, h);
    RETURN cn;
  END Interface;


PROCEDURE Module(n: M3AST_AS_F.Module; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Module := SRC_NODE(n, NEW(M3AST_AS.Module).init());
  BEGIN
    IF n.as_unsafe # NIL THEN cn.as_unsafe := h.Copy(n.as_unsafe); END;
    cn.as_id := h.Copy(n.as_id);
    cn.as_export_s := CopySeqUsed_interface_id(n.as_export_s, h);
    CopyUNIT_WITH_BODY(n, cn, h);
    RETURN cn;
  END Module;


PROCEDURE Interface_gen_def(n: M3AST_AS_F.Interface_gen_def; 
    h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Interface_gen_def := SRC_NODE(n, NEW(M3AST_AS.Interface_gen_def).init());
  BEGIN
    cn.vEXTERNAL_DECL.pg_external := 
      CopyEXTERNAL_DECL(n.vEXTERNAL_DECL.pg_external, h); 
    cn.as_id := h.Copy(n.as_id);
    CopyUNIT_GEN_DEF(n, cn, h);
    CopyUNIT_WITH_BODY(n, cn, h);
    RETURN cn;
  END Interface_gen_def;


PROCEDURE Module_gen_def(n: M3AST_AS_F.Module_gen_def; 
    h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Module_gen_def := SRC_NODE(n, NEW(M3AST_AS.Module_gen_def).init());
  BEGIN
    cn.as_id := h.Copy(n.as_id);
    CopyUNIT_GEN_DEF(n, cn, h);
    CopyUNIT_WITH_BODY(n, cn, h);
    RETURN cn;
  END Module_gen_def;


PROCEDURE Interface_gen_ins(n: M3AST_AS_F.Interface_gen_ins; 
    h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Interface_gen_ins := SRC_NODE(n, NEW(M3AST_AS.Interface_gen_ins).init());
  BEGIN
    IF n.as_unsafe # NIL THEN cn.as_unsafe := h.Copy(n.as_unsafe); END;
    cn.as_id := h.Copy(n.as_id);
    cn.as_gen_id := h.Copy(n.as_gen_id);
    cn.as_id_s := CopySeqUsed_interface_id(n.as_id_s, h);
    RETURN cn;
  END Interface_gen_ins;


PROCEDURE Module_gen_ins(n: M3AST_AS_F.Module_gen_ins; 
    h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Module_gen_ins := SRC_NODE(n, NEW(M3AST_AS.Module_gen_ins).init());
  BEGIN
    IF n.as_unsafe # NIL THEN cn.as_unsafe := h.Copy(n.as_unsafe); END;
    cn.as_id := h.Copy(n.as_id);
    cn.as_export_s := CopySeqUsed_interface_id(n.as_export_s, h);
    cn.as_gen_id := h.Copy(n.as_gen_id);
    cn.as_id_s := CopySeqUsed_interface_id(n.as_id_s,h);
    RETURN cn;
  END Module_gen_ins;


PROCEDURE Unsafe(
    n: M3AST_AS_F.Unsafe; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN SRC_NODE(n, NEW(M3AST_AS.Unsafe).init());
  END Unsafe;


PROCEDURE Simple_import(n: M3AST_AS_F.Simple_import; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Simple_import := SRC_NODE(n, NEW(M3AST_AS.Simple_import).init());
  VAR
    m: M3AST_AS.Import_item;
    iter := SeqM3AST_AS_Import_item.NewIter(n.as_import_item_s);
  BEGIN
    VAR ns := SeqM3AST_AS_Import_item.Null;
    BEGIN
    WHILE SeqM3AST_AS_Import_item.Next(iter, m) DO
      SeqM3AST_AS_Import_item.AddRear(ns, h.Copy(m));
    END;
    cn.as_import_item_s := ns;
    END;
    RETURN cn;
  END Simple_import;

PROCEDURE Import_item(
    n: M3AST_AS_F.Import_item; h: AST_CopyRep.Handle;
    ): AST.NODE RAISES ANY =
  VAR cn: M3AST_AS.Import_item := SRC_NODE(n, NEW(M3AST_AS.Import_item).init());
  BEGIN
    cn.as_intf_id := h.Copy(n.as_intf_id);
    IF n.as_id # NIL THEN cn.as_id := h.Copy(n.as_id); END;
    RETURN cn;
  END Import_item;

PROCEDURE From_import(n: M3AST_AS_F.From_import; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.From_import := SRC_NODE(n, NEW(M3AST_AS.From_import).init());
  VAR
    m: M3AST_AS.Used_def_id;
    iter := SeqM3AST_AS_Used_def_id.NewIter(n.as_id_s);
  BEGIN
    cn.as_intf_id := h.Copy(n.as_intf_id);
    VAR ns := SeqM3AST_AS_Used_def_id.Null;
    BEGIN
    WHILE SeqM3AST_AS_Used_def_id.Next(iter, m) DO
      SeqM3AST_AS_Used_def_id.AddRear(ns, h.Copy(m));
    END;
    cn.as_id_s := ns;
    END;
    RETURN cn;
  END From_import;


PROCEDURE Revelation_s(n: M3AST_AS_F.Revelation_s; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Revelation_s := SRC_NODE(n, NEW(M3AST_AS.Revelation_s).init());
  VAR
    m: M3AST_AS.REVELATION;
    iter := SeqM3AST_AS_REVELATION.NewIter(n.as_reveal_s);
  BEGIN
    VAR ns := SeqM3AST_AS_REVELATION.Null;
    BEGIN
    WHILE SeqM3AST_AS_REVELATION.Next(iter, m) DO
      SeqM3AST_AS_REVELATION.AddRear(ns, h.Copy(m));
    END;
    cn.as_reveal_s := ns;
    END;
    RETURN cn;
  END Revelation_s;


PROCEDURE Const_decl_s(n: M3AST_AS_F.Const_decl_s; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Const_decl_s := SRC_NODE(n, NEW(M3AST_AS.Const_decl_s).init());
  VAR
    m: M3AST_AS.Const_decl;
    iter := SeqM3AST_AS_Const_decl.NewIter(n.as_const_decl_s);
  BEGIN
    cn.vEXTERNAL_DECL.pg_external := 
      CopyEXTERNAL_DECL(n.vEXTERNAL_DECL.pg_external, h);
    VAR ns := SeqM3AST_AS_Const_decl.Null;
    BEGIN
    WHILE SeqM3AST_AS_Const_decl.Next(iter, m) DO
      SeqM3AST_AS_Const_decl.AddRear(ns, h.Copy(m));
    END;
    cn.as_const_decl_s := ns;
    END;
    RETURN cn;
  END Const_decl_s;


PROCEDURE Type_decl_s(n: M3AST_AS_F.Type_decl_s; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Type_decl_s := SRC_NODE(n, NEW(M3AST_AS.Type_decl_s).init());
  VAR
    m: M3AST_AS.TYPE_DECL;
    iter := SeqM3AST_AS_TYPE_DECL.NewIter(n.as_type_decl_s);
  BEGIN
    cn.vEXTERNAL_DECL.pg_external := 
      CopyEXTERNAL_DECL(n.vEXTERNAL_DECL.pg_external, h);
    VAR ns := SeqM3AST_AS_TYPE_DECL.Null;
    BEGIN
    WHILE SeqM3AST_AS_TYPE_DECL.Next(iter, m) DO
      SeqM3AST_AS_TYPE_DECL.AddRear(ns, h.Copy(m));
    END;
    cn.as_type_decl_s := ns;
    END;
    RETURN cn;
  END Type_decl_s;


PROCEDURE Var_decl_s(n: M3AST_AS_F.Var_decl_s; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Var_decl_s := SRC_NODE(n, NEW(M3AST_AS.Var_decl_s).init());
  VAR
    m: M3AST_AS.Var_decl;
    iter := SeqM3AST_AS_Var_decl.NewIter(n.as_var_decl_s);
  BEGIN
    cn.vEXTERNAL_DECL.pg_external := 
      CopyEXTERNAL_DECL(n.vEXTERNAL_DECL.pg_external, h);
    VAR ns := SeqM3AST_AS_Var_decl.Null;
    BEGIN
    WHILE SeqM3AST_AS_Var_decl.Next(iter, m) DO
      SeqM3AST_AS_Var_decl.AddRear(ns, h.Copy(m));
    END;
    cn.as_var_decl_s := ns;
    END;
    RETURN cn;
  END Var_decl_s;


PROCEDURE Exc_decl_s(n: M3AST_AS_F.Exc_decl_s; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Exc_decl_s := SRC_NODE(n, NEW(M3AST_AS.Exc_decl_s).init());
  VAR
    m: M3AST_AS.Exc_decl;
    iter := SeqM3AST_AS_Exc_decl.NewIter(n.as_exc_decl_s);
  BEGIN
    cn.vEXTERNAL_DECL.pg_external := 
      CopyEXTERNAL_DECL(n.vEXTERNAL_DECL.pg_external, h);
    VAR ns := SeqM3AST_AS_Exc_decl.Null;
    BEGIN
    WHILE SeqM3AST_AS_Exc_decl.Next(iter, m) DO
      SeqM3AST_AS_Exc_decl.AddRear(ns, h.Copy(m));
    END;
    cn.as_exc_decl_s := ns;
    END;
    RETURN cn;
  END Exc_decl_s;


PROCEDURE Proc_decl(n: M3AST_AS_F.Proc_decl; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Proc_decl := SRC_NODE(n, NEW(M3AST_AS.Proc_decl).init());
  BEGIN
    cn.vEXTERNAL_DECL.pg_external := 
      CopyEXTERNAL_DECL(n.vEXTERNAL_DECL.pg_external, h);
    IF n.pg_inline # NIL THEN cn.pg_inline := h.Copy(n.pg_inline); END;
    cn.as_id := h.Copy(n.as_id);
    cn.as_type := h.Copy(n.as_type);
    IF n.as_body # NIL THEN cn.as_body := h.Copy(n.as_body); END;
    RETURN cn;
  END Proc_decl;


PROCEDURE Const_decl(n: M3AST_AS_F.Const_decl; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Const_decl := SRC_NODE(n, NEW(M3AST_AS.Const_decl).init());
  BEGIN
    cn.as_id := h.Copy(n.as_id);
    IF n.as_type # NIL THEN cn.as_type := h.Copy(n.as_type); END;
    cn.as_exp := h.Copy(n.as_exp);
    RETURN cn;
  END Const_decl;


PROCEDURE Var_decl(n: M3AST_AS_F.Var_decl; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Var_decl := SRC_NODE(n, NEW(M3AST_AS.Var_decl).init());
  VAR
    m: M3AST_AS.Var_id;
    iter := SeqM3AST_AS_Var_id.NewIter(n.as_id_s);
  BEGIN
    VAR ns := SeqM3AST_AS_Var_id.Null;
    BEGIN
    WHILE SeqM3AST_AS_Var_id.Next(iter, m) DO
      SeqM3AST_AS_Var_id.AddRear(ns, h.Copy(m));
    END;
    cn.as_id_s := ns;
    END;
    IF n.as_type # NIL THEN cn.as_type := h.Copy(n.as_type); END;
    IF n.as_default # NIL THEN cn.as_default := h.Copy(n.as_default); END;
    RETURN cn;
  END Var_decl;


PROCEDURE Exc_decl(n: M3AST_AS_F.Exc_decl; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Exc_decl := SRC_NODE(n, NEW(M3AST_AS.Exc_decl).init());
  BEGIN
    cn.as_id := h.Copy(n.as_id);
    IF n.as_type # NIL THEN cn.as_type := h.Copy(n.as_type); END;
    RETURN cn;
  END Exc_decl;


PROCEDURE Subtype_decl(n: M3AST_AS_F.Subtype_decl; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Subtype_decl := SRC_NODE(n, NEW(M3AST_AS.Subtype_decl).init());
  BEGIN
    cn.as_id := h.Copy(n.as_id);
    cn.as_type := h.Copy(n.as_type);
    RETURN cn;
  END Subtype_decl;


PROCEDURE Concrete_decl(n: M3AST_AS_F.Concrete_decl; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Concrete_decl := SRC_NODE(n, NEW(M3AST_AS.Concrete_decl).init());
  BEGIN
    cn.as_id := h.Copy(n.as_id);
    cn.as_type := h.Copy(n.as_type);
    RETURN cn;
  END Concrete_decl;


PROCEDURE Subtype_reveal(
    n: M3AST_AS_F.Subtype_reveal;
    h: AST_CopyRep.Handle): AST.NODE RAISES ANY =
  VAR cn: M3AST_AS.Subtype_reveal := SRC_NODE(n, NEW(M3AST_AS.Subtype_reveal).init());
  BEGIN
    cn.as_qual_id := h.Copy(n.as_qual_id);
    cn.as_type := h.Copy(n.as_type);
    RETURN cn;
  END Subtype_reveal;


PROCEDURE Concrete_reveal(n: M3AST_AS_F.Concrete_reveal;
    h: AST_CopyRep.Handle): AST.NODE RAISES ANY =
  VAR cn: M3AST_AS.Concrete_reveal := SRC_NODE(n, NEW(M3AST_AS.Concrete_reveal).init());
  BEGIN
    cn.as_qual_id := h.Copy(n.as_qual_id);
    cn.as_type := h.Copy(n.as_type);
    RETURN cn;
  END Concrete_reveal;


PROCEDURE Named_type(n: M3AST_AS_F.Named_type; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Named_type := SRC_NODE(n, NEW(M3AST_AS.Named_type).init());
  BEGIN
    cn.as_qual_id := h.Copy(n.as_qual_id);
    RETURN cn;
  END Named_type;


PROCEDURE Integer_type(
    n: M3AST_AS_F.Integer_type; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN SRC_NODE(n, NEW(M3AST_AS.Integer_type).init());
  END Integer_type;

PROCEDURE Real_type(
    n: M3AST_AS_F.Real_type; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN SRC_NODE(n, NEW(M3AST_AS.Real_type).init());
  END Real_type;

PROCEDURE LongReal_type(
    n: M3AST_AS_F.LongReal_type; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN SRC_NODE(n, NEW(M3AST_AS.LongReal_type).init());
  END LongReal_type;

PROCEDURE Extended_type(
    n: M3AST_AS_F.Extended_type; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN SRC_NODE(n, NEW(M3AST_AS.Extended_type).init());
  END Extended_type;

PROCEDURE Null_type(
    n: M3AST_AS_F.Null_type; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN SRC_NODE(n, NEW(M3AST_AS.Null_type).init());
  END Null_type;

PROCEDURE RefAny_type(
    n: M3AST_AS_F.RefAny_type; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN SRC_NODE(n, NEW(M3AST_AS.RefAny_type).init());
  END RefAny_type;

PROCEDURE Address_type(
    n: M3AST_AS_F.Address_type; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN SRC_NODE(n, NEW(M3AST_AS.Address_type).init());
  END Address_type;

PROCEDURE Root_type(
    n: M3AST_AS_F.Root_type; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  VAR cn: M3AST_AS.Root_type := SRC_NODE(n, NEW(M3AST_AS.Root_type).init());
  BEGIN
    cn.as_trace_mode := n.as_trace_mode;
    RETURN cn;
  END Root_type;

PROCEDURE Enumeration_type(n: M3AST_AS_F.Enumeration_type;
    h: AST_CopyRep.Handle): AST.NODE RAISES ANY =
  VAR cn: M3AST_AS.Enumeration_type := SRC_NODE(n, NEW(M3AST_AS.Enumeration_type).init());
  VAR
    m: M3AST_AS.Enum_id;
    iter := SeqM3AST_AS_Enum_id.NewIter(n.as_id_s);
  BEGIN
    VAR ns := SeqM3AST_AS_Enum_id.Null;
    BEGIN
    WHILE SeqM3AST_AS_Enum_id.Next(iter, m) DO
      SeqM3AST_AS_Enum_id.AddRear(ns, h.Copy(m));
    END;
    cn.as_id_s := ns;
    END;
    RETURN cn;
  END Enumeration_type;


PROCEDURE Subrange_type(n: M3AST_AS_F.Subrange_type; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Subrange_type := SRC_NODE(n, NEW(M3AST_AS.Subrange_type).init());
  BEGIN
    cn.as_range := h.Copy(n.as_range);
    RETURN cn;
  END Subrange_type;


PROCEDURE Array_type(n: M3AST_AS_F.Array_type; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Array_type := SRC_NODE(n, NEW(M3AST_AS.Array_type).init());
  VAR
    m: M3AST_AS.M3TYPE;
    iter := SeqM3AST_AS_M3TYPE.NewIter(n.as_indextype_s);
  BEGIN
    VAR ns := SeqM3AST_AS_M3TYPE.Null;
    BEGIN
    WHILE SeqM3AST_AS_M3TYPE.Next(iter, m) DO
      SeqM3AST_AS_M3TYPE.AddRear(ns, h.Copy(m));
    END;
    cn.as_indextype_s := ns;
    END;
    cn.as_elementtype := h.Copy(n.as_elementtype);
    RETURN cn;
  END Array_type;


PROCEDURE Record_type(n: M3AST_AS_F.Record_type; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Record_type := SRC_NODE(n, NEW(M3AST_AS.Record_type).init());
  VAR
    m: M3AST_AS.Fields;
    iter := SeqM3AST_AS_Fields.NewIter(n.as_fields_s);
  BEGIN
    VAR ns := SeqM3AST_AS_Fields.Null;
    BEGIN
    WHILE SeqM3AST_AS_Fields.Next(iter, m) DO
      SeqM3AST_AS_Fields.AddRear(ns, h.Copy(m));
    END;
    cn.as_fields_s := ns;
    END;
        RETURN cn;
  END Record_type;


PROCEDURE Object_type(n: M3AST_AS_F.Object_type; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Object_type := SRC_NODE(n, NEW(M3AST_AS.Object_type).init());
  VAR
    m: M3AST_AS.Fields;
    iter := SeqM3AST_AS_Fields.NewIter(n.as_fields_s);
    m2: M3AST_AS.Method;
    iter2 := SeqM3AST_AS_Method.NewIter(n.as_method_s);
    m3: M3AST_AS.Override;
    iter3 := SeqM3AST_AS_Override.NewIter(n.as_override_s);
  BEGIN
    IF n.as_ancestor # NIL THEN cn.as_ancestor := h.Copy(n.as_ancestor); END;
    IF n.as_brand # NIL THEN cn.as_brand := h.Copy(n.as_brand); END;
    VAR ns := SeqM3AST_AS_Fields.Null;
    BEGIN
    WHILE SeqM3AST_AS_Fields.Next(iter, m) DO
      SeqM3AST_AS_Fields.AddRear(ns, h.Copy(m));
    END;
    cn.as_fields_s := ns;
    END;
    VAR ns := SeqM3AST_AS_Method.Null;
    BEGIN
    WHILE SeqM3AST_AS_Method.Next(iter2, m2) DO
      SeqM3AST_AS_Method.AddRear(ns, h.Copy(m2));
    END;
    cn.as_method_s := ns;
    END;
    VAR ns := SeqM3AST_AS_Override.Null;
    BEGIN
    WHILE SeqM3AST_AS_Override.Next(iter3, m3) DO
      SeqM3AST_AS_Override.AddRear(ns, h.Copy(m3));
    END;
    cn.as_override_s := ns;
    END;
        RETURN cn;
  END Object_type;


PROCEDURE Set_type(n: M3AST_AS_F.Set_type; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Set_type := SRC_NODE(n, NEW(M3AST_AS.Set_type).init());
  BEGIN
    cn.as_type := h.Copy(n.as_type);
    RETURN cn;
  END Set_type;


PROCEDURE Procedure_type(n: M3AST_AS_F.Procedure_type; 
    h: AST_CopyRep.Handle): AST.NODE RAISES ANY =
  VAR cn: M3AST_AS.Procedure_type := SRC_NODE(n, NEW(M3AST_AS.Procedure_type).init());
  VAR
    m: M3AST_AS.Formal_param;
    iter := SeqM3AST_AS_Formal_param.NewIter(n.as_formal_param_s);
  BEGIN
    VAR ns := SeqM3AST_AS_Formal_param.Null;
    BEGIN
    WHILE SeqM3AST_AS_Formal_param.Next(iter, m) DO
      SeqM3AST_AS_Formal_param.AddRear(ns, h.Copy(m));
    END;
    cn.as_formal_param_s := ns;
    END;
    IF n.as_result_type # NIL THEN cn.as_result_type := h.Copy(n.as_result_type); END;
    IF n.as_raises # NIL THEN cn.as_raises := h.Copy(n.as_raises); END;
    RETURN cn;
  END Procedure_type;


PROCEDURE Ref_type(n: M3AST_AS_F.Ref_type; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Ref_type := SRC_NODE(n, NEW(M3AST_AS.Ref_type).init());
  BEGIN
    IF n.as_trace_mode # NIL THEN cn.as_trace_mode := h.Copy(n.as_trace_mode); END;
    IF n.as_brand # NIL THEN cn.as_brand := h.Copy(n.as_brand) END;
    cn.as_type := h.Copy(n.as_type);
    RETURN cn;
  END Ref_type;


PROCEDURE Packed_type(n: M3AST_AS_F.Packed_type; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Packed_type := SRC_NODE(n, NEW(M3AST_AS.Packed_type).init());
  BEGIN
    cn.as_exp := h.Copy(n.as_exp);
    cn.as_type := h.Copy(n.as_type);
    RETURN cn;
  END Packed_type;


PROCEDURE Opaque_type(n: M3AST_AS_F.Opaque_type; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Opaque_type := SRC_NODE(n, NEW(M3AST_AS.Opaque_type).init());
  BEGIN
    cn.as_type := h.Copy(n.as_type);
    RETURN cn;
  END Opaque_type;


PROCEDURE Brand(
    n: M3AST_AS_F.Brand; h: AST_CopyRep.Handle
    ): AST.NODE RAISES ANY=
  VAR cn: M3AST_AS.Brand := SRC_NODE(n, NEW(M3AST_AS.Brand).init());
  BEGIN
    IF n.as_exp # NIL THEN cn.as_exp := h.Copy(n.as_exp) END;
    RETURN cn;
  END Brand;


PROCEDURE Untraced(
    n: M3AST_AS_F.Untraced; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN SRC_NODE(n, NEW(M3AST_AS.Untraced).init());
  END Untraced;


PROCEDURE Fields(n: M3AST_AS_F.Fields; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Fields := SRC_NODE(n, NEW(M3AST_AS.Fields).init());
  VAR
    m: M3AST_AS.Field_id;
    iter := SeqM3AST_AS_Field_id.NewIter(n.as_id_s);
  BEGIN
    VAR ns := SeqM3AST_AS_Field_id.Null;
    BEGIN
    WHILE SeqM3AST_AS_Field_id.Next(iter, m) DO
      SeqM3AST_AS_Field_id.AddRear(ns, h.Copy(m));
    END;
    cn.as_id_s := ns;
    END;
    IF n.as_type # NIL THEN cn.as_type := h.Copy(n.as_type); END;
    IF n.as_default # NIL THEN cn.as_default := h.Copy(n.as_default); END;
    RETURN cn;
  END Fields;


PROCEDURE Method(n: M3AST_AS_F.Method; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Method := SRC_NODE(n, NEW(M3AST_AS.Method).init());
  BEGIN
    cn.as_id := h.Copy(n.as_id);
    IF n.as_type # NIL THEN cn.as_type := h.Copy(n.as_type); END;
    IF n.as_default # NIL THEN cn.as_default := h.Copy(n.as_default); END;
    RETURN cn;
  END Method;


PROCEDURE Override(n: M3AST_AS_F.Override; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Override := SRC_NODE(n, NEW(M3AST_AS.Override).init());
  BEGIN
    cn.as_id := h.Copy(n.as_id);
    IF n.as_default # NIL THEN cn.as_default := h.Copy(n.as_default); END;
    RETURN cn;
  END Override;


PROCEDURE Inline(
    n: M3AST_PG_F.Inline; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN SRC_NODE(n, NEW(M3AST_PG.Inline).init());
  END Inline;


PROCEDURE Formal_param(n: M3AST_AS_F.Formal_param; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Formal_param := SRC_NODE(n, NEW(M3AST_AS.Formal_param).init());
  VAR
    m: M3AST_AS.FORMAL_ID;
    iter := SeqM3AST_AS_FORMAL_ID.NewIter(n.as_id_s);
  BEGIN
    VAR ns := SeqM3AST_AS_FORMAL_ID.Null;
    BEGIN
    WHILE SeqM3AST_AS_FORMAL_ID.Next(iter, m) DO
      SeqM3AST_AS_FORMAL_ID.AddRear(ns, h.Copy(m));
    END;
    cn.as_id_s := ns;
    END;
    IF n.as_formal_type # NIL THEN cn.as_formal_type := h.Copy(n.as_formal_type); END;
    IF n.as_default # NIL THEN cn.as_default := h.Copy(n.as_default); END;
    RETURN cn;
  END Formal_param;


PROCEDURE Raisees_some(n: M3AST_AS_F.Raisees_some; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Raisees_some := SRC_NODE(n, NEW(M3AST_AS.Raisees_some).init());
  VAR
    m: M3AST_AS.Qual_used_id;
    iter := SeqM3AST_AS_Qual_used_id.NewIter(n.as_raisees_s);
  BEGIN
    VAR ns := SeqM3AST_AS_Qual_used_id.Null;
    BEGIN
    WHILE SeqM3AST_AS_Qual_used_id.Next(iter, m) DO
      SeqM3AST_AS_Qual_used_id.AddRear(ns, h.Copy(m));
    END;
    cn.as_raisees_s := ns;
    END;
    RETURN cn;
  END Raisees_some;


PROCEDURE Raisees_any(n: M3AST_AS_F.Raisees_any; 
    <*UNUSED*> h: AST_CopyRep.Handle): AST.NODE =
  VAR cn: M3AST_AS.Raisees_any := SRC_NODE(n, NEW(M3AST_AS.Raisees_any).init());
  BEGIN
    RETURN cn;
  END Raisees_any;


PROCEDURE Range(n: M3AST_AS_F.Range; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Range := SRC_NODE(n, NEW(M3AST_AS.Range).init());
  BEGIN
    cn.as_exp1 := h.Copy(n.as_exp1);
    cn.as_exp2 := h.Copy(n.as_exp2);
    RETURN cn;
  END Range;


PROCEDURE Range_EXP(n: M3AST_AS_F.Range_EXP; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Range_EXP := SRC_NODE(n, NEW(M3AST_AS.Range_EXP).init());
  BEGIN
    cn.as_exp := h.Copy(n.as_exp);
    RETURN cn;
  END Range_EXP;

PROCEDURE LITERAL(n, cn: M3AST_LX.LITERAL): M3AST_AS.LITERAL=
  BEGIN
    cn.lx_litrep := n.lx_litrep;
    RETURN cn;
  END LITERAL;


PROCEDURE Integer_literal(
    n: M3AST_AS_F.Integer_literal; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN LITERAL(n, SRC_NODE(n, NEW(M3AST_AS.Integer_literal).init()));
  END Integer_literal;


PROCEDURE Real_literal(
    n: M3AST_AS_F.Real_literal; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN LITERAL(n, SRC_NODE(n, NEW(M3AST_AS.Real_literal).init()));
  END Real_literal;


PROCEDURE LongReal_literal(
    n: M3AST_AS_F.LongReal_literal; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN LITERAL(n, SRC_NODE(n, NEW(M3AST_AS.LongReal_literal).init()));
  END LongReal_literal;


PROCEDURE Extended_literal(
    n: M3AST_AS_F.Extended_literal; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN LITERAL(n, SRC_NODE(n, NEW(M3AST_AS.Extended_literal).init()));
  END Extended_literal;


PROCEDURE Nil_literal(
    n: M3AST_AS_F.Nil_literal; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN LITERAL(n, SRC_NODE(n, NEW(M3AST_AS.Nil_literal).init()));
  END Nil_literal;


PROCEDURE Text_literal(
    n: M3AST_AS_F.Text_literal; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN LITERAL(n, SRC_NODE(n, NEW(M3AST_AS.Text_literal).init()));
  END Text_literal;


PROCEDURE Char_literal(
    n: M3AST_AS_F.Char_literal; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN LITERAL(n, SRC_NODE(n, NEW(M3AST_AS.Char_literal).init()));
  END Char_literal;


PROCEDURE Exp_used_id(
    n: M3AST_AS_F.Exp_used_id; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  VAR
    cn: M3AST_AS.Exp_used_id;
  BEGIN
    cn := SRC_NODE(n, NEW(M3AST_AS.Exp_used_id).init());
    EVAL ID(n.vUSED_ID, cn.vUSED_ID);
    RETURN cn;
  END Exp_used_id;


PROCEDURE Constructor(n: M3AST_AS_F.Constructor; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Constructor := SRC_NODE(n, NEW(M3AST_AS.Constructor).init());
  VAR
    m: M3AST_AS.CONS_ELEM;
    iter := SeqM3AST_AS_CONS_ELEM.NewIter(n.as_element_s);
  BEGIN
    cn.as_type := h.Copy(n.as_type);
    VAR ns := SeqM3AST_AS_CONS_ELEM.Null;
    BEGIN
    WHILE SeqM3AST_AS_CONS_ELEM.Next(iter, m) DO
      SeqM3AST_AS_CONS_ELEM.AddRear(ns, h.Copy(m));
    END;
    cn.as_element_s := ns;
    END;
    IF n.as_propagate # NIL THEN cn.as_propagate := h.Copy(n.as_propagate); END;
    RETURN cn;
  END Constructor;


PROCEDURE RANGE_EXP_elem(n: M3AST_AS_F.RANGE_EXP_elem; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.RANGE_EXP_elem := SRC_NODE(n, NEW(M3AST_AS.RANGE_EXP_elem).init());
  BEGIN
    cn.as_range_exp := h.Copy(n.as_range_exp);
    RETURN cn;
  END RANGE_EXP_elem;


PROCEDURE Actual_elem(n: M3AST_AS_F.Actual_elem; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Actual_elem := SRC_NODE(n, NEW(M3AST_AS.Actual_elem).init());
  BEGIN
    cn.as_actual := h.Copy(n.as_actual);
    RETURN cn;
  END Actual_elem;


PROCEDURE Propagate(
    n: M3AST_AS_F.Propagate; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN SRC_NODE(n, NEW(M3AST_AS.Propagate).init());
  END Propagate;

PROCEDURE BINARY(n, cn: M3AST_AS_F.BINARY; h: AST_CopyRep.Handle
                ): M3AST_AS.BINARY=
  <*FATAL ANY*>
  BEGIN
    cn.as_exp1 := h.Copy(n.as_exp1);
    cn.as_exp2 := h.Copy(n.as_exp2);
    RETURN cn;
  END BINARY;


PROCEDURE Plus(
    n: M3AST_AS_F.Plus; h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN BINARY(n, SRC_NODE(n, NEW(M3AST_AS.Plus).init()), h);
  END Plus;


PROCEDURE Minus(
    n: M3AST_AS_F.Minus; h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN BINARY(n, SRC_NODE(n, NEW(M3AST_AS.Minus).init()), h);
  END Minus;

PROCEDURE Times(
    n: M3AST_AS_F.Times; h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN BINARY(n, SRC_NODE(n, NEW(M3AST_AS.Times).init()), h);
  END Times;

PROCEDURE Rdiv(
    n: M3AST_AS_F.Rdiv; h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN BINARY(n, SRC_NODE(n, NEW(M3AST_AS.Rdiv).init()), h);
  END Rdiv;

PROCEDURE Textcat(
    n: M3AST_AS_F.Textcat; h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN BINARY(n, SRC_NODE(n, NEW(M3AST_AS.Textcat).init()), h);
  END Textcat;

PROCEDURE Div(
    n: M3AST_AS_F.Div; h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN BINARY(n, SRC_NODE(n, NEW(M3AST_AS.Div).init()), h);
  END Div;

PROCEDURE Mod(
    n: M3AST_AS_F.Mod; h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN BINARY(n, SRC_NODE(n, NEW(M3AST_AS.Mod).init()), h);
  END Mod;

PROCEDURE Eq(
    n: M3AST_AS_F.Eq; h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN BINARY(n, SRC_NODE(n, NEW(M3AST_AS.Eq).init()), h);
  END Eq;

PROCEDURE Ne(
    n: M3AST_AS_F.Ne; h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN BINARY(n, SRC_NODE(n, NEW(M3AST_AS.Ne).init()), h);
  END Ne;

PROCEDURE Gt(
    n: M3AST_AS_F.Gt; h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN BINARY(n, SRC_NODE(n, NEW(M3AST_AS.Gt).init()), h);
  END Gt;

PROCEDURE Lt(
    n: M3AST_AS_F.Lt; h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN BINARY(n, SRC_NODE(n, NEW(M3AST_AS.Lt).init()), h);
  END Lt;

PROCEDURE Ge(
    n: M3AST_AS_F.Ge; h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN BINARY(n, SRC_NODE(n, NEW(M3AST_AS.Ge).init()), h);
  END Ge;

PROCEDURE Le(
    n: M3AST_AS_F.Le; h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN BINARY(n, SRC_NODE(n, NEW(M3AST_AS.Le).init()), h);
  END Le;

PROCEDURE And(
    n: M3AST_AS_F.And; h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN BINARY(n, SRC_NODE(n, NEW(M3AST_AS.And).init()), h);
  END And;

PROCEDURE Or(
    n: M3AST_AS_F.Or; h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN BINARY(n, SRC_NODE(n, NEW(M3AST_AS.Or).init()), h);
  END Or;

PROCEDURE In(
    n: M3AST_AS_F.In; h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN BINARY(n, SRC_NODE(n, NEW(M3AST_AS.In).init()), h);
  END In;

PROCEDURE Select(
    n: M3AST_AS_F.Select; h: AST_CopyRep.Handle;
    ): AST.NODE=
  <*FATAL ANY*>
  VAR cn: M3AST_AS.Select := SRC_NODE(n, NEW(M3AST_AS.Select).init());
  BEGIN
    cn.as_exp := h.Copy(n.as_exp);
    cn.as_id := h.Copy(n.as_id);
    RETURN cn;
  END Select;

PROCEDURE UNARY(n, cn: M3AST_AS_F.UNARY; h: AST_CopyRep.Handle
               ): M3AST_AS_F.UNARY=
  <*FATAL ANY*>
  BEGIN
    cn.as_exp := h.Copy(n.as_exp);
    RETURN cn;
  END UNARY;


PROCEDURE Not(
    n: M3AST_AS_F.Not; h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN UNARY(n, SRC_NODE(n, NEW(M3AST_AS.Not).init()), h);
  END Not;


PROCEDURE Unaryplus(
    n: M3AST_AS_F.Unaryplus; h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN UNARY(n, SRC_NODE(n, NEW(M3AST_AS.Unaryplus).init()), h);
  END Unaryplus;

PROCEDURE Unaryminus(
    n: M3AST_AS_F.Unaryminus; h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN UNARY(n, SRC_NODE(n, NEW(M3AST_AS.Unaryminus).init()), h);
  END Unaryminus;

PROCEDURE Deref(
    n: M3AST_AS_F.Deref; h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN UNARY(n, SRC_NODE(n, NEW(M3AST_AS.Deref).init()), h);
  END Deref;

PROCEDURE Call(n: M3AST_AS_F.Call; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Call := SRC_NODE(n, NEW(M3AST_AS.Call).init());
  BEGIN
    RETURN EitherCall(n, cn, h);
  END Call;

PROCEDURE EitherCall(n: M3AST_AS_F.Call;
    cn: M3AST_AS.Call; h: AST_CopyRep.Handle): AST.NODE
    RAISES ANY =
  VAR
    m: M3AST_AS.Actual;
    iter := SeqM3AST_AS_Actual.NewIter(n.as_param_s);
  BEGIN
    cn.as_callexp := h.Copy(n.as_callexp);
    VAR ns := SeqM3AST_AS_Actual.Null;
    BEGIN
    WHILE SeqM3AST_AS_Actual.Next(iter, m) DO
      SeqM3AST_AS_Actual.AddRear(ns, h.Copy(m));
    END;
    cn.as_param_s := ns;
    END;
    RETURN cn;
  END EitherCall;

PROCEDURE NEWCall(n: M3AST_AS_F.NEWCall; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.NEWCall := SRC_NODE(n, NEW(M3AST_AS.NEWCall).init());
  BEGIN
    RETURN EitherCall(n, cn, h);
  END NEWCall;

PROCEDURE Index(n: M3AST_AS_F.Index; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Index := SRC_NODE(n, NEW(M3AST_AS.Index).init());
  VAR
    m: M3AST_AS.EXP;
    iter := SeqM3AST_AS_EXP.NewIter(n.as_exp_s);
  BEGIN
    cn.as_array := h.Copy(n.as_array);
    VAR ns := SeqM3AST_AS_EXP.Null;
    BEGIN
    WHILE SeqM3AST_AS_EXP.Next(iter, m) DO
      SeqM3AST_AS_EXP.AddRear(ns, h.Copy(m));
    END;
    cn.as_exp_s := ns;
    END;
    RETURN cn;
  END Index;


PROCEDURE Actual(n: M3AST_AS_F.Actual; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Actual := SRC_NODE(n, NEW(M3AST_AS.Actual).init());
  BEGIN
    IF n.as_id # NIL THEN cn.as_id := h.Copy(n.as_id); END;
    cn.as_exp_type := h.Copy(n.as_exp_type);
    RETURN cn;
  END Actual;


PROCEDURE Assign_st(n: M3AST_AS_F.Assign_st; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Assign_st := SRC_NODE(n, NEW(M3AST_AS.Assign_st).init());
  BEGIN
    cn.as_lhs_exp := h.Copy(n.as_lhs_exp);
    cn.as_rhs_exp := h.Copy(n.as_rhs_exp);
    RETURN cn;
  END Assign_st;


PROCEDURE Call_st(n: M3AST_AS_F.Call_st; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Call_st := SRC_NODE(n, NEW(M3AST_AS.Call_st).init());
  BEGIN
    cn.as_call := h.Copy(n.as_call);
    RETURN cn;
  END Call_st;


PROCEDURE Case_st(n: M3AST_AS_F.Case_st; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Case_st := SRC_NODE(n, NEW(M3AST_AS.Case_st).init());
  VAR
    m: M3AST_AS.Case;
    iter := SeqM3AST_AS_Case.NewIter(n.as_case_s);
  BEGIN
    cn.as_exp := h.Copy(n.as_exp);
    VAR ns := SeqM3AST_AS_Case.Null;
    BEGIN
    WHILE SeqM3AST_AS_Case.Next(iter, m) DO
      SeqM3AST_AS_Case.AddRear(ns, h.Copy(m));
    END;
    cn.as_case_s := ns;
    END;
    IF n.as_else # NIL THEN cn.as_else := h.Copy(n.as_else); END;
        RETURN cn;
  END Case_st;


PROCEDURE Eval_st(n: M3AST_AS_F.Eval_st; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Eval_st := SRC_NODE(n, NEW(M3AST_AS.Eval_st).init());
  BEGIN
    cn.as_exp := h.Copy(n.as_exp);
    RETURN cn;
  END Eval_st;


PROCEDURE Exit_st(
    n: M3AST_AS_F.Exit_st; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN SRC_NODE(n, NEW(M3AST_AS.Exit_st).init());
  END Exit_st;


PROCEDURE For_st(n: M3AST_AS_F.For_st; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.For_st := SRC_NODE(n, NEW(M3AST_AS.For_st).init());
  BEGIN
    cn.as_id := h.Copy(n.as_id);
    cn.as_from := h.Copy(n.as_from);
    cn.as_to := h.Copy(n.as_to);
    IF n.as_by # NIL THEN cn.as_by := h.Copy(n.as_by); END;
    cn.as_stm_s := CopySeqStm(n.as_stm_s, h);
        RETURN cn;
  END For_st;


PROCEDURE If_st(n: M3AST_AS_F.If_st; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.If_st := SRC_NODE(n, NEW(M3AST_AS.If_st).init());
  VAR
    m: M3AST_AS.Elsif;
    iter := SeqM3AST_AS_Elsif.NewIter(n.as_elsif_s);
  BEGIN
    cn.as_exp := h.Copy(n.as_exp);
    cn.as_stm_s := CopySeqStm(n.as_stm_s, h);
    VAR ns := SeqM3AST_AS_Elsif.Null;
    BEGIN
    WHILE SeqM3AST_AS_Elsif.Next(iter, m) DO
      SeqM3AST_AS_Elsif.AddRear(ns, h.Copy(m));
    END;
    cn.as_elsif_s := ns;
    END;
    IF n.as_else # NIL THEN cn.as_else := h.Copy(n.as_else); END;
        RETURN cn;
  END If_st;


PROCEDURE Lock_st(n: M3AST_AS_F.Lock_st; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Lock_st := SRC_NODE(n, NEW(M3AST_AS.Lock_st).init());
  BEGIN
    cn.as_exp := h.Copy(n.as_exp);
    cn.as_stm_s := CopySeqStm(n.as_stm_s, h);
        RETURN cn;
  END Lock_st;


PROCEDURE Loop_st(n: M3AST_AS_F.Loop_st; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Loop_st := SRC_NODE(n, NEW(M3AST_AS.Loop_st).init());
  BEGIN
    cn.as_stm_s := CopySeqStm(n.as_stm_s, h);
        RETURN cn;
  END Loop_st;


PROCEDURE Raise_st(n: M3AST_AS_F.Raise_st; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Raise_st := SRC_NODE(n, NEW(M3AST_AS.Raise_st).init());
  BEGIN
    cn.as_qual_id := h.Copy(n.as_qual_id);
    IF n.as_exp_void # NIL THEN cn.as_exp_void := h.Copy(n.as_exp_void); END;
    RETURN cn;
  END Raise_st;


PROCEDURE Repeat_st(n: M3AST_AS_F.Repeat_st; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Repeat_st := SRC_NODE(n, NEW(M3AST_AS.Repeat_st).init());
  BEGIN
    cn.as_stm_s := CopySeqStm(n.as_stm_s, h);
    cn.as_exp := h.Copy(n.as_exp);
        RETURN cn;
  END Repeat_st;


PROCEDURE Return_st(n: M3AST_AS_F.Return_st; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Return_st := SRC_NODE(n, NEW(M3AST_AS.Return_st).init());
  BEGIN
    IF n.as_exp # NIL THEN cn.as_exp := h.Copy(n.as_exp); END;
    RETURN cn;
  END Return_st;


PROCEDURE Try_st(n: M3AST_AS_F.Try_st; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Try_st := SRC_NODE(n, NEW(M3AST_AS.Try_st).init());
  BEGIN
    cn.as_stm_s := CopySeqStm(n.as_stm_s, h);
    cn.as_try_tail := h.Copy(n.as_try_tail);
        RETURN cn;
  END Try_st;


PROCEDURE Typecase_st(n: M3AST_AS_F.Typecase_st; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Typecase_st := SRC_NODE(n, NEW(M3AST_AS.Typecase_st).init());
  VAR
    m: M3AST_AS.Tcase;
    iter := SeqM3AST_AS_Tcase.NewIter(n.as_tcase_s);
  BEGIN
    cn.as_exp := h.Copy(n.as_exp);
    VAR ns := SeqM3AST_AS_Tcase.Null;
    BEGIN
    WHILE SeqM3AST_AS_Tcase.Next(iter, m) DO
      SeqM3AST_AS_Tcase.AddRear(ns, h.Copy(m));
    END;
    cn.as_tcase_s := ns;
    END;
    IF n.as_else # NIL THEN cn.as_else := h.Copy(n.as_else); END;
        RETURN cn;
  END Typecase_st;


PROCEDURE While_st(n: M3AST_AS_F.While_st; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.While_st := SRC_NODE(n, NEW(M3AST_AS.While_st).init());
  BEGIN
    cn.as_exp := h.Copy(n.as_exp);
    cn.as_stm_s := CopySeqStm(n.as_stm_s, h);
        RETURN cn;
  END While_st;


PROCEDURE With_st(n: M3AST_AS_F.With_st; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.With_st := SRC_NODE(n, NEW(M3AST_AS.With_st).init());
  VAR
    m: M3AST_AS.Binding;
    iter := SeqM3AST_AS_Binding.NewIter(n.as_binding_s);
  BEGIN
    VAR ns := SeqM3AST_AS_Binding.Null;
    BEGIN
    WHILE SeqM3AST_AS_Binding.Next(iter, m) DO
      SeqM3AST_AS_Binding.AddRear(ns, h.Copy(m));
    END;
    cn.as_binding_s := ns;
    END;
    cn.as_stm_s := CopySeqStm(n.as_stm_s, h);
        RETURN cn;
  END With_st;


PROCEDURE Case(
    n: M3AST_AS_F.Case; h: AST_CopyRep.Handle
    ): AST.NODE RAISES ANY=
  VAR cn: M3AST_AS.Case := SRC_NODE(n, NEW(M3AST_AS.Case).init());
  VAR
    m: M3AST_AS.RANGE_EXP;
    iter := SeqM3AST_AS_RANGE_EXP.NewIter(n.as_case_label_s);
  BEGIN
    VAR ns := SeqM3AST_AS_RANGE_EXP.Null;
    BEGIN
    WHILE SeqM3AST_AS_RANGE_EXP.Next(iter, m) DO
      SeqM3AST_AS_RANGE_EXP.AddRear(ns, h.Copy(m));
    END;
    cn.as_case_label_s := ns;
    END;
    cn.as_stm_s := CopySeqStm(n.as_stm_s, h);
    RETURN cn;
  END Case;

PROCEDURE Block(n: M3AST_AS_F.Block; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Block := SRC_NODE(n, NEW(M3AST_AS.Block).init());
  VAR
    m: M3AST_AS.DECL_REVL;
    iter := SeqM3AST_AS_DECL_REVL.NewIter(n.as_decl_s);
  BEGIN
    VAR ns := SeqM3AST_AS_DECL_REVL.Null;
    BEGIN
    WHILE SeqM3AST_AS_DECL_REVL.Next(iter, m) DO
      SeqM3AST_AS_DECL_REVL.AddRear(ns, h.Copy(m));
    END;
    cn.as_decl_s := ns;
    END;
    cn.as_stm_s := CopySeqStm(n.as_stm_s, h);
    RETURN cn;
  END Block;


PROCEDURE Else_stm(n: M3AST_AS_F.Else_stm; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Else_stm := SRC_NODE(n, NEW(M3AST_AS.Else_stm).init());
  BEGIN
    cn.as_stm_s := CopySeqStm(n.as_stm_s, h);
    RETURN cn;
  END Else_stm;


PROCEDURE By(n: M3AST_AS_F.By; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.By := SRC_NODE(n, NEW(M3AST_AS.By).init());
  BEGIN
    cn.as_exp := h.Copy(n.as_exp);
    RETURN cn;
  END By;


PROCEDURE Elsif(n: M3AST_AS_F.Elsif; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Elsif := SRC_NODE(n, NEW(M3AST_AS.Elsif).init());
  BEGIN
    cn.as_exp := h.Copy(n.as_exp);
    cn.as_stm_s := CopySeqStm(n.as_stm_s, h);
    RETURN cn;
  END Elsif;


PROCEDURE Try_except(n: M3AST_AS_F.Try_except; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Try_except := SRC_NODE(n, NEW(M3AST_AS.Try_except).init());
  VAR
    m: M3AST_AS.Handler;
    iter := SeqM3AST_AS_Handler.NewIter(n.as_handler_s);
  BEGIN
    VAR ns := SeqM3AST_AS_Handler.Null;
    BEGIN
    WHILE SeqM3AST_AS_Handler.Next(iter, m) DO
      SeqM3AST_AS_Handler.AddRear(ns, h.Copy(m));
    END;
    cn.as_handler_s := ns;
    END;
    IF n.as_else # NIL THEN cn.as_else := h.Copy(n.as_else); END;
    RETURN cn;
  END Try_except;


PROCEDURE Try_finally(n: M3AST_AS_F.Try_finally; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Try_finally := SRC_NODE(n, NEW(M3AST_AS.Try_finally).init());
  BEGIN
    cn.as_stm_s := CopySeqStm(n.as_stm_s, h);
    RETURN cn;
  END Try_finally;


PROCEDURE Tcase(n: M3AST_AS_F.Tcase; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Tcase := SRC_NODE(n, NEW(M3AST_AS.Tcase).init());
  VAR
    m: M3AST_AS.M3TYPE;
    iter := SeqM3AST_AS_M3TYPE.NewIter(n.as_type_s);
  BEGIN
    VAR ns := SeqM3AST_AS_M3TYPE.Null;
    BEGIN
    WHILE SeqM3AST_AS_M3TYPE.Next(iter, m) DO
      SeqM3AST_AS_M3TYPE.AddRear(ns, h.Copy(m));
    END;
    cn.as_type_s := ns;
    END;
    IF n.as_id # NIL THEN cn.as_id := h.Copy(n.as_id) END;
    cn.as_stm_s := CopySeqStm(n.as_stm_s, h);
    RETURN cn;
  END Tcase;


PROCEDURE Handler(n: M3AST_AS_F.Handler; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Handler := SRC_NODE(n, NEW(M3AST_AS.Handler).init());
  VAR
    m: M3AST_AS.Qual_used_id;
    iter := SeqM3AST_AS_Qual_used_id.NewIter(n.as_qual_id_s);
  BEGIN
    VAR ns := SeqM3AST_AS_Qual_used_id.Null;
    BEGIN
    WHILE SeqM3AST_AS_Qual_used_id.Next(iter, m) DO
      SeqM3AST_AS_Qual_used_id.AddRear(ns, h.Copy(m));
    END;
    cn.as_qual_id_s := ns;
    END;
    IF n.as_id # NIL THEN cn.as_id := h.Copy(n.as_id); END;
    cn.as_stm_s := CopySeqStm(n.as_stm_s, h);
    RETURN cn;
  END Handler;


PROCEDURE Binding(n: M3AST_AS_F.Binding; h: AST_CopyRep.Handle): AST.NODE
     RAISES ANY =
  VAR cn: M3AST_AS.Binding := SRC_NODE(n, NEW(M3AST_AS.Binding).init());
  BEGIN
    cn.as_id := h.Copy(n.as_id);
    cn.as_exp := h.Copy(n.as_exp);
    RETURN cn;
  END Binding;

PROCEDURE External(
    n: M3AST_PG_F.External; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  VAR cn: M3AST_PG.External;
  BEGIN
    cn := SRC_NODE(n, NEW(M3AST_PG.External).init());
    cn.lx_lang_spec := n.lx_lang_spec;
    RETURN cn;
  END External;


PROCEDURE Bad_EXP(
    n: M3AST_AS_F.Bad_EXP; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN SRC_NODE(n, NEW(M3AST_AS.Bad_EXP).init());
  END Bad_EXP;


PROCEDURE Bad_M3TYPE(
    n: M3AST_AS_F.Bad_M3TYPE; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN SRC_NODE(n, NEW(M3AST_AS.Bad_M3TYPE).init());
  END Bad_M3TYPE;


PROCEDURE Bad_STM(
    n: M3AST_AS_F.Bad_STM; <*UNUSED*> h: AST_CopyRep.Handle;
    ): AST.NODE=
  BEGIN
    RETURN SRC_NODE(n, NEW(M3AST_AS.Bad_STM).init());
  END Bad_STM;



BEGIN
END M3AST_AS_Copy.
