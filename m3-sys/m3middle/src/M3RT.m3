(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* see also m3-libs/m3core/src/runtime/ex* *)

MODULE M3RT;

IMPORT Target;

PROCEDURE Init () =
  VAR
    IP := Target.Integer.pack; (* 32 or 64, same as Target.Address.pack *)
    AP := Target.Address.pack; (* 32 or 64, same as Target.Integer.pack *)
    CP := Target.Char.pack;    (* 8 *)
  BEGIN
    (* closure offsets *)
    CL_marker := 0;                (* : INTEGER *)
    CL_proc   := CL_marker + IP;   (* : PROCEDURE() *)
    CL_frame  := CL_proc   + AP;   (* : ADDRESS *)
    CL_SIZE   := CL_frame  + AP;

    (* exception descriptors *)
    ED_uid      := 0;                (* : INTEGER (ExceptionUID) *)
    ED_name     := ED_uid + IP;      (* : ADDRESS (String) *)
    ED_implicit := ED_name + AP;     (* : INTEGER (boolean) *)
    ED_SIZE     := ED_implicit + IP;

    (* exception scope descriptors *)
    EX_class       := 0;                   (* : CHAR(HandlerClass)*)
    EX_outermost   := EX_class + CP;       (* : CHAR(BOOLEAN)     *)
    EX_end_of_list := EX_outermost + CP;   (* : CHAR(BOOLEAN)     *)
    EX_pad         := EX_end_of_list + CP; (* : CHAR(BOOLEAN)     *)
    EX_start       := MAX (EX_pad + CP, EX_class + AP); (* : ADDRESS *)
    EX_stop        := EX_start + AP;       (* : ADDRESS           *)
    EX_excepts     := EX_stop  + AP;       (* : ADDRESS           *)
    EX_offset      := EX_excepts + AP;     (* : INTEGER           *)
    EX_SIZE        := EX_offset + IP;

    (* exception info record *)
    EA_exception   := 0;                   (* : ADDRESS *)
    EA_arg         := EA_exception + AP;   (* : ADDRESS *)
    EA_module      := EA_arg + AP;         (* : ADDRESS *)
    EA_line        := EA_module + AP;      (* : INTEGER *)
    EA_pc          := EA_line + IP;        (* : ADDRESS *)
    EA_info0       := EA_pc + AP;          (* : ADDRESS *)
    EA_info1       := EA_info0 + AP;       (* : ADDRESS *)
    EA_un_except   := EA_info1 + AP;       (* : ADDRESS *)
    EA_un_arg      := EA_un_except + AP;   (* : ADDRESS *)
    EA_SIZE        := EA_un_arg + AP;

    (* all exception frames  (== all of a RaisesNone frame) *)
    EF_next        := 0;                   (* : ADDRESS *)
    EF_class       := EF_next + AP;        (* : INTEGER(HandlerClass) *)
    EF_SIZE        := EF_class + IP;

    (* Except, ExceptElse, and Finally  frames *)
    EF1_handles    := EF_SIZE;             (* : ADDRESS *)
    EF1_info       := EF1_handles + AP;    (* : RTException.Activation *)
    EF1_jmpbuf     := EF1_info + EA_SIZE;  (* : jmp_buf *)
    IF Target.Alloca_jmpbuf THEN
      EF1_SIZE       := EF1_jmpbuf + AP;
    ELSE
      <* ASSERT Target.Jumpbuf_size > 0 *>
      EF1_jmpbuf     := RoundUp (EF1_jmpbuf, 128); (* : jmp_buf *)
      EF1_SIZE       := EF1_jmpbuf + Target.Jumpbuf_size;
    END;

    (* FinallyProc frames *)
    EF2_handler    := EF_SIZE;            (* : ADDRESS (PROC) *)
    EF2_frame      := EF2_handler + AP;   (* : ADDRESS *)
    EF2_info       := EF2_frame + AP;     (* : ADDRESS *)
    EF2_SIZE       := EF2_info + AP;

    (* Raises frames *)
    EF3_raises     := EF_SIZE;            (* : ADDRESS *)
    EF3_SIZE       := EF3_raises + AP;

    (* Lock frames *)
    EF4_mutex      := EF_SIZE;            (* : MUTEX *)
    EF4_SIZE       := EF4_mutex + AP;

    (* typecell offsets *)
    TC_typecode       := 0;                      (* : INTEGER *)
    TC_selfID         := TC_typecode       + IP; (* : INTEGER *)
    TC_fp             := TC_selfID         + IP; (* : 64-bit fingerprint *)
    TC_traced         := TC_fp             + 64; (* : BYTE = ORD (BOOLEAN) *)
    TC_kind           := TC_traced         + 8;  (* : BYTE = ORD (TypeKind) *)
    TC_link_state     := TC_kind           + 8;  (* : BYTE *)
    TC_dataAlignment  := TC_link_state     + 8;  (* : BYTE *)
    TC_dataSize       := TC_traced         + IP; (* : INTEGER *)
    TC_type_map       := TC_dataSize       + IP; (* : ADDRESS *)
    TC_gc_map         := TC_type_map       + AP; (* : ADDRESS *)
    TC_type_desc      := TC_gc_map         + AP; (* : ADDRESS *)
    TC_initProc       := TC_type_desc      + AP; (* : PROC()  *)
    TC_brand          := TC_initProc       + AP; (* : ADDRESS *)
    TC_name           := TC_brand          + AP; (* : ADDRESS *)
    TC_next           := TC_name           + AP; (* : ADDRESS *)
    TC_SIZE           := TC_next           + AP;
    TC_ALIGN          := MAX (Target.Address.align, Target.Integer.align);
    <* ASSERT Target.Address.align = Target.Integer.align *>

    OTC_parentID       := TC_SIZE;                 (* : INTEGER *)
    OTC_linkProc       := OTC_parentID       + IP; (* : PROC()  *)
    OTC_dataOffset     := OTC_linkProc       + AP; (* : INTEGER *)
    OTC_methodOffset   := OTC_dataOffset     + IP; (* : INTEGER *)
    OTC_methodSize     := OTC_methodOffset   + IP; (* : INTEGER *)
    OTC_defaultMethods := OTC_methodSize     + IP; (* : ADDRESS *)
    OTC_parent         := OTC_defaultMethods + AP; (* : ADDRESS *)
    OTC_SIZE           := OTC_parent         + AP;

    ATC_nDimensions    := TC_SIZE;                (* : INTEGER *)
    ATC_elementSize    := ATC_nDimensions   + IP; (* : INTEGER *)
    ATC_SIZE           := ATC_elementSize   + IP;

    RV_lhs_id  := 0;                  (* : INTEGER *)
    RV_rhs_id  := RV_lhs_id + IP;     (* : INTEGER *)
    RV_SIZE    := RV_rhs_id + IP;

    (* dope vector offsets *)
    OA_elt_ptr := 0;                                (*: ADDRESS *)
    OA_sizes   := OA_elt_ptr + AP; (*: ARRAY [0..depth] OF INT*)
    OA_size_0  := OA_sizes;                         (*: INTEGER *)
    OA_size_1  := OA_size_0 + IP;  (*: INTEGER *)

    (* offsets and size of an RT0.ModuleInfo record *)
    MI_file           := 0;                      (* : ADDRESS *)
    MI_type_cells     := MI_file + AP;           (* : ADDRESS *)
    MI_type_cell_ptrs := MI_type_cells + AP;     (* : ADDRESS *)
    MI_full_rev       := MI_type_cell_ptrs + AP; (* : ADDRESS *)
    MI_part_rev       := MI_full_rev + AP;       (* : ADDRESS *)
    MI_proc_info      := MI_part_rev + AP;       (* : ADDRESS *)
    MI_try_scopes     := MI_proc_info + AP;      (* : ADDRESS *)
    MI_var_map        := MI_try_scopes + AP;     (* : ADDRESS *)
    MI_gc_map         := MI_var_map + AP;        (* : ADDRESS *)
    MI_imports        := MI_gc_map + AP;         (* : ADDRESS *)
    MI_link_state     := MI_imports + AP;        (* : INTEGER *)
    MI_binder         := MI_link_state + IP;     (* : PROC()  *)
    MI_gc_flags       := MI_binder + AP;         (* : INTEGER  *)
    MI_SIZE           := MI_gc_flags + IP;

    (* offsets and size of an RT0.ImportInfo record *)
    II_import := 0;               (* : ADDRESS (ModulePtr) *)
    II_binder := II_import + AP;  (* : ADDRESS (Binder)    *)
    II_next   := II_binder + AP;  (* : ADDRESS (ImportPtr) *)
    II_SIZE   := II_next + AP;

    (* offsets and size of an RT0.ProcInfo record *)
    PI_proc   := 0;               (* : ADDRESS *)
    PI_name   := PI_proc + AP;    (* : ADDRESS *)
    PI_SIZE   := PI_name + AP;

    (* offsets in a MUTEX method list *)
    MUTEX_acquire := 0 * AP;          (*: PROC() *)
    MUTEX_release := 1 * AP;          (*: PROC() *)
  END Init;

PROCEDURE RoundUp (a, b: INTEGER): INTEGER =
  BEGIN
    RETURN (a + b - 1) DIV b * b;
  END RoundUp;

BEGIN
END M3RT.
