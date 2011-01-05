(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Jul 25 09:10:40 PDT 1994 by kalsow     *)

(*  Modula-3 runtime interface

    This interface defines the data structures needed by the compiler
    to interface to the Modula-3 runtime.

    Note that all runtime structures have their fields naturally aligned.
*)

INTERFACE M3RT;

(*------------------------------------------- procedure and closure types ---*)

(* closure offsets *)
VAR
  CL_marker : CARDINAL;   (* : INTEGER *)
  CL_proc   : CARDINAL;   (* : PROCEDURE() *)
  CL_frame  : CARDINAL;   (* : ADDRESS *)
  CL_SIZE   : CARDINAL;

CONST
  CL_marker_value = -1;

(*------------------------------------------------------ exception values ---*)

VAR
  ED_uid      : CARDINAL;  (* : INTEGER (ExceptionUID) *)
  ED_name     : CARDINAL;  (* : ADDRESS (String) *)
  ED_implicit : CARDINAL;  (* : INTEGER (boolean) *)
  ED_SIZE     : CARDINAL;

(*------------------------------------------------------ exception support --*)
(* Exception scope descriptors for systems with stack walkers. *)

TYPE
  HandlerClass = { Except, ExceptElse,
                   Finally, FinallyProc,
                   Raises, RaisesNone,
                   Lock };

VAR
  EX_class       : CARDINAL; (* : CHAR(HandlerClass)*)
  EX_outermost   : CARDINAL; (* : CHAR(BOOLEAN)     *)
  EX_end_of_list : CARDINAL; (* : CHAR(BOOLEAN)     *)
  EX_pad         : CARDINAL; (* : CHAR(BOOLEAN)     *)
  EX_start       : CARDINAL; (* : ADDRESS           *)
  EX_stop        : CARDINAL; (* : ADDRESS           *)
  EX_excepts     : CARDINAL; (* : ADDRESS           *)
  EX_offset      : CARDINAL; (* : INTEGER           *)
  EX_SIZE        : CARDINAL;

VAR (* RTException.Activation *)
  EA_exception   : CARDINAL; (* : ADDRESS *)
  EA_arg         : CARDINAL; (* : ADDRESS *)
  EA_module      : CARDINAL; (* : ADDRESS *)
  EA_line        : CARDINAL; (* : INTEGER *)
  EA_pc          : CARDINAL; (* : ADDRESS *)
  EA_info0       : CARDINAL; (* : ADDRESS *)
  EA_info1       : CARDINAL; (* : ADDRESS *)
  EA_un_except   : CARDINAL; (* : ADDRESS *)
  EA_un_arg      : CARDINAL; (* : ADDRESS *)
  EA_SIZE        : CARDINAL;

(*------------------------------------------------------- exception frames --*)
(* Explicit exception frames for systems without stack walkers.  *)

VAR (* all frames  (== all of a RaisesNone frame) *)
  EF_next        : CARDINAL;  (* : ADDRESS *)
  EF_class       : CARDINAL;  (* : INTEGER(HandlerClass) *)
  EF_SIZE        : CARDINAL;

VAR (* Except, ExceptElse, and Finally  frames *)
  EF1_handles    : CARDINAL;  (* : ADDRESS *)
  EF1_info       : CARDINAL;  (* : RTException.Activation *)
  EF1_jmpbuf     : CARDINAL;  (* : jmp_buf *)
  EF1_SIZE       : CARDINAL;

VAR (* FinallyProc frames *)
  EF2_handler    : CARDINAL;  (* : ADDRESS (PROC) *)
  EF2_frame      : CARDINAL;  (* : ADDRESS *)
  EF2_info       : CARDINAL;  (* : ADDRESS *)
  EF2_SIZE       : CARDINAL;

VAR (* Raises frames *)
  EF3_raises     : CARDINAL;   (* : ADDRESS *)
  EF3_SIZE       : CARDINAL;

VAR (* Lock frames *)
  EF4_mutex      : CARDINAL;   (* : MUTEX *)
  EF4_SIZE       : CARDINAL;

(*---------------------------------------------------- runtime type system --*)
(* Reference types are represented by global variables call "typecells". *)

TYPE
  TypeKind = { Unknown, Ref, Obj, Array };

VAR (* typecell offsets *)
  TC_typecode       : CARDINAL; (* : INTEGER *)
  TC_selfID         : CARDINAL; (* : INTEGER *)
  TC_fp             : CARDINAL; (* : 64-bit fingerprint *)
  TC_traced         : CARDINAL; (* : BYTE = ORD (BOOLEAN) *)
  TC_kind           : CARDINAL; (* : BYTE = ORD (TypeKind) *)
  TC_link_state     : CARDINAL; (* : BYTE *)
  TC_dataAlignment  : CARDINAL; (* : BYTE *)
  TC_dataSize       : CARDINAL; (* : INTEGER *)
  TC_type_map       : CARDINAL; (* : ADDRESS *)
  TC_gc_map         : CARDINAL; (* : ADDRESS *)
  TC_type_desc      : CARDINAL; (* : ADDRESS *)
  TC_initProc       : CARDINAL; (* : PROC()  *)
  TC_brand          : CARDINAL; (* : ADDRESS *)
  TC_name           : CARDINAL; (* : ADDRESS *)
  TC_next           : CARDINAL; (* : ADDRESS *)
  TC_SIZE           : CARDINAL;
  TC_ALIGN          : CARDINAL;

VAR (* OBJECT typecells *)
  OTC_parentID       : CARDINAL; (* : INTEGER *)
  OTC_linkProc       : CARDINAL; (* : PROC()  *)
  OTC_dataOffset     : CARDINAL; (* : INTEGER *)
  OTC_methodOffset   : CARDINAL; (* : INTEGER *)
  OTC_methodSize     : CARDINAL; (* : INTEGER *)
  OTC_defaultMethods : CARDINAL; (* : ADDRESS *)
  OTC_parent         : CARDINAL; (* : ADDRESS *)
  OTC_SIZE           : CARDINAL;

VAR (* REF ARRAY typecells *)
  ATC_nDimensions    : CARDINAL; (* : INTEGER *)
  ATC_elementSize    : CARDINAL; (* : INTEGER *)
  ATC_SIZE           : CARDINAL;

VAR (* opaque revelations *)
  RV_lhs_id         : CARDINAL; (* : INTEGER *)
  RV_rhs_id         : CARDINAL; (* : INTEGER *)
  RV_SIZE           : CARDINAL;

(*------------------------------------------------------------ open arrays --*)

VAR (* dope vector offsets *)
  OA_elt_ptr : CARDINAL; (*: ADDRESS *)
  OA_sizes   : CARDINAL; (*: ARRAY [0..depth] OF INT*)
  OA_size_0  : CARDINAL; (*: INTEGER *)
  OA_size_1  : CARDINAL; (*: INTEGER *)

(*----------------------------------------------------------- module info ---*)

VAR (* offsets and size of an RT0.ModuleInfo record *)
  MI_file           : CARDINAL; (* : ADDRESS *)
  MI_type_cells     : CARDINAL; (* : ADDRESS *)
  MI_type_cell_ptrs : CARDINAL; (* : ADDRESS *)
  MI_full_rev       : CARDINAL; (* : ADDRESS *)
  MI_part_rev       : CARDINAL; (* : ADDRESS *)
  MI_proc_info      : CARDINAL; (* : ADDRESS *)
  MI_try_scopes     : CARDINAL; (* : ADDRESS *)
  MI_var_map        : CARDINAL; (* : ADDRESS *)
  MI_gc_map         : CARDINAL; (* : ADDRESS *)
  MI_imports        : CARDINAL; (* : ADDRESS *)
  MI_link_state     : CARDINAL; (* : INTEGER *)
  MI_binder         : CARDINAL; (* : PROC()  *)
  MI_gc_flags       : CARDINAL; (* : INTEGER *)
  MI_SIZE           : CARDINAL;

VAR (* offsets and size of an RT0.ImportInfo record *)
  II_import : CARDINAL;  (* : ADDRESS (ModulePtr) *)
  II_binder : CARDINAL;  (* : ADDRESS (Binder)    *)
  II_next   : CARDINAL;  (* : ADDRESS (ImportPtr) *)
  II_SIZE   : CARDINAL;

VAR (* offsets and size of an RT0.ProcInfo record *)
  PI_proc   : CARDINAL; (* : ADDRESS *)
  PI_name   : CARDINAL; (* : ADDRESS *)
  PI_SIZE   : CARDINAL;

(*----------------------------------------------------------- ref headers ---*)

CONST (* bit offsets and sizes of the fields within a REF's header word *)
  RH_forwarded_offset = 0;
  RH_forwarded_size   = 1;
  RH_typecode_offset  = RH_forwarded_offset + RH_forwarded_size;
  RH_typecode_size    = 20;
  RH_dirty_offset     = RH_typecode_offset + RH_typecode_size;
  RH_dirty_size       = 1;
  RH_gray_offset      = RH_dirty_offset + RH_dirty_size;
  RH_gray_size        = 1;

CONST (* builtin, constant typecodes *)
  NULL_typecode          = 0;
  TEXT_typecode          = 1;
  ADDRESS_typecode       = 2;
  REFANY_typecode        = 3;
  ROOT_typecode          = 4;
  UNTRACED_ROOT_typecode = 5;
  MUTEX_typecode         = 6;

VAR (* offsets in a MUTEX method list *)
  MUTEX_acquire : CARDINAL; (*: PROC() *)
  MUTEX_release : CARDINAL; (*: PROC() *)

(*-------------------------------------------------------- initialization ---*)

PROCEDURE Init ();
(* initializes the values above to reflect the values exported by Target *)

END M3RT.

