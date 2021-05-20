INTERFACE M3CG_MultiPass;

(* This persists a run of M3CG calls to memory, so that multiple
 * passes can be run over it. It provides for two or more different approaches.
 * 1 You can walk the data calling ISTYPE. This is presumably too slow to bother with.
 * 2 You can walk the data looking at the enum op. This lets you handle multiple cases in closer together code.
 * 3 You can implement "mini" M3CGs and pass them to call(). This lets you split up your code more.
 * I expect to use #3.
 *)

IMPORT M3CG, Target, M3CG_AssertFalse;
FROM M3CG IMPORT Type, MType, IType, RType, AType, ZType, Sign;
FROM M3CG IMPORT Name, Alignment, Label;
FROM M3CG IMPORT Frequency, CallingConvention, CompareOp, ConvertOp, AtomicOp;
FROM M3CG IMPORT BitSize, ByteSize, BitOffset, ByteOffset, RuntimeError;
FROM M3CG IMPORT MemoryOrder;
FROM M3CG_Binary IMPORT Op;
FROM M3CG IMPORT NoQID, TypeUID;

TYPE cg_t = M3CG.T;
TYPE typeid_t = M3CG.TypeUID;

TYPE T <: Public;

TYPE Public = M3CG_AssertFalse.T OBJECT
    data: REF ARRAY OF op_t;
    op_data: ARRAY Op OF REF ARRAY OF op_t;
    op_counts := ARRAY Op OF INTEGER{0, ..};
    
    (* parameter to Replay indicating to keep whatever vars/procs are first gotten, except NIL *)
    reuse_refs := FALSE;
    replay: Replay_t := NIL;
METHODS
    Replay(
        cg: M3CG.T;
        VAR index: INTEGER;
        data: REF ARRAY OF op_t := NIL;
        start := 0;
        end := LAST(INTEGER); (* one past end *)
        );
    Init(): T;
END;

TYPE Replay_t <: REFANY;

TYPE op_t = OBJECT
    op: Op;
    METHODS
          replay(replay: Replay_t; cg: M3CG.T);
END;

TYPE op_tag_t = op_t OBJECT
(* For operations that create a var or a proc or maybe labels. *)
    tag: INTEGER;
END;

(* These create procs. *)
TYPE import_procedure_t = op_tag_t OBJECT name: Name; n_params: INTEGER; return_type: Type; callingConvention: CallingConvention; return_typeid: TypeUID := 0; return_typename := NoQID; OVERRIDES replay := replay_import_procedure END;
TYPE declare_procedure_t = op_tag_t OBJECT name: Name; n_params: INTEGER; return_type: Type; level: INTEGER; callingConvention: CallingConvention; exported: BOOLEAN; parent: INTEGER(*proc_t*); return_typeid: TypeUID := 0; return_typename := NoQID; OVERRIDES replay := replay_declare_procedure END;

(* These create vars. *)
TYPE declare_segment_t = op_tag_t OBJECT name: Name; typeid: typeid_t; is_const: BOOLEAN; OVERRIDES replay := replay_declare_segment END;
TYPE declare_global_t = op_tag_t OBJECT name: Name; byte_size: ByteSize; alignment: Alignment; type: Type; typeid: typeid_t; exported, inited: BOOLEAN; OVERRIDES replay := replay_declare_global END;
TYPE declare_constant_t = op_tag_t OBJECT name: Name; byte_size: ByteSize; alignment: Alignment; type: Type; typeid: typeid_t; exported, inited: BOOLEAN; OVERRIDES replay := replay_declare_constant END;
TYPE declare_local_t = op_tag_t OBJECT name: Name; byte_size: ByteSize; alignment: Alignment; type: Type; typeid: typeid_t; in_memory, up_level: BOOLEAN; frequency: Frequency; OVERRIDES replay := replay_declare_local END;
TYPE declare_param_t = op_tag_t OBJECT name: Name; byte_size: ByteSize; alignment: Alignment; type: Type; typeid: typeid_t; in_memory, up_level: BOOLEAN; frequency: Frequency; typename := NoQID; OVERRIDES replay := replay_declare_param END;
TYPE declare_temp_t = op_tag_t OBJECT byte_size: ByteSize; alignment: Alignment; type: Type; in_memory: BOOLEAN; OVERRIDES replay := replay_declare_temp END;
TYPE import_global_t = op_tag_t OBJECT name: Name; byte_size: ByteSize; alignment: Alignment; type: Type; typeid: typeid_t; OVERRIDES replay := replay_import_global END;

TYPE set_error_handler_t = op_t OBJECT proc: PROCEDURE(msg: TEXT); OVERRIDES replay := replay_set_error_handler END;
TYPE begin_procedure_t = op_t OBJECT proc: INTEGER(*proc_t*); OVERRIDES replay := replay_begin_procedure END;
TYPE end_procedure_t = op_t OBJECT proc: INTEGER(*proc_t*); OVERRIDES replay := replay_end_procedure END;
TYPE begin_unit_t = op_t OBJECT optimize: INTEGER; OVERRIDES replay := replay_begin_unit END;
TYPE end_unit_t = op_t OBJECT OVERRIDES replay := replay_end_unit END;
TYPE import_unit_t = op_t OBJECT name: Name; OVERRIDES replay := replay_import_unit END;
TYPE export_unit_t = op_t OBJECT name: Name; OVERRIDES replay := replay_export_unit END;
TYPE set_source_file_t = op_t OBJECT file: TEXT; OVERRIDES replay := replay_set_source_file END;
TYPE set_source_line_t = op_t OBJECT line: INTEGER; OVERRIDES replay := replay_set_source_line END;

TYPE declare_typename_t = op_t OBJECT typeid: typeid_t; name: Name; OVERRIDES replay := replay_declare_typename END;
TYPE declare_array_t = op_t OBJECT typeid, index_typeid, element_typeid: typeid_t; bit_size: BitSize; OVERRIDES replay := replay_declare_array END;
TYPE declare_open_array_t = op_t OBJECT typeid, element_typeid: typeid_t; bit_size: BitSize; OVERRIDES replay := replay_declare_open_array END;
TYPE declare_enum_t = op_t OBJECT typeid: typeid_t; n_elts: INTEGER; bit_size: BitSize; OVERRIDES replay := replay_declare_enum END;
TYPE declare_enum_elt_t = op_t OBJECT name: Name; OVERRIDES replay := replay_declare_enum_elt END;
TYPE declare_packed_t = op_t OBJECT typeid: typeid_t; bit_size: BitSize; base: typeid_t; OVERRIDES replay := replay_declare_packed END;
TYPE declare_record_t = op_t OBJECT typeid: typeid_t; bit_size: BitSize; n_fields: INTEGER; OVERRIDES replay := replay_declare_record END;
TYPE declare_field_t = op_t OBJECT name: Name; bit_offset: BitOffset; bit_size: BitSize; typeid: typeid_t; OVERRIDES replay := replay_declare_field END;
TYPE declare_set_t = op_t OBJECT typeid, domain_typeid: typeid_t; bit_size: BitSize; OVERRIDES replay := replay_declare_set END;
TYPE declare_subrange_t = op_t OBJECT typeid, domain_typeid: typeid_t; min, max: Target.Int; bit_size: BitSize; OVERRIDES replay := replay_declare_subrange END;
TYPE declare_pointer_t = op_t OBJECT typeid, target_typeid: typeid_t; brand: TEXT; traced: BOOLEAN; OVERRIDES replay := replay_declare_pointer END;
TYPE declare_indirect_t = op_t OBJECT typeid, target_typeid: typeid_t; target_typename := NoQID; OVERRIDES replay := replay_declare_indirect END;
TYPE declare_proctype_t = op_t OBJECT typeid: typeid_t; n_formals: INTEGER; return_typeid: typeid_t; n_raises: INTEGER; callingConvention: CallingConvention; result_typename := NoQID; OVERRIDES replay := replay_declare_proctype END;
TYPE declare_formal_t = op_t OBJECT name: Name; typeid: typeid_t; typename := NoQID; OVERRIDES replay := replay_declare_formal END;
TYPE declare_raises_t = op_t OBJECT name: Name; OVERRIDES replay := replay_declare_raises END;
TYPE declare_object_t = op_t OBJECT typeid, super_typeid: typeid_t; brand: TEXT; traced: BOOLEAN; n_fields, n_methods: INTEGER; fields_bit_size: BitSize; OVERRIDES replay := replay_declare_object END;
TYPE declare_method_t = op_t OBJECT name: Name; signature: typeid_t; OVERRIDES replay := replay_declare_method END;
TYPE declare_opaque_t = op_t OBJECT typeid, super_typeid: typeid_t; OVERRIDES replay := replay_declare_opaque END;
TYPE reveal_opaque_t = op_t OBJECT lhs_typeid, rhs_typeid: typeid_t; OVERRIDES replay := replay_reveal_opaque END;
TYPE declare_exception_t = op_t OBJECT name: Name; arg_typeid: typeid_t; raise_proc: BOOLEAN; base: INTEGER(*var_t*); offset: INTEGER; OVERRIDES replay := replay_declare_exception END;
TYPE widechar_size_t = op_t OBJECT size: INTEGER; OVERRIDES replay := replay_widechar_size END;

TYPE set_runtime_proc_t = op_t OBJECT name: Name; proc: INTEGER(*proc_t*); OVERRIDES replay := replay_set_runtime_proc END;
TYPE bind_segment_t = op_t OBJECT segment: INTEGER(*var_t*); byte_size: ByteSize; alignment: Alignment; type: Type; exported, inited: BOOLEAN; OVERRIDES replay := replay_bind_segment END;
TYPE free_temp_t = op_t OBJECT var: INTEGER(*var_t*); OVERRIDES replay := replay_free_temp END;
TYPE begin_init_t = op_t OBJECT var: INTEGER(*var_t*); OVERRIDES replay := replay_begin_init END;
TYPE end_init_t = op_t OBJECT var: INTEGER(*var_t*); OVERRIDES replay := replay_end_init END;
TYPE init_int_t = op_t OBJECT byte_offset: ByteOffset; int: Target.Int; type: Type; OVERRIDES replay := replay_init_int END;
TYPE init_proc_t = op_t OBJECT byte_offset: ByteOffset; proc: INTEGER(*proc_t*); OVERRIDES replay := replay_init_proc END;
TYPE init_label_t = op_t OBJECT byte_offset: ByteOffset; label: Label; OVERRIDES replay := replay_init_label END;
TYPE init_var_t = op_t OBJECT byte_offset: ByteOffset; var: INTEGER(*var_t*); bias: ByteOffset; OVERRIDES replay := replay_init_var END;
TYPE init_offset_t = op_t OBJECT byte_offset: ByteOffset; var: INTEGER(*var_t*); OVERRIDES replay := replay_init_offset END;
TYPE init_chars_t = op_t OBJECT byte_offset: ByteOffset; text: TEXT; OVERRIDES replay := replay_init_chars END;
TYPE init_float_t = op_t OBJECT byte_offset: ByteOffset; float: Target.Float; OVERRIDES replay := replay_init_float END;
TYPE begin_block_t = op_t OBJECT OVERRIDES replay := replay_begin_block END;
TYPE end_block_t = op_t OBJECT OVERRIDES replay := replay_end_block END;
TYPE note_procedure_origin_t = op_t OBJECT proc: INTEGER(*proc_t*); OVERRIDES replay := replay_note_procedure_origin END;
TYPE set_label_t = op_t OBJECT label: Label; barrier: BOOLEAN; OVERRIDES replay := replay_set_label END;
TYPE jump_t = op_t OBJECT label: Label; OVERRIDES replay := replay_jump END;
TYPE if_true_t = op_t OBJECT type: IType; label: Label; frequency: Frequency; OVERRIDES replay := replay_if_true END;
TYPE if_false_t = op_t OBJECT type: IType; label: Label; frequency: Frequency; OVERRIDES replay := replay_if_false END;
TYPE if_compare_t = op_t OBJECT type: ZType; compare_op: CompareOp; label: Label; frequency: Frequency; OVERRIDES replay := replay_if_compare END;
TYPE case_jump_t = op_t OBJECT type: IType; labels: REF ARRAY OF Label; OVERRIDES replay := replay_case_jump END;
TYPE exit_proc_t = op_t OBJECT type: Type; OVERRIDES replay := replay_exit_proc END;
TYPE load_t = op_t OBJECT var: INTEGER(*var_t*); byte_offset: ByteOffset; mtype: MType; ztype: ZType; OVERRIDES replay := replay_load END;
TYPE store_t = op_t OBJECT var: INTEGER(*var_t*); byte_offset: ByteOffset; ztype: ZType; mtype: MType; OVERRIDES replay := replay_store END;
TYPE load_address_t = op_t OBJECT var: INTEGER(*var_t*); byte_offset: ByteOffset; OVERRIDES replay := replay_load_address END;
TYPE load_indirect_t = op_t OBJECT byte_offset: ByteOffset; mtype: MType; ztype: ZType; OVERRIDES replay := replay_load_indirect END;
TYPE store_indirect_t = op_t OBJECT byte_offset: ByteOffset; ztype: ZType; mtype: MType; OVERRIDES replay := replay_store_indirect END;
TYPE load_nil_t = op_t OBJECT OVERRIDES replay := replay_load_nil END;
TYPE load_integer_t = op_t OBJECT type: IType; int: Target.Int; OVERRIDES replay := replay_load_integer END;
TYPE load_float_t = op_t OBJECT type: RType; float: Target.Float; OVERRIDES replay := replay_load_float END;
TYPE compare_t = op_t OBJECT ztype: ZType; itype: IType; compare_op: CompareOp; OVERRIDES replay := replay_compare END;
TYPE add_t = op_t OBJECT type: AType; OVERRIDES replay := replay_add END;
TYPE subtract_t = op_t OBJECT type: AType; OVERRIDES replay := replay_subtract END;
TYPE multiply_t = op_t OBJECT type: AType; OVERRIDES replay := replay_multiply END;
TYPE divide_t = op_t OBJECT type: RType; OVERRIDES replay := replay_divide END;
TYPE div_t = op_t OBJECT type: IType; a, b: Sign; OVERRIDES replay := replay_div END;
TYPE mod_t = op_t OBJECT type: IType; a, b: Sign; OVERRIDES replay := replay_mod END;
TYPE negate_t = op_t OBJECT type: AType; OVERRIDES replay := replay_negate END;
TYPE abs_t = op_t OBJECT type: AType; OVERRIDES replay := replay_abs END;
TYPE max_t = op_t OBJECT type: ZType; OVERRIDES replay := replay_max END;
TYPE min_t = op_t OBJECT type: ZType; OVERRIDES replay := replay_min END;
TYPE cvt_int_t = op_t OBJECT rtype: RType; itype: IType; convert_op: ConvertOp; OVERRIDES replay := replay_cvt_int END;
TYPE cvt_float_t = op_t OBJECT atype: AType; rtype: RType; OVERRIDES replay := replay_cvt_float END;
TYPE set_union_t = op_t OBJECT byte_size: ByteSize; OVERRIDES replay := replay_set_union END;
TYPE set_difference_t = op_t OBJECT byte_size: ByteSize; OVERRIDES replay := replay_set_difference END;
TYPE set_intersection_t = op_t OBJECT byte_size: ByteSize; OVERRIDES replay := replay_set_intersection END;
TYPE set_sym_difference_t = op_t OBJECT byte_size: ByteSize; OVERRIDES replay := replay_set_sym_difference END;
TYPE set_member_t = op_t OBJECT byte_size: ByteSize; type: IType; OVERRIDES replay := replay_set_member END;
TYPE set_compare_t = op_t OBJECT byte_size: ByteSize; compare_op: CompareOp; type: IType; OVERRIDES replay := replay_set_compare END;
TYPE set_range_t = op_t OBJECT byte_size: ByteSize; type: IType; OVERRIDES replay := replay_set_range END;
TYPE set_singleton_t = op_t OBJECT byte_size: ByteSize; type: IType; OVERRIDES replay := replay_set_singleton END;
TYPE not_t = op_t OBJECT type: IType; OVERRIDES replay := replay_not END;
TYPE and_t = op_t OBJECT type: IType; OVERRIDES replay := replay_and END;
TYPE or_t = op_t OBJECT type: IType; OVERRIDES replay := replay_or END;
TYPE xor_t = op_t OBJECT type: IType; OVERRIDES replay := replay_xor END;
TYPE shift_t = op_t OBJECT type: IType; OVERRIDES replay := replay_shift END;
TYPE shift_left_t = op_t OBJECT type: IType; OVERRIDES replay := replay_shift_left END;
TYPE shift_right_t = op_t OBJECT type: IType; OVERRIDES replay := replay_shift_right END;
TYPE rotate_t = op_t OBJECT type: IType; OVERRIDES replay := replay_rotate END;
TYPE rotate_left_t = op_t OBJECT type: IType; OVERRIDES replay := replay_rotate_left END;
TYPE rotate_right_t = op_t OBJECT type: IType; OVERRIDES replay := replay_rotate_right END;
TYPE widen_t = op_t OBJECT sign: BOOLEAN; OVERRIDES replay := replay_widen END;
TYPE chop_t = op_t OBJECT OVERRIDES replay := replay_chop END;
TYPE extract_t = op_t OBJECT type: IType; sign_extend: BOOLEAN; OVERRIDES replay := replay_extract END;
TYPE extract_n_t = op_t OBJECT type: IType; sign_extend: BOOLEAN; count: CARDINAL; OVERRIDES replay := replay_extract_n END;
TYPE extract_mn_t = op_t OBJECT type: IType; sign_extend: BOOLEAN; offset, count: CARDINAL; OVERRIDES replay := replay_extract_mn END;
TYPE insert_t = op_t OBJECT type: IType; OVERRIDES replay := replay_insert END;
TYPE insert_n_t = op_t OBJECT type: IType; count: CARDINAL; OVERRIDES replay := replay_insert_n END;
TYPE insert_mn_t = op_t OBJECT type: IType; offset, count: CARDINAL; OVERRIDES replay := replay_insert_mn END;
TYPE swap_t = op_t OBJECT a, b: Type; OVERRIDES replay := replay_swap END;
TYPE pop_t = op_t OBJECT type: Type; OVERRIDES replay := replay_pop END;
TYPE copy_n_t = op_t OBJECT itype: IType; mtype: MType; overlap: BOOLEAN; OVERRIDES replay := replay_copy_n END;
TYPE copy_t = op_t OBJECT n: INTEGER; mtype: MType; overlap: BOOLEAN; OVERRIDES replay := replay_copy END;
TYPE zero_n_t = op_t OBJECT itype: IType; mtype: MType; OVERRIDES replay := replay_zero_n END;
TYPE zero_t = op_t OBJECT n: INTEGER; type: MType; OVERRIDES replay := replay_zero END;
TYPE loophole_t = op_t OBJECT from, to: ZType; OVERRIDES replay := replay_loophole END;
TYPE abort_t = op_t OBJECT code: RuntimeError; OVERRIDES replay := replay_abort END;
TYPE check_nil_t = op_t OBJECT code: RuntimeError; OVERRIDES replay := replay_check_nil END;
TYPE check_lo_t = op_t OBJECT type: IType; i: Target.Int; code: RuntimeError; OVERRIDES replay := replay_check_lo END;
TYPE check_hi_t = op_t OBJECT type: IType; i: Target.Int; code: RuntimeError; OVERRIDES replay := replay_check_hi END;
TYPE check_range_t = op_t OBJECT type: IType; a, b: Target.Int; code: RuntimeError; OVERRIDES replay := replay_check_range END;
TYPE check_index_t = op_t OBJECT type: IType; code: RuntimeError; OVERRIDES replay := replay_check_index END;
TYPE check_eq_t = op_t OBJECT type: IType; code: RuntimeError; OVERRIDES replay := replay_check_eq END;
TYPE add_offset_t = op_t OBJECT i: INTEGER; OVERRIDES replay := replay_add_offset END;
TYPE index_address_t = op_t OBJECT type: IType; size: INTEGER; OVERRIDES replay := replay_index_address END;
TYPE start_call_direct_t = op_t OBJECT proc: INTEGER(*proc_t*); level: INTEGER; type: Type; OVERRIDES replay := replay_start_call_direct END;
TYPE start_call_indirect_t = op_t OBJECT type: Type; callingConvention: CallingConvention; OVERRIDES replay := replay_start_call_indirect END;
TYPE pop_param_t = op_t OBJECT type: MType; OVERRIDES replay := replay_pop_param END;
TYPE pop_struct_t = op_t OBJECT typeid: typeid_t; byte_size: ByteSize; alignment: Alignment; OVERRIDES replay := replay_pop_struct END;
TYPE pop_static_link_t = op_t OBJECT OVERRIDES replay := replay_pop_static_link END;
TYPE call_direct_t = op_t OBJECT proc: INTEGER(*proc_t*); type: Type; OVERRIDES replay := replay_call_direct END;
TYPE call_indirect_t = op_t OBJECT type: Type; callingConvention: CallingConvention; OVERRIDES replay := replay_call_indirect END;
TYPE load_procedure_t = op_t OBJECT proc: INTEGER(*proc_t*); OVERRIDES replay := replay_load_procedure END;
TYPE load_static_link_t = op_t OBJECT proc: INTEGER(*proc_t*); OVERRIDES replay := replay_load_static_link END;
TYPE comment_t = op_t OBJECT a, b, c, d: TEXT := NIL; OVERRIDES replay := replay_comment END;
TYPE store_ordered_t = op_t OBJECT ztype: ZType; mtype: MType; order: MemoryOrder; OVERRIDES replay := replay_store_ordered END;
TYPE load_ordered_t = op_t OBJECT mtype: MType; ztype: ZType; order: MemoryOrder; OVERRIDES replay := replay_load_ordered END;
TYPE exchange_t = op_t OBJECT mtype: MType; ztype: ZType; order: MemoryOrder; OVERRIDES replay := replay_exchange END;
TYPE compare_exchange_t = op_t OBJECT mtype: MType; ztype: ZType; r: IType; success, failure: MemoryOrder; OVERRIDES replay := replay_compare_exchange END;
TYPE fence_t = op_t OBJECT order: MemoryOrder; OVERRIDES replay := replay_fence END;
TYPE fetch_and_op_t = op_t OBJECT atomic_op: AtomicOp; mtype: MType; ztype: ZType; order: MemoryOrder; OVERRIDES replay := replay_fetch_and_op END;

PROCEDURE end_unit(self: T);

PROCEDURE replay_import_procedure(self: import_procedure_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_declare_procedure(self: declare_procedure_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_set_error_handler(self: set_error_handler_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_begin_procedure(self: begin_procedure_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_end_procedure(self: end_procedure_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_import_global(self: import_global_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_declare_segment(self: declare_segment_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_declare_global(self: declare_global_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_declare_constant(self: declare_constant_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_declare_local(self: declare_local_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_declare_param(self: declare_param_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_declare_temp(self: declare_temp_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_begin_unit(self: begin_unit_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_end_unit(self: end_unit_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_import_unit(self: import_unit_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_export_unit(self: export_unit_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_set_source_file(self: set_source_file_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_set_source_line(self: set_source_line_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_declare_typename(self: declare_typename_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_declare_array(self: declare_array_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_declare_open_array(self: declare_open_array_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_declare_enum(self: declare_enum_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_declare_enum_elt(self: declare_enum_elt_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_declare_packed(self: declare_packed_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_declare_record(self: declare_record_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_declare_field(self: declare_field_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_declare_set(self: declare_set_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_declare_subrange(self: declare_subrange_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_declare_pointer(self: declare_pointer_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_declare_indirect(self: declare_indirect_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_declare_proctype(self: declare_proctype_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_declare_formal(self: declare_formal_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_declare_raises(self: declare_raises_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_declare_object(self: declare_object_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_declare_method(self: declare_method_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_declare_opaque(self: declare_opaque_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_reveal_opaque(self: reveal_opaque_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_declare_exception(self: declare_exception_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_widechar_size(self: widechar_size_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_set_runtime_proc(self: set_runtime_proc_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_bind_segment(self: bind_segment_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_free_temp(self: free_temp_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_begin_init(self: begin_init_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_end_init(self: end_init_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_init_int(self: init_int_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_init_proc(self: init_proc_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_init_label(self: init_label_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_init_var(self: init_var_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_init_offset(self: init_offset_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_init_chars(self: init_chars_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_init_float(self: init_float_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_begin_block(self: begin_block_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_end_block(self: end_block_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_note_procedure_origin(self: note_procedure_origin_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_set_label(self: set_label_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_jump(self: jump_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_if_true(self: if_true_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_if_false(self: if_false_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_if_compare(self: if_compare_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_case_jump(self: case_jump_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_exit_proc(self: exit_proc_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_load(self: load_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_store(self: store_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_load_address(self: load_address_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_load_indirect(self: load_indirect_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_store_indirect(self: store_indirect_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_load_nil(self: load_nil_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_load_integer(self: load_integer_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_load_float(self: load_float_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_compare(self: compare_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_add(self: add_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_subtract(self: subtract_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_multiply(self: multiply_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_divide(self: divide_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_div(self: div_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_mod(self: mod_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_negate(self: negate_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_abs(self: abs_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_max(self: max_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_min(self: min_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_cvt_int(self: cvt_int_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_cvt_float(self: cvt_float_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_set_union(self: set_union_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_set_difference(self: set_difference_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_set_intersection(self: set_intersection_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_set_sym_difference(self: set_sym_difference_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_set_member(self: set_member_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_set_compare(self: set_compare_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_set_range(self: set_range_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_set_singleton(self: set_singleton_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_not(self: not_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_and(self: and_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_or(self: or_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_xor(self: xor_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_shift(self: shift_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_shift_left(self: shift_left_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_shift_right(self: shift_right_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_rotate(self: rotate_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_rotate_left(self: rotate_left_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_rotate_right(self: rotate_right_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_widen(self: widen_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_chop(self: chop_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_extract(self: extract_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_extract_n(self: extract_n_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_extract_mn(self: extract_mn_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_insert(self: insert_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_insert_n(self: insert_n_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_insert_mn(self: insert_mn_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_swap(self: swap_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_pop(self: pop_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_copy_n(self: copy_n_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_copy(self: copy_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_zero_n(self: zero_n_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_zero(self: zero_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_loophole(self: loophole_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_abort(self: abort_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_check_nil(self: check_nil_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_check_lo(self: check_lo_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_check_hi(self: check_hi_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_check_range(self: check_range_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_check_index(self: check_index_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_check_eq(self: check_eq_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_add_offset(self: add_offset_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_index_address(self: index_address_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_start_call_direct(self: start_call_direct_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_start_call_indirect(self: start_call_indirect_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_pop_param(self: pop_param_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_pop_struct(self: pop_struct_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_pop_static_link(self: pop_static_link_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_call_direct(self: call_direct_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_call_indirect(self: call_indirect_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_load_procedure(self: load_procedure_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_load_static_link(self: load_static_link_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_comment(self: comment_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_store_ordered(self: store_ordered_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_load_ordered(self: load_ordered_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_exchange(self: exchange_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_compare_exchange(self: compare_exchange_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_fence(self: fence_t; replay: Replay_t; cg: cg_t);
PROCEDURE replay_fetch_and_op(self: fetch_and_op_t; replay: Replay_t; cg: cg_t);

END M3CG_MultiPass.
