MODULE M3CG_AssertFalse;

IMPORT M3CG, M3CG_Ops;
IMPORT Target;
FROM M3CG IMPORT Name, ByteOffset, TypeUID, CallingConvention;
FROM M3CG IMPORT BitSize, ByteSize, Alignment, Frequency;
FROM M3CG IMPORT Var, Proc, Label, Sign, BitOffset;
FROM M3CG IMPORT Type, ZType, AType, RType, IType, MType;
FROM M3CG IMPORT CompareOp, ConvertOp, AtomicOp, RuntimeError;
FROM M3CG IMPORT MemoryOrder;

REVEAL
T = Public BRANDED "M3CG_AssertFalse.T" OBJECT
OVERRIDES    
    next_label := next_label;
    set_error_handler := set_error_handler;
    begin_unit := begin_unit;
    end_unit := end_unit;
    import_unit := import_unit;
    export_unit := export_unit;
    set_source_file := set_source_file;
    set_source_line := set_source_line;
    declare_typename := declare_typename;
    declare_array := declare_array;
    declare_open_array := declare_open_array;
    declare_enum := declare_enum;
    declare_enum_elt := declare_enum_elt;
    declare_packed := declare_packed;
    declare_record := declare_record;
    declare_field := declare_field;
    declare_set := declare_set;
    declare_subrange := declare_subrange;
    declare_pointer := declare_pointer;
    declare_indirect := declare_indirect;
    declare_proctype := declare_proctype;
    declare_formal := declare_formal;
    declare_raises := declare_raises;
    declare_object := declare_object;
    declare_method := declare_method;
    declare_opaque := declare_opaque;
    reveal_opaque := reveal_opaque;
    set_runtime_proc := set_runtime_proc;
    import_global := import_global;
    declare_segment := declare_segment;
    bind_segment := bind_segment;
    declare_global := declare_global;
    declare_constant := declare_constant;
    declare_local := declare_local;
    declare_param := declare_param;
    declare_temp := declare_temp;
    free_temp := free_temp;
    declare_exception := declare_exception;
    widechar_size := widechar_size; 
    begin_init := begin_init;
    end_init := end_init;
    init_int := init_int;
    init_proc := init_proc;
    init_label := init_label;
    init_var := init_var;
    init_offset := init_offset;
    init_chars := init_chars;
    init_float := init_float;
    import_procedure := import_procedure;
    declare_procedure := declare_procedure;
    begin_procedure := begin_procedure;
    end_procedure := end_procedure;
    begin_block := begin_block;
    end_block := end_block;
    note_procedure_origin := note_procedure_origin;
    set_label := set_label;
    jump := jump;
    if_true := if_true;
    if_false := if_false;
    if_compare := if_compare;
    case_jump := case_jump;
    exit_proc := exit_proc;
    load := load;
    store := store;
    load_address := load_address;
    load_indirect := load_indirect;
    store_indirect := store_indirect;
    load_nil := load_nil;
    load_integer := load_integer;
    load_float := load_float;
    compare := compare;
    add := add;
    subtract := subtract;
    multiply := multiply;
    divide := divide;
    div := div;
    mod := mod;
    negate := negate;
    abs := abs;
    max := max;
    min := min;
    cvt_int := cvt_int;
    cvt_float := cvt_float;
    set_union := set_union;
    set_difference := set_difference;
    set_intersection := set_intersection;
    set_sym_difference := set_sym_difference;
    set_member := set_member;
    set_compare := set_compare;
    set_range := set_range;
    set_singleton := set_singleton;
    not := not;
    and := and;
    or := or;
    xor := xor;
    shift := shift;
    shift_left := shift_left;
    shift_right := shift_right;
    rotate := rotate;
    rotate_left := rotate_left;
    rotate_right := rotate_right;
    widen := widen;
    chop := chop;
    extract := extract;
    extract_n := extract_n;
    extract_mn := extract_mn;
    insert := insert;
    insert_n := insert_n;
    insert_mn := insert_mn;
    swap := swap;
    pop := pop;
    copy := copy;
    copy_n := copy_n;
    zero := zero;
    zero_n := zero_n;
    loophole := loophole;
    abort := abort;
    check_nil := check_nil;
    check_lo := check_lo;
    check_hi := check_hi;
    check_range := check_range;
    check_index := check_index;
    check_eq := check_eq;
    add_offset := add_offset;
    index_address := index_address;
    start_call_direct := start_call_direct;
    call_direct := call_direct;
    start_call_indirect := start_call_indirect;
    call_indirect := call_indirect;
    pop_param := pop_param;
    pop_struct := pop_struct;
    pop_static_link := pop_static_link;
    load_procedure := load_procedure;
    load_static_link := load_static_link;
    comment := comment;
    store_ordered := store_ordered;
    load_ordered := load_ordered;
    exchange := exchange;
    compare_exchange := compare_exchange;
    fence := fence;
    fetch_and_op := fetch_and_op;
END;

PROCEDURE AssertFalse() =
BEGIN
  <* ASSERT FALSE *>
END AssertFalse;

<*NOWARN*>PROCEDURE next_label(self: T; n: INTEGER := 1): Label =
BEGIN
AssertFalse();
RETURN 0;
END next_label;

<*NOWARN*>PROCEDURE import_global(self: T; name: Name; byte_size: ByteSize; alignment: Alignment; type: Type; typeid: TypeUID; typename: Name): Var =
BEGIN
AssertFalse();
RETURN NIL;
END import_global;

<*NOWARN*>PROCEDURE declare_segment(self: T; name: Name; typeid: TypeUID; is_const: BOOLEAN): Var =
BEGIN
AssertFalse();
RETURN NIL;
END declare_segment;

<*NOWARN*>PROCEDURE declare_global(self: T; name: Name; byte_size: ByteSize; alignment: Alignment; type: Type; typeid: TypeUID; exported, inited: BOOLEAN; typename: Name): Var =
BEGIN
AssertFalse();
RETURN NIL;
END declare_global;

<*NOWARN*>PROCEDURE declare_constant(self: T; name: Name; byte_size: ByteSize; alignment: Alignment; type: Type; typeid: TypeUID; exported, inited: BOOLEAN; typename: Name): Var =
BEGIN
AssertFalse();
RETURN NIL;
END declare_constant;

<*NOWARN*>PROCEDURE declare_local(self: T; name: Name; byte_size: ByteSize; alignment: Alignment; type: Type; typeid: TypeUID; in_memory, up_level: BOOLEAN; frequency: Frequency; typename: Name): Var =
BEGIN
AssertFalse();
RETURN NIL;
END declare_local;

<*NOWARN*>PROCEDURE declare_param(self: T; name: Name; byte_size: ByteSize; alignment: Alignment; type: Type; typeid: TypeUID; in_memory, up_level: BOOLEAN; frequency: Frequency; typename: Name): Var =
BEGIN
AssertFalse();
RETURN NIL;
END declare_param;

<*NOWARN*>PROCEDURE declare_temp(self: T; byte_size: ByteSize; alignment: Alignment; type: Type; in_memory: BOOLEAN; typename: Name): Var =
BEGIN
AssertFalse();
RETURN NIL;
END declare_temp;

<*NOWARN*>PROCEDURE import_procedure(self: T; name: Name; n_params: INTEGER; ret_type: Type; callingConvention: CallingConvention; return_typeid: TypeUID; return_typename: Name): Proc =
BEGIN
AssertFalse();
RETURN NIL;
END import_procedure;

<*NOWARN*>PROCEDURE declare_procedure(self: T; name: Name; n_params: INTEGER; return_type: Type; level: INTEGER; callingConvention: CallingConvention; exported: BOOLEAN; parent: Proc; return_typeid: TypeUID; return_typename: Name): Proc =
BEGIN
AssertFalse();
RETURN NIL;
END declare_procedure;

<*NOWARN*>PROCEDURE set_error_handler(self: T; p: M3CG_Ops.ErrorHandler) = BEGIN AssertFalse(); END set_error_handler;
<*NOWARN*>PROCEDURE begin_unit(self: T; optimize: INTEGER) = BEGIN AssertFalse(); END begin_unit;
<*NOWARN*>PROCEDURE end_unit(self: T) = BEGIN AssertFalse(); END end_unit;
<*NOWARN*>PROCEDURE import_unit(self: T; name: Name) = BEGIN AssertFalse(); END import_unit;
<*NOWARN*>PROCEDURE export_unit(self: T; name: Name) = BEGIN AssertFalse(); END export_unit;
<*NOWARN*>PROCEDURE set_source_file(self: T; file: TEXT) = BEGIN AssertFalse(); END set_source_file;
<*NOWARN*>PROCEDURE set_source_line(self: T; line: INTEGER) = BEGIN AssertFalse(); END set_source_line;
<*NOWARN*>PROCEDURE declare_typename(self: T; typeid: TypeUID; name: Name) = BEGIN AssertFalse(); END declare_typename;
<*NOWARN*>PROCEDURE declare_array(self: T; typeid, index_typeid, element_typeid: TypeUID; bit_size: BitSize; element_typename: Name) = BEGIN AssertFalse(); END declare_array;
<*NOWARN*>PROCEDURE declare_open_array(self: T; typeid, element_typeid: TypeUID; bit_size: BitSize; element_typename: Name) = BEGIN AssertFalse(); END declare_open_array;
<*NOWARN*>PROCEDURE declare_enum(self: T; typeid: TypeUID; n_elts: INTEGER; bit_size: BitSize) = BEGIN AssertFalse(); END declare_enum;
<*NOWARN*>PROCEDURE declare_enum_elt(self: T; name: Name) = BEGIN AssertFalse(); END declare_enum_elt;
<*NOWARN*>PROCEDURE declare_packed(self: T; typeid: TypeUID; bit_size: BitSize; base: TypeUID; base_typename: Name) = BEGIN AssertFalse(); END declare_packed;
<*NOWARN*>PROCEDURE declare_record(self: T; typeid: TypeUID; bit_size: BitSize; n_fields: INTEGER) = BEGIN AssertFalse(); END declare_record;
<*NOWARN*>PROCEDURE declare_field(self: T; name: Name; bit_offset: BitOffset; bit_size: BitSize; typeid: TypeUID; typename: Name) = BEGIN AssertFalse(); END declare_field;
<*NOWARN*>PROCEDURE declare_set(self: T; t, domain: TypeUID; bit_size: BitSize; domain_typename: Name) = BEGIN AssertFalse(); END declare_set;
<*NOWARN*>PROCEDURE declare_subrange(self: T; typeid, domain_typeid: TypeUID; READONLY min, max: Target.Int; bit_size: BitSize; domain_typename: Name) = BEGIN AssertFalse(); END declare_subrange;
<*NOWARN*>PROCEDURE declare_pointer(self: T; typeid, target_typeid: TypeUID; brand: TEXT; traced: BOOLEAN; target_typename: Name) = BEGIN AssertFalse(); END declare_pointer;
<*NOWARN*>PROCEDURE declare_indirect(self: T; typeid, target_typeid: TypeUID; target_typename: Name) = BEGIN AssertFalse(); END declare_indirect;
<*NOWARN*>PROCEDURE declare_proctype(self: T; typeid: TypeUID; n_formals: INTEGER; result: TypeUID; n_raises: INTEGER; callingConvention: CallingConvention; result_typename: Name) = BEGIN AssertFalse(); END declare_proctype;
<*NOWARN*>PROCEDURE declare_formal(self: T; name: Name; typeid: TypeUID; typename: Name) = BEGIN AssertFalse(); END declare_formal;
<*NOWARN*>PROCEDURE declare_raises(self: T; name: Name) = BEGIN AssertFalse(); END declare_raises;
<*NOWARN*>PROCEDURE declare_object(self: T; typeid, super_typeid: TypeUID; brand: TEXT; traced: BOOLEAN; n_fields, n_methods: INTEGER; field_size: BitSize; super_typename: Name) = BEGIN AssertFalse(); END declare_object;
<*NOWARN*>PROCEDURE declare_method(self: T; name: Name; signature: TypeUID) = BEGIN AssertFalse(); END declare_method;
<*NOWARN*>PROCEDURE declare_opaque(self: T; typeid, super_typeid: TypeUID) = BEGIN AssertFalse(); END declare_opaque;
<*NOWARN*>PROCEDURE reveal_opaque(self: T; lhs_typeid, rhs_typeid: TypeUID) = BEGIN AssertFalse(); END reveal_opaque;
<*NOWARN*>PROCEDURE declare_exception(self: T; name: Name; arg_typeid: TypeUID; raise_proc: BOOLEAN; base: Var; offset: INTEGER) = BEGIN AssertFalse(); END declare_exception;
<*NOWARN*>PROCEDURE widechar_size(self: T; size: INTEGER) = BEGIN AssertFalse(); END widechar_size;
<*NOWARN*>PROCEDURE set_runtime_proc(self: T; name: Name; proc: Proc) = BEGIN AssertFalse(); END set_runtime_proc;
<*NOWARN*>PROCEDURE bind_segment(self: T; segment: Var; byte_size: ByteSize; alignment: Alignment; type: Type; exported, inited: BOOLEAN) = BEGIN AssertFalse(); END bind_segment;
<*NOWARN*>PROCEDURE free_temp(self: T; var: Var) = BEGIN AssertFalse(); END free_temp;
<*NOWARN*>PROCEDURE begin_init(self: T; var: Var) = BEGIN AssertFalse(); END begin_init;
<*NOWARN*>PROCEDURE end_init(self: T; var: Var) = BEGIN AssertFalse(); END end_init;
<*NOWARN*>PROCEDURE init_int(self: T; byte_offset: ByteOffset; READONLY value: Target.Int; type: Type) = BEGIN AssertFalse(); END init_int;
<*NOWARN*>PROCEDURE init_proc(self: T; byte_offset: ByteOffset; value: Proc) = BEGIN AssertFalse(); END init_proc;
<*NOWARN*>PROCEDURE init_label(self: T; byte_offset: ByteOffset; value: Label) = BEGIN AssertFalse(); END init_label;
<*NOWARN*>PROCEDURE init_var(self: T; byte_offset: ByteOffset; value: Var; bias: ByteOffset) = BEGIN AssertFalse(); END init_var;
<*NOWARN*>PROCEDURE init_offset(self: T; byte_offset: ByteOffset; value: Var) = BEGIN AssertFalse(); END init_offset;
<*NOWARN*>PROCEDURE init_chars(self: T; byte_offset: ByteOffset; value: TEXT) = BEGIN AssertFalse(); END init_chars;
<*NOWARN*>PROCEDURE init_float(self: T; byte_offset: ByteOffset; READONLY f: Target.Float) = BEGIN AssertFalse(); END init_float;
<*NOWARN*>PROCEDURE begin_procedure(self: T; proc: Proc) = BEGIN AssertFalse(); END begin_procedure;
<*NOWARN*>PROCEDURE end_procedure(self: T; proc: Proc) = BEGIN AssertFalse(); END end_procedure;
<*NOWARN*>PROCEDURE begin_block(self: T) = BEGIN AssertFalse(); END begin_block;
<*NOWARN*>PROCEDURE end_block(self: T) = BEGIN AssertFalse(); END end_block;
<*NOWARN*>PROCEDURE note_procedure_origin(self: T; proc: Proc) = BEGIN AssertFalse(); END note_procedure_origin;
<*NOWARN*>PROCEDURE set_label(self: T; label: Label; barrier: BOOLEAN) = BEGIN AssertFalse(); END set_label;
<*NOWARN*>PROCEDURE jump(self: T; label: Label) = BEGIN AssertFalse(); END jump;
<*NOWARN*>PROCEDURE if_true(self: T; type: IType; label: Label; frequency: Frequency) = BEGIN AssertFalse(); END if_true;
<*NOWARN*>PROCEDURE if_false(self: T; type: IType; label: Label; frequency: Frequency) = BEGIN AssertFalse(); END if_false;
<*NOWARN*>PROCEDURE if_compare(self: T; type: ZType; op: CompareOp; label: Label; frequency: Frequency) = BEGIN AssertFalse(); END if_compare;
<*NOWARN*>PROCEDURE case_jump(self: T; type: IType; READONLY labels: ARRAY OF Label) = BEGIN AssertFalse(); END case_jump;
<*NOWARN*>PROCEDURE exit_proc(self: T; type: Type) = BEGIN AssertFalse(); END exit_proc;
<*NOWARN*>PROCEDURE load(self: T; var: Var; byte_offset: ByteOffset; mtype: MType; ztype: ZType) = BEGIN AssertFalse(); END load;
<*NOWARN*>PROCEDURE store(self: T; var: Var; byte_offset: ByteOffset; ztype: ZType; mtype: MType) = BEGIN AssertFalse(); END store;
<*NOWARN*>PROCEDURE load_address(self: T; var: Var; byte_offset: ByteOffset) = BEGIN AssertFalse(); END load_address;
<*NOWARN*>PROCEDURE load_indirect(self: T; byte_offset: ByteOffset; mtype: MType; ztype: ZType) = BEGIN AssertFalse(); END load_indirect;
<*NOWARN*>PROCEDURE store_indirect(self: T; byte_offset: ByteOffset; ztype: ZType; mtype: MType) = BEGIN AssertFalse(); END store_indirect;
<*NOWARN*>PROCEDURE load_nil(self: T) = BEGIN AssertFalse(); END load_nil;
<*NOWARN*>PROCEDURE load_integer(self: T; type: IType; READONLY int: Target.Int) = BEGIN AssertFalse(); END load_integer;
<*NOWARN*>PROCEDURE load_float(self: T; type: RType; READONLY float: Target.Float) = BEGIN AssertFalse(); END load_float;
<*NOWARN*>PROCEDURE compare(self: T; ztype: ZType; itype: IType; op: CompareOp) = BEGIN AssertFalse(); END compare;
<*NOWARN*>PROCEDURE add(self: T; type: AType) = BEGIN AssertFalse(); END add;
<*NOWARN*>PROCEDURE subtract(self: T; type: AType) = BEGIN AssertFalse(); END subtract;
<*NOWARN*>PROCEDURE multiply(self: T; type: AType) = BEGIN AssertFalse(); END multiply;
<*NOWARN*>PROCEDURE divide(self: T; type: RType) = BEGIN AssertFalse(); END divide;
<*NOWARN*>PROCEDURE div(self: T; type: IType; a, b: Sign) = BEGIN AssertFalse(); END div;
<*NOWARN*>PROCEDURE mod(self: T; type: IType; a, b: Sign) = BEGIN AssertFalse(); END mod;
<*NOWARN*>PROCEDURE negate(self: T; type: AType) = BEGIN AssertFalse(); END negate;
<*NOWARN*>PROCEDURE abs(self: T; type: AType) = BEGIN AssertFalse(); END abs;
<*NOWARN*>PROCEDURE max(self: T; type: ZType) = BEGIN AssertFalse(); END max;
<*NOWARN*>PROCEDURE min(self: T; type: ZType) = BEGIN AssertFalse(); END min;
<*NOWARN*>PROCEDURE cvt_int(self: T; rtype: RType; itype: IType; op: ConvertOp) = BEGIN AssertFalse(); END cvt_int;
<*NOWARN*>PROCEDURE cvt_float(self: T; atype: AType; rtype: RType) = BEGIN AssertFalse(); END cvt_float;
<*NOWARN*>PROCEDURE set_union(self: T; byte_size: ByteSize) = BEGIN AssertFalse(); END set_union;
<*NOWARN*>PROCEDURE set_difference(self: T; byte_size: ByteSize) = BEGIN AssertFalse(); END set_difference;
<*NOWARN*>PROCEDURE set_intersection(self: T; byte_size: ByteSize) = BEGIN AssertFalse(); END set_intersection;
<*NOWARN*>PROCEDURE set_sym_difference(self: T; byte_size: ByteSize) = BEGIN AssertFalse(); END set_sym_difference;
<*NOWARN*>PROCEDURE set_member(self: T; byte_size: ByteSize; type: IType) = BEGIN AssertFalse(); END set_member;
<*NOWARN*>PROCEDURE set_compare(self: T; byte_size: ByteSize; op: CompareOp; type: IType) = BEGIN AssertFalse(); END set_compare;
<*NOWARN*>PROCEDURE set_range(self: T; byte_size: ByteSize; type: IType) = BEGIN AssertFalse(); END set_range;
<*NOWARN*>PROCEDURE set_singleton(self: T; byte_size: ByteSize; type: IType) = BEGIN AssertFalse(); END set_singleton;
<*NOWARN*>PROCEDURE not(self: T; type: IType) = BEGIN AssertFalse(); END not;
<*NOWARN*>PROCEDURE and(self: T; type: IType) = BEGIN AssertFalse(); END and;
<*NOWARN*>PROCEDURE or(self: T; type: IType) = BEGIN AssertFalse(); END or;
<*NOWARN*>PROCEDURE xor(self: T; type: IType) = BEGIN AssertFalse(); END xor;
<*NOWARN*>PROCEDURE shift(self: T; type: IType) = BEGIN AssertFalse(); END shift;
<*NOWARN*>PROCEDURE shift_left(self: T; type: IType) = BEGIN AssertFalse(); END shift_left;
<*NOWARN*>PROCEDURE shift_right(self: T; type: IType) = BEGIN AssertFalse(); END shift_right;
<*NOWARN*>PROCEDURE rotate(self: T; type: IType) = BEGIN AssertFalse(); END rotate;
<*NOWARN*>PROCEDURE rotate_left(self: T; type: IType) = BEGIN AssertFalse(); END rotate_left;
<*NOWARN*>PROCEDURE rotate_right(self: T; type: IType) = BEGIN AssertFalse(); END rotate_right;
<*NOWARN*>PROCEDURE widen(self: T; sign: BOOLEAN) = BEGIN AssertFalse(); END widen;
<*NOWARN*>PROCEDURE chop(self: T) = BEGIN AssertFalse(); END chop;
<*NOWARN*>PROCEDURE extract(self: T; type: IType; sign: BOOLEAN) = BEGIN AssertFalse(); END extract;
<*NOWARN*>PROCEDURE extract_n(self: T; type: IType; sign: BOOLEAN; n: CARDINAL) = BEGIN AssertFalse(); END extract_n;
<*NOWARN*>PROCEDURE extract_mn(self: T; type: IType; sign: BOOLEAN; m, n: CARDINAL) = BEGIN AssertFalse(); END extract_mn;
<*NOWARN*>PROCEDURE insert(self: T; type: IType) = BEGIN AssertFalse(); END insert;
<*NOWARN*>PROCEDURE insert_n(self: T; type: IType; n: CARDINAL) = BEGIN AssertFalse(); END insert_n;
<*NOWARN*>PROCEDURE insert_mn(self: T; type: IType; m, n: CARDINAL) = BEGIN AssertFalse(); END insert_mn;
<*NOWARN*>PROCEDURE swap(self: T; a, b: Type) = BEGIN AssertFalse(); END swap;
<*NOWARN*>PROCEDURE pop(self: T; type: Type) = BEGIN AssertFalse(); END pop;
<*NOWARN*>PROCEDURE copy_n(self: T; itype: IType; mtype: MType; overlap: BOOLEAN) = BEGIN AssertFalse(); END copy_n;
<*NOWARN*>PROCEDURE copy(self: T; n: INTEGER; mtype: MType; overlap: BOOLEAN) = BEGIN AssertFalse(); END copy;
<*NOWARN*>PROCEDURE zero_n(self: T; itype: IType; mtype: MType) = BEGIN AssertFalse(); END zero_n;
<*NOWARN*>PROCEDURE zero(self: T; n: INTEGER; type: MType) = BEGIN AssertFalse(); END zero;
<*NOWARN*>PROCEDURE loophole(self: T; from, to: ZType) = BEGIN AssertFalse(); END loophole;
<*NOWARN*>PROCEDURE abort(self: T; code: RuntimeError) = BEGIN AssertFalse(); END abort;
<*NOWARN*>PROCEDURE check_nil(self: T; code: RuntimeError) = BEGIN AssertFalse(); END check_nil;
<*NOWARN*>PROCEDURE check_lo(self: T; type: IType; READONLY i: Target.Int; code: RuntimeError) = BEGIN AssertFalse(); END check_lo;
<*NOWARN*>PROCEDURE check_hi(self: T; type: IType; READONLY i: Target.Int; code: RuntimeError) = BEGIN AssertFalse(); END check_hi;
<*NOWARN*>PROCEDURE check_range(self: T; type: IType; READONLY a, b: Target.Int; code: RuntimeError) = BEGIN AssertFalse(); END check_range;
<*NOWARN*>PROCEDURE check_index(self: T; type: IType; code: RuntimeError) = BEGIN AssertFalse(); END check_index;
<*NOWARN*>PROCEDURE check_eq(self: T; type: IType; code: RuntimeError) = BEGIN AssertFalse(); END check_eq;
<*NOWARN*>PROCEDURE add_offset(self: T; i: INTEGER) = BEGIN AssertFalse(); END add_offset;
<*NOWARN*>PROCEDURE index_address(self: T; type: IType; size: INTEGER) = BEGIN AssertFalse(); END index_address;
<*NOWARN*>PROCEDURE start_call_direct(self: T; proc: Proc; level: INTEGER; type: Type) = BEGIN AssertFalse(); END start_call_direct;
<*NOWARN*>PROCEDURE start_call_indirect(self: T; type: Type; callingConvention: CallingConvention) = BEGIN AssertFalse(); END start_call_indirect;
<*NOWARN*>PROCEDURE pop_param(self: T; type: MType) = BEGIN AssertFalse(); END pop_param;
<*NOWARN*>PROCEDURE pop_struct(self: T; typeid: TypeUID; byte_size: ByteSize; alignment: Alignment) = BEGIN AssertFalse(); END pop_struct;
<*NOWARN*>PROCEDURE pop_static_link(self: T) = BEGIN AssertFalse(); END pop_static_link;
<*NOWARN*>PROCEDURE call_direct(self: T; proc: Proc; type: Type) = BEGIN AssertFalse(); END call_direct;
<*NOWARN*>PROCEDURE call_indirect(self: T; type: Type; callingConvention: CallingConvention) = BEGIN AssertFalse(); END call_indirect;
<*NOWARN*>PROCEDURE load_procedure(self: T; proc: Proc) = BEGIN AssertFalse(); END load_procedure;
<*NOWARN*>PROCEDURE load_static_link(self: T; proc: Proc) = BEGIN AssertFalse(); END load_static_link;
<*NOWARN*>PROCEDURE comment(self: T; a, b, c, d: TEXT := NIL) = BEGIN AssertFalse(); END comment;
<*NOWARN*>PROCEDURE store_ordered(self: T; ztype: ZType; mtype: MType; order: MemoryOrder) = BEGIN AssertFalse(); END store_ordered;
<*NOWARN*>PROCEDURE load_ordered(self: T; mtype: MType; ztype: ZType; order: MemoryOrder) = BEGIN AssertFalse(); END load_ordered;
<*NOWARN*>PROCEDURE exchange(self: T; mtype: MType; ztype: ZType; order: MemoryOrder) = BEGIN AssertFalse(); END exchange;
<*NOWARN*>PROCEDURE compare_exchange(self: T; mtype: MType; ztype: ZType; r: IType; success, failure: MemoryOrder) = BEGIN AssertFalse(); END compare_exchange;
<*NOWARN*>PROCEDURE fence(self: T; order: MemoryOrder) = BEGIN AssertFalse(); END fence;
<*NOWARN*>PROCEDURE fetch_and_op(self: T; op: AtomicOp; mtype: MType; ztype: ZType; order: MemoryOrder) = BEGIN AssertFalse(); END fetch_and_op;

BEGIN
END M3CG_AssertFalse.
