MODULE M3CG_DoNothing;

IMPORT M3CG, M3CG_Ops;
IMPORT Target;
FROM M3CG IMPORT Name, ByteOffset, TypeUID, CallingConvention;
FROM M3CG IMPORT BitSize, ByteSize, Alignment, Frequency;
FROM M3CG IMPORT Var, Proc, Label, Sign, BitOffset;
FROM M3CG IMPORT Type, ZType, AType, RType, IType, MType;
FROM M3CG IMPORT CompareOp, ConvertOp, AtomicOp, RuntimeError;
FROM M3CG IMPORT MemoryOrder;

TYPE WrVar = Var OBJECT tag: INTEGER END;
TYPE WrProc = Proc OBJECT tag: INTEGER END;

TYPE
  U = M3CG.T OBJECT
    next_label_id := 1;
    next_var := 1;
    next_proc := 1;
    next_scope := 1;
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

PROCEDURE New(): M3CG.T =
BEGIN
  RETURN NEW(U);
END New;

PROCEDURE next_label(u: U; n: INTEGER := 1): Label =
VAR label := u.next_label_id;
BEGIN
  INC(u.next_label_id, n);
  RETURN label;
END next_label;

<*NOWARN*>PROCEDURE set_error_handler(<*UNUSED*> u: U; <*UNUSED*> p: M3CG_Ops.ErrorHandler) = BEGIN END set_error_handler;
<*NOWARN*>PROCEDURE begin_unit(u: U; optimize: INTEGER) = BEGIN END begin_unit;
<*NOWARN*>PROCEDURE end_unit(u: U) = BEGIN END end_unit;
<*NOWARN*>PROCEDURE import_unit(u: U; n: Name) = BEGIN END import_unit;
<*NOWARN*>PROCEDURE export_unit(u: U; n: Name) = BEGIN END export_unit;
<*NOWARN*>PROCEDURE set_source_file(u: U; file: TEXT) = BEGIN END set_source_file;
<*NOWARN*>PROCEDURE set_source_line(u: U; line: INTEGER) = BEGIN END set_source_line;
<*NOWARN*>PROCEDURE declare_typename(u: U; t: TypeUID; n: Name) = BEGIN END declare_typename;
<*NOWARN*>PROCEDURE declare_array(u: U; t, index, elt: TypeUID; s: BitSize) = BEGIN END declare_array;
<*NOWARN*>PROCEDURE declare_open_array(u: U; t, elt: TypeUID; s: BitSize) = BEGIN END declare_open_array;
<*NOWARN*>PROCEDURE declare_enum(u: U; t: TypeUID; n_elts: INTEGER; s: BitSize) = BEGIN END declare_enum;
<*NOWARN*>PROCEDURE declare_enum_elt(u: U; n: Name) = BEGIN END declare_enum_elt;
<*NOWARN*>PROCEDURE declare_packed(u: U; t: TypeUID; s: BitSize; base: TypeUID) = BEGIN END declare_packed;
<*NOWARN*>PROCEDURE declare_record(u: U; t: TypeUID; s: BitSize; n_fields: INTEGER) = BEGIN END declare_record;
<*NOWARN*>PROCEDURE declare_field(u: U; n: Name; o: BitOffset; s: BitSize; t: TypeUID) = BEGIN END declare_field;
<*NOWARN*>PROCEDURE declare_set(u: U; t, domain: TypeUID; s: BitSize) = BEGIN END declare_set;
<*NOWARN*>PROCEDURE declare_subrange(u: U; t, domain: TypeUID; READONLY min, max: Target.Int; s: BitSize) = BEGIN END declare_subrange;
<*NOWARN*>PROCEDURE declare_pointer(u: U; t, target: TypeUID; brand: TEXT; traced: BOOLEAN) = BEGIN END declare_pointer;
<*NOWARN*>PROCEDURE declare_indirect(u: U; t, target: TypeUID) = BEGIN END declare_indirect;
<*NOWARN*>PROCEDURE declare_proctype(u: U; t: TypeUID; n_formals: INTEGER; result: TypeUID; n_raises: INTEGER; callingConvention: CallingConvention) = BEGIN END declare_proctype;
<*NOWARN*>PROCEDURE declare_formal(u: U; n: Name; t: TypeUID) = BEGIN END declare_formal;
<*NOWARN*>PROCEDURE declare_raises(u: U; n: Name) = BEGIN END declare_raises;
<*NOWARN*>PROCEDURE declare_object(u: U; t, super: TypeUID; brand: TEXT; traced: BOOLEAN; n_fields, n_methods: INTEGER; field_size: BitSize) = BEGIN END declare_object;
<*NOWARN*>PROCEDURE declare_method(u: U; n: Name; signature: TypeUID) = BEGIN END declare_method;
<*NOWARN*>PROCEDURE declare_opaque(u: U; t, super: TypeUID) = BEGIN END declare_opaque;
<*NOWARN*>PROCEDURE reveal_opaque(u: U; lhs, rhs: TypeUID) = BEGIN END reveal_opaque;
<*NOWARN*>PROCEDURE declare_exception(u: U; n: Name; arg_type: TypeUID; raise_proc: BOOLEAN; base: Var; offset: INTEGER) = BEGIN END declare_exception;
<*NOWARN*>PROCEDURE set_runtime_proc(u: U; n: Name; p: Proc) = BEGIN END set_runtime_proc;

PROCEDURE NewVar(u: U): Var =
VAR v := NEW(WrVar, tag := u.next_var);
BEGIN
  INC(u.next_var);
  RETURN v;
END NewVar;

<*NOWARN*>PROCEDURE import_global(u: U; n: Name; s: ByteSize; a: Alignment; t: Type; type: TypeUID): Var =
BEGIN
  RETURN NewVar(u);
END import_global;

<*NOWARN*>PROCEDURE declare_segment(u: U; n: Name; type: TypeUID; is_const: BOOLEAN): Var =
BEGIN
  RETURN NewVar(u);
END declare_segment;

<*NOWARN*>PROCEDURE bind_segment(u: U; seg: Var; s: ByteSize; a: Alignment; t: Type; exported, inited: BOOLEAN) = BEGIN END bind_segment;

<*NOWARN*>PROCEDURE declare_global(u: U; n: Name; s: ByteSize; a: Alignment; t: Type; type: TypeUID; exported, inited: BOOLEAN): Var =
BEGIN
  RETURN NewVar(u);
END declare_global;

<*NOWARN*>PROCEDURE declare_constant(u: U; n: Name; s: ByteSize; a: Alignment; t: Type; type: TypeUID; exported, inited: BOOLEAN): Var =
BEGIN
  RETURN NewVar(u);
END declare_constant;

<*NOWARN*>PROCEDURE declare_local(u: U; n: Name; s: ByteSize; a: Alignment; t: Type; type: TypeUID; in_memory, up_level: BOOLEAN; f: Frequency): Var =
BEGIN
  RETURN NewVar(u);
END declare_local;

<*NOWARN*>PROCEDURE declare_param(u: U; n: Name; s: ByteSize; a: Alignment; t: Type; type: TypeUID; in_memory, up_level: BOOLEAN; f: Frequency): Var =
BEGIN
  RETURN NewVar(u);
END declare_param;

<*NOWARN*>PROCEDURE declare_temp(u: U; s: ByteSize; a: Alignment; t: Type; in_memory: BOOLEAN): Var =
BEGIN
  RETURN NewVar(u);
END declare_temp;

<*NOWARN*>PROCEDURE free_temp(u: U; v: Var) = BEGIN END free_temp;
<*NOWARN*>PROCEDURE begin_init(u: U; v: Var) = BEGIN END begin_init;
<*NOWARN*>PROCEDURE end_init(u: U; v: Var) = BEGIN END end_init;
<*NOWARN*>PROCEDURE init_int(u: U; o: ByteOffset; READONLY value: Target.Int; t: Type) = BEGIN END init_int;
<*NOWARN*>PROCEDURE init_proc(u: U; o: ByteOffset; value: Proc) = BEGIN END init_proc;
<*NOWARN*>PROCEDURE init_label(u: U; o: ByteOffset; value: Label) = BEGIN END init_label;
<*NOWARN*>PROCEDURE init_var(u: U; o: ByteOffset; value: Var; bias: ByteOffset) = BEGIN END init_var;
<*NOWARN*>PROCEDURE init_offset(u: U; o: ByteOffset; value: Var) = BEGIN END init_offset;
<*NOWARN*>PROCEDURE init_chars(u: U; o: ByteOffset; value: TEXT) = BEGIN END init_chars;
<*NOWARN*>PROCEDURE init_float(u: U; o: ByteOffset; READONLY f: Target.Float) = BEGIN END init_float;

PROCEDURE NewProc(u: U): Proc =
VAR p := NEW(WrProc, tag := u.next_proc);
BEGIN
  INC(u.next_proc);
  RETURN p;
END NewProc;

<*NOWARN*>PROCEDURE import_procedure(u: U; n: Name; n_params: INTEGER; ret_type: Type; callingConvention: CallingConvention): Proc =
BEGIN
  RETURN NewProc(u);
END import_procedure;

<*NOWARN*>PROCEDURE declare_procedure(u: U; n: Name; n_params: INTEGER; return_type: Type; lev: INTEGER; callingConvention: CallingConvention; exported: BOOLEAN; parent: Proc): Proc =
BEGIN
  RETURN NewProc(u);
END declare_procedure;

<*NOWARN*>PROCEDURE begin_procedure(u: U; p: Proc) = BEGIN END begin_procedure;
<*NOWARN*>PROCEDURE end_procedure(u: U; p: Proc) = BEGIN END end_procedure;
<*NOWARN*>PROCEDURE begin_block(u: U) = BEGIN END begin_block;
<*NOWARN*>PROCEDURE end_block(u: U) = BEGIN END end_block;
<*NOWARN*>PROCEDURE note_procedure_origin(u: U; p: Proc) = BEGIN END note_procedure_origin;
<*NOWARN*>PROCEDURE set_label(u: U; l: Label; barrier: BOOLEAN) = BEGIN END set_label;
<*NOWARN*>PROCEDURE jump(u: U; l: Label) = BEGIN END jump;
<*NOWARN*>PROCEDURE if_true(u: U; t: IType; l: Label; f: Frequency) = BEGIN END if_true;
<*NOWARN*>PROCEDURE if_false(u: U; t: IType; l: Label; f: Frequency) = BEGIN END if_false;
<*NOWARN*>PROCEDURE if_compare(u: U; t: ZType; op: CompareOp; l: Label; f: Frequency) = BEGIN END if_compare;
<*NOWARN*>PROCEDURE case_jump(u: U; t: IType; READONLY labels: ARRAY OF Label) = BEGIN END case_jump;
<*NOWARN*>PROCEDURE exit_proc(u: U; t: Type) = BEGIN END exit_proc;
<*NOWARN*>PROCEDURE load(u: U; v: Var; o: ByteOffset; t: MType; z: ZType) = BEGIN END load;
<*NOWARN*>PROCEDURE store(u: U; v: Var; o: ByteOffset; t: ZType; z: MType) = BEGIN END store;
<*NOWARN*>PROCEDURE load_address(u: U; v: Var; o: ByteOffset) = BEGIN END load_address;
<*NOWARN*>PROCEDURE load_indirect(u: U; o: ByteOffset; t: MType; z: ZType) = BEGIN END load_indirect;
<*NOWARN*>PROCEDURE store_indirect(u: U; o: ByteOffset; t: ZType; z: MType) = BEGIN END store_indirect;
<*NOWARN*>PROCEDURE load_nil(u: U) = BEGIN END load_nil;
<*NOWARN*>PROCEDURE load_integer(u: U; t: IType; READONLY i: Target.Int) = BEGIN END load_integer;
<*NOWARN*>PROCEDURE load_float(u: U; t: RType; READONLY f: Target.Float) = BEGIN END load_float;
<*NOWARN*>PROCEDURE compare(u: U; t: ZType; z: IType; op: CompareOp) = BEGIN END compare;
<*NOWARN*>PROCEDURE add(u: U; t: AType) = BEGIN END add;
<*NOWARN*>PROCEDURE subtract(u: U; t: AType) = BEGIN END subtract;
<*NOWARN*>PROCEDURE multiply(u: U; t: AType) = BEGIN END multiply;
<*NOWARN*>PROCEDURE divide(u: U; t: RType) = BEGIN END divide;
<*NOWARN*>PROCEDURE div(u: U; t: IType; a, b: Sign) = BEGIN END div;
<*NOWARN*>PROCEDURE mod(u: U; t: IType; a, b: Sign) = BEGIN END mod;
<*NOWARN*>PROCEDURE negate(u: U; t: AType) = BEGIN END negate;
<*NOWARN*>PROCEDURE abs(u: U; t: AType) = BEGIN END abs;
<*NOWARN*>PROCEDURE max(u: U; t: ZType) = BEGIN END max;
<*NOWARN*>PROCEDURE min(u: U; t: ZType) = BEGIN END min;
<*NOWARN*>PROCEDURE cvt_int(u: U; t: RType; x: IType; op: ConvertOp) = BEGIN END cvt_int;
<*NOWARN*>PROCEDURE cvt_float(u: U; t: AType; x: RType) = BEGIN END cvt_float;
<*NOWARN*>PROCEDURE set_union(u: U; s: ByteSize) = BEGIN END set_union;
<*NOWARN*>PROCEDURE set_difference(u: U; s: ByteSize) = BEGIN END set_difference;
<*NOWARN*>PROCEDURE set_intersection(u: U; s: ByteSize) = BEGIN END set_intersection;
<*NOWARN*>PROCEDURE set_sym_difference(u: U; s: ByteSize) = BEGIN END set_sym_difference;
<*NOWARN*>PROCEDURE set_member(u: U; s: ByteSize; t: IType) = BEGIN END set_member;
<*NOWARN*>PROCEDURE set_compare(u: U; s: ByteSize; op: CompareOp; t: IType) = BEGIN END set_compare;
<*NOWARN*>PROCEDURE set_range(u: U; s: ByteSize; t: IType) = BEGIN END set_range;
<*NOWARN*>PROCEDURE set_singleton(u: U; s: ByteSize; t: IType) = BEGIN END set_singleton;
<*NOWARN*>PROCEDURE not(u: U; t: IType) = BEGIN END not;
<*NOWARN*>PROCEDURE and(u: U; t: IType) = BEGIN END and;
<*NOWARN*>PROCEDURE or(u: U; t: IType) = BEGIN END or;
<*NOWARN*>PROCEDURE xor(u: U; t: IType) = BEGIN END xor;
<*NOWARN*>PROCEDURE shift(u: U; t: IType) = BEGIN END shift;
<*NOWARN*>PROCEDURE shift_left(u: U; t: IType) = BEGIN END shift_left;
<*NOWARN*>PROCEDURE shift_right(u: U; t: IType) = BEGIN END shift_right;
<*NOWARN*>PROCEDURE rotate(u: U; t: IType) = BEGIN END rotate;
<*NOWARN*>PROCEDURE rotate_left(u: U; t: IType) = BEGIN END rotate_left;
<*NOWARN*>PROCEDURE rotate_right(u: U; t: IType) = BEGIN END rotate_right;
<*NOWARN*>PROCEDURE widen(u: U; sign: BOOLEAN) = BEGIN END widen;
<*NOWARN*>PROCEDURE chop(u: U) = BEGIN END chop;
<*NOWARN*>PROCEDURE extract(u: U; t: IType; sign: BOOLEAN) = BEGIN END extract;
<*NOWARN*>PROCEDURE extract_n(u: U; t: IType; sign: BOOLEAN; n: CARDINAL) = BEGIN END extract_n;
<*NOWARN*>PROCEDURE extract_mn(u: U; t: IType; sign: BOOLEAN; m, n: CARDINAL) = BEGIN END extract_mn;
<*NOWARN*>PROCEDURE insert(u: U; t: IType) = BEGIN END insert;
<*NOWARN*>PROCEDURE insert_n(u: U; t: IType; n: CARDINAL) = BEGIN END insert_n;
<*NOWARN*>PROCEDURE insert_mn(u: U; t: IType; m, n: CARDINAL) = BEGIN END insert_mn;
<*NOWARN*>PROCEDURE swap(u: U; a, b: Type) = BEGIN END swap;
<*NOWARN*>PROCEDURE pop(u: U; t: Type) = BEGIN END pop;
<*NOWARN*>PROCEDURE copy_n(u: U; z: IType; t: MType; overlap: BOOLEAN) = BEGIN END copy_n;
<*NOWARN*>PROCEDURE copy(u: U; n: INTEGER; t: MType; overlap: BOOLEAN) = BEGIN END copy;
<*NOWARN*>PROCEDURE zero_n(u: U; z: IType; t: MType) = BEGIN END zero_n;
<*NOWARN*>PROCEDURE zero(u: U; n: INTEGER; t: MType) = BEGIN END zero;
<*NOWARN*>PROCEDURE loophole(u: U; from, two: ZType) = BEGIN END loophole;
<*NOWARN*>PROCEDURE abort(u: U; code: RuntimeError) = BEGIN END abort;
<*NOWARN*>PROCEDURE check_nil(u: U; code: RuntimeError) = BEGIN END check_nil;
<*NOWARN*>PROCEDURE check_lo(u: U; t: IType; READONLY i: Target.Int; code: RuntimeError) = BEGIN END check_lo;
<*NOWARN*>PROCEDURE check_hi(u: U; t: IType; READONLY i: Target.Int; code: RuntimeError) = BEGIN END check_hi;
<*NOWARN*>PROCEDURE check_range(u: U; t: IType; READONLY a, b: Target.Int; code: RuntimeError) = BEGIN END check_range;
<*NOWARN*>PROCEDURE check_index(u: U; t: IType; code: RuntimeError) = BEGIN END check_index;
<*NOWARN*>PROCEDURE check_eq(u: U; t: IType; code: RuntimeError) = BEGIN END check_eq;
<*NOWARN*>PROCEDURE add_offset(u: U; i: INTEGER) = BEGIN END add_offset;
<*NOWARN*>PROCEDURE index_address(u: U; t: IType; size: INTEGER) = BEGIN END index_address;
<*NOWARN*>PROCEDURE start_call_direct(u: U; p: Proc; lev: INTEGER; t: Type) = BEGIN END start_call_direct;
<*NOWARN*>PROCEDURE start_call_indirect(u: U; t: Type; callingConvention: CallingConvention) = BEGIN END start_call_indirect;
<*NOWARN*>PROCEDURE pop_param(u: U; t: MType) = BEGIN END pop_param;
<*NOWARN*>PROCEDURE pop_struct(u: U; t: TypeUID; s: ByteSize; a: Alignment) = BEGIN END pop_struct;
<*NOWARN*>PROCEDURE pop_static_link(u: U) = BEGIN END pop_static_link;
<*NOWARN*>PROCEDURE call_direct(u: U; p: Proc; t: Type) = BEGIN END call_direct;
<*NOWARN*>PROCEDURE call_indirect(u: U; t: Type; callingConvention: CallingConvention) = BEGIN END call_indirect;
<*NOWARN*>PROCEDURE load_procedure(u: U; p: Proc) = BEGIN END load_procedure;
<*NOWARN*>PROCEDURE load_static_link(u: U; p: Proc) = BEGIN END load_static_link;
<*NOWARN*>PROCEDURE comment(u: U; a, b, c, d: TEXT := NIL) = BEGIN END comment;
<*NOWARN*>PROCEDURE store_ordered(u: U; t: ZType; z: MType; order: MemoryOrder) = BEGIN END store_ordered;
<*NOWARN*>PROCEDURE load_ordered(u: U; t: MType; z: ZType; order: MemoryOrder) = BEGIN END load_ordered;
<*NOWARN*>PROCEDURE exchange(u: U; t: MType; z: ZType; order: MemoryOrder) = BEGIN END exchange;
<*NOWARN*>PROCEDURE compare_exchange(u: U; t: MType; z: ZType; r: IType; success, failure: MemoryOrder) = BEGIN END compare_exchange;
<*NOWARN*>PROCEDURE fence(u: U; order: MemoryOrder) = BEGIN END fence;
<*NOWARN*>PROCEDURE fetch_and_op(u: U; op: AtomicOp; t: MType; z: ZType; order: MemoryOrder) = BEGIN END fetch_and_op;

BEGIN
END M3CG_DoNothing.
