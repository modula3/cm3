MODULE M3CG_MultiPass;

IMPORT M3CG, M3CG_Ops, RefSeq, Target;
FROM M3CG IMPORT Type, MType, IType, RType, AType, ZType, Sign;
FROM M3CG IMPORT Name, Var, Proc, Alignment, TypeUID, Label;
FROM M3CG IMPORT Frequency, CallingConvention, CompareOp, ConvertOp, AtomicOp;
FROM M3CG IMPORT BitSize, ByteSize, BitOffset, ByteOffset, RuntimeError;
FROM M3CG IMPORT MemoryOrder;
FROM M3CG_Binary IMPORT Op;

TYPE var_t = Var OBJECT tag: INTEGER END;
TYPE proc_t = Proc OBJECT tag: INTEGER END;

(* vars and procs (and labels?); maximum count not known ahead of time *)
TYPE GrowableRefs_t =  OBJECT
    data: RefSeq.T := NIL;
METHODS
    Init(): GrowableRefs_t := GrowableRefs_Init;
    Size(): INTEGER := GrowableRefs_Size;
    NewRef(ref: REFANY): INTEGER := GrowableRefs_NewRef;
    NewVar(): var_t := GrowableRefs_NewVar;
    NewProc(): proc_t := GrowableRefs_NewProc;
END;

(* vars and procs (and labels?); maximum count known ahead of time *)
TYPE StaticRefs_t = OBJECT
    data: REF ARRAY OF REFANY := NIL;
METHODS
    Init(count: INTEGER): StaticRefs_t := StaticRefs_Init;
    GetVar(index: INTEGER): var_t := StaticRefs_GetVar;
    GetProc(index: INTEGER): proc_t := StaticRefs_GetProc;
END;

PROCEDURE StaticRefs_Init(self: StaticRefs_t; count: INTEGER): StaticRefs_t =
BEGIN
    self.data := NEW(REF ARRAY OF REFANY, count);
    RETURN self;
END StaticRefs_Init;

PROCEDURE StaticRefs_GetVar(self: StaticRefs_t; tag: INTEGER): var_t =
BEGIN
    IF tag = 0 THEN
        RETURN NIL;
    END;
    RETURN NARROW(self.data[tag], var_t);
END StaticRefs_GetVar;

PROCEDURE StaticRefs_GetProc(self: StaticRefs_t; tag: INTEGER): proc_t =
BEGIN
    IF tag = 0 THEN
        RETURN NIL;
    END;
    RETURN NARROW(self.data[tag], proc_t);
END StaticRefs_GetProc;

REVEAL
T = Public BRANDED "M3CG_MultiPass.T" OBJECT
    refs: GrowableRefs_t := NIL;
    next_label_id := 1;
    total_ops: INTEGER := 0;
    private_data: RefSeq.T := NIL;
METHODS
    Add(a: op_t) := Add;
OVERRIDES
    Init := Init;
    Replay := Replay;

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

PROCEDURE Init(self: T): T =
BEGIN
    self.private_data := NEW(RefSeq.T).init();
    self.refs := NEW(GrowableRefs_t).Init();
    EVAL self.refs.NewRef(NIL); (* throw away zero *)
    RETURN self;
END Init;

REVEAL Replay_t = BRANDED "M3CG_MultiPass.Replay_t" OBJECT
    refs: StaticRefs_t; (* vars and procs *)
    reuse_refs := FALSE;
METHODS
    Init(refcount: INTEGER): Replay_t := Replay_Init;
    PutRef(tag: INTEGER; ref: REFANY) := Replay_PutRef;
    GetVar(ref: INTEGER): M3CG.Var := Replay_GetVar;
    GetProc(ref: INTEGER): M3CG.Proc := Replay_GetProc;
END;

PROCEDURE Replay(
    self: T;
    cg: M3CG.T;
    VAR index: INTEGER;
    data: REF ARRAY OF op_t := NIL;
    start := 0;
    end := LAST(INTEGER); (* one past end *)
    ) =
VAR context: Replay_t := NIL;
BEGIN
    IF self.replay # NIL THEN
        context := self.replay;
        context.reuse_refs := self.reuse_refs;
    ELSE
        context := NEW(Replay_t).Init(self.refs.Size());
    END;
    IF data = NIL THEN
        data := self.data;
    END;
    start := MAX(start, FIRST(data^));
    end := MIN(end, LAST(data^));
    FOR i := start TO end DO
        <* ASSERT data[i] # NIL *>
        index := i;
        data[i].replay(context, cg);
    END;
    IF self.reuse_refs THEN
        self.replay := context;
    END;
END Replay;

PROCEDURE Add(self: T; a: op_t) =
BEGIN
    INC(self.total_ops);
    INC(self.op_counts[a.op]);
    self.private_data.addhi(a);
END Add;

PROCEDURE end_unit(self: T) =
VAR opt: op_t;
    op: Op;
BEGIN
    self.Add(NEW(end_unit_t, op := Op.end_unit));
    
    (* convert private_data into a sealed array of op_t
       and also form per-op arrays to optimize some small passes
    *)

    WITH private_data = self.private_data,
         data = NEW(REF ARRAY OF op_t, private_data.size()),
         op_data = self.op_data,
         op_counts = self.op_counts
    DO
        FOR i := FIRST(Op) TO LAST(Op) DO
            op_data[i] := NEW(REF ARRAY OF op_t, op_counts[i]);
            op_counts[i] := 0;
        END;
        FOR i := FIRST(data^) TO LAST(data^) DO
            opt := private_data.get(i);
            data[i] := opt;
            op := opt.op;
            op_data[op][op_counts[op]] := opt;
            INC(op_counts[op]);
        END;
        self.data := data;
    END;
END end_unit;

PROCEDURE GrowableRefs_Init(self: GrowableRefs_t): GrowableRefs_t =
BEGIN
    self.data := NEW(RefSeq.T).init();
    RETURN self;
END GrowableRefs_Init;

PROCEDURE GrowableRefs_Size(self: GrowableRefs_t): INTEGER =
BEGIN
    RETURN self.data.size();
END GrowableRefs_Size;

PROCEDURE GrowableRefs_NewRef(self: GrowableRefs_t; ref: REFANY): INTEGER =
BEGIN
    self.data.addhi(ref);
    RETURN self.data.size() - 1;
END GrowableRefs_NewRef;

PROCEDURE GrowableRefs_NewVar(self: GrowableRefs_t): var_t =
VAR ref := NEW(var_t);
BEGIN
    ref.tag := self.NewRef(ref);
    RETURN ref;
END GrowableRefs_NewVar;

PROCEDURE GrowableRefs_NewProc(self: GrowableRefs_t): proc_t =
VAR ref := NEW(proc_t);
BEGIN
    ref.tag := self.NewRef(ref);
    RETURN ref;
END GrowableRefs_NewProc;

PROCEDURE next_label(self: T; label_count: INTEGER := 1): Label =
VAR label := self.next_label_id;
BEGIN
    self.next_label_id := label + label_count;
    RETURN label;
END next_label;

PROCEDURE import_global(self: T; name: Name; byte_size: ByteSize; alignment: Alignment; type: Type; typeid: TypeUID): Var =
VAR var := self.refs.NewVar();
BEGIN
self.Add(NEW(import_global_t, op := Op.import_global, name := name, byte_size := byte_size, alignment := alignment, type := type, typeid := typeid, tag := var.tag));
RETURN var;
END import_global;

PROCEDURE declare_segment(self: T; name: Name; typeid: TypeUID; is_const: BOOLEAN): Var =
VAR var := self.refs.NewVar();
BEGIN
self.Add(NEW(declare_segment_t, op := Op.declare_segment, name := name, typeid := typeid, is_const := is_const, tag := var.tag));
RETURN var;
END declare_segment;

PROCEDURE declare_global(self: T; name: Name; byte_size: ByteSize; alignment: Alignment; type: Type; typeid: TypeUID; exported, inited: BOOLEAN): Var =
VAR var := self.refs.NewVar();
BEGIN
self.Add(NEW(declare_global_t, op := Op.declare_global, name := name, byte_size := byte_size, alignment := alignment, type := type, typeid := typeid, exported := exported, inited := inited, tag := var.tag));
RETURN var;
END declare_global;

PROCEDURE declare_constant(self: T; name: Name; byte_size: ByteSize; alignment: Alignment; type: Type; typeid: TypeUID; exported, inited: BOOLEAN): Var =
VAR var := self.refs.NewVar();
BEGIN
self.Add(NEW(declare_constant_t, op := Op.declare_constant, name := name, byte_size := byte_size, alignment := alignment, type := type, typeid := typeid, exported := exported, inited := inited, tag := var.tag));
RETURN var;
END declare_constant;

PROCEDURE declare_local(self: T; name: Name; byte_size: ByteSize; alignment: Alignment; type: Type; typeid: TypeUID; in_memory, up_level: BOOLEAN; frequency: Frequency): Var =
VAR var := self.refs.NewVar();
BEGIN
self.Add(NEW(declare_local_t, op := Op.declare_local, name := name, byte_size := byte_size, alignment := alignment, type := type, typeid := typeid, in_memory := in_memory, up_level := up_level, frequency := frequency, tag := var.tag));
RETURN var;
END declare_local;

PROCEDURE declare_param(self: T; name: Name; byte_size: ByteSize; alignment: Alignment; type: Type; typeid: TypeUID; in_memory, up_level: BOOLEAN; frequency: Frequency; qid := M3CG.NoQID): Var =
VAR var := self.refs.NewVar();
BEGIN
self.Add(NEW(declare_param_t, op := Op.declare_param, name := name, byte_size := byte_size, alignment := alignment, type := type, typeid := typeid, in_memory := in_memory, up_level := up_level, frequency := frequency, tag := var.tag, qid := qid));
RETURN var;
END declare_param;

PROCEDURE declare_temp(self: T; byte_size: ByteSize; alignment: Alignment; type: Type; in_memory: BOOLEAN): Var =
VAR var := self.refs.NewVar();
BEGIN
self.Add(NEW(declare_temp_t, op := Op.declare_temp, byte_size := byte_size, alignment := alignment, type := type, in_memory := in_memory, tag := var.tag));
RETURN var;
END declare_temp;

PROCEDURE import_procedure(self: T; name: Name; n_params: INTEGER; return_type: Type; callingConvention: CallingConvention): Proc =
VAR proc := self.refs.NewProc();
BEGIN
self.Add(NEW(import_procedure_t, op := Op.import_procedure, name := name, n_params := n_params, return_type := return_type, callingConvention := callingConvention, tag := proc.tag));
RETURN proc;
END import_procedure;

PROCEDURE declare_procedure(self: T; name: Name; n_params: INTEGER; return_type: Type; level: INTEGER; callingConvention: CallingConvention; exported: BOOLEAN; parent: Proc): Proc =
VAR proc := self.refs.NewProc();
    parent_tag := 0;
BEGIN
    IF parent # NIL THEN
        parent_tag := NARROW(parent, proc_t).tag;
    END;
    self.Add(NEW(declare_procedure_t, op := Op.declare_procedure, name := name, n_params := n_params, return_type := return_type, level := level,
                 callingConvention := callingConvention, exported := exported, parent := parent_tag, tag := proc.tag));
    RETURN proc;
END declare_procedure;

PROCEDURE set_error_handler(self: T; proc: PROCEDURE(msg: TEXT)) =
BEGIN
self.Add(NEW(set_error_handler_t, op := Op.set_error_handler, proc := proc));
END set_error_handler;

PROCEDURE begin_procedure(self: T; proc: Proc) =
BEGIN
self.Add(NEW(begin_procedure_t, op := Op.begin_procedure, proc := NARROW(proc, proc_t).tag));
END begin_procedure;

PROCEDURE end_procedure(self: T; proc: Proc) =
BEGIN
self.Add(NEW(end_procedure_t, op := Op.end_procedure, proc := NARROW(proc, proc_t).tag));
END end_procedure;

PROCEDURE begin_unit(self: T; optimize: INTEGER) =
BEGIN
self.Add(NEW(begin_unit_t, op := Op.begin_unit, optimize := optimize));
END begin_unit;

PROCEDURE import_unit(self: T; name: Name) =
BEGIN
self.Add(NEW(import_unit_t, op := Op.import_unit, name := name));
END import_unit;

PROCEDURE export_unit(self: T; name: Name) = BEGIN
self.Add(NEW(export_unit_t, op := Op.export_unit, name := name));
END export_unit;

PROCEDURE set_source_file(self: T; file: TEXT) =
BEGIN
self.Add(NEW(set_source_file_t, op := Op.set_source_file, file := file));
END set_source_file;

PROCEDURE set_source_line(self: T; line: INTEGER) =
BEGIN
self.Add(NEW(set_source_line_t, op := Op.set_source_line, line := line));
END set_source_line;

PROCEDURE declare_typename(self: T; typeid: TypeUID; name: Name) =
BEGIN
self.Add(NEW(declare_typename_t, op := Op.declare_typename, typeid := typeid, name := name));
END declare_typename;

PROCEDURE declare_array(self: T; typeid, index_typeid, element_typeid: TypeUID; bit_size: BitSize) =
BEGIN
self.Add(NEW(declare_array_t, op := Op.declare_array, typeid := typeid, index_typeid := index_typeid, element_typeid := element_typeid, bit_size := bit_size));
END declare_array;

PROCEDURE declare_open_array(self: T; typeid, element_typeid: TypeUID; bit_size: BitSize) =
BEGIN
self.Add(NEW(declare_open_array_t, op := Op.declare_open_array, typeid := typeid, element_typeid := element_typeid, bit_size := bit_size));
END declare_open_array;

PROCEDURE declare_enum(self: T; typeid: TypeUID; n_elts: INTEGER; bit_size: BitSize) =
BEGIN
self.Add(NEW(declare_enum_t, op := Op.declare_enum, typeid := typeid, n_elts := n_elts, bit_size := bit_size));
END declare_enum;

PROCEDURE declare_enum_elt(self: T; name: Name) =
BEGIN
self.Add(NEW(declare_enum_elt_t, op := Op.declare_enum_elt, name := name));
END declare_enum_elt;

PROCEDURE declare_packed(self: T; typeid: TypeUID; bit_size: BitSize; base: TypeUID) =
BEGIN
self.Add(NEW(declare_packed_t, op := Op.declare_packed, typeid := typeid, bit_size := bit_size, base := base));
END declare_packed;

PROCEDURE declare_record(self: T; typeid: TypeUID; bit_size: BitSize; n_fields: INTEGER) =
BEGIN
self.Add(NEW(declare_record_t, op := Op.declare_record, typeid := typeid, bit_size := bit_size, n_fields := n_fields));
END declare_record;

PROCEDURE declare_field(self: T; name: Name; bit_offset: BitOffset; bit_size: BitSize; typeid: TypeUID) =
BEGIN
self.Add(NEW(declare_field_t, op := Op.declare_field, name := name, bit_offset := bit_offset, bit_size := bit_size, typeid := typeid));
END declare_field;

PROCEDURE declare_set(self: T; typeid, domain_typeid: TypeUID; bit_size: BitSize) =
BEGIN
self.Add(NEW(declare_set_t, op := Op.declare_set, typeid := typeid, domain_typeid := domain_typeid, bit_size := bit_size));
END declare_set;

PROCEDURE declare_subrange(self: T; typeid, domain_typeid: TypeUID; READONLY min, max: Target.Int; bit_size: BitSize) =
BEGIN
self.Add(NEW(declare_subrange_t, op := Op.declare_subrange, typeid := typeid, domain_typeid := domain_typeid, min := min, max := max, bit_size := bit_size));
END declare_subrange;

PROCEDURE declare_pointer(self: T; typeid, target_typeid: TypeUID; brand: TEXT; traced: BOOLEAN) =
BEGIN
self.Add(NEW(declare_pointer_t, op := Op.declare_pointer, typeid := typeid, target_typeid := target_typeid, brand := brand, traced := traced));
END declare_pointer;

PROCEDURE declare_indirect(self: T; typeid, target_typeid: TypeUID) =
BEGIN
self.Add(NEW(declare_indirect_t, op := Op.declare_indirect, typeid := typeid, target_typeid := target_typeid));
END declare_indirect;

PROCEDURE declare_proctype(self: T; typeid: TypeUID; n_formals: INTEGER; return_typeid: TypeUID; n_raises: INTEGER; callingConvention: CallingConvention) =
BEGIN
self.Add(NEW(declare_proctype_t, op := Op.declare_proctype, typeid := typeid, n_formals := n_formals, return_typeid := return_typeid, n_raises := n_raises, callingConvention := callingConvention));
END declare_proctype;

PROCEDURE declare_formal(self: T; name: Name; typeid: TypeUID) =
BEGIN
self.Add(NEW(declare_formal_t, op := Op.declare_formal, name := name, typeid := typeid));
END declare_formal;

PROCEDURE declare_raises(self: T; name: Name) =
BEGIN
self.Add(NEW(declare_raises_t, op := Op.declare_raises, name := name));
END declare_raises;

PROCEDURE declare_object(self: T; typeid, super_typeid: TypeUID; brand: TEXT; traced: BOOLEAN; n_fields, n_methods: INTEGER; fields_bit_size: BitSize) =
BEGIN
self.Add(NEW(declare_object_t, op := Op.declare_raises, typeid := typeid, super_typeid := super_typeid, brand := brand, traced := traced, n_fields := n_fields, n_methods := n_methods, fields_bit_size := fields_bit_size));
END declare_object;

PROCEDURE declare_method(self: T; name: Name; signature: TypeUID) =
BEGIN
self.Add(NEW(declare_method_t, op := Op.declare_method, name := name, signature := signature));
END declare_method;

PROCEDURE declare_opaque(self: T; typeid, super_typeid: TypeUID) =
BEGIN
self.Add(NEW(declare_opaque_t, op := Op.declare_opaque, typeid := typeid, super_typeid := super_typeid));
END declare_opaque;

PROCEDURE reveal_opaque(self: T; lhs_typeid, rhs_typeid: TypeUID) =
BEGIN
self.Add(NEW(reveal_opaque_t, op := Op.reveal_opaque, lhs_typeid := lhs_typeid, rhs_typeid := rhs_typeid));
END reveal_opaque;

PROCEDURE declare_exception(self: T; name: Name; arg_typeid: TypeUID; raise_proc: BOOLEAN; base: Var; offset: INTEGER) =
BEGIN
self.Add(NEW(declare_exception_t, op := Op.declare_exception, name := name, arg_typeid := arg_typeid, raise_proc := raise_proc, base := NARROW(base, var_t).tag, offset := offset));
END declare_exception;

PROCEDURE widechar_size(self: T; size: INTEGER) =
BEGIN
self.Add(NEW(widechar_size_t, op := Op.widechar_size, size := size));
END widechar_size;

PROCEDURE set_runtime_proc(self: T; name: Name; proc: Proc) =
BEGIN
self.Add(NEW(set_runtime_proc_t, op := Op.set_runtime_proc, name := name, proc := NARROW(proc, proc_t).tag));
END set_runtime_proc;

PROCEDURE bind_segment(self: T; segment: Var; byte_size: ByteSize; alignment: Alignment; type: Type; exported, inited: BOOLEAN) =
BEGIN
self.Add(NEW(bind_segment_t, op := Op.declare_segment, segment := NARROW(segment, var_t).tag, byte_size := byte_size, alignment := alignment, type := type, exported := exported, inited := inited));
END bind_segment;

PROCEDURE free_temp(self: T; var: Var) =
BEGIN
self.Add(NEW(free_temp_t, op := Op.free_temp, var := NARROW(var, var_t).tag));
END free_temp;

PROCEDURE begin_init(self: T; var: Var) =
BEGIN
self.Add(NEW(begin_init_t, op := Op.begin_init, var := NARROW(var, var_t).tag));
END begin_init;

PROCEDURE end_init(self: T; var: Var) =
BEGIN
self.Add(NEW(end_init_t, op := Op.end_init, var := NARROW(var, var_t).tag));
END end_init;

PROCEDURE init_int(self: T; byte_offset: ByteOffset; READONLY int: Target.Int; type: Type) =
BEGIN
self.Add(NEW(init_int_t, op := Op.init_int, byte_offset := byte_offset, int := int, type := type));
END init_int;

PROCEDURE init_proc(self: T; byte_offset: ByteOffset; proc: Proc) =
BEGIN
self.Add(NEW(init_proc_t, op := Op.init_proc, byte_offset := byte_offset, proc := NARROW(proc, proc_t).tag));
END init_proc;

PROCEDURE init_label(self: T; byte_offset: ByteOffset; label: Label) =
BEGIN
self.Add(NEW(init_label_t, op := Op.init_label, byte_offset := byte_offset, label := label));
END init_label;

PROCEDURE init_var(self: T; byte_offset: ByteOffset; var: Var; bias: ByteOffset) =
BEGIN
self.Add(NEW(init_var_t, op := Op.init_var, byte_offset := byte_offset, var := NARROW(var, var_t).tag, bias := bias));
END init_var;

PROCEDURE init_offset(self: T; byte_offset: ByteOffset; var: Var) =
BEGIN
self.Add(NEW(init_offset_t, op := Op.init_offset, byte_offset := byte_offset, var := NARROW(var, var_t).tag));
END init_offset;

PROCEDURE init_chars(self: T; byte_offset: ByteOffset; text: TEXT) =
BEGIN
self.Add(NEW(init_chars_t, op := Op.init_chars, byte_offset := byte_offset, text := text));
END init_chars;

PROCEDURE init_float(self: T; byte_offset: ByteOffset; READONLY float: Target.Float) =
BEGIN
self.Add(NEW(init_float_t, op := Op.init_float, byte_offset := byte_offset, float := float));
END init_float;

PROCEDURE begin_block(self: T) = BEGIN
self.Add(NEW(begin_block_t, op := Op.begin_block));
END begin_block;

PROCEDURE end_block(self: T) = BEGIN
self.Add(NEW(end_block_t, op := Op.end_block));
END end_block;

PROCEDURE note_procedure_origin(self: T; proc: Proc) = BEGIN
self.Add(NEW(note_procedure_origin_t, op := Op.note_procedure_origin, proc := NARROW(proc, proc_t).tag));
END note_procedure_origin;

PROCEDURE set_label(self: T; label: Label; barrier: BOOLEAN) = BEGIN
self.Add(NEW(set_label_t, op := Op.set_label, label := label, barrier := barrier));
END set_label;

PROCEDURE jump(self: T; label: Label) =
BEGIN
self.Add(NEW(jump_t, op := Op.jump, label := label));
END jump;

PROCEDURE if_true(self: T; type: IType; label: Label; frequency: Frequency) =
BEGIN
self.Add(NEW(if_true_t, op := Op.if_true, type := type, label := label, frequency := frequency));
END if_true;

PROCEDURE if_false(self: T; type: IType; label: Label; frequency: Frequency) =
BEGIN
self.Add(NEW(if_false_t, op := Op.if_false, type := type, label := label, frequency := frequency));
END if_false;

PROCEDURE if_compare(self: T; type: ZType; op: CompareOp; label: Label; frequency: Frequency) = BEGIN
self.Add(NEW(if_compare_t, op := Op.if_compare, type := type, compare_op := op, label := label, frequency := frequency));
END if_compare;

PROCEDURE case_jump(self: T; type: IType; READONLY labels: ARRAY OF Label) =
VAR a := NEW(REF ARRAY OF Label, NUMBER(labels));
BEGIN
a^ := labels;
self.Add(NEW(case_jump_t, op := Op.case_jump, type := type, labels := a));
END case_jump;

PROCEDURE exit_proc(self: T; type: Type) = BEGIN
self.Add(NEW(exit_proc_t, op := Op.exit_proc, type := type));
END exit_proc;

PROCEDURE load(self: T; var: Var; byte_offset: ByteOffset; mtype: MType; ztype: ZType) = BEGIN
self.Add(NEW(load_t, op := Op.load, var := NARROW(var, var_t).tag, byte_offset := byte_offset, mtype := mtype, ztype := ztype));
END load;

PROCEDURE store(self: T; var: Var; byte_offset: ByteOffset; ztype: ZType; mtype: MType) = BEGIN
self.Add(NEW(store_t, op := Op.store, var := NARROW(var, var_t).tag, byte_offset := byte_offset, mtype := mtype, ztype := ztype));
END store;

PROCEDURE load_address(self: T; var: Var; byte_offset: ByteOffset) = BEGIN
self.Add(NEW(load_address_t, op := Op.load_address, var := NARROW(var, var_t).tag, byte_offset := byte_offset));
END load_address;

PROCEDURE load_indirect(self: T; byte_offset: ByteOffset; mtype: MType; ztype: ZType) = BEGIN
self.Add(NEW(load_indirect_t, op := Op.load_indirect, byte_offset := byte_offset, mtype := mtype, ztype := ztype));
END load_indirect;

PROCEDURE store_indirect(self: T; byte_offset: ByteOffset; ztype: ZType; mtype: MType) = BEGIN
self.Add(NEW(store_indirect_t, op := Op.store_indirect, byte_offset := byte_offset, mtype := mtype, ztype := ztype));
END store_indirect;

PROCEDURE load_nil(self: T) = BEGIN
self.Add(NEW(load_nil_t, op := Op.load_nil));
END load_nil;

PROCEDURE load_integer(self: T; type: IType; READONLY int: Target.Int) = BEGIN
self.Add(NEW(load_integer_t, op := Op.load_integer, type := type, int := int));
END load_integer;

PROCEDURE load_float(self: T; type: RType; READONLY float: Target.Float) = BEGIN
self.Add(NEW(load_float_t, op := Op.load_float, type := type, float := float));
END load_float;

PROCEDURE compare(self: T; ztype: ZType; itype: IType; op: CompareOp) = BEGIN
self.Add(NEW(compare_t, op := Op.compare, ztype := ztype, itype := itype, compare_op := op));
END compare;

PROCEDURE add(self: T; type: AType) = BEGIN
self.Add(NEW(add_t, op := Op.add, type := type));
END add;

PROCEDURE subtract(self: T; type: AType) = BEGIN
self.Add(NEW(subtract_t, op := Op.subtract, type := type));
END subtract;

PROCEDURE multiply(self: T; type: AType) = BEGIN
self.Add(NEW(multiply_t, op := Op.multiply, type := type));
END multiply;

PROCEDURE divide(self: T; type: RType) = BEGIN
self.Add(NEW(divide_t, op := Op.divide, type := type));
END divide;

PROCEDURE div(self: T; type: IType; a, b: Sign) = BEGIN
self.Add(NEW(div_t, op := Op.div, type := type, a := a, b := b));
END div;

PROCEDURE mod(self: T; type: IType; a, b: Sign) =
BEGIN
self.Add(NEW(mod_t, op := Op.mod, type := type, a := a, b := b));
END mod;

PROCEDURE negate(self: T; type: AType) =
BEGIN
self.Add(NEW(negate_t, op := Op.negate, type := type));
END negate;

PROCEDURE abs(self: T; type: AType) =
BEGIN
self.Add(NEW(abs_t, op := Op.abs, type := type));
END abs;

PROCEDURE max(self: T; type: ZType) =
BEGIN
self.Add(NEW(max_t, op := Op.max, type := type));
END max;

PROCEDURE min(self: T; type: ZType) =
BEGIN
self.Add(NEW(min_t, op := Op.min, type := type));
END min;

PROCEDURE cvt_int(self: T; rtype: RType; itype: IType; op: ConvertOp) =
BEGIN
self.Add(NEW(cvt_int_t, op := Op.cvt_int, rtype := rtype, itype := itype, convert_op := op));
END cvt_int;

PROCEDURE cvt_float(self: T; atype: AType; rtype: RType) =
BEGIN
self.Add(NEW(cvt_float_t, op := Op.cvt_float, atype := atype, rtype := rtype));
END cvt_float;

PROCEDURE set_union(self: T; byte_size: ByteSize) =
BEGIN
self.Add(NEW(set_union_t, op := Op.set_union, byte_size := byte_size));
END set_union;

PROCEDURE set_difference(self: T; byte_size: ByteSize) =
BEGIN
self.Add(NEW(set_difference_t, op := Op.set_difference, byte_size := byte_size));
END set_difference;

PROCEDURE set_intersection(self: T; byte_size: ByteSize) =
BEGIN
self.Add(NEW(set_intersection_t, op := Op.set_intersection, byte_size := byte_size));
END set_intersection;

PROCEDURE set_sym_difference(self: T; byte_size: ByteSize) =
BEGIN
self.Add(NEW(set_sym_difference_t, op := Op.set_sym_difference, byte_size := byte_size));
END set_sym_difference;

PROCEDURE set_member(self: T; byte_size: ByteSize; type: IType) =
BEGIN
self.Add(NEW(set_member_t, op := Op.set_member, byte_size := byte_size, type := type));
END set_member;

PROCEDURE set_compare(self: T; byte_size: ByteSize; op: CompareOp; type: IType) =
BEGIN
self.Add(NEW(set_compare_t, op := Op.set_compare, byte_size := byte_size, compare_op := op, type := type));
END set_compare;

PROCEDURE set_range(self: T; byte_size: ByteSize; type: IType) =
BEGIN
self.Add(NEW(set_range_t, op := Op.set_range, byte_size := byte_size, type := type));
END set_range;

PROCEDURE set_singleton(self: T; byte_size: ByteSize; type: IType) =
BEGIN
self.Add(NEW(set_singleton_t, op := Op.set_singleton, byte_size := byte_size, type := type));
END set_singleton;

PROCEDURE not(self: T; type: IType) =
BEGIN
self.Add(NEW(not_t, op := Op.not, type := type));
END not;

PROCEDURE and(self: T; type: IType) =
BEGIN
self.Add(NEW(and_t, op := Op.and, type := type));
END and;

PROCEDURE or(self: T; type: IType) =
BEGIN
self.Add(NEW(or_t, op := Op.or, type := type));
END or;

PROCEDURE xor(self: T; type: IType) =
BEGIN
self.Add(NEW(xor_t, op := Op.xor, type := type));
END xor;

PROCEDURE shift(self: T; type: IType) =
BEGIN
self.Add(NEW(shift_t, op := Op.shift, type := type));
END shift;

PROCEDURE shift_left(self: T; type: IType) =
BEGIN
self.Add(NEW(shift_left_t, op := Op.shift_left, type := type));
END shift_left;

PROCEDURE shift_right(self: T; type: IType) =
BEGIN
self.Add(NEW(shift_right_t, op := Op.shift_right, type := type));
END shift_right;

PROCEDURE rotate(self: T; type: IType) =
BEGIN
self.Add(NEW(rotate_t, op := Op.rotate, type := type));
END rotate;

PROCEDURE rotate_left(self: T; type: IType) =
BEGIN
self.Add(NEW(rotate_left_t, op := Op.rotate_left, type := type));
END rotate_left;

PROCEDURE rotate_right(self: T; type: IType) =
BEGIN
self.Add(NEW(rotate_right_t, op := Op.rotate_right, type := type));
END rotate_right;

PROCEDURE widen(self: T; sign: BOOLEAN) =
BEGIN
self.Add(NEW(widen_t, op := Op.widen, sign := sign));
END widen;

PROCEDURE chop(self: T) =
BEGIN
self.Add(NEW(chop_t, op := Op.chop));
END chop;

PROCEDURE extract(self: T; type: IType; sign_extend: BOOLEAN) =
BEGIN
self.Add(NEW(extract_t, op := Op.extract, type := type, sign_extend := sign_extend));
END extract;

PROCEDURE extract_n(self: T; type: IType; sign_extend: BOOLEAN; count: CARDINAL) =
BEGIN
self.Add(NEW(extract_n_t, op := Op.extract_n, type := type, sign_extend := sign_extend, count := count));
END extract_n;

PROCEDURE extract_mn(self: T; type: IType; sign_extend: BOOLEAN; offset, count: CARDINAL) =
BEGIN
self.Add(NEW(extract_mn_t, op := Op.extract_mn, type := type, sign_extend := sign_extend, offset := offset, count := count));
END extract_mn;

PROCEDURE insert(self: T; type: IType) =
BEGIN
self.Add(NEW(insert_t, op := Op.insert, type := type));
END insert;

PROCEDURE insert_n(self: T; type: IType; count: CARDINAL) =
BEGIN
self.Add(NEW(insert_n_t, op := Op.insert_n, type := type, count := count));
END insert_n;

PROCEDURE insert_mn(self: T; type: IType; offset, count: CARDINAL) =
BEGIN
self.Add(NEW(insert_mn_t, op := Op.insert_mn, type := type, offset := offset, count := count));
END insert_mn;

PROCEDURE swap(self: T; a, b: Type) =
BEGIN
self.Add(NEW(swap_t, op := Op.swap, a := a, b := b));
END swap;

PROCEDURE pop(self: T; type: Type) =
BEGIN
self.Add(NEW(pop_t, op := Op.pop, type := type));
END pop;

PROCEDURE copy_n(self: T; itype: IType; mtype: MType; overlap: BOOLEAN) =
BEGIN
self.Add(NEW(copy_n_t, op := Op.copy_n, itype := itype, mtype := mtype, overlap := overlap));
END copy_n;

PROCEDURE copy(self: T; n: INTEGER; mtype: MType; overlap: BOOLEAN) =
BEGIN
self.Add(NEW(copy_t, op := Op.copy, n := n, mtype := mtype, overlap := overlap));
END copy;

PROCEDURE zero_n(self: T; itype: IType; mtype: MType) =
BEGIN
self.Add(NEW(zero_n_t, op := Op.zero_n, itype := itype, mtype := mtype));
END zero_n;

PROCEDURE zero(self: T; n: INTEGER; type: MType) =
BEGIN
self.Add(NEW(zero_t, op := Op.zero, n := n, type := type));
END zero;

PROCEDURE loophole(self: T; from, to: ZType) =
BEGIN
self.Add(NEW(loophole_t, op := Op.loophole, from := from, to := to));
END loophole;

PROCEDURE abort(self: T; code: RuntimeError) =
BEGIN
self.Add(NEW(abort_t, op := Op.abort, code := code));
END abort;

PROCEDURE check_nil(self: T; code: RuntimeError) =
BEGIN
self.Add(NEW(check_nil_t, op := Op.check_nil, code := code));
END check_nil;

PROCEDURE check_lo(self: T; type: IType; READONLY i: Target.Int; code: RuntimeError) =
BEGIN
self.Add(NEW(check_lo_t, op := Op.check_lo, type := type, i := i, code := code));
END check_lo;

PROCEDURE check_hi(self: T; type: IType; READONLY i: Target.Int; code: RuntimeError) =
BEGIN
self.Add(NEW(check_hi_t, op := Op.check_hi, type := type, i := i, code := code));
END check_hi;

PROCEDURE check_range(self: T; type: IType; READONLY a, b: Target.Int; code: RuntimeError) =
BEGIN
self.Add(NEW(check_range_t, op := Op.check_range, type := type, a := a, b := b, code := code));
END check_range;

PROCEDURE check_index(self: T; type: IType; code: RuntimeError) =
BEGIN
self.Add(NEW(check_index_t, op := Op.check_index, type := type, code := code));
END check_index;

PROCEDURE check_eq(self: T; type: IType; code: RuntimeError) =
BEGIN
self.Add(NEW(check_eq_t, op := Op.check_eq, type := type, code := code));
END check_eq;

PROCEDURE add_offset(self: T; i: INTEGER) =
BEGIN
self.Add(NEW(add_offset_t, op := Op.add_offset, i := i));
END add_offset;

PROCEDURE index_address(self: T; type: IType; size: INTEGER) =
BEGIN
self.Add(NEW(index_address_t, op := Op.index_address, type := type, size := size));
END index_address;

PROCEDURE start_call_direct(self: T; proc: Proc; level: INTEGER; type: Type) =
BEGIN
self.Add(NEW(start_call_direct_t, op := Op.start_call_direct, proc := NARROW(proc, proc_t).tag, level := level, type := type));
END start_call_direct;

PROCEDURE start_call_indirect(self: T; type: Type; callingConvention: CallingConvention) =
BEGIN
self.Add(NEW(start_call_indirect_t, op := Op.start_call_indirect, type := type, callingConvention := callingConvention));
END start_call_indirect;

PROCEDURE pop_param(self: T; type: MType) =
BEGIN
self.Add(NEW(pop_param_t, op := Op.pop_param, type := type));
END pop_param;

PROCEDURE pop_struct(self: T; typeid: TypeUID; byte_size: ByteSize; alignment: Alignment) =
BEGIN
self.Add(NEW(pop_struct_t, op := Op.pop_struct, typeid := typeid, byte_size := byte_size, alignment := alignment));
END pop_struct;

PROCEDURE pop_static_link(self: T) =
BEGIN
self.Add(NEW(pop_static_link_t, op := Op.pop_static_link));
END pop_static_link;

PROCEDURE call_direct(self: T; proc: Proc; type: Type) =
BEGIN
self.Add(NEW(call_direct_t, op := Op.call_direct, proc := NARROW(proc, proc_t).tag, type := type));
END call_direct;

PROCEDURE call_indirect(self: T; type: Type; callingConvention: CallingConvention) =
BEGIN
self.Add(NEW(call_indirect_t, op := Op.call_indirect, type := type, callingConvention := callingConvention));
END call_indirect;

PROCEDURE load_procedure(self: T; proc: Proc) =
BEGIN
self.Add(NEW(load_procedure_t, op := Op.load_procedure, proc := NARROW(proc, proc_t).tag));
END load_procedure;

PROCEDURE load_static_link(self: T; proc: Proc) =
BEGIN
self.Add(NEW(load_static_link_t, op := Op.load_static_link, proc := NARROW(proc, proc_t).tag));
END load_static_link;

PROCEDURE comment(self: T; a, b, c, d: TEXT := NIL) =
BEGIN
self.Add(NEW(comment_t, op := Op.comment, a := a, b := b, c := c, d := d));
END comment;

PROCEDURE store_ordered(self: T; ztype: ZType; mtype: MType; order: MemoryOrder) =
BEGIN
self.Add(NEW(store_ordered_t, op := Op.store_ordered, mtype := mtype, ztype := ztype, order := order));
END store_ordered;

PROCEDURE load_ordered(self: T; mtype: MType; ztype: ZType; order: MemoryOrder) =
BEGIN
self.Add(NEW(load_ordered_t, op := Op.load_ordered, mtype := mtype, ztype := ztype, order := order));
END load_ordered;

PROCEDURE exchange(self: T; mtype: MType; ztype: ZType; order: MemoryOrder) =
BEGIN
self.Add(NEW(exchange_t, op := Op.exchange, mtype := mtype, ztype := ztype, order := order));
END exchange;

PROCEDURE compare_exchange(self: T; mtype: MType; ztype: ZType; r: IType; success, failure: MemoryOrder) =
BEGIN
self.Add(NEW(compare_exchange_t, op := Op.compare_exchange, mtype := mtype, ztype := ztype, r := r, success := success, failure := failure));
END compare_exchange;

PROCEDURE fence(self: T; order: MemoryOrder) =
BEGIN
self.Add(NEW(fence_t, op := Op.fence, order := order));
END fence;

PROCEDURE fetch_and_op(self: T; op: AtomicOp; mtype: MType; ztype: ZType; order: MemoryOrder) =
BEGIN
self.Add(NEW(fetch_and_op_t, op := Op.fetch_and_op, atomic_op := op, mtype := mtype, ztype := ztype, order := order));
END fetch_and_op;

PROCEDURE Replay_Init(self: Replay_t; refcount: INTEGER): Replay_t =
BEGIN
    self.refs := NEW(StaticRefs_t).Init(refcount);
    RETURN self;
END Replay_Init;

PROCEDURE Replay_PutRef(self: Replay_t; tag: INTEGER; ref: REFANY) =
BEGIN
    IF self.reuse_refs = FALSE OR self.refs.data[tag] = NIL THEN
        self.refs.data[tag] := ref;
    END;
END Replay_PutRef;

PROCEDURE Replay_GetProc(self: Replay_t; ref: INTEGER): M3CG.Proc =
VAR proc := self.refs.data[ref];
BEGIN
    IF proc = NIL THEN
        RETURN NIL;
    END;
    RETURN NARROW(proc, M3CG.Proc);
END Replay_GetProc;

PROCEDURE Replay_GetVar(self: Replay_t; ref: INTEGER): M3CG.Var =
VAR var := self.refs.data[ref];
BEGIN
    IF var = NIL THEN
        RETURN NIL;
    END;
    RETURN NARROW(var, M3CG.Var);
END Replay_GetVar;

PROCEDURE replay_declare_segment(self: declare_segment_t; replay: Replay_t; cg: cg_t) =
BEGIN
    replay.PutRef(self.tag, cg.declare_segment(self.name, self.typeid, self.is_const));
END replay_declare_segment;

PROCEDURE replay_declare_global(self: declare_global_t; replay: Replay_t; cg: cg_t) =
BEGIN
    replay.PutRef(self.tag, cg.declare_global(self.name, self.byte_size, self.alignment, self.type, self.typeid, self.exported, self.inited));
END replay_declare_global;

PROCEDURE replay_declare_constant(self: declare_constant_t; replay: Replay_t; cg: cg_t) =
BEGIN
    replay.PutRef(self.tag, cg.declare_constant(self.name, self.byte_size, self.alignment, self.type, self.typeid, self.exported, self.inited));
END replay_declare_constant;

PROCEDURE replay_declare_local(self: declare_local_t; replay: Replay_t; cg: cg_t) =
BEGIN
    replay.PutRef(self.tag, cg.declare_local(self.name, self.byte_size, self.alignment, self.type, self.typeid, self.in_memory, self.up_level, self.frequency));
END replay_declare_local;

PROCEDURE replay_declare_param(self: declare_param_t; replay: Replay_t; cg: cg_t) =
BEGIN
    replay.PutRef(self.tag, cg.declare_param(self.name, self.byte_size, self.alignment, self.type, self.typeid, self.in_memory, self.up_level, self.frequency, self.qid));
END replay_declare_param;

PROCEDURE replay_declare_temp(self: declare_temp_t; replay: Replay_t; cg: cg_t) =
BEGIN
    replay.PutRef(self.tag, cg.declare_temp(self.byte_size, self.alignment, self.type, self.in_memory));
END replay_declare_temp;

PROCEDURE replay_import_procedure(self: import_procedure_t; replay: Replay_t; cg: cg_t) =
BEGIN
    replay.PutRef(self.tag, cg.import_procedure(self.name, self.n_params, self.return_type, self.callingConvention));
END replay_import_procedure;

PROCEDURE replay_declare_procedure(self: declare_procedure_t; replay: Replay_t; cg: cg_t) =
BEGIN
    replay.PutRef(self.tag, cg.declare_procedure(self.name, self.n_params, self.return_type,
        self.level, self.callingConvention, self.exported, replay.GetProc(self.parent)));
END replay_declare_procedure;

PROCEDURE replay_import_global(self: import_global_t; replay: Replay_t; cg: cg_t) =
BEGIN
    replay.PutRef(self.tag, cg.import_global(self.name, self.byte_size, self.alignment, self.type, self.typeid));
END replay_import_global;

PROCEDURE replay_begin_procedure(self: begin_procedure_t; replay: Replay_t; cg: cg_t) =
BEGIN
    cg.begin_procedure(replay.GetProc(self.proc));
END replay_begin_procedure;

PROCEDURE replay_end_procedure(self: end_procedure_t; replay: Replay_t; cg: cg_t) =
BEGIN
    cg.end_procedure(replay.GetProc(self.proc));
END replay_end_procedure;

PROCEDURE replay_set_error_handler(self: set_error_handler_t; <*UNUSED*>replay: Replay_t; cg: cg_t) =
BEGIN
    cg.set_error_handler(self.proc);
END replay_set_error_handler;

PROCEDURE replay_set_runtime_proc(self: set_runtime_proc_t; replay: Replay_t; cg: cg_t) =
BEGIN
    cg.set_runtime_proc(self.name, replay.GetProc(self.proc));
END replay_set_runtime_proc;

PROCEDURE replay_begin_unit(self: begin_unit_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.begin_unit(self.optimize); END replay_begin_unit;
PROCEDURE replay_end_unit(<*UNUSED*>self: end_unit_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.end_unit(); END replay_end_unit;
PROCEDURE replay_import_unit(self: import_unit_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.import_unit(self.name); END replay_import_unit;
PROCEDURE replay_export_unit(self: export_unit_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.export_unit(self.name); END replay_export_unit;
PROCEDURE replay_set_source_file(self: set_source_file_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.set_source_file(self.file); END replay_set_source_file;
PROCEDURE replay_set_source_line(self: set_source_line_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.set_source_line(self.line); END replay_set_source_line;
PROCEDURE replay_declare_typename(self: declare_typename_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.declare_typename(self.typeid, self.name); END replay_declare_typename;
PROCEDURE replay_declare_array(self: declare_array_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.declare_array(self.typeid, self.index_typeid, self.element_typeid, self.bit_size); END replay_declare_array;
PROCEDURE replay_declare_open_array(self: declare_open_array_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.declare_open_array(self.typeid, self.element_typeid, self.bit_size); END replay_declare_open_array;
PROCEDURE replay_declare_enum(self: declare_enum_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.declare_enum(self.typeid, self.n_elts, self.bit_size); END replay_declare_enum;
PROCEDURE replay_declare_enum_elt(self: declare_enum_elt_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.declare_enum_elt(self.name); END replay_declare_enum_elt;
PROCEDURE replay_declare_packed(self: declare_packed_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.declare_packed(self.typeid, self.bit_size, self.base); END replay_declare_packed;
PROCEDURE replay_declare_record(self: declare_record_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.declare_record(self.typeid, self.bit_size, self.n_fields); END replay_declare_record;
PROCEDURE replay_declare_field(self: declare_field_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.declare_field(self.name, self.bit_offset, self.bit_size, self.typeid); END replay_declare_field;
PROCEDURE replay_declare_set(self: declare_set_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.declare_set(self.typeid, self.domain_typeid, self.bit_size); END replay_declare_set;
PROCEDURE replay_declare_subrange(self: declare_subrange_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.declare_subrange(self.typeid, self.domain_typeid, self.min, self.max, self.bit_size); END replay_declare_subrange;
PROCEDURE replay_declare_pointer(self: declare_pointer_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.declare_pointer(self.typeid, self.target_typeid, self.brand, self.traced); END replay_declare_pointer;
PROCEDURE replay_declare_indirect(self: declare_indirect_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.declare_indirect(self.typeid, self.target_typeid); END replay_declare_indirect;
PROCEDURE replay_declare_proctype(self: declare_proctype_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.declare_proctype(self.typeid, self.n_formals, self. return_typeid, self.n_raises, self.callingConvention); END replay_declare_proctype;
PROCEDURE replay_declare_formal(self: declare_formal_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.declare_formal(self.name, self.typeid); END replay_declare_formal;
PROCEDURE replay_declare_raises(self: declare_raises_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.declare_raises(self.name); END replay_declare_raises;
PROCEDURE replay_declare_object(self: declare_object_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.declare_object(self.typeid, self.super_typeid, self.brand, self.traced, self.n_fields, self.n_methods, self.fields_bit_size); END replay_declare_object;
PROCEDURE replay_declare_method(self: declare_method_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.declare_method(self.name, self.signature); END replay_declare_method;
PROCEDURE replay_declare_opaque(self: declare_opaque_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.declare_opaque(self.typeid, self.super_typeid); END replay_declare_opaque;
PROCEDURE replay_reveal_opaque(self: reveal_opaque_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.reveal_opaque(self.lhs_typeid, self.rhs_typeid); END replay_reveal_opaque;

PROCEDURE replay_declare_exception(self: declare_exception_t; replay: Replay_t; cg: cg_t) =
BEGIN
    cg.declare_exception(self.name, self.arg_typeid, self.raise_proc,
                         replay.GetVar(self.base), self.offset);
END replay_declare_exception;

PROCEDURE replay_widechar_size(self: widechar_size_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = 
BEGIN cg.widechar_size(self.size); 
END replay_widechar_size;

PROCEDURE replay_bind_segment(self: bind_segment_t; replay: Replay_t; cg: cg_t) =
BEGIN
    cg.bind_segment(replay.GetVar(self.segment), self.byte_size, self.alignment, self.type, self.exported, self.inited);
END replay_bind_segment;

PROCEDURE replay_free_temp(self: free_temp_t; replay: Replay_t; cg: cg_t) =
BEGIN
    cg.free_temp(replay.GetVar(self.var));
END replay_free_temp;

PROCEDURE replay_begin_init(self: begin_init_t; replay: Replay_t; cg: cg_t) =
BEGIN
    cg.begin_init(replay.GetVar(self.var));
END replay_begin_init;

PROCEDURE replay_end_init(self: end_init_t; replay: Replay_t; cg: cg_t) =
BEGIN
    cg.end_init(replay.GetVar(self.var));
END replay_end_init;

PROCEDURE replay_init_proc(self: init_proc_t; replay: Replay_t; cg: cg_t) =
BEGIN
    cg.init_proc(self.byte_offset, replay.GetProc(self.proc));
END replay_init_proc;

PROCEDURE replay_note_procedure_origin(self: note_procedure_origin_t; replay: Replay_t; cg: cg_t) =
BEGIN
    cg.note_procedure_origin(replay.GetProc(self.proc));
END replay_note_procedure_origin;

PROCEDURE replay_start_call_direct(self: start_call_direct_t; replay: Replay_t; cg: cg_t) =
BEGIN
    cg.start_call_direct(replay.GetProc(self.proc), self.level, self.type);
END replay_start_call_direct;

PROCEDURE replay_call_direct(self: call_direct_t; replay: Replay_t; cg: cg_t) =
BEGIN
    cg.call_direct(replay.GetProc(self.proc), self.type);
END replay_call_direct;

PROCEDURE replay_load_procedure(self: load_procedure_t; replay: Replay_t; cg: cg_t) =
BEGIN
    cg.load_procedure(replay.GetProc(self.proc));
END replay_load_procedure;

PROCEDURE replay_load_static_link(self: load_static_link_t; replay: Replay_t; cg: cg_t) =
BEGIN
    cg.load_static_link(replay.GetProc(self.proc));
END replay_load_static_link;

PROCEDURE replay_init_var(self: init_var_t; replay: Replay_t; cg: cg_t) =
BEGIN
    cg.init_var(self.byte_offset, replay.GetVar(self.var), self.bias);
END replay_init_var;

PROCEDURE replay_init_offset(self: init_offset_t; replay: Replay_t; cg: cg_t) =
BEGIN cg.init_offset(self.byte_offset, replay.GetVar(self.var)); END replay_init_offset;

PROCEDURE replay_load(self: load_t; replay: Replay_t; cg: cg_t) =
BEGIN cg.load(replay.GetVar(self.var), self.byte_offset, self.mtype, self.ztype); END replay_load;

PROCEDURE replay_store(self: store_t; replay: Replay_t; cg: cg_t) =
BEGIN cg.store(replay.GetVar(self.var), self.byte_offset, self.ztype, self.mtype); END replay_store;

PROCEDURE replay_load_address(self: load_address_t; replay: Replay_t; cg: cg_t) =
BEGIN cg.load_address(replay.GetVar(self.var), self.byte_offset); END replay_load_address;

PROCEDURE replay_init_int(self: init_int_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.init_int(self.byte_offset, self.int, self.type); END replay_init_int;
PROCEDURE replay_init_label(self: init_label_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.init_label(self.byte_offset, self.label); END replay_init_label;
PROCEDURE replay_init_chars(self: init_chars_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.init_chars(self.byte_offset, self.text); END replay_init_chars;
PROCEDURE replay_init_float(self: init_float_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.init_float(self.byte_offset, self.float); END replay_init_float;
PROCEDURE replay_begin_block(<*UNUSED*>self: begin_block_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.begin_block(); END replay_begin_block;
PROCEDURE replay_end_block(<*UNUSED*>self: end_block_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.end_block(); END replay_end_block;
PROCEDURE replay_set_label(self: set_label_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.set_label(self.label, self.barrier); END replay_set_label;
PROCEDURE replay_jump(self: jump_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.jump(self.label); END replay_jump;
PROCEDURE replay_if_true(self: if_true_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.if_true(self.type, self.label, self.frequency); END replay_if_true;
PROCEDURE replay_if_false(self: if_false_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.if_false(self.type, self.label, self.frequency); END replay_if_false;
PROCEDURE replay_if_compare(self: if_compare_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.if_compare(self.type, self.compare_op, self.label, self.frequency); END replay_if_compare;
PROCEDURE replay_case_jump(self: case_jump_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.case_jump(self.type, self.labels^); END replay_case_jump;
PROCEDURE replay_exit_proc(self: exit_proc_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.exit_proc(self.type); END replay_exit_proc;
PROCEDURE replay_load_indirect(self: load_indirect_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.load_indirect(self.byte_offset, self.mtype, self.ztype); END replay_load_indirect;
PROCEDURE replay_store_indirect(self: store_indirect_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.store_indirect(self.byte_offset, self.ztype, self.mtype); END replay_store_indirect;
PROCEDURE replay_load_nil(<*UNUSED*>self: load_nil_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.load_nil(); END replay_load_nil;
PROCEDURE replay_load_integer(self: load_integer_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.load_integer(self.type, self.int); END replay_load_integer;
PROCEDURE replay_load_float(self: load_float_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.load_float(self.type, self.float); END replay_load_float;
PROCEDURE replay_compare(self: compare_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.compare(self.ztype, self.itype, self.compare_op); END replay_compare;
PROCEDURE replay_add(self: add_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.add(self.type); END replay_add;
PROCEDURE replay_subtract(self: subtract_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.subtract(self.type); END replay_subtract;
PROCEDURE replay_multiply(self: multiply_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.multiply(self.type); END replay_multiply;
PROCEDURE replay_divide(self: divide_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.divide(self.type); END replay_divide;
PROCEDURE replay_div(self: div_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.div(self.type, self.a, self.b); END replay_div;
PROCEDURE replay_mod(self: mod_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.mod(self.type, self.a, self.b); END replay_mod;
PROCEDURE replay_negate(self: negate_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.negate(self.type); END replay_negate;
PROCEDURE replay_abs(self: abs_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.abs(self.type); END replay_abs;
PROCEDURE replay_max(self: max_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.max(self.type); END replay_max;
PROCEDURE replay_min(self: min_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.min(self.type); END replay_min;
PROCEDURE replay_cvt_int(self: cvt_int_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.cvt_int(self.rtype, self.itype, self.convert_op); END replay_cvt_int;
PROCEDURE replay_cvt_float(self: cvt_float_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.cvt_float(self.atype, self.rtype); END replay_cvt_float;
PROCEDURE replay_set_union(self: set_union_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.set_union(self.byte_size); END replay_set_union;
PROCEDURE replay_set_difference(self: set_difference_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.set_difference(self.byte_size); END replay_set_difference;
PROCEDURE replay_set_intersection(self: set_intersection_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.set_intersection(self.byte_size); END replay_set_intersection;
PROCEDURE replay_set_sym_difference(self: set_sym_difference_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.set_sym_difference(self.byte_size); END replay_set_sym_difference;
PROCEDURE replay_set_member(self: set_member_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.set_member(self.byte_size, self.type); END replay_set_member;
PROCEDURE replay_set_compare(self: set_compare_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.set_compare(self.byte_size, self.compare_op, self.type); END replay_set_compare;
PROCEDURE replay_set_range(self: set_range_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.set_range(self.byte_size, self.type); END replay_set_range;
PROCEDURE replay_set_singleton(self: set_singleton_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.set_singleton(self.byte_size, self.type); END replay_set_singleton;
PROCEDURE replay_not(self: not_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.not(self.type); END replay_not;
PROCEDURE replay_and(self: and_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.and(self.type); END replay_and;
PROCEDURE replay_or(self: or_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.or(self.type); END replay_or;
PROCEDURE replay_xor(self: xor_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.xor(self.type); END replay_xor;
PROCEDURE replay_shift(self: shift_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.shift(self.type); END replay_shift;
PROCEDURE replay_shift_left(self: shift_left_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.shift_left(self.type); END replay_shift_left;
PROCEDURE replay_shift_right(self: shift_right_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.shift_right(self.type); END replay_shift_right;
PROCEDURE replay_rotate(self: rotate_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.rotate(self.type); END replay_rotate;
PROCEDURE replay_rotate_left(self: rotate_left_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.rotate_left(self.type); END replay_rotate_left;
PROCEDURE replay_rotate_right(self: rotate_right_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.rotate_right(self.type); END replay_rotate_right;
PROCEDURE replay_widen(self: widen_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.widen(self.sign); END replay_widen;
PROCEDURE replay_chop(<*UNUSED*>self: chop_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.chop(); END replay_chop;
PROCEDURE replay_extract(self: extract_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.extract(self.type, self.sign_extend); END replay_extract;
PROCEDURE replay_extract_n(self: extract_n_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.extract_n(self.type, self.sign_extend, self.count); END replay_extract_n;
PROCEDURE replay_extract_mn(self: extract_mn_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.extract_mn(self.type, self.sign_extend, self.offset, self.count); END replay_extract_mn;
PROCEDURE replay_insert(self: insert_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.insert(self.type); END replay_insert;
PROCEDURE replay_insert_n(self: insert_n_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.insert_n(self.type, self.count); END replay_insert_n;
PROCEDURE replay_insert_mn(self: insert_mn_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.insert_mn(self.type, self.offset, self.count); END replay_insert_mn;
PROCEDURE replay_swap(self: swap_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.swap(self.a, self.b); END replay_swap;
PROCEDURE replay_pop(self: pop_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.pop(self.type); END replay_pop;
PROCEDURE replay_copy_n(self: copy_n_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.copy_n(self.itype, self.mtype, self.overlap); END replay_copy_n;
PROCEDURE replay_copy(self: copy_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.copy(self.n, self.mtype, self.overlap); END replay_copy;
PROCEDURE replay_zero_n(self: zero_n_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.zero_n(self.itype, self.mtype); END replay_zero_n;
PROCEDURE replay_zero(self: zero_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.zero(self.n, self.type); END replay_zero;
PROCEDURE replay_loophole(self: loophole_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.loophole(self.from, self.to); END replay_loophole;
PROCEDURE replay_abort(self: abort_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.abort(self.code); END replay_abort;
PROCEDURE replay_check_nil(self: check_nil_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.check_nil(self.code); END replay_check_nil;
PROCEDURE replay_check_lo(self: check_lo_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.check_lo(self.type, self.i, self.code); END replay_check_lo;
PROCEDURE replay_check_hi(self: check_hi_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.check_hi(self.type, self.i, self.code); END replay_check_hi;
PROCEDURE replay_check_range(self: check_range_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.check_range(self.type, self.a, self.b, self.code); END replay_check_range;
PROCEDURE replay_check_index(self: check_index_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.check_index(self.type, self.code); END replay_check_index;
PROCEDURE replay_check_eq(self: check_eq_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.check_eq(self.type, self.code); END replay_check_eq;
PROCEDURE replay_add_offset(self: add_offset_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.add_offset(self.i); END replay_add_offset;
PROCEDURE replay_index_address(self: index_address_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.index_address(self.type, self.size) END replay_index_address;
PROCEDURE replay_start_call_indirect(self: start_call_indirect_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.start_call_indirect(self.type, self.callingConvention); END replay_start_call_indirect;
PROCEDURE replay_pop_param(self: pop_param_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.pop_param(self.type); END replay_pop_param;
PROCEDURE replay_pop_struct(self: pop_struct_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.pop_struct(self.typeid, self.byte_size, self.alignment); END replay_pop_struct;
PROCEDURE replay_pop_static_link(<*UNUSED*>self: pop_static_link_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.pop_static_link(); END replay_pop_static_link;
PROCEDURE replay_call_indirect(self: call_indirect_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.call_indirect(self.type, self.callingConvention); END replay_call_indirect;
PROCEDURE replay_comment(self: comment_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.comment(self.a, self.b, self.c, self.d); END replay_comment;
PROCEDURE replay_store_ordered(self: store_ordered_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.store_ordered(self.ztype, self.mtype, self.order); END replay_store_ordered;
PROCEDURE replay_load_ordered(self: load_ordered_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.load_ordered(self.mtype, self.ztype, self.order); END replay_load_ordered;
PROCEDURE replay_exchange(self: exchange_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.exchange(self.mtype, self.ztype, self.order); END replay_exchange;
PROCEDURE replay_compare_exchange(self: compare_exchange_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.compare_exchange(self.mtype, self.ztype, self.r, self.success, self.failure); END replay_compare_exchange;

PROCEDURE replay_fence(self: fence_t; <*UNUSED*>replay: Replay_t; cg: cg_t) = BEGIN cg.fence(self.order); END replay_fence;
PROCEDURE replay_fetch_and_op(self: fetch_and_op_t; <*UNUSED*>replay: Replay_t; cg: M3CG.T) = BEGIN cg.fetch_and_op(self.atomic_op, self.mtype, self.ztype, self.order); END replay_fetch_and_op;

BEGIN
END M3CG_MultiPass.
