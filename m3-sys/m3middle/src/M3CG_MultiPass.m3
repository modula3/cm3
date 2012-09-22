UNSAFE MODULE M3CG_MultiPass;

IMPORT M3CG, RefSeq, Target;
FROM M3CG IMPORT Type, MType, IType, RType, AType, ZType, Sign;
FROM M3CG IMPORT Name, Var, Proc, Alignment, TypeUID, Label;
FROM M3CG IMPORT Frequency, CallingConvention, CompareOp, ConvertOp, AtomicOp;
FROM M3CG IMPORT BitSize, ByteSize, BitOffset, ByteOffset, RuntimeError;
FROM M3CG IMPORT MemoryOrder;
IMPORT M3CG_Ops;
FROM M3CG_Binary IMPORT Op;

REVEAL
T = Public BRANDED "M3CG_MultiPass.T" OBJECT

data: RefSeq.T := NIL;
next_label_id := 1;
next_var := 1;
next_proc := 1;
next_scope := 1;

METHODS

Add(a: REFANY) := Add;

OVERRIDES

get_data := get_data;

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

PROCEDURE New(): T =
BEGIN
RETURN NEW(T);
END New;

PROCEDURE get_data(self: T): REF ARRAY OF REFANY =
VAR data := self.data;
    a := NEW(REF ARRAY OF REFANY, data.size());
BEGIN
    FOR b := FIRST(a^) TO LAST(a^) DO
        a[b] := data.get(b);
    END;
    RETURN a;
END get_data;

PROCEDURE Add(self: T; a: REFANY) =
BEGIN
self.data.addhi(a);
END Add;

PROCEDURE next_label(self: T; label_count: INTEGER := 1): Label =
VAR label := self.next_label_id;
BEGIN
self.Add(NEW(next_label_t, label_count := label_count));
RETURN label;
END next_label;

PROCEDURE NewVar(self: T): var_t =
VAR v := NEW(var_t, tag := self.next_var);
BEGIN
INC(self.next_var);
RETURN v;
END NewVar;

PROCEDURE import_global(self: T; name: Name; byte_size: ByteSize; alignment: Alignment; type: Type; typeid: TypeUID): Var =
BEGIN
self.Add(NEW(import_global_t, op := Op.import_global, name := name, byte_size := byte_size, alignment := alignment, type := type, typeid := typeid));
RETURN NewVar(self);
END import_global;

PROCEDURE declare_segment(self: T; name: Name; typeid: TypeUID; is_const: BOOLEAN): Var =
BEGIN
self.Add(NEW(declare_segment_t, op := Op.declare_segment, name := name, typeid := typeid, is_const := is_const));
RETURN NewVar(self);
END declare_segment;

PROCEDURE declare_global(self: T; name: Name; byte_size: ByteSize; alignment: Alignment; typeid: Type; type: TypeUID; exported, inited: BOOLEAN): Var =
BEGIN
self.Add(NEW(declare_global_t, op := Op.declare_global, name := name, byte_size := byte_size, alignment := alignment, type := type, typeid := typeid, exported := exported, inited := inited));
RETURN NewVar(self);
END declare_global;

PROCEDURE declare_constant(self: T; name: Name; byte_size: ByteSize; alignment: Alignment; type: Type; typeid: TypeUID; exported, inited: BOOLEAN): Var =
BEGIN
self.Add(NEW(declare_constant_t, op := Op.declare_constant, name := name, byte_size := byte_size, alignment := alignment, type := type, typeid := typeid, exported := exported, inited := inited));
RETURN NewVar(self);
END declare_constant;

PROCEDURE declare_local(self: T; name: Name; byte_size: ByteSize; alignment: Alignment; type: Type; typeid: TypeUID; in_memory, up_level: BOOLEAN; frequency: Frequency): Var =
BEGIN
self.Add(NEW(declare_local_t, op := Op.declare_local, name := name, byte_size := byte_size, alignment := alignment, type := type, typeid := typeid, in_memory := in_memory, up_level := up_level, frequency := frequency));
RETURN NewVar(self);
END declare_local;

PROCEDURE declare_param(self: T; name: Name; byte_size: ByteSize; alignment: Alignment; type: Type; typeid: TypeUID; in_memory, up_level: BOOLEAN; frequency: Frequency): Var =
BEGIN
self.Add(NEW(declare_param_t, op := Op.declare_param, name := name, byte_size := byte_size, alignment := alignment, type := type, typeid := typeid, in_memory := in_memory, up_level := up_level, frequency := frequency));
RETURN NewVar(self);
END declare_param;

PROCEDURE declare_temp(self: T; byte_size: ByteSize; alignment: Alignment; type: Type; in_memory: BOOLEAN): Var =
BEGIN
self.Add(NEW(declare_temp_t, op := Op.declare_temp, byte_size := byte_size, alignment := alignment, type := type, in_memory := in_memory));
RETURN NewVar(self);
END declare_temp;

PROCEDURE NewProc(self: T): proc_t =
VAR p := NEW(proc_t, tag := self.next_proc);
BEGIN
INC(self.next_proc);
RETURN p;
END NewProc;

PROCEDURE import_procedure(self: T; name: Name; n_params: INTEGER; return_type: Type; callingConvention: CallingConvention): Proc =
BEGIN
self.Add(NEW(import_procedure_t, name := name, n_params := n_params, return_type := return_type, callingConvention := callingConvention));
RETURN NewProc(self);
END import_procedure;

PROCEDURE declare_procedure(self: T; name: Name; n_params: INTEGER; return_type: Type; level: INTEGER; callingConvention: CallingConvention; exported: BOOLEAN; parent: Proc): Proc =
BEGIN
self.Add(NEW(declare_procedure_t, name := name, n_params := n_params, return_type := return_type, level := level, callingConvention := callingConvention, exported := exported, parent := NARROW(parent, proc_t)));
RETURN NewProc(self);
END declare_procedure;

PROCEDURE set_error_handler(self: T; proc: PROCEDURE(msg: TEXT)) =
BEGIN
self.Add(NEW(set_error_handler_t, proc := proc));
END set_error_handler;

PROCEDURE begin_procedure(self: T; proc: Proc) =
BEGIN
self.Add(NEW(begin_procedure_t, proc := NARROW(proc, proc_t)));
END begin_procedure;

PROCEDURE end_procedure(self: T; proc: Proc) =
BEGIN
self.Add(NEW(end_procedure_t, proc := NARROW(proc, proc_t)));
END end_procedure;

PROCEDURE begin_unit(self: T; optimize: INTEGER) =
BEGIN
self.Add(NEW(begin_unit_t, optimize := optimize));
END begin_unit;

PROCEDURE end_unit(self: T) =
BEGIN
self.Add(NEW(end_unit_t));
END end_unit;

PROCEDURE import_unit(self: T; name: Name) =
BEGIN
self.Add(NEW(import_unit_t, name := name));
END import_unit;

PROCEDURE export_unit(self: T; name: Name) = BEGIN
self.Add(NEW(export_unit_t, name := name));
END export_unit;

PROCEDURE set_source_file(self: T; file: TEXT) =
BEGIN
self.Add(NEW(set_source_file_t, file := file));
END set_source_file;

PROCEDURE set_source_line(self: T; line: INTEGER) =
BEGIN
self.Add(NEW(set_source_line_t, line := line));
END set_source_line;

PROCEDURE declare_typename(self: T; typeid: TypeUID; name: Name) =
BEGIN
self.Add(NEW(declare_typename_t, typeid := typeid, name := name));
END declare_typename;

PROCEDURE declare_array(self: T; typeid, index_typeid, element_typeid: TypeUID; bit_size: BitSize) =
BEGIN
self.Add(NEW(declare_array_t, typeid := typeid, index_typeid := index_typeid, element_typeid := element_typeid, bit_size := bit_size));
END declare_array;

PROCEDURE declare_open_array(self: T; typeid, element_typeid: TypeUID; bit_size: BitSize) =
BEGIN
self.Add(NEW(declare_open_array_t, typeid := typeid, element_typeid := element_typeid, bit_size := bit_size));
END declare_open_array;

PROCEDURE declare_enum(self: T; typeid: TypeUID; n_elts: INTEGER; bit_size: BitSize) =
BEGIN
self.Add(NEW(declare_enum_t, typeid := typeid, n_elts := n_elts, bit_size := bit_size));
END declare_enum;

PROCEDURE declare_enum_elt(self: T; name: Name) =
BEGIN
self.Add(NEW(declare_enum_elt_t, name := name));
END declare_enum_elt;

PROCEDURE declare_packed(self: T; typeid: TypeUID; bit_size: BitSize; base: TypeUID) =
BEGIN
self.Add(NEW(declare_packed_t, typeid := typeid, bit_size := bit_size, base := base));
END declare_packed;

PROCEDURE declare_record(self: T; typeid: TypeUID; bit_size: BitSize; n_fields: INTEGER) =
BEGIN
self.Add(NEW(declare_record_t, typeid := typeid, bit_size := bit_size, n_fields := n_fields));
END declare_record;

PROCEDURE declare_field(self: T; name: Name; bit_offset: BitOffset; bit_size: BitSize; typeid: TypeUID) =
BEGIN
self.Add(NEW(declare_field_t, name := name, bit_offset := bit_offset, bit_size := bit_size, typeid := typeid));
END declare_field;

PROCEDURE declare_set(self: T; typeid, domain_typeid: TypeUID; bit_size: BitSize) =
BEGIN
self.Add(NEW(declare_set_t, typeid := typeid, domain_typeid := domain_typeid, bit_size := bit_size));
END declare_set;

PROCEDURE declare_subrange(self: T; typeid, domain_typeid: TypeUID; READONLY min, max: Target.Int; bit_size: BitSize) =
BEGIN
self.Add(NEW(declare_subrange_t, typeid := typeid, domain_typeid := domain_typeid, min := min, max := max, bit_size := bit_size));
END declare_subrange;

PROCEDURE declare_pointer(self: T; typeid, target_typeid: TypeUID; brand: TEXT; traced: BOOLEAN) =
BEGIN
self.Add(NEW(declare_pointer_t, typeid := typeid, target_typeid := target_typeid, brand := brand, traced := traced));
END declare_pointer;

PROCEDURE declare_indirect(self: T; typeid, target_typeid: TypeUID) =
BEGIN
self.Add(NEW(declare_indirect_t, typeid := typeid, target_typeid := target_typeid));
END declare_indirect;

PROCEDURE declare_proctype(self: T; typeid: TypeUID; n_formals: INTEGER; return_typeid: TypeUID; n_raises: INTEGER; callingConvention: CallingConvention) =
BEGIN
self.Add(NEW(declare_proctype_t, typeid := typeid, n_formals := n_formals, return_typeid := return_typeid, n_raises := n_raises, callingConvention := callingConvention));
END declare_proctype;

PROCEDURE declare_formal(self: T; name: Name; typeid: TypeUID) =
BEGIN
self.Add(NEW(declare_formal_t, name := name, typeid := typeid));
END declare_formal;

PROCEDURE declare_raises(self: T; name: Name) =
BEGIN
self.Add(NEW(declare_raises_t, name := name));
END declare_raises;

PROCEDURE declare_object(self: T; typeid, super_typeid: TypeUID; brand: TEXT; traced: BOOLEAN; n_fields, n_methods: INTEGER; field_size: BitSize) =
BEGIN
self.Add(NEW(declare_object_t, typeid := typeid, super_typeid := super_typeid, brand := brand, traced := traced, n_fields := n_fields, n_methods := n_methods, field_size := field_size));
END declare_object;

PROCEDURE declare_method(self: T; name: Name; signature: TypeUID) =
BEGIN
self.Add(NEW(declare_method_t, name := name, signature := signature));
END declare_method;

PROCEDURE declare_opaque(self: T; typeid, super_typeid: TypeUID) =
BEGIN
self.Add(NEW(declare_opaque_t, typeid := typeid, super_typeid := super_typeid));
END declare_opaque;

PROCEDURE reveal_opaque(self: T; lhs_typeid, rhs_typeid: TypeUID) =
BEGIN
self.Add(NEW(reveal_opaque_t, lhs_typeid := lhs_typeid, rhs_typeid := rhs_typeid));
END reveal_opaque;

PROCEDURE declare_exception(self: T; name: Name; arg_typeid: TypeUID; raise_proc: BOOLEAN; base: Var; offset: INTEGER) =
BEGIN
self.Add(NEW(declare_exception_t, name := name, arg_typeid := arg_typeid, raise_proc := raise_proc, base := NARROW(base, var_t), offset := offset));
END declare_exception;

PROCEDURE set_runtime_proc(self: T; name: Name; proc: Proc) =
BEGIN
self.Add(NEW(set_runtime_proc_t, name := name, proc := proc));
END set_runtime_proc;

PROCEDURE bind_segment(self: T; segment: Var; byte_size: ByteSize; alignment: Alignment; type: Type; exported, inited: BOOLEAN) =
BEGIN
self.Add(NEW(bind_segment_t, segment := NARROW(segment, var_t), byte_size := byte_size, alignment := alignment, type := type, exported := exported, inited := inited));
END bind_segment;

PROCEDURE free_temp(self: T; var: Var) =
BEGIN
self.Add(NEW(free_temp_t, var := NARROW(var, var_t)));
END free_temp;

PROCEDURE begin_init(self: T; var: Var) =
BEGIN
self.Add(NEW(begin_init_t, var := NARROW(var, var_t)));
END begin_init;

PROCEDURE end_init(self: T; var: Var) =
BEGIN
self.Add(NEW(end_init_t, var := NARROW(var, var_t)));
END end_init;

PROCEDURE init_int(self: T; byte_offset: ByteOffset; READONLY int: Target.Int; type: Type) =
BEGIN
self.Add(NEW(init_int_t, byte_offset := byte_offset, int := int, type := type));
END init_int;

PROCEDURE init_proc(self: T; byte_offset: ByteOffset; proc: Proc) =
BEGIN
self.Add(NEW(init_proc_t, byte_offset := byte_offset, proc := NARROW(proc, proc_t)));
END init_proc;

PROCEDURE init_label(self: T; byte_offset: ByteOffset; label: Label) =
BEGIN
self.Add(NEW(init_label_t, byte_offset := byte_offset, label := label));
END init_label;

PROCEDURE init_var(self: T; byte_offset: ByteOffset; var: Var; bias: ByteOffset) =
BEGIN
self.Add(NEW(init_var_t, byte_offset := byte_offset, var := NARROW(var, var_t), bias := bias));
END init_var;

PROCEDURE init_offset(self: T; byte_offset: ByteOffset; var: Var) =
BEGIN
self.Add(NEW(init_offset_t, byte_offset := byte_offset, var := NARROW(var, var_t)));
END init_offset;

PROCEDURE init_chars(self: T; byte_offset: ByteOffset; text: TEXT) =
BEGIN
self.Add(NEW(init_chars_t, byte_offset := byte_offset, text := text));
END init_chars;

PROCEDURE init_float(self: T; byte_offset: ByteOffset; READONLY float: Target.Float) =
BEGIN
self.Add(NEW(init_float_t, byte_offset := byte_offset, float := float));
END init_float;

PROCEDURE begin_block(self: T) = BEGIN
self.Add(NEW(begin_block_t));
END begin_block;

PROCEDURE end_block(self: T) = BEGIN
self.Add(NEW(end_block_t));
END end_block;

PROCEDURE note_procedure_origin(self: T; proc: Proc) = BEGIN
self.Add(NEW(note_procedure_origin_t, proc := NARROW(proc, proc_t)));
END note_procedure_origin;

PROCEDURE set_label(self: T; label: Label; barrier: BOOLEAN) = BEGIN
self.Add(NEW(set_label_t, label := label, barrier := barrier));
END set_label;

PROCEDURE jump(self: T; label: Label) =
BEGIN
self.Add(NEW(jump_t, label := label));
END jump;

PROCEDURE if_true(self: T; type: IType; label: Label; frequency: Frequency) =
BEGIN
self.Add(NEW(if_true_t, type := type, label := label, frequency := frequency));
END if_true;

PROCEDURE if_false(self: T; type: IType; label: Label; frequency: Frequency) =
BEGIN
self.Add(NEW(if_false_t, type := type, label := label, frequency := frequency));
END if_false;

PROCEDURE if_compare(self: T; type: ZType; op: CompareOp; label: Label; frequency: Frequency) = BEGIN
self.Add(NEW(if_compare_t, type := type, compare_op := op, label := label, frequency := frequency));
END if_compare;

PROCEDURE case_jump(self: T; type: IType; READONLY labels: ARRAY OF Label) =
VAR a := NEW(REF ARRAY OF Label, NUMBER(labels));
BEGIN
a^ := labels;
self.Add(NEW(case_jump_t, type := type, labels := a));
END case_jump;

PROCEDURE exit_proc(self: T; type: Type) = BEGIN
self.Add(NEW(exit_proc_t, type := type));
END exit_proc;

PROCEDURE load(self: T; var: Var; byte_offset: ByteOffset; mtype: MType; ztype: ZType) = BEGIN
self.Add(NEW(load_t, var := NARROW(var, var_t), byte_offset := byte_offset, mtype := mtype, ztype := ztype));
END load;

PROCEDURE store(self: T; var: Var; byte_offset: ByteOffset; ztype: ZType; mtype: MType) = BEGIN
self.Add(NEW(store_t, var := NARROW(var, var_t), byte_offset := byte_offset, mtype := mtype, ztype := ztype));
END store;

PROCEDURE load_address(self: T; var: Var; byte_offset: ByteOffset) = BEGIN
self.Add(NEW(load_address_t, var := NARROW(var, var_t), byte_offset := byte_offset));
END load_address;

PROCEDURE load_indirect(self: T; byte_offset: ByteOffset; mtype: MType; ztype: ZType) = BEGIN
self.Add(NEW(load_indirect_t, byte_offset := byte_offset, mtype := mtype, ztype := ztype));
END load_indirect;

PROCEDURE store_indirect(self: T; byte_offset: ByteOffset; ztype: ZType; mtype: MType) = BEGIN
self.Add(NEW(store_indirect_t, byte_offset := byte_offset, mtype := mtype, ztype := ztype));
END store_indirect;

PROCEDURE load_nil(self: T) = BEGIN
self.Add(NEW(load_nil_t));
END load_nil;

PROCEDURE load_integer(self: T; type: IType; READONLY int: Target.Int) = BEGIN
self.Add(NEW(load_integer_t, type := type, int := int));
END load_integer;

PROCEDURE load_float(self: T; type: RType; READONLY float: Target.Float) = BEGIN
self.Add(NEW(load_float_t, type := type, float := float));
END load_float;

PROCEDURE compare(self: T; ztype: ZType; itype: IType; op: CompareOp) = BEGIN
self.Add(NEW(compare_t, ztype := ztype, itype := itype, compare_op := op));
END compare;

PROCEDURE add(self: T; type: AType) = BEGIN
self.Add(NEW(add_t, type := type));
END add;

PROCEDURE subtract(self: T; type: AType) = BEGIN
self.Add(NEW(subtract_t, type := type));
END subtract;

PROCEDURE multiply(self: T; type: AType) = BEGIN
self.Add(NEW(multiply_t, type := type));
END multiply;

PROCEDURE divide(self: T; type: RType) = BEGIN
self.Add(NEW(divide_t, type := type));
END divide;

PROCEDURE div(self: T; type: IType; a, b: Sign) = BEGIN
self.Add(NEW(div_t, type := type, a := a, b := b));
END div;

PROCEDURE mod(self: T; type: IType; a, b: Sign) =
BEGIN
self.Add(NEW(mod_t, type := type, a := a, b := b));
END mod;

PROCEDURE negate(self: T; type: AType) =
BEGIN
self.Add(NEW(negate_t, type := type));
END negate;

PROCEDURE abs(self: T; type: AType) = BEGIN self.Add(NEW(abs_t, type := type)); END abs;
PROCEDURE max(self: T; type: ZType) = BEGIN self.Add(NEW(max_t, type := type)); END max;
PROCEDURE min(self: T; type: ZType) = BEGIN self.Add(NEW(min_t, type := type)); END min;

PROCEDURE cvt_int(self: T; rtype: RType; itype: IType; op: ConvertOp) = BEGIN self.Add(NEW(cvt_int_t, rtype := rtype, itype := itype, convert_op := op)); END cvt_int;
PROCEDURE cvt_float(self: T; atype: AType; rtype: RType) = BEGIN self.Add(NEW(cvt_float_t, atype := atype, rtype := rtype)); END cvt_float;

PROCEDURE set_union(self: T; byte_size: ByteSize) = BEGIN self.Add(NEW(set_union_t, byte_size := byte_size)); END set_union;
PROCEDURE set_difference(self: T; byte_size: ByteSize) = BEGIN self.Add(NEW(set_difference_t, byte_size := byte_size)); END set_difference;
PROCEDURE set_intersection(self: T; byte_size: ByteSize) = BEGIN self.Add(NEW(set_intersection_t, byte_size := byte_size)); END set_intersection;
PROCEDURE set_sym_difference(self: T; byte_size: ByteSize) = BEGIN self.Add(NEW(set_sym_difference_t, byte_size := byte_size)); END set_sym_difference;
PROCEDURE set_member(self: T; byte_size: ByteSize; type: IType) = BEGIN self.Add(NEW(set_member_t, byte_size := byte_size, type := type)); END set_member;
PROCEDURE set_compare(self: T; byte_size: ByteSize; op: CompareOp; type: IType) = BEGIN self.Add(NEW(set_compare_t, byte_size := byte_size, compare_op := op, type := type)); END set_compare;
PROCEDURE set_range(self: T; byte_size: ByteSize; type: IType) = BEGIN self.Add(NEW(set_range_t, byte_size := byte_size, type := type)); END set_range;
PROCEDURE set_singleton(self: T; byte_size: ByteSize; type: IType) = BEGIN self.Add(NEW(set_singleton_t, byte_size := byte_size, type := type)); END set_singleton;

PROCEDURE not(self: T; type: IType) = BEGIN self.Add(NEW(not_t, type := type)); END not;

PROCEDURE and(self: T; type: IType) = BEGIN self.Add(NEW(and_t, type := type)); END and;
PROCEDURE or(self: T; type: IType) = BEGIN self.Add(NEW(or_t, type := type)); END or;
PROCEDURE xor(self: T; type: IType) = BEGIN self.Add(NEW(xor_t, type := type)); END xor;
PROCEDURE shift(self: T; type: IType) = BEGIN self.Add(NEW(shift_t, type := type)); END shift;
PROCEDURE shift_left(self: T; type: IType) = BEGIN self.Add(NEW(shift_left_t, type := type)); END shift_left;
PROCEDURE shift_right(self: T; type: IType) = BEGIN self.Add(NEW(shift_right_t, type := type)); END shift_right;
PROCEDURE rotate(self: T; type: IType) = BEGIN self.Add(NEW(rotate_t, type := type)); END rotate;
PROCEDURE rotate_left(self: T; type: IType) = BEGIN self.Add(NEW(rotate_left_t, type := type)); END rotate_left;
PROCEDURE rotate_right(self: T; type: IType) = BEGIN self.Add(NEW(rotate_right_t, type := type)); END rotate_right;
PROCEDURE widen(self: T; sign: BOOLEAN) = BEGIN self.Add(NEW(widen_t, sign := sign)); END widen;
PROCEDURE chop(self: T) = BEGIN self.Add(NEW(chop_t)); END chop;

PROCEDURE extract(self: T; type: IType; sign_extend: BOOLEAN) = BEGIN self.Add(NEW(extract_t, type := type, sign_extend := sign_extend)); END extract;
PROCEDURE extract_n(self: T; type: IType; sign_extend: BOOLEAN; count: CARDINAL) = BEGIN self.Add(NEW(extract_n_t, type := type, sign_extend := sign_extend, count := count)); END extract_n;
PROCEDURE extract_mn(self: T; type: IType; sign_extend: BOOLEAN; offset, count: CARDINAL) = BEGIN self.Add(NEW(extract_mn_t, type := type, sign_extend := sign_extend, offset := offset, count := count)); END extract_mn;
PROCEDURE insert(self: T; type: IType) = BEGIN self.Add(NEW(insert_t, type := type)); END insert;
PROCEDURE insert_n(self: T; type: IType; count: CARDINAL) = BEGIN self.Add(NEW(insert_n_t, type := type, count := count)); END insert_n;
PROCEDURE insert_mn(self: T; type: IType; offset, count: CARDINAL) = BEGIN self.Add(NEW(insert_mn_t, type := type, offset := offset, count := count)); END insert_mn;
PROCEDURE swap(self: T; a, b: Type) = BEGIN self.Add(NEW(swap_t, a := a, b := b)); END swap;
PROCEDURE pop(self: T; type: Type) = BEGIN self.Add(NEW(pop_t, type := type)); END pop;

PROCEDURE copy_n(self: T; itype: IType; mtype: MType; overlap: BOOLEAN) = BEGIN self.Add(NEW(copy_n_t, itype := itype, mtype := mtype, overlap := overlap)); END copy_n;
PROCEDURE copy(self: T; n: INTEGER; mtype: MType; overlap: BOOLEAN) = BEGIN self.Add(NEW(copy_t, n := n, mtype := mtype, overlap := overlap)); END copy;
PROCEDURE zero_n(self: T; itype: IType; mtype: MType) = BEGIN self.Add(NEW(zero_n_t, itype := itype, mtype := mtype)); END zero_n;
PROCEDURE zero(self: T; n: INTEGER; type: MType) = BEGIN self.Add(NEW(zero_t, n := n, type := type)); END zero;
PROCEDURE loophole(self: T; from, to: ZType) = BEGIN self.Add(NEW(loophole_t, from := from, to := to)); END loophole;
PROCEDURE abort(self: T; code: RuntimeError) = BEGIN self.Add(NEW(abort_t, code := code)); END abort;
PROCEDURE check_nil(self: T; code: RuntimeError) = BEGIN self.Add(NEW(check_nil_t, code := code)); END check_nil;
PROCEDURE check_lo(self: T; type: IType; READONLY i: Target.Int; code: RuntimeError) = BEGIN self.Add(NEW(check_lo_t, type := type, i := i, code := code)); END check_lo;
PROCEDURE check_hi(self: T; type: IType; READONLY i: Target.Int; code: RuntimeError) = BEGIN self.Add(NEW(check_hi_t, type := type, i := i, code := code)); END check_hi;
PROCEDURE check_range(self: T; type: IType; READONLY a, b: Target.Int; code: RuntimeError) = BEGIN self.Add(NEW(check_range_t, type := type, a := a, b := b, code := code)); END check_range;
PROCEDURE check_index(self: T; type: IType; code: RuntimeError) = BEGIN self.Add(NEW(check_index_t, type := type, code := code)); END check_index;
PROCEDURE check_eq(self: T; type: IType; code: RuntimeError) = BEGIN self.Add(NEW(check_eq_t, type := type, code := code)); END check_eq;
PROCEDURE add_offset(self: T; i: INTEGER) = BEGIN self.Add(NEW(add_offset_t, i := i)); END add_offset;

PROCEDURE index_address(self: T; type: IType; size: INTEGER) = BEGIN self.Add(NEW(index_address_t, type := type, size := size)); END index_address;
PROCEDURE start_call_direct(self: T; proc: Proc; level: INTEGER; type: Type) = BEGIN self.Add(NEW(start_call_direct_t, proc := NARROW(proc, proc_t), level := level, type := type)); END start_call_direct;
PROCEDURE start_call_indirect(self: T; type: Type; callingConvention: CallingConvention) = BEGIN self.Add(NEW(start_call_indirect_t, type := type, callingConvention := callingConvention)); END start_call_indirect;
PROCEDURE pop_param(self: T; type: MType) = BEGIN self.Add(NEW(pop_param_t, type := type)); END pop_param;
PROCEDURE pop_struct(self: T; typeid: TypeUID; byte_size: ByteSize; alignment: Alignment) =
    BEGIN self.Add(NEW(pop_struct_t, typeid := typeid, byte_size := byte_size, alignment := alignment)); END pop_struct;
PROCEDURE pop_static_link(self: T) = BEGIN self.Add(NEW(pop_static_link_t)); END pop_static_link;
PROCEDURE call_direct(self: T; proc: Proc; type: Type) = BEGIN self.Add(NEW(call_direct_t, proc := NARROW(proc, proc_t), type := type)); END call_direct;
PROCEDURE call_indirect(self: T; type: Type; callingConvention: CallingConvention) = BEGIN self.Add(NEW(call_indirect_t, type := type, callingConvention := callingConvention)); END call_indirect;

PROCEDURE load_procedure(self: T; proc: Proc) = BEGIN self.Add(NEW(load_procedure_t, proc := NARROW(proc, proc_t))); END load_procedure;
PROCEDURE load_static_link(self: T; proc: Proc) = BEGIN self.Add(NEW(load_static_link_t, proc := NARROW(proc, proc_t))); END load_static_link;
PROCEDURE comment(self: T; a, b, c, d: TEXT := NIL) = BEGIN self.Add(NEW(comment_t, a := a, b := b, c := c, d := d)); END comment;

PROCEDURE store_ordered(self: T; ztype: ZType; mtype: MType; order: MemoryOrder) = BEGIN self.Add(NEW(store_ordered_t, mtype := mtype, ztype := ztype, order := order)); END store_ordered;
PROCEDURE load_ordered(self: T; mtype: MType; ztype: ZType; order: MemoryOrder) = BEGIN self.Add(NEW(load_ordered_t, mtype := mtype, ztype := ztype, order := order)); END load_ordered;
PROCEDURE exchange(self: T; mtype: MType; ztype: ZType; order: MemoryOrder) = BEGIN self.Add(NEW(exchange_t, mtype := mtype, ztype := ztype, order := order)); END exchange;

PROCEDURE compare_exchange(self: T; mtype: MType; ztype: ZType; r: IType; success, failure: MemoryOrder) = BEGIN self.Add(NEW(compare_exchange_t, mtype := mtype, ztype := ztype, r := r, success := success, failure := failure)); END compare_exchange;

PROCEDURE fence(self: T; order: MemoryOrder) = BEGIN self.Add(NEW(fence_t, order := order)); END fence;

PROCEDURE fetch_and_op(self: T; op: AtomicOp; mtype: MType; ztype: ZType; order: MemoryOrder) = BEGIN self.Add(NEW(fetch_and_op_t, atomic_op := op, mtype := mtype, ztype := ztype, order := order)); END fetch_and_op;


<*NOWARN*>PROCEDURE call_import_procedure(self: import_procedure_t; cg: cg_t) = BEGIN EVAL cg.import_procedure(self.name, self.n_params, self.return_type, self.callingConvention); END call_import_procedure;
<*NOWARN*>PROCEDURE call_declare_procedure(self: declare_procedure_t; cg: cg_t) = BEGIN EVAL cg.declare_procedure(self.name, self.n_params, self.return_type, self.level, self.callingConvention, self.exported, self.parent) END call_declare_procedure;
<*NOWARN*>PROCEDURE call_next_label(self: next_label_t; cg: cg_t) = BEGIN EVAL cg.next_label(self.label_count); END call_next_label;
<*NOWARN*>PROCEDURE call_set_error_handler(self: set_error_handler_t; cg: cg_t) = BEGIN cg.set_error_handler(self.proc); END call_set_error_handler;
<*NOWARN*>PROCEDURE call_begin_procedure(self: begin_procedure_t; cg: cg_t) = BEGIN END call_begin_procedure;
<*NOWARN*>PROCEDURE call_end_procedure(self: end_procedure_t; cg: cg_t) = BEGIN END call_end_procedure;
<*NOWARN*>PROCEDURE call_import_global(self: import_global_t; cg: cg_t) = BEGIN END call_import_global;
<*NOWARN*>PROCEDURE call_declare_segment(self: declare_segment_t; cg: cg_t) = BEGIN END call_declare_segment;
<*NOWARN*>PROCEDURE call_declare_global(self: declare_global_t; cg: cg_t) = BEGIN END call_declare_global;
<*NOWARN*>PROCEDURE call_declare_constant(self: declare_constant_t; cg: cg_t) = BEGIN END call_declare_constant;
<*NOWARN*>PROCEDURE call_declare_local(self: declare_local_t; cg: cg_t) = BEGIN END call_declare_local;
<*NOWARN*>PROCEDURE call_declare_param(self: declare_param_t; cg: cg_t) = BEGIN END call_declare_param;
<*NOWARN*>PROCEDURE call_declare_temp(self: declare_temp_t; cg: cg_t) = BEGIN END call_declare_temp;
<*NOWARN*>PROCEDURE call_begin_unit(self: begin_unit_t; cg: cg_t) = BEGIN END call_begin_unit;
<*NOWARN*>PROCEDURE call_end_unit(self: end_unit_t; cg: cg_t) = BEGIN END call_end_unit;
<*NOWARN*>PROCEDURE call_import_unit(self: import_unit_t; cg: cg_t) = BEGIN END call_import_unit;
<*NOWARN*>PROCEDURE call_export_unit(self: export_unit_t; cg: cg_t) = BEGIN END call_export_unit;
<*NOWARN*>PROCEDURE call_set_source_file(self: set_source_file_t; cg: cg_t) = BEGIN END call_set_source_file;
<*NOWARN*>PROCEDURE call_set_source_line(self: set_source_line_t; cg: cg_t) = BEGIN END call_set_source_line;
<*NOWARN*>PROCEDURE call_declare_typename(self: declare_typename_t; cg: cg_t) = BEGIN END call_declare_typename;
<*NOWARN*>PROCEDURE call_declare_array(self: declare_array_t; cg: cg_t) = BEGIN END call_declare_array;
<*NOWARN*>PROCEDURE call_declare_open_array(self: declare_open_array_t; cg: cg_t) = BEGIN END call_declare_open_array;
<*NOWARN*>PROCEDURE call_declare_enum(self: declare_enum_t; cg: cg_t) = BEGIN END call_declare_enum;
<*NOWARN*>PROCEDURE call_declare_enum_elt(self: declare_enum_elt_t; cg: cg_t) = BEGIN END call_declare_enum_elt;
<*NOWARN*>PROCEDURE call_declare_packed(self: declare_packed_t; cg: cg_t) = BEGIN END call_declare_packed;
<*NOWARN*>PROCEDURE call_declare_record(self: declare_record_t; cg: cg_t) = BEGIN END call_declare_record;
<*NOWARN*>PROCEDURE call_declare_field(self: declare_field_t; cg: cg_t) = BEGIN END call_declare_field;
<*NOWARN*>PROCEDURE call_declare_set(self: declare_set_t; cg: cg_t) = BEGIN END call_declare_set;
<*NOWARN*>PROCEDURE call_declare_subrange(self: declare_subrange_t; cg: cg_t) = BEGIN END call_declare_subrange;
<*NOWARN*>PROCEDURE call_declare_pointer(self: declare_pointer_t; cg: cg_t) = BEGIN END call_declare_pointer;
<*NOWARN*>PROCEDURE call_declare_indirect(self: declare_indirect_t; cg: cg_t) = BEGIN END call_declare_indirect;
<*NOWARN*>PROCEDURE call_declare_proctype(self: declare_proctype_t; cg: cg_t) = BEGIN END call_declare_proctype;
<*NOWARN*>PROCEDURE call_declare_formal(self: declare_formal_t; cg: cg_t) = BEGIN END call_declare_formal;
<*NOWARN*>PROCEDURE call_declare_raises(self: declare_raises_t; cg: cg_t) = BEGIN END call_declare_raises;
<*NOWARN*>PROCEDURE call_declare_object(self: declare_object_t; cg: cg_t) = BEGIN END call_declare_object;
<*NOWARN*>PROCEDURE call_declare_method(self: declare_method_t; cg: cg_t) = BEGIN END call_declare_method;
<*NOWARN*>PROCEDURE call_declare_opaque(self: declare_opaque_t; cg: cg_t) = BEGIN END call_declare_opaque;
<*NOWARN*>PROCEDURE call_reveal_opaque(self: reveal_opaque_t; cg: cg_t) = BEGIN END call_reveal_opaque;
<*NOWARN*>PROCEDURE call_declare_exception(self: declare_exception_t; cg: cg_t) = BEGIN END call_declare_exception;
<*NOWARN*>PROCEDURE call_set_runtime_proc(self: set_runtime_proc_t; cg: cg_t) = BEGIN END call_set_runtime_proc;
<*NOWARN*>PROCEDURE call_bind_segment(self: bind_segment_t; cg: cg_t) = BEGIN END call_bind_segment;
<*NOWARN*>PROCEDURE call_free_temp(self: free_temp_t; cg: cg_t) = BEGIN END call_free_temp;
<*NOWARN*>PROCEDURE call_begin_init(self: begin_init_t; cg: cg_t) = BEGIN END call_begin_init;
<*NOWARN*>PROCEDURE call_end_init(self: end_init_t; cg: cg_t) = BEGIN END call_end_init;
<*NOWARN*>PROCEDURE call_init_int(self: init_int_t; cg: cg_t) = BEGIN END call_init_int;
<*NOWARN*>PROCEDURE call_init_proc(self: init_proc_t; cg: cg_t) = BEGIN END call_init_proc;
<*NOWARN*>PROCEDURE call_init_label(self: init_label_t; cg: cg_t) = BEGIN END call_init_label;
<*NOWARN*>PROCEDURE call_init_var(self: init_var_t; cg: cg_t) = BEGIN END call_init_var;
<*NOWARN*>PROCEDURE call_init_offset(self: init_offset_t; cg: cg_t) = BEGIN END call_init_offset;
<*NOWARN*>PROCEDURE call_init_chars(self: init_chars_t; cg: cg_t) = BEGIN END call_init_chars;
<*NOWARN*>PROCEDURE call_init_float(self: init_float_t; cg: cg_t) = BEGIN END call_init_float;
<*NOWARN*>PROCEDURE call_begin_block(self: begin_block_t; cg: cg_t) = BEGIN END call_begin_block;
<*NOWARN*>PROCEDURE call_end_block(self: end_block_t; cg: cg_t) = BEGIN END call_end_block;
<*NOWARN*>PROCEDURE call_note_procedure_origin(self: note_procedure_origin_t; cg: cg_t) = BEGIN END call_note_procedure_origin;
<*NOWARN*>PROCEDURE call_set_label(self: set_label_t; cg: cg_t) = BEGIN END call_set_label;
<*NOWARN*>PROCEDURE call_jump(self: jump_t; cg: cg_t) = BEGIN END call_jump;
<*NOWARN*>PROCEDURE call_if_true(self: if_true_t; cg: cg_t) = BEGIN END call_if_true;
<*NOWARN*>PROCEDURE call_if_false(self: if_false_t; cg: cg_t) = BEGIN END call_if_false;
<*NOWARN*>PROCEDURE call_if_compare(self: if_compare_t; cg: cg_t) = BEGIN END call_if_compare;
<*NOWARN*>PROCEDURE call_case_jump(self: case_jump_t; cg: cg_t) = BEGIN END call_case_jump;
<*NOWARN*>PROCEDURE call_exit_proc(self: exit_proc_t; cg: cg_t) = BEGIN END call_exit_proc;
<*NOWARN*>PROCEDURE call_load(self: load_t; cg: cg_t) = BEGIN END call_load;
<*NOWARN*>PROCEDURE call_store(self: store_t; cg: cg_t) = BEGIN END call_store;
<*NOWARN*>PROCEDURE call_load_address(self: load_address_t; cg: cg_t) = BEGIN END call_load_address;
<*NOWARN*>PROCEDURE call_load_indirect(self: load_indirect_t; cg: cg_t) = BEGIN END call_load_indirect;
<*NOWARN*>PROCEDURE call_store_indirect(self: store_indirect_t; cg: cg_t) = BEGIN END call_store_indirect;
<*NOWARN*>PROCEDURE call_load_nil(self: load_nil_t; cg: cg_t) = BEGIN END call_load_nil;
<*NOWARN*>PROCEDURE call_load_integer(self: load_integer_t; cg: cg_t) = BEGIN END call_load_integer;
<*NOWARN*>PROCEDURE call_load_float(self: load_float_t; cg: cg_t) = BEGIN END call_load_float;
<*NOWARN*>PROCEDURE call_compare(self: compare_t; cg: cg_t) = BEGIN END call_compare;
<*NOWARN*>PROCEDURE call_add(self: add_t; cg: cg_t) = BEGIN END call_add;
<*NOWARN*>PROCEDURE call_subtract(self: subtract_t; cg: cg_t) = BEGIN END call_subtract;
<*NOWARN*>PROCEDURE call_multiply(self: multiply_t; cg: cg_t) = BEGIN END call_multiply;
<*NOWARN*>PROCEDURE call_divide(self: divide_t; cg: cg_t) = BEGIN END call_divide;
<*NOWARN*>PROCEDURE call_div(self: div_t; cg: cg_t) = BEGIN END call_div;
<*NOWARN*>PROCEDURE call_mod(self: mod_t; cg: cg_t) = BEGIN END call_mod;
<*NOWARN*>PROCEDURE call_negate(self: negate_t; cg: cg_t) = BEGIN END call_negate;
<*NOWARN*>PROCEDURE call_abs(self: abs_t; cg: cg_t) = BEGIN END call_abs;
<*NOWARN*>PROCEDURE call_max(self: max_t; cg: cg_t) = BEGIN END call_max;
<*NOWARN*>PROCEDURE call_min(self: min_t; cg: cg_t) = BEGIN END call_min;
<*NOWARN*>PROCEDURE call_cvt_int(self: cvt_int_t; cg: cg_t) = BEGIN END call_cvt_int;
<*NOWARN*>PROCEDURE call_cvt_float(self: cvt_float_t; cg: cg_t) = BEGIN END call_cvt_float;
<*NOWARN*>PROCEDURE call_set_union(self: set_union_t; cg: cg_t) = BEGIN END call_set_union;
<*NOWARN*>PROCEDURE call_set_difference(self: set_difference_t; cg: cg_t) = BEGIN END call_set_difference;
<*NOWARN*>PROCEDURE call_set_intersection(self: set_intersection_t; cg: cg_t) = BEGIN END call_set_intersection;
<*NOWARN*>PROCEDURE call_set_sym_difference(self: set_sym_difference_t; cg: cg_t) = BEGIN END call_set_sym_difference;
<*NOWARN*>PROCEDURE call_set_member(self: set_member_t; cg: cg_t) = BEGIN END call_set_member;
<*NOWARN*>PROCEDURE call_set_compare(self: set_compare_t; cg: cg_t) = BEGIN END call_set_compare;
<*NOWARN*>PROCEDURE call_set_range(self: set_range_t; cg: cg_t) = BEGIN END call_set_range;
<*NOWARN*>PROCEDURE call_set_singleton(self: set_singleton_t; cg: cg_t) = BEGIN END call_set_singleton;
<*NOWARN*>PROCEDURE call_not(self: not_t; cg: cg_t) = BEGIN END call_not;
<*NOWARN*>PROCEDURE call_and(self: and_t; cg: cg_t) = BEGIN END call_and;
<*NOWARN*>PROCEDURE call_or(self: or_t; cg: cg_t) = BEGIN END call_or;
<*NOWARN*>PROCEDURE call_xor(self: xor_t; cg: cg_t) = BEGIN END call_xor;
<*NOWARN*>PROCEDURE call_shift(self: shift_t; cg: cg_t) = BEGIN END call_shift;
<*NOWARN*>PROCEDURE call_shift_left(self: shift_left_t; cg: cg_t) = BEGIN END call_shift_left;
<*NOWARN*>PROCEDURE call_shift_right(self: shift_right_t; cg: cg_t) = BEGIN END call_shift_right;
<*NOWARN*>PROCEDURE call_rotate(self: rotate_t; cg: cg_t) = BEGIN END call_rotate;
<*NOWARN*>PROCEDURE call_rotate_left(self: rotate_left_t; cg: cg_t) = BEGIN END call_rotate_left;
<*NOWARN*>PROCEDURE call_rotate_right(self: rotate_right_t; cg: cg_t) = BEGIN END call_rotate_right;
<*NOWARN*>PROCEDURE call_widen(self: widen_t; cg: cg_t) = BEGIN END call_widen;
<*NOWARN*>PROCEDURE call_chop(self: chop_t; cg: cg_t) = BEGIN END call_chop;
<*NOWARN*>PROCEDURE call_extract(self: extract_t; cg: cg_t) = BEGIN END call_extract;
<*NOWARN*>PROCEDURE call_extract_n(self: extract_n_t; cg: cg_t) = BEGIN END call_extract_n;
<*NOWARN*>PROCEDURE call_extract_mn(self: extract_mn_t; cg: cg_t) = BEGIN END call_extract_mn;
<*NOWARN*>PROCEDURE call_insert(self: insert_t; cg: cg_t) = BEGIN END call_insert;
<*NOWARN*>PROCEDURE call_insert_n(self: insert_n_t; cg: cg_t) = BEGIN END call_insert_n;
<*NOWARN*>PROCEDURE call_insert_mn(self: insert_mn_t; cg: cg_t) = BEGIN END call_insert_mn;
<*NOWARN*>PROCEDURE call_swap(self: swap_t; cg: cg_t) = BEGIN END call_swap;
<*NOWARN*>PROCEDURE call_pop(self: pop_t; cg: cg_t) = BEGIN END call_pop;
<*NOWARN*>PROCEDURE call_copy_n(self: copy_n_t; cg: cg_t) = BEGIN END call_copy_n;
<*NOWARN*>PROCEDURE call_copy(self: copy_t; cg: cg_t) = BEGIN END call_copy;
<*NOWARN*>PROCEDURE call_zero_n(self: zero_n_t; cg: cg_t) = BEGIN END call_zero_n;
<*NOWARN*>PROCEDURE call_zero(self: zero_t; cg: cg_t) = BEGIN END call_zero;
<*NOWARN*>PROCEDURE call_loophole(self: loophole_t; cg: cg_t) = BEGIN END call_loophole;
<*NOWARN*>PROCEDURE call_abort(self: abort_t; cg: cg_t) = BEGIN END call_abort;
<*NOWARN*>PROCEDURE call_check_nil(self: check_nil_t; cg: cg_t) = BEGIN END call_check_nil;
<*NOWARN*>PROCEDURE call_check_lo(self: check_lo_t; cg: cg_t) = BEGIN END call_check_lo;
<*NOWARN*>PROCEDURE call_check_hi(self: check_hi_t; cg: cg_t) = BEGIN END call_check_hi;
<*NOWARN*>PROCEDURE call_check_range(self: check_range_t; cg: cg_t) = BEGIN END call_check_range;
<*NOWARN*>PROCEDURE call_check_index(self: check_index_t; cg: cg_t) = BEGIN END call_check_index;
<*NOWARN*>PROCEDURE call_check_eq(self: check_eq_t; cg: cg_t) = BEGIN END call_check_eq;
<*NOWARN*>PROCEDURE call_add_offset(self: add_offset_t; cg: cg_t) = BEGIN END call_add_offset;
<*NOWARN*>PROCEDURE call_index_address(self: index_address_t; cg: cg_t) = BEGIN END call_index_address;
<*NOWARN*>PROCEDURE call_start_call_direct(self: start_call_direct_t; cg: cg_t) = BEGIN END call_start_call_direct;
<*NOWARN*>PROCEDURE call_start_call_indirect(self: start_call_indirect_t; cg: cg_t) = BEGIN END call_start_call_indirect;
<*NOWARN*>PROCEDURE call_pop_param(self: pop_param_t; cg: cg_t) = BEGIN END call_pop_param;
<*NOWARN*>PROCEDURE call_pop_struct(self: pop_struct_t; cg: cg_t) = BEGIN END call_pop_struct;
<*NOWARN*>PROCEDURE call_pop_static_link(self: pop_static_link_t; cg: cg_t) = BEGIN END call_pop_static_link;
<*NOWARN*>PROCEDURE call_call_direct(self: call_direct_t; cg: cg_t) = BEGIN END call_call_direct;
<*NOWARN*>PROCEDURE call_call_indirect(self: call_indirect_t; cg: cg_t) = BEGIN END call_call_indirect;
<*NOWARN*>PROCEDURE call_load_procedure(self: load_procedure_t; cg: cg_t) = BEGIN END call_load_procedure;
<*NOWARN*>PROCEDURE call_load_static_link(self: load_static_link_t; cg: cg_t) = BEGIN END call_load_static_link;
<*NOWARN*>PROCEDURE call_comment(self: comment_t; cg: cg_t) = BEGIN END call_comment;
<*NOWARN*>PROCEDURE call_store_ordered(self: store_ordered_t; cg: cg_t) = BEGIN END call_store_ordered;
<*NOWARN*>PROCEDURE call_load_ordered(self: load_ordered_t; cg: cg_t) = BEGIN END call_load_ordered;
<*NOWARN*>PROCEDURE call_exchange(self: exchange_t; cg: cg_t) = BEGIN END call_exchange;
<*NOWARN*>PROCEDURE call_compare_exchange(self: compare_exchange_t; cg: cg_t) = BEGIN END call_compare_exchange;
PROCEDURE call_fence(self: fence_t; cg: cg_t) = BEGIN cg.fence(self.order); END call_fence;
PROCEDURE call_fetch_and_op(self: fetch_and_op_t; cg: M3CG_Ops.Public) = BEGIN cg.fetch_and_op(self.atomic_op, self.mtype, self.ztype, self.order); END call_fetch_and_op;


BEGIN
END M3CG_MultiPass.
