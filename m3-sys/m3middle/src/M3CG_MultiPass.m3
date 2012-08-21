UNSAFE MODULE M3CG_MultiPass;

IMPORT M3CG, RefSeq, Target;
FROM M3CG IMPORT Type, MType, IType, RType, AType, ZType, Sign;
FROM M3CG IMPORT Name, Var, Proc, Alignment, TypeUID, Label;
FROM M3CG IMPORT Frequency, CallingConvention, CompareOp, ConvertOp, AtomicOp;
FROM M3CG IMPORT BitSize, ByteSize, BitOffset, ByteOffset, RuntimeError;
FROM M3CG IMPORT MemoryOrder;

TYPE TVar = Var OBJECT tag: INTEGER END;
TYPE TProc = Proc OBJECT tag: INTEGER END;

REVEAL
T = Public BRANDED "M3CG_MultiPass.T" OBJECT

data: RefSeq.T := NIL;
next_label_id := 1;
next_var := 1;
next_proc := 1;
next_scope := 1;

METHODS

Add (a: REFANY) := Add;

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

PROCEDURE New (): T =
BEGIN
  RETURN NIL;
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

PROCEDURE Add (self: T; a: REFANY) =
BEGIN
  self.data.addhi(a);
END Add;

PROCEDURE next_label (self: T;  n: INTEGER := 1): Label =
  VAR label := self.next_label_id;
  BEGIN
    self.Add(NEW(next_label_t, n := n));
    RETURN label;
  END next_label;

PROCEDURE set_error_handler (self: T;  p: PROCEDURE (msg: TEXT)) =
  BEGIN
    self.Add(NEW(set_error_handler_t, p := p));
  END set_error_handler;

<*NOWARN*>PROCEDURE begin_unit(self: T; optimize: INTEGER) = BEGIN END begin_unit;
<*NOWARN*>PROCEDURE end_unit(self: T) = BEGIN END end_unit;
<*NOWARN*>PROCEDURE import_unit(self: T; n: Name) = BEGIN END import_unit;
<*NOWARN*>PROCEDURE export_unit(self: T; n: Name) = BEGIN END export_unit;
<*NOWARN*>PROCEDURE set_source_file(self: T; file: TEXT) = BEGIN END set_source_file;
<*NOWARN*>PROCEDURE set_source_line(self: T; line: INTEGER) = BEGIN END set_source_line;
<*NOWARN*>PROCEDURE declare_typename(self: T; t: TypeUID; n: Name) = BEGIN END declare_typename;
<*NOWARN*>PROCEDURE declare_array(self: T; t, index, elt: TypeUID; s: BitSize) = BEGIN END declare_array;
<*NOWARN*>PROCEDURE declare_open_array(self: T; t, elt: TypeUID; s: BitSize) = BEGIN END declare_open_array;
<*NOWARN*>PROCEDURE declare_enum(self: T; t: TypeUID; n_elts: INTEGER; s: BitSize) = BEGIN END declare_enum;
<*NOWARN*>PROCEDURE declare_enum_elt(self: T; n: Name) = BEGIN END declare_enum_elt;
<*NOWARN*>PROCEDURE declare_packed(self: T; t: TypeUID; s: BitSize; base: TypeUID) = BEGIN END declare_packed;
<*NOWARN*>PROCEDURE declare_record(self: T; t: TypeUID; s: BitSize; n_fields: INTEGER) = BEGIN END declare_record;
<*NOWARN*>PROCEDURE declare_field(self: T; n: Name; o: BitOffset; s: BitSize; t: TypeUID) = BEGIN END declare_field;
<*NOWARN*>PROCEDURE declare_set(self: T; t, domain: TypeUID; s: BitSize) = BEGIN END declare_set;
<*NOWARN*>PROCEDURE declare_subrange(self: T; t, domain: TypeUID; READONLY min, max: Target.Int; s: BitSize) = BEGIN END declare_subrange;
<*NOWARN*>PROCEDURE declare_pointer(self: T; t, target: TypeUID; brand: TEXT; traced: BOOLEAN) = BEGIN END declare_pointer;
<*NOWARN*>PROCEDURE declare_indirect(self: T; t, target: TypeUID) = BEGIN END declare_indirect;
<*NOWARN*>PROCEDURE declare_proctype(self: T; t: TypeUID; n_formals: INTEGER; result: TypeUID; n_raises: INTEGER; callingConvention: CallingConvention) = BEGIN END declare_proctype;
<*NOWARN*>PROCEDURE declare_formal(self: T; n: Name; t: TypeUID) = BEGIN END declare_formal;
<*NOWARN*>PROCEDURE declare_raises(self: T; n: Name) = BEGIN END declare_raises;
<*NOWARN*>PROCEDURE declare_object(self: T; t, super: TypeUID; brand: TEXT; traced: BOOLEAN; n_fields, n_methods: INTEGER; field_size: BitSize) = BEGIN END declare_object;
<*NOWARN*>PROCEDURE declare_method(self: T; n: Name; signature: TypeUID) = BEGIN END declare_method;
<*NOWARN*>PROCEDURE declare_opaque(self: T; t, super: TypeUID) = BEGIN END declare_opaque;
<*NOWARN*>PROCEDURE reveal_opaque(self: T; lhs, rhs: TypeUID) = BEGIN END reveal_opaque;
<*NOWARN*>PROCEDURE declare_exception(self: T; n: Name; arg_type: TypeUID; raise_proc: BOOLEAN; base: Var; offset: INTEGER) = BEGIN END declare_exception;
<*NOWARN*>PROCEDURE set_runtime_proc(self: T; n: Name; p: Proc) = BEGIN END set_runtime_proc;

PROCEDURE NewVar(self: T): Var =
VAR v := NEW(TVar, tag := self.next_var);
BEGIN
INC(self.next_var);
RETURN v;
END NewVar;

<*NOWARN*>PROCEDURE import_global(self: T; n: Name; s: ByteSize; a: Alignment; t: Type; type: TypeUID): Var =
BEGIN
RETURN NewVar(self);
END import_global;

<*NOWARN*>PROCEDURE declare_segment(self: T; n: Name; type: TypeUID; is_const: BOOLEAN): Var =
BEGIN
RETURN NewVar(self);
END declare_segment;

<*NOWARN*>PROCEDURE bind_segment(self: T; seg: Var; s: ByteSize; a: Alignment; t: Type; exported, inited: BOOLEAN) = BEGIN END bind_segment;

<*NOWARN*>PROCEDURE declare_global(self: T; n: Name; s: ByteSize; a: Alignment; t: Type; type: TypeUID; exported, inited: BOOLEAN): Var =
BEGIN
RETURN NewVar(self);
END declare_global;

<*NOWARN*>PROCEDURE declare_constant(self: T; n: Name; s: ByteSize; a: Alignment; t: Type; type: TypeUID; exported, inited: BOOLEAN): Var =
BEGIN
RETURN NewVar(self);
END declare_constant;

<*NOWARN*>PROCEDURE declare_local(self: T; n: Name; s: ByteSize; a: Alignment; t: Type; type: TypeUID; in_memory, up_level: BOOLEAN; f: Frequency): Var =
BEGIN
RETURN NewVar(self);
END declare_local;

<*NOWARN*>PROCEDURE declare_param(self: T; n: Name; s: ByteSize; a: Alignment; t: Type; type: TypeUID; in_memory, up_level: BOOLEAN; f: Frequency): Var =
BEGIN
RETURN NewVar(self);
END declare_param;

<*NOWARN*>PROCEDURE declare_temp(self: T; s: ByteSize; a: Alignment; t: Type; in_memory: BOOLEAN): Var =
BEGIN
RETURN NewVar(self);
END declare_temp;

<*NOWARN*>PROCEDURE free_temp(self: T; v: Var) = BEGIN END free_temp;
<*NOWARN*>PROCEDURE begin_init(self: T; v: Var) = BEGIN END begin_init;
<*NOWARN*>PROCEDURE end_init(self: T; v: Var) = BEGIN END end_init;
<*NOWARN*>PROCEDURE init_int(self: T; o: ByteOffset; READONLY value: Target.Int; t: Type) = BEGIN END init_int;
<*NOWARN*>PROCEDURE init_proc(self: T; o: ByteOffset; value: Proc) = BEGIN END init_proc;
<*NOWARN*>PROCEDURE init_label(self: T; o: ByteOffset; value: Label) = BEGIN END init_label;
<*NOWARN*>PROCEDURE init_var(self: T; o: ByteOffset; value: Var; bias: ByteOffset) = BEGIN END init_var;
<*NOWARN*>PROCEDURE init_offset(self: T; o: ByteOffset; value: Var) = BEGIN END init_offset;
<*NOWARN*>PROCEDURE init_chars(self: T; o: ByteOffset; value: TEXT) = BEGIN END init_chars;
<*NOWARN*>PROCEDURE init_float(self: T; o: ByteOffset; READONLY f: Target.Float) = BEGIN END init_float;

PROCEDURE NewProc(self: T): Proc =
VAR p := NEW(TProc, tag := self.next_proc);
BEGIN
INC(self.next_proc);
RETURN p;
END NewProc;

<*NOWARN*>PROCEDURE import_procedure(self: T; n: Name; n_params: INTEGER; ret_type: Type; callingConvention: CallingConvention): Proc =
BEGIN
RETURN NewProc(self);
END import_procedure;

<*NOWARN*>PROCEDURE declare_procedure(self: T; n: Name; n_params: INTEGER; return_type: Type; lev: INTEGER; callingConvention: CallingConvention; exported: BOOLEAN; parent: Proc): Proc =
BEGIN
RETURN NewProc(self);
END declare_procedure;

<*NOWARN*>PROCEDURE begin_procedure(self: T; p: Proc) = BEGIN END begin_procedure;
<*NOWARN*>PROCEDURE end_procedure(self: T; p: Proc) = BEGIN END end_procedure;
<*NOWARN*>PROCEDURE begin_block(self: T) = BEGIN END begin_block;
<*NOWARN*>PROCEDURE end_block(self: T) = BEGIN END end_block;
<*NOWARN*>PROCEDURE note_procedure_origin(self: T; p: Proc) = BEGIN END note_procedure_origin;
<*NOWARN*>PROCEDURE set_label(self: T; l: Label; barrier: BOOLEAN) = BEGIN END set_label;
<*NOWARN*>PROCEDURE jump(self: T; l: Label) = BEGIN END jump;
<*NOWARN*>PROCEDURE if_true(self: T; t: IType; l: Label; f: Frequency) = BEGIN END if_true;
<*NOWARN*>PROCEDURE if_false(self: T; t: IType; l: Label; f: Frequency) = BEGIN END if_false;
<*NOWARN*>PROCEDURE if_compare(self: T; t: ZType; op: CompareOp; l: Label; f: Frequency) = BEGIN END if_compare;
<*NOWARN*>PROCEDURE case_jump(self: T; t: IType; READONLY labels: ARRAY OF Label) = BEGIN END case_jump;
<*NOWARN*>PROCEDURE exit_proc(self: T; t: Type) = BEGIN END exit_proc;
<*NOWARN*>PROCEDURE load(self: T; v: Var; o: ByteOffset; t: MType; z: ZType) = BEGIN END load;
<*NOWARN*>PROCEDURE store(self: T; v: Var; o: ByteOffset; t: ZType; z: MType) = BEGIN END store;
<*NOWARN*>PROCEDURE load_address(self: T; v: Var; o: ByteOffset) = BEGIN END load_address;
<*NOWARN*>PROCEDURE load_indirect(self: T; o: ByteOffset; t: MType; z: ZType) = BEGIN END load_indirect;
<*NOWARN*>PROCEDURE store_indirect(self: T; o: ByteOffset; t: ZType; z: MType) = BEGIN END store_indirect;
<*NOWARN*>PROCEDURE load_nil(self: T) = BEGIN END load_nil;
<*NOWARN*>PROCEDURE load_integer(self: T; t: IType; READONLY i: Target.Int) = BEGIN END load_integer;
<*NOWARN*>PROCEDURE load_float(self: T; t: RType; READONLY f: Target.Float) = BEGIN END load_float;
<*NOWARN*>PROCEDURE compare(self: T; t: ZType; z: IType; op: CompareOp) = BEGIN END compare;
<*NOWARN*>PROCEDURE add(self: T; t: AType) = BEGIN END add;
<*NOWARN*>PROCEDURE subtract(self: T; t: AType) = BEGIN END subtract;
<*NOWARN*>PROCEDURE multiply(self: T; t: AType) = BEGIN END multiply;
<*NOWARN*>PROCEDURE divide(self: T; t: RType) = BEGIN END divide;
<*NOWARN*>PROCEDURE div(self: T; t: IType; a, b: Sign) = BEGIN END div;
<*NOWARN*>PROCEDURE mod(self: T; t: IType; a, b: Sign) = BEGIN END mod;
<*NOWARN*>PROCEDURE negate(self: T; t: AType) = BEGIN END negate;
<*NOWARN*>PROCEDURE abs(self: T; t: AType) = BEGIN END abs;
<*NOWARN*>PROCEDURE max(self: T; t: ZType) = BEGIN END max;
<*NOWARN*>PROCEDURE min(self: T; t: ZType) = BEGIN END min;
<*NOWARN*>PROCEDURE cvt_int(self: T; t: RType; x: IType; op: ConvertOp) = BEGIN END cvt_int;
<*NOWARN*>PROCEDURE cvt_float(self: T; t: AType; x: RType) = BEGIN END cvt_float;
<*NOWARN*>PROCEDURE set_union(self: T; s: ByteSize) = BEGIN END set_union;
<*NOWARN*>PROCEDURE set_difference(self: T; s: ByteSize) = BEGIN END set_difference;
<*NOWARN*>PROCEDURE set_intersection(self: T; s: ByteSize) = BEGIN END set_intersection;
<*NOWARN*>PROCEDURE set_sym_difference(self: T; s: ByteSize) = BEGIN END set_sym_difference;
<*NOWARN*>PROCEDURE set_member(self: T; s: ByteSize; t: IType) = BEGIN END set_member;
<*NOWARN*>PROCEDURE set_compare(self: T; s: ByteSize; op: CompareOp; t: IType) = BEGIN END set_compare;
<*NOWARN*>PROCEDURE set_range(self: T; s: ByteSize; t: IType) = BEGIN END set_range;
<*NOWARN*>PROCEDURE set_singleton(self: T; s: ByteSize; t: IType) = BEGIN END set_singleton;
<*NOWARN*>PROCEDURE not(self: T; t: IType) = BEGIN END not;
<*NOWARN*>PROCEDURE and(self: T; t: IType) = BEGIN END and;
<*NOWARN*>PROCEDURE or(self: T; t: IType) = BEGIN END or;
<*NOWARN*>PROCEDURE xor(self: T; t: IType) = BEGIN END xor;
<*NOWARN*>PROCEDURE shift(self: T; t: IType) = BEGIN END shift;
<*NOWARN*>PROCEDURE shift_left(self: T; t: IType) = BEGIN END shift_left;
<*NOWARN*>PROCEDURE shift_right(self: T; t: IType) = BEGIN END shift_right;
<*NOWARN*>PROCEDURE rotate(self: T; t: IType) = BEGIN END rotate;
<*NOWARN*>PROCEDURE rotate_left(self: T; t: IType) = BEGIN END rotate_left;
<*NOWARN*>PROCEDURE rotate_right(self: T; t: IType) = BEGIN END rotate_right;
<*NOWARN*>PROCEDURE widen(self: T; sign: BOOLEAN) = BEGIN END widen;
<*NOWARN*>PROCEDURE chop(self: T) = BEGIN END chop;
<*NOWARN*>PROCEDURE extract(self: T; t: IType; sign: BOOLEAN) = BEGIN END extract;
<*NOWARN*>PROCEDURE extract_n(self: T; t: IType; sign: BOOLEAN; n: CARDINAL) = BEGIN END extract_n;
<*NOWARN*>PROCEDURE extract_mn(self: T; t: IType; sign: BOOLEAN; m, n: CARDINAL) = BEGIN END extract_mn;
<*NOWARN*>PROCEDURE insert(self: T; t: IType) = BEGIN END insert;
<*NOWARN*>PROCEDURE insert_n(self: T; t: IType; n: CARDINAL) = BEGIN END insert_n;
<*NOWARN*>PROCEDURE insert_mn(self: T; t: IType; m, n: CARDINAL) = BEGIN END insert_mn;
<*NOWARN*>PROCEDURE swap(self: T; a, b: Type) = BEGIN END swap;
<*NOWARN*>PROCEDURE pop(self: T; t: Type) = BEGIN END pop;
<*NOWARN*>PROCEDURE copy_n(self: T; z: IType; t: MType; overlap: BOOLEAN) = BEGIN END copy_n;
<*NOWARN*>PROCEDURE copy(self: T; n: INTEGER; t: MType; overlap: BOOLEAN) = BEGIN END copy;
<*NOWARN*>PROCEDURE zero_n(self: T; z: IType; t: MType) = BEGIN END zero_n;
<*NOWARN*>PROCEDURE zero(self: T; n: INTEGER; t: MType) = BEGIN END zero;
<*NOWARN*>PROCEDURE loophole(self: T; from, two: ZType) = BEGIN END loophole;
<*NOWARN*>PROCEDURE abort(self: T; code: RuntimeError) = BEGIN END abort;
<*NOWARN*>PROCEDURE check_nil(self: T; code: RuntimeError) = BEGIN END check_nil;
<*NOWARN*>PROCEDURE check_lo(self: T; t: IType; READONLY i: Target.Int; code: RuntimeError) = BEGIN END check_lo;
<*NOWARN*>PROCEDURE check_hi(self: T; t: IType; READONLY i: Target.Int; code: RuntimeError) = BEGIN END check_hi;
<*NOWARN*>PROCEDURE check_range(self: T; t: IType; READONLY a, b: Target.Int; code: RuntimeError) = BEGIN END check_range;
<*NOWARN*>PROCEDURE check_index(self: T; t: IType; code: RuntimeError) = BEGIN END check_index;
<*NOWARN*>PROCEDURE check_eq(self: T; t: IType; code: RuntimeError) = BEGIN END check_eq;
<*NOWARN*>PROCEDURE add_offset(self: T; i: INTEGER) = BEGIN END add_offset;
<*NOWARN*>PROCEDURE index_address(self: T; t: IType; size: INTEGER) = BEGIN END index_address;
<*NOWARN*>PROCEDURE start_call_direct(self: T; p: Proc; lev: INTEGER; t: Type) = BEGIN END start_call_direct;
<*NOWARN*>PROCEDURE start_call_indirect(self: T; t: Type; callingConvention: CallingConvention) = BEGIN END start_call_indirect;
<*NOWARN*>PROCEDURE pop_param(self: T; t: MType) = BEGIN END pop_param;
<*NOWARN*>PROCEDURE pop_struct(self: T; t: TypeUID; s: ByteSize; a: Alignment) = BEGIN END pop_struct;
<*NOWARN*>PROCEDURE pop_static_link(self: T) = BEGIN END pop_static_link;
<*NOWARN*>PROCEDURE call_direct(self: T; p: Proc; t: Type) = BEGIN END call_direct;
<*NOWARN*>PROCEDURE call_indirect(self: T; t: Type; callingConvention: CallingConvention) = BEGIN END call_indirect;
<*NOWARN*>PROCEDURE load_procedure(self: T; p: Proc) = BEGIN END load_procedure;
<*NOWARN*>PROCEDURE load_static_link(self: T; p: Proc) = BEGIN END load_static_link;
<*NOWARN*>PROCEDURE comment(self: T; a, b, c, d: TEXT := NIL) = BEGIN END comment;
<*NOWARN*>PROCEDURE store_ordered(self: T; t: ZType; z: MType; order: MemoryOrder) = BEGIN END store_ordered;
<*NOWARN*>PROCEDURE load_ordered(self: T; t: MType; z: ZType; order: MemoryOrder) = BEGIN END load_ordered;
<*NOWARN*>PROCEDURE exchange(self: T; t: MType; z: ZType; order: MemoryOrder) = BEGIN END exchange;
<*NOWARN*>PROCEDURE compare_exchange(self: T; t: MType; z: ZType; r: IType; success, failure: MemoryOrder) = BEGIN END compare_exchange;
<*NOWARN*>PROCEDURE fence(self: T; order: MemoryOrder) = BEGIN END fence;
<*NOWARN*>PROCEDURE fetch_and_op(self: T; op: AtomicOp; t: MType; z: ZType; order: MemoryOrder) = BEGIN END fetch_and_op;

BEGIN
END M3CG_MultiPass.

