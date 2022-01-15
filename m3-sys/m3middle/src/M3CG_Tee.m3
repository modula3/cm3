(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)

(* Modified from file M3CG.m3.  Changes are numerous, but the
   majority follow straightforward patterns, done via regular
   expression search-and-replace.  Rodney M. Bates, 2021-01-15,
   rodney.m.bates@acm.org
*)

MODULE M3CG_Tee;

IMPORT M3CG, M3CG_Ops, Target;

FROM M3CG IMPORT Name, ByteOffset, CallingConvention;
FROM M3CG IMPORT ByteSize, Alignment, Frequency, RuntimeError;
FROM M3CG IMPORT Var, Proc, Label, Sign, CompareOp, ConvertOp, AtomicOp;
FROM M3CG IMPORT Type, ZType, AType, RType, IType, MType;
FROM M3CG IMPORT MemoryOrder, TypeUID;
FROM M3CG IMPORT BitSize, BitOffset, No_label;


TYPE 
  U = M3CG.T BRANDED "M3CG_Tee.T" OBJECT
    child2: M3CG.T;
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
    declare_exception := declare_exception;
    widechar_size := widechar_size; 
    set_runtime_proc := set_runtime_proc;
    import_global := import_global;
    declare_segment := declare_segment;
    bind_segment := bind_segment;
    declare_global := declare_global;
    declare_constant := declare_constant;
    declare_local := declare_local;
    declare_param := declare_param;
    declare_temp :=declare_temp;
    free_temp := free_temp;
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
    load_address := load_address;
    load_indirect := load_indirect;
    store := store;
    store_indirect := store_indirect;
    load_nil := load_nil;
    load_integer := load_integer;
    load_float := load_float;
    compare := compare;
    add := add;
    subtract := subtract;
    multiply := multiply;
    divide := divide;
    negate := negate;
    abs := abs;
    max := max;
    min := min;
    cvt_int := cvt_int;
    cvt_float := cvt_float;
    div := div;
    mod := mod;
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
    copy_n := copy_n;
    copy := copy;
    zero_n := zero_n;
    zero := zero;
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

PROCEDURE New (child1, child2: M3CG.T): M3CG.T =
(* A new code generator that forwards each operation
   to both child1 and child2. *)

  BEGIN
    RETURN NEW (U, child := child1, child2 := child2);
  END New;

TYPE VarPair = Var OBJECT v1, v2: Var END;
TYPE ProcPair = Proc OBJECT p1, p2: Proc END;
TYPE LabelPair = RECORD l1, l2: Label END;
TYPE LabelPairsRef = REF ARRAY OF LabelPair;
VAR nextLabel:Label := No_label;
VAR Labels: LabelPairsRef := NIL;
CONST initLabelsSize = 250;

(*----------------------------------------------------------- ID counters ---*)

PROCEDURE next_label (xx: U;  n: INTEGER := 1): Label =
  VAR l1, l2, toLabel, result: Label;
  VAR newLabels: LabelPairsRef;
  BEGIN
    l1 := xx.child.next_label (n);
    l2 := xx.child2.next_label (n);
    toLabel := nextLabel + n;
    IF Labels = NIL THEN
      Labels := NEW (LabelPairsRef, MAX (toLabel * 2, initLabelsSize));
      nextLabel := 0;
    ELSIF toLabel > NUMBER (Labels^) THEN
      newLabels := NEW (LabelPairsRef, toLabel * 2);
      SUBARRAY(newLabels^, 0, nextLabel) := SUBARRAY(Labels^,0,nextLabel);
      Labels := newLabels;
    END;
    result := nextLabel;
    FOR I := nextLabel TO toLabel - 1 DO
      WITH pair = Labels^[I] DO
        pair.l1 := l1;
        pair.l2 := l2;
        INC (l1);
        INC (l2);
      END;
    END;
    nextLabel := toLabel;
    RETURN result;
  END next_label;

(*------------------------------------------------ READONLY configuration ---*)

PROCEDURE set_error_handler (xx: U;  p: PROCEDURE (msg: TEXT)) =
  BEGIN
    xx.child.set_error_handler (p);
    xx.child2.set_error_handler (p);
  END set_error_handler;

(*----------------------------------------------------- compilation units ---*)

PROCEDURE begin_unit (xx: U;  optimize : INTEGER) =
  BEGIN
    xx.child.begin_unit (optimize);
    xx.child2.begin_unit (optimize);
  END begin_unit;

PROCEDURE end_unit (xx: U) =
  BEGIN
    xx.child.end_unit ();
    xx.child2.end_unit ();
  END end_unit;

PROCEDURE import_unit (xx: U;  n: Name) =
  BEGIN
    xx.child.import_unit (n);
    xx.child2.import_unit (n);
  END import_unit;

PROCEDURE export_unit (xx: U;  n: Name) =
  BEGIN
    xx.child.export_unit (n);
    xx.child2.export_unit (n);
  END export_unit;

(*------------------------------------------------ debugging line numbers ---*)

PROCEDURE set_source_file (xx: U;  file: TEXT) =
  BEGIN
    xx.child.set_source_file (file);
    xx.child2.set_source_file (file);
  END set_source_file;

PROCEDURE set_source_line (xx: U; line: INTEGER) =
  BEGIN
    xx.child.set_source_line (line);
    xx.child2.set_source_line (line);
  END set_source_line;

(*------------------------------------------- debugging type declarations ---*)

PROCEDURE declare_typename (xx: U;  t: TypeUID;  n: Name) =
  BEGIN
    xx.child.declare_typename (t, n);
    xx.child2.declare_typename (t, n);
  END declare_typename;

PROCEDURE declare_array (xx: U;  t, index, elt: TypeUID;  s: BitSize; element_typename: Name) =
  BEGIN
    xx.child.declare_array (t, index, elt, s, element_typename);
    xx.child2.declare_array (t, index, elt, s, element_typename);
  END declare_array;

PROCEDURE declare_open_array (xx: U;  t, elt: TypeUID;  s: BitSize; element_typename: Name) =
  BEGIN
    xx.child.declare_open_array (t, elt, s, element_typename);
    xx.child2.declare_open_array (t, elt, s, element_typename);
  END declare_open_array;

PROCEDURE declare_enum (xx: U;  t: TypeUID;  n_elts: INTEGER;  s: BitSize) =
  BEGIN
    xx.child.declare_enum (t, n_elts, s);
    xx.child2.declare_enum (t, n_elts, s);
  END declare_enum;

PROCEDURE declare_enum_elt (xx: U;  n: Name) =
  BEGIN
    xx.child.declare_enum_elt (n);
    xx.child2.declare_enum_elt (n);
  END declare_enum_elt;

PROCEDURE declare_packed (xx: U;  t: TypeUID;  s: BitSize;  base: TypeUID; <*UNUSED*>base_typename: Name) =
  BEGIN
    xx.child.declare_packed (t, s, base);
    xx.child2.declare_packed (t, s, base);
  END declare_packed;

PROCEDURE declare_record (xx: U; t: TypeUID;  s: BitSize;
                          n_fields: INTEGER)=
  BEGIN
    xx.child.declare_record (t, s, n_fields);
    xx.child2.declare_record (t, s, n_fields);
  END declare_record;

PROCEDURE declare_field (xx: U;  n: Name;  o: BitOffset;  s: BitSize;
                         t: TypeUID; typename: Name)=
  BEGIN
    xx.child.declare_field (n, o, s, t, typename);
    xx.child2.declare_field (n, o, s, t, typename);
  END declare_field;

PROCEDURE declare_set (xx: U;  t, domain: TypeUID;  s: BitSize; <*UNUSED*>domain_typename: Name) =
  BEGIN
    xx.child.declare_set (t, domain, s);
    xx.child2.declare_set (t, domain, s);
  END declare_set;

PROCEDURE declare_subrange (xx: U; t, domain: TypeUID;
                            READONLY min, max: Target.Int;
                            s: BitSize; <*UNUSED*>domain_typename: Name) =
  BEGIN
    xx.child.declare_subrange (t, domain, min, max, s);
    xx.child2.declare_subrange (t, domain, min, max, s);
  END declare_subrange;


PROCEDURE declare_pointer (xx: U;  t, target: TypeUID;  brand: TEXT;
                           traced: BOOLEAN; target_typename: Name) =
  BEGIN
    xx.child.declare_pointer (t, target, brand, traced, target_typename);
    xx.child2.declare_pointer (t, target, brand, traced, target_typename);
  END declare_pointer;


PROCEDURE declare_indirect (xx: U;  t, target: TypeUID; target_typename: Name) =
  BEGIN
    xx.child.declare_indirect (t, target, target_typename);
    xx.child2.declare_indirect (t, target, target_typename);
  END declare_indirect;


PROCEDURE declare_proctype (xx: U; t: TypeUID; n_formals: INTEGER;
                            result: TypeUID;  n_raises: INTEGER;
                            cc: CallingConvention; result_typename: Name) =
  BEGIN
    xx.child.declare_proctype (t, n_formals, result, n_raises, cc, result_typename);
    xx.child2.declare_proctype (t, n_formals, result, n_raises, cc, result_typename);
  END declare_proctype;

PROCEDURE declare_formal (xx: U;  n: Name;  t: TypeUID; typename: Name) =
  BEGIN
    xx.child.declare_formal (n, t, typename);
    xx.child2.declare_formal (n, t, typename);
  END declare_formal;

PROCEDURE declare_raises (xx: U;  n: Name) =
  BEGIN
    xx.child.declare_raises (n);
    xx.child2.declare_raises (n);
  END declare_raises;


PROCEDURE declare_object (xx: U; t, super: TypeUID;
                          brand: TEXT;  traced: BOOLEAN;
                          n_fields, n_methods: INTEGER;
                          field_size: BitSize; super_typename: Name) =
  BEGIN
    xx.child.declare_object (t, super, brand, traced, n_fields, n_methods,
                             field_size, super_typename);
  END declare_object;

PROCEDURE declare_method (xx: U;  n: Name;  signature: TypeUID) =
  BEGIN
    xx.child.declare_method (n, signature);
    xx.child2.declare_method (n, signature);
  END declare_method;

PROCEDURE declare_opaque (xx: U;  t, super: TypeUID) =
  BEGIN
    xx.child.declare_opaque (t, super);
    xx.child2.declare_opaque (t, super);
  END declare_opaque;

PROCEDURE reveal_opaque (xx: U;  lhs, rhs: TypeUID) =
  BEGIN
    xx.child.reveal_opaque (lhs, rhs);
    xx.child2.reveal_opaque (lhs, rhs);
  END reveal_opaque;

PROCEDURE declare_exception (xx: U;  n: Name;  arg_type: TypeUID;
                           raise_proc: BOOLEAN;  base: Var;  offset: INTEGER) =
  VAR vp: VarPair := base;
  BEGIN
    xx.child.declare_exception (n, arg_type, raise_proc, vp.v1, offset);
    xx.child2.declare_exception (n, arg_type, raise_proc, vp.v2, offset);
  END declare_exception;

PROCEDURE widechar_size (xx: U; size: INTEGER) = 
  BEGIN
    xx.child.widechar_size (size);
    xx.child2.widechar_size (size);
  END widechar_size;

(*--------------------------------------------------------- runtime hooks ---*)

PROCEDURE set_runtime_proc (xx: U;  n: Name;  p: Proc) =
  VAR pp: ProcPair := p;
  BEGIN
    xx.child.set_runtime_proc (n, pp.p1);
    xx.child2.set_runtime_proc (n, pp.p2);
  END set_runtime_proc;

(*------------------------------------------------- variable declarations ---*)

PROCEDURE import_global (xx: U;  n: Name;  s: ByteSize;  a: Alignment;
                         t: Type;  m3t: TypeUID; typename: Name): Var =
  VAR result := NEW(VarPair);
  BEGIN
    result.v1 := xx.child.import_global (n, s, a, t, m3t, typename);
    result.v2 := xx.child2.import_global (n, s, a, t, m3t, typename);
    RETURN result;
  END import_global;

PROCEDURE declare_segment (xx: U;  n: Name;  m3t: TypeUID; is_const: BOOLEAN): Var =
  VAR result := NEW(VarPair);
  BEGIN
    result.v1 := xx.child.declare_segment (n, m3t, is_const);
    result.v2 := xx.child2.declare_segment (n, m3t, is_const);
    RETURN result;
  END declare_segment;

PROCEDURE bind_segment (xx: U;  seg: Var;  s: ByteSize;  a: Alignment;
                        t: Type;  exported, inited: BOOLEAN) =
  VAR vp: VarPair := seg;
  BEGIN
    xx.child.bind_segment (vp.v1, s, a, t, exported, inited);
    xx.child2.bind_segment (vp.v2, s, a, t, exported, inited);
  END bind_segment;

PROCEDURE declare_global (xx: U;  n: Name;  s: ByteSize;  a: Alignment;
                     t: Type;  m3t: TypeUID;  exported, inited: BOOLEAN; <*UNUSED*>typename: Name): Var =
  VAR result := NEW(VarPair);
  BEGIN
    result.v1 := xx.child.declare_global (n, s, a, t, m3t, exported, inited);
    result.v2 := xx.child2.declare_global (n, s, a, t, m3t, exported, inited);
    RETURN result;
  END declare_global;

PROCEDURE declare_constant (xx: U;  n: Name;  s: ByteSize;  a: Alignment;
                     t: Type;  m3t: TypeUID;  exported, inited: BOOLEAN; typename: Name): Var =
  VAR result := NEW(VarPair);
  BEGIN
    result.v1 := xx.child.declare_constant (n, s, a, t, m3t, exported, inited, typename);
    result.v2 := xx.child2.declare_constant (n, s, a, t, m3t, exported, inited, typename);
    RETURN result;
  END declare_constant;

PROCEDURE declare_local (xx: U;  n: Name;  s: ByteSize;  a: Alignment;
                         t: Type;  m3t: TypeUID;  in_memory, up_level: BOOLEAN;
                         f: Frequency; typename: Name): Var =
  VAR result := NEW(VarPair);
  BEGIN
    result.v1 := xx.child.declare_local (n, s, a, t, m3t, in_memory, up_level, f, typename);
    result.v2 := xx.child2.declare_local (n, s, a, t, m3t, in_memory, up_level, f, typename);
    RETURN result;
  END declare_local;

PROCEDURE declare_param (xx: U;  n: Name;  s: ByteSize;  a: Alignment;
                         t: Type;  m3t: TypeUID;  in_memory, up_level: BOOLEAN;
                         f: Frequency; typename: Name): Var =
  VAR result := NEW(VarPair);
  BEGIN
    result.v1 := xx.child.declare_param (n, s, a, t, m3t, in_memory, up_level, f, typename);
    result.v2 := xx.child2.declare_param (n, s, a, t, m3t, in_memory, up_level, f, typename);
    RETURN result;
  END declare_param;

PROCEDURE declare_temp (xx: U;  s: ByteSize;  a: Alignment;  t: Type;
                        in_memory:BOOLEAN; typename: Name): Var =
  VAR result := NEW(VarPair);
  BEGIN
    result.v1 := xx.child.declare_temp (s, a, t, in_memory, typename);
    result.v2 := xx.child2.declare_temp (s, a, t, in_memory, typename);
    RETURN result;
  END declare_temp;

PROCEDURE free_temp (xx: U;  v: Var) =
  VAR vp: VarPair := v;
  BEGIN
    xx.child.free_temp (vp.v1);
    xx.child2.free_temp (vp.v2);
  END free_temp;

(*---------------------------------------- static variable initialization ---*)

PROCEDURE begin_init (xx: U;  v: Var) =
  VAR vp: VarPair := v;
  BEGIN
    xx.child.begin_init (vp.v1);
    xx.child2.begin_init (vp.v2);
  END begin_init;

PROCEDURE end_init (xx: U;  v: Var) =
  VAR vp: VarPair := v;
  BEGIN
    xx.child.end_init (vp.v1);
    xx.child2.end_init (vp.v2);
  END end_init;

PROCEDURE init_int (xx: U;  o: ByteOffset;  READONLY value: Target.Int;
                    t: Type) =
  BEGIN
    xx.child.init_int (o, value, t);
    xx.child2.init_int (o, value, t);
  END init_int;

PROCEDURE init_proc (xx: U;  o: ByteOffset;  value: Proc) =
  VAR pp: ProcPair := value;
  BEGIN
    xx.child.init_proc (o, pp.p1);
    xx.child2.init_proc (o, pp.p2);
  END init_proc;

PROCEDURE init_label (xx: U;  o: ByteOffset;  value: Label) =
  BEGIN
    xx.child.init_label (o, Labels^[value].l1);
    xx.child2.init_label (o, Labels^[value].l2);
  END init_label;

PROCEDURE init_var (xx: U;  o: ByteOffset;  value: Var;  bias: ByteOffset) =
  VAR vp: VarPair := value;
  BEGIN
    xx.child.init_var (o, vp.v1, bias);
    xx.child2.init_var (o, vp.v2, bias);
  END init_var;

PROCEDURE init_offset (xx: U;  o: ByteOffset;  value: Var) =
  VAR vp: VarPair := value;
  BEGIN
    xx.child.init_offset (o, vp.v1);
    xx.child2.init_offset (o, vp.v2);
  END init_offset;

PROCEDURE init_chars (xx: U;  o: ByteOffset;  value: TEXT) =
  BEGIN
    xx.child.init_chars (o, value);
    xx.child2.init_chars (o, value);
  END init_chars;

PROCEDURE init_float (xx: U;  o: ByteOffset;  READONLY f: Target.Float) =
  BEGIN
    xx.child.init_float (o, f);
    xx.child2.init_float (o, f);
  END init_float;

(*------------------------------------------------------------ procedures ---*)

PROCEDURE import_procedure (xx: U;  n: Name;  n_params: INTEGER;
                          ret_type: Type;  cc: CallingConvention;
                          return_typeid: TypeUID;
                          return_typename: Name): Proc =
  VAR result := NEW(ProcPair);
  BEGIN
    result.p1 := xx.child.import_procedure (n, n_params, ret_type, cc, return_typeid, return_typename);
    result.p2 := xx.child2.import_procedure (n, n_params, ret_type, cc, return_typeid, return_typename);
    RETURN result;
  END import_procedure;

PROCEDURE declare_procedure (xx: U;  n: Name;  n_params: INTEGER;
                             return_type: Type;  lev: INTEGER;
                             cc: CallingConvention;
                             exported: BOOLEAN;  parent: Proc;
                             return_typeid: TypeUID;
                             return_typename: Name): Proc =
  VAR result := NEW(ProcPair);
  BEGIN
    result.p1 := xx.child.declare_procedure
      (n, n_params, return_type, lev, cc, exported, parent,
       return_typeid, return_typename);
    result.p2 := xx.child2.declare_procedure
      (n, n_params, return_type, lev, cc, exported, parent,
       return_typeid, return_typename);
    RETURN result;
  END declare_procedure;

PROCEDURE begin_procedure (xx: U;  p: Proc) =
  VAR pp: ProcPair := p;
  BEGIN
    xx.child.begin_procedure (pp.p1);
    xx.child2.begin_procedure (pp.p2);
  END begin_procedure;

PROCEDURE end_procedure (xx: U;  p: Proc) =
  VAR pp: ProcPair := p;
  BEGIN
    xx.child.end_procedure (pp.p1);
    xx.child2.end_procedure (pp.p2);
  END end_procedure;

PROCEDURE begin_block (xx: U) =
  BEGIN
    xx.child.begin_block ();
    xx.child2.begin_block ();
  END begin_block;

PROCEDURE end_block (xx: U) =
  BEGIN
    xx.child.end_block ();
    xx.child2.end_block ();
  END end_block;

PROCEDURE note_procedure_origin (xx: U;  p: Proc) =
  VAR pp: ProcPair := p;
  BEGIN
    xx.child.note_procedure_origin (pp.p1);
    xx.child2.note_procedure_origin (pp.p2);
  END note_procedure_origin;

(*------------------------------------------------------------ statements ---*)

PROCEDURE set_label (xx: U;  l: Label;  barrier: BOOLEAN) =
  BEGIN
    xx.child.set_label (Labels^[l].l1, barrier);
    xx.child2.set_label (Labels^[l].l2, barrier);
  END set_label;

PROCEDURE jump (xx: U; l: Label) =
  BEGIN
    xx.child.jump (Labels^[l].l1);
    xx.child2.jump (Labels^[l].l2);
  END jump;

PROCEDURE if_true (xx: U;  t: IType;  l: Label;  f: Frequency) =
  BEGIN
    xx.child.if_true (t, Labels^[l].l1, f);
    xx.child2.if_true (t, Labels^[l].l2, f);
  END if_true;

PROCEDURE if_false (xx: U;  t: IType;  l: Label;  f: Frequency) =
  BEGIN
    xx.child.if_false (t, Labels^[l].l1, f);
    xx.child2.if_false (t, Labels^[l].l2, f);
  END if_false;

PROCEDURE if_compare (xx: U;  t: ZType;  op: CompareOp;  l: Label;  f: Frequency) =
  BEGIN
    xx.child.if_compare (t, op, Labels^[l].l1, f);
    xx.child2.if_compare (t, op, Labels^[l].l2, f);
  END if_compare;

PROCEDURE case_jump (xx: U;  t: IType;  READONLY labels: ARRAY OF Label) =
  BEGIN
    xx.child.case_jump (t, labels);
    xx.child2.case_jump (t, labels);
  END case_jump;

PROCEDURE exit_proc (xx: U;  t: Type) =
  BEGIN
    xx.child.exit_proc (t);
    xx.child2.exit_proc (t);
  END exit_proc;

(*------------------------------------------------------------ load/store ---*)

PROCEDURE load (xx: U;  v: Var;  o: ByteOffset;  t: MType;  u: ZType) =
  VAR vp: VarPair := v;
  BEGIN
    xx.child.load (vp.v1, o, t, u);
    xx.child2.load (vp.v2, o, t, u);
  END load;

PROCEDURE store (xx: U;  v: Var;  o: ByteOffset;  t: ZType;  u: MType) =
  VAR vp: VarPair := v;
  BEGIN
    xx.child.store (vp.v1, o, t, u);
    xx.child2.store (vp.v2, o, t, u);
  END store;

PROCEDURE load_address (xx: U;  v: Var;  o: ByteOffset) =
  VAR vp: VarPair := v;
  BEGIN
    xx.child.load_address (vp.v1, o);
    xx.child2.load_address (vp.v2, o);
  END load_address;

PROCEDURE load_indirect (xx: U;  o: ByteOffset;  t: MType;  u: ZType) =
  BEGIN
    xx.child.load_indirect (o, t, u);
    xx.child2.load_indirect (o, t, u);
  END load_indirect;

PROCEDURE store_indirect (xx: U;  o: ByteOffset;  t: ZType;  u: MType) =
  BEGIN
    xx.child.store_indirect (o, t, u);
    xx.child2.store_indirect (o, t, u);
  END store_indirect;

(*-------------------------------------------------------------- literals ---*)

PROCEDURE load_nil (xx: U) =
  BEGIN
    xx.child.load_nil ();
    xx.child2.load_nil ();
  END load_nil;

PROCEDURE load_integer (xx: U;  t: IType;  READONLY i: Target.Int) =
  BEGIN
    xx.child.load_integer (t, i);
    xx.child2.load_integer (t, i);
  END load_integer;

PROCEDURE load_float (xx: U;  t: RType;  READONLY f: Target.Float) =
  BEGIN
    xx.child.load_float (t, f);
    xx.child2.load_float (t, f);
  END load_float;

(*------------------------------------------------------------ arithmetic ---*)

PROCEDURE compare (xx: U;  t: ZType;  u: IType;  op: CompareOp) =
  BEGIN
    xx.child.compare (t, u, op);
    xx.child2.compare (t, u, op);
  END compare;

PROCEDURE add (xx: U;  t: AType) =
  BEGIN
    xx.child.add (t);
    xx.child2.add (t);
  END add;

PROCEDURE subtract (xx: U;  t: AType) =
  BEGIN
    xx.child.subtract (t);
    xx.child2.subtract (t);
  END subtract;

PROCEDURE multiply (xx: U;  t: AType) =
  BEGIN
    xx.child.multiply (t);
    xx.child2.multiply (t);
  END multiply;

PROCEDURE divide (xx: U;  t: RType) =
  BEGIN
    xx.child.divide (t);
    xx.child2.divide (t);
  END divide;

PROCEDURE div (xx: U;  t: IType;  a, b: Sign) =
  BEGIN
    xx.child.div (t, a, b);
    xx.child2.div (t, a, b);
  END div;

PROCEDURE mod (xx: U;  t: IType;  a, b: Sign) =
  BEGIN
    xx.child.mod (t, a, b);
    xx.child2.mod (t, a, b);
  END mod;

PROCEDURE negate (xx: U;  t: AType) =
  BEGIN
    xx.child.negate (t);
    xx.child2.negate (t);
  END negate;

PROCEDURE abs (xx: U;  t: AType) =
  BEGIN
    xx.child.abs (t);
    xx.child2.abs (t);
  END abs;

PROCEDURE max (xx: U;  t: ZType) =
  BEGIN
    xx.child.max (t);
    xx.child2.max (t);
  END max;

PROCEDURE min (xx: U;  t: ZType) =
  BEGIN
    xx.child.min (t);
    xx.child2.min (t);
  END min;

PROCEDURE cvt_int (xx: U;  t: RType;  u: IType;  op: ConvertOp) =
  BEGIN
    xx.child.cvt_int (t, u, op);
    xx.child2.cvt_int (t, u, op);
  END cvt_int;

PROCEDURE cvt_float (xx: U;  t: AType;  u: RType) =
  BEGIN
    xx.child.cvt_float (t, u);
    xx.child2.cvt_float (t, u);
  END cvt_float;

(*------------------------------------------------------------------ sets ---*)

PROCEDURE set_union (xx: U;  s: ByteSize) =
  BEGIN
    xx.child.set_union (s);
    xx.child2.set_union (s);
  END set_union;

PROCEDURE set_difference (xx: U;  s: ByteSize) =
  BEGIN
    xx.child.set_difference (s);
    xx.child2.set_difference (s);
  END set_difference;

PROCEDURE set_intersection (xx: U;  s: ByteSize) =
  BEGIN
    xx.child.set_intersection (s);
    xx.child2.set_intersection (s);
  END set_intersection;

PROCEDURE set_sym_difference (xx: U;  s: ByteSize) =
  BEGIN
    xx.child.set_sym_difference (s);
    xx.child2.set_sym_difference (s);
  END set_sym_difference;

PROCEDURE set_member (xx: U;  s: ByteSize;  t: IType) =
  BEGIN
    xx.child.set_member (s, t);
    xx.child2.set_member (s, t);
  END set_member;

PROCEDURE set_compare (xx: U;  s: ByteSize;  op: CompareOp;  t: IType) =
  BEGIN
    xx.child.set_compare (s, op, t);
    xx.child2.set_compare (s, op, t);
  END set_compare;

PROCEDURE set_range (xx: U;  s: ByteSize;  t: IType) =
  BEGIN
    xx.child.set_range (s, t);
    xx.child2.set_range (s, t);
  END set_range;

PROCEDURE set_singleton (xx: U;  s: ByteSize;  t: IType) =
  BEGIN
    xx.child.set_singleton (s, t);
    xx.child2.set_singleton (s, t);
  END set_singleton;

(*------------------------------------------------- Word.T bit operations ---*)

PROCEDURE not (xx: U;  t: IType) =
  BEGIN
    xx.child.not (t);
    xx.child2.not (t);
  END not;

PROCEDURE and (xx: U;  t: IType) =
  BEGIN
    xx.child.and (t);
    xx.child2.and (t);
  END and;

PROCEDURE or (xx: U;  t: IType) =
  BEGIN
    xx.child.or (t);
    xx.child2.or (t);
  END or;

PROCEDURE xor (xx: U;  t: IType) =
  BEGIN
    xx.child.xor (t);
    xx.child2.xor (t);
  END xor;

PROCEDURE shift (xx: U;  t: IType) =
  BEGIN
    xx.child.shift (t);
    xx.child2.shift (t);
  END shift;

PROCEDURE shift_left (xx: U;  t: IType) =
  BEGIN
    xx.child.shift_left (t);
    xx.child2.shift_left (t);
  END shift_left;

PROCEDURE shift_right (xx: U;  t: IType) =
  BEGIN
    xx.child.shift_right (t);
    xx.child2.shift_right (t);
  END shift_right;

PROCEDURE rotate (xx: U;  t: IType) =
  BEGIN
    xx.child.rotate (t);
    xx.child2.rotate (t);
  END rotate;

PROCEDURE rotate_left (xx: U;  t: IType) =
  BEGIN
    xx.child.rotate_left (t);
    xx.child2.rotate_left (t);
  END rotate_left;

PROCEDURE rotate_right (xx: U;  t: IType) =
  BEGIN
    xx.child.rotate_right (t);
    xx.child2.rotate_right (t);
  END rotate_right;

PROCEDURE widen (xx: U;  sign: BOOLEAN) =
  BEGIN
    xx.child.widen (sign);
    xx.child2.widen (sign);
  END widen;

PROCEDURE chop (xx: U) =
  BEGIN
    xx.child.chop ();
    xx.child2.chop ();
  END chop;

PROCEDURE extract (xx: U;  t: IType;  sign: BOOLEAN) =
  BEGIN
    xx.child.extract (t, sign);
    xx.child2.extract (t, sign);
  END extract;

PROCEDURE extract_n (xx: U;  t: IType;  sign: BOOLEAN;  n: CARDINAL) =
  BEGIN
    xx.child.extract_n (t, sign, n);
    xx.child2.extract_n (t, sign, n);
  END extract_n;

PROCEDURE extract_mn (xx: U;  t: IType;  sign: BOOLEAN;  m, n: CARDINAL) =
  BEGIN
    xx.child.extract_mn (t, sign, m, n);
    xx.child2.extract_mn (t, sign, m, n);
  END extract_mn;

PROCEDURE insert (xx: U;  t: IType) =
  BEGIN
    xx.child.insert (t);
    xx.child2.insert (t);
  END insert;

PROCEDURE insert_n (xx: U;  t: IType;  n: CARDINAL) =
  BEGIN
    xx.child.insert_n (t, n);
    xx.child2.insert_n (t, n);
  END insert_n;

PROCEDURE insert_mn (xx: U;  t: IType;  m, n: CARDINAL) =
  BEGIN
    xx.child.insert_mn (t, m, n);
    xx.child2.insert_mn (t, m, n);
  END insert_mn;

(*------------------------------------------------ misc. stack/memory ops ---*)

PROCEDURE swap (xx: U;  a, b: Type) =
  BEGIN
    xx.child.swap (a, b);
    xx.child2.swap (a, b);
  END swap;

PROCEDURE pop (xx: U;  t: Type) =
  BEGIN
    xx.child.pop (t);
    xx.child2.pop (t);
  END pop;

PROCEDURE copy_n (xx: U;  u: IType;  t: MType;  overlap: BOOLEAN) =
  BEGIN
    xx.child.copy_n (u, t, overlap);
    xx.child2.copy_n (u, t, overlap);
  END copy_n;

PROCEDURE copy (xx: U;  n: INTEGER;  t: MType;  overlap: BOOLEAN) =
  BEGIN
    xx.child.copy (n, t, overlap);
    xx.child2.copy (n, t, overlap);
  END copy;

PROCEDURE zero_n (xx: U;  u: IType;  t: MType) =
  BEGIN
    xx.child.zero_n (u, t);
    xx.child2.zero_n (u, t);
  END zero_n;

PROCEDURE zero (xx: U;  n: INTEGER;  t: MType) =
  BEGIN
    xx.child.zero (n, t);
    xx.child2.zero (n, t);
  END zero;

(*----------------------------------------------------------- conversions ---*)

PROCEDURE loophole (xx: U;  from, two: ZType) =
  BEGIN
    xx.child.loophole (from, two);
    xx.child2.loophole (from, two);
  END loophole;

(*------------------------------------------------ traps & runtime checks ---*)

PROCEDURE abort (xx: U;  code: RuntimeError) =
  BEGIN
    xx.child.abort (code);
    xx.child2.abort (code);
  END abort;

PROCEDURE check_nil (xx: U;  code: RuntimeError) =
  BEGIN
    xx.child.check_nil (code);
    xx.child2.check_nil (code);
  END check_nil;

PROCEDURE check_lo (xx: U;  t: IType;  READONLY i: Target.Int;
                    code: RuntimeError) =
  BEGIN
    xx.child.check_lo (t, i, code);
    xx.child2.check_lo (t, i, code);
  END check_lo;

PROCEDURE check_hi (xx: U;  t: IType;  READONLY i: Target.Int;
                    code: RuntimeError) =
  BEGIN
    xx.child.check_hi (t, i, code);
    xx.child2.check_hi (t, i, code);
  END check_hi;

PROCEDURE check_range (xx: U;  t: IType;  READONLY a, b: Target.Int;
                       code: RuntimeError) =
  BEGIN
    xx.child.check_range (t, a, b, code);
    xx.child2.check_range (t, a, b, code);
  END check_range;

PROCEDURE check_index (xx: U;  t: IType;  code: RuntimeError) =
  BEGIN
    xx.child.check_index (t, code);
    xx.child2.check_index (t, code);
  END check_index;

PROCEDURE check_eq (xx: U;  t: IType;  code: RuntimeError) =
  BEGIN
    xx.child.check_eq (t, code);
    xx.child2.check_eq (t, code);
  END check_eq;

(*---------------------------------------------------- address arithmetic ---*)

PROCEDURE add_offset (xx: U; i: INTEGER) =
  BEGIN
    xx.child.add_offset (i);
    xx.child2.add_offset (i);
  END add_offset;

PROCEDURE index_address (xx: U;  t: IType;  size: INTEGER) =
  BEGIN
    xx.child.index_address (t, size);
    xx.child2.index_address (t, size);
  END index_address;

(*------------------------------------------------------- procedure calls ---*)

PROCEDURE start_call_direct (xx: U;  p: Proc;  lev: INTEGER;  t: Type) =
  VAR pp: ProcPair := p;
  BEGIN
    xx.child.start_call_direct (pp.p1, lev, t);
    xx.child2.start_call_direct (pp.p2, lev, t);
  END start_call_direct;

PROCEDURE start_call_indirect (xx: U;  t: Type;  cc: CallingConvention) =
  BEGIN
    xx.child.start_call_indirect (t, cc);
    xx.child2.start_call_indirect (t, cc);
  END start_call_indirect;

PROCEDURE pop_param (xx: U;  t: MType) =
  BEGIN
    xx.child.pop_param (t);
    xx.child2.pop_param (t);
  END pop_param;

PROCEDURE pop_struct (xx: U;  t: TypeUID;  s: ByteSize;  a: Alignment) =
  BEGIN
    xx.child.pop_struct (t, s, a);
    xx.child2.pop_struct (t, s, a);
  END pop_struct;

PROCEDURE pop_static_link (xx: U) =
  BEGIN
    xx.child.pop_static_link ();
    xx.child2.pop_static_link ();
  END pop_static_link;

PROCEDURE call_direct (xx: U; p: Proc;  t: Type) =
  VAR pp: ProcPair := p;
  BEGIN
    xx.child.call_direct (pp.p1, t);
    xx.child2.call_direct (pp.p2, t);
  END call_direct;

PROCEDURE call_indirect (xx: U;  t: Type;  cc: CallingConvention) =
  BEGIN
    xx.child.call_indirect (t, cc);
    xx.child2.call_indirect (t, cc);
  END call_indirect;

(*------------------------------------------- procedure and closure types ---*)

PROCEDURE load_procedure (xx: U;  p: Proc) =
  VAR pp: ProcPair := p;
  BEGIN
    xx.child.load_procedure (pp.p1);
    xx.child2.load_procedure (pp.p2);
  END load_procedure;

PROCEDURE load_static_link (xx: U;  p: Proc) =
  VAR pp: ProcPair := p;
  BEGIN
    xx.child.load_static_link (pp.p1);
    xx.child2.load_static_link (pp.p2);
  END load_static_link;

(*----------------------------------------------------------------- misc. ---*)

PROCEDURE comment (xx: U;  a, b, c, d: TEXT := NIL) =
  BEGIN
    xx.child.comment (a, b, c, d);
    xx.child2.comment (a, b, c, d);
  END comment;

(*--------------------------------------------------------------- atomics ---*)

PROCEDURE store_ordered (xx: U;  t: ZType;  u: MType;  order: MemoryOrder) =
  BEGIN
    xx.child.store_ordered (t, u, order);
    xx.child2.store_ordered (t, u, order);
  END store_ordered;

PROCEDURE load_ordered (xx: U;  t: MType;  u: ZType;  order: MemoryOrder) =
  BEGIN
    xx.child.load_ordered (t, u, order);
    xx.child2.load_ordered (t, u, order);
  END load_ordered;

PROCEDURE exchange (xx: U;  t: MType;  u: ZType;  order: MemoryOrder) =
  BEGIN
    xx.child.exchange (t, u, order);
    xx.child2.exchange (t, u, order);
  END exchange;

PROCEDURE compare_exchange (xx: U;  t: MType;  u: ZType;  r: IType;
                            success, failure: MemoryOrder) =
  BEGIN
    xx.child.compare_exchange (t, u, r, success, failure);
    xx.child2.compare_exchange (t, u, r, success, failure);
  END compare_exchange;

PROCEDURE fence (xx: U;  order: MemoryOrder) =
  BEGIN
    xx.child.fence (order);
    xx.child2.fence (order);
  END fence;

PROCEDURE fetch_and_op (xx: U;  op: AtomicOp;  t: MType;  u: ZType;
                        order: MemoryOrder) =
  BEGIN
    xx.child.fetch_and_op (op, t, u, order);
    xx.child2.fetch_and_op (op, t, u, order);
  END fetch_and_op;

BEGIN
END M3CG_Tee.
