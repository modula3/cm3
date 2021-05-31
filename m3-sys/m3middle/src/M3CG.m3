(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Jun 20 16:02:46 PDT 1995 by kalsow     *)
(*      modified on Tue May 25 14:13:23 PDT 1993 by muller     *)

MODULE M3CG EXPORTS M3CG, M3CG_Ops;

IMPORT Text, Word; 
IMPORT Target;

REVEAL
  T = Public BRANDED "M3CG.T" OBJECT OVERRIDES
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

(*----------------------------------------------------------- UIDs ----------*)

CONST UIDCharTbl 
  = ARRAY [0..61] OF CHAR 
    {'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
     'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 
     'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 
     'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 
     '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'};  

PROCEDURE FormatUID(tUID: TypeUID) : TEXT = 
  VAR resultChars : ARRAY [ 0 .. 5 ] OF CHAR; 
    remainder: Word.T;
  BEGIN 
    IF tUID = NO_UID THEN
      RETURN "zzzzzz" 
    ELSE
      remainder := Word.And(tUID,16_FFFFFFFF); 
      (* ^In case this runs on a 64-bit machine. *) 
      FOR RI := 5 TO 0 BY -1 DO
        resultChars[RI] := UIDCharTbl [Word.Mod(remainder, 62)];
        remainder := Word.Divide(remainder, 62);   
      END; 
    <*ASSERT remainder = 0*> 
      RETURN Text.FromChars(resultChars); 
    END; 
  END FormatUID; 

(*----------------------------------------------------------- ID counters ---*)

PROCEDURE next_label (xx: T;  n: INTEGER := 1): Label =
  BEGIN
    RETURN xx.child.next_label (n);
  END next_label;

(*------------------------------------------------ READONLY configuration ---*)

PROCEDURE set_error_handler (xx: T;  p: PROCEDURE (msg: TEXT)) =
  BEGIN
    xx.child.set_error_handler (p);
  END set_error_handler;

(*----------------------------------------------------- compilation units ---*)

PROCEDURE begin_unit (xx: T;  optimize : INTEGER) =
  BEGIN
    xx.child.begin_unit (optimize);
  END begin_unit;

PROCEDURE end_unit (xx: T) =
  BEGIN
    xx.child.end_unit ();
  END end_unit;

PROCEDURE import_unit (xx: T;  n: Name) =
  BEGIN
    xx.child.import_unit (n);
  END import_unit;

PROCEDURE export_unit (xx: T;  n: Name) =
  BEGIN
    xx.child.export_unit (n);
  END export_unit;

(*------------------------------------------------ debugging line numbers ---*)

PROCEDURE set_source_file (xx: T;  file: TEXT) =
  BEGIN
    xx.child.set_source_file (file);
  END set_source_file;

PROCEDURE set_source_line (xx: T; line: INTEGER) =
  BEGIN
    xx.child.set_source_line (line);
  END set_source_line;

(*------------------------------------------- debugging type declarations ---*)

PROCEDURE declare_typename (xx: T;  t: TypeUID;  n: Name) =
  BEGIN
    xx.child.declare_typename (t, n);
  END declare_typename;

PROCEDURE declare_array (xx: T;  t, index, elt: TypeUID;  s: BitSize; element_typename: Name) =
  BEGIN
    xx.child.declare_array (t, index, elt, s, element_typename);
  END declare_array;

PROCEDURE declare_open_array (xx: T;  t, elt: TypeUID;  s: BitSize; element_typename: Name) =
  BEGIN
    xx.child.declare_open_array (t, elt, s, element_typename);
  END declare_open_array;

PROCEDURE declare_enum (xx: T;  t: TypeUID;  n_elts: INTEGER;  s: BitSize) =
  BEGIN
    xx.child.declare_enum (t, n_elts, s);
  END declare_enum;

PROCEDURE declare_enum_elt (xx: T;  n: Name) =
  BEGIN
    xx.child.declare_enum_elt (n);
  END declare_enum_elt;

PROCEDURE declare_packed (xx: T;  t: TypeUID;  s: BitSize;  base: TypeUID; <*UNUSED*>base_typename: Name) =
  BEGIN
    xx.child.declare_packed (t, s, base);
  END declare_packed;

PROCEDURE declare_record (xx: T; t: TypeUID;  s: BitSize;
                          n_fields: INTEGER)=
  BEGIN
    xx.child.declare_record (t, s, n_fields);
  END declare_record;

PROCEDURE declare_field (xx: T;  n: Name;  o: BitOffset;  s: BitSize;
                         t: TypeUID; typename: Name)=
  BEGIN
    xx.child.declare_field (n, o, s, t, typename);
  END declare_field;

PROCEDURE declare_set (xx: T;  t, domain: TypeUID;  s: BitSize; <*UNUSED*>domain_typename: Name) =
  BEGIN
    xx.child.declare_set (t, domain, s);
  END declare_set;

PROCEDURE declare_subrange (xx: T; t, domain: TypeUID;
                            READONLY min, max: Target.Int;
                            s: BitSize; <*UNUSED*>domain_typename: Name) =
  BEGIN
    xx.child.declare_subrange (t, domain, min, max, s);
  END declare_subrange;


PROCEDURE declare_pointer (xx: T;  t, target: TypeUID;  brand: TEXT;
                           traced: BOOLEAN; target_typename: Name) =
  BEGIN
    xx.child.declare_pointer (t, target, brand, traced, target_typename);
  END declare_pointer;


PROCEDURE declare_indirect (xx: T;  t, target: TypeUID; target_typename: Name) =
  BEGIN
    xx.child.declare_indirect (t, target, target_typename);
  END declare_indirect;


PROCEDURE declare_proctype (xx: T; t: TypeUID; n_formals: INTEGER;
                            result: TypeUID;  n_raises: INTEGER;
                            cc: CallingConvention; result_typename: Name) =
  BEGIN
    xx.child.declare_proctype (t, n_formals, result, n_raises, cc, result_typename);
  END declare_proctype;

PROCEDURE declare_formal (xx: T;  n: Name;  t: TypeUID; typename: Name) =
  BEGIN
    xx.child.declare_formal (n, t, typename);
  END declare_formal;

PROCEDURE declare_raises (xx: T;  n: Name) =
  BEGIN
    xx.child.declare_raises (n);
  END declare_raises;


PROCEDURE declare_object (xx: T; t, super: TypeUID;
                          brand: TEXT;  traced: BOOLEAN;
                          n_fields, n_methods: INTEGER;
                          field_size: BitSize; super_typename: Name) =
  BEGIN
    xx.child.declare_object (t, super, brand, traced, n_fields, n_methods,
                             field_size, super_typename);
  END declare_object;

PROCEDURE declare_method (xx: T;  n: Name;  signature: TypeUID) =
  BEGIN
    xx.child.declare_method (n, signature);
  END declare_method;

PROCEDURE declare_opaque (xx: T;  t, super: TypeUID) =
  BEGIN
    xx.child.declare_opaque (t, super);
  END declare_opaque;

PROCEDURE reveal_opaque (xx: T;  lhs, rhs: TypeUID) =
  BEGIN
    xx.child.reveal_opaque (lhs, rhs);
  END reveal_opaque;

PROCEDURE declare_exception (xx: T;  n: Name;  arg_type: TypeUID;
                           raise_proc: BOOLEAN;  base: Var;  offset: INTEGER) =
  BEGIN
    xx.child.declare_exception (n, arg_type, raise_proc, base, offset);
  END declare_exception;

PROCEDURE widechar_size (xx: T; size: INTEGER) = 
  BEGIN
    xx.child.widechar_size (size);
  END widechar_size;

(*--------------------------------------------------------- runtime hooks ---*)

PROCEDURE set_runtime_proc (xx: T;  n: Name;  p: Proc) =
  BEGIN
    xx.child.set_runtime_proc (n, p);
  END set_runtime_proc;

(*------------------------------------------------- variable declarations ---*)

PROCEDURE import_global (xx: T;  n: Name;  s: ByteSize;  a: Alignment;
                         t: Type;  m3t: TypeUID; typename: Name): Var =
  BEGIN
    RETURN xx.child.import_global (n, s, a, t, m3t, typename);
  END import_global;

PROCEDURE declare_segment (xx: T;  n: Name;  m3t: TypeUID; is_const: BOOLEAN): Var =
  BEGIN
    RETURN xx.child.declare_segment (n, m3t, is_const);
  END declare_segment;

PROCEDURE bind_segment (xx: T;  seg: Var;  s: ByteSize;  a: Alignment;
                        t: Type;  exported, inited: BOOLEAN) =
  BEGIN
    xx.child.bind_segment (seg, s, a, t, exported, inited);
  END bind_segment;

PROCEDURE declare_global (xx: T;  n: Name;  s: ByteSize;  a: Alignment;
                     t: Type;  m3t: TypeUID;  exported, inited: BOOLEAN; <*UNUSED*>typename: Name): Var =
  BEGIN
    RETURN xx.child.declare_global (n, s, a, t, m3t, exported, inited);
  END declare_global;

PROCEDURE declare_constant (xx: T;  n: Name;  s: ByteSize;  a: Alignment;
                     t: Type;  m3t: TypeUID;  exported, inited: BOOLEAN; typename: Name): Var =
  BEGIN
    RETURN xx.child.declare_constant (n, s, a, t, m3t, exported, inited, typename);
  END declare_constant;

PROCEDURE declare_local (xx: T;  n: Name;  s: ByteSize;  a: Alignment;
                         t: Type;  m3t: TypeUID;  in_memory, up_level: BOOLEAN;
                         f: Frequency; typename: Name): Var =
  BEGIN
    RETURN xx.child.declare_local (n, s, a, t, m3t, in_memory, up_level, f, typename);
  END declare_local;

PROCEDURE declare_param (xx: T;  n: Name;  s: ByteSize;  a: Alignment;
                         t: Type;  m3t: TypeUID;  in_memory, up_level: BOOLEAN;
                         f: Frequency; typename: Name): Var =
  BEGIN
    RETURN xx.child.declare_param (n, s, a, t, m3t, in_memory, up_level, f, typename);
  END declare_param;

PROCEDURE declare_temp (xx: T;  s: ByteSize;  a: Alignment;  t: Type;
                        in_memory:BOOLEAN; typename: Name): Var =
  BEGIN
    RETURN xx.child.declare_temp (s, a, t, in_memory, typename);
  END declare_temp;

PROCEDURE free_temp (xx: T;  v: Var) =
  BEGIN
    xx.child.free_temp (v);
  END free_temp;

(*---------------------------------------- static variable initialization ---*)

PROCEDURE begin_init (xx: T;  v: Var) =
  BEGIN
    xx.child.begin_init (v);
  END begin_init;

PROCEDURE end_init (xx: T;  v: Var) =
  BEGIN
    xx.child.end_init (v);
  END end_init;

PROCEDURE init_int (xx: T;  o: ByteOffset;  READONLY value: Target.Int;
                    t: Type) =
  BEGIN
    xx.child.init_int (o, value, t);
  END init_int;

PROCEDURE init_proc (xx: T;  o: ByteOffset;  value: Proc) =
  BEGIN
    xx.child.init_proc (o, value);
  END init_proc;

PROCEDURE init_label (xx: T;  o: ByteOffset;  value: Label) =
  BEGIN
    xx.child.init_label (o, value);
  END init_label;

PROCEDURE init_var (xx: T;  o: ByteOffset;  value: Var;  bias: ByteOffset) =
  BEGIN
    xx.child.init_var (o, value, bias);
  END init_var;

PROCEDURE init_offset (xx: T;  o: ByteOffset;  value: Var) =
  BEGIN
    xx.child.init_offset (o, value);
  END init_offset;

PROCEDURE init_chars (xx: T;  o: ByteOffset;  value: TEXT) =
  BEGIN
    xx.child.init_chars (o, value);
  END init_chars;

PROCEDURE init_float (xx: T;  o: ByteOffset;  READONLY f: Target.Float) =
  BEGIN
    xx.child.init_float (o, f);
  END init_float;

(*------------------------------------------------------------ procedures ---*)

PROCEDURE import_procedure (xx: T;  n: Name;  n_params: INTEGER;
                          ret_type: Type;  cc: CallingConvention;
                          return_typeid: TypeUID;
                          return_typename: Name): Proc =
  BEGIN
    RETURN xx.child.import_procedure (n, n_params, ret_type, cc, return_typeid, return_typename);
  END import_procedure;

PROCEDURE declare_procedure (xx: T;  n: Name;  n_params: INTEGER;
                             return_type: Type;  lev: INTEGER;
                             cc: CallingConvention;
                             exported: BOOLEAN;  parent: Proc;
                             return_typeid: TypeUID;
                             return_typename: Name): Proc =
  BEGIN
    RETURN xx.child.declare_procedure (n, n_params, return_type,
                                       lev, cc, exported, parent,
                                       return_typeid, return_typename);
  END declare_procedure;

PROCEDURE begin_procedure (xx: T;  p: Proc) =
  BEGIN
    xx.child.begin_procedure (p);
  END begin_procedure;

PROCEDURE end_procedure (xx: T;  p: Proc) =
  BEGIN
    xx.child.end_procedure (p);
  END end_procedure;

PROCEDURE begin_block (xx: T) =
  BEGIN
    xx.child.begin_block ();
  END begin_block;

PROCEDURE end_block (xx: T) =
  BEGIN
    xx.child.end_block ();
  END end_block;

PROCEDURE note_procedure_origin (xx: T;  p: Proc) =
  BEGIN
    xx.child.note_procedure_origin (p);
  END note_procedure_origin;

(*------------------------------------------------------------ statements ---*)

PROCEDURE set_label (xx: T;  l: Label;  barrier: BOOLEAN) =
  BEGIN
    xx.child.set_label (l, barrier);
  END set_label;

PROCEDURE jump (xx: T; l: Label) =
  BEGIN
    xx.child.jump (l);
  END jump;

PROCEDURE if_true (xx: T;  t: IType;  l: Label;  f: Frequency) =
  BEGIN
    xx.child.if_true (t, l, f);
  END if_true;

PROCEDURE if_false (xx: T;  t: IType;  l: Label;  f: Frequency) =
  BEGIN
    xx.child.if_false (t, l, f);
  END if_false;

PROCEDURE if_compare (xx: T;  t: ZType;  op: CompareOp;  l: Label;  f: Frequency) =
  BEGIN
    xx.child.if_compare (t, op, l, f);
  END if_compare;

PROCEDURE case_jump (xx: T;  t: IType;  READONLY labels: ARRAY OF Label) =
  BEGIN
    xx.child.case_jump (t, labels);
  END case_jump;

PROCEDURE exit_proc (xx: T;  t: Type) =
  BEGIN
    xx.child.exit_proc (t);
  END exit_proc;

(*------------------------------------------------------------ load/store ---*)

PROCEDURE load (xx: T;  v: Var;  o: ByteOffset;  t: MType;  u: ZType) =
  BEGIN
    xx.child.load (v, o, t, u);
  END load;

PROCEDURE store (xx: T;  v: Var;  o: ByteOffset;  t: ZType;  u: MType) =
  BEGIN
    xx.child.store (v, o, t, u);
  END store;

PROCEDURE load_address (xx: T;  v: Var;  o: ByteOffset) =
  BEGIN
    xx.child.load_address (v, o);
  END load_address;

PROCEDURE load_indirect (xx: T;  o: ByteOffset;  t: MType;  u: ZType) =
  BEGIN
    xx.child.load_indirect (o, t, u);
  END load_indirect;

PROCEDURE store_indirect (xx: T;  o: ByteOffset;  t: ZType;  u: MType) =
  BEGIN
    xx.child.store_indirect (o, t, u);
  END store_indirect;

(*-------------------------------------------------------------- literals ---*)

PROCEDURE load_nil (xx: T) =
  BEGIN
    xx.child.load_nil ();
  END load_nil;

PROCEDURE load_integer (xx: T;  t: IType;  READONLY i: Target.Int) =
  BEGIN
    xx.child.load_integer (t, i);
  END load_integer;

PROCEDURE load_float (xx: T;  t: RType;  READONLY f: Target.Float) =
  BEGIN
    xx.child.load_float (t, f);
  END load_float;

(*------------------------------------------------------------ arithmetic ---*)

PROCEDURE compare (xx: T;  t: ZType;  u: IType;  op: CompareOp) =
  BEGIN
    xx.child.compare (t, u, op);
  END compare;

PROCEDURE add (xx: T;  t: AType) =
  BEGIN
    xx.child.add (t);
  END add;

PROCEDURE subtract (xx: T;  t: AType) =
  BEGIN
    xx.child.subtract (t);
  END subtract;

PROCEDURE multiply (xx: T;  t: AType) =
  BEGIN
    xx.child.multiply (t);
  END multiply;

PROCEDURE divide (xx: T;  t: RType) =
  BEGIN
    xx.child.divide (t);
  END divide;

PROCEDURE div (xx: T;  t: IType;  a, b: Sign) =
  BEGIN
    xx.child.div (t, a, b);
  END div;

PROCEDURE mod (xx: T;  t: IType;  a, b: Sign) =
  BEGIN
    xx.child.mod (t, a, b);
  END mod;

PROCEDURE negate (xx: T;  t: AType) =
  BEGIN
    xx.child.negate (t);
  END negate;

PROCEDURE abs (xx: T;  t: AType) =
  BEGIN
    xx.child.abs (t);
  END abs;

PROCEDURE max (xx: T;  t: ZType) =
  BEGIN
    xx.child.max (t);
  END max;

PROCEDURE min (xx: T;  t: ZType) =
  BEGIN
    xx.child.min (t);
  END min;

PROCEDURE cvt_int (xx: T;  t: RType;  u: IType;  op: ConvertOp) =
  BEGIN
    xx.child.cvt_int (t, u, op);
  END cvt_int;

PROCEDURE cvt_float (xx: T;  t: AType;  u: RType) =
  BEGIN
    xx.child.cvt_float (t, u);
  END cvt_float;

(*------------------------------------------------------------------ sets ---*)

PROCEDURE set_union (xx: T;  s: ByteSize) =
  BEGIN
    xx.child.set_union (s);
  END set_union;

PROCEDURE set_difference (xx: T;  s: ByteSize) =
  BEGIN
    xx.child.set_difference (s);
  END set_difference;

PROCEDURE set_intersection (xx: T;  s: ByteSize) =
  BEGIN
    xx.child.set_intersection (s);
  END set_intersection;

PROCEDURE set_sym_difference (xx: T;  s: ByteSize) =
  BEGIN
    xx.child.set_sym_difference (s);
  END set_sym_difference;

PROCEDURE set_member (xx: T;  s: ByteSize;  t: IType) =
  BEGIN
    xx.child.set_member (s, t);
  END set_member;

PROCEDURE set_compare (xx: T;  s: ByteSize;  op: CompareOp;  t: IType) =
  BEGIN
    xx.child.set_compare (s, op, t);
  END set_compare;

PROCEDURE set_range (xx: T;  s: ByteSize;  t: IType) =
  BEGIN
    xx.child.set_range (s, t);
  END set_range;

PROCEDURE set_singleton (xx: T;  s: ByteSize;  t: IType) =
  BEGIN
    xx.child.set_singleton (s, t);
  END set_singleton;

(*------------------------------------------------- Word.T bit operations ---*)

PROCEDURE not (xx: T;  t: IType) =
  BEGIN
    xx.child.not (t);
  END not;

PROCEDURE and (xx: T;  t: IType) =
  BEGIN
    xx.child.and (t);
  END and;

PROCEDURE or (xx: T;  t: IType) =
  BEGIN
    xx.child.or (t);
  END or;

PROCEDURE xor (xx: T;  t: IType) =
  BEGIN
    xx.child.xor (t);
  END xor;

PROCEDURE shift (xx: T;  t: IType) =
  BEGIN
    xx.child.shift (t);
  END shift;

PROCEDURE shift_left (xx: T;  t: IType) =
  BEGIN
    xx.child.shift_left (t);
  END shift_left;

PROCEDURE shift_right (xx: T;  t: IType) =
  BEGIN
    xx.child.shift_right (t);
  END shift_right;

PROCEDURE rotate (xx: T;  t: IType) =
  BEGIN
    xx.child.rotate (t);
  END rotate;

PROCEDURE rotate_left (xx: T;  t: IType) =
  BEGIN
    xx.child.rotate_left (t);
  END rotate_left;

PROCEDURE rotate_right (xx: T;  t: IType) =
  BEGIN
    xx.child.rotate_right (t);
  END rotate_right;

PROCEDURE widen (xx: T;  sign: BOOLEAN) =
  BEGIN
    xx.child.widen (sign);
  END widen;

PROCEDURE chop (xx: T) =
  BEGIN
    xx.child.chop ();
  END chop;

PROCEDURE extract (xx: T;  t: IType;  sign: BOOLEAN) =
  BEGIN
    xx.child.extract (t, sign);
  END extract;

PROCEDURE extract_n (xx: T;  t: IType;  sign: BOOLEAN;  n: CARDINAL) =
  BEGIN
    xx.child.extract_n (t, sign, n);
  END extract_n;

PROCEDURE extract_mn (xx: T;  t: IType;  sign: BOOLEAN;  m, n: CARDINAL) =
  BEGIN
    xx.child.extract_mn (t, sign, m, n);
  END extract_mn;

PROCEDURE insert (xx: T;  t: IType) =
  BEGIN
    xx.child.insert (t);
  END insert;

PROCEDURE insert_n (xx: T;  t: IType;  n: CARDINAL) =
  BEGIN
    xx.child.insert_n (t, n);
  END insert_n;

PROCEDURE insert_mn (xx: T;  t: IType;  m, n: CARDINAL) =
  BEGIN
    xx.child.insert_mn (t, m, n);
  END insert_mn;

(*------------------------------------------------ misc. stack/memory ops ---*)

PROCEDURE swap (xx: T;  a, b: Type) =
  BEGIN
    xx.child.swap (a, b);
  END swap;

PROCEDURE pop (xx: T;  t: Type) =
  BEGIN
    xx.child.pop (t);
  END pop;

PROCEDURE copy_n (xx: T;  u: IType;  t: MType;  overlap: BOOLEAN) =
  BEGIN
    xx.child.copy_n (u, t, overlap);
  END copy_n;

PROCEDURE copy (xx: T;  n: INTEGER;  t: MType;  overlap: BOOLEAN) =
  BEGIN
    xx.child.copy (n, t, overlap);
  END copy;

PROCEDURE zero_n (xx: T;  u: IType;  t: MType) =
  BEGIN
    xx.child.zero_n (u, t);
  END zero_n;

PROCEDURE zero (xx: T;  n: INTEGER;  t: MType) =
  BEGIN
    xx.child.zero (n, t);
  END zero;

(*----------------------------------------------------------- conversions ---*)

PROCEDURE loophole (xx: T;  from, two: ZType) =
  BEGIN
    xx.child.loophole (from, two);
  END loophole;

(*------------------------------------------------ traps & runtime checks ---*)

PROCEDURE abort (xx: T;  code: RuntimeError) =
  BEGIN
    xx.child.abort (code);
  END abort;

PROCEDURE check_nil (xx: T;  code: RuntimeError) =
  BEGIN
    xx.child.check_nil (code);
  END check_nil;

PROCEDURE check_lo (xx: T;  t: IType;  READONLY i: Target.Int;
                    code: RuntimeError) =
  BEGIN
    xx.child.check_lo (t, i, code);
  END check_lo;

PROCEDURE check_hi (xx: T;  t: IType;  READONLY i: Target.Int;
                    code: RuntimeError) =
  BEGIN
    xx.child.check_hi (t, i, code);
  END check_hi;

PROCEDURE check_range (xx: T;  t: IType;  READONLY a, b: Target.Int;
                       code: RuntimeError) =
  BEGIN
    xx.child.check_range (t, a, b, code);
  END check_range;

PROCEDURE check_index (xx: T;  t: IType;  code: RuntimeError) =
  BEGIN
    xx.child.check_index (t, code);
  END check_index;

PROCEDURE check_eq (xx: T;  t: IType;  code: RuntimeError) =
  BEGIN
    xx.child.check_eq (t, code);
  END check_eq;

(*---------------------------------------------------- address arithmetic ---*)

PROCEDURE add_offset (xx: T; i: INTEGER) =
  BEGIN
    xx.child.add_offset (i);
  END add_offset;

PROCEDURE index_address (xx: T;  t: IType;  size: INTEGER) =
  BEGIN
    xx.child.index_address (t, size);
  END index_address;

(*------------------------------------------------------- procedure calls ---*)

PROCEDURE start_call_direct (xx: T;  p: Proc;  lev: INTEGER;  t: Type) =
  BEGIN
    xx.child.start_call_direct (p, lev, t);
  END start_call_direct;

PROCEDURE start_call_indirect (xx: T;  t: Type;  cc: CallingConvention) =
  BEGIN
    xx.child.start_call_indirect (t, cc);
  END start_call_indirect;

PROCEDURE pop_param (xx: T;  t: MType) =
  BEGIN
    xx.child.pop_param (t);
  END pop_param;

PROCEDURE pop_struct (xx: T;  t: TypeUID;  s: ByteSize;  a: Alignment) =
  BEGIN
    xx.child.pop_struct (t, s, a);
  END pop_struct;

PROCEDURE pop_static_link (xx: T) =
  BEGIN
    xx.child.pop_static_link ();
  END pop_static_link;

PROCEDURE call_direct (xx: T; p: Proc;  t: Type) =
  BEGIN
    xx.child.call_direct (p, t);
  END call_direct;

PROCEDURE call_indirect (xx: T;  t: Type;  cc: CallingConvention) =
  BEGIN
    xx.child.call_indirect (t, cc);
  END call_indirect;

(*------------------------------------------- procedure and closure types ---*)

PROCEDURE load_procedure (xx: T;  p: Proc) =
  BEGIN
    xx.child.load_procedure (p);
  END load_procedure;

PROCEDURE load_static_link (xx: T;  p: Proc) =
  BEGIN
    xx.child.load_static_link (p);
  END load_static_link;

(*----------------------------------------------------------------- misc. ---*)

PROCEDURE comment (xx: T;  a, b, c, d: TEXT := NIL) =
  BEGIN
    xx.child.comment (a, b, c, d);
  END comment;

(*--------------------------------------------------------------- atomics ---*)

PROCEDURE store_ordered (xx: T;  t: ZType;  u: MType;  order: MemoryOrder) =
  BEGIN
    xx.child.store_ordered (t, u, order);
  END store_ordered;

PROCEDURE load_ordered (xx: T;  t: MType;  u: ZType;  order: MemoryOrder) =
  BEGIN
    xx.child.load_ordered (t, u, order);
  END load_ordered;

PROCEDURE exchange (xx: T;  t: MType;  u: ZType;  order: MemoryOrder) =
  BEGIN
    xx.child.exchange (t, u, order);
  END exchange;

PROCEDURE compare_exchange (xx: T;  t: MType;  u: ZType;  r: IType;
                            success, failure: MemoryOrder) =
  BEGIN
    xx.child.compare_exchange (t, u, r, success, failure);
  END compare_exchange;

PROCEDURE fence (xx: T;  order: MemoryOrder) =
  BEGIN
    xx.child.fence (order);
  END fence;

PROCEDURE fetch_and_op (xx: T;  op: AtomicOp;  t: MType;  u: ZType;
                        order: MemoryOrder) =
  BEGIN
    xx.child.fetch_and_op (op, t, u, order);
  END fetch_and_op;

BEGIN
END M3CG.
