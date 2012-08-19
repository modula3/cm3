MODULE M3CG_DoNothing;

IMPORT Wr, Text, IntRefTbl, Word;
IMPORT M3Buf, M3ID, M3CG, M3CG_Ops, M3CG_Binary;
IMPORT Target, TInt AS TargetInt, TFloat, TWord;
FROM TFloat IMPORT Byte;

FROM M3CG IMPORT Name, ByteOffset, TypeUID, CallingConvention;
FROM M3CG IMPORT BitSize, ByteSize, Alignment, Frequency;
FROM M3CG IMPORT Var, Proc, Label, Sign, BitOffset;
FROM M3CG IMPORT Type, ZType, AType, RType, IType, MType;
FROM M3CG IMPORT CompareOp, ConvertOp, AtomicOp, RuntimeError;
FROM M3CG IMPORT MemoryOrder;

TYPE WrVar  = Var    OBJECT tag: INTEGER END;
TYPE WrProc = Proc   OBJECT tag: INTEGER END;

TYPE
  U = M3CG.T OBJECT
        next_label_id := 1;
        next_var      := 1;
        next_proc     := 1;
        next_scope    := 1;
      OVERRIDES
        next_label := next_label;
        set_error_handler := set_error_handler;
        begin_unit := begin_unit;
        end_unit   := end_unit;
        import_unit := import_unit;
        export_unit := export_unit;
        set_source_file := set_source_file;
        set_source_line := set_source_line;
        declare_typename := declare_typename;
        declare_array := declare_array;
        declare_open_array := declare_open_array;
        declare_enum := declare_enum;
        declare_enum_elt := declare_enum_elt;
        declare_packed  := declare_packed;
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
        import_global  := import_global;
        declare_segment := declare_segment;
        bind_segment := bind_segment;
        declare_global := declare_global;
        declare_constant := declare_constant;
        declare_local  := declare_local;
        declare_param  := declare_param;
        declare_temp   := declare_temp;
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
        if_true  := if_true;
        if_false := if_false;
        if_compare := if_compare;
        case_jump := case_jump;
        exit_proc := exit_proc;
        load  := load;
        store := store;
        load_address := load_address;
        load_indirect := load_indirect;
        store_indirect := store_indirect;
        load_nil      := load_nil;
        load_integer  := load_integer;
        load_float    := load_float;
        compare  := compare;
        add      := add;
        subtract := subtract;
        multiply := multiply;
        divide   := divide;
        div      := div;
        mod      := mod;
        negate   := negate;
        abs      := abs;
        max      := max;
        min      := min;
        cvt_int  := cvt_int;
        cvt_float := cvt_float;
        set_union          := set_union;
        set_difference     := set_difference;
        set_intersection   := set_intersection;
        set_sym_difference := set_sym_difference;
        set_member         := set_member;
        set_compare  := set_compare;
        set_range    := set_range;
        set_singleton := set_singleton;
        not := not;
        and := and;
        or  := or;
        xor := xor;
        shift        := shift;
        shift_left   := shift_left;
        shift_right  := shift_right;
        rotate       := rotate;
        rotate_left  := rotate_left;
        rotate_right := rotate_right;
        widen := widen;
        chop := chop;
        extract := extract;
        extract_n := extract_n;
        extract_mn := extract_mn;
        insert  := insert;
        insert_n  := insert_n;
        insert_mn  := insert_mn;
        swap := swap;
        pop  := pop;
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

(*---------------------------------------------------------------------------*)

PROCEDURE New (): M3CG.T =
  BEGIN
    RETURN NEW (U);
  END New;

(*----------------------------------------------------------- ID counters ---*)

PROCEDURE next_label (u: U;  n: INTEGER := 1): Label =
  VAR label := u.next_label_id;
  BEGIN
    INC (u.next_label_id, n);
    RETURN label;
  END next_label;

(*------------------------------------------------ READONLY configuration ---*)

PROCEDURE set_error_handler (<*UNUSED*> u: U;
                             <*UNUSED*> p: M3CG_Ops.ErrorHandler) =
  BEGIN
    (* skip -- we don't generate any errors *)
  END set_error_handler;

(*----------------------------------------------------- compilation units ---*)

PROCEDURE begin_unit (u: U;  optimize: INTEGER) =
  (* called before any other method to initialize the compilation unit *)
  BEGIN
  END begin_unit;

PROCEDURE end_unit (u: U) =
  (* called after all other methods to finalize the unit and write the
     resulting object *)
  BEGIN
  END end_unit;

PROCEDURE import_unit (u: U;  n: Name) =
  (* note that the current compilation unit imports the interface 'n' *)
  BEGIN
  END import_unit;

PROCEDURE export_unit (u: U;  n: Name) =
  (* note that the current compilation unit exports the interface 'n' *)
  BEGIN
  END export_unit;

(*------------------------------------------------ debugging line numbers ---*)

PROCEDURE set_source_file (u: U; file: TEXT) =
  (* Sets the current source file name.  Subsequent statements
     and expressions are associated with this source location. *)
  BEGIN
  END set_source_file;

PROCEDURE set_source_line (u: U; line: INTEGER) =
  (* Sets the current source line number.  Subsequent statements
   and expressions are associated with this source location. *)
  BEGIN
  END set_source_line;

(*------------------------------------------- debugging type declarations ---*)

PROCEDURE declare_typename (u: U;  t: TypeUID;  n: Name) =
  BEGIN
  END declare_typename;

PROCEDURE declare_array (u: U;  t, index, elt: TypeUID;  s: BitSize) =
  BEGIN
  END declare_array;

PROCEDURE declare_open_array (u: U;  t, elt: TypeUID;  s: BitSize) =
  BEGIN
  END declare_open_array;

PROCEDURE declare_enum (u: U;  t: TypeUID;  n_elts: INTEGER;  s: BitSize) =
  BEGIN
  END declare_enum;

PROCEDURE declare_enum_elt (u: U;  n: Name) =
  BEGIN
  END declare_enum_elt;

PROCEDURE declare_packed  (u: U;  t: TypeUID;  s: BitSize;  base: TypeUID) =
  BEGIN
  END declare_packed;

PROCEDURE declare_record (u: U; t: TypeUID;  s: BitSize;
                          n_fields: INTEGER)=
  BEGIN
  END declare_record;

PROCEDURE declare_field (u: U;  n: Name;  o: BitOffset;  s: BitSize;
                         t: TypeUID)=
  BEGIN
  END declare_field;

PROCEDURE declare_set (u: U;  t, domain: TypeUID;  s: BitSize) =
  BEGIN
  END declare_set;

PROCEDURE declare_subrange (u: U; t, domain: TypeUID;
                            READONLY min, max: Target.Int;
                            s: BitSize) =
  BEGIN
  END declare_subrange;

PROCEDURE declare_pointer (u: U;  t, target: TypeUID;  brand: TEXT;
                           traced: BOOLEAN) =
  BEGIN
  END declare_pointer;


PROCEDURE declare_indirect (u: U;  t, target: TypeUID) =
  BEGIN
  END declare_indirect;


PROCEDURE declare_proctype (u: U;  t: TypeUID;  n_formals: INTEGER;
                            result: TypeUID;  n_raises: INTEGER;
                            cc: CallingConvention) =
  BEGIN
  END declare_proctype;

PROCEDURE declare_formal (u: U;  n: Name;  t: TypeUID) =
  BEGIN
  END declare_formal;

PROCEDURE declare_raises (u: U;  n: Name) =
  BEGIN
  END declare_raises;


PROCEDURE declare_object (u: U;  t, super: TypeUID;
                          brand: TEXT;  traced: BOOLEAN;
                          n_fields, n_methods: INTEGER;
                          field_size: BitSize) =
  BEGIN
  END declare_object;

PROCEDURE declare_method (u: U;  n: Name;  signature: TypeUID) =
  BEGIN
  END declare_method;

PROCEDURE declare_opaque (u: U;  t, super: TypeUID) =
  BEGIN
  END declare_opaque;

PROCEDURE reveal_opaque (u: U;  lhs, rhs: TypeUID) =
  BEGIN
  END reveal_opaque;

PROCEDURE declare_exception (u: U;  n: Name;  arg_type: TypeUID;
                           raise_proc: BOOLEAN;  base: Var;  offset: INTEGER) =
  BEGIN
  END declare_exception;

(*--------------------------------------------------------- runtime hooks ---*)

PROCEDURE set_runtime_proc (u: U;  n: Name;  p: Proc) = BEGIN END set_runtime_proc;

(*------------------------------------------------- variable declarations ---*)

PROCEDURE NewVar (u: U): Var =
  VAR v := NEW (WrVar, tag := u.next_var);
  BEGIN
    INC (u.next_var);
    RETURN v;
  END NewVar;

PROCEDURE import_global (u: U;  n: Name;  s: ByteSize;  a: Alignment;
                         t: Type;  m3t: TypeUID): Var =
  BEGIN
    RETURN NewVar (u);
  END import_global;

PROCEDURE declare_segment (u: U;  n: Name;  m3t: TypeUID;  is_const: BOOLEAN): Var =
  BEGIN
    RETURN NewVar (u);
  END declare_segment;

PROCEDURE bind_segment (u: U;  seg: Var;  s: ByteSize;  a: Alignment;
                        t: Type;  exported, inited: BOOLEAN) =
  BEGIN
  END bind_segment;

PROCEDURE declare_global (u: U;  n: Name;  s: ByteSize;  a: Alignment;
                     t: Type;  m3t: TypeUID;  exported, inited: BOOLEAN): Var =
  BEGIN
    RETURN NewVar (u);
  END declare_global;

PROCEDURE declare_constant (u: U;  n: Name;  s: ByteSize;  a: Alignment;
                     t: Type;  m3t: TypeUID;  exported, inited: BOOLEAN): Var =
  BEGIN
    RETURN NewVar (u);
  END declare_constant;

PROCEDURE declare_local (u: U;  n: Name;  s: ByteSize;  a: Alignment;
                         t: Type;  m3t: TypeUID;  in_memory, up_level: BOOLEAN;
                         f: Frequency): Var =
  BEGIN
    RETURN NewVar (u);
  END declare_local;

PROCEDURE declare_param (u: U;  n: Name;  s: ByteSize;  a: Alignment;
                         t: Type;  m3t: TypeUID;  in_memory, up_level: BOOLEAN;
                         f: Frequency): Var =
  BEGIN
    RETURN NewVar (u);
  END declare_param;

PROCEDURE declare_temp   (u: U;  s: ByteSize;  a: Alignment;  t: Type;
                          in_memory:BOOLEAN): Var =
  VAR v := NewVar (u);
  BEGIN
    RETURN v;
  END declare_temp;

PROCEDURE free_temp (u: U;  v: Var) =
  BEGIN
  END free_temp;

(*---------------------------------------- static variable initialization ---*)

PROCEDURE begin_init (u: U;  v: Var) =
  BEGIN
  END begin_init;

PROCEDURE end_init (u: U;  v: Var) =
  BEGIN
  END end_init;

PROCEDURE init_int (u: U;  o: ByteOffset;  READONLY value: Target.Int;
                    t: Type) =
  BEGIN
  END init_int;

PROCEDURE init_proc (u: U;  o: ByteOffset;  value: Proc) =
  BEGIN
  END init_proc;

PROCEDURE init_label (u: U;  o: ByteOffset;  value: Label) =
  BEGIN
  END init_label;

PROCEDURE init_var (u: U;  o: ByteOffset;  value: Var;  bias: ByteOffset) =
  BEGIN
  END init_var;

PROCEDURE init_offset (u: U;  o: ByteOffset;  value: Var) =
  BEGIN
  END init_offset;

PROCEDURE init_chars (u: U;  o: ByteOffset;  value: TEXT) =
  BEGIN
  END init_chars;

PROCEDURE init_float (u: U;  o: ByteOffset;  READONLY f: Target.Float) =
  BEGIN
  END init_float;

(*------------------------------------------------------------ procedures ---*)

PROCEDURE NewProc (u: U): Proc =
  VAR p := NEW (WrProc, tag := u.next_proc);
  BEGIN
    INC (u.next_proc);
    RETURN p;
  END NewProc;

PROCEDURE import_procedure (u: U;  n: Name;  n_params: INTEGER;
                          ret_type: Type;  cc: CallingConvention): Proc =
  VAR p := NewProc (u);
  BEGIN
    RETURN p;
  END import_procedure;

PROCEDURE declare_procedure (u: U;  n: Name;  n_params: INTEGER;
                             return_type: Type;  lev: INTEGER;
                             cc: CallingConvention;
                             exported: BOOLEAN;  parent: Proc): Proc =
  VAR p := NewProc (u);
  BEGIN
    RETURN p;
  END declare_procedure;

PROCEDURE begin_procedure (u: U;  p: Proc) =
  BEGIN
  END begin_procedure;


PROCEDURE end_procedure (u: U;  p: Proc) =
  BEGIN
  END end_procedure;

PROCEDURE begin_block (u: U) =
  (* marks the beginning of a nested anonymous block *)
  BEGIN
  END begin_block;

PROCEDURE end_block (u: U) =
  (* marks the ending of a nested anonymous block *)
  BEGIN
  END end_block;

PROCEDURE note_procedure_origin (u: U;  p: Proc) =
  BEGIN
  END note_procedure_origin;

(*------------------------------------------------------------ statements ---*)

PROCEDURE set_label (u: U;  l: Label;  barrier: BOOLEAN) =
  (* define 'l' to be at the current pc *)
  BEGIN
  END set_label;

PROCEDURE jump (u: U; l: Label) =
  (* GOTO l *)
  BEGIN
  END jump;

PROCEDURE if_true  (u: U;  t: IType;  l: Label;  f: Frequency) =
  (* IF (s0.t # 0) GOTO l ; pop *)
  BEGIN
  END if_true;

PROCEDURE if_false (u: U;  t: IType;  l: Label;  f: Frequency) =
  (* IF (s0.t = 0) GOTO l ; pop *)
  BEGIN
  END if_false;

PROCEDURE if_compare (u: U;  t: ZType;  op: CompareOp;  l: Label;  f: Frequency) =
  (* IF (s1.t  op  s0.t) GOTO l ; pop(2) *)
  BEGIN
  END if_compare;

PROCEDURE case_jump (u: U;  t: IType;  READONLY labels: ARRAY OF Label) =
  (* "GOTO labels[s0.t] ; pop" with no range checking on s0.t *)
  BEGIN
  END case_jump;

PROCEDURE exit_proc (u: U; t: Type) =
  (* Returns s0.t if the stack is non-empty, otherwise returns no value. *)
  BEGIN
  END exit_proc;

(*------------------------------------------------------------ load/store ---*)

PROCEDURE load  (u: U;  v: Var;  o: ByteOffset;  t: MType;  z: ZType) =
  BEGIN
  END load;

PROCEDURE store  (u: U;  v: Var;  o: ByteOffset;  t: ZType;  z: MType) =
  BEGIN
  END store;

PROCEDURE load_address (u: U;  v: Var;  o: ByteOffset) =
  BEGIN
  END load_address;

PROCEDURE load_indirect (u: U;  o: ByteOffset;  t: MType;  z: ZType) =
  BEGIN
  END load_indirect;

PROCEDURE store_indirect (u: U;  o: ByteOffset;  t: ZType;  z: MType) =
  BEGIN
  END store_indirect;

(*-------------------------------------------------------------- literals ---*)

PROCEDURE load_nil (u: U) =
  (* push ; s0.A := a *)
  BEGIN
  END load_nil;

PROCEDURE load_integer  (u: U;  t: IType;  READONLY i: Target.Int) =
  (* push ; s0.t:= i *)
  BEGIN
  END load_integer;

PROCEDURE load_float    (u: U;  t: RType;  READONLY f: Target.Float) =
  (* push ; s0.t := f *)
  BEGIN
  END load_float;

(*------------------------------------------------------------ arithmetic ---*)

PROCEDURE compare (u: U;  t: ZType;  z: IType;  op: CompareOp) =
  (* s1.z := (s1.t  op  s0.t)  ; pop *)
  BEGIN
  END compare;

PROCEDURE add (u: U;  t: AType) =
  (* s1.t := s1.t + s0.t ; pop *)
  BEGIN
  END add;

PROCEDURE subtract (u: U;  t: AType) =
  (* s1.t := s1.t - s0.t ; pop *)
  BEGIN
  END subtract;

PROCEDURE multiply (u: U;  t: AType) =
  (* s1.t := s1.t * s0.t ; pop *)
  BEGIN
  END multiply;

PROCEDURE divide (u: U;  t: RType) =
  (* s1.t := s1.t / s0.t ; pop *)
  BEGIN
  END divide;

PROCEDURE div (u: U;  t: IType;  a, b: Sign) =
  (* s1.t := s1.t DIV s0.t ; pop *)
  BEGIN
  END div;

PROCEDURE mod (u: U;  t: IType;  a, b: Sign) =
  (* s1.t := s1.t MOD s0.t ; pop *)
  BEGIN
  END mod;

PROCEDURE negate (u: U;  t: AType) =
  (* s0.t := - s0.t *)
  BEGIN
  END negate;

PROCEDURE abs      (u: U;  t: AType) =
  (* s0.t := ABS (s0.t) (noop on Words) *)
  BEGIN
  END abs;

PROCEDURE max      (u: U;  t: ZType) =
  (* s1.t := MAX (s1.t, s0.t) ; pop *)
  BEGIN
  END max;

PROCEDURE min      (u: U;  t: ZType) =
  (* s1.t := MIN (s1.t, s0.t) ; pop *)
  BEGIN
  END min;

PROCEDURE cvt_int (u: U;  t: RType;  x: IType;  op: ConvertOp) =
  (* s0.x := op (s0.t) *)
  BEGIN
  END cvt_int;

PROCEDURE cvt_float (u: U;  t: AType;  x: RType) =
  (* s0.x := FLOAT (s0.t, x) *)
  BEGIN
  END cvt_float;

(*------------------------------------------------------------------ sets ---*)

PROCEDURE set_union (u: U;  s: ByteSize) =
  (* s1.B := s1.B + s0.B ; pop *)
  BEGIN
  END set_union;

PROCEDURE set_difference (u: U;  s: ByteSize) =
  (* s1.B := s1.B - s0.B ; pop *)
  BEGIN
  END set_difference;

PROCEDURE set_intersection (u: U;  s: ByteSize) =
  (* s1.B := s1.B * s0.B ; pop *)
  BEGIN
  END set_intersection;

PROCEDURE set_sym_difference (u: U;  s: ByteSize) =
  (* s1.B := s1.B / s0.B ; pop *)
  BEGIN
  END set_sym_difference;

PROCEDURE set_member (u: U;  s: ByteSize;  t: IType) =
  (* s1.t := (s0.t IN s1.B) ; pop *)
  BEGIN
  END set_member;

PROCEDURE set_compare (u: U;  s: ByteSize;  op: CompareOp;  t: IType) =
  (* s1.t := (s1.B  op  s0.B) ; pop *)
  BEGIN
  END set_compare;

PROCEDURE set_range (u: U;  s: ByteSize;  t: IType) =
  (* s2.A [s1.t .. s0.t] := 1's; pop(3)*)
  BEGIN
  END set_range;

PROCEDURE set_singleton (u: U;  s: ByteSize;  t: IType) =
  (* s1.A [s0.t] := 1; pop(2) *)
  BEGIN
  END set_singleton;

(*------------------------------------------------- Word.T bit operations ---*)

PROCEDURE not (u: U;  t: IType) =
  (* s0.t := Word.Not (s0.t) *)
  BEGIN
  END not;

PROCEDURE and (u: U;  t: IType) =
  (* s1.t := Word.And (s1.t, s0.t) ; pop *)
  BEGIN
  END and;

PROCEDURE or  (u: U;  t: IType) =
  (* s1.t := Word.Or  (s1.t, s0.t) ; pop *)
  BEGIN
  END or;

PROCEDURE xor (u: U;  t: IType) =
  (* s1.t := Word.Xor (s1.t, s0.t) ; pop *)
  BEGIN
  END xor;

PROCEDURE shift (u: U;  t: IType) =
  (* s1.t := Word.Shift  (s1.t, s0.t) ; pop *)
  BEGIN
  END shift;

PROCEDURE shift_left   (u: U;  t: IType) =
  (* s1.t := Word.Shift  (s1.t, s0.t) ; pop *)
  BEGIN
  END shift_left;

PROCEDURE shift_right  (u: U;  t: IType) =
  (* s1.t := Word.Shift  (s1.t, -s0.t) ; pop *)
  BEGIN
  END shift_right;

PROCEDURE rotate (u: U;  t: IType) =
  (* s1.t := Word.Rotate (s1.t, s0.t) ; pop *)
  BEGIN
  END rotate;

PROCEDURE rotate_left  (u: U;  t: IType) =
  (* s1.t := Word.Rotate (s1.t, s0.t) ; pop *)
  BEGIN
  END rotate_left;

PROCEDURE rotate_right (u: U;  t: IType) =
  (* s1.t := Word.Rotate (s1.t, -s0.t) ; pop *)
  BEGIN
  END rotate_right;

PROCEDURE widen (u: U;  sign: BOOLEAN) =
  (* s0.I64 := s0.I32;  IF sign THEN SignExtend s0;  *)
  BEGIN
  END widen;

PROCEDURE chop (u: U) =
  (* s0.I32 := Word.And (s0.I64, 16_ffffffff);  *)
  BEGIN
  END chop;

PROCEDURE extract (u: U;  t: IType;  sign: BOOLEAN) =
  (* s2.t := Word.Extract(s2.t, s1.t, s0.t);
     IF sign THEN SignExtend s2 END; pop(2) *)
  BEGIN
  END extract;

PROCEDURE extract_n (u: U;  t: IType;  sign: BOOLEAN;  n: CARDINAL) =
  (* s1.t := Word.Extract(s1.t, s0.t, n);
     IF sign THEN SignExtend s1 END; pop(1) *)
  BEGIN
  END extract_n;

PROCEDURE extract_mn (u: U;  t: IType;  sign: BOOLEAN;  m, n: CARDINAL) =
  (* s0.t := Word.Extract(s0.t, m, n);
     IF sign THEN SignExtend s0 END; *)
  BEGIN
  END extract_mn;

PROCEDURE insert  (u: U;  t: IType) =
  (* s3.t := Word.Insert (s3.t, s2.t, s1.t, s0.t) ; pop(3) *)
  BEGIN
  END insert;

PROCEDURE insert_n  (u: U;  t: IType;  n: CARDINAL) =
  (* s2.t := Word.Insert (s2.t, s1.t, s0.t, n) ; pop(2) *)
  BEGIN
  END insert_n;

PROCEDURE insert_mn  (u: U;  t: IType;  m, n: CARDINAL) =
  (* s1.t := Word.Insert (s1.t, s0.t, m, n) ; pop(2) *)
  BEGIN
  END insert_mn;

(*------------------------------------------------ misc. stack/memory ops ---*)

PROCEDURE swap (u: U;  a, b: Type) =
  (* tmp := s1 ; s1 := s0 ; s0 := tmp *)
  BEGIN
  END swap;

PROCEDURE pop  (u: U;  t: Type) =
  (* pop(1) (i.e. discard s0) *)
  BEGIN
  END pop;

PROCEDURE copy_n (u: U;  z: IType;  t: MType;  overlap: BOOLEAN) =
  (* Mem[s2.A:s0.z] := Mem[s1.A:s0.z]; pop(3)*)
  BEGIN
  END copy_n;

PROCEDURE copy (u: U;  n: INTEGER;  t: MType;  overlap: BOOLEAN) =
  (* Mem[s2.A:sz] := Mem[s1.A:sz]; pop(2)*)
  BEGIN
  END copy;

PROCEDURE zero_n (u: U;  z: IType;  t: MType) =
  (* Mem[s1.A:s0.z] := 0; pop(2) *)
  BEGIN
  END zero_n;

PROCEDURE zero (u: U;  n: INTEGER;  t: MType) =
  (* Mem[s1.A:sz] := 0; pop(1) *)
  BEGIN
  END zero;

<*NOWARN*>PROCEDURE loophole (u: U;  from, two: ZType) = BEGIN END loophole;
<*NOWARN*>PROCEDURE abort (u: U;  code: RuntimeError) = BEGIN END abort;
<*NOWARN*>PROCEDURE check_nil (u: U;  code: RuntimeError) = BEGIN END check_nil;
<*NOWARN*>PROCEDURE check_lo (u: U;  t: IType;  READONLY i: Target.Int;  code: RuntimeError) = BEGIN END check_lo;
<*NOWARN*>PROCEDURE check_hi (u: U;  t: IType;  READONLY i: Target.Int;  code: RuntimeError) = BEGIN END check_hi;
<*NOWARN*>PROCEDURE check_range (u: U;  t: IType;  READONLY a, b: Target.Int;  code: RuntimeError) = BEGIN END check_range;
<*NOWARN*>PROCEDURE check_index (u: U;  t: IType;  code: RuntimeError) = BEGIN END check_index;
<*NOWARN*>PROCEDURE check_eq (u: U;  t: IType;  code: RuntimeError) = BEGIN END check_eq;
<*NOWARN*>PROCEDURE add_offset (u: U;  i: INTEGER) = BEGIN END add_offset;
<*NOWARN*>PROCEDURE index_address (u: U;  t: IType;  size: INTEGER) = BEGIN END index_address;
<*NOWARN*>PROCEDURE start_call_direct (u: U;  p: Proc;  lev: INTEGER;  t: Type) = BEGIN END start_call_direct;
<*NOWARN*>PROCEDURE start_call_indirect (u: U;  t: Type;  cc: CallingConvention) = BEGIN END start_call_indirect;
<*NOWARN*>PROCEDURE pop_param (u: U;  t: MType) = BEGIN END pop_param;
<*NOWARN*>PROCEDURE pop_struct (u: U;  t: TypeUID;  s: ByteSize;  a: Alignment) = BEGIN END pop_struct;
<*NOWARN*>PROCEDURE pop_static_link (u: U) = BEGIN END pop_static_link;
<*NOWARN*>PROCEDURE call_direct (u: U; p: Proc;  t: Type) = BEGIN END call_direct;
<*NOWARN*>PROCEDURE call_indirect (u: U;  t: Type;  cc: CallingConvention) = BEGIN END call_indirect;
<*NOWARN*>PROCEDURE load_procedure (u: U;  p: Proc) = BEGIN END load_procedure;
<*NOWARN*>PROCEDURE load_static_link (u: U;  p: Proc) = BEGIN END load_static_link;
<*NOWARN*>PROCEDURE comment (u: U;  a, b, c, d: TEXT := NIL) = BEGIN END comment;
<*NOWARN*>PROCEDURE store_ordered (u: U;  t: ZType;  z: MType;  order: MemoryOrder) = BEGIN END store_ordered;
<*NOWARN*>PROCEDURE load_ordered (u: U;  t: MType;  z: ZType;  order: MemoryOrder) = BEGIN END load_ordered;
<*NOWARN*>PROCEDURE exchange (u: U;  t: MType;  z: ZType;  order: MemoryOrder) = BEGIN END exchange;
<*NOWARN*>PROCEDURE compare_exchange (u: U;  t: MType;  z: ZType;  r: IType; success, failure: MemoryOrder) = BEGIN END compare_exchange;
<*NOWARN*>PROCEDURE fence (u: U;  order: MemoryOrder) = BEGIN END fence;
<*NOWARN*>PROCEDURE fetch_and_op (u: U;  op: AtomicOp;  t: MType;  z: ZType; order: MemoryOrder) = BEGIN END fetch_and_op;

BEGIN
END M3CG_DoNothing.
