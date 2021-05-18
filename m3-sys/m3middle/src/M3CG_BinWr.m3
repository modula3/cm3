(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE M3CG_BinWr;

IMPORT Wr, Text, IntRefTbl, Word;
IMPORT M3Buf, M3ID, M3CG, M3CG_Ops, M3CG_Binary;
IMPORT Target, TInt AS TargetInt, TFloat, TWord;

FROM M3CG IMPORT Name, ByteOffset, TypeUID, CallingConvention;
FROM M3CG IMPORT BitSize, ByteSize, Alignment, Frequency;
FROM M3CG IMPORT Var, Proc, Label, Sign, BitOffset;
FROM M3CG IMPORT Type, ZType, AType, RType, IType, MType;
FROM M3CG IMPORT CompareOp, ConvertOp, AtomicOp, RuntimeError;
FROM M3CG IMPORT MemoryOrder;

TYPE Bop    = M3CG_Binary.Op;
TYPE WrVar  = Var    OBJECT tag: INTEGER END;
TYPE WrProc = Proc   OBJECT tag: INTEGER END;

TYPE
  RuntimeHook = REF RECORD
    name   : Name;
    proc   : Proc       := NIL;
    var    : Var        := NIL;
    offset : ByteOffset := 0;
  END;

TYPE
  U = M3CG.T OBJECT
        wr            : Wr.T := NIL;
        buf           : M3Buf.T := NIL;
        buf_len       : INTEGER := 0;
        runtime       : IntRefTbl.T := NIL;  (* Name -> RuntimeHook *)
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

(*------------------------------------------------------------------- I/O ---*)

PROCEDURE Cmd (u: U; cmd: Bop) =
  BEGIN
    OutB (u, ORD (cmd));
  END Cmd;

PROCEDURE ZName (u: U;  n: Name) =
  BEGIN
    IF (n = M3ID.NoID)
      THEN OutI (u, -1);
      ELSE Txt (u, M3ID.ToText (n));
    END;
  END ZName;

PROCEDURE VName (u: U;  v: Var) =
  BEGIN
    TYPECASE v OF
    | NULL     => OutI (u, 0);
    | WrVar(x) => OutI (u, x.tag);
    ELSE <*ASSERT FALSE*>
    END;
  END VName;

PROCEDURE PName (u: U;  p: Proc) =
  BEGIN
    TYPECASE p OF
    | NULL      => OutI (u, 0);
    | WrProc(x) => OutI (u, x.tag);
    ELSE <*ASSERT FALSE*>
    END;
  END PName;

PROCEDURE TName (u: U;  t: Type) =
  BEGIN
    OutB (u, ORD (t));
  END TName;

PROCEDURE Flt (u: U;  READONLY f: Target.Float) =
  VAR
    buf : ARRAY [0..BYTESIZE (EXTENDED)] OF TFloat.Byte;
    len := TFloat.ToBytes (f, buf);
  BEGIN
    OutB (u, ORD (TFloat.Prec (f)));
    FOR i := 0 TO len-1 DO
      OutB (u, buf[i]);
    END;
  END Flt;

PROCEDURE Bool (u: U;  b: BOOLEAN) =
  BEGIN
    OutB (u, ORD (b));
  END Bool;

PROCEDURE Lab (u: U;  i: Label) =
  BEGIN
    OutI (u, i);
  END Lab;

PROCEDURE Tipe (u: U;  t: TypeUID) =
  BEGIN
    OutI (u, t);
  END Tipe;

PROCEDURE Int (u: U;  i: INTEGER) =
  BEGIN
    OutI (u, i);
  END Int;

PROCEDURE TInt (u: U;  READONLY i: Target.Int) =
  VAR x: INTEGER;
  BEGIN
    IF TargetInt.ToInt (i, x)
      THEN OutI (u, x);
      ELSE AddBigX (u, i);
    END;
  END TInt;

PROCEDURE BInt (u: U;  i: INTEGER) =
  BEGIN
    OutI (u, i);
  END BInt;

PROCEDURE Txt (u: U;  t: TEXT) =
  VAR len: INTEGER;
  BEGIN
    IF (t = NIL) THEN
      OutI (u, -1);
    ELSE
      len := Text.Length (t);
      OutI (u, len);
      M3Buf.PutText (u.buf, t);
      INC (u.buf_len, len);
      IF (u.buf_len >= 16_F000) THEN Flush (u) END;
    END;
  END Txt;

(*--------------------------------------------------------- low level I/O ---*)

PROCEDURE Flush (u: U) =
  BEGIN
    M3Buf.Flush (u.buf, u.wr);
    u.buf_len := 0;
  END Flush;

PROCEDURE OutB  (u: U;  i: [0..255]) =
  BEGIN
    M3Buf.PutChar (u.buf, VAL (i, CHAR));
    INC (u.buf_len);
    IF (u.buf_len >= 16_F000) THEN Flush (u) END;
  END OutB;

PROCEDURE OutI  (u: U;  i: INTEGER) =
  BEGIN
    IF (0 <= i) AND (i <= M3CG_Binary.LastRegular) THEN
      OutB (u, i);
    ELSIF (i < 0) THEN
      IF (-255 <= i) THEN
        i := -i;
        OutB (u, M3CG_Binary.NInt1);
        OutB (u, i);
      ELSIF (i >= -16_FFFF) THEN
        i := -i;
        OutB (u, M3CG_Binary.NInt2);
        OutB (u, Word.And (i, 16_FF));
        OutB (u, Word.And (Word.RightShift (i, 8), 16_FF));
      ELSE
        AddBigInt (u, i);
      END;
    ELSE (* i > 0 *)
      IF (i <= 255) THEN
        OutB (u, M3CG_Binary.Int1);
        OutB (u, i);
      ELSIF (i <= 16_FFFF) THEN
        OutB (u, M3CG_Binary.Int2);
        OutB (u, Word.And (i, 16_FF));
        OutB (u, Word.And (Word.RightShift (i, 8), 16_FF));
      ELSE
        AddBigInt (u, i);
      END;
    END;
  END OutI;

TYPE
  IntDesc = RECORD
    negative : BOOLEAN;
    n_bytes  : CARDINAL;
    bytes    : ARRAY [0..7] OF [0..255];
  END;

PROCEDURE AddBigInt (u: U;  i: INTEGER) =
  VAR z: IntDesc;
  BEGIN
    z.negative := (i < 0);
    IF (i < 0) THEN i := Word.Minus (0, i); END;
    z.n_bytes := 0;
    WHILE (i # 0) DO
      z.bytes[z.n_bytes] := Word.And (i, 16_ff);  INC (z.n_bytes);
      i := Word.RightShift (i, 8);
    END;
    DumpInt (u, z);
  END AddBigInt;

PROCEDURE AddBigX (u: U;  READONLY i: Target.Int) =
  VAR n := i;  z: IntDesc;
  BEGIN
    z.negative := TargetInt.LT (n, TargetInt.Zero);
    IF (z.negative) THEN TWord.Subtract (TargetInt.Zero, i, n); END;
    z.n_bytes := TargetInt.ToUnsignedBytes (n, z.bytes);
    DumpInt (u, z);
  END AddBigX;

PROCEDURE DumpInt (u: U;  VAR i: IntDesc) =
  CONST
    Tag4 = ARRAY BOOLEAN OF [0..255] { M3CG_Binary.NInt4, M3CG_Binary.Int4 };
    Tag8 = ARRAY BOOLEAN OF [0..255] { M3CG_Binary.NInt8, M3CG_Binary.Int8 };
  VAR tag: [0..255];  cnt := i.n_bytes;
  BEGIN
    (* zero fill the rest of the bytes *)
    FOR x := cnt TO LAST(i.bytes) DO i.bytes[x] := 0; END;

    (* select the encoding *)
    IF (cnt <= 4)
      THEN  tag := Tag4 [NOT i.negative];  cnt := 4;
      ELSE  tag := Tag8 [NOT i.negative];  cnt := 8;
    END;

    (* finally, dump the bytes *)
    OutB (u, tag);
    FOR x := 0 TO cnt-1 DO
      OutB (u, i.bytes[x]);
    END;
  END DumpInt;

(*---------------------------------------------------------------------------*)

PROCEDURE New (output: Wr.T): M3CG.T =
  VAR mbuf := M3Buf.New ();
  BEGIN
    M3Buf.AttachDrain (mbuf, output);
    RETURN NEW (U, wr := output, buf := mbuf, buf_len := 0,
                runtime := NEW (IntRefTbl.Default).init (20));
  END New;

(*----------------------------------------------------------- ID counters ---*)

PROCEDURE next_label (u: U;  n: INTEGER := 1): Label =
  VAR x := u.next_label_id;
  BEGIN
    INC (u.next_label_id, n);
    RETURN x;
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
    Int (u, M3CG_Binary.Version);
    Cmd (u, Bop.begin_unit);
    Int (u, optimize);
  END begin_unit;

PROCEDURE end_unit (u: U) =
  (* called after all other methods to finalize the unit and write the
     resulting object *)
  BEGIN
    Cmd   (u, Bop.end_unit);
    Flush (u);
  END end_unit;

PROCEDURE import_unit (u: U;  n: Name) =
  (* note that the current compilation unit imports the interface 'n' *)
  BEGIN
    Cmd   (u, Bop.import_unit);
    ZName (u, n);
  END import_unit;

PROCEDURE export_unit (u: U;  n: Name) =
  (* note that the current compilation unit exports the interface 'n' *)
  BEGIN
    Cmd   (u, Bop.export_unit);
    ZName (u, n);
  END export_unit;

(*------------------------------------------------ debugging line numbers ---*)

PROCEDURE set_source_file (u: U; file: TEXT) =
  (* Sets the current source file name.  Subsequent statements
     and expressions are associated with this source location. *)
  BEGIN
    Cmd  (u, Bop.set_source_file);
    Txt  (u, file);
  END set_source_file;

PROCEDURE set_source_line (u: U; line: INTEGER) =
  (* Sets the current source line number.  Subsequent statements
   and expressions are associated with this source location. *)
  BEGIN
    Cmd (u, Bop.set_source_line);
    Int (u, line);
  END set_source_line;

(*------------------------------------------- debugging type declarations ---*)

PROCEDURE declare_typename (u: U;  t: TypeUID;  n: Name) =
  BEGIN
    Cmd   (u, Bop.declare_typename);
    Tipe  (u, t);
    ZName (u, n);
  END declare_typename;

PROCEDURE declare_array (u: U;  t, index, elt: TypeUID;  s: BitSize) =
  BEGIN
    Cmd  (u, Bop.declare_array);
    Tipe (u, t);
    Tipe (u, index);
    Tipe (u, elt);
    BInt (u, s);
  END declare_array;

PROCEDURE declare_open_array (u: U;  t, elt: TypeUID;  s: BitSize) =
  BEGIN
    Cmd  (u, Bop.declare_open_array);
    Tipe (u, t);
    Tipe (u, elt);
    BInt (u, s);
  END declare_open_array;

PROCEDURE declare_enum (u: U;  t: TypeUID;  n_elts: INTEGER;  s: BitSize) =
  BEGIN
    Cmd  (u, Bop.declare_enum);
    Tipe (u, t);
    Int  (u, n_elts);
    BInt (u, s);
  END declare_enum;

PROCEDURE declare_enum_elt (u: U;  n: Name) =
  BEGIN
    Cmd   (u, Bop.declare_enum_elt);
    ZName (u, n);
  END declare_enum_elt;

PROCEDURE declare_packed  (u: U;  t: TypeUID;  s: BitSize;  base: TypeUID) =
  BEGIN
    Cmd  (u, Bop.declare_packed);
    Tipe (u, t);
    BInt (u, s);
    Tipe (u, base);
  END declare_packed;

PROCEDURE declare_record (u: U; t: TypeUID;  s: BitSize;
                          n_fields: INTEGER)=
  BEGIN
    Cmd  (u, Bop.declare_record);
    Tipe (u, t);
    BInt (u, s);
    Int  (u, n_fields);
  END declare_record;

PROCEDURE declare_field (u: U;  n: Name;  o: BitOffset;  s: BitSize;
                         t: TypeUID)=
  BEGIN
    Cmd   (u, Bop.declare_field);
    ZName (u, n);
    BInt  (u, o);
    BInt  (u, s);
    Tipe  (u, t);
  END declare_field;

PROCEDURE declare_set (u: U;  t, domain: TypeUID;  s: BitSize) =
  BEGIN
    Cmd  (u, Bop.declare_set);
    Tipe (u, t);
    Tipe (u, domain);
    BInt (u, s);
  END declare_set;

PROCEDURE declare_subrange (u: U; t, domain: TypeUID;
                            READONLY min, max: Target.Int;
                            s: BitSize) =
  BEGIN
    Cmd  (u, Bop.declare_subrange);
    Tipe (u, t);
    Tipe (u, domain);
    TInt (u, min);
    TInt (u, max);
    BInt (u, s);
  END declare_subrange;

PROCEDURE declare_pointer (u: U;  t, target: TypeUID;  brand: TEXT;
                           traced: BOOLEAN) =
  BEGIN
    Cmd  (u, Bop.declare_pointer);
    Tipe (u, t);
    Tipe (u, target);
    Txt  (u, brand);
    Bool (u, traced);
  END declare_pointer;


PROCEDURE declare_indirect (u: U;  t, target: TypeUID) =
  BEGIN
    Cmd  (u, Bop.declare_indirect);
    Tipe (u, t);
    Tipe (u, target);
  END declare_indirect;


PROCEDURE declare_proctype (u: U;  t: TypeUID;  n_formals: INTEGER;
                            result: TypeUID;  n_raises: INTEGER;
                            cc: CallingConvention) =
  BEGIN
    Cmd  (u, Bop.declare_proctype);
    Tipe (u, t);
    Int  (u, n_formals);
    Tipe (u, result);
    Int  (u, n_raises);
    OutB (u, cc.m3cg_id);
  END declare_proctype;

PROCEDURE declare_formal (u: U;  n: Name;  t: TypeUID) =
  BEGIN
    Cmd   (u, Bop.declare_formal);
    ZName (u, n);
    Tipe  (u, t);
  END declare_formal;

PROCEDURE declare_raises (u: U;  n: Name) =
  BEGIN
    Cmd   (u, Bop.declare_raises);
    ZName (u, n);
  END declare_raises;


PROCEDURE declare_object (u: U;  t, super: TypeUID;
                          brand: TEXT;  traced: BOOLEAN;
                          n_fields, n_methods: INTEGER;
                          field_size: BitSize) =
  BEGIN
    Cmd  (u, Bop.declare_object);
    Tipe (u, t);
    Tipe (u, super);
    Txt  (u, brand);
    Bool (u, traced);
    Int  (u, n_fields);
    Int  (u, n_methods);
    BInt (u, field_size);
  END declare_object;

PROCEDURE declare_method (u: U;  n: Name;  signature: TypeUID) =
  BEGIN
    Cmd   (u, Bop.declare_method);
    ZName (u, n);
    Tipe  (u, signature);
  END declare_method;

PROCEDURE declare_opaque (u: U;  t, super: TypeUID) =
  BEGIN
    Cmd   (u, Bop.declare_opaque);
    Tipe  (u, t);
    Tipe  (u, super);
  END declare_opaque;

PROCEDURE reveal_opaque (u: U;  lhs, rhs: TypeUID) =
  BEGIN
    Cmd   (u, Bop.reveal_opaque);
    Tipe  (u, lhs);
    Tipe  (u, rhs);
  END reveal_opaque;

PROCEDURE declare_exception (u: U;  n: Name;  arg_type: TypeUID;
                           raise_proc: BOOLEAN;  base: Var;  offset: INTEGER) =
  BEGIN
    Cmd   (u, Bop.declare_exception);
    ZName (u, n);
    Tipe  (u, arg_type);
    Bool  (u, raise_proc);
    VName (u, base);
    Int   (u, offset);
  END declare_exception;

PROCEDURE widechar_size (u: U;  size: INTEGER) =
  BEGIN
    Cmd (u, Bop.widechar_size);
    Int (u, size);
  END widechar_size;

(*--------------------------------------------------------- runtime hooks ---*)

PROCEDURE GetRuntimeHook (u: U;  n: Name): RuntimeHook =
  VAR ref: REFANY;  e: RuntimeHook;
  BEGIN
    IF u.runtime.get (n, ref) THEN
      e := ref;
    ELSE
      e := NEW (RuntimeHook, name := n, proc := NIL, var := NIL, offset := 0);
      EVAL u.runtime.put (n, e);
    END;
    RETURN e;
  END GetRuntimeHook;

PROCEDURE set_runtime_proc (u: U;  n: Name;  p: Proc) =
  VAR e := GetRuntimeHook (u, n);
  BEGIN
    e.proc := p;
    Cmd   (u, Bop.set_runtime_proc);
    ZName (u, n);
    PName (u, p);
  END set_runtime_proc;

(*------------------------------------------------- variable declarations ---*)

PROCEDURE NewVar (u: U): Var =
  VAR v := NEW (WrVar, tag := u.next_var);
  BEGIN
    INC (u.next_var);
    RETURN v;
  END NewVar;

PROCEDURE import_global (u: U;  n: Name;  s: ByteSize;  a: Alignment;
                         t: Type;  m3t: TypeUID): Var =
  VAR v := NewVar (u);
  BEGIN
    Cmd   (u, Bop.import_global);
    ZName (u, n);
    Int   (u, s);
    Int   (u, a);
    TName (u, t);
    Tipe  (u, m3t);
    VName (u, v);
    RETURN v;
  END import_global;

PROCEDURE declare_segment (u: U;  n: Name;  m3t: TypeUID;  is_const: BOOLEAN): Var =
  VAR v := NewVar (u);
  BEGIN
    Cmd   (u, Bop.declare_segment);
    ZName (u, n);
    Tipe  (u, m3t);
    Bool  (u, is_const);
    VName (u, v);
    RETURN v;
  END declare_segment;

PROCEDURE bind_segment (u: U;  seg: Var;  s: ByteSize;  a: Alignment;
                        t: Type;  exported, inited: BOOLEAN) =
  BEGIN
    Cmd   (u, Bop.bind_segment);
    VName (u, seg);
    Int   (u, s);
    Int   (u, a);
    TName (u, t);
    Bool  (u, exported);
    Bool  (u, inited);
  END bind_segment;

PROCEDURE declare_global (u: U;  n: Name;  s: ByteSize;  a: Alignment;
                     t: Type;  m3t: TypeUID;  exported, inited: BOOLEAN): Var =
  VAR v := NewVar (u);
  BEGIN
    Cmd   (u, Bop.declare_global);
    ZName (u, n);
    Int   (u, s);
    Int   (u, a);
    TName (u, t);
    Tipe  (u, m3t);
    Bool  (u, exported);
    Bool  (u, inited);
    VName (u, v);
    RETURN v;
  END declare_global;

PROCEDURE declare_constant (u: U;  n: Name;  s: ByteSize;  a: Alignment;
                     t: Type;  m3t: TypeUID;  exported, inited: BOOLEAN): Var =
  VAR v := NewVar (u);
  BEGIN
    Cmd   (u, Bop.declare_constant);
    ZName (u, n);
    Int   (u, s);
    Int   (u, a);
    TName (u, t);
    Tipe  (u, m3t);
    Bool  (u, exported);
    Bool  (u, inited);
    VName (u, v);
    RETURN v;
  END declare_constant;

PROCEDURE declare_local (u: U;  n: Name;  s: ByteSize;  a: Alignment;
                         t: Type;  m3t: TypeUID;  in_memory, up_level: BOOLEAN;
                         f: Frequency): Var =
  VAR v := NewVar (u);
  BEGIN
    Cmd   (u, Bop.declare_local);
    ZName (u, n);
    Int   (u, s);
    Int   (u, a);
    TName (u, t);
    Tipe  (u, m3t);
    Bool  (u, in_memory);
    Bool  (u, up_level);
    Int   (u, f);
    VName (u, v);
    RETURN v;
  END declare_local;

PROCEDURE declare_param (u: U;  n: Name;  s: ByteSize;  a: Alignment;
                         t: Type;  m3t: TypeUID;  in_memory, up_level: BOOLEAN;
                         f: Frequency; <*UNUSED*>typename := M3CG.NoQID): Var =
  VAR v := NewVar (u);
  BEGIN
    Cmd   (u, Bop.declare_param);
    ZName (u, n);
    Int   (u, s);
    Int   (u, a);
    TName (u, t);
    Tipe  (u, m3t);
    Bool  (u, in_memory);
    Bool  (u, up_level);
    Int   (u, f);
    VName (u, v);
    (* TODO typename but it is not used downstream and can be omitted indefinitely *)
    RETURN v;
  END declare_param;

PROCEDURE declare_temp   (u: U;  s: ByteSize;  a: Alignment;  t: Type;
                          in_memory:BOOLEAN): Var =
  VAR v := NewVar (u);
  BEGIN
    Cmd   (u, Bop.declare_temp);
    Int   (u, s);
    Int   (u, a);
    TName (u, t);
    Bool  (u, in_memory);
    VName (u, v);
    RETURN v;
  END declare_temp;

PROCEDURE free_temp (u: U;  v: Var) =
  BEGIN
    Cmd   (u, Bop.free_temp);
    VName (u, v);
  END free_temp;

(*---------------------------------------- static variable initialization ---*)

PROCEDURE begin_init (u: U;  v: Var) =
  BEGIN
    Cmd   (u, Bop.begin_init);
    VName (u, v);
  END begin_init;

PROCEDURE end_init (u: U;  v: Var) =
  BEGIN
    Cmd   (u, Bop.end_init);
    VName (u, v);
  END end_init;

PROCEDURE init_int (u: U;  o: ByteOffset;  READONLY value: Target.Int;
                    t: Type) =
  BEGIN
    Cmd   (u, Bop.init_int);
    Int   (u, o);
    TInt  (u, value);
    TName (u, t);
  END init_int;

PROCEDURE init_proc (u: U;  o: ByteOffset;  value: Proc) =
  BEGIN
    Cmd   (u, Bop.init_proc);
    Int   (u, o);
    PName (u, value);
  END init_proc;

PROCEDURE init_label (u: U;  o: ByteOffset;  value: Label) =
  BEGIN
    Cmd   (u, Bop.init_label);
    Int   (u, o);
    Lab   (u, value);
  END init_label;

PROCEDURE init_var (u: U;  o: ByteOffset;  value: Var;  bias: ByteOffset) =
  BEGIN
    Cmd   (u, Bop.init_var);
    Int   (u, o);
    VName (u, value);
    Int   (u, bias);
  END init_var;

PROCEDURE init_offset (u: U;  o: ByteOffset;  value: Var) =
  BEGIN
    Cmd   (u, Bop.init_offset);
    Int   (u, o);
    VName (u, value);
  END init_offset;

PROCEDURE init_chars (u: U;  o: ByteOffset;  value: TEXT) =
  BEGIN
    Cmd   (u, Bop.init_chars);
    Int   (u, o);
    Txt   (u, value);
  END init_chars;

PROCEDURE init_float (u: U;  o: ByteOffset;  READONLY f: Target.Float) =
  BEGIN
    Cmd   (u, Bop.init_float);
    Int   (u, o);
    Flt   (u, f);
  END init_float;

(*------------------------------------------------------------ procedures ---*)

PROCEDURE NewProc (u: U): Proc =
  VAR p := NEW (WrProc, tag := u.next_proc);
  BEGIN
    INC (u.next_proc);
    RETURN p;
  END NewProc;

PROCEDURE import_procedure (u: U;  n: Name;  n_params: INTEGER;
                            ret_type: Type; cc: CallingConvention;
                            <*UNUSED*>return_typename := M3CG.NoQID): Proc =
  VAR p := NewProc (u);
  BEGIN
    Cmd   (u, Bop.import_procedure);
    ZName (u, n);
    Int   (u, n_params);
    TName (u, ret_type);
    OutB  (u, cc.m3cg_id);
    PName (u, p);
    RETURN p;
  END import_procedure;

PROCEDURE declare_procedure (u: U;  n: Name;  n_params: INTEGER;
                             return_type: Type;  lev: INTEGER;
                             cc: CallingConvention; exported: BOOLEAN;
                             parent: Proc; <*UNUSED*>return_typename := M3CG.NoQID): Proc =
  VAR p := NewProc (u);
  BEGIN
    Cmd   (u, Bop.declare_procedure);
    ZName (u, n);
    Int   (u, n_params);
    TName (u, return_type);
    Int   (u, lev);
    OutB  (u, cc.m3cg_id);
    Bool  (u, exported);
    PName (u, parent);
    PName (u, p);
    (* TODO return_typename but it is not used downstream and can be omitted indefinitely *)
    RETURN p;
  END declare_procedure;

PROCEDURE begin_procedure (u: U;  p: Proc) =
  BEGIN
    Cmd   (u, Bop.begin_procedure);
    PName (u, p);
  END begin_procedure;


PROCEDURE end_procedure (u: U;  p: Proc) =
  BEGIN
    Cmd   (u, Bop.end_procedure);
    PName (u, p);
  END end_procedure;

PROCEDURE begin_block (u: U) =
  (* marks the beginning of a nested anonymous block *)
  BEGIN
    Cmd   (u, Bop.begin_block);
  END begin_block;

PROCEDURE end_block (u: U) =
  (* marks the ending of a nested anonymous block *)
  BEGIN
    Cmd   (u, Bop.end_block);
  END end_block;

PROCEDURE note_procedure_origin (u: U;  p: Proc) =
  BEGIN
    Cmd   (u, Bop.note_procedure_origin);
    PName (u, p);
  END note_procedure_origin;

(*------------------------------------------------------------ statements ---*)

PROCEDURE set_label (u: U;  l: Label;  barrier: BOOLEAN) =
  (* define 'l' to be at the current pc *)
  BEGIN
    Cmd   (u, Bop.set_label);
    Lab   (u, l);
    Bool  (u, barrier);
  END set_label;

PROCEDURE jump (u: U; l: Label) =
  (* GOTO l *)
  BEGIN
    Cmd   (u, Bop.jump);
    Lab   (u, l);
  END jump;

PROCEDURE if_true  (u: U;  t: IType;  l: Label;  f: Frequency) =
  (* IF (s0.t # 0) GOTO l ; pop *)
  BEGIN
    Cmd   (u, Bop.if_true);
    TName (u, t);
    Lab   (u, l);
    Int   (u, f);
  END if_true;

PROCEDURE if_false (u: U;  t: IType;  l: Label;  f: Frequency) =
  (* IF (s0.t = 0) GOTO l ; pop *)
  BEGIN
    Cmd   (u, Bop.if_false);
    TName (u, t);
    Lab   (u, l);
    Int   (u, f);
  END if_false;

PROCEDURE if_compare (u: U;  t: ZType;  op: CompareOp;  l: Label;  f: Frequency) =
  (* IF (s1.t  op  s0.t) GOTO l ; pop(2) *)
  CONST OpName = ARRAY CompareOp OF Bop {
            Bop.if_eq, Bop.if_ne, Bop.if_gt, Bop.if_ge, Bop.if_lt, Bop.if_le };
  BEGIN
    Cmd   (u, OpName [op]);
    TName (u, t);
    Lab   (u, l);
    Int   (u, f);
  END if_compare;

PROCEDURE case_jump (u: U;  t: IType;  READONLY labels: ARRAY OF Label) =
  (* "GOTO labels[s0.t] ; pop" with no range checking on s0.t *)
  BEGIN
    Cmd   (u, Bop.case_jump);
    TName (u, t);
    Int   (u, NUMBER(labels));
    FOR i := FIRST (labels) TO LAST (labels) DO  Lab (u, labels [i]);  END;
  END case_jump;

PROCEDURE exit_proc (u: U; t: Type) =
  (* Returns s0.t if the stack is non-empty, otherwise returns no value. *)
  BEGIN
    Cmd   (u, Bop.exit_proc);
    TName (u, t);
  END exit_proc;

(*------------------------------------------------------------ load/store ---*)

PROCEDURE load  (u: U;  v: Var;  o: ByteOffset;  t: MType;  z: ZType) =
  BEGIN
    Cmd   (u, Bop.load);
    VName (u, v);
    Int   (u, o);
    TName (u, t);
    TName (u, z);
  END load;

PROCEDURE store  (u: U;  v: Var;  o: ByteOffset;  t: ZType;  z: MType) =
  BEGIN
    Cmd   (u, Bop.store);
    VName (u, v);
    Int   (u, o);
    TName (u, t);
    TName (u, z);
  END store;

PROCEDURE load_address (u: U;  v: Var;  o: ByteOffset) =
  BEGIN
    Cmd   (u, Bop.load_address);
    VName (u, v);
    Int   (u, o);
  END load_address;

PROCEDURE load_indirect (u: U;  o: ByteOffset;  t: MType;  z: ZType) =
  BEGIN
    Cmd   (u, Bop.load_indirect);
    Int   (u, o);
    TName (u, t);
    TName (u, z);
  END load_indirect;

PROCEDURE store_indirect (u: U;  o: ByteOffset;  t: ZType;  z: MType) =
  BEGIN
    Cmd   (u, Bop.store_indirect);
    Int   (u, o);
    TName (u, t);
    TName (u, z);
  END store_indirect;


(*-------------------------------------------------------------- literals ---*)

PROCEDURE load_nil (u: U) =
  (* push ; s0.A := a *)
  BEGIN
    Cmd   (u, Bop.load_nil);
  END load_nil;

PROCEDURE load_integer  (u: U;  t: IType;  READONLY i: Target.Int) =
  (* push ; s0.t:= i *)
  BEGIN
    Cmd   (u, Bop.load_integer);
    TName (u, t);
    TInt  (u, i);
  END load_integer;

PROCEDURE load_float    (u: U;  t: RType;  READONLY f: Target.Float) =
  (* push ; s0.t := f *)
  BEGIN
    Cmd   (u, Bop.load_float);
    TName (u, t);
    Flt   (u, f);
  END load_float;

(*------------------------------------------------------------ arithmetic ---*)

PROCEDURE compare (u: U;  t: ZType;  z: IType;  op: CompareOp) =
  (* s1.z := (s1.t  op  s0.t)  ; pop *)
  CONST OpName = ARRAY CompareOp OF Bop {
                   Bop.eq, Bop.ne, Bop.gt, Bop.ge, Bop.lt, Bop.le };
  BEGIN
    Cmd   (u, OpName [op]);
    TName (u, t);
    TName (u, z);
  END compare;

PROCEDURE add (u: U;  t: AType) =
  (* s1.t := s1.t + s0.t ; pop *)
  BEGIN
    Cmd   (u, Bop.add);
    TName (u, t);
  END add;

PROCEDURE subtract (u: U;  t: AType) =
  (* s1.t := s1.t - s0.t ; pop *)
  BEGIN
    Cmd   (u, Bop.subtract);
    TName (u, t);
  END subtract;

PROCEDURE multiply (u: U;  t: AType) =
  (* s1.t := s1.t * s0.t ; pop *)
  BEGIN
    Cmd   (u, Bop.multiply);
    TName (u, t);
  END multiply;

PROCEDURE divide (u: U;  t: RType) =
  (* s1.t := s1.t / s0.t ; pop *)
  BEGIN
    Cmd   (u, Bop.divide);
    TName (u, t);
  END divide;

PROCEDURE div (u: U;  t: IType;  a, b: Sign) =
  (* s1.t := s1.t DIV s0.t ; pop *)
  BEGIN
    Cmd   (u, Bop.div);
    TName (u, t);
    OutB  (u, ORD (a));
    OutB  (u, ORD (b));
  END div;

PROCEDURE mod (u: U;  t: IType;  a, b: Sign) =
  (* s1.t := s1.t MOD s0.t ; pop *)
  BEGIN
    Cmd   (u, Bop.mod);
    TName (u, t);
    OutB  (u, ORD (a));
    OutB  (u, ORD (b));
  END mod;

PROCEDURE negate (u: U;  t: AType) =
  (* s0.t := - s0.t *)
  BEGIN
    Cmd   (u, Bop.negate);
    TName (u, t);
  END negate;

PROCEDURE abs      (u: U;  t: AType) =
  (* s0.t := ABS (s0.t) (noop on Words) *)
  BEGIN
    Cmd   (u, Bop.abs);
    TName (u, t);
  END abs;

PROCEDURE max      (u: U;  t: ZType) =
  (* s1.t := MAX (s1.t, s0.t) ; pop *)
  BEGIN
    Cmd   (u, Bop.max);
    TName (u, t);
  END max;

PROCEDURE min      (u: U;  t: ZType) =
  (* s1.t := MIN (s1.t, s0.t) ; pop *)
  BEGIN
    Cmd   (u, Bop.min);
    TName (u, t);
  END min;

PROCEDURE cvt_int (u: U;  t: RType;  x: IType;  op: ConvertOp) =
  (* s0.x := op (s0.t) *)
  CONST OpName = ARRAY ConvertOp OF Bop {
                   Bop.round, Bop.trunc, Bop.floor, Bop.ceiling } ;
  BEGIN
    Cmd   (u, OpName [op]);
    TName (u, t);
    TName (u, x);
  END cvt_int;

PROCEDURE cvt_float (u: U;  t: AType;  x: RType) =
  (* s0.x := FLOAT (s0.t, x) *)
  BEGIN
    Cmd   (u, Bop.cvt_float);
    TName (u, t);
    TName (u, x);
  END cvt_float;

(*------------------------------------------------------------------ sets ---*)

PROCEDURE set_union (u: U;  s: ByteSize) =
  (* s1.B := s1.B + s0.B ; pop *)
  BEGIN
    Cmd   (u, Bop.set_union);
    Int   (u, s);
  END set_union;

PROCEDURE set_difference (u: U;  s: ByteSize) =
  (* s1.B := s1.B - s0.B ; pop *)
  BEGIN
    Cmd   (u, Bop.set_difference);
    Int   (u, s);
  END set_difference;

PROCEDURE set_intersection (u: U;  s: ByteSize) =
  (* s1.B := s1.B * s0.B ; pop *)
  BEGIN
    Cmd   (u, Bop.set_intersection);
    Int   (u, s);
  END set_intersection;

PROCEDURE set_sym_difference (u: U;  s: ByteSize) =
  (* s1.B := s1.B / s0.B ; pop *)
  BEGIN
    Cmd   (u, Bop.set_sym_difference);
    Int   (u, s);
  END set_sym_difference;

PROCEDURE set_member (u: U;  s: ByteSize;  t: IType) =
  (* s1.t := (s0.t IN s1.B) ; pop *)
  BEGIN
    Cmd   (u, Bop.set_member);
    Int   (u, s);
    TName (u, t);
  END set_member;

PROCEDURE set_compare (u: U;  s: ByteSize;  op: CompareOp;  t: IType) =
  (* s1.t := (s1.B  op  s0.B) ; pop *)
  CONST OpName = ARRAY CompareOp OF Bop {
          Bop.set_eq, Bop.set_ne, Bop.set_gt, Bop.set_ge, Bop.set_lt, Bop.set_le };
  BEGIN
    Cmd   (u, OpName [op]);
    Int   (u, s);
    TName (u, t);
  END set_compare;

PROCEDURE set_range (u: U;  s: ByteSize;  t: IType) =
  (* s2.A [s1.t .. s0.t] := 1's; pop(3)*)
  BEGIN
    Cmd   (u, Bop.set_range);
    Int   (u, s);
    TName (u, t);
  END set_range;

PROCEDURE set_singleton (u: U;  s: ByteSize;  t: IType) =
  (* s1.A [s0.t] := 1; pop(2) *)
  BEGIN
    Cmd   (u, Bop.set_singleton);
    Int   (u, s);
    TName (u, t);
  END set_singleton;

(*------------------------------------------------- Word.T bit operations ---*)

PROCEDURE not (u: U;  t: IType) =
  (* s0.t := Word.Not (s0.t) *)
  BEGIN
    Cmd   (u, Bop.not);
    TName (u, t);
  END not;

PROCEDURE and (u: U;  t: IType) =
  (* s1.t := Word.And (s1.t, s0.t) ; pop *)
  BEGIN
    Cmd   (u, Bop.and);
    TName (u, t);
  END and;

PROCEDURE or  (u: U;  t: IType) =
  (* s1.t := Word.Or  (s1.t, s0.t) ; pop *)
  BEGIN
    Cmd   (u, Bop.or);
    TName (u, t);
  END or;

PROCEDURE xor (u: U;  t: IType) =
  (* s1.t := Word.Xor (s1.t, s0.t) ; pop *)
  BEGIN
    Cmd   (u, Bop.xor);
    TName (u, t);
  END xor;

PROCEDURE shift (u: U;  t: IType) =
  (* s1.t := Word.Shift  (s1.t, s0.t) ; pop *)
  BEGIN
    Cmd   (u, Bop.shift);
    TName (u, t);
  END shift;

PROCEDURE shift_left   (u: U;  t: IType) =
  (* s1.t := Word.Shift  (s1.t, s0.t) ; pop *)
  BEGIN
    Cmd   (u, Bop.shift_left);
    TName (u, t);
  END shift_left;

PROCEDURE shift_right  (u: U;  t: IType) =
  (* s1.t := Word.Shift  (s1.t, -s0.t) ; pop *)
  BEGIN
    Cmd   (u, Bop.shift_right);
    TName (u, t);
  END shift_right;

PROCEDURE rotate (u: U;  t: IType) =
  (* s1.t := Word.Rotate (s1.t, s0.t) ; pop *)
  BEGIN
    Cmd   (u, Bop.rotate);
    TName (u, t);
  END rotate;

PROCEDURE rotate_left  (u: U;  t: IType) =
  (* s1.t := Word.Rotate (s1.t, s0.t) ; pop *)
  BEGIN
    Cmd   (u, Bop.rotate_left);
    TName (u, t);
  END rotate_left;

PROCEDURE rotate_right (u: U;  t: IType) =
  (* s1.t := Word.Rotate (s1.t, -s0.t) ; pop *)
  BEGIN
    Cmd   (u, Bop.rotate_right);
    TName (u, t);
  END rotate_right;

PROCEDURE widen (u: U;  sign: BOOLEAN) =
  (* s0.I64 := s0.I32;  IF sign THEN SignExtend s0;  *)
  BEGIN
    Cmd   (u, Bop.widen);
    Bool  (u, sign);
  END widen;

PROCEDURE chop (u: U) =
  (* s0.I32 := Word.And (s0.I64, 16_ffffffff);  *)
  BEGIN
    Cmd   (u, Bop.chop);
  END chop;

PROCEDURE extract (u: U;  t: IType;  sign: BOOLEAN) =
  (* s2.t := Word.Extract(s2.t, s1.t, s0.t);
     IF sign THEN SignExtend s2 END; pop(2) *)
  BEGIN
    Cmd   (u, Bop.extract);
    TName (u, t);
    Bool  (u, sign);
  END extract;

PROCEDURE extract_n (u: U;  t: IType;  sign: BOOLEAN;  n: CARDINAL) =
  (* s1.t := Word.Extract(s1.t, s0.t, n);
     IF sign THEN SignExtend s1 END; pop(1) *)
  BEGIN
    Cmd   (u, Bop.extract_n);
    TName (u, t);
    Bool  (u, sign);
    Int   (u, n);
  END extract_n;

PROCEDURE extract_mn (u: U;  t: IType;  sign: BOOLEAN;  m, n: CARDINAL) =
  (* s0.t := Word.Extract(s0.t, m, n);
     IF sign THEN SignExtend s0 END; *)
  BEGIN
    Cmd   (u, Bop.extract_mn);
    TName (u, t);
    Bool  (u, sign);
    Int   (u, m);
    Int   (u, n);
  END extract_mn;

PROCEDURE insert  (u: U;  t: IType) =
  (* s3.t := Word.Insert (s3.t, s2.t, s1.t, s0.t) ; pop(3) *)
  BEGIN
    Cmd   (u, Bop.insert);
    TName (u, t);
  END insert;

PROCEDURE insert_n  (u: U;  t: IType;  n: CARDINAL) =
  (* s2.t := Word.Insert (s2.t, s1.t, s0.t, n) ; pop(2) *)
  BEGIN
    Cmd   (u, Bop.insert_n);
    TName (u, t);
    Int   (u, n);
  END insert_n;

PROCEDURE insert_mn  (u: U;  t: IType;  m, n: CARDINAL) =
  (* s1.t := Word.Insert (s1.t, s0.t, m, n) ; pop(2) *)
  BEGIN
    Cmd   (u, Bop.insert_mn);
    TName (u, t);
    Int   (u, m);
    Int   (u, n);
  END insert_mn;

(*------------------------------------------------ misc. stack/memory ops ---*)

PROCEDURE swap (u: U;  a, b: Type) =
  (* tmp := s1 ; s1 := s0 ; s0 := tmp *)
  BEGIN
    Cmd   (u, Bop.swap);
    TName (u, a);
    TName (u, b);
  END swap;

PROCEDURE pop  (u: U;  t: Type) =
  (* pop(1) (i.e. discard s0) *)
  BEGIN
    Cmd   (u, Bop.pop);
    TName (u, t);
  END pop;

PROCEDURE copy_n (u: U;  z: IType;  t: MType;  overlap: BOOLEAN) =
  (* Mem[s2.A:s0.z] := Mem[s1.A:s0.z]; pop(3)*)
  BEGIN
    Cmd   (u, Bop.copy_n);
    TName (u, z);
    TName (u, t);
    Bool  (u, overlap);
  END copy_n;

PROCEDURE copy (u: U;  n: INTEGER;  t: MType;  overlap: BOOLEAN) =
  (* Mem[s2.A:sz] := Mem[s1.A:sz]; pop(2)*)
  BEGIN
    Cmd   (u, Bop.copy);
    Int   (u, n);
    TName (u, t);
    Bool  (u, overlap);
  END copy;

PROCEDURE zero_n (u: U;  z: IType;  t: MType) =
  (* Mem[s1.A:s0.z] := 0; pop(2) *)
  BEGIN
    Cmd   (u, Bop.zero_n);
    TName (u, z);
    TName (u, t);
  END zero_n;

PROCEDURE zero (u: U;  n: INTEGER;  t: MType) =
  (* Mem[s1.A:sz] := 0; pop(1) *)
  BEGIN
    Cmd   (u, Bop.zero);
    Int   (u, n);
    TName (u, t);
  END zero;

(*----------------------------------------------------------- conversions ---*)

PROCEDURE loophole (u: U;  from, two: ZType) =
  (* s0.to := LOOPHOLE(s0.from, to) *)
  BEGIN
    Cmd   (u, Bop.loophole);
    TName (u, from);
    TName (u, two);
  END loophole;

(*------------------------------------------------ traps & runtime checks ---*)

PROCEDURE abort (u: U;  code: RuntimeError) =
  BEGIN
    Cmd   (u, Bop.abort);
    Int   (u, ORD (code));
  END abort;

PROCEDURE check_nil (u: U;  code: RuntimeError) =
  (* IF (s0.A = NIL) THEN abort(code) *)
  BEGIN
    Cmd   (u, Bop.check_nil);
    Int   (u, ORD (code));
  END check_nil;

PROCEDURE check_lo (u: U;  t: IType;  READONLY i: Target.Int;  code: RuntimeError) =
  (* IF (s0.t < i) THEN abort(code) *)
  BEGIN
    Cmd   (u, Bop.check_lo);
    TName (u, t);
    TInt  (u, i);
    Int   (u, ORD (code));
  END check_lo;

PROCEDURE check_hi (u: U;  t: IType;  READONLY i: Target.Int;  code: RuntimeError) =
  (* IF (i < s0.t) THEN abort(code) *)
  BEGIN
    Cmd   (u, Bop.check_hi);
    TName (u, t);
    TInt  (u, i);
    Int   (u, ORD (code));
  END check_hi;

PROCEDURE check_range (u: U;  t: IType;  READONLY a, b: Target.Int;  code: RuntimeError) =
  (* IF (s0.t < a) OR (b < s0.t) THEN abort(code) *)
  BEGIN
    Cmd   (u, Bop.check_range);
    TName (u, t);
    TInt  (u, a);
    TInt  (u, b);
    Int   (u, ORD (code));
  END check_range;

PROCEDURE check_index (u: U;  t: IType;  code: RuntimeError) =
  BEGIN
    Cmd   (u, Bop.check_index);
    TName (u, t);
    Int   (u, ORD (code));
  END check_index;

PROCEDURE check_eq (u: U;  t: IType;  code: RuntimeError) =
  (* IF (s0.t # s1.t) THEN abort(code);  Pop (2) *)
  BEGIN
    Cmd   (u, Bop.check_eq);
    TName (u, t);
    Int   (u, ORD (code));
  END check_eq;

(*---------------------------------------------------- address arithmetic ---*)

PROCEDURE add_offset (u: U;  i: INTEGER) =
  (* s0.A := s0.A + i *)
  BEGIN
    Cmd   (u, Bop.add_offset);
    Int   (u, i);
  END add_offset;

PROCEDURE index_address (u: U;  t: IType;  size: INTEGER) =
  (* s1.A := s1.A + s0.t * size ; pop *)
  BEGIN
    Cmd   (u, Bop.index_address);
    TName (u, t);
    Int   (u, size);
  END index_address;

(*------------------------------------------------------- procedure calls ---*)

PROCEDURE start_call_direct (u: U;  p: Proc;  lev: INTEGER;  t: Type) =
  (* begin a procedure call to a procedure at static level 'lev'. *)
  BEGIN
    Cmd   (u, Bop.start_call_direct);
    PName (u, p);
    Int   (u, lev);
    TName (u, t);
  END start_call_direct;

PROCEDURE start_call_indirect (u: U;  t: Type;  cc: CallingConvention) =
  (* begin a procedure call to a procedure at static level 'lev'. *)
  BEGIN
    Cmd   (u, Bop.start_call_indirect);
    TName (u, t);
    OutB  (u, cc.m3cg_id);
  END start_call_indirect;

PROCEDURE pop_param (u: U;  t: MType) =
  (* pop s0 and make it the "next" paramter in the current call *)
  BEGIN
    Cmd   (u, Bop.pop_param);
    TName (u, t);
  END pop_param;

PROCEDURE pop_struct (u: U;  t: TypeUID;  s: ByteSize;  a: Alignment) =
  (* pop s0 and make it the "next" paramter in the current call *)
  BEGIN
    Cmd   (u, Bop.pop_struct);
    Tipe  (u, t);
    Int   (u, s);
    Int   (u, a);
  END pop_struct;

PROCEDURE pop_static_link (u: U) =
  BEGIN
    Cmd   (u, Bop.pop_static_link);
  END pop_static_link;

PROCEDURE call_direct (u: U; p: Proc;  t: Type) =
  (* call the procedure identified by block b.  The procedure
     returns a value of type t. *)
  BEGIN
    Cmd   (u, Bop.call_direct);
    PName (u, p);
    TName (u, t);
  END call_direct;

PROCEDURE call_indirect (u: U;  t: Type;  cc: CallingConvention) =
  (* call the procedure whose address is in s0.A and pop s0.  The
     procedure returns a value of type t. *)
  BEGIN
    Cmd   (u, Bop.call_indirect);
    TName (u, t);
    OutB  (u, cc.m3cg_id);
  END call_indirect;

(*------------------------------------------- procedure and closure types ---*)

PROCEDURE load_procedure (u: U;  p: Proc) =
  (* push; s0.A := ADDR (p's body) *)
  BEGIN
    Cmd   (u, Bop.load_procedure);
    PName (u, p);
  END load_procedure;

PROCEDURE load_static_link (u: U;  p: Proc) =
  (* push; s0.A := (static link needed to call p, NIL for top-level procs) *)
  BEGIN
    Cmd   (u, Bop.load_static_link);
    PName (u, p);
  END load_static_link;

(*----------------------------------------------------------------- misc. ---*)

PROCEDURE comment (u: U;  a, b, c, d: TEXT := NIL) =
  VAR msg: TEXT := "";
  BEGIN
    IF (a # NIL) THEN msg := msg & a; END;
    IF (b # NIL) THEN msg := msg & b; END;
    IF (c # NIL) THEN msg := msg & c; END;
    IF (d # NIL) THEN msg := msg & d; END;
    Cmd (u, Bop.comment);
    Txt (u, msg);
  END comment;

(*--------------------------------------------------------------- atomics ---*)

PROCEDURE store_ordered (u: U;  t: ZType;  z: MType;  order: MemoryOrder) =
  BEGIN
    Cmd   (u, Bop.store_ordered);
    TName (u, t);
    TName (u, z);
    Int   (u, ORD(order));
  END store_ordered;

PROCEDURE load_ordered (u: U;  t: MType;  z: ZType;  order: MemoryOrder) =
  BEGIN
    Cmd   (u, Bop.load_ordered);
    TName (u, t);
    TName (u, z);
    Int   (u, ORD(order));
  END load_ordered;

PROCEDURE exchange (u: U;  t: MType;  z: ZType;  order: MemoryOrder) =
  BEGIN
    Cmd   (u, Bop.exchange);
    TName (u, t);
    TName (u, z);
    Int   (u, ORD(order));
  END exchange;

PROCEDURE compare_exchange (u: U;  t: MType;  z: ZType;  r: IType;
                            success, failure: MemoryOrder) =
  BEGIN
    Cmd   (u, Bop.compare_exchange);
    TName (u, t);
    TName (u, z);
    TName (u, r);
    Int   (u, ORD(success));
    Int   (u, ORD(failure));
  END compare_exchange;

PROCEDURE fence (u: U;  order: MemoryOrder) =
  BEGIN
    Cmd   (u, Bop.fence);
    Int   (u, ORD(order));
  END fence;

PROCEDURE fetch_and_op (u: U;  op: AtomicOp;  t: MType;  z: ZType;
                        order: MemoryOrder) =
  CONST
    OpName = ARRAY AtomicOp OF Bop { Bop.fetch_and_add, Bop.fetch_and_sub,
                                     Bop.fetch_and_or,  Bop.fetch_and_and,
                                     Bop.fetch_and_xor };
  BEGIN
    Cmd   (u, OpName [op]);
    TName (u, t);
    TName (u, z);
    Int   (u, ORD(order));
  END fetch_and_op;

BEGIN
END M3CG_BinWr.
