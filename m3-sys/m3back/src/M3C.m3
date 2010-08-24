(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Jun 20 16:19:09 PDT 1995 by kalsow     *)
(*      modified on Wed Nov 23 13:57:47 PST 1994 by isard      *)

MODULE M3C;

IMPORT Wr, Text, Fmt, IntRefTbl, Word;
IMPORT M3CG, M3ID, M3CG_Ops, Target, TFloat;
IMPORT TIntN, TWordN;
IMPORT M3ObjFile, TargetMap;

FROM TargetMap IMPORT CG_Bytes;

FROM M3CG IMPORT Name, ByteOffset, TypeUID, CallingConvention;
FROM M3CG IMPORT BitSize, ByteSize, Alignment, Frequency;
FROM M3CG IMPORT Var, Proc, Label, No_label, Sign, BitOffset;
FROM M3CG IMPORT Type, ZType, AType, RType, IType, MType;
FROM M3CG IMPORT CompareOp, ConvertOp, RuntimeError, MemoryOrder, AtomicOp;

FROM M3CG_Ops IMPORT ErrorHandler;

FROM M3ObjFile IMPORT Seg;

IMPORT Wrx86, Stackx86, Codex86;

FROM Stackx86 IMPORT MaxMin, ShiftType;
FROM Codex86 IMPORT Cond, Op, FOp, unscond, revcond, FloatBytes;

TYPE
  RuntimeHook = REF RECORD
    name   : Name;
    proc   : Proc;
    var    : Var;
    offset : ByteOffset;
  END;

REVEAL
  U = Public BRANDED "M3x86.U" OBJECT
        rawwr           : Wr.T := NIL;
        wr              : Wr.T := NIL;
        debug           := FALSE;
        Err             : ErrorHandler := NIL;
        runtime         : IntRefTbl.T := NIL;  (* Name -> RuntimeHook *)
        textsym         : INTEGER;
        init_count      : INTEGER;

        (* What determines the sizes here? Historically it was 2. *)
        call_param_size := ARRAY [0 .. 9] OF INTEGER { 0, .. };
        in_proc_call    : [0 .. 10] := 0;
        static_link     := ARRAY [0 .. 9] OF x86Var { NIL, .. };

        current_proc    : x86Proc := NIL;
        param_proc      : x86Proc := NIL;
        in_proc         : BOOLEAN;
        procframe_ptr   : ByteOffset;
        exit_proclabel  : Label := -1;
        last_exitbranch := -1;
        n_params        : INTEGER;
        next_var        := 1;
        next_proc       := 1;
        next_scope      := 1;
        builtins        : ARRAY Builtin OF x86Proc;
        global_var      : x86Var := NIL;
        lineno          : INTEGER;
        source_file     : TEXT := NIL;
        reportlabel     : Label;
        usedfault       := FALSE;
      OVERRIDES
        NewVar := NewVar;
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
        set_runtime_hook := set_runtime_hook;
        get_runtime_hook := get_runtime_hook;
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
        debug_set_label := debug_set_label;
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
        set_compare   := set_compare;
        set_range     := set_range;
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

CONST
  CompareOpName = ARRAY CompareOp OF TEXT {
                          " EQ", " NE", " GT", " GE", " LT", " LE" };
  CompareOpCond = ARRAY CompareOp OF Cond {
                          Cond.E, Cond.NE, Cond.G, Cond.GE, Cond.L, Cond.LE };

  CompareOpProc = ARRAY [CompareOp.GT .. CompareOp.LE] OF Builtin {
        Builtin.set_gt, Builtin.set_ge, Builtin.set_lt, Builtin.set_le };

CONST
  ConvertOpName = ARRAY ConvertOp OF TEXT {
                    " round", " trunc", " floor", " ceiling" };
  ConvertOpKind = ARRAY ConvertOp OF FlToInt {
                    FlToInt.Round, FlToInt.Truncate,
                    FlToInt.Floor, FlToInt.Ceiling };

CONST
  Alignmask = ARRAY [1 .. 8] OF INTEGER
    (* 1 => -1     2 => -2      3  4 => -4      5  6  7  8 => -8 *)
    { 16_FFFFFFFF, 16_FFFFFFFE, 0, 16_FFFFFFFC, 0, 0, 0, 16_FFFFFFF8 };

(*---------------------------------------------------------------------------*)

PROCEDURE New (logfile: Wr.T; obj: M3ObjFile.T): M3CG.T =
  VAR u := NEW (U,
                obj := obj,
                runtime := NEW (IntRefTbl.Default).init (20));
  BEGIN

    IntType[Type.Int8]   := Target.Int8;
    IntType[Type.Int16]  := Target.Int16;
    IntType[Type.Int32]  := Target.Int32;
    IntType[Type.Int64]  := Target.Int64;
    IntType[Type.Word8]  := Target.Word8;
    IntType[Type.Word16] := Target.Word16;
    IntType[Type.Word32] := Target.Word32;
    IntType[Type.Word64] := Target.Word64;
    TIntN.Init();

    IF logfile # NIL THEN
      u.debug := TRUE;
      u.wr := Wrx86.New (logfile);
    ELSE
      u.wr := NIL;
    END;

    FOR b := FIRST (u.builtins) TO LAST (u.builtins) DO
      u.builtins[b] := NIL;
    END;

    RETURN u;
  END New;

(*----------------------------------------------------------- ID counters ---*)

PROCEDURE next_label (u: U;  n: INTEGER := 1): Label =
  BEGIN
    RETURN -1;
  END next_label;

(*------------------------------------------------ READONLY configuration ---*)

PROCEDURE set_error_handler (u: U; p: ErrorHandler) =
  BEGIN
    u.Err := p;
  END set_error_handler;

(*----------------------------------------------------- compilation units ---*)

PROCEDURE begin_unit (u: U;  optimize : INTEGER) =
  (* called before any other method to initialize the compilation unit *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd ("begin_unit");
      u.wr.Int (optimize);
      u.wr.NL  ();
    END;

  END begin_unit;

PROCEDURE end_unit   (u: U) =
  (* called after all other methods to finalize the unit and write the
     resulting object *)
  BEGIN
    IF u.usedfault THEN
      makereportproc(u);
    END;

    IF u.debug THEN
      u.wr.Cmd ("end_unit");
      u.wr.NL  ();
    END;

  END end_unit;

PROCEDURE import_unit (u: U;  n: Name) =
  (* note that the current compilation unit imports the interface 'n' *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("import_unit");
      u.wr.ZName (n);
      u.wr.NL    ();
    END
  END import_unit;

PROCEDURE export_unit (u: U;  n: Name) =
  (* note that the current compilation unit exports the interface 'n' *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("export_unit");
      u.wr.ZName (n);
      u.wr.NL    ();
    END
  END export_unit;

(*------------------------------------------------ debugging line numbers ---*)

PROCEDURE set_source_file (u: U; file: TEXT) =
  (* Sets the current source file name.  Subsequent statements
     and expressions are associated with this source location. *)
  BEGIN
    IF u.debug THEN
      u.wr.OutT ("\t\t\t\t\t-----FILE ");
      u.wr.OutT (file);
      u.wr.OutT ("  -----");
      u.wr.NL ();
    END;

  END set_source_file;

PROCEDURE set_source_line (u: U; line: INTEGER) =
  (* Sets the current source line number.  Subsequent statements
   and expressions are associated with this source location. *)
  BEGIN
    IF u.debug THEN
      u.wr.OutT ("\t\t\t\t\t-----LINE");
      u.wr.Int  (line);
      u.wr.OutT ("  -----");
      u.wr.NL ();
    END;

  END set_source_line;


(*------------------------------------------- debugging type declarations ---*)

PROCEDURE declare_typename (u: U;  type: TypeUID;  n: Name) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_typename");
      u.wr.Tipe  (type);
      u.wr.ZName (n);
      u.wr.NL    ();
    END
  END declare_typename;

PROCEDURE declare_array (u: U;  type, index, elt: TypeUID;  s: BitSize) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_array");
      u.wr.Tipe (type);
      u.wr.Tipe (index);
      u.wr.Tipe (elt);
      u.wr.BInt (s);
      u.wr.NL   ();
    END
  END declare_array;

PROCEDURE declare_open_array (u: U;  type, elt: TypeUID;  s: BitSize) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_open_array");
      u.wr.Tipe (type);
      u.wr.Tipe (elt);
      u.wr.BInt (s);
      u.wr.NL   ();
    END
  END declare_open_array;

PROCEDURE declare_enum (u: U;  type: TypeUID;  n_elts: INTEGER;  s: BitSize) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_enum");
      u.wr.Tipe (type);
      u.wr.Int  (n_elts);
      u.wr.BInt (s);
      u.wr.NL   ();
    END
  END declare_enum;

PROCEDURE declare_enum_elt (u: U;  n: Name) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_enum_elt");
      u.wr.ZName (n);
      u.wr.NL    ();
    END
  END declare_enum_elt;

PROCEDURE declare_packed  (u: U;  type: TypeUID;  s: BitSize;  base: TypeUID) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_packed");
      u.wr.Tipe (type);
      u.wr.BInt (s);
      u.wr.Tipe (base);
      u.wr.NL   ();
    END
  END declare_packed;

PROCEDURE declare_record (u: U; type: TypeUID;  s: BitSize; n_fields: INTEGER) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_record");
      u.wr.Tipe (type);
      u.wr.BInt (s);
      u.wr.Int  (n_fields);
      u.wr.NL   ();
    END
  END declare_record;

PROCEDURE declare_field (u: U; n: Name; o: BitOffset; s: BitSize; type: TypeUID) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_field");
      u.wr.ZName (n);
      u.wr.BInt  (o);
      u.wr.BInt  (s);
      u.wr.Tipe  (type);
      u.wr.NL    ();
    END
  END declare_field;

PROCEDURE declare_set (u: U;  type, domain: TypeUID;  s: BitSize) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_set");
      u.wr.Tipe (type);
      u.wr.Tipe (domain);
      u.wr.BInt (s);
      u.wr.NL    ();
    END
  END declare_set;

PROCEDURE declare_subrange (u: U; type, domain: TypeUID;
                            READONLY min, max: Target.Int;
                            s: BitSize) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_subrange");
      u.wr.Tipe (type);
      u.wr.Tipe (domain);
      u.wr.TInt (TIntN.FromTargetInt(min, NUMBER(min))); (* What about s for size? *)
      u.wr.TInt (TIntN.FromTargetInt(max, NUMBER(max))); (* What about s for size? *)
      u.wr.BInt (s);
      u.wr.NL   ();
    END
  END declare_subrange;

PROCEDURE declare_pointer (u: U; type, target: TypeUID; brand: TEXT; traced: BOOLEAN) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_pointer");
      u.wr.Tipe (type);
      u.wr.Tipe (target);
      u.wr.Txt  (brand);
      u.wr.Bool (traced);
      u.wr.NL   ();
    END
  END declare_pointer;


PROCEDURE declare_indirect (u: U;  type, target: TypeUID) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_indirect");
      u.wr.Tipe (type);
      u.wr.Tipe (target);
      u.wr.NL   ();
    END
  END declare_indirect;


PROCEDURE declare_proctype (u: U;  type: TypeUID;  n_formals: INTEGER;
                            result: TypeUID;  n_raises: INTEGER;
                            cc: CallingConvention) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_proctype");
      u.wr.Tipe (type);
      u.wr.Int  (n_formals);
      u.wr.Tipe (result);
      u.wr.Int  (n_raises);
      u.wr.Txt  (cc.name);
      u.wr.NL   ();
    END
  END declare_proctype;

PROCEDURE declare_formal (u: U;  n: Name;  type: TypeUID) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_formal");
      u.wr.ZName (n);
      u.wr.Tipe  (type);
      u.wr.NL    ();
    END
  END declare_formal;

PROCEDURE declare_raises (u: U;  n: Name) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_raises");
      u.wr.ZName (n);
      u.wr.NL    ();
    END
  END declare_raises;


PROCEDURE declare_object (u: U;  type, super: TypeUID;
                          brand: TEXT;  traced: BOOLEAN;
                          n_fields, n_methods: INTEGER;
                          field_size: BitSize) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_object");
      u.wr.Tipe (type);
      u.wr.Tipe (super);
      u.wr.Txt  (brand);
      u.wr.Bool (traced);
      u.wr.Int  (n_fields);
      u.wr.Int  (n_methods);
      u.wr.BInt (field_size);
      u.wr.NL   ();
    END
  END declare_object;

PROCEDURE declare_method (u: U;  n: Name;  signature: TypeUID) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_method");
      u.wr.ZName (n);
      u.wr.Tipe  (signature);
      u.wr.NL    ();
    END
  END declare_method;

PROCEDURE declare_opaque (u: U;  type, super: TypeUID) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_opaque");
      u.wr.Tipe  (type);
      u.wr.Tipe  (super);
      u.wr.NL    ();
    END
  END declare_opaque;

PROCEDURE reveal_opaque (u: U;  lhs, rhs: TypeUID) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("reveal_opaque");
      u.wr.Tipe  (lhs);
      u.wr.Tipe  (rhs);
      u.wr.NL    ();
    END
  END reveal_opaque;

PROCEDURE declare_exception (u: U;  n: Name;  arg_type: TypeUID;
                           raise_proc: BOOLEAN;  base: Var;  offset: INTEGER) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_exception");
      u.wr.ZName (n);
      u.wr.Tipe  (arg_type);
      u.wr.Bool  (raise_proc);
      u.wr.VName (base);
      u.wr.Int   (offset);
      u.wr.NL    ();
    END
  END declare_exception;

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
    IF u.debug THEN
      u.wr.Cmd   ("set_runtime_proc");
      u.wr.ZName (n);
      u.wr.PName (p);
      u.wr.NL    ();
    END;

    e.proc := p;
  END set_runtime_proc;

PROCEDURE set_runtime_hook (u: U;  n: Name;  v: Var;  o: ByteOffset) =
  VAR e := GetRuntimeHook (u, n);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("set_runtime_hook");
      u.wr.ZName (n);
      u.wr.VName (v);
      u.wr.Int   (o);
      u.wr.NL    ();
    END;

    e.var := v;
    e.offset := o;
  END set_runtime_hook;

PROCEDURE get_runtime_hook (u: U;  n: Name;
                            VAR p: Proc; VAR v: Var; VAR o: ByteOffset) =
  VAR e := GetRuntimeHook (u, n);
  BEGIN
    p := e.proc;
    v := e.var;
    o := e.offset;
  END get_runtime_hook;

(*------------------------------------------------- variable declarations ---*)

PROCEDURE NewVar (u: U; type: Type; uid: TypeUID; s: ByteSize; a: Alignment;
                  name: Name := M3ID.NoID): x86Var =
  VAR v := NEW (x86Var, tag := u.next_var, var_type := type, var_size := s,
                var_align := a, seg := Seg.Data);
  BEGIN
    IF name = M3ID.NoID THEN
      v.name := M3ID.Add("T$" & Fmt.Int(v.tag));
    ELSIF uid = -1 THEN
      v.name := M3ID.Add("_M" & M3ID.ToText(name));
    ELSE
      v.name := M3ID.Add("_" & M3ID.ToText(name));
    END;

    INC (u.next_var);
    RETURN v;
  END NewVar;

PROCEDURE import_global (u: U;  n: Name;  s: ByteSize;  a: Alignment;
                         type: Type;  m3t: TypeUID): Var =
  VAR v := NewVar(u, type, m3t, s, a, n);
  BEGIN
    v.offset := 0;
    v.loc := VLoc.global;

    IF u.debug THEN
      u.wr.Cmd   ("import_global");
      u.wr.ZName (n);
      u.wr.Int   (s);
      u.wr.Int   (a);
      u.wr.TName (type);
      u.wr.Tipe  (m3t);
      u.wr.VName (v);
      u.wr.NL    ();
    END;

    RETURN v;
  END import_global;

PROCEDURE declare_segment (u: U;  n: Name;  m3t: TypeUID;  is_const: BOOLEAN): Var =
  CONST SegMap = ARRAY BOOLEAN(*is_const*) OF Seg { Seg.Data, Seg.Text };
  VAR v := NewVar(u, Type.Void, m3t, 0, 4, n);
  BEGIN
    IF (u.global_var = NIL) AND (NOT is_const) THEN
      u.global_var := v;
    END;

    v.seg    := SegMap [is_const];
    v.offset := 0;
    v.loc    := VLoc.global;

    IF u.debug THEN
      u.wr.Cmd   ("declare_segment");
      u.wr.ZName (n);
      u.wr.Tipe  (m3t);
      u.wr.Bool  (is_const);
      u.wr.VName (v);
      u.wr.NL    ();
    END;

    RETURN v;
  END declare_segment;

PROCEDURE bind_segment (u: U;  v: Var;  s: ByteSize;  a: Alignment;
                        type: Type;  exported, inited: BOOLEAN) =
  VAR realvar := NARROW(v, x86Var);
  BEGIN
    <* ASSERT inited *>

    realvar.var_type := type;
    realvar.var_size := s;
    realvar.var_align := a;

    IF u.debug THEN
      u.wr.Cmd   ("bind_segment");
      u.wr.VName (v);
      u.wr.Int   (s);
      u.wr.Int   (a);
      u.wr.TName (type);
      u.wr.Bool  (exported);
      u.wr.Bool  (inited);
      u.wr.NL    ();
    END
  END bind_segment;

PROCEDURE declare_global (u: U;  n: Name;  s: ByteSize;  a: Alignment;
                     type: Type;  m3t: TypeUID;  exported, inited: BOOLEAN): Var =
  BEGIN
    RETURN DeclareGlobal(u, n, s, a, type, m3t, exported, inited, FALSE);
  END declare_global;

PROCEDURE declare_constant (u: U;  n: Name;  s: ByteSize;  a: Alignment;
                     type: Type;  m3t: TypeUID;  exported, inited: BOOLEAN): Var =
  BEGIN
    RETURN DeclareGlobal(u, n, s, a, type, m3t, exported, inited, TRUE);
  END declare_constant;

PROCEDURE DeclareGlobal (u: U;  n: Name;  s: ByteSize;  a: Alignment;
                         type: Type;  m3t: TypeUID;
                         exported, inited, is_const: BOOLEAN): Var =
  CONST SegMap  = ARRAY BOOLEAN OF Seg  { Seg.Data, Seg.Text };
  CONST DeclTag = ARRAY BOOLEAN OF TEXT { "declare_global", "declare_constant" };
  VAR v := NewVar(u, type, m3t, s, a, n);
  BEGIN
    v.loc := VLoc.global;
    v.seg := SegMap [is_const];
    IF u.debug THEN
      u.wr.Cmd   (DeclTag [is_const]);
      u.wr.ZName (n);
      u.wr.Int   (s);
      u.wr.Int   (a);
      u.wr.TName (type);
      u.wr.Tipe  (m3t);
      u.wr.Bool  (exported);
      u.wr.Bool  (inited);
      u.wr.VName (v);
      u.wr.NL    ();
    END;
    RETURN v;
  END DeclareGlobal;

PROCEDURE declare_local (u: U;  n: Name;  s: ByteSize;  a: Alignment;
                         type: Type;  m3t: TypeUID;  in_memory, up_level: BOOLEAN;
                         f: Frequency): Var =
  VAR v: x86Var;
  BEGIN
    IF u.in_proc THEN
      v := get_temp_var (u, type, s, a, n);
    ELSE
      v := create_temp_var (u, type, s, a, n);
    END;

    IF u.debug THEN
      u.wr.Cmd   ("declare_local");
      u.wr.ZName (n);
      u.wr.Int   (s);
      u.wr.Int   (a);
      u.wr.TName (type);
      u.wr.Tipe  (m3t);
      u.wr.Bool  (in_memory);
      u.wr.Bool  (up_level);
      u.wr.Int   (f);
      u.wr.VName (v);
      u.wr.Int   (v.offset);
      u.wr.NL    ();
    END;

    RETURN v;
  END declare_local;

PROCEDURE mangle_procname (base: M3ID.T; arg_size: INTEGER;
                           std_call: BOOLEAN): M3ID.T =
  VAR buf: ARRAY [0..3] OF CHAR;
      txt := M3ID.ToText(base);
      len := Text.Length(txt);
  BEGIN
    (* return the 64bit functions unchanged *)

    IF len > NUMBER(buf) THEN
      Text.SetChars(SUBARRAY(buf, 0, NUMBER(buf)), txt);
      IF buf = ARRAY OF CHAR{'_', 'm', '3', '_'} THEN
        RETURN base
      END;
    END;

    IF std_call THEN
      RETURN M3ID.Add(Fmt.F ("_%s@%s", txt, Fmt.Int (arg_size)));
    ELSE
      RETURN M3ID.Add(Fmt.F ("_%s",    txt));
    END;
  END mangle_procname;

PROCEDURE declare_param (u: U;  n: Name;  s: ByteSize;  a: Alignment;
                         type: Type;  m3t: TypeUID;  in_memory, up_level: BOOLEAN;
                         f: Frequency): Var =
  VAR v := NewVar(u, type, m3t, s, 4, n);
  BEGIN
    (* Assume a = 4 and ESP is dword aligned... *)

    s := (s + 3) DIV 4 * 4;

    v.offset := u.param_proc.paramsize;
    v.loc := VLoc.temp;

    v.parent := u.param_proc;

    INC(u.param_proc.paramsize, s);

    <* ASSERT u.n_params > 0 *>
    DEC(u.n_params);

    IF u.n_params = 0 AND u.param_proc.stdcall THEN
      (* callee cleans & mangled name *)
      u.param_proc.name := mangle_procname(u.param_proc.name,
                                           u.param_proc.paramsize - 8,
                                           std_call := TRUE);

    END;

    IF u.debug THEN
      u.wr.Cmd   ("declare_param");
      u.wr.ZName (n);
      u.wr.Int   (s);
      u.wr.Int   (a);
      u.wr.TName (type);
      u.wr.Tipe  (m3t);
      u.wr.Bool  (in_memory);
      u.wr.Bool  (up_level);
      u.wr.Int   (f);
      u.wr.VName (v);
      u.wr.Int   (v.offset);
      u.wr.NL    ();
    END;

    RETURN v;
  END declare_param;

PROCEDURE declare_temp (u: U; s: ByteSize; a: Alignment; type: Type; in_memory:BOOLEAN): Var =
  VAR v: x86Var;
  BEGIN
    <* ASSERT u.in_proc *>

    v := get_temp_var(u, type, s, a);

    IF u.debug THEN
      u.wr.Cmd   ("declare_temp");
      u.wr.Int   (s);
      u.wr.Int   (a);
      u.wr.TName (type);
      u.wr.Bool  (in_memory);
      u.wr.VName (v);
      u.wr.Int   (v.offset);
      u.wr.NL    ();
    END;

    RETURN v;
  END declare_temp;

PROCEDURE get_temp_var (u: U; type: Type; s: ByteSize; a: Alignment;
                        n: Name := M3ID.NoID): x86Var =
  BEGIN

    (* round size and alignment up to 4 *)

    IF s < 4 THEN
      s := 4;
    END;

    IF a < 4 THEN
      a := 4;
    END;

    (* reuse an existing temporary variable if possible *)

    FOR i := 0 TO u.current_proc.tempsize - 1 DO
      WITH temp = u.current_proc.temparr[i] DO
        IF temp.free AND temp.var.var_size = s AND temp.var.var_align >= a THEN

          (* reinitialize existing temporary variable *)

          temp.free := FALSE;
          temp.var.var_type := type;
          temp.var.stack_temp := FALSE;
          temp.var.scope := u.next_scope - 1;
          RETURN temp.var;
        END
      END
    END;

    (* grow temporary variable array if necessary *)

    IF u.current_proc.tempsize = u.current_proc.templimit THEN
      expand_temp(u);
    END;

    (* initialize new temporary variable *)

    WITH temp = u.current_proc.temparr[u.current_proc.tempsize] DO
      temp.var := create_temp_var(u, type, s, a, n);
      <* ASSERT temp.var.var_type = type *>
      temp.free := FALSE;
      temp.var.scope := u.next_scope - 1;
    END;

    INC(u.current_proc.tempsize);

    RETURN u.current_proc.temparr[u.current_proc.tempsize - 1].var;
  END get_temp_var;

PROCEDURE expand_temp (u: U) =
  VAR newarr := NEW(REF ARRAY OF Temp, u.current_proc.templimit * 2);
  BEGIN
    FOR i := 0 TO (u.current_proc.templimit - 1) DO
      newarr[i] := u.current_proc.temparr[i];
    END;

    u.current_proc.templimit := u.current_proc.templimit * 2;
    u.current_proc.temparr := newarr;
  END expand_temp;

PROCEDURE create_temp_var (u: U; type: Type; s: ByteSize; a: Alignment;
                           n: Name): x86Var =
  VAR v := NewVar(u, type, 0, s, a, n);
  BEGIN
    v.loc := VLoc.temp;
    v.parent := u.current_proc;

    u.current_proc.framesize := Word.And(u.current_proc.framesize + a - 1,
                                         Alignmask[a]);

    INC(u.current_proc.framesize, s);

    v.offset := -u.current_proc.framesize;

    RETURN v;
  END create_temp_var;

PROCEDURE free_temp (u: U;  v: Var) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("free_temp");
      u.wr.VName (v);
      u.wr.NL    ();
    END;

    FOR i := 0 TO u.current_proc.tempsize - 1 DO
      IF (NOT u.current_proc.temparr[i].free) AND
         u.current_proc.temparr[i].var = v THEN
        u.current_proc.temparr[i].free := TRUE;
        RETURN;
      END
    END;

    Err(u, "Couldn't find var to free in 'free_temp'");
    <* ASSERT FALSE *>
  END free_temp;

(*---------------------------------------- static variable initialization ---*)

PROCEDURE begin_init (u: U;  v: Var) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("begin_init");
      u.wr.VName (v);
      u.wr.NL    ();
    END;
    u.init_count := 0;
  END begin_init;

PROCEDURE end_init (u: U;  v: Var) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("end_init");
      u.wr.VName (v);
      u.wr.NL    ();
    END;
  END end_init;

PROCEDURE init_int (u: U; o: ByteOffset; READONLY value: Target.Int; type: Type) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("init_int");
      u.wr.Int   (o);
      u.wr.TInt  (TIntN.FromTargetInt(value, CG_Bytes[type]));
      u.wr.TName (type);
      u.wr.NL    ();
    END;

  END init_int;

PROCEDURE init_proc (u: U;  o: ByteOffset;  value: Proc) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("init_proc");
      u.wr.Int   (o);
      u.wr.PName (value);
      u.wr.NL    ();
    END;

  END init_proc;

PROCEDURE init_label (u: U;  o: ByteOffset;  value: Label) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("init_label");
      u.wr.Int   (o);
      u.wr.Lab   (value);
      u.wr.NL    ();
    END;

    pad_init(u, o);

    INC(u.init_count, 4);
  END init_label;

PROCEDURE init_var (u: U;  o: ByteOffset;  value: Var;  bias: ByteOffset) =
  VAR realvar := NARROW(value, x86Var);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("init_var");
      u.wr.Int   (o);
      u.wr.VName (value);
      u.wr.Int   (bias);
      u.wr.NL    ();
    END;

    <* ASSERT realvar.loc = VLoc.global *>

    pad_init(u, o);

    INC(u.init_count, 4);

  END init_var;

PROCEDURE init_offset (u: U;  o: ByteOffset;  value: Var) =
  VAR realvar := NARROW(value, x86Var);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("init_offset");
      u.wr.Int   (o);
      u.wr.VName (value);
      u.wr.NL    ();
    END;

    <* ASSERT realvar.loc = VLoc.temp *>

    pad_init(u, o);

    INC(u.init_count, 4);
  END init_offset;

PROCEDURE init_chars (u: U;  o: ByteOffset;  value: TEXT) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("init_chars");
      u.wr.Int   (o);
      u.wr.Txt   (value);
      u.wr.NL    ();
    END;

    pad_init(u, o);

    WITH len = Text.Length(value) DO
      INC(u.init_count, len);
    END
  END init_chars;

PROCEDURE init_float (u: U;  o: ByteOffset;  READONLY f: Target.Float) =
  VAR flarr: FloatBytes;
      size: INTEGER;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("init_float");
      u.wr.Int   (o);
      u.wr.Flt   (f);
      u.wr.NL    ();
    END;

    size := TFloat.ToBytes(f, flarr);

    <* ASSERT size = 4 OR size = 8 *>

    pad_init(u, o);

  END init_float;

PROCEDURE pad_init (u: U; o: ByteOffset) =
  BEGIN
    <* ASSERT u.init_count <= o *>
    <* ASSERT o <= u.init_varstore.var_size *>

    u.init_count := o;
  END pad_init;

(*------------------------------------------------------------ procedures ---*)

PROCEDURE NewProc (u: U; n: Name; n_params: INTEGER;
                   ret_type: Type;  cc: CallingConvention): x86Proc =
  VAR p := NEW (x86Proc, tag := u.next_proc, n_params := n_params,
                proc_type := ret_type, stdcall := (cc.m3cg_id = 1));
  BEGIN
    IF n = M3ID.NoID THEN
      p.name := M3ID.Add("P$" & Fmt.Int(p.tag));
    ELSE
      p.name := n;
    END;

    p.templimit := 16;
    p.temparr := NEW(REF ARRAY OF Temp, p.templimit);

    INC (u.next_proc);
    RETURN p;
  END NewProc;

PROCEDURE import_procedure (u: U;  n: Name;  n_params: INTEGER;
                            ret_type: Type;  cc: CallingConvention): Proc =
  VAR p := NewProc (u, n, n_params, ret_type, cc);
  BEGIN
    p.import := TRUE;

    u.n_params := n_params;

    IF (n_params = 0 OR NOT p.stdcall) AND Text.Length(M3ID.ToText(n)) > 0 THEN
      p.name := mangle_procname(p.name, 0, p.stdcall);
    END;

    u.param_proc := p;

    IF u.debug THEN
      u.wr.Cmd   ("import_procedure");
      u.wr.ZName (n);
      u.wr.Int   (n_params);
      u.wr.TName (ret_type);
      u.wr.Txt   (cc.name);
      u.wr.PName (p);
      u.wr.NL    ();
    END;

    RETURN p;
  END import_procedure;

PROCEDURE declare_procedure (u: U;  n: Name;  n_params: INTEGER;
                             return_type: Type;  lev: INTEGER;
                             cc: CallingConvention;
                             exported: BOOLEAN;  parent: Proc): Proc =
  VAR p := NewProc (u, n, n_params, return_type, cc);
  BEGIN
    p.exported := exported;

    p.lev := lev;
    p.parent := parent;

    IF p.lev # 0 THEN
      INC(p.framesize, 4);
    END;

    u.n_params := n_params;

    IF n_params = 0 OR NOT p.stdcall THEN
      p.name   := mangle_procname(p.name, 0, p.stdcall);
    END;

    u.param_proc := p;

    IF NOT u.in_proc THEN u.current_proc := p; END;

    IF u.debug THEN
      u.wr.Cmd   ("declare_procedure");
      u.wr.ZName (n);
      u.wr.Int   (n_params);
      u.wr.TName (return_type);
      u.wr.Int   (lev);
      u.wr.Txt   (cc.name);
      u.wr.Bool  (exported);
      u.wr.PName (parent);
      u.wr.PName (p);
      u.wr.NL    ();
    END;

    RETURN p;
  END declare_procedure;

PROCEDURE begin_procedure (u: U;  p: Proc) =
  VAR realproc := NARROW(p, x86Proc);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("begin_procedure");
      u.wr.PName (p);
      u.wr.NL    ();
    END;

    <* ASSERT NOT u.in_proc *>
    u.in_proc := TRUE;

    u.current_proc := p;
    u.last_exitbranch := -1;
    u.exit_proclabel := -1;

    (* Mark non-volatiles as not used, until known otherwise. *)

    u.proc_reguse[EBX] := FALSE;
    u.proc_reguse[ESI] := FALSE;
    u.proc_reguse[EDI] := FALSE;

    realproc.bound := TRUE;

    WHILE realproc.usage # NIL DO
      realproc.usage := realproc.usage.link;
    END;

    u.current_proc.tempsize := 0;

    <* ASSERT u.next_scope = 1 *>

    begin_block(u);
  END begin_procedure;

PROCEDURE end_procedure (u: U;  p: Proc) =
  VAR realproc := NARROW(p, x86Proc);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("end_procedure");
      u.wr.PName (p);
      u.wr.NL    ();
    END;

    procedure_epilogue(u);

    <* ASSERT u.in_proc *>
    <* ASSERT u.current_proc = p *>

    u.current_proc.framesize := Word.And(u.current_proc.framesize + 3,
                                         16_FFFFFFFC);

    u.in_proc := FALSE;

    end_block(u);
  END end_procedure;

PROCEDURE begin_block (u: U) =
  (* marks the beginning of a nested anonymous block *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("begin_block");
      u.wr.NL    ();
    END;

    INC(u.next_scope);
  END begin_block;

PROCEDURE end_block (u: U) =
  (* marks the ending of a nested anonymous block *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("end_block");
      u.wr.NL    ();
    END;

    <* ASSERT u.next_scope > 1 *>
    DEC(u.next_scope);

    free_locals(u, u.next_scope);
  END end_block;

PROCEDURE free_locals (u: U; scope: INTEGER) =
  BEGIN
    FOR i := 0 TO u.current_proc.tempsize - 1 DO
      IF (NOT u.current_proc.temparr[i].free) AND
         u.current_proc.temparr[i].var.scope = scope THEN
        u.current_proc.temparr[i].free := TRUE;
      END
    END
  END free_locals;

PROCEDURE note_procedure_origin (u: U;  p: Proc) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("note_procedure_origin");
      u.wr.PName (p);
      u.wr.NL    ();
    END
  END note_procedure_origin;

(*------------------------------------------------------------ statements ---*)

PROCEDURE debug_set_label (u: U;  label: Label) =
  BEGIN
    IF u.debug THEN
      u.wr.OutT  ("set_label");
      u.wr.Lab   (label);
      u.wr.NL    ();
    END;
  END debug_set_label;

PROCEDURE set_label (u: U;  label: Label;  <*UNUSED*> barrier: BOOLEAN) =
  (* define 'label' to be at the current pc *)
  BEGIN
    (* print ("L" & label & ":;\n") *)
  END set_label;

PROCEDURE jump (u: U; label: Label) =
  (* GOTO label *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("jump");
      u.wr.Lab   (label);
      u.wr.NL    ();
    END;    
    (* print ("goto L" & label & ";\n") *)
  END jump;

PROCEDURE if_true  (u: U;  type: IType;  label: Label; <*UNUSED*> f: Frequency) =
  (* IF (s0.type # 0) GOTO label ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("if_true");
      u.wr.TName (type);
      u.wr.Lab   (label);
      u.wr.NL    ();
    END;    
    (*
    print("if (" & pop() & ") goto L" & label & ";\n");
    *)
  END if_true;

PROCEDURE if_false (u: U;   type: IType;  label: Label; <*UNUSED*> f: Frequency) =
  (* IF (s0.type = 0) GOTO label ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("if_false");
      u.wr.TName (type);
      u.wr.Lab   (label);
      u.wr.NL    ();
    END;
    (*
    print("if (!(" & pop() & ")) goto L" & label & ";\n");
    *)
  END if_false;

PROCEDURE if_compare (u: U;  type: ZType;  op: CompareOp;  label: Label;
                      <*UNUSED*> f: Frequency) =
  (* IF (s1.type  op  s0.type) GOTO label ; pop(2) *)
  VAR cond := CompareOpCond [op];
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("if_compare");
      u.wr.TName (type);
      u.wr.OutT  (CompareOpName [op]);
      u.wr.Lab   (label);
      u.wr.NL    ();
    END;
    (*
    a = pop();
    b = pop();
    print("if ((a) " & op  & "(b)) goto L" & label & ";\n");
    *)
  END if_compare;

PROCEDURE case_jump (u: U;  type: IType;  READONLY labels: ARRAY OF Label) =
  (* "GOTO labels[s0.type] ; pop" with no range checking on s0.type *)
  VAR stack0: INTEGER;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("case_jump");
      u.wr.TName (type);
      u.wr.Int   (NUMBER(labels));
      FOR i := FIRST (labels) TO LAST (labels) DO  u.wr.Lab (labels [i]);  END;
      u.wr.NL    ();
    END;
    (*
    *)
  END case_jump;

PROCEDURE exit_proc (u: U; type: Type) =
  (* Returns s0.type if type is not Void, otherwise returns no value. *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("exit_proc");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print("goto LExit;\n");
  END exit_proc;

PROCEDURE procedure_epilogue (u: U) =
  BEGIN
    print("LExit: return;\n");
  END procedure_epilogue;

(*------------------------------------------------------------ load/store ---*)

PROCEDURE load  (u: U;  v: Var;  o: ByteOffset;  type: MType;  type_multiple_of_32: ZType) =
(* push; s0.u := Mem [ ADR(v) + o ].type ;  The only allowed (type->u) conversions
   are {Int,Word}{8,16} -> {Int,Word}{32,64} and {Int,Word}32 -> {Int,Word}64.
   The source type, type, determines whether the value is sign-extended or
   zero-extended. *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load");
      u.wr.VName (v);
      u.wr.Int   (o);
      u.wr.TName (type);
      u.wr.TName (type_multiple_of_32);
      u.wr.NL    ();
    END;

    <* ASSERT CG_Bytes[type_multiple_of_32] >= CG_Bytes[type] *>

  END load;

PROCEDURE store (u: U;  v: Var;  o: ByteOffset;  type_multiple_of_32: ZType;  type: MType;  ) =
(* Mem [ ADR(v) + o ].u := s0.type; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("store");
      u.wr.VName (v);
      u.wr.Int   (o);
      u.wr.TName (type_multiple_of_32);
      u.wr.TName (type);
      u.wr.NL    ();
    END;

    <* ASSERT CG_Bytes[type_multiple_of_32] >= CG_Bytes[type] *>

  END store;

PROCEDURE load_address (u: U;  v: Var;  o: ByteOffset) =
(* push; s0.A := ADR(v) + o *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_address");
      u.wr.VName (v);
      u.wr.Int   (o);
      u.wr.NL    ();
    END;

  END load_address;

PROCEDURE load_indirect (u: U;  o: ByteOffset;  type: MType;  type_multiple_of_32: ZType) =
(* s0.type_multiple_of_32 := Mem [s0.A + o].type  *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_indirect");
      u.wr.Int   (o);
      u.wr.TName (type);
      u.wr.TName (type_multiple_of_32);
      u.wr.NL    ();
    END;

    <* ASSERT CG_Bytes[type_multiple_of_32] >= CG_Bytes[type] *>

  END load_indirect;

PROCEDURE store_indirect (u: U;  o: ByteOffset;  type_multiple_of_32: ZType;  type: MType) =
(* Mem [s1.A + o].type := s0.type_multiple_of_32; pop (2) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("store_indirect");
      u.wr.Int   (o);
      u.wr.TName (type_multiple_of_32);
      u.wr.TName (type);
      u.wr.NL    ();
    END;

    <* ASSERT CG_Bytes[type_multiple_of_32] >= CG_Bytes[type] *>

  END store_indirect;

(*-------------------------------------------------------------- literals ---*)

PROCEDURE load_nil (u: U) =
  (* push ; s0.A := a *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_nil");
      u.wr.NL    ();
    END;

  END load_nil;

PROCEDURE load_integer  (u: U;  type: IType;  READONLY j: Target.Int) =
  (* push ; s0.type := i *)
  VAR i := TIntN.FromTargetInt(j, CG_Bytes[type]);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_integer");
      u.wr.TName (type);
      u.wr.TInt  (i);
      u.wr.NL    ();
    END;

  END load_integer;

PROCEDURE load_float    (u: U;  type: RType;  READONLY f: Target.Float) =
  (* push ; s0.type := f *)
  VAR size: INTEGER;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_float");
      u.wr.TName (type);
      u.wr.Flt   (f);
      u.wr.NL    ();
    END;

    size := TFloat.ToBytes(f, flarr);
    IF size # CG_Bytes[type] THEN
      Err(u, "Floating size mismatch in load_float");
    END;
  END load_float;

(*------------------------------------------------------------ arithmetic ---*)

PROCEDURE compare (u: U;  type: ZType; result_type: IType;  op: CompareOp) =
  (* s1.result_type := (s1.type  op  s0.type)  ; pop *)

  (*
    Comparison often needs to convert part of EFLAGS to a register-sized boolean.
      Or if one is lucky, just do a conditional branch based on EFLAGS
      and never materialize a register-sized boolean.

    Historically Modula-3 m3back did comparisons like this.
      cmp
      setCcOp to memory temporary on stack; setcc only sets one byte
      xor result_reg, result_reg ; result_reg := 0
      mov result_reg_low, memory ; now have a register-sized boolean

    We can do much better.

    setCcOp is a family of instructions that materialize
    a computation of EFLAGS as an 8 bit boolean.
    There is sete, setne, setg, setl, seta, setb, etc.
    Anything you might conditionally branch on jcc, also has setcc.
    A "catch" however is it only gives you an 8 bit boolean, and
    code often wants a register sized boolean.

    Let's take the following C code as representative of our tasks,
    and observe how the C compiler optimizes it, and match it.
    That is a general technique I often follow, look
    at the optimized C output and match it.
    
        signed

        int signed_LT(int a, int b) { return a < b; }
            xor eax, eax
            cmp
            setl al

        int signed_LE(int a, int b) { return a <= b; }
            xor eax, eax
            cmp
            setle al

        EQ and NE are the same for signed vs. unsigned

        int EQ(int a, int b) { return a == b; }
            xor eax, eax
            cmp
            sete al

        int NE(int a, int b) { return a != b; }
            xor eax, eax
            cmp
            setne al

        GE and GT are the same as LT and LE but with either operands reversed
            or the setcc condition altered.

        int signed_GE(int a, int b) { return a >= b; }
            xor eax, eax
            cmp
            setge al

        int signed_GT(int a, int b) { return a > b; }
            xor eax, eax
            cmp
            setg al

        unsigned

        int unsigned_LT(unsigned a, unsigned b) { return a < b; }
            cmp
            sbb eax, eax
            neg eax
            
        Let's understand this.
            sbb is subtract with carry/borrow.
            subtract from self is zero, and then carry/borrow
            is one more -- either 0 or -1.
            And then neg to turn -1 to 1.
            So sbb, neg materialize carry as a register-sized boolean. 

        int unsigned_LE(unsigned a, unsigned b) { return a <= b; }
            cmp
            sbb eax, eax
            inc eax

        Let's understand this.
            sbb is subtract with carry/borrow.
            subtract from self is zero, and then carry/borrow
            is one more -- either 0 or -1.
            And then inc turns -1 to 0, 0 to 1.
            So sbb, inc materialize carry as a register-sized boolean, inverted. 

        int unsigned_GE(unsigned a, unsigned b) { return a >= b; }
            cmp parameters reversed vs. LT
            sbb eax, eax ; see unsigned_LE for explanation
            inc eax      ; see unsigned_LE for explanation

        int unsigned_GT(unsigned a, unsigned b) { return a > b; }
            cmp
            sbb eax, eax ; see unsigned_LT for explanation
            neg eax      ; see unsigned_LE for explanation

        int unsigned_EQ(unsigned a, unsigned b) { return a == b; }
            same as signed:
            xor eax, eax
            cmp
            sete al

        int unsigned_NE(unsigned a, unsigned b) { return a != b; }
            same as signed:
            xor eax, eax
            cmp
            setne al

        Fill these in if they prove interesting.
        Actually they are.
        Signed comparison to zero of a value in a register is
        sometimes done with test reg, reg.
        Sometimes the zero for the return value in progress
        doubles as the zero for the comparison.
        Also unsigned compare to zero is special. For example,
            unsigned values are always GE zero, never LT zero.

        int signed_GE0(int a) { return a >= 0; }
            xor eax, eax
            if a is in memory
                cmp a to eax
            else if a is in register (__fastcall to simulate)
                test a, a
            setge al

        int signed_GT0(int a) { return a > 0; }
            xor eax, eax
            if a is in memory
                cmp a to eax
            else if a is in register (__fastcall to simulate)
                test a, a
            setg al

        int signed_LT0(int a) { return a < 0; }
            xor eax, eax
            if a is in memory
                cmp a to eax
            else if a is in register (__fastcall to simulate)
                test a, a
            setl al

        int signed_LE0(int a) { return a <= 0; }
            xor eax, eax
            cmp to eax
            setle al

        int signed_EQ0(int a) { return a == 0; }
            xor eax, eax
            cmp to eax
            sete al

        int signed_NE0(int a, int b) { return a != 0; }
            xor eax, eax
            cmp to eax
            setne al

        int unsigned_GE0(unsigned a) { return a >= 0; }
            This is always true.
            xor eax, eax
            inc eax

        int unsigned_GT0(unsigned a) { return a > 0; }
            xor eax, eax
            cmp to eax (reversed?)
            sbb eax, eax ; see unsigned_LT for explanation
            neg eax      ; see unsigned_LT for explanation

        int unsigned_LT0(unsigned a) { return a < 0; }
            This is always false.
            xor eax, eax

        int unsigned_LE0(unsigned a) { return a <= 0; }
            Same as EQ0.

        int unsigned_EQ0(unsigned a) { return a == 0; }
            input is in memory
            xor eax, eax
            cmp to eax
            sete al
            
        int __fastcall unsigned_EQ0(unsigned a) { return a == 0; }
            input is in a register
            xor eax, eax
            test reg, reg
            sete al

        int unsigned_NE0(unsigned a) { return a != 0; }
            same as EQ0 but setne instead of set

    From the code for these functions, we observe that
    there are multiple approaches, depending on what in EFLAGS is needed.

    There is:
        xor reg,reg
        cmp
        setcc reg_low_byte

    and
        cmp
        some math involving EFLAGS, such as sbb.

    The xor presumably has to precede the cmp in order to not lose the EFLAGS.

  *)
  VAR r: Regno := -1;
      reversed := FALSE;
      cond := CompareOpCond [op];
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("compare");
      u.wr.TName (type);
      u.wr.TName (result_type);
      u.wr.OutT  (CompareOpName [op]);
      u.wr.NL    ();
    END;

    <* ASSERT cond # Cond.Z AND cond # Cond.NZ *>

  END compare;

PROCEDURE add (u: U;  type: AType) =
  (* s1.type := s1.type + s0.type ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("add");
      u.wr.TName (type);
      u.wr.NL    ();
    END;

  END add;

PROCEDURE subtract (u: U;  type: AType) =
  (* s1.type := s1.type - s0.type ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("subtract");
      u.wr.TName (type);
      u.wr.NL    ();
    END;

  END subtract;

PROCEDURE multiply (u: U;  type: AType) =
  (* s1.type := s1.type * s0.type ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("multiply");
      u.wr.TName (type);
      u.wr.NL    ();
    END;

  END multiply;

PROCEDURE divide (u: U;  type: RType) =
  (* s1.type := s1.type / s0.type ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("divide");
      u.wr.TName (type);
      u.wr.NL    ();
    END;    
    (*
      stack[1] := ((type)stack[1]) & "/" & (type)stack[0]
      pop();
    *)
  END divide;

CONST SignName = ARRAY Sign OF TEXT { " P", " N", " X" };

PROCEDURE div (u: U;  type: IType;  a, b: Sign) =
  (* s1.type := s1.type DIV s0.type ; pop *)
  VAR builtin: Builtin;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("div");
      u.wr.TName (type);
      u.wr.OutT  (SignName [a]);
      u.wr.OutT  (SignName [b]);
      u.wr.NL    ();
    END;
    (*
      stack[1] := m3_div & "type(" ((type)stack[1]) & "/" & (type)stack[0] & ")"
      pop();
    *)
  END div;

PROCEDURE mod (u: U;  type: IType;  a, b: Sign) =
  (* s1.type := s1.type MOD s0.type ; pop *)
  VAR builtin: Builtin;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("mod");
      u.wr.TName (type);
      u.wr.OutT  (SignName [a]);
      u.wr.OutT  (SignName [b]);
      u.wr.NL    ();
    END;
    (*
      stack[1] := m3_div & "type(" ((type)stack[1]) % "/" & (type)stack[0] & ")"
      pop();
    *)
  END mod;

PROCEDURE negate (u: U;  type: AType) =
  (* s0.type := - s0.type *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("negate");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    (*
      stack[0] := - "(type)" & stack[0]
    *)
  END negate;

PROCEDURE abs      (u: U;  type: AType) =
  (* s0.type := ABS (s0.type) (noop on Words) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("abs");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    (*
      stack[0] := "m3_abs&type(" & stack[0] & ")"
    *)
  END abs;

PROCEDURE max      (u: U;  type: ZType) =
  (* s1.type := MAX (s1.type, s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("max");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    (*
      stack[1] := "m3_max&type(" & stack[0] & "," stack[1] & ")"
      pop();
    *)
  END max;

PROCEDURE min      (u: U;  type: ZType) =
  (* s1.type := MIN (s1.type, s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("min");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    (*
      stack[1] := "m3_min&type(" & stack[0] & "," stack[1] & ")"
      pop();
    *)
  END min;

PROCEDURE cvt_int (u: U;  type: RType;  x: IType;  op: ConvertOp) =
  (* s0.x := ROUND (s0.type) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("cvt_int");
      u.wr.TName (type);
      u.wr.TName (x);
      u.wr.OutT  (ConvertOpName [op]);
      u.wr.NL    ();
    END;
    (*
      stack[0] := "((long)(" & stack[0] & ")"
    *)
  END cvt_int;

PROCEDURE cvt_float (u: U;  type: AType;  x: RType) =
  (* s0.x := FLOAT (s0.type, x) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("cvt_float");
      u.wr.TName (type);
      u.wr.TName (x);
      u.wr.NL    ();
    END;
    (*
      stack[0] := "((double)(" & stack[0] & ")"
    *)
  END cvt_float;

(*------------------------------------------------------------------ sets ---*)

PROCEDURE set_op3(u: U;  s: ByteSize; op: TEXT) =
  (* s2.B := s1.B op s0.B ; pop(3) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   (BuiltinDesc[builtin].name);
      u.wr.Int   (s);
      u.wr.NL    ();
    END;    
    (*
      stack[2] := "m3_" & op & "(" & stack[0] & stack[1] & stack[2] & ")"
      pop();
      pop();
    *)
  END set_op3;

PROCEDURE set_union (u: U;  s: ByteSize) =
  (* s2.B := s1.B + s0.B ; pop(2) *)
  BEGIN
    set_op3(u, s, "set_union");
  END set_union;

PROCEDURE set_difference (u: U;  s: ByteSize) =
  (* s2.B := s1.B - s0.B ; pop(2) *)
  BEGIN
    set_op3(u, s, "set_difference");
  END set_difference;

PROCEDURE set_intersection (u: U;  s: ByteSize) =
  (* s2.B := s1.B * s0.B ; pop(2) *)
  BEGIN
    set_op3(u, s, "set_intersection");
  END set_intersection;

PROCEDURE set_sym_difference (u: U;  s: ByteSize) =
  (* s2.B := s1.B / s0.B ; pop(2) *)
  BEGIN
    set_op3(u, s, "set_sym_difference");
  END set_sym_difference;

PROCEDURE set_member (u: U;  s: ByteSize;  type: IType) =
  (* s1.type := (s0.type IN s1.B) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("set_member");
      u.wr.Int   (s);
      u.wr.TName (type);
      u.wr.NL    ();
    END;
  END set_member;

PROCEDURE set_compare (u: U;  s: ByteSize;  op: CompareOp;  type: IType) =
  (* s1.type := (s1.B  op  s0.B)  ; pop *)
  VAR proc: Builtin;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("set_compare");
      u.wr.Int   (s);
      u.wr.OutT  (CompareOpName [op]);
      u.wr.TName (type);
      u.wr.NL    ();
    END;
  END set_compare;

PROCEDURE set_range (u: U;  s: ByteSize;  type: IType) =
  (* s2.A [s1.type .. s0.type] := 1's; pop(3) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("set_range");
      u.wr.Int   (s);
      u.wr.TName (type);
      u.wr.NL    ();
    END;
  END set_range;

PROCEDURE set_singleton (u: U;  s: ByteSize;  type: IType) =
  (* s1.A [s0.type] := 1; pop(2) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("set_singleton");
      u.wr.Int   (s);
      u.wr.TName (type);
      u.wr.NL    ();
    END;

  END set_singleton;

(*------------------------------------------------- Word.T bit operations ---*)

PROCEDURE not (u: U;  type: IType) =
  (* s0.type := Word.Not (s0.type) *)
  VAR not: TIntN.T;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("not");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    (*
    stack[0] := "((type)~(type)" & stack[0] & ")";
    *)
  END not;

PROCEDURE and (u: U;  type: IType) =
  (* s1.type := Word.And (s1.type, s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("and");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    (*
    stack[1] := "(((type)" & stack[0] & ") & (type)(stack[1] & "))";
    pop();
    *)
  END and;

PROCEDURE or  (u: U;  type: IType) =
  (* s1.type := Word.Or  (s1.type, s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("or");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    (*
    stack[1] := "(((type)" & stack[0] & ") | (type)(stack[1] & "))";
    pop();
    *)
  END or;

PROCEDURE xor (u: U;  type: IType) =
  (* s1.type := Word.Xor (s1.type, s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("xor");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    (*
    stack[1] := "(((type)" & stack[0] & ") ^ (type)(stack[1] & "))";
    pop();
    *)
  END xor;

PROCEDURE shift_left   (u: U;  type: IType) =
  (* s1.type := Word.Shift  (s1.type, s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("shift_left");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    (*
    stack[1] := "(((type)" & stack[1] & ") << (type)(stack[0] & "))";
    pop();
    *)
  END shift_left;

PROCEDURE shift_right  (u: U;  type: IType) =
  (* s1.type := Word.Shift  (s1.type, -s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("shift_right");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    (*
    stack[1] := "((type)(((unsigned type)" & stack[1] & ") >> (type)(stack[0] & ")))";
    pop();
    *)
  END shift_right;

PROCEDURE shift (u: U;  type: IType) =
  (* s1.type := Word.Shift  (s1.type, s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("shift");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
  END shift;

PROCEDURE rotate (u: U;  type: IType) =
  (* s1.type := Word.Rotate (s1.type, s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("rotate");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
  END rotate;

PROCEDURE rotate_left  (u: U;  type: IType) =
  (* s1.type := Word.Rotate (s1.type, s0.type) ; pop *)
  VAR rotateCount: INTEGER;
      rotate: TIntN.T;
      and: TIntN.T;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("rotate_left");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
  END rotate_left;

PROCEDURE rotate_right (u: U;  type: IType) =
  (* s1.type := Word.Rotate (s1.type, -s0.type) ; pop *)
  VAR rotateCount: INTEGER;
      rotate: TIntN.T;
      and: TIntN.T;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("rotate_right");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
  END rotate_right;

PROCEDURE widen (u: U;  sign_extend: BOOLEAN) =
  (* s0.I64 := s0.I32;  IF sign_extend THEN SignExtend s0;  *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("widen");
      u.wr.Bool  (sign_extend);
      u.wr.NL    ();
    END;
    <*ASSERT FALSE*>
  END widen;

PROCEDURE chop (u: U) =
  (* s0.I32 := Word.And (s0.I64, 16_ffffffff);  *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("chop");
      u.wr.NL    ();
    END;
    <*ASSERT FALSE*>
  END chop;

PROCEDURE extract (u: U;  type: IType;  sign_extend: BOOLEAN) =
  (* s2.type := Word.Extract(s2.type, s1.type, s0.type);
     IF sign_extend THEN SignExtend s2 END; pop(2) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("extract");
      u.wr.TName (type);
      u.wr.Bool  (sign_extend);
      u.wr.NL    ();
    END;
  END extract;

PROCEDURE extract_n (u: U;  type: IType;  sign_extend: BOOLEAN;  n: CARDINAL) =
  (* s1.type := Word.Extract(s1.type, s0.type, n);
     IF sign_extend THEN SignExtend s1 END; pop(1) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("extract_n");
      u.wr.TName (type);
      u.wr.Bool  (sign_extend);
      u.wr.Int   (n);
      u.wr.NL    ();
    END;
  END extract_n;

PROCEDURE extract_mn (u: U;  type: IType;  sign_extend: BOOLEAN;  m, n: CARDINAL) =
  (* s0.type := Word.Extract(s0.type, m, n);
     IF sign_extend THEN SignExtend s0 END; *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("extract_mn");
      u.wr.TName (type);
      u.wr.Bool  (sign_extend);
      u.wr.Int   (m);
      u.wr.Int   (n);
      u.wr.NL    ();
    END;
  END extract_mn;

PROCEDURE insert  (u: U;  type: IType) =
  (* s3.type := Word.Insert (s3.type, s2.type, s1.type, s0.type) ; pop(3) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("insert");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
  END insert;

PROCEDURE insert_n  (u: U;  type: IType;  n: CARDINAL) =
  (* s2.type := Word.Insert (s2.type, s1.type, s0.type, n) ; pop(2) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("insert_n");
      u.wr.TName (type);
      u.wr.Int   (n);
      u.wr.NL    ();
    END;
  END insert_n;

PROCEDURE insert_mn  (u: U;  type: IType;  m, n: CARDINAL) =
  (* s1.type := Word.Insert (s1.type, s0.type, m, n) ; pop(2) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("insert_mn");
      u.wr.TName (type);
      u.wr.Int   (m);
      u.wr.Int   (n);
      u.wr.NL    ();
    END;
  END insert_mn;

(*------------------------------------------------ misc. stack/memory ops ---*)

PROCEDURE swap (u: U;  a, b: Type) =
  (* tmp := s1 ; s1 := s0 ; s0 := tmp *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("swap");
      u.wr.TName (a);
      u.wr.TName (b);
      u.wr.NL    ();
    END;
    temp := stack[1];
    stack[1] := stack[0];
    stack[0] := stack[1];
  END swap;

PROCEDURE pop (u: U;  type: Type) =
  (* pop(1) (i.e. discard s0) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("pop");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
  END pop;

PROCEDURE copy_n (u: U;  type_multiple_of_32: IType;  type: MType;  overlap: BOOLEAN) =
  (* Mem[s2.A:s0.type_multiple_of_32] := Mem[s1.A:s0.type_multiple_of_32]; pop(3)*)
  CONST Mover = ARRAY BOOLEAN OF Builtin { Builtin.memcpy, Builtin.memmove };
  VAR n: INTEGER;  mover := Mover [overlap];
      shift: TIntN.T;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("copy_n");
      u.wr.TName (type_multiple_of_32);
      u.wr.TName (type);
      u.wr.Bool  (overlap);
      u.wr.NL    ();
    END;
  END copy_n;

PROCEDURE inline_copy (u: U; n, size: INTEGER; forward: BOOLEAN) =
  BEGIN
  END inline_copy;

PROCEDURE string_copy (u: U; n, size: INTEGER; forward: BOOLEAN) =
  VAR tn, tNMinus1, tsize, tint: TIntN.T;
  BEGIN
  END string_copy;

PROCEDURE copy (u: U;  n: INTEGER;  type: MType;  overlap: BOOLEAN) =
  (* Mem[s1.A:sz] := Mem[s0.A:sz]; pop(2)*)
  VAR size := CG_Bytes[type];
      forward, end: Label := No_label;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("copy");
      u.wr.Int   (n);
      u.wr.TName (type);
      u.wr.Bool  (overlap);
      u.wr.NL    ();
    END;
  END copy;

PROCEDURE zero_n (u: U;  type_multiple_of_32: IType;  type: MType) =
  (* Mem[s1.A:s0.type_multiple_of_32] := 0; pop(2) *)
(*VAR n: INTEGER;
      shift: TIntN.T;*)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("zero_n");
      u.wr.TName (type_multiple_of_32);
      u.wr.TName (type);
      u.wr.NL    ();
    END;

    <* ASSERT FALSE *>

    (* zero_n is implemented incorrectly in the gcc backend,
     * therefore it must not be used.
    *)
  END zero_n;

PROCEDURE zero (u: U;  n: INTEGER;  type: MType) =
  (* Mem[s0.A:sz] := 0; pop(1) *)
  VAR size := CG_Bytes[type];
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("zero");
      u.wr.Int   (n);
      u.wr.TName (type);
      u.wr.NL    ();
    END;
  END zero;

(*----------------------------------------------------------- conversions ---*)

PROCEDURE loophole (u: U;  from, to: ZType) =
  (* s0.to := LOOPHOLE(s0.from, to) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("loophole");
      u.wr.TName (from);
      u.wr.TName (to);
      u.wr.NL    ();
    END;
  END loophole;

(*------------------------------------------------ traps & runtime checks ---*)

PROCEDURE abort (u: U;  code: RuntimeError) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("abort");
      u.wr.Int   (ORD (code));
      u.wr.NL    ();
    END;
    reportfault(u, code);
  END abort;

PROCEDURE check_nil (u: U;  code: RuntimeError) =
  (* IF (s0.A = NIL) THEN abort(code) *)
  VAR safelab: Label;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("check_nil");
      u.wr.Int   (ORD (code));
      u.wr.NL    ();
    END;
    print("assert(!(" & stack[0] & "))\n");
  END check_nil;

PROCEDURE check_lo (u: U;  type: IType;  READONLY j: Target.Int;  code: RuntimeError) =
  (* IF (s0.type < i) THEN abort(code) *)
  VAR safelab: Label;
      i := TIntN.FromTargetInt(j, CG_Bytes[type]);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("check_lo");
      u.wr.TName (type);
      u.wr.TInt  (i);
      u.wr.Int   (ORD (code));
      u.wr.NL    ();
    END;
    print("if (((type)(" & stack[0] & ")) < i) m3_abort(" & code & ");\n");
  END check_lo;

PROCEDURE check_hi (u: U;  type: IType;  READONLY j: Target.Int;  code: RuntimeError) =
  (* IF (i < s0.type) THEN abort(code) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("check_hi");
      u.wr.TName (type);
      u.wr.TInt  (i);
      u.wr.Int   (ORD (code));
      u.wr.NL    ();
    END;
    print("if (i < ((type)(" & stack[0] & "))) m3_abort(" & code & ");\n");
  END check_hi;

PROCEDURE check_range (u: U;  type: IType;  READONLY xa, xb: Target.Int;  code: RuntimeError) =
  (* IF (s0.type < a) OR (b < s0.type) THEN abort(code) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("check_range");
      u.wr.TInt (a);
      u.wr.TInt (b);
      u.wr.Int   (ORD (code));
      u.wr.NL    ();
    END;
    print("{ const type temp = " & stack[0] & ";\n");
    print("if ((temp < " & a & ") || (temp > " & b & ")) m3_abort(" & code & ");}\n");
  END check_range;

PROCEDURE check_index (u: U;  type: IType;  code: RuntimeError) =
  (* IF NOT (0 <= s1.type < s0.type) THEN
       abort(code)
     END;
     pop *)
  (* s0.type is guaranteed to be positive so the unsigned
     check (s0.W <= s1.W) is sufficient. *)
  VAR safelab: Label;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("check_index");
      u.wr.TName (type);
      u.wr.Int   (ORD (code));
      u.wr.NL    ();
    END;
    print("if ((" & stack[0] & ") <= (" & stack[1])) m3_abort(" & code &");\n");
    pop();
  END check_index;

PROCEDURE check_eq (u: U;  type: IType;  code: RuntimeError) =
  (* IF (s0.type # s1.type) THEN
       abort(code);
       Pop (2) *)
  VAR safelab: Label;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("check_eq");
      u.wr.TName (type);
      u.wr.Int   (ORD (code));
      u.wr.NL    ();
    END;

  END check_eq;

PROCEDURE reportfault (u: U;  code: RuntimeError) =
  (* 32: see M3CG.RuntimeError, RuntimeError.T *)
  VAR info := ORD (code) + u.lineno * 32;
  BEGIN
    <* ASSERT ORD (code) < 32 *> (* lose fault code not ok *)
    (* ASSERT u.lineno <= (LAST(INTEGER) DIV 32) *) (* losing line number ok *)
    u.usedfault := TRUE;
  END reportfault;

PROCEDURE makereportproc (u: U) =
  VAR
    repproc      : Proc;
    repfault     : Var;
    repfoff      : ByteOffset;
    labelname    : TEXT;
    reportsymbol : INTEGER;
  BEGIN
    get_runtime_hook(u, M3ID.Add ("ReportFault"), repproc, repfault, repfoff);

  END makereportproc;

(*---------------------------------------------------- address arithmetic ---*)

PROCEDURE add_offset (u: U; i: INTEGER) =
  (* s0.A := s0.A + i *)
  VAR ti, imm_plus_i: TIntN.T;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("add_offset");
      u.wr.Int   (i);
      u.wr.NL    ();
    END;

    IF NOT TIntN.FromHostInteger(i, Target.Integer.bytes, ti) THEN
      Err(u, "add_offset: failed to convert i to target integer");
    END;

  END add_offset;

PROCEDURE log2 (int: INTEGER): INTEGER =
(* Return log2(int) if int is a power of 2, -1 if it is 0, otherwise -2 *)
  BEGIN
    IF Word.And(int, int-1) # 0 THEN
      RETURN -2;
    END;

    IF int = 0 THEN
      RETURN -1;
    END;

    FOR i := 0 TO 31 DO
      int := Word.Shift(int, -1);
      IF int = 0 THEN
        RETURN i;
      END;
    END;

    RETURN -1;
  END log2;

PROCEDURE index_address (u: U;  type: IType;  size: INTEGER) =
  (* s1.A := s1.A + s0.type * size ; pop *)
  VAR shift: INTEGER;
      neg := FALSE;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("index_address");
      u.wr.TName (type);
      u.wr.Int   (size);
      u.wr.NL    ();
    END;

    IF size = 0 THEN
      Err(u, "size = 0 in index_address");
    END;

    IF size < 0 THEN
      size := -size;
      neg := TRUE;
    END;

    shift := log2(size);

  END index_address;

(*------------------------------------------------------- procedure calls ---*)

PROCEDURE call_64 (u: U; builtin: Builtin) =
  BEGIN

    (* all 64bit helpers pop their parameters, even if they are __cdecl named. *)

    u.call_param_size[u.in_proc_call - 1] := 0;

    (* There is a problem with our register bookkeeping, such
     * that we can't preserve non volatiles across function calls,
     * and we even get confused about volatiles (they
     * should be computed after the function call, not before).
     *)

    call_int_proc (u, builtin);

  END call_64;

PROCEDURE do_rotate_or_shift_64 (u: U; builtin: Builtin) =
  BEGIN
  END do_rotate_or_shift_64;

PROCEDURE start_call_direct (u: U;  p: Proc;  lev: INTEGER;  type: Type) =
  (* begin a procedure call to a procedure at static level 'lev'. *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("start_call_direct");
      u.wr.PName (p);
      u.wr.Int   (lev);
      u.wr.TName (type);
      u.wr.NL    ();
    END;

    (* ASSERT u.in_proc_call < 2 *) (* ? *)

    u.static_link[u.in_proc_call] := NIL;
    u.call_param_size[u.in_proc_call] := 0;
    INC(u.in_proc_call);
  END start_call_direct;

PROCEDURE start_call_indirect (u: U;  type: Type;  cc: CallingConvention) =
  (* begin a procedure call to a procedure at static level 'lev'. *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("start_call_indirect");
      u.wr.TName (type);
      u.wr.Txt   (cc.name);
      u.wr.NL    ();
    END;

    (* ASSERT u.in_proc_call < 2 *) (* ? *)

    u.static_link[u.in_proc_call] := NIL;
    u.call_param_size[u.in_proc_call] := 0;
    INC(u.in_proc_call);
  END start_call_indirect;

PROCEDURE pop_param (u: U;  type: MType) =
  (* pop s0 and make it the "next" parameter in the current call *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("pop_param");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
  END pop_param;

PROCEDURE pop_struct (u: U;  s: ByteSize;  a: Alignment) =
  (* pop s0 and make it the "next" parameter in the current call
   * NOTE that we implement call by value, the struct is
   * copied to temporary space on the machine stack
   *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("pop_struct");
      u.wr.Int   (s);
      u.wr.Int   (a);
      u.wr.NL    ();
    END;

    <* ASSERT u.in_proc_call > 0 *>
    INC(u.call_param_size[u.in_proc_call - 1], s);
  END pop_struct;

PROCEDURE pop_static_link (u: U) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("pop_static_link");
      u.wr.NL    ();
    END;

    <* ASSERT u.in_proc_call > 0 *>
    u.static_link[u.in_proc_call - 1] := declare_temp(u, 4, 4, Type.Addr, FALSE);

  END pop_static_link;

PROCEDURE call_direct (u: U; p: Proc;  type: Type) =
  VAR realproc := NARROW(p, x86Proc);
      call_param_size: TIntN.T;
  (* call the procedure identified by block b.  The procedure
     returns a value of type type. *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("call_direct");
      u.wr.PName (p);
      u.wr.TName (type);
      u.wr.NL    ();
    END;

    <* ASSERT u.in_proc_call > 0 *>

    IF realproc.lev # 0 THEN
      load_static_link_toC(u, p);
    END;

    IF type = Type.Struct THEN
      type := Type.Addr;
    END;

    DEC(u.in_proc_call);
  END call_direct;

PROCEDURE call_indirect (u: U; type: Type;  cc: CallingConvention) =
  (* call the procedure whose address is in s0.A and pop s0.  The
     procedure returns a value of type type. *)
  VAR call_param_size: TIntN.T;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("call_indirect");
      u.wr.TName (type);
      u.wr.Txt   (cc.name);
      u.wr.NL    ();
    END;

    <* ASSERT u.in_proc_call > 0 *>
    u.static_link[u.in_proc_call - 1] := NIL;

    DEC(u.in_proc_call);
  END call_indirect;

(*------------------------------------------- procedure and closure types ---*)

PROCEDURE load_procedure (u: U;  p: Proc) =
  VAR realproc := NARROW(p, x86Proc);
  (* push; s0.A := ADDR (p's body) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_procedure");
      u.wr.PName (p);
      u.wr.NL    ();
    END;
  END load_procedure;

PROCEDURE load_static_link (u: U;  p: Proc) =
  VAR realproc := NARROW(p, x86Proc);
  (* push; s0.A := (static link needed to call p, NIL for top-level procs) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_static_link");
      u.wr.PName (p);
      u.wr.NL    ();
    END;

  END load_static_link;

PROCEDURE load_static_link_toC (u: U;  p: Proc) =
  VAR realproc := NARROW(p, x86Proc);
  (* push; s0.A := (static link needed to call p, NIL for top-level procs) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_static_link_toC");
      u.wr.PName (p);
      u.wr.NL    ();
    END;
  END load_static_link_toC;

(*----------------------------------------------------------------- misc. ---*)

PROCEDURE comment (u: U;  a, b, c, d: TEXT := NIL) =
  VAR i: INTEGER := -1;
  BEGIN
    Cmt (u, a, i);
    Cmt (u, b, i);
    Cmt (u, c, i);
    Cmt (u, d, i);
    Cmt (u, "\n", i);
  END comment;

PROCEDURE Cmt (u: U;  text: TEXT;  VAR width: INTEGER) =
  VAR ch: CHAR;
  BEGIN
    IF (NOT u.debug) OR (text = NIL) THEN
      RETURN
    END;
    FOR i := 0 TO Text.Length (text) - 1 DO
      ch := Text.GetChar (text, i);
      IF (ch = '\n' OR ch = '\r') THEN
        u.wr.OutC (ch);
        width := -1;
      ELSE
        IF (width = -1) THEN
          u.wr.OutT ("\t# ");
          width := 0;
        END;
        u.wr.OutC (ch);
      END
    END;
  END Cmt;


(*--------------------------------------------------------------- atomics ---*)

PROCEDURE store_ordered (x: U; type_multiple_of_32: ZType; type: MType; <*UNUSED*>order: MemoryOrder) =
(* Mem [s1.A].u := s0.type;
   pop (2) *)
  VAR retry: Label;
  BEGIN
    IF x.debug THEN
      x.wr.Cmd   ("store_ordered");
      x.wr.TName (type_multiple_of_32);
      x.wr.TName (type);
      x.wr.NL    ();
    END;
  END store_ordered;

PROCEDURE load_ordered (x: U; type: MType; type_multiple_of_32: ZType; <*UNUSED*>order: MemoryOrder) =
(* s0.type_multiple_of_32 := Mem [s0.A].type  *)
  BEGIN
    IF x.debug THEN
      x.wr.Cmd   ("load_ordered");
      x.wr.TName (type);
      x.wr.TName (type_multiple_of_32);
      x.wr.NL    ();
    END;
  END load_ordered;

PROCEDURE exchange (u: U; type: MType; type_multiple_of_32: ZType; <*UNUSED*>order: MemoryOrder) =
(* tmp := Mem [s1.A + o].type;
   Mem [s1.A + o].type := s0.type_multiple_of_32;
   s0.type_multiple_of_32 := tmp;
   pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("exchange");
      u.wr.TName (type);
      u.wr.TName (type_multiple_of_32);
      u.wr.NL    ();
    END;
  END exchange;

PROCEDURE compare_exchange_helper (x: U; type: Type) =
  BEGIN
  END compare_exchange_helper;

PROCEDURE compare_exchange (x: U; type: MType; type_multiple_of_32: ZType; result_type: IType;
                            <*UNUSED*>success, failure: MemoryOrder) =
(* original := Mem[s2.A].type;
   spurious_failure := whatever;
   IF original = Mem[s1.A].type AND NOT spurious_failure THEN
     Mem [s2.A].type := s0.type_multiple_of_32;
     s2.result_type := 1;
   ELSE
     Mem [s2.A].type := original; x86 really does rewrite the original value, atomically
     s2.result_type := 0;
   END;
   pop(2);
   This is permitted to fail spuriously.
   That is, even if Mem[s2.a] = Mem[s1.a], we might
     still go down the then branch.
*)
  BEGIN

    IF x.debug THEN
      x.wr.Cmd   ("compare_exchange");
      x.wr.TName (type);
      x.wr.TName (type_multiple_of_32);
      x.wr.TName (result_type);
      x.wr.NL    ();
    END;
  END compare_exchange;

PROCEDURE fence (u: U; <*UNUSED*>order: MemoryOrder) =
(*
 * x86: Exchanging any memory with any register is a serializing instruction.
 *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("fence");
      u.wr.NL    ();
    END;
  END fence;

CONST AtomicOpToOp = ARRAY AtomicOp OF Op { Op.oADD, Op.oSUB, Op.oOR, Op.oAND, Op.oXOR };
CONST AtomicOpName = ARRAY AtomicOp OF TEXT { "add", "sub", "or", "and", "xor" };

PROCEDURE fetch_and_op (x: U; atomic_op: AtomicOp; type: MType; type_multiple_of_32: ZType;
                        <*UNUSED*>order: MemoryOrder) =
(* original := Mem [s1.A].type;
   Mem [s1.A].type := original op s0.type_multiple_of_32;
   s1.type_multiple_of_32 := original;
   pop

=> store the new value, return the old value

Generally we use interlocked compare exchange loop.
Some operations can be done better though.
*)
  BEGIN
    IF x.debug THEN
      x.wr.Cmd   ("fetch_and_op");
      x.wr.OutT  (AtomicOpName[atomic_op]);
      x.wr.TName (type);
      x.wr.TName (type_multiple_of_32);
      x.wr.NL    ();
    END;
  END fetch_and_op;

PROCEDURE Err(t: U; err: TEXT) =
  BEGIN
    t.Err(err);
    <* ASSERT FALSE *>
  END Err;

BEGIN
END M3C.
