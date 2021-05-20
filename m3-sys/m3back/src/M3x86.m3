(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Jun 20 16:19:09 PDT 1995 by kalsow     *)
(*      modified on Wed Nov 23 13:57:47 PST 1994 by isard      *)

MODULE M3x86 EXPORTS M3x86, M3x86Rep;

IMPORT Wr, Text, Fmt, IntRefTbl, Word;
IMPORT M3CG, M3ID, M3CG_Ops, Target, TFloat;
IMPORT TIntN, TWordN;
IMPORT M3ObjFile, TargetMap;

FROM TargetMap IMPORT CG_Bytes;

FROM M3CG IMPORT Name, ByteOffset, TypeUID, CallingConvention;
FROM M3CG IMPORT BitSize, ByteSize, Alignment, Frequency;
FROM M3CG IMPORT Var, Proc, Label, No_label, Sign, BitOffset;
FROM M3CG IMPORT Type, ZType, AType, RType, IType, MType, QID, NoQID;
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
  END;

Call_t = RECORD
  param_size := 0;
  static_link: x86Var := NIL;
  next: REF Call_t := NIL;
END;

REVEAL
  U = Public BRANDED "M3x86.U" OBJECT
        rawwr           : Wr.T := NIL;
        wr              : Wrx86.T := NIL;
        cg              : Codex86.T := NIL;
        vstack          : Stackx86.T := NIL;
        obj             : M3ObjFile.T := NIL;
        debug           := FALSE;
        Err             : ErrorHandler := NIL;
        runtime         : IntRefTbl.T := NIL;  (* Name -> RuntimeHook *)
        textsym         := 0;
        init_varstore   : x86Var := NIL;
        init_count      := 0;

        (* calls are never nested; results are stored in temporaries and repushed
        F1(F2(F3()) compiles to
          temp = F3()
          temp = F2(temp)
          F1(temp)
          (whether it is the same temp, is a different matter; I don't know)

          However:
            VAR: a, b: LONGINT;
            F(a * b);
            generates an internal/helper call while calling F,
            so we use a stack of Call_t instead of a fixed sized array.
        *)
        call            : REF Call_t := NIL;
        current_proc    : x86Proc := NIL;
        param_proc      : x86Proc := NIL;
        in_proc         := FALSE;
        procframe_ptr   : ByteOffset;
        exit_proclabel  : Label := -1;
        last_exitbranch := -1;
        n_params        := 0;
        next_var        := 1;
        next_proc       := 1;
        next_scope      := 1;
        builtins        : ARRAY Builtin OF x86Proc;
        global_var      : x86Var := NIL;
        lineno          := 0;
        source_file     : TEXT := NIL;
        reportlabel     : Label;
        usedfault       := FALSE;

        (* Alloca is a special case in various places. *)
        alloca_id       := M3ID.NoID;
        chkstk_id       := M3ID.NoID;
        calling_alloca  := FALSE;
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
        widechar_size := widechar_size; 
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

    u.cg     := Codex86.New (u, u.wr);
    u.vstack := Stackx86.New (u, u.cg, u.debug);

    FOR b := FIRST (u.builtins) TO LAST (u.builtins) DO
      u.builtins[b] := NIL;
    END;

    RETURN u;
  END New;

(*----------------------------------------------------------- ID counters ---*)

PROCEDURE next_label (u: U;  n: INTEGER := 1): Label =
  BEGIN
    RETURN u.cg.reserve_labels(n);
  END next_label;

(*------------------------------------------------ READONLY configuration ---*)

PROCEDURE set_error_handler (u: U; p: ErrorHandler) =
  BEGIN
    u.Err := p;
    u.cg.set_error_handler(p);
    u.vstack.set_error_handler(p);
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

    u.cg.set_obj(u.obj);

    u.cg.init();
    u.vstack.init();

    u.next_var := 1;
    u.next_proc := 1;
    u.next_scope := 1;
    u.global_var := NIL;

    u.reportlabel := u.cg.reserve_labels(1);
    u.usedfault := FALSE;

    FOR b := FIRST (u.builtins) TO LAST (u.builtins) DO
      u.builtins [b] := NIL;
    END;

    (* alloca is the name the front end uses -- as a portable
     * interface to all backends.
     * __chkstack is the actual name.
     *)
    u.alloca_id := M3ID.Add("alloca");
    u.chkstk_id := M3ID.Add("__chkstk");

    u.textsym := u.obj.define_symbol(M3ID.Add("TextSegment"), Seg.Text, 0);
    u.cg.set_textsym(u.textsym);
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

    u.vstack.end();
    u.cg.end();
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

    u.source_file := file;
    u.obj.set_source_file(file);
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

    u.lineno := line;

    u.obj.set_source_line(line);
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

PROCEDURE declare_indirect (u: U;  type, target: TypeUID; <*UNUSED*>target_typename: QID) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_indirect");
      u.wr.Tipe (type);
      u.wr.Tipe (target);
      (* TODO target_typename *)
      u.wr.NL   ();
    END
  END declare_indirect;

PROCEDURE declare_proctype (u: U;  type: TypeUID;  n_formals: INTEGER;
                            result: TypeUID;  n_raises: INTEGER;
                            cc: CallingConvention; <*UNUSED*>result_typename: QID) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_proctype");
      u.wr.Tipe (type);
      u.wr.Int  (n_formals);
      u.wr.Tipe (result);
      u.wr.Int  (n_raises);
      u.wr.Txt  (cc.name);
      (* TODO result_typename *)
      u.wr.NL   ();
    END
  END declare_proctype;

PROCEDURE declare_formal (u: U;  n: Name;  type: TypeUID; <*UNUSED*>typename: QID) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_formal");
      u.wr.ZName (n);
      u.wr.Tipe  (type);
      (* TODO typename *)
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
      e := NEW (RuntimeHook, name := n, proc := NIL);
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

PROCEDURE get_runtime_hook (u: U;  n: Name;
                            VAR p: Proc) =
  VAR e := GetRuntimeHook (u, n);
  BEGIN
    p := e.proc;
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
    v.symbol := u.obj.import_symbol(v.name);
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
    v.symbol := u.obj.define_symbol(v.name, v.seg, 0);
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

    IF exported THEN
      u.obj.export_symbol(realvar.symbol);
    END;

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
    IF inited THEN
      v.symbol := u.obj.define_symbol (v.name, v.seg, 0);
    ELSE
      v.symbol := u.obj.define_bss_symbol (v.name, s, a);
    END;
    IF exported THEN
      u.obj.export_symbol (v.symbol);
    END;
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
                         f: Frequency; <*UNUSED*>typename := NoQID): Var =
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

      IF u.param_proc.import THEN
        u.param_proc.symbol := u.obj.import_symbol(u.param_proc.name);
      ELSE
        u.param_proc.symbol := u.obj.define_symbol(u.param_proc.name, Seg.Text, 0);
      END;

      IF u.param_proc.exported THEN
        u.obj.export_symbol(u.param_proc.symbol);
      END
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
  VAR realvar := NARROW(v, x86Var);
      offs, pad: INTEGER;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("begin_init");
      u.wr.VName (v);
      u.wr.NL    ();
    END;

    <* ASSERT u.init_varstore = NIL *>

    u.init_varstore := realvar;

    offs := u.obj.cursor(realvar.seg);

    IF Word.And(offs, realvar.var_align - 1) # 0 THEN
      pad := realvar.var_align - Word.And(offs, realvar.var_align - 1);
      INC(offs, pad);
      IF Word.And(pad, 3) # 0 THEN
        u.obj.append(realvar.seg, 0, Word.And(pad, 3));
        pad := Word.And(pad, 16_FFFFFFFC);
      END;

      pad := pad DIV 4;
      FOR i := 1 TO pad DO
        u.obj.append(realvar.seg, 0, 4);
      END
    END;

    u.obj.move_symbol(realvar.symbol, offs);

    u.init_count := 0;
  END begin_init;

PROCEDURE end_init (u: U;  v: Var) =
  VAR realvar := NARROW(v, x86Var);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("end_init");
      u.wr.VName (v);
      u.wr.NL    ();
    END;

    <* ASSERT v = u.init_varstore *>

    pad_init(u, realvar.var_size);
    u.init_varstore := NIL;
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

    pad_init(u, o);
    u.obj.appendBytes(u.init_varstore.seg, SUBARRAY(value, 0, CG_Bytes[type]));
    INC(u.init_count, CG_Bytes[type]);

  END init_int;

PROCEDURE init_proc (u: U;  o: ByteOffset;  value: Proc) =
  VAR realproc := NARROW(value, x86Proc);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("init_proc");
      u.wr.Int   (o);
      u.wr.PName (value);
      u.wr.NL    ();
    END;

    pad_init(u, o);

    u.obj.append(u.init_varstore.seg, 0, 4);
    INC(u.init_count, 4);

    u.obj.relocate(u.init_varstore.symbol, o, realproc.symbol);
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

    u.cg.log_label_init(u.init_varstore, o, value);

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

    u.obj.append(u.init_varstore.seg, bias, 4);
    INC(u.init_count, 4);

    u.obj.relocate(u.init_varstore.symbol, o, realvar.symbol);
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

    u.obj.append(u.init_varstore.seg, realvar.offset, 4);
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
      FOR i := 0 TO len - 1 DO
        u.obj.append(u.init_varstore.seg, ORD(Text.GetChar(value, i)), 1);
      END;

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

    FOR i := 0 TO size - 1 DO
      u.obj.append(u.init_varstore.seg, flarr[i], 1);
      INC(u.init_count);
    END;
  END init_float;

PROCEDURE pad_init (u: U; o: ByteOffset) =
  BEGIN
    <* ASSERT u.init_count <= o *>
    <* ASSERT o <= u.init_varstore.var_size *>

    FOR i := u.init_count TO o - 1 DO
      u.obj.append(u.init_varstore.seg, 0, 1);
    END;

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

(* This should be in m3middle, as it must agree with m3front,
   and is potentially useful by other backends. Alternatively
   attach a boolean/flags/enum to all procs (i.e. in import_procedure);
   alternatively, RTHooks? *)
PROCEDURE IsAlloca (u: U;  n: Name;  n_params: INTEGER;
                    ret_type: Type;  cc: CallingConvention): BOOLEAN =
BEGIN
  RETURN n = u.alloca_id (* The main criteria is the name. *)
    AND n_params = 1     (* Also check its signature. *)
    AND ret_type = Type.Addr
    AND cc.m3cg_id = Target.CDECL;
END IsAlloca;

PROCEDURE import_procedure (u: U;  n: Name;  n_params: INTEGER;
                            ret_type: Type;  cc: CallingConvention;
                            <*UNUSED*>return_typeid: TypeUID := 0;
                            <*UNUSED*>return_typename := NoQID): Proc =
  VAR p := NewProc (u, n, n_params, ret_type, cc);
  BEGIN
    p.import := TRUE;

    u.n_params := n_params;

    IF IsAlloca (u, n, n_params, ret_type, cc) THEN
      p.is_alloca := TRUE;   (* Alloca is repeatedly a special case. Leave a mark for other code. *)
      p.name := u.chkstk_id; (* Alloca is actually __chkstk. *)
      p.symbol := u.obj.import_symbol(p.name);
    ELSE
      IF (n_params = 0 OR NOT p.stdcall) AND Text.Length(M3ID.ToText(n)) > 0 THEN
        p.name := mangle_procname(p.name, 0, p.stdcall);
        p.symbol := u.obj.import_symbol(p.name);
      END;
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
                             cc: CallingConvention; exported: BOOLEAN;
                             parent: Proc;
                             <*UNUSED*>return_typeid: TypeUID;
                             <*UNUSED*>return_typename: QID): Proc =
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
      p.symbol := u.obj.define_symbol(p.name, Seg.Text, 0);
      IF exported THEN u.obj.export_symbol(p.symbol); END
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

    u.vstack.clearall ();

    <* ASSERT NOT u.in_proc *>
    u.in_proc := TRUE;

    u.current_proc := p;
    u.cg.set_current_proc(p);
    u.vstack.set_current_proc(p);
    u.last_exitbranch := -1;
    u.exit_proclabel := -1;

    (* Mark non-volatiles as not used, until known otherwise. *)

    u.proc_reguse[EBX] := FALSE;
    u.proc_reguse[ESI] := FALSE;
    u.proc_reguse[EDI] := FALSE;

    realproc.offset := u.obj.cursor(Seg.Text);
    realproc.bound := TRUE;

    WHILE realproc.usage # NIL DO
      u.obj.patch(Seg.Text, realproc.usage.loc, realproc.offset -
                  (realproc.usage.loc + 4), 4);
      realproc.usage := realproc.usage.link;
    END;

    u.obj.move_symbol(realproc.symbol, realproc.offset);

    u.obj.begin_procedure(realproc.symbol);

    u.cg.pushOp(u.cg.reg[EBP]);
    u.cg.movOp(u.cg.reg[EBP], u.cg.reg[ESP]);

    u.cg.immOp(Op.oSUB, u.cg.reg[ESP], TWordN.Max16);
    u.procframe_ptr := u.obj.cursor(Seg.Text) - 4;

    u.cg.pushOp(u.cg.reg[EBX]);
    u.cg.pushOp(u.cg.reg[ESI]);
    u.cg.pushOp(u.cg.reg[EDI]);

    IF u.current_proc.lev # 0 THEN
      u.cg.store_ind(u.cg.reg[ECX], u.cg.reg[EBP], -4, Type.Addr);
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

    u.obj.patch(Seg.Text, u.procframe_ptr, u.current_proc.framesize, 4);

    u.in_proc := FALSE;

    u.obj.end_procedure(realproc.symbol);

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
    u.cg.set_label(label);

    u.vstack.clearall();
  END set_label;

PROCEDURE jump (u: U; label: Label) =
  (* GOTO label *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("jump");
      u.wr.Lab   (label);
      u.wr.NL    ();
    END;

    u.cg.brOp(Cond.Always, label);
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

    u.vstack.doimm (Op.oCMP, TZero, FALSE);
    u.cg.brOp (Cond.NZ, label);
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

    u.vstack.doimm (Op.oCMP, TZero, FALSE);
    u.cg.brOp (Cond.Z, label);
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

    CASE type OF
    | Type.Word32, Type.Int32, Type.Word64, Type.Int64, Type.Addr =>
        u.vstack.unlock();
        IF u.vstack.dobin(Op.oCMP, TRUE, FALSE, type) THEN
          cond := revcond[cond];
        END;
    | Type.Reel, Type.LReel, Type.XReel =>
        IF u.cg.ftop_inmem THEN
          u.cg.binFOp (FOp.fCOMP, 1);
        ELSE
          u.cg.binFOp (FOp.fCOMPP, 1);
          cond := revcond[cond];
        END;
        u.vstack.discard (2);
        u.vstack.unlock ();
        u.vstack.corrupt (EAX, operandPart := 0);
        u.cg.noargFOp (FOp.fNSTSWAX);
        u.cg.noargOp (Op.oSAHF);
    END;

    CASE type OF
    | Type.Word32, Type.Word64, Type.Addr,
      Type.Reel, Type.LReel, Type.XReel => (* FCOM sets the unsigned compare flags *)
        cond := unscond[cond];
    ELSE
    END;

    u.cg.brOp(cond, label);
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

    stack0 := u.vstack.pos(0, "case_jump");
    u.vstack.unlock();
    u.vstack.find(stack0, Force.anyreg);
    u.cg.case_jump(u.vstack.op(stack0), labels);
    u.vstack.discard(1);
  END case_jump;

PROCEDURE exit_proc (u: U; type: Type) =
  (* Returns s0.type if type is not Void, otherwise returns no value. *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("exit_proc");
      u.wr.TName (type);
      u.wr.NL    ();
    END;

    IF type # Type.Void THEN
      u.vstack.unlock();

      WITH stack0 = u.vstack.pos(0, "exit_proc") DO
        IF Target.FloatType[type] THEN
          u.cg.f_exitproc();
        ELSIF TypeIs64(type) THEN
          u.vstack.find(stack0, Force.regset, RegSet { EAX, EDX });
        ELSE
          u.vstack.find(stack0, Force.regset, RegSet { EAX });
        END
      END;

      u.vstack.discard(1);
    END;

    IF u.exit_proclabel = -1 THEN
      u.exit_proclabel := u.cg.reserve_labels(1, FALSE);
    END;

    u.last_exitbranch := u.obj.cursor(Seg.Text);

    u.cg.brOp(Cond.Always, u.exit_proclabel);
  END exit_proc;

PROCEDURE procedure_epilogue (u: U) =
  CONST NOP = 16_90;
  BEGIN
    IF u.exit_proclabel = -1 THEN
      RETURN;
      (* Strange as it may seem, some procedures have no exit points. *)
    END;

    IF u.last_exitbranch = u.obj.cursor(Seg.Text) - 5 THEN
      (* Don't generate a branch to the epilogue at the last exit
         point of the procedure *)
      u.cg.set_label(u.exit_proclabel, offset := -5);
      u.obj.backup(Seg.Text, 5);
    ELSE
      u.cg.set_label(u.exit_proclabel);
    END;

    IF u.proc_reguse[EDI] THEN
      u.cg.popOp(u.cg.reg[EDI]);
    ELSE
      u.obj.patch(Seg.Text, u.procframe_ptr + 6, NOP, 1);
    END;

    IF u.proc_reguse[ESI] THEN
      u.cg.popOp(u.cg.reg[ESI]);
    ELSE
      u.obj.patch(Seg.Text, u.procframe_ptr + 5, NOP, 1);
    END;

    IF u.proc_reguse[EBX] THEN
      u.cg.popOp(u.cg.reg[EBX]);
    ELSE
      u.obj.patch(Seg.Text, u.procframe_ptr + 4, NOP, 1);
    END;

    u.cg.noargOp(Op.oLEAVE);
    IF u.current_proc.stdcall THEN
      u.cg.cleanretOp(u.current_proc.paramsize - 8);
    ELSE
      u.cg.noargOp(Op.oRET);
    END
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

    u.vstack.push(MVar {var := v, mvar_offset := o, mvar_type := type});
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

    u.vstack.pop(MVar {var := v, mvar_offset := o, mvar_type := type});
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

    u.vstack.doloadaddress(v, o);
  END load_address;

PROCEDURE load_indirect (u: U;  o: ByteOffset;  type: MType;  type_multiple_of_32: ZType) =
(* s0.type_multiple_of_32 := Mem [s0.A + o].type  *)
  VAR newreg: ARRAY OperandPart OF Regno;
      size: OperandSize;
      regset: RegSet;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_indirect");
      u.wr.Int   (o);
      u.wr.TName (type);
      u.wr.TName (type_multiple_of_32);
      u.wr.NL    ();
    END;

    <* ASSERT CG_Bytes[type_multiple_of_32] >= CG_Bytes[type] *>

    u.vstack.unlock();

    WITH stack0 = u.vstack.pos(0, "load_indirect") DO
      u.vstack.find(stack0, Force.anyreg, AllRegisters, TRUE);
      IF Target.FloatType [type] THEN
        u.cg.f_loadind(u.vstack.op(stack0), o, type);
        u.vstack.dealloc_reg(stack0, operandPart := 0);
        u.vstack.set_fstack(stack0);
      ELSE
        size := GetTypeSize(type);
        <* ASSERT size = GetTypeSize(type_multiple_of_32) *>

        (* allocate the registers *)

        IF CG_Bytes[type] = 1 THEN
          <* ASSERT size = 1 *>
          regset := RegistersForByteOperations;
        ELSE
          regset := AllRegisters;
        END;

        FOR i := 0 TO size - 1 DO
          newreg[i] := u.vstack.freereg(regset, operandPart := i);
          regset := (regset - RegSet{newreg[i]});
          <* ASSERT newreg[i] # -1 *>
        END;

        (* do the loads *)

        FOR i := 0 TO size - 1 DO
          u.cg.load_ind(newreg[i], u.vstack.op(stack0), o + 4 * i, type);
        END;

        (* do the bookkeeping about the loads *)
        (* previous contents of stack0 was just an address, no loop over size *)

        u.vstack.dealloc_reg(stack0, operandPart := 0);
        FOR i := 0 TO size - 1 DO
          u.vstack.set_reg(stack0, newreg[i], operandPart := i);
        END;
      END;
      u.vstack.set_type(stack0, type_multiple_of_32);
    END
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

    u.vstack.unlock();
    WITH (* stack0 = u.vstack.pos(0, "store_indirect"), *)
         stack1 = u.vstack.pos(1, "store_indirect") DO
      IF Target.FloatType [type] THEN
        u.vstack.find(stack1, Force.anyreg, AllRegisters, TRUE);
        u.cg.f_storeind(u.vstack.op(stack1), o, type);
        u.vstack.discard(2);
      ELSE
        u.vstack.dostoreind(o, type);
      END
    END
  END store_indirect;

(*-------------------------------------------------------------- literals ---*)

PROCEDURE load_nil (u: U) =
  (* push ; s0.A := a *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_nil");
      u.wr.NL    ();
    END;

    u.vstack.unlock();
    u.vstack.pushimmT(TZero, Type.Addr);
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

    u.vstack.unlock();
    u.vstack.pushimmT(i, type);
  END load_integer;

PROCEDURE load_float    (u: U;  type: RType;  READONLY f: Target.Float) =
  (* push ; s0.type := f *)
  VAR flarr: FloatBytes;
      size: INTEGER;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_float");
      u.wr.TName (type);
      u.wr.Flt   (f);
      u.wr.NL    ();
    END;

    u.vstack.pushnew(type, Force.any);
    size := TFloat.ToBytes(f, flarr);
    IF size # CG_Bytes[type] THEN
      Err(u, "Floating size mismatch in load_float");
    END;
    u.cg.f_loadlit(flarr, type);
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

    IF Target.FloatType [type] THEN
      condset(u, cond, type);
      RETURN;
    END;

    IF TypeIsSigned(type) OR op IN SET OF CompareOp{CompareOp.EQ, CompareOp.NE} THEN
      u.vstack.unlock();
      r := u.vstack.freereg(RegistersForByteOperations, operandPart := 0);
      u.cg.binOp(Op.oXOR, u.cg.reg[r], u.cg.reg[r]);
      reversed := u.vstack.dobin(Op.oCMP, TRUE, FALSE, type);
      IF reversed THEN
        cond := revcond[cond];
      END;
      u.cg.setccOp(u.cg.reg[r], cond);
    ELSE
      u.vstack.unlock();
      IF op IN SET OF CompareOp{CompareOp.LE, CompareOp.GT} THEN
        u.vstack.swap();
        IF op = CompareOp.LE THEN
          op := CompareOp.GE;
        ELSE
          op := CompareOp.LT;
        END;
      END;
      reversed := u.vstack.dobin(Op.oCMP, FALSE, FALSE, type);
      <* ASSERT NOT reversed *>
      u.vstack.unlock();
      r := u.vstack.freereg(operandPart := 0);
      u.cg.binOp(Op.oSBB, u.cg.reg[r], u.cg.reg[r]);
      IF op = CompareOp.LT THEN
        u.cg.unOp(Op.oNEG, u.cg.reg[r]);
      ELSE
        u.cg.incOp(u.cg.reg[r]);
      END;
    END;
    u.vstack.unlock();
    u.vstack.pushnew(Type.Word32, Force.regset, RegSet{r});
  END compare;

PROCEDURE add (u: U;  type: AType) =
  (* s1.type := s1.type + s0.type ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("add");
      u.wr.TName (type);
      u.wr.NL    ();
    END;

    IF Target.FloatType [type] THEN
      u.cg.binFOp(FOp.fADDP, 1);
      u.vstack.discard(1);
    ELSE
      u.vstack.unlock();
      EVAL u.vstack.dobin(Op.oADD, TRUE, TRUE, type);
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

    IF Target.FloatType [type] THEN
      u.cg.binFOp(FOp.fSUBP, 1);
      u.vstack.discard(1);
    ELSE
      u.vstack.unlock();
      EVAL u.vstack.dobin(Op.oSUB, FALSE, TRUE, type);
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

    IF TypeIs64(type) THEN
      start_int_proc (u, Builtin.mul64);
      pop_param(u, Type.Word64);
      pop_param(u, Type.Word64);
      call_64 (u, Builtin.mul64);
    ELSIF Target.FloatType [type] THEN
      u.cg.binFOp(FOp.fMUL, 1);
      u.vstack.discard(1);
    ELSIF type = Type.Int32 THEN
      u.vstack.doimul();
    ELSE
      <* ASSERT type = Type.Word32 *>
      u.vstack.doumul();
    END
  END multiply;

PROCEDURE divide (u: U;  type: RType) =
  (* s1.type := s1.type / s0.type ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("divide");
      u.wr.TName (type);
      u.wr.NL    ();
    END;

    u.cg.binFOp(FOp.fDIV, 1);
    u.vstack.discard(1);
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

    IF TypeIs64(type) THEN
      CASE type OF Type.Int64  => builtin := Builtin.div64;
              | Type.Word64 => builtin := Builtin.udiv64;
              ELSE <* ASSERT FALSE *>
      END;
      u.vstack.swap();
      start_int_proc (u, builtin);
      pop_param(u, Type.Word64);
      pop_param(u, Type.Word64);
      call_64 (u, builtin);
      RETURN;
    END;

    IF TypeIsUnsigned(type) THEN
      a := Sign.Positive;
      b := Sign.Positive;
    END;

    u.vstack.dodiv(a, b);
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

    IF TypeIs64(type) THEN
      CASE type OF Type.Int64  => builtin := Builtin.mod64;
              | Type.Word64 => builtin := Builtin.umod64;
              ELSE <* ASSERT FALSE *>
      END;
      u.vstack.swap();
      start_int_proc (u, builtin);
      pop_param(u, Type.Word64);
      pop_param(u, Type.Word64);
      call_64 (u, builtin);
      RETURN;
    END;

    IF TypeIsUnsigned(type) THEN
      a := Sign.Positive;
      b := Sign.Positive;
    END;

    u.vstack.domod(a, b);
  END mod;

PROCEDURE negate (u: U;  type: AType) =
  (* s0.type := - s0.type *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("negate");
      u.wr.TName (type);
      u.wr.NL    ();
    END;

    IF Target.FloatType [type] THEN
      u.cg.noargFOp(FOp.fCHS);
    ELSE
      u.vstack.doneg();
    END
  END negate;

PROCEDURE abs      (u: U;  type: AType) =
  (* s0.type := ABS (s0.type) (noop on Words) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("abs");
      u.wr.TName (type);
      u.wr.NL    ();
    END;

    IF TypeIsUnsigned(type) THEN
      RETURN;
    ELSIF TypeIsSigned(type) THEN
      u.vstack.doabs();
    ELSE
      u.cg.noargFOp(FOp.fABS);
    END
  END abs;

PROCEDURE max      (u: U;  type: ZType) =
  (* s1.type := MAX (s1.type, s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("max");
      u.wr.TName (type);
      u.wr.NL    ();
    END;

    u.vstack.domaxmin(type, MaxMin.Max);
  END max;

PROCEDURE min      (u: U;  type: ZType) =
  (* s1.type := MIN (s1.type, s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("min");
      u.wr.TName (type);
      u.wr.NL    ();
    END;

    u.vstack.domaxmin(type, MaxMin.Min);
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

    u.vstack.fltoint(ConvertOpKind [op], x);
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

    IF Target.FloatType [type] THEN
      RETURN;
    END;

    u.vstack.inttoflt();
  END cvt_float;

(*------------------------------------------------------------------ sets ---*)

PROCEDURE set_op3(u: U;  s: ByteSize; builtin: Builtin) =
  (* s2.B := s1.B op s0.B ; pop(3) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   (BuiltinDesc[builtin].name);
      u.wr.Int   (s);
      u.wr.NL    ();
    END;

    start_int_proc (u, builtin);
    load_stack_param (u, Type.Addr, 2);
    load_stack_param (u, Type.Addr, 1);
    load_stack_param (u, Type.Addr, 0);
    u.vstack.discard (3);
    u.vstack.pushimmI (s * 8, Type.Word32);
    pop_param (u, Type.Word32);
    call_int_proc (u, builtin);
  END set_op3;

PROCEDURE set_union (u: U;  s: ByteSize) =
  (* s2.B := s1.B + s0.B ; pop(3) *)
  BEGIN
    set_op3(u, s, Builtin.set_union);
  END set_union;

PROCEDURE set_difference (u: U;  s: ByteSize) =
  (* s2.B := s1.B - s0.B ; pop(3) *)
  BEGIN
    set_op3(u, s, Builtin.set_difference);
  END set_difference;

PROCEDURE set_intersection (u: U;  s: ByteSize) =
  (* s2.B := s1.B * s0.B ; pop(3) *)
  BEGIN
    set_op3(u, s, Builtin.set_intersection);
  END set_intersection;

PROCEDURE set_sym_difference (u: U;  s: ByteSize) =
  (* s2.B := s1.B / s0.B ; pop(3) *)
  BEGIN
    set_op3(u, s, Builtin.set_sym_difference);
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

    u.vstack.unlock();

    WITH stack0 = u.vstack.pos(0, "set_member"),
         stack1 = u.vstack.pos(1, "set_member") DO

      (* Better would be:
      IF u.vstack.loc(stack0) # OLoc.imm OR TWordN.GT(u.vstack.op(stack0).imm, TWordN.Max8) THEN
        u.vstack.find(stack0, Force.anyreg);
      ELSE
        u.vstack.find(stack0, Force.any);
      END;
      u.vstack.find(stack1, Force.any);

      but we don't have things quite right, so settle.
      *)
      u.vstack.find(stack0, Force.anyreg);
      u.vstack.find(stack1, Force.anyreg);

      u.cg.bitTestOp(u.vstack.op(stack1), u.vstack.op(stack0));
      u.vstack.discard(2);
    END;

    u.vstack.unlock();

    (* see the end of condset

    u.vstack.pushnew(Type.Word8, Force.mem);
    WITH stop0 = u.vstack.op(u.vstack.pos(0, "set_singleton")) DO
      stop0.mvar.var.stack_temp := FALSE;
      u.cg.setccOp(stop0, Cond.B);          B = unsigned below = C = carry
    END;

    4 instructions:

    0000003F: 0F 92 45 F0        setb        byte ptr [ebp-10h]
    00000043: 33 D2              xor         edx,edx
    00000045: 8A 55 F0           mov         dl,byte ptr [ebp-10h]
    00000048: 89 55 F4           mov         dword ptr [ebp-0Ch],edx

    Let's try something else.
    Goal is to capture the carry flag as a boolean in a Word.
    *)

    (* Convert carry to register-sized boolean. *)

    u.vstack.pushnew(Type.Word32, Force.anyreg);
    WITH stop0 = u.vstack.op(u.vstack.pos(0, "set_member")) DO
      u.cg.binOp(Op.oSBB, stop0, stop0); (* 0 if carry was clear, -1 if carry was set *)
      u.cg.unOp(Op.oNEG, stop0);         (* 0 if carry was clear,  1 if carry was set *)
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

    IF op = CompareOp.EQ OR op = CompareOp.NE THEN
      proc := Builtin.memcmp;
      start_int_proc (u, proc);
      u.vstack.pushimmI(s, Type.Word32);
      pop_param(u, Type.Word32);
      pop_param(u, Type.Addr);
      pop_param(u, Type.Addr);
      call_int_proc (u, proc);

      (* If EAX = 0, we want 1, if EAX # 0, we want 0.
       * Compile this and match it:
       * int F1();
       * int F2() { return F1() == 0; }
       * int F3() { return F1() != 0; }
       *)
      u.cg.unOp(Op.oNEG, u.cg.reg[EAX]);
      u.cg.binOp(Op.oSBB, u.cg.reg[EAX], u.cg.reg[EAX]);
      IF op = CompareOp.EQ THEN
        u.cg.incOp(u.cg.reg[EAX]);
      ELSE
        u.cg.unOp(Op.oNEG, u.cg.reg[EAX]);
      END
    ELSE
      proc := CompareOpProc [op];
      start_int_proc (u, proc);
      u.vstack.swap();
      pop_param(u, Type.Addr);
      pop_param(u, Type.Addr);
      u.vstack.pushimmI(s * 8, Type.Word32);
      pop_param(u, Type.Word32);
      call_int_proc (u, proc);
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

    start_int_proc (u, Builtin.set_range);
    load_stack_param(u, Type.Addr, 2);
    load_stack_param(u, type, 1);
    pop_param(u, type);
    u.vstack.discard(2);
    call_int_proc (u, Builtin.set_range);
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

    (* bit test and set -- we don't care about the test *)

    WITH stack0 = u.vstack.pos(0, "set_singleton"),
         stack1 = u.vstack.pos(1, "set_singleton") DO

      u.vstack.unlock();

      (* single byte constants can be immediate
       * but the front end never generates that
       *)

      <* ASSERT u.vstack.loc(stack0) # OLoc.imm *>

      (*IF u.vstack.loc(stack0) # OLoc.imm OR TWordN.GT(u.vstack.op(stack0).imm, TWordN.Max8) THEN*)
        u.vstack.find(stack0, Force.anyreg);
      (*ELSE*)
        (*u.vstack.find(stack0, Force.any);*)
      (*END;*)
      u.vstack.find(stack1, Force.any);
      u.cg.bitTestAndSetOp(u.vstack.op(stack1), u.vstack.op(stack0));
      u.vstack.discard(2);
    END

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

    WITH stack0 = u.vstack.pos(0, "not") DO
      IF u.vstack.loc(stack0) = OLoc.imm THEN
        TWordN.Not (u.vstack.op(stack0).imm, not);
        u.vstack.set_imm(stack0, not);
      ELSE
        u.vstack.unlock();
        u.vstack.find(stack0, Force.anytemp);
        u.cg.unOp(Op.oNOT, u.vstack.op(stack0));
        u.vstack.newdest(u.vstack.op(stack0));
      END
    END
  END not;

PROCEDURE and (u: U;  type: IType) =
  (* s1.type := Word.And (s1.type, s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("and");
      u.wr.TName (type);
      u.wr.NL    ();
    END;

    u.vstack.unlock();
    EVAL u.vstack.dobin(Op.oAND, TRUE, TRUE, type);
  END and;

PROCEDURE or  (u: U;  type: IType) =
  (* s1.type := Word.Or  (s1.type, s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("or");
      u.wr.TName (type);
      u.wr.NL    ();
    END;

    u.vstack.unlock();
    EVAL u.vstack.dobin(Op.oOR, TRUE, TRUE, type);
  END or;

PROCEDURE xor (u: U;  type: IType) =
  (* s1.type := Word.Xor (s1.type, s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("xor");
      u.wr.TName (type);
      u.wr.NL    ();
    END;

    u.vstack.unlock();
    EVAL u.vstack.dobin(Op.oXOR, TRUE, TRUE, type);
  END xor;

PROCEDURE shift_left   (u: U;  type: IType) =
  (* s1.type := Word.Shift  (s1.type, s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("shift_left");
      u.wr.TName (type);
      u.wr.NL    ();
    END;

    EVAL u.vstack.doshift (type, ShiftType.LeftAlreadyBounded);
  END shift_left;

PROCEDURE shift_right  (u: U;  type: IType) =
  (* s1.type := Word.Shift  (s1.type, -s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("shift_right");
      u.wr.TName (type);
      u.wr.NL    ();
    END;

    EVAL u.vstack.doshift (type, ShiftType.RightAlreadyBounded);
  END shift_right;

PROCEDURE shift (u: U;  type: IType) =
  (* s1.type := Word.Shift  (s1.type, s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("shift");
      u.wr.TName (type);
      u.wr.NL    ();
    END;

    EVAL u.vstack.doshift (type, ShiftType.UnboundedPositiveIsLeft);
  END shift;

PROCEDURE rotate (u: U;  type: IType) =
  (* s1.type := Word.Rotate (s1.type, s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("rotate");
      u.wr.TName (type);
      u.wr.NL    ();
    END;

    IF u.vstack.dorotate(type) THEN
      RETURN;
    END;

    do_rotate_or_shift_64 (u, Builtin.rotate64);
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

    u.vstack.unlock();
    WITH stack0 = u.vstack.pos(0, "rotate_left"),
         stack1 = u.vstack.pos(1, "rotate_left") DO
      IF u.vstack.loc(stack0) = OLoc.imm THEN
        IF u.vstack.loc(stack1) = OLoc.imm THEN
          IF NOT TIntN.ToHostInteger(u.vstack.op(stack0).imm, rotateCount) THEN
            Err(u, "unable to convert rotate count to host integer");
          END;
          TWordN.Rotate(u.vstack.op(stack1).imm, rotateCount, rotate);
          u.vstack.set_imm(stack1, rotate);
        ELSE
          TWordN.And(u.vstack.op(stack0).imm, MaximumShift[type], and);
          u.vstack.set_imm(stack0, and);
          IF TypeIs64(type) THEN
            do_rotate_or_shift_64(u, Builtin.rotate_left64);
            RETURN;
          END;
          u.vstack.find(stack1, Force.anytemp);
          u.cg.immOp(Op.oROL, u.vstack.op(stack1), u.vstack.op(stack0).imm);
          u.vstack.newdest(u.vstack.op(stack1));
        END
      ELSE
        IF TypeIs64(type) THEN
          do_rotate_or_shift_64(u, Builtin.rotate_left64);
          RETURN;
        END;
        u.vstack.find(stack0, Force.regset, RegSet {ECX});
        u.vstack.find(stack1, Force.anytemp);

        IF u.vstack.loc(stack1) = OLoc.imm THEN
          u.vstack.find(stack1, Force.anyreg);
        END;

        u.cg.unOp(Op.oROL, u.vstack.op(stack1));
        u.vstack.newdest(u.vstack.op(stack1));
      END;

      u.vstack.discard(1);
    END
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

    u.vstack.unlock();
    WITH stack0 = u.vstack.pos(0, "rotate_right"),
         stack1 = u.vstack.pos(1, "rotate_right") DO
      IF u.vstack.loc(stack0) = OLoc.imm THEN
        IF u.vstack.loc(stack1) = OLoc.imm THEN
          IF NOT TIntN.ToHostInteger(u.vstack.op(stack0).imm, rotateCount) THEN
            Err(u, "unable to convert rotate count to host integer");
          END;
          TWordN.Rotate(u.vstack.op(stack1).imm, -rotateCount, rotate);
          u.vstack.set_imm(stack1, rotate);
        ELSE
          TWordN.And(u.vstack.op(stack0).imm, MaximumShift[type], and);
          u.vstack.set_imm(stack0, and);
          IF TypeIs64(type) THEN
            do_rotate_or_shift_64(u, Builtin.rotate_right64);
            RETURN;
          END;
          u.vstack.find(stack1, Force.anytemp);
          u.cg.immOp(Op.oROR, u.vstack.op(stack1), u.vstack.op(stack0).imm);
          u.vstack.newdest(u.vstack.op(stack1));
        END
      ELSE
        IF TypeIs64(type) THEN
          do_rotate_or_shift_64(u, Builtin.rotate_right64);
          RETURN;
        END;
        u.vstack.find(stack0, Force.regset, RegSet {ECX});
        u.vstack.find(stack1, Force.anytemp);

        IF u.vstack.loc(stack1) = OLoc.imm THEN
          u.vstack.find(stack1, Force.anyreg);
        END;

        u.cg.unOp(Op.oROR, u.vstack.op(stack1));
        u.vstack.newdest(u.vstack.op(stack1));
      END;

      u.vstack.discard(1);
    END
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

    u.vstack.doextract(type, sign_extend);
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

    u.vstack.doextract_n(type, sign_extend, n);
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

    u.vstack.doextract_mn(type, sign_extend, m, n);
  END extract_mn;

PROCEDURE insert  (u: U;  type: IType) =
  (* s3.type := Word.Insert (s3.type, s2.type, s1.type, s0.type) ; pop(3) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("insert");
      u.wr.TName (type);
      u.wr.NL    ();
    END;

    u.vstack.doinsert(type);
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

    u.vstack.doinsert_n(type, n);
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

    u.vstack.doinsert_mn(type, m, n);
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

    u.vstack.swap();
  END swap;

PROCEDURE pop (u: U;  type: Type) =
  (* pop(1) (i.e. discard s0) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("pop");
      u.wr.TName (type);
      u.wr.NL    ();
    END;

    u.vstack.unlock();

    IF Target.FloatType [type] THEN
      WITH stack0 = u.vstack.pos(0, "pop") DO
        <* ASSERT u.vstack.loc(stack0) = OLoc.fstack *>
        u.cg.fstack_discard();
      END
    END;

    u.vstack.discard(1);
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

    WITH stack0 = u.vstack.pos(0, "copy_n") DO
      IF u.vstack.loc(stack0) = OLoc.imm THEN
        IF NOT TIntN.ToHostInteger(u.vstack.op(stack0).imm, n) THEN
          Err(u, "copy_n: unable to convert to host integer");
        END;
        u.vstack.discard(1);
        copy(u, n, type, overlap);
        RETURN;
      END
    END;

    IF CG_Bytes[type] # 1 THEN
      WITH stack0 = u.vstack.pos(0, "copy_n") DO
        u.vstack.unlock();

        CASE CG_Bytes[type] OF
          2 => shift := TIntN.One;
        | 4 => shift := TIntN.Two;
        | 8 => shift := TIntN.Three;
        ELSE
          Err(u, "Unknown MType size in copy_n");
        END;

        u.vstack.find(stack0, Force.anyreg);

        u.cg.immOp(Op.oSHL, u.vstack.op(stack0), shift);
      END
    END;

    start_int_proc (u, mover);
    pop_param (u, type_multiple_of_32);
    pop_param (u, Type.Addr);
    pop_param (u, Type.Addr);
    call_int_proc (u, mover);

    u.vstack.discard(1);
  END copy_n;

CONST MAXINLINECOPY = 8;

CONST faketype = ARRAY [1 .. 4] OF MType
  { Type.Word8, Type.Word16, Type.Word32, Type.Word32 };

PROCEDURE inline_copy (u: U; n, size: INTEGER; forward: BOOLEAN) =
  VAR start, end, step: INTEGER;
      movereg: Regno;
  BEGIN
    IF forward THEN
      start := 0; end := n - 1; step := 1;
    ELSE
      start := n - 1; end := 0; step := -1;
    END;

    movereg := u.vstack.freereg(operandPart := 0);

    WITH stop0 = u.vstack.op(u.vstack.pos(0, "inline_copy")),
         stop1 = u.vstack.op(u.vstack.pos(1, "inline_copy")) DO
      FOR i := start TO end BY step DO
        u.cg.fast_load_ind(movereg, stop0, i * size, size);
        u.cg.store_ind(u.cg.reg[movereg], stop1, i * size, faketype[size]);
      END
    END
  END inline_copy;

PROCEDURE string_copy (u: U; n, size: INTEGER; forward: BOOLEAN) =
  VAR tn, tNMinus1, tsize, tint: TIntN.T;
  BEGIN
    u.vstack.corrupt(ECX, operandPart := 0);
    u.cg.movImmI(u.cg.reg[ECX], n);

    IF forward THEN
      u.cg.noargOp(Op.oCLD);
    ELSE
      IF NOT TIntN.FromHostInteger(n, Target.Integer.bytes, tn) THEN
        Err(u, "string_copy: unable to convert n to target int");
      END;
      IF NOT TIntN.FromHostInteger(size, Target.Integer.bytes, tsize) THEN
        Err(u, "string_copy: unable to convert size to target int");
      END;
      IF NOT TIntN.Subtract(tn, TIntN.One, tNMinus1) THEN
        Err(u, "string_copy: Subtract overflowed");
      END;
      IF NOT TIntN.Multiply(tNMinus1, tsize, tint) THEN
        Err(u, "string_copy: Multiply overflowed");
      END;
      u.cg.immOp(Op.oADD, u.cg.reg[ESI], tint);
      u.cg.immOp(Op.oADD, u.cg.reg[EDI], tint);
      u.cg.noargOp(Op.oSTD);
    END;

    u.cg.noargOp(Op.oREP);
    CASE size OF
      1 => u.cg.noargOp(Op.oMOVSB);
    | 2 => u.cg.MOVSWOp();
    | 4 => u.cg.noargOp(Op.oMOVSD);
    ELSE
      Err(u, "Illegal size in copy");
    END;

    IF NOT forward THEN
      u.cg.noargOp(Op.oCLD);
    END
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

    IF size = 1 AND Word.And(n, 3) = 0 THEN
      n := Word.Shift(n, -2);
      size := 4;
    END;

    IF size = 2 AND Word.And(n, 1) = 0 THEN
      n := Word.Shift(n, -1);
      size := 4;
    END;

    IF size = 8 THEN
      n := Word.Shift(n, 1);
      size := 4;
    END;

    u.vstack.unlock();

    WITH stack0 = u.vstack.pos(0, "copy"), stack1 = u.vstack.pos(1, "copy") DO
      IF n > MAXINLINECOPY THEN
        u.vstack.find(stack0, Force.regset, RegSet { ESI } );
        u.vstack.find(stack1, Force.regset, RegSet { EDI } );
        u.proc_reguse[ESI] := TRUE;
        u.proc_reguse[EDI] := TRUE;
      ELSE
        u.vstack.find(stack0, Force.anyreg, AllRegisters, TRUE);
        u.vstack.find(stack1, Force.anyreg, AllRegisters, TRUE);
      END
    END;

    IF overlap AND n > 1 THEN
      forward := u.cg.reserve_labels(1, TRUE);
      end := u.cg.reserve_labels(1, TRUE);
      u.cg.binOp(Op.oCMP, u.cg.reg[ESI], u.cg.reg[EDI]);
      u.cg.brOp(Cond.GE, forward);

      IF n <= MAXINLINECOPY THEN
        inline_copy(u, n, size, FALSE);
      ELSE
        string_copy(u, n, size, FALSE);
      END;

      u.cg.brOp(Cond.Always, end);
      u.cg.set_label(forward);
    END;

    IF n <= MAXINLINECOPY THEN
      inline_copy(u, n, size, TRUE);
    ELSE
      string_copy(u, n, size, TRUE);
    END;

    IF overlap AND n > 1 THEN
      u.cg.set_label(end);
    END;

    IF n > MAXINLINECOPY THEN
      u.vstack.newdest(u.cg.reg[ESI]);
      u.vstack.newdest(u.cg.reg[EDI]);
    END;

    u.vstack.discard(2);
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

    WITH stack0 = u.vstack.pos(0, "zero_n") DO
      IF u.vstack.loc(stack0) = OLoc.imm THEN
        IF NOT TIntN.ToHostInteger(u.vstack.op(stack0).imm, n) THEN
          Err(u, "zero_n: unable to convert to host integer");
        END;
        u.vstack.discard(1);
        zero(u, n, type);
        RETURN;
      END
    END;

    IF CG_Bytes[type] # 1 THEN
      WITH stack0 = u.vstack.pos(0, "zero_n") DO
        u.vstack.unlock();
        u.vstack.find(stack0, Force.anyreg);

        CASE CG_Bytes[type] OF
          2 => shift := TIntN.One;
        | 4 => shift := TIntN.Two;
        | 8 => shift := TIntN.Three;
        ELSE
          Err(u, "Unknown MType size in zero_n");
        END;

        u.cg.immOp(Op.oSHL, u.vstack.op(stack0), shift);
      END
    END;

    start_int_proc (u, Builtin.memset);
    pop_param (u, type_multiple_of_32);
    u.vstack.pushimmT (TZero, Type.Word32);
    pop_param (u, Type.Word32);
    pop_param (u, Type.Addr);
    call_int_proc (u, Builtin.memset);

    u.vstack.discard(1);
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

    IF size = 1 AND Word.And(n, 3) = 0 THEN
      n := Word.Shift(n, -2);
      size := 4;
    END;

    IF size = 2 AND Word.And(n, 1) = 0 THEN
      n := Word.Shift(n, -1);
      size := 4;
    END;

    IF size = 8 THEN
      n := Word.Shift(n, 1);
      size := 4;
    END;

    u.vstack.unlock();

    IF n > MAXINLINECOPY THEN
      u.vstack.find(u.vstack.pos(0, "zero"), Force.regset, RegSet { EDI } );
      u.vstack.corrupt(EAX, operandPart := 0);
      u.vstack.corrupt(ECX, operandPart := 0);

      u.cg.binOp(Op.oXOR, u.cg.reg[EAX], u.cg.reg[EAX]);
      u.cg.movImmI(u.cg.reg[ECX], n);

      u.cg.noargOp(Op.oCLD);
      u.cg.noargOp(Op.oREP);
      CASE size OF
        1 => u.cg.noargOp(Op.oSTOSB);
      | 2 => u.cg.STOSWOp();
      | 4 => u.cg.noargOp(Op.oSTOSD);
      ELSE
             Err(u, "Illegal size in zero");
      END;
      u.vstack.newdest(u.cg.reg[EDI]);

    ELSE
      WITH stack0 = u.vstack.pos(0, "zero"),
           stop0 = u.vstack.op(stack0) DO
        u.vstack.find(stack0, Force.anyreg, AllRegisters, TRUE);
        FOR i := 0 TO n - 1 DO
          u.cg.store_ind(Operand { loc := OLoc.imm, imm := TZero, optype := type },
                         stop0, i * size, faketype[size]);
        END
      END
    END;

    u.vstack.discard(1);
  END zero;

(*----------------------------------------------------- internal procedures ---*)

TYPE
  Builtin = {
    set_union, set_difference, set_intersection, set_sym_difference,
    set_range, set_lt, set_le, set_gt, set_ge,
    memmove, memcpy, memset, memcmp,
    mul64,
    udiv64, umod64,
    div64, mod64,
    rotate_left64, rotate_right64, rotate64
  };

(* union .. sym_difference -> (n_bits, *c, *b, *a): Void
   range                   -> (b, a, *s): Void
   eq .. ge                -> (n_bits, *b, *a): Int
   member                  -> (elt, *set): Int
   singleton               -> (a, *s): Void *)

TYPE
  BP = RECORD
    name     : TEXT;
    n_params : INTEGER; (* counted in 32bit words *)
    ret_type : Type;
    lang     : INTEGER; (* Target.STDCALL or TARGET.CDECL *)
  END;

VAR
  BuiltinDesc := ARRAY Builtin OF BP {
    BP { "set_union",          4, Type.Void,  Target.STDCALL },
    BP { "set_difference",     4, Type.Void,  Target.STDCALL },
    BP { "set_intersection",   4, Type.Void,  Target.STDCALL },
    BP { "set_sym_difference", 4, Type.Void,  Target.STDCALL },
    BP { "set_range",          3, Type.Void,  Target.STDCALL },
    BP { "set_lt",             3, Type.Int32, Target.STDCALL },
    BP { "set_le",             3, Type.Int32, Target.STDCALL },
    BP { "set_gt",             3, Type.Int32, Target.STDCALL },
    BP { "set_ge",             3, Type.Int32, Target.STDCALL },
    BP { "memmove",            3, Type.Addr,  Target.CDECL },
    BP { "memcpy",             3, Type.Addr,  Target.CDECL },
    BP { "memset",             3, Type.Addr,  Target.CDECL },
    BP { "memcmp",             3, Type.Int32, Target.CDECL },

    (* custom calling convention: parameters pushed, removed
     * by callee, but name is not __stdcall, call_64 pokes
     * the parameter size to 0
     *)
    BP { "_allmul",          0, Type.Word64, Target.CDECL }, (* 64bit multiply; signed or unsigned *)
    BP { "_aulldiv",         0, Type.Word64, Target.CDECL }, (* 64bit unsigned divide *)
    BP { "_aullrem",         0, Type.Word64, Target.CDECL }, (* 64bit unsigned mod/remainder *)

    BP { "m3_div64",         4, Type.Int64,  Target.STDCALL },
    BP { "m3_mod64",         4, Type.Int64,  Target.STDCALL },
    BP { "m3_rotate_left64", 3, Type.Word64, Target.STDCALL },
    BP { "m3_rotate_right64",3, Type.Word64, Target.STDCALL },
    BP { "m3_rotate64",      3, Type.Word64, Target.STDCALL }
  };

PROCEDURE start_int_proc (u: U;  b: Builtin) =
  BEGIN
    WITH proc = u.builtins[b],
         desc = BuiltinDesc [b] DO
      IF proc = NIL THEN
        proc := import_procedure (u, M3ID.Add (desc.name),
                                  desc.n_params, desc.ret_type,
                                  Target.ConventionFromID (desc.lang));
        FOR i := 1 TO desc.n_params DO
          EVAL declare_param (u, M3ID.NoID, 4, 4, Type.Word32, 0, FALSE, FALSE, 100);
        END;
      END;
      start_call_direct (u, proc, 0, desc.ret_type);
    END;
  END start_int_proc;

PROCEDURE call_int_proc (u: U;  b: Builtin) =
  BEGIN
    call_direct (u, u.builtins[b], BuiltinDesc[b].ret_type);
  END call_int_proc;

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

    u.vstack.doloophole(from, to);

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

    u.vstack.unlock();
    WITH stack0 = u.vstack.pos(0, "check_nil") DO
      IF u.vstack.loc(stack0) = OLoc.imm THEN
        IF TIntN.EQ(u.vstack.op(stack0).imm, TZero) THEN
          reportfault(u, code);
        END
      ELSE
        u.vstack.find(stack0, Force.anyreg, AllRegisters, TRUE);

        IF NOT u.vstack.non_nil(u.vstack.reg(stack0)) THEN
          u.cg.immOp(Op.oCMP, u.vstack.op(stack0), TZero);
          safelab := u.cg.reserve_labels(1, TRUE);
          u.cg.brOp(Cond.NE, safelab);
          reportfault(u, code);
          u.cg.set_label(safelab);
        END;

        u.vstack.set_non_nil(u.vstack.reg(stack0));
      END;
    END;
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

    u.vstack.unlock();
    WITH stack0 = u.vstack.pos(0, "check_lo") DO
      IF u.vstack.loc(stack0) = OLoc.imm THEN
        IF TIntN.LT(u.vstack.op(stack0).imm, i) THEN
          reportfault(u, code);
        END
      ELSE
        u.vstack.find(stack0, Force.anyreg);
        IF TIntN.GE(u.vstack.lower(u.vstack.reg(stack0)), i) THEN
          (* ok *)
        ELSIF TIntN.LT(u.vstack.upper(u.vstack.reg(stack0)), i) THEN
          reportfault(u, code);
        ELSE
          u.cg.immOp(Op.oCMP, u.vstack.op(stack0), i);
          safelab := u.cg.reserve_labels(1, TRUE);
          u.cg.brOp(Cond.GE, safelab);
          reportfault(u, code);
          u.cg.set_label(safelab);
          u.vstack.set_lower(u.vstack.reg(stack0), i);
        END
      END
    END
  END check_lo;

PROCEDURE check_hi (u: U;  type: IType;  READONLY j: Target.Int;  code: RuntimeError) =
  (* IF (i < s0.type) THEN abort(code) *)
  VAR safelab: Label;
      i := TIntN.FromTargetInt(j, CG_Bytes[type]);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("check_hi");
      u.wr.TName (type);
      u.wr.TInt  (i);
      u.wr.Int   (ORD (code));
      u.wr.NL    ();
    END;

    u.vstack.unlock();
    WITH stack0 = u.vstack.pos(0, "check_hi") DO
      IF u.vstack.loc(stack0) = OLoc.imm THEN
        IF TIntN.LT(i, u.vstack.op(stack0).imm) THEN
          reportfault(u, code);
        END
      ELSE
        u.vstack.find(stack0, Force.anyreg);
        IF TIntN.LE(u.vstack.upper(u.vstack.reg(stack0)), i) THEN
          (* ok *)
        ELSIF TIntN.GT(u.vstack.lower(u.vstack.reg(stack0)), i) THEN
          reportfault(u, code);
        ELSE
          u.cg.immOp(Op.oCMP, u.vstack.op(stack0), i);
          safelab := u.cg.reserve_labels(1, TRUE);
          u.cg.brOp(Cond.LE, safelab);
          reportfault(u, code);
          u.cg.set_label(safelab);
          u.vstack.set_upper(u.vstack.reg(stack0), i);
        END
      END
    END
  END check_hi;

PROCEDURE check_range (u: U;  type: IType;  READONLY xa, xb: Target.Int;  code: RuntimeError) =
  (* IF (s0.type < a) OR (b < s0.type) THEN abort(code) *)
  VAR lo, hi: TIntN.T;
      safelab, outrange: Label;
      a := TIntN.FromTargetInt(xa, CG_Bytes[type]);
      b := TIntN.FromTargetInt(xb, CG_Bytes[type]);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("check_range");
      u.wr.TInt (a);
      u.wr.TInt (b);
      u.wr.Int   (ORD (code));
      u.wr.NL    ();
    END;

    u.vstack.unlock();
    WITH stack0 = u.vstack.pos(0, "check_range") DO
      IF u.vstack.loc(stack0) = OLoc.imm THEN
        lo := u.vstack.op(stack0).imm;
        IF TIntN.LT(lo, a) OR TIntN.LT(b, lo) THEN
          reportfault(u, code);
        END;
        RETURN;
      END;

      u.vstack.find(stack0, Force.anyreg);
      WITH reg = u.vstack.reg(stack0) DO
        lo := u.vstack.lower(reg);
        hi := u.vstack.upper(reg);
        IF TIntN.LE(a, lo) AND TIntN.LE(hi, b) THEN
          (* ok *)
        ELSIF TIntN.LT(hi, a) OR TIntN.LT(b, lo) THEN
          reportfault(u, code);
        ELSIF TIntN.LE(hi, b) THEN
          check_lo(u, type, xa, code);
        ELSIF TIntN.GE(lo, a) THEN
          check_hi(u, type, xb, code);
        ELSIF TIntN.EQ(a, TZero) THEN
          (* 0 <= x <= b  ==>   UNSIGNED(x) <= b *)
          safelab := u.cg.reserve_labels(1, TRUE);
          u.cg.immOp(Op.oCMP, u.vstack.op(stack0), b);
          u.cg.brOp(unscond [Cond.LE], safelab);
          reportfault(u, code);
          u.cg.set_label(safelab);
          u.vstack.set_upper(reg, b);
          u.vstack.set_lower(reg, a);
        ELSE
          safelab := u.cg.reserve_labels(1, TRUE);
          outrange := u.cg.reserve_labels(1, TRUE);
          u.cg.immOp(Op.oCMP, u.vstack.op(stack0), a);
          u.cg.brOp(Cond.L, outrange);
          u.cg.immOp(Op.oCMP, u.vstack.op(stack0), b);
          u.cg.brOp(Cond.LE, safelab);
          u.cg.set_label(outrange);
          reportfault(u, code);
          u.cg.set_label(safelab);
          u.vstack.set_upper(reg, b);
          u.vstack.set_lower(reg, a);
        END;
      END
    END
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

    u.vstack.unlock();
    WITH stack0 = u.vstack.pos(0, "check_index"),
         stack1 = u.vstack.pos(1, "check_index") DO
      IF u.vstack.loc(stack0) = OLoc.imm AND
         u.vstack.loc(stack1) = OLoc.imm THEN
        IF TWordN.LE(u.vstack.op(stack0).imm, u.vstack.op(stack1).imm) THEN
          reportfault(u, code);
        END
      ELSE

        u.vstack.find(stack0, Force.any);
        u.vstack.find(stack1, Force.anyregimm);
        IF u.vstack.loc(stack0) = OLoc.mem THEN
          u.vstack.find(stack0, Force.anyregimm);
        END;

        safelab := u.cg.reserve_labels(1, TRUE);

        IF u.vstack.loc(stack0) = OLoc.imm THEN
          u.cg.binOp(Op.oCMP, u.vstack.op(stack1), u.vstack.op(stack0));
          u.cg.brOp(Cond.B, safelab);
        ELSE
          u.cg.binOp(Op.oCMP, u.vstack.op(stack0), u.vstack.op(stack1));
          u.cg.brOp(Cond.A, safelab);
        END;

        reportfault(u, code);
        u.cg.set_label(safelab);
      END;
    END;

    u.vstack.discard(1);
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

    u.vstack.unlock();
    WITH stack0 = u.vstack.pos(0, "check_index"),
         stack1 = u.vstack.pos(1, "check_index") DO
      u.vstack.find(stack0, Force.any);
      u.vstack.find(stack1, Force.anyregimm);
      IF u.vstack.loc(stack0) = OLoc.mem THEN
        u.vstack.find(stack0, Force.anyregimm);
      END;

      IF u.vstack.loc(stack0) = OLoc.imm THEN
        u.cg.binOp(Op.oCMP, u.vstack.op(stack1), u.vstack.op(stack0));
      ELSE
        u.cg.binOp(Op.oCMP, u.vstack.op(stack0), u.vstack.op(stack1));
      END;

      safelab := u.cg.reserve_labels(1, TRUE);
      u.cg.brOp(Cond.E, safelab);
      reportfault(u, code);
      u.cg.set_label(safelab);
    END;

    u.vstack.discard(2);
  END check_eq;

PROCEDURE reportfault (u: U;  code: RuntimeError) =
  (* 32: see M3CG.RuntimeError, RuntimeError.T *)
  VAR info := ORD (code) + u.lineno * 32;
  BEGIN
    <* ASSERT ORD (code) < 32 *> (* lose fault code not ok *)
    (* ASSERT u.lineno <= (LAST(INTEGER) DIV 32) *) (* losing line number ok *)
    u.cg.movImmI(u.cg.reg[EAX], info);
    u.cg.intCall(u.reportlabel);
    u.usedfault := TRUE;
  END reportfault;

PROCEDURE makereportproc (u: U) =
  VAR
    repproc      : Proc;
    labelname    : TEXT;
    reportsymbol : INTEGER;
  BEGIN
    get_runtime_hook(u, M3ID.Add ("ReportFault"), repproc);

    u.cg.set_label(u.reportlabel);

    labelname := M3ID.ToText (u.global_var.name) & "_CRASH";

    reportsymbol := u.obj.define_symbol(M3ID.Add(labelname), Seg.Text,
                                        u.obj.cursor(Seg.Text));
    u.obj.begin_procedure(reportsymbol);

    u.cg.pushOp(u.cg.reg[EBP]);
    u.cg.movOp(u.cg.reg[EBP], u.cg.reg[ESP]);

    u.cg.pushOp(u.cg.reg[EAX]);  (* runtime error code + line number *)

    IF (repproc # NIL) THEN
      start_call_direct(u, repproc, 0, Type.Void);
      INC(u.call.param_size, 4); (* remember error code *)
      load_address(u, u.global_var, 0);
      pop_param(u, Type.Addr);
      call_direct(u, repproc, Type.Void);
    ELSE
      u.Err ("cannot locate the runtime procedure: ReportFault !")
    END;

    u.obj.end_procedure(reportsymbol);
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

    u.vstack.unlock();
    WITH stack0 = u.vstack.pos(0, "add_offset") DO
      IF u.vstack.loc(stack0) = OLoc.imm THEN
        IF NOT TIntN.Add(u.vstack.op(stack0).imm, ti, imm_plus_i) THEN
          Err(u, "add_offset: Add overflowed");
        END;
        u.vstack.set_imm(stack0, imm_plus_i);
      ELSE
        u.vstack.find(stack0, Force.anytemp, AllRegisters, TRUE);

        u.cg.immOp(Op.oADD, u.vstack.op(stack0), ti);

        u.vstack.newdest(u.vstack.op(stack0));
      END
    END
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

    u.vstack.doindex_address(shift, size, neg);
  END index_address;

(*------------------------------------------------------- procedure calls ---*)

PROCEDURE PushCall(u: U) =
BEGIN
  u.call := NEW(REF Call_t, next := u.call);
END PushCall;

PROCEDURE PopCall(u: U) =
BEGIN
  u.call := u.call.next;
END PopCall;

PROCEDURE call_64 (u: U; builtin: Builtin) =
  BEGIN

    (* all 64bit helpers pop their parameters, even if they are __cdecl named. *)

    u.call.param_size := 0;

    (* There is a problem with our register bookkeeping, such
     * that we can't preserve non volatiles across function calls,
     * and we even get confused about volatiles (they
     * should be computed after the function call, not before).
     *)

    u.vstack.all_to_mem(); (* hack *)

    call_int_proc (u, builtin);

  END call_64;

PROCEDURE do_rotate_or_shift_64 (u: U; builtin: Builtin) =
  BEGIN
    start_int_proc (u, builtin);
    pop_param(u, Type.Word32); (* shift count *)
    pop_param(u, Type.Word64); (* value to shift *)
    call_64 (u, builtin);
  END do_rotate_or_shift_64;

PROCEDURE start_call_direct (u: U;  p: Proc;  lev: INTEGER;  type: Type) =
  (* begin a procedure call to a procedure at static level 'lev'. *)
  VAR proc := NARROW(p, x86Proc);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("start_call_direct");
      u.wr.PName (p);
      u.wr.Int   (lev);
      u.wr.TName (type);
      u.wr.NL    ();
    END;

    PushCall(u);
    <* ASSERT u.calling_alloca = FALSE *>
    u.calling_alloca := proc.is_alloca;
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

    PushCall(u);
    <* ASSERT u.calling_alloca = FALSE *>
    u.calling_alloca := FALSE;
  END start_call_indirect;

PROCEDURE pop_param (u: U;  type: MType) =
  (* pop s0 and make it the "next" parameter in the current call *)
  BEGIN
    (*IF u.debug THEN
      u.wr.Cmd   ("pop_param");
      u.wr.TName (type);
      u.wr.NL    ();
    END;*)

    load_stack_param(u, type, 0);
    u.vstack.discard(1);

  END pop_param;

PROCEDURE load_stack_param (u: U; type: MType; depth: INTEGER) =
  (* make value at vstack[depth] the next parameter in the current call *)
  VAR opA: ARRAY OperandPart OF Operand;
      size: OperandSize;
  BEGIN

    IF u.debug THEN
      u.wr.Cmd   ("load_stack_param");
      u.wr.TName (type);
      u.wr.Int   (depth);
      u.wr.NL    ();
    END;

    u.vstack.unlock();

    <* ASSERT u.call # NIL *>

    WITH stack = u.vstack.pos(depth, "load_stack_param") DO
      IF Target.FloatType [type] THEN
        <* ASSERT u.calling_alloca = FALSE *> (* Alloca returns a pointer *)
        <* ASSERT depth = 0 *>
        IF type = Type.Reel THEN
          u.cg.immOp(Op.oSUB, u.cg.reg[ESP], TIntN.Four);
        ELSE
          u.cg.immOp(Op.oSUB, u.cg.reg[ESP], TIntN.Eight);
        END;
        u.cg.f_storeind(u.cg.reg[ESP], 0, type);
      ELSE
        IF u.calling_alloca THEN (* Alloca takes its first and only parameter in eax. *)
          <* ASSERT u.call.param_size = 0 *>
          u.vstack.find(stack, Force.regset, RegSet { EAX });
        ELSE
          u.vstack.find(stack, Force.anyregimm);
          size := SplitOperand(u.vstack.op(stack), opA);
          FOR i := size - 1 TO 0 BY -1 DO
            u.cg.pushOp(opA[i]);
          END;
        END;
      END;
    END;

    IF NOT u.calling_alloca THEN (* Alloca does not pass parameters on the stack (eax instead). *)

      <* ASSERT CG_Bytes[type] <= 4 OR CG_Bytes[type] = 8 *>
      IF CG_Bytes[type] <= 4 THEN
        INC(u.call.param_size, 4);
      ELSE
        INC(u.call.param_size, 8);
      END

    END;

  END load_stack_param;

PROCEDURE pop_struct (u: U;  type: TypeUID;  s: ByteSize;  a: Alignment) =
  (* pop s0 and make it the "next" parameter in the current call
   * NOTE that we implement call by value, the struct is
   * copied to temporary space on the machine stack
   *)
  VAR ts: TIntN.T;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("pop_struct");
      u.wr.Tipe  (type);
      u.wr.Int   (s);
      u.wr.Int   (a);
      u.wr.NL    ();
    END;

    <* ASSERT u.calling_alloca = FALSE *>

    <* ASSERT u.call # NIL *>

    (* round struct size up to multiple of 4 or 8 *)

    <* ASSERT a <= 4 OR a = 8 *>
    IF a <= 4 THEN
      s := Word.And(s + 3, 16_FFFFFFFC);
    ELSE
      s := Word.And(s + 7, Alignmask[8]);
    END;

    u.vstack.unlock();

    WITH stack0 = u.vstack.pos(0, "pop_struct") DO

      IF NOT TIntN.FromHostInteger(s, Target.Integer.bytes, ts) THEN
        Err(u, "pop_struct: unable to convert s to target int");
      END;

      (* if the struct is "large", use rep mov to copy it to the machine stack *)

      IF TIntN.GT(ts, TIntN.ThirtyTwo) THEN
        u.cg.immOp(Op.oSUB, u.cg.reg[ESP], ts);

        u.vstack.find(stack0, Force.regset, RegSet { ESI });
        u.vstack.corrupt(EDI, operandPart := 0);
        u.vstack.corrupt(ECX, operandPart := 0);

        u.cg.movOp(u.cg.reg[EDI], u.cg.reg[ESP]);
        u.cg.movImmI(u.cg.reg[ECX], s DIV 4);

        u.cg.noargOp(Op.oCLD);
        u.cg.noargOp(Op.oREP);
        u.cg.noargOp(Op.oMOVSD);

        u.vstack.newdest(u.cg.reg[ESI]);
      ELSE

        (* if the struct is "small", use a few load/push to copy it to the machine stack *)

        u.vstack.find(stack0, Force.anyreg, AllRegisters, TRUE);

        WITH temp = u.vstack.freereg(operandPart := 0) DO
          FOR i := 1 TO (s DIV 4) DO
            u.cg.load_ind(temp, u.vstack.op(stack0), s - (i * 4), Type.Word32);
            u.cg.pushOp(u.cg.reg[temp]);
          END
        END
      END
    END;

    u.vstack.discard(1);

    INC(u.call.param_size, s);
  END pop_struct;

PROCEDURE pop_static_link (u: U) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("pop_static_link");
      u.wr.NL    ();
    END;

    <* ASSERT u.calling_alloca = FALSE *> (* Alloca does not take a static link. *)

    <* ASSERT u.call # NIL *>

    u.call.static_link := declare_temp(u, 4, 4, Type.Addr, FALSE);

    u.vstack.pop(MVar {var := u.call.static_link,
                       mvar_offset := 0, mvar_type := Type.Addr} );
  END pop_static_link;

PROCEDURE TypeIs64 (type: Type): BOOLEAN =
  BEGIN
    RETURN type IN (SET OF Type{Type.Int64, Type.Word64});
  END TypeIs64;

PROCEDURE TypeIsUnsigned (type: Type): BOOLEAN =
  BEGIN
    RETURN type IN (SET OF Type{Type.Word32, Type.Word64});
  END TypeIsUnsigned;

PROCEDURE TypeIsSigned (type: Type): BOOLEAN =
  BEGIN
    RETURN type IN (SET OF Type{Type.Int32, Type.Int64});
  END TypeIsSigned;

PROCEDURE SplitMVar(READONLY mvar: MVar; VAR mvarA: ARRAY OperandPart OF MVar): OperandSize =
  VAR type := mvar.mvar_type;
  BEGIN
    mvarA[0] := mvar;
    IF NOT TypeIs64(type) THEN
      RETURN 1;
    END;
    mvarA[1] := mvar;
    IF mvar.var # NIL THEN
      (* <* ASSERT mvar.var.var_size = CG_Bytes[type] *> *)
    END;
    INC(mvarA[1].mvar_offset, 4);
    mvarA[0].mvar_type := Type.Word32;      (* low part of 64bit integer is always unsigned *)
    IF type = Type.Int64 THEN
      mvarA[1].mvar_type := Type.Int32;     (* high part signedness is same as unsplit type *)
    ELSIF type = Type.Word64 THEN
      mvarA[1].mvar_type := Type.Word32;    (* high part signedness is same as unsplit type *)
    ELSE
      <* ASSERT FALSE *>
    END;
    RETURN 2;
  END SplitMVar;

PROCEDURE SplitImm(type: Type; READONLY imm: TIntN.T; VAR immA: ARRAY OperandPart OF TIntN.T): OperandSize =
  BEGIN
    TWordN.And(imm, TWordN.Max32, immA[0]);
    TWordN.RightShift(imm, 32, immA[1]);
    RETURN GetTypeSize(type);
  END SplitImm;

PROCEDURE GetTypeSize(type: Type): OperandSize =
(* In "words" or "registers": 1 or 2 *)
  BEGIN
    RETURN 1 + ORD(TypeIs64(type));
  END GetTypeSize;

PROCEDURE GetOperandSize(READONLY op: Operand): OperandSize =
  BEGIN
    RETURN GetTypeSize(op.optype);
  END GetOperandSize;

PROCEDURE SplitOperand(READONLY op: Operand; VAR opA: ARRAY OperandPart OF Operand): OperandSize =
  VAR type := op.optype;
      mvarA: ARRAY OperandPart OF MVar;
      immA: ARRAY OperandPart OF TIntN.T;
  BEGIN
    opA[0] := op;

    IF GetTypeSize(type) = 1 THEN
      RETURN 1;
    END;

    opA[1] := op;
    opA[0].optype := Type.Word32;     (* low part of 64bit integer is always unsigned *)
    IF type = Type.Int64 THEN
      opA[1].optype := Type.Int32;    (* high part signedness is same as unsplit type *)
    ELSIF type = Type.Word64 THEN
      opA[1].optype := Type.Word32;   (* high part signedness is same as unsplit type *)
    ELSE
      <* ASSERT FALSE *>
    END;

    CASE op.loc OF
    | OLoc.fstack => <* ASSERT FALSE *>
    | OLoc.imm =>
        EVAL SplitImm(type, op.imm, immA);
        opA[0].imm := immA[0];
        opA[1].imm := immA[1];
    | OLoc.register =>
        opA[0].reg[0] := op.reg[0];
        opA[1].reg[0] := op.reg[1];
    | OLoc.mem =>
        EVAL SplitMVar(op.mvar, mvarA);
        opA[0].mvar := mvarA[0];
        opA[1].mvar := mvarA[1];
    END;
    RETURN 2;
  END SplitOperand;

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

    <* ASSERT u.call # NIL *>

    IF realproc.lev # 0 THEN
      load_static_link_toC(u, p);
    END;

    u.vstack.releaseall();

    IF realproc.import THEN
      u.cg.absCall(p);
    ELSE
      IF realproc.bound THEN
        u.cg.relCall(realproc.offset - (u.obj.cursor(Seg.Text) + 5));
      ELSE
        u.cg.relCall(0);
        realproc.usage := NEW(ProcList, loc := u.obj.cursor(Seg.Text) - 4,
                              link := realproc.usage);
      END
    END;

    IF (NOT realproc.stdcall) (* => caller cleans *)
       AND u.call.param_size > 0 THEN

        IF NOT TIntN.FromHostInteger(u.call.param_size, Target.Integer.bytes, call_param_size) THEN
          Err(u, "call_direct: unable to convert param_size to target integer");
        END;
        u.cg.immOp(Op.oADD, u.cg.reg[ESP], call_param_size);
    END;

    IF type = Type.Struct THEN
      type := Type.Addr;
    END;

    IF u.calling_alloca THEN (* Alloca returns its value in esp. *)
      (* Ideally:
       *   u.vstack.pushnew(type, Force.regset, RegSet { ESP });
       * but pushnew does not like that currently, so burn
       * an instruction in an already slow path.
       *)
      u.cg.movOp(u.cg.reg[EAX], u.cg.reg[ESP]);
    END;

    IF type # Type.Void THEN
      IF Target.FloatType [type] THEN
        u.vstack.pushnew(type, Force.any);
        u.cg.f_pushnew();
      ELSIF TypeIs64(type) THEN
        u.vstack.pushnew(type, Force.regset, RegSet { EAX, EDX });
      ELSE
        u.vstack.pushnew(FixReturnValue(u, type), Force.regset, RegSet { EAX });
      END
    END;

    PopCall(u);
    u.calling_alloca := FALSE;
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

    <* ASSERT u.calling_alloca = FALSE *>
    <* ASSERT u.call # NIL *>

    u.vstack.releaseall();

    IF u.call.static_link # NIL THEN
      (*u.vstack.corrupt(ECX, operandPart := 0);*)
      u.cg.movOp(u.cg.reg[ECX],
                 Operand { loc := OLoc.mem, optype := Type.Addr,
                           mvar :=
                             MVar { var := u.call.static_link,
                                    mvar_offset := 0,
                                    mvar_type := Type.Addr } } );
      free_temp(u, u.call.static_link);
      u.call.static_link := NIL;
    END;

    u.cg.rmCall(u.vstack.op(u.vstack.pos(0, "call_indirect")));
    u.vstack.discard(1);

    IF (cc.m3cg_id = 0) AND u.call.param_size > 0 THEN

      (* caller-cleans calling convention *)

      IF NOT TIntN.FromHostInteger(u.call.param_size, Target.Integer.bytes, call_param_size) THEN
        Err(u, "call_indirect: unable to convert param_size to target integer");
      END;

      u.cg.immOp(Op.oADD, u.cg.reg[ESP], call_param_size);
    END;

    IF type = Type.Struct THEN
      type := Type.Addr;
    END;

    IF type # Type.Void THEN
      IF Target.FloatType [type] THEN
        u.vstack.pushnew(type, Force.any);
        u.cg.f_pushnew();
      ELSIF TypeIs64(type) THEN
        u.vstack.pushnew(type, Force.regset, RegSet { EAX, EDX });
      ELSE
        u.vstack.pushnew(FixReturnValue(u, type), Force.regset, RegSet { EAX });
      END
    END;

    PopCall(u);
  END call_indirect;

PROCEDURE FixReturnValue (u: U;  type: Type): Type =
  (* The Microsoft C compiler does not return full 32-bit values in EAX
     for functions that return 8-bit return types.
     Likewise for 16-bit return types prior to Visual C++ 5.0.
     This code generator assumes that registers always contain 32-bit values.
     We compensate here. *)
  BEGIN
    CASE type OF
    | Type.Int8 => (* 8-bit signed integer *)
        u.cg.CBWOp ();            (* AX  := SIGN-EXTEND (AL) *)
        u.cg.noargOp (Op.oCWDE);  (* EAX := SIGN-EXTEND (AX) *)
        type := Type.Int32;

    | Type.Int16 => (* 16-bit signed integer *)
        (* EAX := SIGN-EXTEND (AX) *)
        u.cg.noargOp (Op.oCWDE);
        type := Type.Int32;

    | Type.Word8 => (* 8-bit unsigned integer *)
        u.cg.immOp (Op.oAND, u.cg.reg[EAX], TWordN.Max8);  (* EAX &= 16_FF *)
        type := Type.Word32;

    | Type.Word16 => (* 16-bit unsigned integer *)
        u.cg.immOp (Op.oAND, u.cg.reg[EAX], TWordN.Max16);  (* EAX &= 16_FFFF *)
        type := Type.Word32;

    ELSE (* value is ok *)
    END;
    RETURN type;
  END FixReturnValue;

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

    u.vstack.unlock();
    u.vstack.pushnew(Type.Addr, Force.anyreg);
    WITH stack0 = u.vstack.pos(0, "load_procedure") DO
      u.cg.movDummyReloc(u.vstack.op(stack0), realproc.symbol);
    END
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

    u.vstack.unlock();

    IF realproc.lev = 0 THEN
      u.vstack.pushimmT(TZero, Type.Word32);
    ELSE
      u.vstack.pushnew(Type.Addr, Force.anyreg);
      u.cg.get_frame(u.vstack.op(u.vstack.pos(0, "load_static_link")).reg[0],
                     realproc.parent, u.current_proc);
    END
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

    IF realproc.lev = 0 THEN
      u.vstack.corrupt(ECX, operandPart := 0);
      u.cg.movImmT(u.cg.reg[ECX], TZero);
    ELSE
      u.vstack.unlock();
      u.vstack.corrupt(ECX, operandPart := 0);
      u.cg.get_frame(ECX, realproc.parent, u.current_proc);
    END
  END load_static_link_toC;

(*---------------------------------------------------------- produce code ---*)

PROCEDURE fltregcmp (u: U): BOOLEAN =
  VAR reversed := FALSE;
  BEGIN
    IF u.cg.ftop_inmem THEN
      u.cg.binFOp(FOp.fCOMP, 1);
    ELSE
      u.cg.binFOp(FOp.fCOMPP, 1);
      reversed := TRUE;
    END;
    u.vstack.discard(2);

    u.vstack.unlock();
    u.vstack.corrupt(EAX, operandPart := 0);
    u.cg.noargFOp(FOp.fNSTSWAX);
    u.cg.noargOp(Op.oSAHF);

    RETURN reversed;
  END fltregcmp;

PROCEDURE condset (u: U; cond: Cond; type: ZType) =
  VAR reversed := FALSE;
  BEGIN

    (* This function used to deal with any integer type
     * as well, but that isn't needed presently.
     *)
    <* ASSERT Target.FloatType[type] *>

    reversed := fltregcmp(u);
    IF reversed THEN
      cond := revcond[cond];
    END;

    (* FCOM sets the unsigned compare flags *)
    cond := unscond[cond];

    u.vstack.pushnew(Type.Word8, Force.mem);
    WITH stop0 = u.vstack.op(u.vstack.pos(0, "condset")) DO
      stop0.mvar.var.stack_temp := FALSE;
      u.cg.setccOp(stop0, cond);
    END
  END condset;

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

    <* ASSERT CG_Bytes[type_multiple_of_32] >= CG_Bytes[type] *>

    IF TypeIs64(type) THEN
      (* see: http://niallryan.com/node/137
       * see fetch_and_op
       *)

      x.vstack.unlock();
      x.vstack.pushnew(type, Force.regset, RegSet{EDX, EAX});
 
      WITH oldValue       = x.vstack.pos(0, "fetch_and_op"),
           newValue       = x.vstack.pos(1, "fetch_and_op"),
           atomicVariable = x.vstack.pos(2, "fetch_and_op") DO
 
        x.vstack.find(newValue, Force.regset, RegSet{ECX, EBX});
        x.proc_reguse[EBX] := TRUE;

        (* x.vstack.find(atomicVariable, Force.any); bug *)
        x.vstack.find(atomicVariable, Force.anyreg);
        x.cg.load_ind(EAX, x.vstack.op(atomicVariable), 0, type);
        x.cg.load_ind(EDX, x.vstack.op(atomicVariable), 4, type);
        retry := x.next_label();
        x.cg.set_label(retry);
        x.cg.lock_compare_exchange(x.vstack.op(atomicVariable), x.vstack.op(newValue), type);
        x.cg.brOp(Cond.NE, retry);
        x.vstack.newdest(x.vstack.op(atomicVariable)); (* Is this needed? *)
        x.vstack.newdest(x.vstack.op(newValue));       (* Is this needed? *)
        x.vstack.newdest(x.vstack.op(oldValue));       (* Is this needed? *)
        x.vstack.discard(3);
      END;
      RETURN;
    END;

    x.fence(MemoryOrder.Sequential);
    x.vstack.unlock();
    WITH stack0 = x.vstack.pos(0, "store_ordered"),
         stack1 = x.vstack.pos(1, "store_ordered") DO
      x.vstack.find(stack0, Force.any);
      x.vstack.find(stack1, Force.mem);
      x.vstack.dostoreind(0, type);
    END;
    x.fence(MemoryOrder.Sequential);
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

    <* ASSERT CG_Bytes[type_multiple_of_32] >= CG_Bytes[type] *>

    IF TypeIs64(type) THEN

      (* see: http://niallryan.com/node/137 *)

      x.vstack.pushimmT(TZero, Type.Word64);
      x.vstack.pushimmT(TZero, Type.Word64);
      compare_exchange_helper(x, type);
      x.vstack.unlock();
      x.vstack.pushnew(type, Force.regset, RegSet{EAX, EDX});
      RETURN;
    END;

    x.vstack.unlock();
    x.fence(MemoryOrder.Sequential);
    x.load_indirect(0, type, type_multiple_of_32);
    x.fence(MemoryOrder.Sequential);
  END load_ordered;

PROCEDURE exchange (u: U; type: MType; type_multiple_of_32: ZType; <*UNUSED*>order: MemoryOrder) =
(* tmp := Mem [s1.A + o].type;
   Mem [s1.A + o].type := s0.type_multiple_of_32;
   s0.type_multiple_of_32 := tmp;
   pop *)
  VAR reg: Regno;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("exchange");
      u.wr.TName (type);
      u.wr.TName (type_multiple_of_32);
      u.wr.NL    ();
    END;

    <* ASSERT CG_Bytes[type_multiple_of_32] >= CG_Bytes[type] *>

    IF TypeIs64(type) THEN
      (* Push arbitrary value for the first compare. *)
      u.vstack.pushimmT(TZero, Type.Word64);
      u.vstack.swap();
      compare_exchange_helper(u, type);
      u.vstack.unlock();
      u.vstack.pushnew(type, Force.regset, RegSet{EAX, EDX});
      RETURN;
    END;

    WITH newValue       = u.vstack.pos(0, "exchange"),
         atomicVariable = u.vstack.pos(1, "exchange") DO
      u.vstack.unlock();
      u.vstack.find(newValue, Force.anyreg);
      u.vstack.find(atomicVariable, Force.anyreg);
      reg := u.vstack.op(newValue).reg[0];
      u.cg.lock_exchange(u.vstack.op(atomicVariable), u.vstack.op(newValue), type);
      u.vstack.discard(2);
      u.vstack.unlock();
      u.vstack.pushnew(type, Force.regset, RegSet{reg});
    END;
  END exchange;

PROCEDURE compare_exchange_helper (x: U; type: Type) =
  BEGIN

    x.vstack.unlock();

    WITH newValue                        = x.vstack.pos(0, "compare_exchange"),
         compareValueAndOldValueIfFailed = x.vstack.pos(1, "compare_exchange"),
         atomicVariable                  = x.vstack.pos(2, "compare_exchange") DO

      IF TypeIs64(type) THEN
        (*
         * 64 bit form has very particular register allocation requirements.
         *)
        x.vstack.find(compareValueAndOldValueIfFailed, Force.regset, RegSet{EAX, EDX});
        x.vstack.find(newValue, Force.regset, RegSet{ECX, EBX});
        x.proc_reguse[EBX] := TRUE;
      ELSE
        x.vstack.find(compareValueAndOldValueIfFailed, Force.regset, RegSet{EAX});
        x.vstack.find(newValue, Force.anyreg);
      END;
      x.vstack.find(atomicVariable, Force.anyreg);
      x.cg.lock_compare_exchange(x.vstack.op(atomicVariable), x.vstack.op(newValue), type);
      x.vstack.discard(3);
    END;

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

    <* ASSERT CG_Bytes[type_multiple_of_32] >= CG_Bytes[type] *>
    <* ASSERT CG_Bytes[result_type] = 4 *>

    compare_exchange_helper(x, type);

    (* Get the zero flag into a register. Is there a better way? *)

    x.vstack.unlock();
    x.vstack.pushnew(Type.Word8, Force.mem);
    WITH stop0 = x.vstack.op(x.vstack.pos(0, "condset")) DO
      stop0.mvar.var.stack_temp := FALSE;
      x.cg.setccOp(stop0, Cond.E);
    END;

  END compare_exchange;

PROCEDURE fence (u: U; <*UNUSED*>order: MemoryOrder) =
(*
 * Exchanging any memory with any register is a serializing instruction.
 *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("fence");
      u.wr.NL    ();
    END;

    <* ASSERT u.call # NIL *>
    <* ASSERT u.current_proc # NIL *>

    u.vstack.unlock();

    IF u.current_proc.fenceVar = NIL THEN
      u.current_proc.fenceVar := get_temp_var(u, Type.Word32, 4, 4);
    END;
    u.vstack.push(MVar{u.current_proc.fenceVar, mvar_type := Type.Word32});
    u.vstack.pushnew(Type.Word32, Force.anyreg);
    EVAL u.vstack.dobin(Op.oXCHG, TRUE, TRUE, Type.Word32);
    u.vstack.discard(1);
  END fence;

CONST AtomicOpToOp = ARRAY AtomicOp OF Op { Op.oXADD, Op.oXADD, Op.oOR, Op.oAND, Op.oXOR };
CONST AtomicOpName = ARRAY AtomicOp OF TEXT { "add", "sub", "or", "and", "xor" };
CONST AtomicAddSub = SET OF AtomicOp { AtomicOp.Add, AtomicOp.Sub };

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
  VAR retry: Label;
      is64 := TypeIs64(type);
      addSub := (NOT is64) AND atomic_op IN AtomicAddSub;
  BEGIN
    IF x.debug THEN
      x.wr.Cmd   ("fetch_and_op");
      x.wr.OutT  (AtomicOpName[atomic_op]);
      x.wr.TName (type);
      x.wr.TName (type_multiple_of_32);
      x.wr.NL    ();
    END;

    <* ASSERT CG_Bytes[type_multiple_of_32] >= CG_Bytes[type] *>

    x.vstack.unlock();

    IF addSub THEN
      x.vstack.pushnew(type, Force.anyreg); (* any? *)
      x.vstack.pushnew(type, Force.anyreg); (* any? *)
    ELSIF is64 THEN
      x.vstack.pushnew(type, Force.regset, RegSet{EDX, EAX});
      x.vstack.pushnew(type, Force.regset, RegSet{ECX, EBX});
      x.proc_reguse[EBX] := TRUE;
    ELSE
      x.vstack.pushnew(type, Force.regset, RegSet{EAX});
      x.vstack.pushnew(type, Force.anyreg);
    END;
    
(*
    mov oldValue, mem-or-reg; oldValue is EAX or EDX:EAX
retry:
    mov newValue, oldValue; oldValue is EAX or EDX:EAX
    op  newValue, secondOperand; newValue is whatever register allocator decides, or ECX:EBX
    lock cmpxchg[8b] BYTE OR WORD or DWORD or QWORD PTR [atomicVariable], newValue
    ; original value is in EAX or EDX:EAX, eq or ne.
    jne retry
    ; EAX or EDX:EAX contains old value
*)
    WITH newValue       = x.vstack.pos(0, "fetch_and_op"),
         oldValue       = x.vstack.pos(1, "fetch_and_op"),
         operand        = x.vstack.pos(2, "fetch_and_op"),
         atomicVariable = x.vstack.pos(3, "fetch_and_op") DO
      IF CG_Bytes[type] < 4 THEN
        x.vstack.find(operand, Force.anyreg);
      ELSE
        x.vstack.find(operand, Force.any);
      END;
      (* x.vstack.find(atomicVariable, Force.any); bug *)
      x.vstack.find(atomicVariable, Force.anyreg);
      
      IF addSub THEN
        IF atomic_op = AtomicOp.Sub THEN
          x.vstack.doneg(operand);
        END;
        x.cg.write_lock_prefix();
        x.cg.binOp(AtomicOpToOp[atomic_op], x.vstack.op(newValue), x.vstack.op(operand));
        x.vstack.doneg(operand);
        x.cg.binOp(AtomicOpToOp[atomic_op], x.vstack.op(oldValue), x.vstack.op(operand));
      ELSE
        x.cg.load_ind(EAX, x.vstack.op(atomicVariable), 0, type);
        IF is64 THEN
          x.cg.load_ind(EDX, x.vstack.op(atomicVariable), 4, type);
        END;
        retry := x.next_label();
        x.cg.set_label(retry);
        x.cg.movOp(x.vstack.op(newValue), x.vstack.op(oldValue));
        x.cg.binOp(AtomicOpToOp[atomic_op], x.vstack.op(newValue), x.vstack.op(operand));
        x.cg.lock_compare_exchange(x.vstack.op(atomicVariable), x.vstack.op(newValue), type);
        x.cg.brOp(Cond.NE, retry);
      END;
      x.vstack.newdest(x.vstack.op(atomicVariable)); (* Is this needed? Probably. *)
      x.vstack.newdest(x.vstack.op(operand));        (* Is this needed? *)
      x.vstack.newdest(x.vstack.op(newValue));       (* Is this needed? *)
      x.vstack.newdest(x.vstack.op(oldValue));       (* Is this needed? *)

      (* Store the new value (already done), return the old value (these discard/swaps). *)
      x.vstack.discard(1);
      x.vstack.swap();
      x.vstack.discard(1);
      x.vstack.swap();
      x.vstack.discard(1);
    END;

  END fetch_and_op;

VAR GWidechar_bitsize : INTEGER; (* So far, not used. *) 

PROCEDURE widechar_size (<*UNUSED*>u: U; bitsize: INTEGER) =
  BEGIN
    GWidechar_bitsize := bitsize
  END widechar_size; 

PROCEDURE Err(t: U; err: TEXT) =
  BEGIN
    t.Err(err);
    <* ASSERT FALSE *>
  END Err;

BEGIN
END M3x86.
