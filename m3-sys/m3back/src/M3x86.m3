(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Jun 20 16:19:09 PDT 1995 by kalsow     *)
(*      modified on Wed Nov 23 13:57:47 PST 1994 by isard      *)

MODULE M3x86 EXPORTS M3x86, M3x86Rep;
 
IMPORT Wr, Text, Fmt, IntRefTbl, Word, Convert;
IMPORT M3CG, M3ID, M3CG_Ops, Target, TInt AS TargetInt, TFloat AS TargetFloat;
IMPORT M3ObjFile, TargetMap;

FROM TargetMap IMPORT CG_Bytes;

FROM M3CG IMPORT Name, ByteOffset, TypeUID, CallingConvention;
FROM M3CG IMPORT BitSize, ByteSize, Alignment, Frequency;
FROM M3CG IMPORT Var, Proc, Label, Sign, BitOffset;
FROM M3CG IMPORT Type, ZType, AType, RType, IType, MType;
FROM M3CG IMPORT CompareOp, ConvertOp, RuntimeError;

FROM M3CG_Ops IMPORT ErrorHandler;

FROM M3ObjFile IMPORT Seg;

IMPORT Wrx86, Stackx86, Codex86;

FROM Stackx86 IMPORT MaxMin;
FROM Codex86 IMPORT Cond, Op, FOp, FIm, unscond, revcond, FloatBytes;

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
        wr              : Wrx86.T := NIL;
        cg              : Codex86.T := NIL;
        vstack          : Stackx86.T := NIL;
        obj             : M3ObjFile.T := NIL;
        debug           := FALSE;
        Err             : ErrorHandler := NIL;
        runtime         : IntRefTbl.T := NIL;  (* Name -> RuntimeHook *)
        textsym         : INTEGER;
        init_varstore   : x86Var := NIL;
        init_count      : INTEGER;
        call_param_size := ARRAY [0 .. 1] OF INTEGER { 0, 0 };
        in_proc_call    := 0;
        static_link     := ARRAY [0 .. 1] OF x86Var { NIL, NIL };
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
      END;

(*---------------------------------------------------------------------------*)

CONST
  CompareOpName = ARRAY CompareOp OF TEXT {
                          " EQ", " NE", " GT", " GE", " LT", " LE" };
  CompareOpCond = ARRAY CompareOp OF Cond {
                          Cond.E, Cond.NE, Cond.G, Cond.GE, Cond.L, Cond.LE };
  CompareOpProc = ARRAY CompareOp OF Builtin {
                          Builtin.set_eq, Builtin.set_ne, Builtin.set_gt,
                          Builtin.set_ge, Builtin.set_lt, Builtin.set_le };

CONST
  ConvertOpName = ARRAY ConvertOp OF TEXT {
                    " round", " trunc", " floor", " ceiling" };
  ConvertOpKind = ARRAY ConvertOp OF FlToInt {
                    FlToInt.Round, FlToInt.Truncate,
                    FlToInt.Floor, FlToInt.Ceiling };

CONST
  Alignmask = ARRAY [1 .. 4] OF INTEGER
    { 16_FFFFFFFF, 16_FFFFFFFE, 0, 16_FFFFFFFC };

(*---------------------------------------------------------------------------*)

PROCEDURE New (logfile: Wr.T; obj: M3ObjFile.T): M3CG.T =
  VAR u := NEW (U,
                obj := obj,
                runtime := NEW (IntRefTbl.Default).init (20));
  BEGIN
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

    u.in_proc_call := 0;

    u.reportlabel := u.cg.reserve_labels(1);
    u.usedfault := FALSE;

    FOR b := FIRST (u.builtins) TO LAST (u.builtins) DO
      u.builtins [b] := NIL;
    END;

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
      u.wr.Flush ();
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

PROCEDURE declare_typename (u: U;  t: TypeUID;  n: Name) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_typename");
      u.wr.Tipe  (t);
      u.wr.ZName (n);
      u.wr.NL    ();
    END
  END declare_typename;

PROCEDURE declare_array (u: U;  t, index, elt: TypeUID;  s: BitSize) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_array");
      u.wr.Tipe (t);
      u.wr.Tipe (index);
      u.wr.Tipe (elt);
      u.wr.BInt (s);
      u.wr.NL   ();
    END
  END declare_array;

PROCEDURE declare_open_array (u: U;  t, elt: TypeUID;  s: BitSize) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_open_array");
      u.wr.Tipe (t);
      u.wr.Tipe (elt);
      u.wr.BInt (s);
      u.wr.NL   ();
    END
  END declare_open_array;

PROCEDURE declare_enum (u: U;  t: TypeUID;  n_elts: INTEGER;  s: BitSize) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_enum");
      u.wr.Tipe (t); 
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

PROCEDURE declare_packed  (u: U;  t: TypeUID;  s: BitSize;  base: TypeUID) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_packed");
      u.wr.Tipe (t);
      u.wr.BInt (s);
      u.wr.Tipe (base);
      u.wr.NL   ();
    END
  END declare_packed;

PROCEDURE declare_record (u: U; t: TypeUID;  s: BitSize;
                          n_fields: INTEGER)=
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_record");
      u.wr.Tipe (t);
      u.wr.BInt (s);
      u.wr.Int  (n_fields);
      u.wr.NL   ();
    END
  END declare_record;

PROCEDURE declare_field (u: U;  n: Name;  o: BitOffset;  s: BitSize;
                         t: TypeUID)=
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_field");
      u.wr.ZName (n);
      u.wr.BInt  (o);
      u.wr.BInt  (s);
      u.wr.Tipe  (t);
      u.wr.NL    ();
    END
  END declare_field;

PROCEDURE declare_set (u: U;  t, domain: TypeUID;  s: BitSize) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_set");
      u.wr.Tipe (t);
      u.wr.Tipe (domain);
      u.wr.BInt (s);
      u.wr.NL    ();
    END
  END declare_set;

PROCEDURE declare_subrange (u: U; t, domain: TypeUID;
                            READONLY min, max: Target.Int;
                            s: BitSize) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_subrange");
      u.wr.Tipe (t);
      u.wr.Tipe (domain);
      u.wr.TInt (min);
      u.wr.TInt (max); 
      u.wr.BInt (s);
      u.wr.NL   ();
    END
  END declare_subrange;

PROCEDURE declare_pointer (u: U;  t, target: TypeUID;  brand: TEXT;
                           traced: BOOLEAN) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_pointer");
      u.wr.Tipe (t);
      u.wr.Tipe (target);
      u.wr.Txt  (brand);
      u.wr.Bool (traced);
      u.wr.NL   ();
    END
  END declare_pointer;


PROCEDURE declare_indirect (u: U;  t, target: TypeUID) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_indirect");
      u.wr.Tipe (t);
      u.wr.Tipe (target);
      u.wr.NL   ();
    END
  END declare_indirect;


PROCEDURE declare_proctype (u: U;  t: TypeUID;  n_formals: INTEGER;
                            result: TypeUID;  n_raises: INTEGER;
                            cc: CallingConvention) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_proctype");
      u.wr.Tipe (t);
      u.wr.Int  (n_formals);
      u.wr.Tipe (result);
      u.wr.Int  (n_raises);
      u.wr.Txt  (cc.name);
      u.wr.NL   ();
    END
  END declare_proctype;

PROCEDURE declare_formal (u: U;  n: Name;  t: TypeUID) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_formal");
      u.wr.ZName (n);
      u.wr.Tipe  (t);
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


PROCEDURE declare_object (u: U;  t, super: TypeUID;
                          brand: TEXT;  traced: BOOLEAN;
                          n_fields, n_methods: INTEGER;
                          field_size: BitSize) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_object");
      u.wr.Tipe (t);
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

PROCEDURE declare_opaque (u: U;  t, super: TypeUID) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_opaque");
      u.wr.Tipe  (t);
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

PROCEDURE NewVar (u: U; t: Type; uid: TypeUID; s: ByteSize; a: Alignment;
                  name: Name := M3ID.NoID): x86Var =
  VAR v := NEW (x86Var, tag := u.next_var, type := t, s := s,
                a := a, seg := Seg.Data);
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
                         t: Type;  m3t: TypeUID): Var =
  VAR v := NewVar(u, t, m3t, s, a, n);
  BEGIN
    v.symbol := u.obj.import_symbol(v.name);
    v.offset := 0;
    v.loc := VLoc.global;

    IF u.debug THEN
      u.wr.Cmd   ("import_global");
      u.wr.ZName (n);
      u.wr.Int   (s);
      u.wr.Int   (a);
      u.wr.TName (t);
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
                        t: Type;  exported, inited: BOOLEAN) =
  VAR realvar := NARROW(v, x86Var);
  BEGIN
    <* ASSERT inited *>

    realvar.type := t;
    realvar.s := s;
    realvar.a := a;

    IF exported THEN
      u.obj.export_symbol(realvar.symbol);
    END;

    IF u.debug THEN
      u.wr.Cmd   ("bind_segment");
      u.wr.VName (v);
      u.wr.Int   (s);
      u.wr.Int   (a);
      u.wr.TName (t);
      u.wr.Bool  (exported);
      u.wr.Bool  (inited);
      u.wr.NL    ();
    END
  END bind_segment;

PROCEDURE declare_global (u: U;  n: Name;  s: ByteSize;  a: Alignment;
                     t: Type;  m3t: TypeUID;  exported, inited: BOOLEAN): Var =
  BEGIN
    RETURN DeclareGlobal(u, n, s, a, t, m3t, exported, inited, FALSE);
  END declare_global;

PROCEDURE declare_constant (u: U;  n: Name;  s: ByteSize;  a: Alignment;
                     t: Type;  m3t: TypeUID;  exported, inited: BOOLEAN): Var =
  BEGIN
    RETURN DeclareGlobal(u, n, s, a, t, m3t, exported, inited, TRUE);
  END declare_constant;

PROCEDURE DeclareGlobal (u: U;  n: Name;  s: ByteSize;  a: Alignment;
                         t: Type;  m3t: TypeUID;
                         exported, inited, is_const: BOOLEAN): Var =
  CONST SegMap  = ARRAY BOOLEAN OF Seg  { Seg.Data, Seg.Text };
  CONST DeclTag = ARRAY BOOLEAN OF TEXT { "declare_global", "declare_constant" };
  VAR v := NewVar(u, t, m3t, s, a, n);
  BEGIN
    v.loc := VLoc.global;
    v.seg := SegMap [is_const];
    IF inited
      THEN v.symbol := u.obj.define_symbol (v.name, v.seg, 0);
      ELSE v.symbol := u.obj.define_bss_symbol (v.name, s, a);
    END;
    IF exported THEN
      u.obj.export_symbol (v.symbol);
    END;
    IF u.debug THEN
      u.wr.Cmd   (DeclTag [is_const]);
      u.wr.ZName (n);
      u.wr.Int   (s);  
      u.wr.Int   (a);  
      u.wr.TName (t);
      u.wr.Tipe  (m3t);
      u.wr.Bool  (exported);
      u.wr.Bool  (inited);
      u.wr.VName (v);
      u.wr.NL    ();
    END;
    RETURN v;
  END DeclareGlobal;

PROCEDURE declare_local (u: U;  n: Name;  s: ByteSize;  a: Alignment;
                         t: Type;  m3t: TypeUID;  in_memory, up_level: BOOLEAN;
                         f: Frequency): Var =
  VAR v: x86Var;
  BEGIN
    IF u.in_proc
      THEN v := get_temp_var (u, t, s, a, n);
      ELSE v := create_temp_var (u, t, s, a, n);
    END;

    IF u.debug THEN
      u.wr.Cmd   ("declare_local");
      u.wr.ZName (n);
      u.wr.Int   (s);  
      u.wr.Int   (a);  
      u.wr.TName (t);
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
  <*FATAL Convert.Failed*>
  VAR buf: ARRAY [0..99] OF CHAR;
      txt: TEXT;
      len: INTEGER;
  BEGIN
    txt := M3ID.ToText(base);
    len := Text.Length(txt);
    IF len < (NUMBER(buf)+10) THEN
      buf [0] := '_';  INC(len);
      Text.SetChars(SUBARRAY(buf, 1, NUMBER(buf)-1), txt);
      IF std_call THEN
        buf [len] := '@'; INC(len);
        INC (len, Convert.FromInt(SUBARRAY(buf, len, NUMBER(buf)-len),
                                   arg_size));
      END;
      RETURN M3ID.FromStr(buf, len);
    ELSE
      IF std_call
        THEN RETURN M3ID.Add(Fmt.F ("_%s@%s", txt, Fmt.Int (arg_size)));
        ELSE RETURN M3ID.Add(Fmt.F ("_%s",    txt));
      END
    END;
  END mangle_procname;

PROCEDURE declare_param (u: U;  n: Name;  s: ByteSize;  a: Alignment;
                         t: Type;  m3t: TypeUID;  in_memory, up_level: BOOLEAN;
                         f: Frequency): Var =
  VAR v := NewVar(u, t, m3t, s, 4, n);
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
        u.param_proc.symbol := u.obj.define_symbol(u.param_proc.name,
                                                   Seg.Text, 0);
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
      u.wr.TName (t);
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

PROCEDURE declare_temp   (u: U;  s: ByteSize;  a: Alignment;  t: Type;
                          in_memory:BOOLEAN): Var =
  VAR v: x86Var;
  BEGIN
    <* ASSERT u.in_proc *>

    v := get_temp_var(u, t, s, a);

    IF u.debug THEN
      u.wr.Cmd   ("declare_temp");
      u.wr.Int   (s);  
      u.wr.Int   (a);
      u.wr.TName (t);
      u.wr.Bool  (in_memory);
      u.wr.VName (v);
      u.wr.Int   (v.offset);
      u.wr.NL    ();
    END;

    RETURN v;
  END declare_temp;

PROCEDURE get_temp_var (u: U; t: Type; s: ByteSize; a: Alignment;
                        n: Name := M3ID.NoID): x86Var =
  BEGIN
    IF s < 4 THEN
      s := 4;
    END;

    IF a < 4 THEN
      a := 4;
    END;

    FOR i := 0 TO u.current_proc.tempsize - 1 DO
      WITH temp = u.current_proc.temparr[i] DO
        IF temp.free AND temp.var.s = s AND temp.var.a >= a THEN
          temp.free := FALSE;
          temp.var.type := t;
          temp.var.stack_temp := FALSE;
          temp.var.scope := u.next_scope - 1;
          RETURN temp.var;
        END
      END
    END;

    IF u.current_proc.tempsize = u.current_proc.templimit THEN
      expand_temp(u);
    END;

    WITH temp = u.current_proc.temparr[u.current_proc.tempsize] DO
      temp.var := create_temp_var(u, t, s, a, n);
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

PROCEDURE create_temp_var (u: U; t: Type; s: ByteSize; a: Alignment;
                           n: Name): x86Var =
  VAR v := NewVar(u, t, 0, s, a, n);
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

    u.Err("Couldn't find var to free in 'free_temp'");
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

    IF Word.And(offs, realvar.a - 1) # 0 THEN
      pad := realvar.a - Word.And(offs, realvar.a - 1);
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

    pad_init(u, realvar.s);
    u.init_varstore := NIL;
  END end_init;

PROCEDURE init_int (u: U;  o: ByteOffset;  READONLY value: Target.Int;
                    t: Type) =
  VAR int: INTEGER;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("init_int");
      u.wr.Int   (o);
      u.wr.TInt  (value);
      u.wr.TName (t);
      u.wr.NL    ();
    END;

    pad_init(u, o);

    EVAL TargetInt.ToInt(value, int);
    u.obj.append(u.init_varstore.seg, int, CG_Bytes[t]);
    INC(u.init_count, CG_Bytes[t]);
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

    size := TargetFloat.ToBytes(f, flarr);

    <* ASSERT size = 4 OR size = 8 *>

    pad_init(u, o);

    FOR i := 0 TO size-1 DO
      u.obj.append(u.init_varstore.seg, flarr[i], 1);
      INC(u.init_count);
    END;
  END init_float;

PROCEDURE pad_init (u: U; o: ByteOffset) =
  BEGIN
    <* ASSERT u.init_count <= o *>
    <* ASSERT o <= u.init_varstore.s *>

    FOR i := u.init_count TO o - 1 DO
      u.obj.append(u.init_varstore.seg, 0, 1);
    END;

    u.init_count := o;
  END pad_init;

(*------------------------------------------------------------ procedures ---*)

PROCEDURE NewProc (u: U; n: Name; n_params: INTEGER;
                   ret_type: Type;  cc: CallingConvention): x86Proc =
  VAR p := NEW (x86Proc, tag := u.next_proc, n_params := n_params,
                type := ret_type, stdcall := (cc.m3cg_id = 1));
  BEGIN
    IF n = M3ID.NoID
      THEN p.name := M3ID.Add("P$" & Fmt.Int(p.tag));
      ELSE p.name := n;
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

    IF n_params = 0 OR NOT p.stdcall THEN
      p.name := mangle_procname(p.name, 0, p.stdcall);
      p.symbol := u.obj.import_symbol(p.name);
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

      u.wr.Flush();
    END;

    u.vstack.clearall ();

    <* ASSERT NOT u.in_proc *>
    u.in_proc := TRUE;

    u.current_proc := p;
    u.cg.set_current_proc(p);
    u.vstack.set_current_proc(p);
    u.last_exitbranch := -1;
    u.exit_proclabel := -1;

    realproc.offset := u.obj.cursor(Seg.Text);
    realproc.bound := TRUE;

    WHILE realproc.usage # NIL DO
      u.obj.patch(Seg.Text, realproc.usage.loc, realproc.offset -
                  (realproc.usage.loc + 4), 4);
      realproc.usage := realproc.usage.link;
    END;

    u.obj.move_symbol(realproc.symbol, realproc.offset);

    u.obj.begin_procedure(realproc.symbol);

    u.cg.pushOp(u.cg.reg[Codex86.EBP]);
    u.cg.movOp(u.cg.reg[Codex86.EBP], u.cg.reg[Codex86.ESP]);

    u.cg.immOp(Op.oSUB, u.cg.reg[Codex86.ESP], 16_FFFF);
    u.procframe_ptr := u.obj.cursor(Seg.Text) - 4;

    u.cg.pushOp(u.cg.reg[Codex86.EBX]);
    u.cg.pushOp(u.cg.reg[Codex86.ESI]);
    u.cg.pushOp(u.cg.reg[Codex86.EDI]);

    IF u.current_proc.lev # 0 THEN
      u.cg.store_ind(u.cg.reg[Codex86.ECX], u.cg.reg[Codex86.EBP],
                     -4, Type.Addr);
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

PROCEDURE set_label (u: U;  l: Label;  <*UNUSED*> barrier: BOOLEAN) =
  (* define 'l' to be at the current pc *)
  BEGIN
    IF u.debug THEN
      u.wr.OutT  (".");
      u.wr.Lab   (l);
      u.wr.NL    ();
    END;

    u.cg.set_label(l);

    u.vstack.clearall();
  END set_label;

PROCEDURE jump (u: U; l: Label) =
  (* GOTO l *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("jump");
      u.wr.Lab   (l);
      u.wr.NL    ();
    END;

    u.cg.brOp(Cond.Always, l);
  END jump;

PROCEDURE if_true  (u: U;  t: IType;  l: Label; <*UNUSED*> f: Frequency) =
  (* IF (s0.t # 0) GOTO l ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("if_true");
      u.wr.TName (t);
      u.wr.Lab   (l);
      u.wr.NL    ();
    END;

    u.vstack.doimm (Op.oCMP, 0, FALSE);
    u.cg.brOp (Cond.NZ, l);
  END if_true;

PROCEDURE if_false (u: U;   t: IType;  l: Label; <*UNUSED*> f: Frequency) =
  (* IF (s0.t = 0) GOTO l ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("if_false");
      u.wr.TName (t);
      u.wr.Lab   (l);
      u.wr.NL    ();
    END;

    u.vstack.doimm (Op.oCMP, 0, FALSE);
    u.cg.brOp (Cond.Z, l);
  END if_false;

PROCEDURE if_compare (u: U;  t: ZType;  op: CompareOp;  l: Label;
                      <*UNUSED*> f: Frequency) =
  (* IF (s1.t  op  s0.t) GOTO l ; pop(2) *)
  VAR cond := CompareOpCond [op];
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("if_compare");
      u.wr.TName (t);
      u.wr.OutT  (CompareOpName [op]);
      u.wr.Lab   (l);
      u.wr.NL    ();
    END;

    CASE t OF
    | Type.Word32 =>
        IF u.vstack.dobin(Op.oCMP, TRUE, FALSE) THEN cond := revcond[cond]; END;
        cond := unscond[cond];
    | Type.Int32 =>
        IF u.vstack.dobin(Op.oCMP, TRUE, FALSE) THEN cond := revcond[cond]; END;
    | Type.Word64 =>
        IF u.vstack.dobin(Op.oCMP, TRUE, FALSE) THEN cond := revcond[cond]; END;
        cond := unscond[cond];
    | Type.Int64 =>
        IF u.vstack.dobin(Op.oCMP, TRUE, FALSE) THEN cond := revcond[cond]; END;
    | Type.Addr =>
        IF u.vstack.dobin(Op.oCMP, TRUE, FALSE) THEN cond := revcond[cond]; END;
        cond := unscond[cond];
    | Type.Reel, Type.LReel, Type.XReel =>
        IF u.cg.ftop_inmem
          THEN u.cg.binFOp (FOp.fCOMP, 1);
          ELSE u.cg.binFOp (FOp.fCOMPP, 1);  cond := revcond[cond];
        END;
        u.vstack.discard (2);
        u.vstack.unlock ();
        u.vstack.corrupt (Codex86.EAX);
        u.cg.noargFOp (FOp.fNSTSWAX);
        u.cg.noargOp (Op.oSAHF);
        cond := unscond[cond]; (* FCOM sets the unsigned compare flags *)
    END;

    u.cg.brOp(cond, l);
  END if_compare;

PROCEDURE case_jump (u: U;  t: IType;  READONLY labels: ARRAY OF Label) =
  (* "GOTO labels[s0.t] ; pop" with no range checking on s0.t *)
  VAR stack0: INTEGER;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("case_jump");
      u.wr.TName (t);
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

PROCEDURE exit_proc (u: U; t: Type) =
  (* Returns s0.t if t is not Void, otherwise returns no value. *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("exit_proc");
      u.wr.TName (t);
      u.wr.NL    ();
    END;

    IF t # Type.Void THEN
      u.vstack.unlock();

      WITH stack0 = u.vstack.pos(0, "exit_proc") DO
        IF Target.FloatType[t] THEN
          u.cg.f_exitproc();
        ELSE
          u.vstack.find(stack0, Force.regset, RegSet { Codex86.EAX });
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
  VAR callee_cleans := u.current_proc.stdcall;
  BEGIN
    IF u.exit_proclabel = -1 THEN
      RETURN;
      (* Strange as it may seem, some procedures have no exit points... *)
    END;

    IF u.last_exitbranch = u.obj.cursor(Seg.Text) - 5 THEN
      (* Don't generate a branch to the epilogue at the last exit
         point of the procedure *)
      u.cg.set_label(u.exit_proclabel, offset := -5);

      u.obj.patch(Seg.Text, u.obj.cursor(Seg.Text) - 5, 16_C95B5E5F, 4);
      (* Intel for POP EDI, POP ESI, POP EBX, LEAVE *)

      IF callee_cleans THEN
        u.obj.patch(Seg.Text, u.obj.cursor(Seg.Text) - 1, 16_C2, 1);
        (* Intel for RET imm16 *)
        u.obj.append(Seg.Text, u.current_proc.paramsize - 8, 2);
        (* And the argument *)
      ELSE
        u.obj.patch(Seg.Text, u.obj.cursor(Seg.Text) - 1, 16_C3, 1);
        (* Intel for RET *)
      END
    ELSE
      u.cg.set_label(u.exit_proclabel);

      u.cg.popOp(u.cg.reg[Codex86.EDI]);
      u.cg.popOp(u.cg.reg[Codex86.ESI]);
      u.cg.popOp(u.cg.reg[Codex86.EBX]);

      u.cg.noargOp(Op.oLEAVE);
      IF callee_cleans THEN
        u.cg.cleanretOp(u.current_proc.paramsize - 8);
      ELSE
        u.cg.noargOp(Op.oRET);
      END
    END
  END procedure_epilogue;

(*------------------------------------------------------------ load/store ---*)

PROCEDURE load  (u: U;  v: Var;  o: ByteOffset;  t: MType;  z: ZType) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load");
      u.wr.VName (v);
      u.wr.Int   (o);  
      u.wr.TName (t);
      u.wr.TName (z);
      u.wr.NL    ();
    END;

    u.vstack.push(MVar {var := v, o := o, t := t});
  END load;

PROCEDURE store (u: U;  v: Var;  o: ByteOffset;  z: ZType;  t: MType;  ) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("store");
      u.wr.VName (v);
      u.wr.Int   (o);  
      u.wr.TName (z);
      u.wr.TName (t);
      u.wr.NL    ();
    END;

    u.vstack.pop(MVar {var := v, o := o, t := t});
  END store;

PROCEDURE load_address (u: U;  v: Var;  o: ByteOffset) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_address");
      u.wr.VName (v);
      u.wr.Int   (o);
      u.wr.NL    ();
    END;

    u.vstack.doloadaddress(v, o);
  END load_address;

PROCEDURE load_indirect (u: U;  o: ByteOffset;  t: MType;  z: ZType) =
  VAR newreg: Regno;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_indirect");
      u.wr.Int   (o);  
      u.wr.TName (t);
      u.wr.TName (z);
      u.wr.NL    ();
    END;

    u.vstack.unlock();
    WITH stack0 = u.vstack.pos(0, "load_indirect") DO
      u.vstack.find(stack0, Force.anyreg, RegSet {}, TRUE);
      IF Target.FloatType [t] THEN
        u.cg.f_loadind(u.vstack.op(stack0), o, t);
        u.vstack.dealloc_reg(stack0);
        u.vstack.set_fstack(stack0);
      ELSE
        IF CG_Bytes[t] = 1 THEN
          newreg := u.vstack.freereg(RegSet { Codex86.EAX, Codex86.EBX,
                                              Codex86.ECX, Codex86.EDX } );
        ELSE
          newreg := u.vstack.freereg();
        END;

        u.cg.load_ind(newreg, u.vstack.op(stack0), o, t);
        u.vstack.dealloc_reg(stack0);
        u.vstack.set_reg(stack0, newreg);
      END
    END
  END load_indirect;

PROCEDURE store_indirect (u: U;  o: ByteOffset;  z: ZType;  t: MType) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("store_indirect");
      u.wr.Int   (o);  
      u.wr.TName (z);
      u.wr.TName (t);
      u.wr.NL    ();
    END;

    u.vstack.unlock();
    WITH (* stack0 = u.vstack.pos(0, "store_indirect"), *)
         stack1 = u.vstack.pos(1, "store_indirect") DO
      IF Target.FloatType [t] THEN
        u.vstack.find(stack1, Force.anyreg, RegSet {}, TRUE);
        u.cg.f_storeind(u.vstack.op(stack1), o, t);
        u.vstack.discard(2);
      ELSE
        u.vstack.dostoreind(o, t);
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

    u.vstack.pushimm(0);
  END load_nil;

PROCEDURE load_integer  (u: U;  t: IType;  READONLY i: Target.Int) =
  (* push ; s0.t := i *)
  VAR int: INTEGER;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_integer");
      u.wr.TName (t);
      u.wr.TInt  (i);
      u.wr.NL    ();
    END;

    IF NOT TargetInt.ToInt(i, int) THEN
      u.Err("Failed to convert target integer in load_integer");
    END;
    u.vstack.unlock();
    u.vstack.pushimm(int);
  END load_integer;

PROCEDURE load_float    (u: U;  t: RType;  READONLY f: Target.Float) =
  (* push ; s0.t := f *)
  VAR flarr: FloatBytes;
      size: INTEGER;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_float");
      u.wr.TName (t);
      u.wr.Flt   (f);
      u.wr.NL    ();
    END;

    u.vstack.pushnew(t, Force.any);
    size := TargetFloat.ToBytes(f, flarr);
    IF size # CG_Bytes[t] THEN
      u.Err("Floating size mismatch in load_float");
    END;
    u.cg.f_loadlit(flarr, t);
  END load_float;

(*------------------------------------------------------------ arithmetic ---*)

PROCEDURE compare (u: U;  t: ZType;  z: IType;  op: CompareOp) =
  (* s1.z := (s1.t  op  s0.t)  ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("compare");
      u.wr.TName (t);
      u.wr.TName (z);
      u.wr.OutT  (CompareOpName [op]);
      u.wr.NL    ();
    END;

    condset(u, CompareOpCond [op], t);
  END compare;

PROCEDURE add (u: U;  t: AType) =
  (* s1.t := s1.t + s0.t ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("add");
      u.wr.TName (t);
      u.wr.NL    ();
    END;

    IF Target.FloatType [t] THEN
      u.cg.binFOp(FOp.fADDP, 1);
      u.vstack.discard(1);
    ELSE
      EVAL u.vstack.dobin(Op.oADD, TRUE, TRUE);
    END
  END add;

PROCEDURE subtract (u: U;  t: AType) =
  (* s1.t := s1.t - s0.t ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("subtract");
      u.wr.TName (t);
      u.wr.NL    ();
    END;

    IF Target.FloatType [t] THEN
      u.cg.binFOp(FOp.fSUBP, 1);
      u.vstack.discard(1);
    ELSE
      EVAL u.vstack.dobin(Op.oSUB, FALSE, TRUE);
    END
  END subtract;

PROCEDURE multiply (u: U;  t: AType) =
  (* s1.t := s1.t * s0.t ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("multiply");
      u.wr.TName (t);
      u.wr.NL    ();
    END;

    IF Target.FloatType [t] THEN
      u.cg.binFOp(FOp.fMUL, 1);
      u.vstack.discard(1);
    ELSE
      IF t = Type.Int32 THEN
        u.vstack.doimul();
      ELSE
        u.vstack.doumul();
      END
    END;
  END multiply;

PROCEDURE divide (u: U;  t: RType) =
  (* s1.t := s1.t / s0.t ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("divide");
      u.wr.TName (t);
      u.wr.NL    ();
    END;

    u.cg.binFOp(FOp.fDIV, 1);
    u.vstack.discard(1);
  END divide;

CONST SignName = ARRAY Sign OF TEXT { " P", " N", " X" };

PROCEDURE div (u: U;  t: IType;  a, b: Sign) =
  (* s1.t := s1.t DIV s0.t ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("div");
      u.wr.TName (t);
      u.wr.OutT  (SignName [a]);
      u.wr.OutT  (SignName [b]);
      u.wr.NL    ();
    END;

    IF t = Type.Word32 THEN
      a := Sign.Positive;
      b := Sign.Positive;
    END;

    u.vstack.dodiv(a, b);
  END div;

PROCEDURE mod (u: U;  t: IType;  a, b: Sign) =
  (* s1.t := s1.t MOD s0.t ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("mod");
      u.wr.TName (t);
      u.wr.OutT  (SignName [a]);
      u.wr.OutT  (SignName [b]);
      u.wr.NL    ();
    END;

    IF t = Type.Word32 THEN
      a := Sign.Positive;
      b := Sign.Positive;
    END;

    u.vstack.domod(a, b);
  END mod;

PROCEDURE negate (u: U;  t: AType) =
  (* s0.t := - s0.t *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("negate");
      u.wr.TName (t);
      u.wr.NL    ();
    END;

    IF Target.FloatType [t] THEN
      u.cg.noargFOp(FOp.fCHS);
    ELSE
      u.vstack.doneg();
    END
  END negate;

PROCEDURE abs      (u: U;  t: AType) =
  (* s0.t := ABS (s0.t) (noop on Words) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("abs");
      u.wr.TName (t);
      u.wr.NL    ();
    END;

    CASE t OF
    | Type.Word32 => (* no-op *)
    | Type.Int32 => u.vstack.doabs();
    ELSE
      u.cg.noargFOp(FOp.fABS);
    END
  END abs;

PROCEDURE max      (u: U;  t: ZType) =
  (* s1.t := MAX (s1.t, s0.t) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("max");
      u.wr.TName (t);
      u.wr.NL    ();
    END;

    u.vstack.domaxmin(t, MaxMin.Max);
  END max;

PROCEDURE min      (u: U;  t: ZType) =
  (* s1.t := MIN (s1.t, s0.t) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("min");
      u.wr.TName (t);
      u.wr.NL    ();
    END;

    u.vstack.domaxmin(t, MaxMin.Min);
  END min;

PROCEDURE cvt_int (u: U;  t: RType;  x: IType;  op: ConvertOp) =
  (* s0.x := ROUND (s0.t) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("cvt_int");
      u.wr.TName (t);
      u.wr.TName (x);
      u.wr.OutT  (ConvertOpName [op]);
      u.wr.NL    ();
    END;

    u.vstack.fltoint(ConvertOpKind [op]);
  END cvt_int;

PROCEDURE cvt_float (u: U;  t: AType;  x: RType) =
  (* s0.x := FLOAT (s0.t, x) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("cvt_float");
      u.wr.TName (t);
      u.wr.TName (x);
      u.wr.NL    ();
    END;

    IF Target.FloatType [t] THEN
      RETURN;
    END;

    u.vstack.inttoflt();
  END cvt_float;

(*------------------------------------------------------------------ sets ---*)

PROCEDURE set_union (u: U;  s: ByteSize) =
  (* s1.B := s1.B + s0.B ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("set_union");
      u.wr.Int   (s);
      u.wr.NL    ();
    END;

    start_int_proc (u, Builtin.set_union);
    load_stack_param (u, Type.Addr, 2);
    load_stack_param (u, Type.Addr, 1);
    pop_param (u, Type.Addr);
    u.vstack.discard (2);
    u.vstack.pushimm (s * 8);
    pop_param (u, Type.Int32);
    call_int_proc (u, Builtin.set_union);
  END set_union;

PROCEDURE set_difference (u: U;  s: ByteSize) =
  (* s1.B := s1.B - s0.B ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("set_difference");
      u.wr.Int   (s);
      u.wr.NL    ();
    END;

    start_int_proc (u, Builtin.set_difference);
    load_stack_param (u, Type.Addr, 2);
    load_stack_param (u, Type.Addr, 1);
    pop_param (u, Type.Addr);
    u.vstack.discard (2);
    u.vstack.pushimm (s * 8);
    pop_param (u, Type.Int32);
    call_int_proc (u, Builtin.set_difference);
  END set_difference;

PROCEDURE set_intersection (u: U;  s: ByteSize) =
  (* s1.B := s1.B * s0.B ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("set_intersection");
      u.wr.Int   (s);
      u.wr.NL    ();
    END;

    start_int_proc (u, Builtin.set_intersection);
    load_stack_param (u, Type.Addr, 2);
    load_stack_param (u, Type.Addr, 1);
    pop_param (u, Type.Addr);
    u.vstack.discard (2);
    u.vstack.pushimm (s * 8);
    pop_param (u, Type.Int32);
    call_int_proc (u, Builtin.set_intersection);
  END set_intersection;

PROCEDURE set_sym_difference (u: U;  s: ByteSize) =
  (* s1.B := s1.B / s0.B ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("set_sym_difference");
      u.wr.Int   (s);
      u.wr.NL    ();
    END;

    start_int_proc (u, Builtin.set_sym_difference);
    load_stack_param (u, Type.Addr, 2);
    load_stack_param (u, Type.Addr, 1);
    pop_param (u, Type.Addr);
    u.vstack.discard (2);
    u.vstack.pushimm (s * 8);
    pop_param (u, Type.Int32);
    call_int_proc (u, Builtin.set_sym_difference);
  END set_sym_difference;

PROCEDURE set_member (u: U;  s: ByteSize;  t: IType) =
  (* s1.t := (s0.t IN s1.B) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("set_member");
      u.wr.Int   (s);
      u.wr.TName (t);
      u.wr.NL    ();
    END;

    start_int_proc (u, Builtin.set_member);
    u.vstack.swap();
    pop_param(u, Type.Addr);
    pop_param(u, t);
    call_int_proc (u, Builtin.set_member);
  END set_member;

PROCEDURE set_compare (u: U;  s: ByteSize;  op: CompareOp;  t: IType) =
  (* s1.t := (s1.B  op  s0.B)  ; pop *)
  VAR proc := CompareOpProc [op];
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("set_eq");
      u.wr.Int   (s);
      u.wr.OutT  (CompareOpName [op]);
      u.wr.TName (t);
      u.wr.NL    ();
    END;

    start_int_proc (u, proc);
    u.vstack.swap();
    pop_param(u, Type.Addr);
    pop_param(u, Type.Addr);
    u.vstack.pushimm(s * 8);
    pop_param(u, Type.Int32);
    call_int_proc (u, proc);
  END set_compare;

PROCEDURE set_range (u: U;  s: ByteSize;  t: IType) =
  (* s2.A [s1.t .. s0.t] := 1's; pop(3)*)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("set_range");
      u.wr.Int   (s);
      u.wr.TName (t);
      u.wr.NL    ();
    END;

    start_int_proc (u, Builtin.set_range);
    load_stack_param(u, Type.Addr, 2);
    load_stack_param(u, t, 1);
    pop_param(u, t);
    u.vstack.discard(2);
    call_int_proc (u, Builtin.set_range);
  END set_range;

PROCEDURE set_singleton (u: U;  s: ByteSize;  t: IType) =
  (* s1.A [s0.t] := 1; pop(2) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("set_singleton");
      u.wr.Int   (s);
      u.wr.TName (t);
      u.wr.NL    ();
    END;

    start_int_proc (u, Builtin.set_singleton);
    u.vstack.swap();
    pop_param(u, Type.Addr);
    pop_param(u, t);
    call_int_proc (u, Builtin.set_singleton);
  END set_singleton;

(*------------------------------------------------- Word.T bit operations ---*)

PROCEDURE not (u: U;  t: IType) =
  (* s0.t := Word.Not (s0.t) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("not");
      u.wr.TName (t);
      u.wr.NL    ();
    END;

    WITH stack0 = u.vstack.pos(0, "not") DO
      IF u.vstack.loc(stack0) = OLoc.imm THEN
        u.vstack.set_imm(stack0, Word.Not (u.vstack.op(stack0).imm));
      ELSE
        u.vstack.unlock();
        u.vstack.find(stack0, Force.anytemp);
        u.cg.unOp(Op.oNOT, u.vstack.op(stack0));
        u.vstack.newdest(u.vstack.op(stack0));
      END
    END
  END not;

PROCEDURE and (u: U;  t: IType) =
  (* s1.t := Word.And (s1.t, s0.t) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("and");
      u.wr.TName (t);
      u.wr.NL    ();
    END;

    EVAL u.vstack.dobin(Op.oAND, TRUE, TRUE);
  END and;

PROCEDURE or  (u: U;  t: IType) =
  (* s1.t := Word.Or  (s1.t, s0.t) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("or");
      u.wr.TName (t);
      u.wr.NL    ();
    END;

    EVAL u.vstack.dobin(Op.oOR, TRUE, TRUE);
  END or;

PROCEDURE xor (u: U;  t: IType) =
  (* s1.t := Word.Xor (s1.t, s0.t) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("xor");
      u.wr.TName (t);
      u.wr.NL    ();
    END;

    EVAL u.vstack.dobin(Op.oXOR, TRUE, TRUE);
  END xor;

PROCEDURE shift (u: U;  t: IType) =
  (* s1.t := Word.Shift  (s1.t, s0.t) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("shift");
      u.wr.TName (t);
      u.wr.NL    ();
    END;

    u.vstack.doshift ();
  END shift;

PROCEDURE shift_left   (u: U;  t: IType) =
  (* s1.t := Word.Shift  (s1.t, s0.t) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("shift_left");
      u.wr.TName (t);
      u.wr.NL    ();
    END;

    u.vstack.unlock();
    WITH stack0 = u.vstack.pos(0, "shift_left"),
         stack1 = u.vstack.pos(1, "shift_left") DO
      IF u.vstack.loc(stack0) = OLoc.imm THEN
        IF u.vstack.loc(stack1) = OLoc.imm THEN
          u.vstack.set_imm(stack1, Word.Shift(u.vstack.op(stack1).imm,
                                              u.vstack.op(stack0).imm));
        ELSE
          u.vstack.find(stack1, Force.anytemp);
          u.vstack.set_imm(stack0, Word.And(u.vstack.op(stack0).imm, 16_1F));
          u.cg.immOp(Op.oSAL, u.vstack.op(stack1), u.vstack.op(stack0).imm);
          u.vstack.newdest(u.vstack.op(stack1));
        END
      ELSE
        u.vstack.find(stack0, Force.regset, RegSet {Codex86.ECX});
        u.vstack.find(stack1, Force.anytemp);

        IF u.vstack.loc(stack1) = OLoc.imm THEN
          u.vstack.find(stack1, Force.anyreg);
        END;

        u.cg.unOp(Op.oSAL, u.vstack.op(stack1));
        u.vstack.newdest(u.vstack.op(stack1));
      END;

      u.vstack.discard(1);
    END
  END shift_left;

PROCEDURE shift_right  (u: U;  t: IType) =
  (* s1.t := Word.Shift  (s1.t, -s0.t) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("shift_right");
      u.wr.TName (t);
      u.wr.NL    ();
    END;

    u.vstack.unlock();
    WITH stack0 = u.vstack.pos(0, "shift_right"),
         stack1 = u.vstack.pos(1, "shift_right") DO
      IF u.vstack.loc(stack0) = OLoc.imm THEN
        IF u.vstack.loc(stack1) = OLoc.imm THEN
          u.vstack.set_imm(stack1, Word.Shift(u.vstack.op(stack1).imm,
                                              -u.vstack.op(stack0).imm));
        ELSE
          u.vstack.find(stack1, Force.anytemp);
          u.vstack.set_imm(stack0, Word.And(u.vstack.op(stack0).imm, 16_1F));
          u.cg.immOp(Op.oSHR, u.vstack.op(stack1), u.vstack.op(stack0).imm);
          u.vstack.newdest(u.vstack.op(stack1));
        END
      ELSE
        u.vstack.find(stack0, Force.regset, RegSet {Codex86.ECX});
        u.vstack.find(stack1, Force.anytemp);

        IF u.vstack.loc(stack1) = OLoc.imm THEN
          u.vstack.find(stack1, Force.anyreg);
        END;

        u.cg.unOp(Op.oSHR, u.vstack.op(stack1));
        u.vstack.newdest(u.vstack.op(stack1));
      END;

      u.vstack.discard(1);
    END
  END shift_right;

PROCEDURE rotate (u: U;  t: IType) =
  (* s1.t := Word.Rotate (s1.t, s0.t) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("rotate");
      u.wr.TName (t);
      u.wr.NL    ();
    END;

    u.vstack.dorotate();
  END rotate;

PROCEDURE rotate_left  (u: U;  t: IType) =
  (* s1.t := Word.Rotate (s1.t, s0.t) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("rotate_left");
      u.wr.TName (t);
      u.wr.NL    ();
    END;

    u.vstack.unlock();
    WITH stack0 = u.vstack.pos(0, "rotate_left"),
         stack1 = u.vstack.pos(1, "rotate_left") DO
      IF u.vstack.loc(stack0) = OLoc.imm THEN
        IF u.vstack.loc(stack1) = OLoc.imm THEN
          u.vstack.set_imm(stack1, Word.Rotate(u.vstack.op(stack1).imm,
                                               u.vstack.op(stack0).imm));
        ELSE
          u.vstack.find(stack1, Force.anytemp);
          u.vstack.set_imm(stack0, Word.And(u.vstack.op(stack0).imm, 16_1F));
          u.cg.immOp(Op.oROL, u.vstack.op(stack1), u.vstack.op(stack0).imm);
          u.vstack.newdest(u.vstack.op(stack1));
        END
      ELSE
        u.vstack.find(stack0, Force.regset, RegSet {Codex86.ECX});
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

PROCEDURE rotate_right (u: U;  t: IType) =
  (* s1.t := Word.Rotate (s1.t, -s0.t) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("rotate_right");
      u.wr.TName (t);
      u.wr.NL    ();
    END;

    u.vstack.unlock();
    WITH stack0 = u.vstack.pos(0, "rotate_right"),
         stack1 = u.vstack.pos(1, "rotate_right") DO
      IF u.vstack.loc(stack0) = OLoc.imm THEN
        IF u.vstack.loc(stack1) = OLoc.imm THEN
          u.vstack.set_imm(stack1, Word.Rotate(u.vstack.op(stack1).imm,
                                               -u.vstack.op(stack0).imm));
        ELSE
          u.vstack.find(stack1, Force.anytemp);
          u.vstack.set_imm(stack0, Word.And(u.vstack.op(stack0).imm, 16_1F));
          u.cg.immOp(Op.oROR, u.vstack.op(stack1), u.vstack.op(stack0).imm);
          u.vstack.newdest(u.vstack.op(stack1));
        END
      ELSE
        u.vstack.find(stack0, Force.regset, RegSet {Codex86.ECX});
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

PROCEDURE widen (u: U;  sign: BOOLEAN) =
  (* s0.I64 := s0.I32;  IF sign THEN SignExtend s0;  *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("widen");
      u.wr.Bool  (sign);
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

PROCEDURE extract (u: U;  t: IType;  sign: BOOLEAN) =
  (* s2.t := Word.Extract(s2.t, s1.t, s0.t);
     IF sign THEN SignExtend s2 END; pop(2) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("extract");
      u.wr.TName (t);
      u.wr.Bool  (sign);
      u.wr.NL    ();
    END;

    u.vstack.doextract(sign);
  END extract;

PROCEDURE extract_n (u: U;  t: IType;  sign: BOOLEAN;  n: INTEGER) =
  (* s1.t := Word.Extract(s1.t, s0.t, n);
     IF sign THEN SignExtend s1 END; pop(1) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("extract_n");
      u.wr.TName (t);
      u.wr.Bool  (sign);
      u.wr.Int   (n);
      u.wr.NL    ();
    END;

    u.vstack.doextract_n(sign, n);
  END extract_n;

PROCEDURE extract_mn (u: U;  t: IType;  sign: BOOLEAN;  m, n: INTEGER) =
  (* s0.t := Word.Extract(s0.t, m, n);
     IF sign THEN SignExtend s0 END; *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("extract_mn");
      u.wr.TName (t);
      u.wr.Bool  (sign);
      u.wr.Int   (m); 
      u.wr.Int   (n);
      u.wr.NL    ();
    END;

    u.vstack.doextract_mn(sign, m, n);
  END extract_mn;

PROCEDURE insert  (u: U;  t: IType) =
  (* s3.t := Word.Insert (s3.t, s2.t, s1.t, s0.t) ; pop(3) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("insert");
      u.wr.TName (t);
      u.wr.NL    ();
    END;

    u.vstack.doinsert();
  END insert;

PROCEDURE insert_n  (u: U;  t: IType;  n: INTEGER) =
  (* s2.t := Word.Insert (s2.t, s1.t, s0.t, n) ; pop(2) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("insert_n");
      u.wr.TName (t);
      u.wr.Int   (n);
      u.wr.NL    ();
    END;

    u.vstack.doinsert_n(n);
  END insert_n;

PROCEDURE insert_mn  (u: U;  t: IType;  m, n: INTEGER) =
  (* s1.t := Word.Insert (s1.t, s0.t, m, n) ; pop(2) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("insert_mn");
      u.wr.TName (t);
      u.wr.Int   (m);
      u.wr.Int   (n);
      u.wr.NL    ();
    END;

    u.vstack.doinsert_mn(m, n);
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

PROCEDURE pop (u: U;  t: Type) =
  (* pop(1) (i.e. discard s0) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("pop");
      u.wr.TName (t);
      u.wr.NL    ();
    END;

    u.vstack.unlock();
    IF Target.FloatType [t] THEN
      WITH stack0 = u.vstack.pos(0, "pop") DO
        <* ASSERT u.vstack.loc(stack0) = OLoc.fstack *>
        u.cg.fstack_discard();
      END
    END;

    u.vstack.discard(1);
  END pop;

PROCEDURE copy_n (u: U;  z: IType;  t: MType;  overlap: BOOLEAN) =
  (* Mem[s2.A:s0.z] := Mem[s1.A:s0.z]; pop(3)*)
  CONST Mover = ARRAY BOOLEAN OF Builtin { Builtin.memcpy, Builtin.memmove };
  VAR shift, n: INTEGER;  mover := Mover [overlap];
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("copy_n");
      u.wr.TName (z);
      u.wr.TName (t);
      u.wr.Bool  (overlap);
      u.wr.NL    ();
    END;

    WITH stack0 = u.vstack.pos(0, "copy_n") DO
      IF u.vstack.loc(stack0) = OLoc.imm THEN
        n := u.vstack.op(stack0).imm;
        u.vstack.discard(1);
        copy(u, n, t, overlap);
        RETURN;
      END
    END;

    IF CG_Bytes[t] # 1 THEN
      WITH stack0 = u.vstack.pos(0, "copy_n") DO
        u.vstack.unlock();

        CASE CG_Bytes[t] OF
          2 => shift := 1;
        | 4 => shift := 2;
        | 8 => shift := 3;
        ELSE
          u.Err("Unknown MType size in copy_n");
        END;

        u.vstack.find(stack0, Force.anyreg);

        u.cg.immOp(Op.oSAL, u.vstack.op(stack0), shift);
      END
    END;

    start_int_proc (u, mover);
    pop_param (u, z);
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

    movereg := u.vstack.freereg();

    WITH stop0 = u.vstack.op(u.vstack.pos(0, "inline_copy")),
         stop1 = u.vstack.op(u.vstack.pos(1, "inline_copy")) DO
      FOR i := start TO end BY step DO
        u.cg.fast_load_ind(movereg, stop0, i * size, size);
        u.cg.store_ind(u.cg.reg[movereg], stop1, i * size, faketype[size]);
      END
    END
  END inline_copy;

PROCEDURE string_copy (u: U; n, size: INTEGER; forward: BOOLEAN) =
  BEGIN
    u.vstack.corrupt(Codex86.ECX);
    u.cg.movImm(u.cg.reg[Codex86.ECX], n);

    IF forward THEN
      u.cg.noargOp(Op.oCLD);
    ELSE
      u.cg.immOp(Op.oADD, u.cg.reg[Codex86.ESI], (n - 1) * size);
      u.cg.immOp(Op.oADD, u.cg.reg[Codex86.EDI], (n - 1) * size);
      u.cg.noargOp(Op.oSTD);
    END;

    u.cg.noargOp(Op.oREP);
    CASE size OF
      1 => u.cg.noargOp(Op.oMOVSB);
    | 2 => u.cg.MOVSWOp();
    | 4 => u.cg.noargOp(Op.oMOVSD);
    ELSE
      u.Err("Illegal size in copy");
    END;

    IF NOT forward THEN
      u.cg.noargOp(Op.oCLD);
    END
  END string_copy;

PROCEDURE copy (u: U;  n: INTEGER;  t: MType;  overlap: BOOLEAN) =
  (* Mem[s1.A:sz] := Mem[s0.A:sz]; pop(2)*)
  VAR size := CG_Bytes[t];
      forward, end: Label;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("copy");
      u.wr.Int   (n);
      u.wr.TName (t);
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
        u.vstack.find(stack0, Force.regset, RegSet { Codex86.ESI } );
        u.vstack.find(stack1, Force.regset, RegSet { Codex86.EDI } );
      ELSE
        u.vstack.find(stack0, Force.anyreg, RegSet {}, TRUE);
        u.vstack.find(stack1, Force.anyreg, RegSet {}, TRUE);
      END
    END;

    IF overlap AND n > 1 THEN
      forward := u.cg.reserve_labels(1, TRUE);
      end := u.cg.reserve_labels(1, TRUE);
      u.cg.binOp(Op.oCMP, u.cg.reg[Codex86.ESI], u.cg.reg[Codex86.EDI]);
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
      u.vstack.newdest(u.cg.reg[Codex86.ESI]);
      u.vstack.newdest(u.cg.reg[Codex86.EDI]);
    END;

    u.vstack.discard(2);
  END copy;

PROCEDURE zero_n (u: U;  z: IType;  t: MType) =
  (* Mem[s1.A:s0.z] := 0; pop(2) *)
  VAR shift, n: INTEGER;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("zero_n");
      u.wr.TName (z);
      u.wr.TName (t);
      u.wr.NL    ();
    END;

    WITH stack0 = u.vstack.pos(0, "zero_n") DO
      IF u.vstack.loc(stack0) = OLoc.imm THEN
        n := u.vstack.op(stack0).imm;
        u.vstack.discard(1);
        zero(u, n, t);
        RETURN;
      END
    END;

    IF CG_Bytes[t] # 1 THEN
      WITH stack0 = u.vstack.pos(0, "zero_n") DO
        u.vstack.unlock();
        u.vstack.find(stack0, Force.anyreg);

        CASE CG_Bytes[t] OF
          2 => shift := 1;
        | 4 => shift := 2;
        | 8 => shift := 3;
        ELSE
          u.Err("Unknown MType size in zero_n");
        END;

        u.cg.immOp(Op.oSAL, u.vstack.op(stack0), shift);
      END
    END;

    start_int_proc (u, Builtin.memset);
    pop_param (u, z);
    u.vstack.pushimm (0);
    pop_param (u, Type.Int32);
    pop_param (u, Type.Addr);
    call_int_proc (u, Builtin.memset);

    u.vstack.discard(1);
  END zero_n;

PROCEDURE zero (u: U;  n: INTEGER;  t: MType) =
  (* Mem[s0.A:sz] := 0; pop(1) *)
  VAR size := CG_Bytes[t];
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("zero");
      u.wr.Int   (n);
      u.wr.TName (t);
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
      u.vstack.find(u.vstack.pos(0, "zero"), Force.regset,
                    RegSet { Codex86.EDI } );
      u.vstack.corrupt(Codex86.EAX);
      u.vstack.corrupt(Codex86.ECX);

      u.cg.binOp(Op.oXOR, u.cg.reg[Codex86.EAX], u.cg.reg[Codex86.EAX]);
      u.cg.movImm(u.cg.reg[Codex86.ECX], n);

      u.cg.noargOp(Op.oCLD);
      u.cg.noargOp(Op.oREP);
      CASE size OF
        1 => u.cg.noargOp(Op.oSTOSB);
      | 2 => u.cg.STOSWOp();
      | 4 => u.cg.noargOp(Op.oSTOSD);
      ELSE
             u.Err("Illegal size in zero");
      END;
      u.vstack.newdest(u.cg.reg[Codex86.EDI]);

    ELSE
      WITH stack0 = u.vstack.pos(0, "zero"), stop0 = u.vstack.op(stack0) DO
        u.vstack.find(stack0, Force.anyreg, RegSet {}, TRUE);
        FOR i := 0 TO n - 1 DO
          u.cg.store_ind(Operand { loc := OLoc.imm, imm := 0 },
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
    set_range, set_eq, set_ne, set_lt, set_le, set_gt, set_ge,
    set_member, set_singleton, memmove, memcpy, memset
  };

(* union .. sym_difference -> (n_bits, *c, *b, *a): Void
   range                   -> (b, a, *s): Void
   eq .. ge                -> (n_bits, *b, *a): Int
   member                  -> (elt, *set): Int
   singleton               -> (a, *s): Void *)

TYPE
  BP = RECORD
    name     : TEXT;
    n_params : INTEGER;
    ret_type : Type;
    lang     : TEXT;
  END;

CONST
  BuiltinDesc = ARRAY Builtin OF BP {
    BP { "set_union",          4, Type.Void,  "C" },
    BP { "set_difference",     4, Type.Void,  "C" },
    BP { "set_intersection",   4, Type.Void,  "C" },
    BP { "set_sym_difference", 4, Type.Void,  "C" },
    BP { "set_range",          3, Type.Void,  "C" },
    BP { "set_eq",             3, Type.Int32, "C" },
    BP { "set_ne",             3, Type.Int32, "C" },
    BP { "set_lt",             3, Type.Int32, "C" },
    BP { "set_le",             3, Type.Int32, "C" },
    BP { "set_gt",             3, Type.Int32, "C" },
    BP { "set_ge",             3, Type.Int32, "C" },
    BP { "set_member",         2, Type.Int32, "C" },
    BP { "set_singleton",      2, Type.Void,  "C" },
    BP { "memmove",            3, Type.Addr,  "C" },
    BP { "memcpy",             3, Type.Addr,  "C" },
    BP { "memset",             3, Type.Addr,  "C" }
  };


PROCEDURE start_int_proc (u: U;  b: Builtin) =
  BEGIN
    WITH  desc = BuiltinDesc [b], proc = u.builtins[b]  DO
      IF proc = NIL THEN
        proc := import_procedure (u, M3ID.Add (desc.name),
                                        desc.n_params, desc.ret_type,
                                        Target.FindConvention (desc.lang));
        FOR i := 1 TO desc.n_params DO
          EVAL declare_param (u, M3ID.NoID, 4, 4, Type.Addr, 0, FALSE, FALSE, 100);
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

PROCEDURE loophole (u: U;  from, two: ZType) =
  (* s0.to := LOOPHOLE(s0.from, to) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("loophole");
      u.wr.TName (from);
      u.wr.TName (two);
      u.wr.NL    ();
    END;

    u.vstack.doloophole(from, two);

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
        IF u.vstack.op(stack0).imm = 0 THEN
          reportfault(u, code);
        END
      ELSE
        u.vstack.find(stack0, Force.anyreg, RegSet {}, TRUE);

        IF NOT u.vstack.non_nil(u.vstack.reg(stack0)) THEN
          u.cg.immOp(Op.oCMP, u.vstack.op(stack0), 0);
          safelab := u.cg.reserve_labels(1, TRUE);
          u.cg.brOp(Cond.NE, safelab);
          reportfault(u, code);
          u.cg.set_label(safelab);
        END;

        u.vstack.set_non_nil(u.vstack.reg(stack0));
      END;
    END;
  END check_nil;

PROCEDURE check_lo (u: U;  t: IType;  READONLY i: Target.Int;  code: RuntimeError) =
  (* IF (s0.t < i) THEN abort(code) *)
  VAR int: INTEGER;  safelab: Label;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("check_lo");
      u.wr.TName (t);
      u.wr.TInt  (i);
      u.wr.Int   (ORD (code));
      u.wr.NL    ();
    END;

    EVAL TargetInt.ToInt(i, int);

    u.vstack.unlock();
    WITH stack0 = u.vstack.pos(0, "check_lo") DO
      IF u.vstack.loc(stack0) = OLoc.imm THEN
        IF u.vstack.op(stack0).imm < int THEN
          reportfault(u, code);
        END
      ELSE
        u.vstack.find(stack0, Force.anyreg);
        IF u.vstack.lower(u.vstack.reg(stack0)) >= int THEN
          (* ok *)
        ELSIF u.vstack.upper(u.vstack.reg(stack0)) < int THEN
          reportfault(u, code);
        ELSE
          u.cg.immOp(Op.oCMP, u.vstack.op(stack0), int);
          safelab := u.cg.reserve_labels(1, TRUE);
          u.cg.brOp(Cond.GE, safelab);
          reportfault(u, code);
          u.cg.set_label(safelab);
          u.vstack.set_lower(u.vstack.reg(stack0), int);
        END
      END
    END
  END check_lo;

PROCEDURE check_hi (u: U;  t: IType;  READONLY i: Target.Int;  code: RuntimeError) =
  (* IF (i < s0.t) THEN abort(code) *)
  VAR int: INTEGER;
      safelab: Label;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("check_hi");
      u.wr.TName (t);
      u.wr.TInt  (i);
      u.wr.Int   (ORD (code));
      u.wr.NL    ();
    END;

    EVAL TargetInt.ToInt(i, int);

    u.vstack.unlock();
    WITH stack0 = u.vstack.pos(0, "check_hi") DO
      IF u.vstack.loc(stack0) = OLoc.imm THEN
        IF int < u.vstack.op(stack0).imm THEN
          reportfault(u, code);
        END
      ELSE
        u.vstack.find(stack0, Force.anyreg);
        IF u.vstack.upper(u.vstack.reg(stack0)) <= int THEN
          (* ok *)
        ELSIF u.vstack.lower(u.vstack.reg(stack0)) > int THEN
          reportfault(u, code);
        ELSE
          u.cg.immOp(Op.oCMP, u.vstack.op(stack0), int);
          safelab := u.cg.reserve_labels(1, TRUE);
          u.cg.brOp(Cond.LE, safelab);
          reportfault(u, code);
          u.cg.set_label(safelab);
          u.vstack.set_upper(u.vstack.reg(stack0), int);
        END
      END
    END
  END check_hi;

PROCEDURE check_range (u: U;  t: IType;  READONLY a, b: Target.Int;  code: RuntimeError) =
  (* IF (s0.t < a) OR (b < s0.t) THEN abort(code) *)
  VAR inta, intb, lo, hi: INTEGER;   safelab, outrange: Label;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("check_range");
      u.wr.TInt (a);
      u.wr.TInt (b);
      u.wr.Int   (ORD (code));
      u.wr.NL    ();
    END;

    EVAL TargetInt.ToInt(a, inta);
    EVAL TargetInt.ToInt(b, intb);

    u.vstack.unlock();
    WITH stack0 = u.vstack.pos(0, "check_range") DO
      IF u.vstack.loc(stack0) = OLoc.imm THEN
        lo := u.vstack.op(stack0).imm;
        IF (lo < inta) OR (intb < lo) THEN
          reportfault(u, code);
        END;
        RETURN;
      END;

      u.vstack.find(stack0, Force.anyreg);
      WITH reg = u.vstack.reg(stack0) DO
        lo := u.vstack.lower(reg);
        hi := u.vstack.upper(reg);
        IF (inta <= lo) AND (hi <= intb) THEN
          (* ok *)
        ELSIF (hi < inta) OR (intb < lo) THEN
          reportfault(u, code);
        ELSIF (hi <= intb) THEN
          check_lo(u, t, a, code);
        ELSIF (lo >= inta) THEN
          check_hi(u, t, b, code);
        ELSIF (inta = 0) THEN
          (* 0 <= x <= b  ==>   UNSIGNED(x) <= b *)
          safelab := u.cg.reserve_labels(1, TRUE);
          u.cg.immOp(Op.oCMP, u.vstack.op(stack0), intb);
          u.cg.brOp(unscond [Cond.LE], safelab);
          reportfault(u, code);
          u.cg.set_label(safelab);
          u.vstack.set_upper(reg, intb);
          u.vstack.set_lower(reg, inta);
        ELSE
          safelab := u.cg.reserve_labels(1, TRUE);
          outrange := u.cg.reserve_labels(1, TRUE);
          u.cg.immOp(Op.oCMP, u.vstack.op(stack0), inta);
          u.cg.brOp(Cond.L, outrange);
          u.cg.immOp(Op.oCMP, u.vstack.op(stack0), intb);
          u.cg.brOp(Cond.LE, safelab);
          u.cg.set_label(outrange);
          reportfault(u, code);
          u.cg.set_label(safelab);
          u.vstack.set_upper(reg, intb);
          u.vstack.set_lower(reg, inta);
        END;
      END
    END
  END check_range;

PROCEDURE check_index (u: U;  t: IType;  code: RuntimeError) =
  (* IF NOT (0 <= s1.t < s0.t) THEN abort(code) END; pop *)
  (* s0.t is guaranteed to be positive so the unsigned
     check (s0.W <= s1.W) is sufficient. *)
  VAR safelab: Label;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("check_index");
      u.wr.TName (t);
      u.wr.Int   (ORD (code));
      u.wr.NL    ();
    END;


    u.vstack.unlock();
    WITH stack0 = u.vstack.pos(0, "check_index"),
         stack1 = u.vstack.pos(1, "check_index") DO
      IF u.vstack.loc(stack0) = OLoc.imm AND
         u.vstack.loc(stack1) = OLoc.imm THEN
        IF Word.LE(u.vstack.op(stack0).imm, u.vstack.op(stack1).imm) THEN
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

PROCEDURE check_eq (u: U;  t: IType;  code: RuntimeError) =
  (* IF (s0.t # s1.t) THEN abort(code);  Pop (2) *)
  VAR safelab: Label;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("check_eq");
      u.wr.TName (t);
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
  VAR info := ORD (code) + u.lineno * 32;
  BEGIN
    u.cg.movImm(u.cg.reg[Codex86.EAX], info);
    u.cg.intCall(u.reportlabel);
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

    u.cg.set_label(u.reportlabel);

    labelname := M3ID.ToText (u.global_var.name) & "_CRASH";

    reportsymbol := u.obj.define_symbol(M3ID.Add(labelname), Seg.Text,
                                        u.obj.cursor(Seg.Text));
    u.obj.begin_procedure(reportsymbol);

    u.cg.pushOp(u.cg.reg[Codex86.EBP]);
    u.cg.movOp(u.cg.reg[Codex86.EBP], u.cg.reg[Codex86.ESP]);

    u.cg.pushOp(u.cg.reg[Codex86.EAX]);  (* runtime error code + line number *)

    IF (repfault # NIL) THEN
      load_address(u, u.global_var, 0);
      INC(u.in_proc_call);
      pop_param(u, Type.Addr);
      DEC(u.in_proc_call);
      load(u, repfault, repfoff, Type.Addr, Type.Addr);
      u.cg.rmCall(u.vstack.op(u.vstack.pos(0, "makereportproc")));
    ELSIF (repproc # NIL) THEN
      start_call_direct(u, repproc, 0, Type.Void);
      INC(u.call_param_size[u.in_proc_call-1], 4); (* remember error code *)
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
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("add_offset");
      u.wr.Int   (i);
      u.wr.NL    ();
    END;

    u.vstack.unlock();
    WITH stack0 = u.vstack.pos(0, "add_offset") DO
      IF u.vstack.loc(stack0) = OLoc.imm THEN
        u.vstack.set_imm(stack0, u.vstack.op(stack0).imm + i);
      ELSE
        u.vstack.find(stack0, Force.anytemp, RegSet {}, TRUE);

        u.cg.immOp(Op.oADD, u.vstack.op(stack0), i);

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

PROCEDURE index_address (u: U;  t: IType;  size: INTEGER) =
  (* s1.A := s1.A + s0.t * size ; pop *)
  VAR shift: INTEGER;
      neg := FALSE;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("index_address");
      u.wr.TName (t);
      u.wr.Int   (size);
      u.wr.NL    ();
    END;

    IF size = 0 THEN
      u.Err("size = 0 in index_address");
    END;

    IF size < 0 THEN
      size := -size;
      neg := TRUE;
    END;

    shift := log2(size);

    u.vstack.doindex_address(shift, size, neg);
  END index_address;

(*------------------------------------------------------- procedure calls ---*)

PROCEDURE start_call_direct (u: U;  p: Proc;  lev: INTEGER;  t: Type) =
  (* begin a procedure call to a procedure at static level 'lev'. *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("start_call_direct");
      u.wr.PName (p);
      u.wr.Int   (lev);  
      u.wr.TName (t);
      u.wr.NL    ();
    END;

    <* ASSERT u.in_proc_call < 2 *>

    u.static_link[u.in_proc_call] := NIL;
    u.call_param_size[u.in_proc_call] := 0;
    INC(u.in_proc_call);
  END start_call_direct;

PROCEDURE start_call_indirect (u: U;  t: Type;  cc: CallingConvention) =
  (* begin a procedure call to a procedure at static level 'lev'. *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("start_call_indirect");
      u.wr.TName (t);
      u.wr.Txt   (cc.name);
      u.wr.NL    ();
    END;

    <* ASSERT u.in_proc_call < 2 *>

    u.static_link[u.in_proc_call] := NIL;
    u.call_param_size[u.in_proc_call] := 0;
    INC(u.in_proc_call);
  END start_call_indirect;

PROCEDURE pop_param (u: U;  t: MType) =
  (* pop s0 and make it the "next" paramter in the current call *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("pop_param");
      u.wr.TName (t);
      u.wr.NL    ();
    END;

    <* ASSERT u.in_proc_call > 0 *>

    u.vstack.unlock();
    WITH stack0 = u.vstack.pos(0, "pop_param") DO
      IF Target.FloatType [t] THEN
        IF t = Type.Reel THEN
          u.cg.immOp(Op.oSUB, u.cg.reg[Codex86.ESP], 4);
        ELSE
          u.cg.immOp(Op.oSUB, u.cg.reg[Codex86.ESP], 8);
        END;

        u.cg.f_storeind(u.cg.reg[Codex86.ESP], 0, t);
      ELSE

        u.vstack.find(stack0, Force.anyregimm);
        u.cg.pushOp(u.vstack.op(stack0));
      END
    END;

    u.vstack.discard(1);

    IF CG_Bytes[t] <= 4 THEN
      INC(u.call_param_size[u.in_proc_call-1], 4);
    ELSE
      <* ASSERT CG_Bytes[t] = 8 *>
      INC(u.call_param_size[u.in_proc_call-1], 8);
    END
  END pop_param;

PROCEDURE load_stack_param (u: U; t: ZType; depth: INTEGER) =
  BEGIN
    u.vstack.unlock();

    <* ASSERT u.in_proc_call > 0 *>

    WITH stack = u.vstack.pos(depth, "load_stack_param") DO
      <* ASSERT NOT Target.FloatType [t] *>

      u.vstack.find(stack, Force.anyregimm);
      u.cg.pushOp(u.vstack.op(stack));
    END;

    INC(u.call_param_size[u.in_proc_call-1], 4);
  END load_stack_param;

PROCEDURE pop_struct (u: U;  s: ByteSize;  a: Alignment) =
  (* pop s0 and make it the "next" parameter in the current call *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("pop_struct");
      u.wr.Int   (s);
      u.wr.Int   (a);
      u.wr.NL    ();
    END;

    <* ASSERT u.in_proc_call > 0 *>

    <* ASSERT a <= 4 *>

    s := Word.And(s + 3, 16_FFFFFFFC);

    u.vstack.unlock();

    WITH stack0 = u.vstack.pos(0, "pop_struct") DO

      IF s > 32 THEN
        u.cg.immOp(Op.oSUB, u.cg.reg[Codex86.ESP], s);

        u.vstack.find(stack0, Force.regset, RegSet { Codex86.ESI });
        u.vstack.corrupt(Codex86.EDI);
        u.vstack.corrupt(Codex86.ECX);

        u.cg.movOp(u.cg.reg[Codex86.EDI], u.cg.reg[Codex86.ESP]);
        u.cg.movImm(u.cg.reg[Codex86.ECX], s DIV 4);

        u.cg.noargOp(Op.oCLD);
        u.cg.noargOp(Op.oREP);
        u.cg.noargOp(Op.oMOVSD);

        u.vstack.newdest(u.cg.reg[Codex86.ESI]);
      ELSE
        u.vstack.find(stack0, Force.anyreg, RegSet {}, TRUE);

        WITH temp = u.vstack.freereg() DO
          FOR i := 1 TO (s DIV 4) DO
            u.cg.load_ind(temp, u.vstack.op(stack0), s - (i * 4), Type.Int32);
            u.cg.pushOp(u.cg.reg[temp]);
          END
        END
      END
    END;

    u.vstack.discard(1);

    INC(u.call_param_size[u.in_proc_call-1], s);
  END pop_struct;

PROCEDURE pop_static_link (u: U) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("pop_static_link");
      u.wr.NL    ();
    END;

    <* ASSERT u.in_proc_call > 0 *>

    u.static_link[u.in_proc_call-1] := declare_temp(u, 4, 4, Type.Addr, FALSE);

    u.vstack.pop(MVar {var := u.static_link[u.in_proc_call-1],
                       o := 0, t := Type.Addr} );
  END pop_static_link;

PROCEDURE call_direct (u: U; p: Proc;  t: Type) =
  VAR realproc := NARROW(p, x86Proc);
  (* call the procedure identified by block b.  The procedure
     returns a value of type t. *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("call_direct");
      u.wr.PName (p);
      u.wr.TName (t);
      u.wr.NL    ();
    END;

    <* ASSERT u.in_proc_call > 0 *>

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
       AND u.call_param_size[u.in_proc_call-1] > 0 THEN
        u.cg.immOp(Op.oADD, u.cg.reg[Codex86.ESP],
                   u.call_param_size[u.in_proc_call-1]);
    END;

    IF t = Type.Struct THEN
      t := Type.Addr;
    END;

    IF t # Type.Void THEN
      IF Target.FloatType [t] THEN
        u.vstack.pushnew(t, Force.any);
        u.cg.f_pushnew();
      ELSE
        u.vstack.pushnew(FixReturnValue(u, t), Force.regset,
                         RegSet { Codex86.EAX });
      END
    END;

    DEC(u.in_proc_call);
  END call_direct;

PROCEDURE call_indirect (u: U; t: Type;  cc: CallingConvention) =
  (* call the procedure whose address is in s0.A and pop s0.  The
     procedure returns a value of type t. *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("call_indirect");
      u.wr.TName (t);
      u.wr.Txt   (cc.name);
      u.wr.NL    ();
    END;

    <* ASSERT u.in_proc_call > 0 *>

    u.vstack.releaseall();

    IF u.static_link[u.in_proc_call-1] # NIL THEN
      u.cg.movOp(u.cg.reg[Codex86.ECX],
                 Operand { loc := OLoc.mem,
                           mvar :=
                             MVar { var := u.static_link[u.in_proc_call-1],
                                    o := 0,
                                    t := Type.Addr } } );
      free_temp(u, u.static_link[u.in_proc_call-1]);
      u.static_link[u.in_proc_call-1] := NIL;
    END;

    u.cg.rmCall(u.vstack.op(u.vstack.pos(0, "call_indirect")));
    u.vstack.discard(1);

    IF (cc.m3cg_id = 0)
      AND u.call_param_size[u.in_proc_call-1] > 0 THEN
      (* caller-cleans calling convention *)
      u.cg.immOp(Op.oADD, u.cg.reg[Codex86.ESP],
                 u.call_param_size[u.in_proc_call-1]);
    END;

    IF t = Type.Struct THEN
      t := Type.Addr;
    END;

    IF t # Type.Void THEN
      IF Target.FloatType [t] THEN
        u.vstack.pushnew(t, Force.any);
        u.cg.f_pushnew();
      ELSE
        u.vstack.pushnew(FixReturnValue(u, t), Force.regset,
                         RegSet { Codex86.EAX });
      END
    END;

    DEC(u.in_proc_call);
  END call_indirect;

PROCEDURE FixReturnValue (u: U;  t: Type): Type =
  (* Apparently, the Microsoft C compiler doesn't return full 32-bit values
     in EAX for procedures with 8 or 16-bit return types, but this code generator
     assumes that registers always contain 32-bit values.  So, we compensate
     here... *)
  BEGIN
    CASE t OF
    | Type.Int8 => (* 8-bit signed integer *)
        u.cg.CBWOp ();            (* AX  := SIGN-EXTEND (AL) *)
        u.cg.noargOp (Op.oCWDE);  (* EAX := SIGN-EXTEND (AX) *)
        t := Type.Int32;

    | Type.Int16 => (* 16-bit signed integer *)
        (* EAX := SIGN-EXTEND (AX) *)
        u.cg.noargOp (Op.oCWDE);
        t := Type.Int32;

    | Type.Int32 => (* 32-bit signed integer *)
        (* no code, just fix the type *)
        t := Type.Int32;

    | Type.Int64 =>
        <*ASSERT FALSE*>

    | Type.Word8 => (* 8-bit unsigned integer *)
        u.cg.immOp (Op.oAND, u.cg.reg[Codex86.EAX], 16_ff);  (* EAX &= 16_ff *)
        t := Type.Word32;

    | Type.Word16 => (* 16-bit unsigned integer *)
        u.cg.immOp (Op.oAND, u.cg.reg[Codex86.EAX], 16_ffff);  (* EAX &= 16_ffff *)
        t := Type.Word32;

    | Type.Word32 => (* 32-bit unsigned Integer *)
        (* no code, just fix the type *)
        t := Type.Word32;

    | Type.Word64 =>
        <*ASSERT FALSE*>

    ELSE (* value is ok *)
    END;
    RETURN t;
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

    IF realproc.lev = 0 THEN
      u.vstack.pushimm(0);
    ELSE
      u.vstack.unlock();
      u.vstack.pushnew(Type.Addr, Force.anyreg);
      u.cg.get_frame(u.vstack.op(u.vstack.pos(0, "load_static_link")).reg,
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
      u.cg.movImm(u.cg.reg[Codex86.ECX], 0);
    ELSE
      u.vstack.unlock();
      u.vstack.corrupt(Codex86.ECX);
      u.cg.get_frame(Codex86.ECX, realproc.parent, u.current_proc);
    END
  END load_static_link_toC;

(*---------------------------------------------------------- produce code ---*)

PROCEDURE intregcmp (u: U; tozero: BOOLEAN): BOOLEAN =
  BEGIN
    IF tozero THEN
      u.vstack.doimm(Op.oCMP, 0, FALSE);
      RETURN FALSE;
    ELSE
      RETURN u.vstack.dobin(Op.oCMP, TRUE, FALSE);
    END
  END intregcmp;

PROCEDURE fltregcmp (u: U; tozero: BOOLEAN): BOOLEAN =
  VAR reversed := FALSE;
  BEGIN
    IF tozero THEN
      u.cg.immFOp(FOp.fCOMP, FIm.Z);
      u.vstack.discard(1);
    ELSE
      IF u.cg.ftop_inmem THEN
        u.cg.binFOp(FOp.fCOMP, 1);
      ELSE
        u.cg.binFOp(FOp.fCOMPP, 1);
        reversed := TRUE;
      END;
      u.vstack.discard(2);
    END;

    u.vstack.unlock();
    u.vstack.corrupt(Codex86.EAX);
    u.cg.noargFOp(FOp.fNSTSWAX);
    u.cg.noargOp(Op.oSAHF);

    RETURN reversed;
  END fltregcmp;

PROCEDURE condset (u: U; cond: Cond; t: ZType) =
  VAR reversed := FALSE;
  BEGIN
    IF NOT Target.FloatType[t] THEN
      reversed := intregcmp(u, cond < Cond.E);
      IF reversed THEN
        cond := revcond[cond];
      END;
      IF t # Type.Int32 THEN
        cond := unscond[cond];
      END
    ELSE
      reversed := fltregcmp(u, cond < Cond.E);
      IF reversed THEN
        cond := revcond[cond];
      END;
      cond := unscond[cond]; (* FCOM sets the unsigned compare flags *)
    END;
    u.vstack.unlock();
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

PROCEDURE Cmt (u: U;  t: TEXT;  VAR width: INTEGER) =
  VAR ch: CHAR;
  BEGIN
    IF (NOT u.debug OR t = NIL) THEN RETURN END;
    FOR i := 0 TO Text.Length (t) - 1 DO
      ch := Text.GetChar (t, i);
      IF (width = -1) THEN u.wr.OutT ("\t# "); width := 0; END;
      IF (ch = '\n') THEN
        u.wr.NL ();
        width := -1;
      ELSE
        u.wr.OutC (ch);
      END
    END;
  END Cmt;

BEGIN
END M3x86.
