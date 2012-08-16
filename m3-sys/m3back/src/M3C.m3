(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Jun 20 16:19:09 PDT 1995 by kalsow     *)
(*      modified on Wed Nov 23 13:57:47 PST 1994 by isard      *)

MODULE M3C;

IMPORT TextSeq, Wr, Text, Fmt;
IMPORT M3CG, M3CG_Ops, Target, TIntN, TFloat, TargetMap;
IMPORT Stdio;
IMPORT RTIO;
FROM TargetMap IMPORT CG_Bytes;
FROM M3CG IMPORT Name, ByteOffset, TypeUID, CallingConvention;
FROM M3CG IMPORT BitSize, ByteSize, Alignment, Frequency;
FROM M3CG IMPORT Var, Proc, Label, Sign, BitOffset;
FROM M3CG IMPORT Type, ZType, AType, RType, IType, MType;
FROM M3CG IMPORT CompareOp, ConvertOp, RuntimeError, MemoryOrder, AtomicOp;
FROM Target IMPORT CGType;
FROM M3CG_Ops IMPORT ErrorHandler;
IMPORT Wrx86, M3ID, TInt;
(*
IMPORT Wrx86, M3ID, M3CField, M3CFieldSeq;
IMPORT SortedIntRefTbl;
*)

(* ztype: zero extended type -- a "larger" type that is a multiple of 32 bits in size
 *                              a type to store in registers, a type
 *                              to store on the compile-time or runtime stack
 *                              a type to pass a parameter as
 * mtype: memory type -- a "smaller" type that is possibly truncated to fit
 *        an in-memory layout
 *)

REVEAL
  U = Public BRANDED "M3C.U" OBJECT
        wr     : Wrx86.T := NIL;
        c      : Wr.T := NIL;
        debug  := FALSE;
        stack  : TextSeq.T := NIL;
        enum_type: TEXT := NIL;
        (*enum: Enum_t := NIL;*)
        enum_id: TEXT := NIL;
        enum_value: CARDINAL := 0;
        unit_name: TEXT := NIL;
        function: CProc := NIL;
        param_count := 0;
        init_inited := FALSE;
        init_exported := FALSE;
        (*init_fields: CFieldSeq := NIL;*)
        init_fields: TextSeq.T := NIL;
        current_init_offset: INTEGER := 0;
        initializer: TextSeq.T := NIL;
        label := 0;
        in_procedure := 0;
        in_block := 0;
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
        pop  := cg_pop;
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

VAR BitSizeToEnumCGType := ARRAY [0..32] OF M3CG.Type { M3CG.Type.Void, .. };

VAR anonymousCounter: INTEGER;

PROCEDURE FixName(name: Name): Name =
BEGIN
  IF name = 0 OR Text.GetChar (M3ID.ToText (name), 0) = '*' THEN
    WITH t = M3ID.Add("L_" & Fmt.Int(anonymousCounter)) DO
      INC(anonymousCounter);
      RETURN t;
    END;
  END;
  RETURN name;
END FixName;

(*
TYPE CField = M3CField.T;
TYPE CFieldSeq = M3CFieldSeq.T;

TYPE Type_t = OBJECT
  bit_size: INTEGER := 0;  (* FUTURE Target.Int or LONGINT *)
  byte_size: INTEGER := 0; (* FUTURE Target.Int or LONGINT *)
  typeid: INTEGER := 0;
  cg_type: M3CG.Type := M3CG.Type.Addr;
  (*name_id: INTEGER;
  name_text: TEXT;*)
END;

(* We probably need "Ordinal_t": Integer_t, Enum_t, Subrange_t *)

TYPE Integer_t = Type_t OBJECT END;
TYPE Float_t  = Type_t OBJECT END;
TYPE Record_t  = Type_t OBJECT END;

TYPE Enum_t  = Type_t OBJECT
  (* min is zero *)
  max: INTEGER; (* FUTURE Target.Int or LONGINT *)
END;

TYPE Subrange_t  = Type_t OBJECT
  min: INTEGER; (* FUTURE Target.Int or LONGINT *)
  max: INTEGER; (* FUTURE Target.Int or LONGINT *)
END;

TYPE Ref_t  = Type_t OBJECT
  referent: Type_t;
END;

TYPE Array_t = Type_t OBJECT
  index_typeid: INTEGER;
  element_typeid: INTEGER;
  index_type: Type_t;
  element_type: Type_t;
END;

TYPE FixedArray_t = Array_t OBJECT END;
TYPE OpenArray_t = Array_t OBJECT END;

VAR typeidToType := NEW(SortedIntRefTbl.Default).init();

PROCEDURE Type_Init(t: Type_t): Type_t =
BEGIN
  IF t.bit_size = 0 THEN
    t.bit_size := TargetMap.CG_Size[t.cg_type];
  END;
  IF t.byte_size = 0 THEN
    t.byte_size := TargetMap.CG_Bytes[t.cg_type];
  END;
  EVAL typeidToType.put(t.typeid, t);
  RETURN t;
END Type_Init;

PROCEDURE TypeidToType_Get(typeid: TypeUID): Type_t =
VAR type: REFANY := NIL;
BEGIN
  EVAL typeidToType.get(typeid, type);
  RETURN NARROW(type, Type_t);
END TypeidToType_Get;
*)

(* see RTBuiltin.mx
   see RT0.i3 *)
<*NOWARN*>CONST UID_INTEGER = 16_195C2A74; (* INTEGER *)
<*NOWARN*>CONST UID_LONGINT = 16_05562176; (* LONGINT *)
<*NOWARN*>CONST UID_WORD = 16_97E237E2; (* CARDINAL *)
<*NOWARN*>CONST UID_LONGWORD = 16_9CED36E7; (* LONGCARD *)
<*NOWARN*>CONST UID_REEL = 16_48E16572; (* REAL *)
<*NOWARN*>CONST UID_LREEL = 16_94FE32F6; (* LONGREAL *)
<*NOWARN*>CONST UID_XREEL = 16_9EE024E3; (* EXTENDED *)
<*NOWARN*>CONST UID_BOOLEAN = 16_1E59237D; (* BOOLEAN [0..1] *)
<*NOWARN*>CONST UID_CHAR = 16_56E16863; (* CHAR [0..255] *)
<*NOWARN*>CONST UID_WIDECHAR = 16_88F439FC;
<*NOWARN*>CONST UID_MUTEX = 16_1541F475; (* MUTEX *)
<*NOWARN*>CONST UID_TEXT = 16_50F86574; (* TEXT *)
<*NOWARN*>CONST UID_UNTRACED_ROOT = 16_898EA789; (* UNTRACED ROOT *)
<*NOWARN*>CONST UID_ROOT = 16_9D8FB489; (* ROOT *)
<*NOWARN*>CONST UID_REFANY = 16_1C1C45E6; (* REFANY *)
<*NOWARN*>CONST UID_ADDR = 16_08402063; (* ADDRESS *)
<*NOWARN*>CONST UID_RANGE_0_31 = 16_2DA6581D; (* [0..31] *)
<*NOWARN*>CONST UID_RANGE_0_63 = 16_2FA3581D; (* [0..63] *)
<*NOWARN*>CONST UID_PROC1 = 16_9C9DE465; (* PROCEDURE (x, y: INTEGER): INTEGER *)
<*NOWARN*>CONST UID_PROC2 = 16_20AD399F; (* PROCEDURE (x, y: INTEGER): BOOLEAN *)
<*NOWARN*>CONST UID_PROC3 = 16_3CE4D13B; (* PROCEDURE (x: INTEGER): INTEGER *)
<*NOWARN*>CONST UID_PROC4 = 16_FA03E372; (* PROCEDURE (x, n: INTEGER): INTEGER *)
<*NOWARN*>CONST UID_PROC5 = 16_509E4C68; (* PROCEDURE (x: INTEGER;  n: [0..31]): INTEGER *)
<*NOWARN*>CONST UID_PROC6 = 16_DC1B3625; (* PROCEDURE (x: INTEGER;  n: [0..63]): INTEGER *)
<*NOWARN*>CONST UID_PROC7 = 16_EE17DF2C; (* PROCEDURE (x: INTEGER;  i, n: CARDINAL): INTEGER *)
<*NOWARN*>CONST UID_PROC8 = 16_B740EFD0; (* PROCEDURE (x, y: INTEGER;  i, n: CARDINAL): INTEGER *)
<*NOWARN*>CONST UID_NULL = 16_48EC756E; (* NULL *)

TYPE CVar = M3CG.Var OBJECT
  name: Name;
  type: Type;
  is_const := FALSE;
END;

TYPE CProc = M3CG.Proc OBJECT
  name: Name;
  n_params: INTEGER := 0; (* FUTURE: remove this *)
  return_type: Type;
  level: INTEGER := 0;
  callingConvention: CallingConvention;
  exported: BOOLEAN;
  parent: CProc := NIL;
  params: REF ARRAY OF CVar;
  locals: TextSeq.T := NIL;
END;

(*CONST IntegerTypeSizes = ARRAY OF INTEGER {8, 16, 32, 64};
CONST IntegerTypeSignedness = ARRAY OF BOOLEAN { FALSE, TRUE };*)

(*---------------------------------------------------------------------------*)

CONST Prefix = ARRAY OF TEXT {
"#ifdef __cplusplus",
"extern \"C\" {",
"#endif",
"#define op ==",
(*
"/* const is extern const in C, but static const in C++,",
" * but gcc gives a warning for the correct portable form \"extern const\"",
" */",
"#if defined(__cplusplus) || !defined(__GNUC__)",
"#define EXTERN_CONST extern const",
"#else",
"#define EXTERN_CONST const",
"#endif",
*)
"#if !(defined(_MSC_VER) || defined(__cdecl))",
"#define __cdecl",
"#endif",
"typedef signed char INT8;",
"typedef unsigned char UINT8, WORD8;",
"typedef short INT16;",
"typedef unsigned short UINT16, WORD16;",
"typedef int INT32;",
"typedef unsigned int UINT32, WORD32;",
"#if !(defined(_MSC_VER) || defined(__DECC) || defined(__int64))",
"#define __int64 long long",
"#endif",
"typedef signed __int64 INT64;",
"typedef unsigned __int64 UINT64;",
(*
"/* WORD_T/INTEGER are always exactly the same size as a pointer.",
" * VMS sometimes has 32bit size_t/ptrdiff_t but 64bit pointers.",
" */",
"#if __INITIAL_POINTER_SIZE == 64",
"typedef __int64 INTEGER;",
"typedef unsigned __int64 WORD_T;",
"#elif defined(_WIN64)",
"typdef INT64 INTEGER;",
"typdef UINT64 WORD_T;",
"#elif defined(_WIN32)",
"typdef INT32 INTEGER;",
"typdef UINT32 WORD_T;",
"#else",
"typdef long INTEGER;",
"typdef unsigned long WORD_T;",
"#endif",
"typedef INT64 LONGINT;",
"typedef UINT64 WORD64, LONGCARD;",
*)
"typedef char *ADDRESS;"
};

<*NOWARN*>CONST Suffix = ARRAY OF TEXT {
"#ifdef __cplusplus",
"} /* extern \"C\" */",
"#endif"
};

CONST typeNames = ARRAY CGType OF TEXT {
    "UINT8",  "INT8",
    "UINT16", "INT16",
    "UINT32", "INT32",
    "UINT64", "INT64",
    "float", "double", "double",
    "ADDRESS",
    "STRUCT",
    "void"
  };

<*NOWARN*>CONST CompareOpSymbols = ARRAY CompareOp OF TEXT { "==", "!=", ">", ">=", "<", "<=" };
CONST ConvertOpName = ARRAY ConvertOp OF TEXT { " round", " trunc", " floor", " ceiling" };
CONST CompareOpName = ARRAY CompareOp OF TEXT { " EQ", " NE", " GT", " GE", " LT", " LE" };

(*---------------------------------------------------------------------------*)

PROCEDURE pop(u: U): TEXT =
  BEGIN
    RETURN u.stack.remlo();
  END pop;

<*NOWARN*>PROCEDURE push(u: U; t: TEXT) =
  BEGIN
    u.stack.addlo(t);
  END push;

PROCEDURE get(u: U; n: CARDINAL): TEXT =
  BEGIN
    RETURN u.stack.get(n);
  END get;

PROCEDURE print(u: U; t: TEXT) = <*FATAL ANY*>
  BEGIN
    Wr.PutText(u.c, t)
  END print;

(*---------------------------------------------------------------------------*)

PROCEDURE New (cfile: Wr.T): M3CG.T =
  VAR u := NEW (U);
  BEGIN
    RTIO.PutText("M3C.New\n"); RTIO.Flush();
    u.wr := Wrx86.New (Stdio.stdout);
    u.debug := TRUE;
    u.c := cfile;
    u.init_fields := NEW(TextSeq.T).init();
    u.initializer := NEW(TextSeq.T).init();
    u.stack := NEW(TextSeq.T).init();
(*
    EVAL Type_Init(NEW(Integer_t, cg_type := Target.Integer.cg_type, typeid := UID_INTEGER));
    EVAL Type_Init(NEW(Integer_t, cg_type := Target.Word.cg_type, typeid := UID_WORD));
    EVAL Type_Init(NEW(Integer_t, cg_type := Target.Int64.cg_type, typeid := UID_LONGINT));
    EVAL Type_Init(NEW(Integer_t, cg_type := Target.Word64.cg_type, typeid := UID_LONGWORD));

    EVAL Type_Init(NEW(Float_t, cg_type := Target.Real.cg_type, typeid := UID_REEL));
    EVAL Type_Init(NEW(Float_t, cg_type := Target.Longreal.cg_type, typeid := UID_LREEL));
    EVAL Type_Init(NEW(Float_t, cg_type := Target.Extended.cg_type, typeid := UID_XREEL));

    EVAL Type_Init(NEW(Enum_t, cg_type := Target.Word8.cg_type, typeid := UID_BOOLEAN, max := 1));
    EVAL Type_Init(NEW(Enum_t, cg_type := Target.Word8.cg_type, typeid := UID_CHAR, max := 16_FF));
    EVAL Type_Init(NEW(Enum_t, cg_type := Target.Word16.cg_type, typeid := UID_WIDECHAR, max := 16_FFFF));

    EVAL Type_Init(NEW(Subrange_t, cg_type := Target.Integer.cg_type, typeid := UID_RANGE_0_31, min := 0, max := 31));
    EVAL Type_Init(NEW(Subrange_t, cg_type := Target.Integer.cg_type, typeid := UID_RANGE_0_63, min := 0, max := 31));

    EVAL Type_Init(NEW(Type_t, cg_type := Target.Address.cg_type, typeid := UID_MUTEX));
    EVAL Type_Init(NEW(Type_t, cg_type := Target.Address.cg_type, typeid := UID_TEXT));
    EVAL Type_Init(NEW(Type_t, cg_type := Target.Address.cg_type, typeid := UID_ROOT));
    EVAL Type_Init(NEW(Type_t, cg_type := Target.Address.cg_type, typeid := UID_REFANY));
    EVAL Type_Init(NEW(Type_t, cg_type := Target.Address.cg_type, typeid := UID_ADDR));
    EVAL Type_Init(NEW(Type_t, cg_type := Target.Address.cg_type, typeid := UID_PROC1));
    EVAL Type_Init(NEW(Type_t, cg_type := Target.Address.cg_type, typeid := UID_PROC2));
    EVAL Type_Init(NEW(Type_t, cg_type := Target.Address.cg_type, typeid := UID_PROC3));
    EVAL Type_Init(NEW(Type_t, cg_type := Target.Address.cg_type, typeid := UID_PROC4));
    EVAL Type_Init(NEW(Type_t, cg_type := Target.Address.cg_type, typeid := UID_PROC5));
    EVAL Type_Init(NEW(Type_t, cg_type := Target.Address.cg_type, typeid := UID_PROC6));
    EVAL Type_Init(NEW(Type_t, cg_type := Target.Address.cg_type, typeid := UID_PROC7));
    EVAL Type_Init(NEW(Type_t, cg_type := Target.Address.cg_type, typeid := UID_PROC8));
*)
    (* EVAL Type_Init(NEW(Type_t, bit_size := 0, byte_size := 0, typeid := UID_NULL)); *)
    RETURN u;
  END New;

(*----------------------------------------------------------- ID counters ---*)

PROCEDURE next_label (u: U; n: INTEGER := 1): Label =
VAR label := u.label;
  BEGIN
    INC(u.label, n);
    RETURN label;
  END next_label;

(*------------------------------------------------ READONLY configuration ---*)

PROCEDURE set_error_handler (<*NOWARN*>u: U; <*NOWARN*>p: ErrorHandler) =
  BEGIN
  END set_error_handler;

(*----------------------------------------------------- compilation units ---*)

PROCEDURE begin_unit (u: U; optimize: INTEGER) =
  (* called before any other method to initialize the compilation unit *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd ("begin_unit");
      u.wr.Int (optimize);
      u.wr.NL  ();
    END;
    print(u, "/* begin unit */\n");
    FOR i := FIRST(Prefix) TO LAST(Prefix) DO
      print(u, Prefix[i]);
      print(u, "\n");
    END;
  END begin_unit;

PROCEDURE end_unit   (u: U) =
  (* called after all other methods to finalize the unit and write the
     resulting object *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd ("end_unit");
      u.wr.NL  ();
    END;
    FOR i := FIRST(Suffix) TO LAST(Suffix) DO
      print(u, Suffix[i]);
      print(u, "\n");
    END;
  END end_unit;

PROCEDURE import_unit (u: U; name: Name) =
  (* note that the current compilation unit imports the interface 'name' *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("import_unit");
      u.wr.ZName (name);
      u.wr.NL    ();
    END
  END import_unit;

PROCEDURE export_unit (u: U; name: Name) =
  (* note that the current compilation unit exports the interface 'name' *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("export_unit");
      u.wr.ZName (name);
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

PROCEDURE declare_typename (u: U; typeid: TypeUID; name: Name) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_typename");
      u.wr.Tipe  (typeid);
      u.wr.ZName (name);
      u.wr.NL    ();
    END;
    print(u, "/* declare_typename */\n");
    (*
    print(u, "typedef M" & Fmt.Unsigned(typeid) & " " & M3ID.ToText(name) & ";\n");
    *)
  END declare_typename;

(*
PROCEDURE TypeIDToText(x: INTEGER): TEXT =
BEGIN
  RETURN "M" & Fmt.Unsigned(x);
END TypeIDToText;
*)

PROCEDURE TypeToText(type: Type): TEXT =
BEGIN
  RETURN typeNames[type];
END TypeToText;

PROCEDURE declare_array (u: U; typeid, index_typeid, element_typeid: TypeUID; total_bit_size: BitSize) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_array");
      u.wr.Tipe (typeid);
      u.wr.Tipe (index_typeid);
      u.wr.Tipe (element_typeid);
      u.wr.BInt (total_bit_size);
      u.wr.NL   ();
    END;
    print (u, "/* declare_array */\n");
(*
    WITH index_type = TypeidToType_Get(index_typeid),
         element_type =  TypeidToType_Get(element_typeid) DO
      IF index_type = NIL THEN
        RTIO.PutText("declare_array nil index_type\n");
        RTIO.Flush();
      END;
      IF element_type = NIL THEN
        RTIO.PutText("declare_array nil element_type\n");
        RTIO.Flush();
      END;
      EVAL typeidToType.put(typeid, NEW(FixedArray_t,
              typeid := typeid,
              byte_size := total_bit_size DIV 8,
              bit_size := total_bit_size,
              index_type := index_type,
              element_type := element_type));

      print(u, "typedef struct{");
      print(u, TypeIDToText(element_typeid));
      print(u, " _elts[");
      print(u, Fmt.Int(total_bit_size DIV element_type.bit_size));
      print(u, "];}");
      print(u, TypeIDToText(typeid));
      print(u, ";\n");
    END;
*)
  END declare_array;

PROCEDURE declare_open_array (u: U; typeid, element_typeid: TypeUID; bit_size: BitSize) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_open_array");
      u.wr.Tipe (typeid);
      u.wr.Tipe (element_typeid);
      u.wr.BInt (bit_size);
      u.wr.NL   ();
    END;
    print (u, "/* declare_open_array */\n");
    <* ASSERT bit_size MOD Target.Integer.size = 0 *>
(*
    WITH element_type = TypeidToType_Get(element_typeid) DO
      IF element_type = NIL THEN
        RTIO.PutText("declare_array nil element_type\n");
        RTIO.Flush();
      END;
      print(u, "typedef struct { ");
      print(u, TypeIDToText(element_typeid));
      print(u, "* _elts; CARDINAL _size");
      IF bit_size > Target.Integer.size * 2 THEN
        print(u, "s[");
        print(u, Fmt.Int((bit_size - Target.Integer.size) DIV Target.Integer.size));
        print(u, "]");
      END;
      print(u, ";}");
      print(u, TypeIDToText(element_typeid));
      print(u, ";\n");
      EVAL typeidToType.put(typeid, NEW(OpenArray_t,
              typeid := typeid,
              byte_size := bit_size DIV 8,
              bit_size := bit_size,
              element_typeid := element_typeid,
              element_type := element_type));
    END;
*)
  END declare_open_array;

PROCEDURE declare_enum (u: U; typeid: TypeUID; n_elts: INTEGER; bit_size: BitSize) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_enum");
      u.wr.Tipe (typeid);
      u.wr.Int  (n_elts);
      u.wr.BInt (bit_size);
      u.wr.NL   ();
    END;
    print (u, "/* declare_enum */\n");
    <* ASSERT bit_size = 8 OR bit_size = 16 OR bit_size = 32 *>
(*
    WITH type = NEW(Enum_t, typeid := typeid, max := n_elts - 1, cg_type := BitSizeToEnumCGType[bit_size]) DO
      <* ASSERT u.enum = NIL *>
      u.enum := type;
      EVAL Type_Init(type);
      u.enum_id := TypeIDToText(typeid);
      u.enum_value := 0;
      u.enum_type := "UINT" & Fmt.Int(bit_size);
      print(u, "typedef " & u.enum_type & " " & u.enum_id & ";\n");
    END;
*)
  END declare_enum;

PROCEDURE declare_enum_elt (u: U; name: Name) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_enum_elt");
      u.wr.ZName (name);
      u.wr.NL    ();
    END;
    print (u, "/* declare_enum_elt */\n");
(*
    print(u, "#define " & u.enum_id & "_" & M3ID.ToText(name) & " ((" & u.enum_type & ")" & Fmt.Int(u.enum_value) & ")\n");
    INC (u.enum_value);
    IF u.enum_value = u.enum.max + 1 THEN
      u.enum := NIL;
      u.enum_id := NIL;
      u.enum_type := NIL;
      u.enum_value := 10000;
    END;
*)
  END declare_enum_elt;

PROCEDURE declare_packed  (u: U; typeid: TypeUID; bit_size: BitSize; base: TypeUID) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_packed");
      u.wr.Tipe (typeid);
      u.wr.BInt (bit_size);
      u.wr.Tipe (base);
      u.wr.NL   ();
    END;
    print (u, "/* declare_packed */\n");
  END declare_packed;

PROCEDURE declare_record (u: U; typeid: TypeUID; bit_size: BitSize; n_fields: INTEGER) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_record");
      u.wr.Tipe (typeid);
      u.wr.BInt (bit_size);
      u.wr.Int  (n_fields);
      u.wr.NL   ();
    END;
    print (u, "/* declare_record */\n");
  END declare_record;

PROCEDURE declare_field (u: U; name: Name; offset: BitOffset; size: BitSize; typeid: TypeUID) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_field");
      u.wr.ZName (name);
      u.wr.BInt  (offset);
      u.wr.BInt  (size);
      u.wr.Tipe  (typeid);
      u.wr.NL    ();
    END;
    print (u, "/* declare_field */\n");
  END declare_field;

PROCEDURE declare_set (u: U; typeid, domain: TypeUID; size: BitSize) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_set");
      u.wr.Tipe (typeid);
      u.wr.Tipe (domain);
      u.wr.BInt (size);
    END;
    print (u, "/* declare_set */\n");
  END declare_set;

PROCEDURE declare_subrange (u: U; typeid, domain: TypeUID;
                            READONLY min, max: Target.Int;
                            size: BitSize) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_subrange");
      u.wr.Tipe (typeid);
      u.wr.Tipe (domain);
      u.wr.TInt (TIntN.FromTargetInt(min, NUMBER(min))); (* What about size? *)
      u.wr.TInt (TIntN.FromTargetInt(max, NUMBER(max))); (* What about size? *)
      u.wr.BInt (size);
    END;
    print (u, "/* declare_subrange */\n");
  END declare_subrange;

PROCEDURE declare_pointer (u: U; typeid, target: TypeUID; brand: TEXT; traced: BOOLEAN) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_pointer");
      u.wr.Tipe (typeid);
      u.wr.Tipe (target);
      u.wr.Txt  (brand);
      u.wr.Bool (traced);
      u.wr.NL   ();
    END;
    print (u, "/* declare_pointer */\n");
  END declare_pointer;

PROCEDURE declare_indirect (u: U; typeid, target: TypeUID) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_indirect");
      u.wr.Tipe (typeid);
      u.wr.Tipe (target);
      u.wr.NL   ();
    END;
    print(u, "/* declare_indirect */\n");
  END declare_indirect;


PROCEDURE declare_proctype (u: U; typeid: TypeUID; n_formals: INTEGER;
                            result: TypeUID; n_raises: INTEGER;
                            callingConvention: CallingConvention) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_proctype");
      u.wr.Tipe (typeid);
      u.wr.Int  (n_formals);
      u.wr.Tipe (result);
      u.wr.Int  (n_raises);
      u.wr.Txt  (callingConvention.name);
      u.wr.NL   ();
    END;
    print (u, "/* declare_proctype */\n");
  END declare_proctype;

PROCEDURE declare_formal (u: U; name: Name; typeid: TypeUID) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_formal");
      u.wr.ZName (name);
      u.wr.Tipe  (typeid);
      u.wr.NL    ();
    END;
    print(u, "/* declare_formal */\n");
  END declare_formal;

PROCEDURE declare_raises (u: U; name: Name) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_raises");
      u.wr.ZName (name);
      u.wr.NL    ();
    END;
    print (u, "/* declare_raises */\n");
  END declare_raises;

PROCEDURE declare_object (u: U; typeid, super: TypeUID;
                          brand: TEXT; traced: BOOLEAN;
                          n_fields, n_methods: INTEGER;
                          field_size: BitSize) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_object");
      u.wr.Tipe (typeid);
      u.wr.Tipe (super);
      u.wr.Txt  (brand);
      u.wr.Bool (traced);
      u.wr.Int  (n_fields);
      u.wr.Int  (n_methods);
      u.wr.BInt (field_size);
      u.wr.NL   ();
    END;
    print (u, "/* declare_object */\n");
  END declare_object;

PROCEDURE declare_method (u: U; name: Name; signature: TypeUID) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_method");
      u.wr.ZName (name);
      u.wr.Tipe  (signature);
      u.wr.NL    ();
    END;
    print (u, "/* declare_method */\n");
  END declare_method;

PROCEDURE declare_opaque (u: U; typeid, super: TypeUID) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_opaque");
      u.wr.Tipe  (typeid);
      u.wr.Tipe  (super);
      u.wr.NL    ();
    END;
    print (u, "/* declare_opaque */\n");
  END declare_opaque;

PROCEDURE reveal_opaque (u: U; lhs, rhs: TypeUID) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("reveal_opaque");
      u.wr.Tipe  (lhs);
      u.wr.Tipe  (rhs);
      u.wr.NL    ();
    END;
    print (u, "/* reveal_opaque */\n");
  END reveal_opaque;

PROCEDURE declare_exception (u: U; name: Name; arg_type: TypeUID;
                           raise_proc: BOOLEAN; base: Var; offset: INTEGER) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_exception");
      u.wr.ZName (name);
      u.wr.Tipe  (arg_type);
      u.wr.Bool  (raise_proc);
      u.wr.VName (base);
      u.wr.Int   (offset);
      u.wr.NL    ();
    END;
    print (u, "/* declare_exception */\n");
  END declare_exception;

(*--------------------------------------------------------- runtime hooks ---*)

PROCEDURE set_runtime_proc (u: U; name: Name; p: Proc) =
  VAR proc := NARROW(p, CProc);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("set_runtime_proc");
      u.wr.ZName (name);
      u.wr.PName (proc);
      u.wr.NL    ();
    END;
    print (u, "/* set_runtime_proc */\n");
  END set_runtime_proc;

(*------------------------------------------------- variable declarations ---*)

PROCEDURE import_global (u: U; name: Name; size: ByteSize; alignment: Alignment; type: Type; typeid: TypeUID): Var =
  VAR var := NEW(CVar, type := type, name := FixName(name));
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("import_global");
      u.wr.ZName (name);
      u.wr.Int   (size);
      u.wr.Int   (alignment);
      u.wr.TName (type);
      u.wr.Tipe  (typeid);
      u.wr.VName (var);
      u.wr.NL    ();
    END;
    print (u, "/* import_global */\n");
    RETURN var;
  END import_global;

PROCEDURE declare_segment (u: U; name: Name; typeid: TypeUID; is_const: BOOLEAN): Var =
VAR fixed_name := FixName(name);
    var := NEW(CVar, name := fixed_name, is_const := is_const);
    text: TEXT := NIL;
    length := 0;
BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_segment");
      u.wr.ZName (name);
      u.wr.Tipe  (typeid);
      u.wr.Bool  (is_const);
      u.wr.VName (var);
      u.wr.NL    ();
    END;
    print (u, "/* declare_segment */\n");
    IF name # 0 THEN
      text := M3ID.ToText(name);
      length := Text.Length(text);
      IF length > 2 THEN
        <* ASSERT Text.GetChar(text, 0) # '_' *>
        <* ASSERT Text.GetChar(text, 1) = '_' OR Text.GetChar(text, 2) = '_' *>
        text := Text.Sub(text, 2);
        WHILE Text.GetChar(text, 0) = '_' DO
          text := Text.Sub(text, 1);
        END;
        u.unit_name := text;
      END;
    END;
    text := M3ID.ToText(fixed_name);
    print(u, "struct " & text & "_t;\n");
    IF is_const THEN
      print(u, "const ");
    END;
    print(u, "static struct " & text & "_t " & text & ";\n");
    RETURN var;
  END declare_segment;

PROCEDURE bind_segment (u: U; v: Var; size: ByteSize; alignment: Alignment;
                        type: Type; exported, inited: BOOLEAN) =
  VAR var := NARROW(v, CVar);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("bind_segment");
      u.wr.VName (var);
      u.wr.Int   (size);
      u.wr.Int   (alignment);
      u.wr.TName (type);
      u.wr.Bool  (exported);
      u.wr.Bool  (inited);
      u.wr.NL    ();
    END;
    print (u, "/* bind_segment */\n");
  END bind_segment;

PROCEDURE declare_global (u: U; name: Name; size: ByteSize; alignment: Alignment;
                     type: Type; typeid: TypeUID; exported, inited: BOOLEAN): Var =
  BEGIN
    print (u, "/* declare_global */\n");
    RETURN DeclareGlobal(u, name, size, alignment, type, typeid, exported, inited, FALSE);
  END declare_global;

PROCEDURE declare_constant (u: U; name: Name; size: ByteSize; alignment: Alignment;
                     type: Type; typeid: TypeUID; exported, inited: BOOLEAN): Var =
  BEGIN
    print (u, "/* declare_constant */\n");
    RETURN DeclareGlobal(u, name, size, alignment, type, typeid, exported, inited, TRUE);
  END declare_constant;

PROCEDURE DeclareGlobal (u: U; name: Name; size: ByteSize; alignment: Alignment;
                         type: Type; typeid: TypeUID;
                         exported, inited, is_const: BOOLEAN): Var =
  CONST DeclTag = ARRAY BOOLEAN OF TEXT { "declare_global", "declare_constant" };
  VAR var := NEW(CVar, name := FixName(name));
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   (DeclTag [is_const]);
      u.wr.ZName (name);
      u.wr.Int   (size);
      u.wr.Int   (alignment);
      u.wr.TName (type);
      u.wr.Tipe  (typeid);
      u.wr.Bool  (exported);
      u.wr.Bool  (inited);
      u.wr.VName (var);
      u.wr.NL    ();
    END;
    RETURN var;
  END DeclareGlobal;

PROCEDURE declare_local (u: U; name: Name; size: ByteSize; alignment: Alignment;
                         type: Type; typeid: TypeUID; in_memory, up_level: BOOLEAN;
                         frequency: Frequency): Var =
VAR var := NEW(CVar, type := type, name := FixName(name));
    text: TEXT;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_local");
      u.wr.ZName (name);
      u.wr.Int   (size);
      u.wr.Int   (alignment);
      u.wr.TName (type);
      u.wr.Tipe  (typeid);
      u.wr.Bool  (in_memory);
      u.wr.Bool  (up_level);
      u.wr.Int   (frequency);
      u.wr.VName (var);
      (*u.wr.Int   (var.offset);*)
      u.wr.NL    ();
    END;
    print (u, "/* declare_local */\n");
    IF type = Type.Struct THEN
      text := "struct { char a[" & Fmt.Int(size) & "];}";
    ELSE
      text := TypeToText(type);
    END;
    text := text & " " & M3ID.ToText(var.name) & ";\n";
    IF u.in_procedure > 0 OR u.in_block > 0 THEN
      print(u, text);
    ELSE
      u.function.locals.addhi(text);
    END;
    RETURN var;
  END declare_local;

PROCEDURE function_prototype(u: U; proc: CProc) =
VAR params := proc.params;
BEGIN
  print(u, TypeToText(proc.return_type));
  print(u, "\n");
  print(u, "__cdecl\n");
  print(u, M3ID.ToText(proc.name));
  IF NUMBER (params^) = 0 THEN
    print(u, "(void)");
  ELSE
    print(u, "(");
    FOR i := FIRST(params^) TO LAST(params^) DO
      WITH param = params[i] DO
        print(u, "\n");
        print(u, TypeToText(param.type));
        print(u, " ");
        print(u, M3ID.ToText(param.name));
        IF i # LAST(params^) THEN
          print(u, ",");
        ELSE
          print(u, ")");
        END;
      END;
    END;
  END;
END function_prototype;

PROCEDURE declare_param (u: U; name: Name; size: ByteSize; alignment: Alignment;
                         type: Type; typeid: TypeUID; in_memory, up_level: BOOLEAN;
                         frequency: Frequency): Var =
VAR var := NEW(CVar, type := type, name := FixName(name), type := type);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_param");
      u.wr.ZName (name);
      u.wr.Int   (size);
      u.wr.Int   (alignment);
      u.wr.TName (type);
      u.wr.Tipe  (typeid);
      u.wr.Bool  (in_memory);
      u.wr.Bool  (up_level);
      u.wr.Int   (frequency);
      u.wr.VName (var);
      (*u.wr.Int   (var.offset);*)
      u.wr.NL    ();
    END;
    print (u, "/* declare_param */\n");
    u.function.params[u.param_count] := var;
    INC(u.param_count);
    IF u.param_count = NUMBER(u.function.params^) THEN
      function_prototype(u, u.function);
      print(u, ";\n");
      u.param_count := -1000; (* catch bugs *)
    END;
    RETURN var;
  END declare_param;

PROCEDURE declare_temp (u: U; size: ByteSize; alignment: Alignment; type: Type; in_memory:BOOLEAN): Var =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_temp");
      u.wr.Int   (size);
      u.wr.Int   (alignment);
      u.wr.TName (type);
      u.wr.Bool  (in_memory);
      (*u.wr.VName (var);*)
      (*u.wr.Int   (var.offset);*)
      u.wr.NL    ();
    END;
    print (u, "/* declare_temp => declare_local */\n");
    RETURN declare_local(u, 0, size, alignment, type, -1, in_memory, FALSE, M3CG.Always);
  END declare_temp;

PROCEDURE free_temp (u: U; v: Var) =
  VAR var := NARROW(v, CVar);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("free_temp");
      u.wr.VName (var);
      u.wr.NL    ();
    END;
    print (u, "/* free_temp */\n");
  END free_temp;

(*---------------------------------------- static variable initialization ---*)

PROCEDURE begin_init (u: U; v: Var) =
  VAR var := NARROW(v, CVar);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("begin_init");
      u.wr.VName (var);
      u.wr.NL    ();
    END;
    print (u, "/* begin_init */\n");
    u.current_init_offset := 0;
  END begin_init;

PROCEDURE end_init (u: U; v: Var) =
  VAR var := NARROW(v, CVar);
      init_fields := u.init_fields;
      initializer := u.initializer;
      var_name := M3ID.ToText(var.name);
      end_name := M3ID.ToText(FixName(0));
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("end_init");
      u.wr.VName (var);
      u.wr.NL    ();
    END;
    print (u, "/* end_init */\n");
    IF var.is_const THEN
      print(u, "const ");
    END;
    print(u, "static struct " & var_name & "_t {\n");
    WHILE init_fields.size() > 0 DO
      print (u, init_fields.remlo());
    END;
    (* one more field due to trailing comma in initializer *)
    print(u, "char " & end_name & ";\n} " & var_name & " = {\n");
    WHILE initializer.size() > 0 DO
      print (u, initializer.remlo());
    END;
    print (u, "0\n};");
  END end_init;

PROCEDURE init_to_offset (u: U; offset: ByteOffset) =
  VAR pad := offset - u.current_init_offset;
      init_fields := u.init_fields;
      initializer := u.initializer;
      pad_name: TEXT;
  BEGIN
    <* ASSERT offset >= u.current_init_offset *>
    IF pad > 0 THEN
      init_fields.addhi ("char ");
      pad_name := M3ID.ToText(FixName(0));
      init_fields.addhi (pad_name);
      init_fields.addhi ("[");
      init_fields.addhi (Fmt.Int(pad));
      init_fields.addhi ("];\n");
      FOR i := 1 TO pad DO
        initializer.addhi ("0,");
        IF (i = pad) OR (i MOD 25 = 0) THEN
          initializer.addhi ("\n");
        END;
      END;
    END;
  END init_to_offset;

PROCEDURE init_int (u: U; offset: ByteOffset; READONLY value: Target.Int; type: Type) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("init_int");
      u.wr.Int   (offset);
      u.wr.TInt  (TIntN.FromTargetInt(value, CG_Bytes[type]));
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print (u, "/* init_int */\n");
    init_to_offset (u, offset);
    u.init_fields.addhi (TypeToText(type));
    u.init_fields.addhi (" ");
    u.init_fields.addhi (M3ID.ToText(FixName(0)));
    u.init_fields.addhi (";\n");
    u.initializer.addhi (TInt.ToText(value));
    u.initializer.addhi (",\n");
    u.current_init_offset := offset + TargetMap.CG_Bytes[type];
  END init_int;

PROCEDURE init_proc (u: U; offset: ByteOffset; p: Proc) =
  VAR proc := NARROW(p, CProc);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("init_proc");
      u.wr.Int   (offset);
      u.wr.PName (proc);
      u.wr.NL    ();
    END;
    print (u, "/* init_proc */\n");
    init_to_offset (u, offset);
    u.init_fields.addhi ("ADDRESS "); (* FUTURE: better typing *)
    u.init_fields.addhi (M3ID.ToText(FixName(0)));
    u.init_fields.addhi (";\n");
    u.initializer.addhi ("(ADDRESS)&");
    u.initializer.addhi (M3ID.ToText(proc.name));
    u.initializer.addhi (",\n");
    u.current_init_offset := offset + TargetMap.CG_Bytes[Type.Addr];
  END init_proc;

PROCEDURE init_label (u: U; offset: ByteOffset; value: Label) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("init_label");
      u.wr.Int   (offset);
      u.wr.Lab   (value);
      u.wr.NL    ();
    END;
    print (u, "/* init_label */\n");
    <* ASSERT FALSE *>
  END init_label;

PROCEDURE init_var (u: U; offset: ByteOffset; v: Var; bias: ByteOffset) =
  VAR var := NARROW(v, CVar);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("init_var");
      u.wr.Int   (offset);
      u.wr.VName (var);
      u.wr.Int   (bias);
      u.wr.NL    ();
    END;
    print (u, "/* init_var */\n");
    init_to_offset (u, offset);
    u.init_fields.addhi ("ADDRESS "); (* FUTURE: better typing *)
    u.init_fields.addhi (M3ID.ToText(FixName(0)));
    u.init_fields.addhi (";\n");
    IF bias # 0 THEN
      u.initializer.addhi (Fmt.Int(bias));
      u.initializer.addhi (" + ");
    END;
    u.initializer.addhi ("(ADDRESS)&");
    u.initializer.addhi (M3ID.ToText(var.name));
    u.initializer.addhi (",\n");
    u.current_init_offset := offset + TargetMap.CG_Bytes[Type.Addr];
  END init_var;

PROCEDURE init_offset (u: U; offset: ByteOffset; value: Var) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("init_offset");
      u.wr.Int   (offset);
      u.wr.VName (value);
      u.wr.NL    ();
    END;
    print (u, "/* init_offset */\n");
    <* ASSERT FALSE *>
  END init_offset;

PROCEDURE init_chars (u: U; offset: ByteOffset; value: TEXT) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("init_chars");
      u.wr.Int   (offset);
      u.wr.Txt   (value);
      u.wr.NL    ();
    END;
    print (u, "/* init_chars */\n");
  END init_chars;

PROCEDURE init_float (u: U; offset: ByteOffset; READONLY float: Target.Float) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("init_float");
      u.wr.Int   (offset);
      u.wr.Flt   (float);
      u.wr.NL    ();
    END;
    print (u, "/* init_float */\n");
  END init_float;

(*------------------------------------------------------------ PROCEDUREs ---*)

PROCEDURE import_procedure (u: U; name: Name; n_params: INTEGER;
                            return_type: Type; callingConvention: CallingConvention): Proc =
VAR proc := NEW(CProc, name := FixName(name), n_params := n_params,
                return_type := return_type,
                callingConvention := callingConvention,
                locals := NEW(TextSeq.T).init(),
                params := NEW(REF ARRAY OF CVar, n_params));
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("import_procedure");
      u.wr.ZName (name);
      u.wr.Int   (n_params);
      u.wr.TName (return_type);
      u.wr.Txt   (callingConvention.name);
      u.wr.PName (proc);
      u.wr.NL    ();
    END;
    print (u, "/* import_procedure */\n");
    u.param_count := 0;
    u.function := proc;
    IF n_params = 0 THEN
      function_prototype(u, proc);
      print(u, ";\n");
    END;
    RETURN proc;
  END import_procedure;

PROCEDURE declare_procedure (u: U; name: Name; n_params: INTEGER;
                             return_type: Type; level: INTEGER;
                             callingConvention: CallingConvention;
                             exported: BOOLEAN; parent: Proc): Proc =
VAR proc := NEW(CProc, name := FixName(name), n_params := n_params,
                return_type := return_type, level := level,
                callingConvention := callingConvention, exported := exported,
                parent := parent,
                locals := NEW(TextSeq.T).init(),
                params := NEW(REF ARRAY OF CVar, n_params));
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_procedure");
      u.wr.ZName (name);
      u.wr.Int   (n_params);
      u.wr.TName (return_type);
      u.wr.Int   (level);
      u.wr.Txt   (callingConvention.name);
      u.wr.Bool  (exported);
      u.wr.PName (parent);
      u.wr.PName (proc);
      u.wr.NL    ();
    END;
    print (u, "/* declare_procedure */\n");
    u.param_count := 0;
    u.function := proc;
    RETURN proc;
  END declare_procedure;

PROCEDURE begin_procedure (u: U; p: Proc) =
  VAR proc := NARROW(p, CProc);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("begin_procedure");
      u.wr.PName (proc);
      u.wr.NL    ();
    END;
    print (u, "/* begin_procedure */\n");
    INC(u.in_procedure);
    u.function := proc;
    function_prototype(u, proc);
    print(u, "\n{\n");
    WHILE proc.locals.size() > 0 DO
      print(u, proc.locals.remlo());
    END;
  END begin_procedure;

PROCEDURE end_procedure (u: U; p: Proc) =
  VAR proc := NARROW(p, CProc);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("end_procedure");
      u.wr.PName (proc);
      u.wr.NL    ();
    END;
    print (u, "/* end_procedure */\n");
    DEC(u.in_procedure);
    print(u, "\n}\n");
  END end_procedure;

PROCEDURE begin_block (u: U) =
  (* marks the beginning of a nested anonymous block *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("begin_block");
      u.wr.NL    ();
    END;
    print (u, "/* begin_block */\n");
    INC(u.in_block);
    print (u, "{\n");
  END begin_block;

PROCEDURE end_block (u: U) =
  (* marks the ending of a nested anonymous block *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("end_block");
      u.wr.NL    ();
    END;
    print (u, "/* end_block */\n");
    DEC(u.in_block);
    print (u, "}\n");
  END end_block;

PROCEDURE note_procedure_origin (u: U; p: Proc) =
  VAR proc := NARROW(p, CProc);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("note_procedure_origin");
      u.wr.PName (proc);
      u.wr.NL    ();
    END;
    print (u, "/* note_procedure_origin */\n");
  END note_procedure_origin;

(*------------------------------------------------------------ statements ---*)

PROCEDURE set_label (u: U; label: Label; <*UNUSED*> barrier: BOOLEAN) =
  (* define 'label' to be at the current pc *)
  BEGIN
    print (u, "/* set_label */\n");
    print(u, "L" & Fmt.Unsigned(label) & ":;\n");
  END set_label;

PROCEDURE jump (u: U; label: Label) =
  (* GOTO label *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("jump");
      u.wr.Lab   (label);
      u.wr.NL    ();
    END;
    print (u, "/* jump */\n");
    print(u, "goto L" & Fmt.Unsigned(label) & ";\n");
  END jump;

PROCEDURE if_true  (u: U; itype: IType; label: Label; <*UNUSED*> frequency: Frequency) =
  (* IF (s0.itype # 0) GOTO label ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("if_true");
      u.wr.TName (itype);
      u.wr.Lab   (label);
      u.wr.NL    ();
    END;
    print (u, "/* if_true */\n");
    print(u, "if (" & pop(u) & ") goto L" & Fmt.Unsigned(label) & ";\n");
  END if_true;

PROCEDURE if_false (u: U; itype: IType; label: Label; <*UNUSED*> frequency: Frequency) =
  (* IF (s0.itype = 0) GOTO label ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("if_false");
      u.wr.TName (itype);
      u.wr.Lab   (label);
      u.wr.NL    ();
    END;
    print (u, "/* if_false */\n");
    print(u, "if (!(" & pop(u) & ")) goto L" & Fmt.Unsigned(label) & ";\n");
  END if_false;

PROCEDURE if_compare (u: U; ztype: ZType; op: CompareOp; label: Label;
                      <*UNUSED*> frequency: Frequency) =
  (* IF (s1.ztype op s0.ztype) GOTO label ; pop(2) *)
  VAR s0 := pop(u);
      s1 := pop(u);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("if_compare");
      u.wr.TName (ztype);
      u.wr.OutT  (CompareOpName [op]);
      u.wr.Lab   (label);
      u.wr.NL    ();
    END;
    print(u, "/* if_compare */\n");
    print(u, "if ((" & s1 & ")" & "op" & "(" & s0 & ")) goto L" & Fmt.Unsigned(label) & ";\n");
  END if_compare;

PROCEDURE case_jump (u: U; itype: IType; READONLY labels: ARRAY OF Label) =
  (* "GOTO labels[s0.itype] ; pop" with no range checking on s0.itype *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("case_jump");
      u.wr.TName (itype);
      u.wr.Int   (NUMBER(labels));
      FOR i := FIRST (labels) TO LAST (labels) DO
        u.wr.Lab (labels [i]);
      END;
      u.wr.NL    ();
    END;
    print(u, "/* case_jump */\n");
  END case_jump;

PROCEDURE exit_proc (u: U; type: Type) =
  (* Returns s0.type if type is not Void, otherwise returns no value. *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("exit_proc");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    IF type = Type.Void THEN
      print(u, "return;\n");
    ELSE
      print(u, "return " & pop (u) & ";\n");
    END;
  END exit_proc;

(*------------------------------------------------------------ load/store ---*)

PROCEDURE load  (u: U; v: Var; offset: ByteOffset; mtype: MType; ztype: ZType) =
(* push; s0.ztype := Mem [ ADR(var) + offset ].mtype ; The only allowed (mtype->ztype) conversions
   are {Int,Word}{8,16} -> {Int,Word}{32,64} and {Int,Word}32 -> {Int,Word}64.
   The source type, mtype, determines whether the value is sign-extended or
   zero-extended. *)
  VAR var := NARROW(v, CVar);
      text := "";
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load");
      u.wr.VName (var);
      u.wr.Int   (offset);
      u.wr.TName (mtype);
      u.wr.TName (ztype);
      u.wr.NL    ();
    END;
    print(u, "/* load */\n");
    <* ASSERT CG_Bytes[ztype] >= CG_Bytes[mtype] *>
    IF ztype # mtype THEN
      text := "(" & TypeToText(ztype) & ")";
    END;
    push (u, "(" & text & "*(" & TypeToText(mtype) & "*)(" & Fmt.Int(offset) & "+(ADDRESS)&" & M3ID.ToText (var.name) & "))");
  END load;

PROCEDURE store (u: U; v: Var; offset: ByteOffset; ztype: ZType; mtype: MType) =
(* Mem [ ADR(var) + offset ].mtype := s0.ztype; pop *)
  VAR var := NARROW(v, CVar);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("store");
      u.wr.VName (var);
      u.wr.Int   (offset);
      u.wr.TName (ztype);
      u.wr.TName (mtype);
      u.wr.NL    ();
    END;
    print(u, "/* store */\n");
    <* ASSERT CG_Bytes[ztype] >= CG_Bytes[mtype] *>
    WITH s0 = pop(u) DO
      print(u, "(*(" & TypeToText(mtype) & "*)(" & Fmt.Int(offset) & "+(ADDRESS)&" & M3ID.ToText(var.name) & "))=(" & TypeToText(ztype) & ")(" & s0 & ");\n");
    END;
  END store;

PROCEDURE load_address (u: U; v: Var; offset: ByteOffset) =
(* push; s0.A := ADR(var) + offset *)
  VAR var := NARROW(v, CVar);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_address");
      u.wr.VName (var);
      u.wr.Int   (offset);
      u.wr.NL    ();
    END;
    print(u, "/* load_address */\n");
    push (u, "(" & Fmt.Int(offset) & "+(ADDRESS)&" & M3ID.ToText (var.name) & ")");
  END load_address;

PROCEDURE load_indirect (u: U; offset: ByteOffset; mtype: MType; ztype: ZType) =
(* s0.ztype := Mem [s0.A + offset].mtype  *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_indirect");
      u.wr.Int   (offset);
      u.wr.TName (mtype);
      u.wr.TName (ztype);
      u.wr.NL    ();
    END;
    print(u, "/* load_indirect */\n");
    <* ASSERT CG_Bytes[ztype] >= CG_Bytes[mtype] *>
    WITH s0 = pop(u) DO
      push (u, "((" & TypeToText(ztype) & ")(*(" & TypeToText(mtype) & "*)(" & Fmt.Int(offset) & "+(ADDRESS)" & s0 & ")))");
    END;
  END load_indirect;

PROCEDURE store_indirect (u: U; offset: ByteOffset; ztype: ZType; mtype: MType) =
(* Mem [s1.A + offset].mtype := s0.ztype; pop (2) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("store_indirect");
      u.wr.Int   (offset);
      u.wr.TName (ztype);
      u.wr.TName (mtype);
      u.wr.NL    ();
    END;
    print(u, "/* store_indirect */\n");
    <* ASSERT CG_Bytes[ztype] >= CG_Bytes[mtype] *>
    WITH s0 = pop(u),
         s1 = pop(u) DO
      print(u, "(*(" & TypeToText(mtype) & "*)(" & Fmt.Int(offset) & "+(ADDRESS)" & s1 & "))=(" & TypeToText(ztype) & ")(" & s0 & ");\n");
    END;
  END store_indirect;

(*-------------------------------------------------------------- literals ---*)

PROCEDURE load_nil (u: U) =
  (* push ; s0.A := a *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_nil");
      u.wr.NL    ();
    END;
    print(u, "/* load_nil */\n");
    push(u, "0"); (* UNDONE NULL or (ADDRESS)0? *)
  END load_nil;

PROCEDURE load_integer  (u: U; type: IType; READONLY i: Target.Int) =
  (* push ; s0.type := i *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_integer");
      u.wr.TName (type);
      u.wr.TInt  (TIntN.FromTargetInt(i, CG_Bytes[type])); (* UNDONE? *)
      u.wr.NL    ();
    END;
    print(u, "/* load_integer */\n");
    (* TODO: use suffixes L, U, UL, ULL, i64, ui64 via #ifdef and macro *)
    push(u, "((" & TypeToText(type) & ")" & TInt.ToText(i) & ")");
  END load_integer;

PROCEDURE load_float    (u: U; type: RType; READONLY float: Target.Float) =
  (* push ; s0.type := float *)
  VAR buffer: ARRAY [0..255] OF CHAR;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_float");
      u.wr.TName (type);
      u.wr.Flt   (float);
      u.wr.NL    ();
    END;
    print(u, "/* load_float */\n");
    (* TODO: use suffixes *)
    push(u, "((" & TypeToText(type) & ")" & Text.FromChars(SUBARRAY(buffer, 0, TFloat.ToChars(float, buffer))) & ")");
  END load_float;

(*------------------------------------------------------------ arithmetic ---*)

PROCEDURE compare (u: U; ztype: ZType; itype: IType; op: CompareOp) =
  (* s1.itype := (s1.ztype op s0.ztype)  ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("compare");
      u.wr.TName (ztype);
      u.wr.TName (itype);
      u.wr.OutT  (CompareOpName [op]);
      u.wr.NL    ();
    END;
    print(u, "/* compare */\n");
    (* ASSERT cond # Cond.Z AND cond # Cond.NZ *)
  END compare;

PROCEDURE add (u: U; type: AType) =
  (* s1.type := s1.type + s0.type ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("add");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "/* add */\n");
  END add;

PROCEDURE subtract (u: U; type: AType) =
  (* s1.type := s1.type - s0.type ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("subtract");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "/* subtract */\n");
  END subtract;

PROCEDURE multiply (u: U; type: AType) =
  (* s1.type := s1.type * s0.type ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("multiply");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "/* multiply */\n");
    (*
      stack[1] := m3_div & "type(" ((type)stack[1]) & "/" & (type)stack[0] & ")"
      pop();
    *)
  END multiply;

PROCEDURE divide (u: U; type: RType) =
  (* s1.type := s1.type / s0.type ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("divide");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "/* divide */\n");
    (*
      stack[1] := ((type)stack[1]) & "/" & (type)stack[0]
      pop();
    *)
  END divide;

CONST SignName = ARRAY Sign OF TEXT { " P", " N", " X" };

PROCEDURE div (u: U; type: IType; a, b: Sign) =
  (* s1.type := s1.type DIV s0.type ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("div");
      u.wr.TName (type);
      u.wr.OutT  (SignName [a]);
      u.wr.OutT  (SignName [b]);
      u.wr.NL    ();
    END;
    print(u, "/* div */\n");
    (*
      stack[1] := m3_div & "type(" ((type)stack[1]) & "/" & (type)stack[0] & ")"
      pop();
    *)
  END div;

PROCEDURE mod (u: U; type: IType; a, b: Sign) =
  (* s1.type := s1.type MOD s0.type ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("mod");
      u.wr.TName (type);
      u.wr.OutT  (SignName [a]);
      u.wr.OutT  (SignName [b]);
      u.wr.NL    ();
    END;
    print(u, "/* mod */\n");
    (*
      stack[1] := m3_div & "type(" ((type)stack[1]) % "/" & (type)stack[0] & ")"
      pop();
    *)
  END mod;

PROCEDURE negate (u: U; type: AType) =
  (* s0.type := - s0.type *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("negate");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "/* negate */\n");
    (*
      stack[0] := - "(type)" & stack[0]
    *)
  END negate;

PROCEDURE abs (u: U; type: AType) =
  (* s0.type := ABS (s0.type) (noop on Words) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("abs");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "/* abs */\n");
    (*
      stack[0] := "m3_abs&type(" & stack[0] & ")"
    *)
  END abs;

PROCEDURE max (u: U; type: ZType) =
  (* s1.type := MAX (s1.type, s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("max");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "/* max */\n");
    (*
      stack[1] := "m3_max&type(" & stack[0] & "," stack[1] & ")"
      pop();
    *)
  END max;

PROCEDURE min (u: U; type: ZType) =
  (* s1.type := MIN (s1.type, s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("min");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "/* min */\n");
    (*
      stack[1] := "m3_min&type(" & stack[0] & "," stack[1] & ")"
      pop();
    *)
  END min;

PROCEDURE cvt_int (u: U; rtype: RType; itype: IType; op: ConvertOp) =
  (* s0.x := ROUND (s0.type) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("cvt_int");
      u.wr.TName (rtype);
      u.wr.TName (itype);
      u.wr.OutT  (ConvertOpName [op]);
      u.wr.NL    ();
    END;
    print(u, "/* cvt_int */\n");
    (*
      stack[0] := "((long)(" & stack[0] & ")"
    *)
  END cvt_int;

PROCEDURE cvt_float (u: U; atype: AType; rtype: RType) =
  (* s0.x := FLOAT (s0.type, x) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("cvt_float");
      u.wr.TName (atype);
      u.wr.TName (rtype);
      u.wr.NL    ();
    END;
    print(u, "/* cvt_float */\n");
    (*
      stack[0] := "((double)(" & stack[0] & ")"
    *)
  END cvt_float;

(*------------------------------------------------------------------ sets ---*)

PROCEDURE set_op3(u: U; size: ByteSize; op: TEXT) =
  (* s2.B := s1.B op s0.B ; pop(3) *)
  BEGIN
    IF u.debug THEN
      (*u.wr.Cmd   (BuiltinDesc[builtin].name);*)
      u.wr.Int   (size);
      u.wr.NL    ();
    END;
    print(u, "/* " & op & " */\n");
    (*
      stack[2] := "m3_" & op & "(" & stack[0] & stack[1] & stack[2] & ")"
      pop();
      pop();
    *)
  END set_op3;

PROCEDURE set_union (u: U; size: ByteSize) =
  (* s2.B := s1.B + s0.B ; pop(2) *)
  BEGIN
    set_op3(u, size, "set_union");
  END set_union;

PROCEDURE set_difference (u: U; size: ByteSize) =
  (* s2.B := s1.B - s0.B ; pop(2) *)
  BEGIN
    set_op3(u, size, "set_difference");
  END set_difference;

PROCEDURE set_intersection (u: U; size: ByteSize) =
  (* s2.B := s1.B * s0.B ; pop(2) *)
  BEGIN
    set_op3(u, size, "set_intersection");
  END set_intersection;

PROCEDURE set_sym_difference (u: U; size: ByteSize) =
  (* s2.B := s1.B / s0.B ; pop(2) *)
  BEGIN
    set_op3(u, size, "set_sym_difference");
  END set_sym_difference;

PROCEDURE set_member (u: U; size: ByteSize; type: IType) =
  (* s1.type := (s0.type IN s1.B) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("set_member");
      u.wr.Int   (size);
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "/* set_member */\n");
  END set_member;

PROCEDURE set_compare (u: U; size: ByteSize; op: CompareOp; type: IType) =
  (* s1.type := (s1.B op s0.B) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("set_compare");
      u.wr.Int   (size);
      u.wr.OutT  (CompareOpName [op]);
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "/* set_compare */\n");
  END set_compare;

PROCEDURE set_range (u: U; size: ByteSize; type: IType) =
  (* s2.A [s1.type .. s0.type] := 1's; pop(3) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("set_range");
      u.wr.Int   (size);
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "/* set_range */\n");
  END set_range;

PROCEDURE set_singleton (u: U; size: ByteSize; type: IType) =
  (* s1.A [s0.type] := 1; pop(2) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("set_singleton");
      u.wr.Int   (size);
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "/* set_singleton */\n");
  END set_singleton;

(*------------------------------------------------- Word.T bit operations ---*)

PROCEDURE not (u: U; type: IType) =
  (* s0.type := Word.Not (s0.type) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("not");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "/* not */\n");
    (*
    stack[0] := "((type)~(type)" & stack[0] & ")";
    *)
  END not;

PROCEDURE and (u: U; type: IType) =
  (* s1.type := Word.And (s1.type, s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("and");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "/* and */\n");
    (*
    stack[1] := "(((type)" & stack[0] & ") & (type)(stack[1] & "))";
    pop();
    *)
  END and;

PROCEDURE or  (u: U; type: IType) =
  (* s1.type := Word.Or  (s1.type, s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("or");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "/* or */\n");
    (*
    stack[1] := "(((type)" & stack[0] & ") | (type)(stack[1] & "))";
    pop();
    *)
  END or;

PROCEDURE xor (u: U; type: IType) =
  (* s1.type := Word.Xor (s1.type, s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("xor");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "/* xor */\n");
    (*
    stack[1] := "(((type)" & stack[0] & ") ^ (type)(stack[1] & "))";
    pop();
    *)
  END xor;

PROCEDURE shift_left   (u: U; type: IType) =
  (* s1.type := Word.Shift  (s1.type, s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("shift_left");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "/* shift_left */\n");
    (*
    stack[1] := "(((type)" & stack[1] & ") << (type)(stack[0] & "))";
    pop();
    *)
  END shift_left;

PROCEDURE shift_right  (u: U; type: IType) =
  (* s1.type := Word.Shift  (s1.type, -s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("shift_right");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "/* shift_right */\n");
    (*
    stack[1] := "((type)(((unsigned type)" & stack[1] & ") >> (type)(stack[0] & ")))";
    pop();
    *)
  END shift_right;

PROCEDURE shift (u: U; type: IType) =
  (* s1.type := Word.Shift  (s1.type, s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("shift");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "/* shift */\n");
  END shift;

PROCEDURE rotate (u: U; type: IType) =
  (* s1.type := Word.Rotate (s1.type, s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("rotate");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "/* rotate */\n");
  END rotate;

PROCEDURE rotate_left  (u: U; type: IType) =
  (* s1.type := Word.Rotate (s1.type, s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("rotate_left");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "/* rotate_left */\n");
  END rotate_left;

PROCEDURE rotate_right (u: U; type: IType) =
  (* s1.type := Word.Rotate (s1.type, -s0.type) ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("rotate_right");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "/* rotate_right */\n");
  END rotate_right;

PROCEDURE widen (u: U; sign_extend: BOOLEAN) =
  (* s0.I64 := s0.I32; IF sign_extend THEN SignExtend s0; *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("widen");
      u.wr.Bool  (sign_extend);
      u.wr.NL    ();
    END;
    <*ASSERT FALSE*>
  END widen;

PROCEDURE chop (u: U) =
  (* s0.I32 := Word.And (s0.I64, 16_ffffffff); *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("chop");
      u.wr.NL    ();
    END;
    <*ASSERT FALSE*>
  END chop;

PROCEDURE extract (u: U; type: IType; sign_extend: BOOLEAN) =
  (* s2.type := Word.Extract(s2.type, s1.type, s0.type);
     IF sign_extend THEN SignExtend s2 END; pop(2) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("extract");
      u.wr.TName (type);
      u.wr.Bool  (sign_extend);
      u.wr.NL    ();
    END;
    print(u, "/* extract */\n");
  END extract;

PROCEDURE extract_n (u: U; type: IType; sign_extend: BOOLEAN; n: CARDINAL) =
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
    print(u, "/* extract_m */\n");
  END extract_n;

PROCEDURE extract_mn (u: U; type: IType; sign_extend: BOOLEAN; m, n: CARDINAL) =
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
    print(u, "/* extract_mn */\n");
  END extract_mn;

PROCEDURE insert  (u: U; type: IType) =
  (* s3.type := Word.Insert (s3.type, s2.type, s1.type, s0.type) ; pop(3) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("insert");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "/* insert */\n");
  END insert;

PROCEDURE insert_n  (u: U; type: IType; n: CARDINAL) =
  (* s2.type := Word.Insert (s2.type, s1.type, s0.type, n) ; pop(2) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("insert_n");
      u.wr.TName (type);
      u.wr.Int   (n);
      u.wr.NL    ();
    END;
    print(u, "/* insert_n */\n");
  END insert_n;

PROCEDURE insert_mn  (u: U; type: IType; m, n: CARDINAL) =
  (* s1.type := Word.Insert (s1.type, s0.type, m, n) ; pop(2) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("insert_mn");
      u.wr.TName (type);
      u.wr.Int   (m);
      u.wr.Int   (n);
      u.wr.NL    ();
    END;
    print(u, "/* insert_mn */\n");
  END insert_mn;

(*------------------------------------------------ misc. stack/memory ops ---*)

PROCEDURE swap (u: U; a, b: Type) =
  (* tmp := s1 ; s1 := s0 ; s0 := tmp *)
  VAR temp := get(u, 1);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("swap");
      u.wr.TName (a);
      u.wr.TName (b);
      u.wr.NL    ();
    END;
    u.stack.put(1, get(u, 0));
    u.stack.put(0, temp);
  END swap;

PROCEDURE cg_pop (u: U; type: Type) =
  (* pop(1) (i.e. discard s0) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("pop");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "/* pop */\n");
    EVAL pop(u);
  END cg_pop;

PROCEDURE copy_n (u: U; itype: IType; mtype: MType; overlap: BOOLEAN) =
  (* Mem[s2.A:s0.ztype] := Mem[s1.A:s0.ztype]; pop(3)*)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("copy_n");
      u.wr.TName (itype);
      u.wr.TName (mtype);
      u.wr.Bool  (overlap);
      u.wr.NL    ();
    END;
    print(u, "/* copy_n */\n");
  END copy_n;

PROCEDURE copy (u: U; n: INTEGER; type: MType; overlap: BOOLEAN) =
  (* Mem[s1.A:sz] := Mem[s0.A:sz]; pop(2)*)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("copy");
      u.wr.Int   (n);
      u.wr.TName (type);
      u.wr.Bool  (overlap);
      u.wr.NL    ();
    END;
    print(u, "/* copy */\n");
  END copy;

PROCEDURE zero_n (u: U; itype: IType; mtype: MType) =
  (* Mem[s1.A:s0.type_multiple_of_32] := 0; pop(2) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("zero_n");
      u.wr.TName (itype);
      u.wr.TName (mtype);
      u.wr.NL    ();
    END;

    <* ASSERT FALSE *>

    (* zero_n is implemented incorrectly in the gcc backend,
     * therefore it must not be used.
    *)
  END zero_n;

PROCEDURE zero (u: U; n: INTEGER; type: MType) =
  (* Mem[s0.A:sz] := 0; pop(1) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("zero");
      u.wr.Int   (n);
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "/* zero */\n");
  END zero;

(*----------------------------------------------------------- conversions ---*)

PROCEDURE loophole (u: U; from, to: ZType) =
  (* s0.to := LOOPHOLE(s0.from, to) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("loophole");
      u.wr.TName (from);
      u.wr.TName (to);
      u.wr.NL    ();
    END;
    print(u, "/* loophole */\n");
    (* If type is already a pointer, then we should not add pointer here.
     * As well, if type does not contain a pointer, then we should store the
     * value in a non-stack-packed temporary and use its address.
     * We don't have code to queue up temporary declarations.
     * (for that matter, to limit/reuse temporaries)
     *)
    u.stack.put(0, "*(" & TypeToText(to) & "*)&" & get(u, 0));
  END loophole;

(*------------------------------------------------ traps & runtime checks ---*)

PROCEDURE abort (u: U; code: RuntimeError) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("abort");
      u.wr.Int   (ORD (code));
      u.wr.NL    ();
    END;
    print(u, "/* abort */\n");
    print(u, "{ m3_abort(" & Fmt.Int(ORD(code)) & ");}\n");
  END abort;

PROCEDURE check_nil (u: U; code: RuntimeError) =
  (* IF (s0.A = NIL) THEN abort(code) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("check_nil");
      u.wr.Int   (ORD (code));
      u.wr.NL    ();
    END;
    print(u, "/* check_nil */\n");
    print(u, "{ const ADDRESS _s0 = " & get(u, 0) & ";\n");
    print(u, "  if (!_s0) m3_abort(" & Fmt.Int(ORD(code)) & ");}\n");
  END check_nil;

PROCEDURE check_lo (u: U; type: IType; READONLY j: Target.Int; code: RuntimeError) =
  (* IF (s0.type < i) THEN abort(code) *)
  VAR typename := TypeToText(type);
      i := TIntN.FromTargetInt(j, CG_Bytes[type]);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("check_lo");
      u.wr.TName (type);
      u.wr.TInt  (i);
      u.wr.Int   (ORD (code));
      u.wr.NL    ();
    END;
    print(u, "/* check_lo */\n");
    print(u, "{ const " & typename & " _i = " & TIntN.ToText(i) & ";\n");
    print(u, "  const " & typename & " _s0 = " & get(u, 0) & ";\n");
    print(u, "  if (_s0 < _i) m3_abort(" & Fmt.Int(ORD(code)) & ");}\n");
  END check_lo;

PROCEDURE check_hi (u: U; type: IType; READONLY j: Target.Int; code: RuntimeError) =
  (* IF (i < s0.type) THEN abort(code) *)
  VAR typename := TypeToText(type);
      i := TIntN.FromTargetInt(j, CG_Bytes[type]);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("check_hi");
      u.wr.TName (type);
      u.wr.TInt  (i);
      u.wr.Int   (ORD (code));
      u.wr.NL    ();
    END;
    print(u, "/* check_hi */\n");
    print(u, "{ const " & typename & " _i = " & TIntN.ToText(i) & ";\n");
    print(u, "  const " & typename & " _s0 = " & get(u, 0) & ";\n");
    print(u, "  if (_i < _s0) m3_abort(" & Fmt.Int(ORD(code)) & ");}\n");
  END check_hi;

PROCEDURE check_range (u: U; type: IType; READONLY xa, xb: Target.Int; code: RuntimeError) =
  (* IF (s0.type < a) OR (b < s0.type) THEN abort(code) *)
  VAR typename := TypeToText(type);
      a := TIntN.FromTargetInt(xa, CG_Bytes[type]);
      b := TIntN.FromTargetInt(xb, CG_Bytes[type]);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("check_range");
      u.wr.TInt  (a);
      u.wr.TInt  (b);
      u.wr.Int   (ORD (code));
      u.wr.NL    ();
    END;
    print(u, "/* check_range */\n");
    print(u, "{ const " & typename & " _a = " & TIntN.ToText(a) & ";\n");
    print(u, "  const " & typename & " _b = " & TIntN.ToText(b) & ";\n");
    print(u, "  const " & typename & " _s0 = " & get(u, 0) & ";\n");
    print(u, "  if ((_s0 < _a) || (_b < _s0)) m3_abort(" & Fmt.Int(ORD(code)) & ");}\n");
  END check_range;

PROCEDURE check_index (u: U; type: IType; code: RuntimeError) =
  (* IF NOT (0 <= s1.type < s0.type) THEN
       abort(code)
     END;
     pop *)
  (* s0.type is guaranteed to be positive so the unsigned
     check (s0.W <= s1.W) is sufficient. *)
  VAR typename := TypeToText(type);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("check_index");
      u.wr.TName (type);
      u.wr.Int   (ORD (code));
      u.wr.NL    ();
    END;
    print(u, "/* check_index */\n");
    print(u, "{ const " & typename & " _array_size = " & pop(u) & ";\n");
    print(u, "  const " & typename & " _index = " & get(u, 0) & ";\n");
    print(u, "  if (_array_size <= _index) m3_abort(" & Fmt.Int(ORD(code)) & ");}\n");
  END check_index;

PROCEDURE check_eq (u: U; type: IType; code: RuntimeError) =
  (* IF (s0.type # s1.type) THEN
       abort(code);
       Pop (2) *)
  VAR typename := TypeToText(type);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("check_eq");
      u.wr.TName (type);
      u.wr.Int   (ORD (code));
      u.wr.NL    ();
    END;
    print(u, "/* check_eq */\n");
    print(u, "{ const " & typename & " _s0 = " & pop(u) & ";\n");
    print(u, "  const " & typename & " _s1 = " & pop(u) & ";\n");
    print(u, "  if (_s0 != s_1) m3_abort(" & Fmt.Int(ORD(code)) & ");}\n");
  END check_eq;

<*NOWARN*>PROCEDURE reportfault (u: U; code: RuntimeError) =
  BEGIN
  END reportfault;

<*NOWARN*>PROCEDURE makereportproc (u: U) =
  BEGIN
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
    print(u, "/* add_offset */\n");
  END add_offset;

PROCEDURE index_address (u: U; type: IType; size: INTEGER) =
  (* s1.A := s1.A + s0.type * size ; pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("index_address");
      u.wr.TName (type);
      u.wr.Int   (size);
      u.wr.NL    ();
    END;
  END index_address;

(*------------------------------------------------------- PROCEDURE calls ---*)

PROCEDURE start_call_direct (u: U; p: Proc; level: INTEGER; type: Type) =
  (* begin a procedure call to a procedure at static level 'level'. *)
  VAR proc := NARROW(p, CProc);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("start_call_direct");
      u.wr.PName (proc);
      u.wr.Int   (level);
      u.wr.TName (type);
      u.wr.NL    ();
    END;
  END start_call_direct;

PROCEDURE start_call_indirect (u: U; type: Type; callingConvention: CallingConvention) =
  (* begin a procedure call to a procedure at static level 'level'. *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("start_call_indirect");
      u.wr.TName (type);
      u.wr.Txt   (callingConvention.name);
      u.wr.NL    ();
    END;
  END start_call_indirect;

PROCEDURE pop_param (u: U; type: MType) =
  (* pop s0 and make it the "next" parameter in the current call *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("pop_param");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
  END pop_param;

PROCEDURE pop_struct (u: U; typeid: TypeUID; size: ByteSize; alignment: Alignment) =
  (* pop s0 and make it the "next" parameter in the current call
   * NOTE: it is passed by value *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("pop_struct");
      u.wr.Tipe  (typeid);
      u.wr.Int   (size);
      u.wr.Int   (alignment);
      u.wr.NL    ();
    END;
  END pop_struct;

PROCEDURE pop_static_link (u: U) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("pop_static_link");
      u.wr.NL    ();
    END;
    print(u, "/* pop_static_link */\n");
  END pop_static_link;

PROCEDURE call_direct (u: U; p: Proc; type: Type) =
  (* call the procedure identified by block b.  The procedure
     returns a value of type type. *)
  VAR proc := NARROW(p, CProc);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("call_direct");
      u.wr.PName (proc);
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, "/* call_direct */\n");
  END call_direct;

PROCEDURE call_indirect (u: U; type: Type; callingConvention: CallingConvention) =
  (* call the procedure whose address is in s0.A and pop s0.  The
     procedure returns a value of type type. *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("call_indirect");
      u.wr.TName (type);
      u.wr.Txt   (callingConvention.name);
      u.wr.NL    ();
    END;
    print(u, "/* call_indirect */\n");
  END call_indirect;

(*------------------------------------------- procedure and closure types ---*)

PROCEDURE load_procedure (u: U; p: Proc) =
  (* push; s0.A := ADDR (proc's body) *)
  VAR proc := NARROW(p, CProc);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_procedure");
      u.wr.PName (proc);
      u.wr.NL    ();
    END;
    print(u, "/* load_procedure */\n");
  END load_procedure;

PROCEDURE load_static_link (u: U; p: Proc) =
  (* push; s0.A := (static link needed to call proc, NIL for top-level procs) *)
  VAR proc := NARROW(p, CProc);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_static_link");
      u.wr.PName (proc);
      u.wr.NL    ();
    END;
    print(u, "/* load_static_link */\n");
    push(u, "0");
  END load_static_link;

(*----------------------------------------------------------------- misc. ---*)

PROCEDURE comment (u: U; a, b, c, d: TEXT := NIL) =
  VAR i: INTEGER := -1;
  BEGIN
    Cmt2 (u, a, i);
    Cmt2 (u, b, i);
    Cmt2 (u, c, i);
    Cmt2 (u, d, i);
    Cmt1 (u, "\n", i);
  END comment;

PROCEDURE Cmt1 (u: U; text: TEXT; VAR width: INTEGER) =
  VAR ch: CHAR;
  BEGIN
    IF (NOT u.debug OR text = NIL) THEN RETURN END;
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
  END Cmt1;

PROCEDURE Cmt2 (u: U; text: TEXT; VAR width: INTEGER) =
  BEGIN
    IF text = NIL THEN
      RETURN
    END;
    Cmt1 (u, text, width);
    print(u, "/* comment: " & text & " */\n");
  END Cmt2;

(*--------------------------------------------------------------- atomics ---*)

PROCEDURE store_ordered (u: U; ztype: ZType; mtype: MType; <*UNUSED*>order: MemoryOrder) =
(* Mem [s1.A].mtype := s0.ztype;
   pop (2) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("store_ordered");
      u.wr.TName (ztype);
      u.wr.TName (mtype);
      u.wr.NL    ();
    END;
    print(u, "/* store_ordered */\n");
  END store_ordered;

PROCEDURE load_ordered (u: U; mtype: MType; ztype: ZType; <*UNUSED*>order: MemoryOrder) =
(* s0.ztype := Mem [s0.A].mtype  *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_ordered");
      u.wr.TName (mtype);
      u.wr.TName (ztype);
      u.wr.NL    ();
    END;
    print(u, "/* load_ordered */\n");
  END load_ordered;

PROCEDURE exchange (u: U; mtype: MType; ztype: ZType; <*UNUSED*>order: MemoryOrder) =
(* tmp := Mem [s1.A + offset].mtype;
   Mem [s1.A + offset].mtype := s0.ztype;
   s0.ztype := tmp;
   pop *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("exchange");
      u.wr.TName (mtype);
      u.wr.TName (ztype);
      u.wr.NL    ();
    END;
    print(u, "/* exchange */\n");
  END exchange;

PROCEDURE compare_exchange (u: U; mtype: MType; ztype: ZType; result_type: IType;
                            <*UNUSED*>success, failure: MemoryOrder) =
(* original := Mem[s2.A].mtype;
   spurious_failure := whatever;
   IF original = Mem[s1.A].mtype AND NOT spurious_failure THEN
     Mem [s2.A].mtype := s0.ztype;
     s2.result_type := 1;
   ELSE
     Mem [s2.A].mtype := original; x86 really does rewrite the original value, atomically
     s2.result_type := 0;
   END;
   pop(2);
   This is permitted to fail spuriously.
   That is, even if Mem[s2.a] = Mem[s1.a], we might
     still go down the then branch.
*)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("compare_exchange");
      u.wr.TName (mtype);
      u.wr.TName (ztype);
      u.wr.TName (result_type);
      u.wr.NL    ();
    END;
    print(u, "/* compare_exchange */\n");
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
    print(u, "/* fence */\n");
  END fence;

CONST AtomicOpName = ARRAY AtomicOp OF TEXT { "add", "sub", "or", "and", "xor" };

PROCEDURE fetch_and_op (u: U; atomic_op: AtomicOp; mtype: MType; ztype: ZType;
                        <*UNUSED*>order: MemoryOrder) =
(* original := Mem [s1.A].mtype;
   Mem [s1.A].mtype := original op s0.ztype;
   s1.ztype := original;
   pop

=> store the new value, return the old value

Generally we use interlocked compare exchange loop.
Some operations can be done better though.
*)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("fetch_and_op");
      u.wr.OutT  (AtomicOpName[atomic_op]);
      u.wr.TName (mtype);
      u.wr.TName (ztype);
      u.wr.NL    ();
    END;
    print(u, "/* fetch_and_op */\n");
  END fetch_and_op;

BEGIN
  BitSizeToEnumCGType[8] := M3CG.Type.Word8;
  BitSizeToEnumCGType[16] := M3CG.Type.Word16;
  BitSizeToEnumCGType[32] := M3CG.Type.Word32;
END M3C.
