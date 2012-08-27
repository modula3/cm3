MODULE M3C;

IMPORT TextSeq, Wr, Text, Fmt, Cstdint;
IMPORT M3CG, M3CG_Ops, Target, TIntN, TFloat, TargetMap;
IMPORT Stdio;
IMPORT RTIO, RTProcess;
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

(* Taken together, these help debugging, as you get more lines in the
C and the error messages reference C line numbers *)
  CONST output_line_directives = FALSE;
  CONST output_extra_newlines = TRUE;

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
        init_fields: TextSeq.T := NIL;
        current_init_offset: INTEGER := 0;
        initializer: TextSeq.T := NIL;
        init_type := Type.Void;
        init_type_count := 0;
        label := 0;
        in_procedure := 0;
        in_block := 0;
        file: TEXT := NIL;
        line: INTEGER := 0;
        line_directive := ""; (* combination of file/line *)
        nl_line_directive := "\n"; (* line_directive + "\n" *)
        last_char_was_newline := FALSE;
        suppress_line_directive: INTEGER := 0;
        in_proc_call : [0 .. 1] := 0; (* based on M3x86 *)
        report_fault: TEXT := NIL; (* based on M3x86 -- reportlabel, global_var *)
        width := 0;
        static_link  := ARRAY [0 .. 1] OF CVar { NIL, NIL };
        param_comma := "";
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

(*
VAR BitSizeToEnumCGType := ARRAY [0..32] OF M3CG.Type { M3CG.Type.Void, .. };
*)

PROCEDURE SetLineDirective(u: U) =
BEGIN
  IF output_line_directives = FALSE THEN
    RETURN;
  END;
  IF u.line > 0 AND u.file # NIL THEN
    u.line_directive := "#line " & Fmt.Int(u.line) & " \"" & u.file & "\"\n";
    u.nl_line_directive := "\n" & u.line_directive;
    IF u.last_char_was_newline THEN
      print(u, u.line_directive);
    ELSE
      print(u, u.nl_line_directive);
    END;
  ELSE
    u.line_directive := "";
    u.nl_line_directive := "\n";
  END;
END SetLineDirective;

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
  proc: CProc;
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
"#define __cdecl /* nothing */",
"#endif",
"#if !defined(_MSC_VER) && !defined(__stdcall)",
"#define __stdcall /* nothing */",
"#endif",
"typedef signed char INT8;",
"typedef unsigned char UINT8, WORD8;",
"typedef short INT16;",
"typedef unsigned short UINT16, WORD16;",
"typedef int INT32;",
"typedef unsigned int UINT32, WORD32;",
"#if defined(_MSC_VER) || defined(__DECC) || defined(__int64)",
"typedef __int64 INT64;",
"typedef unsigned __int64 UINT64;",
"#else",
"typedef long long INT64;",
"typedef unsigned long long UINT64;",
"#endif",
(*
"/* WORD_T/INTEGER are always exactly the same size as a pointer.",
" * VMS sometimes has 32bit size_t/ptrdiff_t but 64bit pointers. */",
*)
"#if __INITIAL_POINTER_SIZE == 64 || defined(_WIN64) || defined(__LP64__) || defined(__x86_64__) || defined(__ppc64__)",
"typedef INT64 INTEGER;",
"typedef UINT64 WORD_T;",
"#else",
"typedef long INTEGER;",
"typedef unsigned long WORD_T;",
"#endif",
"typedef char *ADDRESS;",
"#define NIL ((ADDRESS)0)",
"typedef float REAL;",
"typedef double LONGREAL;",
"typedef /*long*/ double EXTENDED;",
"#define m3_extract_T(T) static T __stdcall m3_extract_##T(T value,WORD_T offset,WORD_T count){return((value>>offset)&~(((~(T)0))<<count));}",
"#define m3_insert_T(T) static T __stdcall m3_insert_##T(T x,T y,WORD_T offset,WORD_T count){T mask=(~((~(T)0)<<count))<<offset;return(((y<<offset)&mask)|(x&~mask));}",
"#define m3_signextend_T(T) static T __stdcall m3_sign_extend_##T(T value,WORD_T count){return(value|((value&(((T)-1)<<(count-1)))?(((T)-1)<<(count-1)):0));}",
"m3_signextend_T(UINT32)",
"m3_signextend_T(UINT64)",
"m3_extract_T(UINT32)",
"m3_extract_T(UINT64)",
"m3_insert_T(UINT32)",
"m3_insert_T(UINT64)",
"#define SET_GRAIN (sizeof(WORD_T)*8)",
(*
"static WORD_T m3_strlen(const char* s) { const char* t = s; while ( *t++) { /* nothing */ } return (WORD_T)(t - s - 1); }",
"static WORD_T m3_uint_to_dec_length(WORD_T i) { WORD_T length = 0; do { length += 1; i /= 10; } while (i); return length; }",
"static char* m3_strrev(char* s, unsigned length) { if (length > 1) { unsigned i = 0; unsigned j = length - 1; while (i < j) { \\",
"char t = s[i]; s[i] = s[j]; s[j] = t; i += 1; j -= 1; } } return s; }",
"static char* __stdcall m3_concat(char* buffer, char** s, WORD_T i);",
"static WORD_T __stdcall m3_concat_length(char** s, WORD_T i) { WORD_T length = 0, j; for (i = 0; i < j; ++i) length += m3_strlen(s[i); return length; }",
#define M3_CONCAT(m3_conc
"static char* __stdcall m3_uint_to_dec(char* buffer, WORD_T i)",
"{ WORD_T length = 0; do { buffer[length++] = "0123456789"[i % 10]; i /= 10; } while (i);",
"  buffer[length] = 0; m3_strrev(buffer, length); return buffer; }",
"#define M3_UINT_DEC(i) m3_uint_to_dec(alloca(m3_uint_to_dec_length(i) + 1), i)",
*)
"void __stdcall RTHooks__ReportFault(ADDRESS module, INTEGER code);",
(*"static void __stdcall m3_abort(const char* message, size_t length){fflush(NIL);write(2, message, length);abort();}\n",*)
"static WORD_T __stdcall m3_set_member(WORD_T elt,WORD_T*set){return(set[elt/SET_GRAIN]&(((WORD_T)1)<<(elt%SET_GRAIN)))!=0;}",
"static void __stdcall m3_set_union(WORD_T n_bits,WORD_T*c,WORD_T*b,WORD_T*a){WORD_T i,n_words = n_bits / SET_GRAIN;for (i = 0; i < n_words; i++)a[i] = b[i] | c[i];}",
"static void __stdcall m3_set_intersection(WORD_T n_bits, WORD_T* c, WORD_T* b, WORD_T* a){WORD_T i,n_words = n_bits / SET_GRAIN;for (i = 0; i < n_words; i++)a[i] = b[i] & c[i];}",
"static void __stdcall m3_set_difference(WORD_T n_bits,WORD_T*c,WORD_T*b,WORD_T*a){WORD_T i,n_words=n_bits/SET_GRAIN;for(i=0;i<n_words;++i)a[i]=b[i]&(~c[i]);}",
"static void __stdcall m3_set_sym_difference(WORD_T n_bits,WORD_T*c,WORD_T*b,WORD_T*a){WORD_T i,n_words=n_bits/SET_GRAIN;for(i=0;i<n_words;++i)a[i]=b[i]^c[i];}",
"static WORD_T __stdcall m3_set_eq(WORD_T n_bits,WORD_T*b,WORD_T*a){return(memcmp(a,b,n_bits/8)==0);}",
"static WORD_T __stdcall m3_set_ne(WORD_T n_bits,WORD_T*b,WORD_T*a){return(memcmp(a,b,n_bits/8)!=0);}",
"static WORD_T __stdcall m3_set_le(WORD_T n_bits,WORD_T*b,WORD_T*a){WORD_T n_words=n_bits/SET_GRAIN;WORD_T i;for(i=0;i<n_words;++i)if(a[i]&(~b[i]))return 0;return 1;}",
"static WORD_T __stdcall m3_set_lt(WORD_T n_bits,WORD_T*b,WORD_T*a){WORD_T n_words=n_bits/SET_GRAIN;WORD_T i,eq=0;for(i=0;i<n_words;++i)if(a[i]&(~b[i]))return 0;else eq|=(a[i]^b[i]);return(eq!=0);}",
"static WORD_T __stdcall m3_set_ge(WORD_T n_bits,WORD_T*b,WORD_T*a){return set_le(n_bits,a,b);}",
"static WORD_T __stdcall m3_set_gt(WORD_T n_bits,WORD_T*b,WORD_T*a){return set_lt(n_bits,a,b);}",
"#define M3_HIGH_BITS(a) ((~(WORD_T)0) << (a))",
"#define M3_LOW_BITS(a)  ((~(WORD_T)0) >> (SET_GRAIN - (a) - 1))",
"static void __stdcall m3_set_range(WORD_T b, WORD_T a, WORD_T*s){if(a>=b){WORD_T i,a_word=a/SET_GRAIN,b_word=b/SET_GRAIN,high_bits=M3_HIGH_BITS(a%SET_GRAIN),low_bits=M3_LOW_BITS(b%SET_GRAIN);if(a_word==b_word){s[a_word]|=(high_bits&low_bits);}else{s[a_word]|=high_bits;for(i=a_word+1;i<b_word;++i)s[i]=~(WORD_T)0;s[b_word]|=low_bits;}}}",
"static void __stdcall m3_set_singleton(WORD_T a,WORD_T*s){s[a/SET_GRAIN]|=(((WORD_T)1)<<(a%SET_GRAIN));}",
"#define m3_shift_T(T) T m3_shift_##T(T value,INTEGER shift){if((shift>=(sizeof(T)*8))||(shift<=-(sizeof(T)*8)))value=0;else if(shift<0)value<<=shift;else if(shift>0)value>>=shift;return value;}",
"m3_shift_T(UINT32)",
"m3_shift_T(UINT64)",
"/* return positive form of a negative value, avoiding overflow */",
"/* T should be an unsigned type */",
"#define M3_POS(T, a) (((T)-((a) + 1)) + 1)",
"#define m3_rotate_left_T(T)  static T __stdcall m3_rotate_left_##T (T a, int b) { return ((a << b) | (a >> ((sizeof(a) * 8) - b))); }",
"#define m3_rotate_right_T(T) static T __stdcall m3_rotate_right_##T(T a, int b) { return ((a >> b) | (a << ((sizeof(a) * 8) - b))); }",
"#define m3_rotate_T(T)       static T __stdcall m3_rotate_##T(T a, int b) { b &= ((sizeof(a) * 8) - 1); if (b > 0) a = m3_rotate_left_##T(a, b); else if (b < 0) a = m3_rotate_right_##T(a, -b); return a; }",
"#define m3_abs_T(T) static T __stdcall m3_abs_##T(T a) { return ((a < 0) ? ((T)-(U##T)a) : a); }",
"#define m3_min_T(T) static T __stdcall m3_min_##T(T a, T b) { return ((a < b) ? a : b); }",
"#define m3_max_T(T) static T __stdcall m3_max_##T(T a, T b) { return ((a > b) ? a : b); }",
"#define m3_div_T(T) static T __stdcall m3_div_##T(T a, T b) \\",
"{ \\",
"  int aneg = (a < 0); \\",
"  int bneg = (b < 0); \\",
"  if (aneg == bneg || a == 0 || b == 0) \\",
"    return (a / b); \\",
"  else \\",
"  { \\",
"    /* round negative result down by rounding positive result up \\",
"       unsigned math is much better defined, see gcc -Wstrict-overflow=4 */ \\",
"    U##T ua = (aneg ? M3_POS(U##T, a) : (U##T)a); \\",
"    U##T ub = (bneg ? M3_POS(U##T, b) : (U##T)b); \\",
"    return -(T)((ua + ub - 1) / ub); \\",
"  } \\",
"}",
"#define m3_mod_T(T) static T __stdcall m3_mod_##T(T a, T b) \\",
"{ \\",
"  int aneg = (a < 0); \\",
"  int bneg = (b < 0); \\",
"  if (aneg == bneg || a == 0 || b == 0) \\",
"    return (a % b); \\",
"  else \\",
"  { \\",
"    U##T ua = (aneg ? M3_POS(U##T, a) : (U##T)a); \\",
"    U##T ub = (bneg ? M3_POS(U##T, b) : (U##T)b); \\",
"    a = (T)(ub - 1 - (ua + ub - 1) % ub); \\",
"    return (bneg ? -a : a); \\",
"  } \\",
"}",
"m3_div_T(INT32)",
"m3_mod_T(INT32)",
"m3_rotate_left_T(UINT32)",
"m3_rotate_right_T(UINT32)",
"m3_rotate_T(UINT32)",
"m3_abs_T(INT32)",
"m3_min_T(UINT32)",
"m3_max_T(UINT32)",
"m3_min_T(INT32)",
"m3_max_T(INT32)",
"m3_div_T(INT64)",
"m3_mod_T(INT64)",
"m3_rotate_left_T(UINT64)",
"m3_rotate_right_T(UINT64)",
"m3_rotate_T(UINT64)",
"m3_abs_T(INT64)",
"m3_min_T(UINT64)",
"m3_max_T(UINT64)",
"m3_min_T(INT64)",
"m3_max_T(INT64)",
"double __cdecl floor(double);",
"double __cdecl ceil(double);",
"INT64 __cdecl llroundl(long double);",
"static INT64 __stdcall m3_floor(EXTENDED f) { return floor(f); }",
"static INT64 __stdcall m3_ceil(EXTENDED f) { return ceil(f); }",
"static INT64 __stdcall m3_trunc(EXTENDED f) { return (INT64)f; }",
"static INT64 __stdcall m3_round(EXTENDED f) { return (INT64)llroundl(f); }",
""};

<*NOWARN*>CONST Suffix = ARRAY OF TEXT {
"#ifdef __cplusplus",
"} /* extern \"C\" */",
"#endif"
};

CONST typeToText = ARRAY CGType OF TEXT {
    "UINT8",  "INT8",  (* FUTURE: U8, I8 *)
    "UINT16", "INT16", (* FUTURE: U16, I16 *)
    "UINT32", "INT32", (* FUTURE: U32, I32 *)
    "UINT64", "INT64", (* FUTURE: U64, I64 *)
    "REAL", "LONGREAL", "EXTENDED", (* FUTURE: R, L, E *)
    "ADDRESS", (* FUTURE: A or P for pointer *)
    "STRUCT", 
    "void" (* FUTURE: V *)
  };

PROCEDURE Tests() = 
  PROCEDURE Trunc1(f: REAL): Cstdint.int16_t = BEGIN RETURN TRUNC(f); END Trunc1;
  PROCEDURE Trunc2(f: REAL): INTEGER = BEGIN RETURN TRUNC(f); END Trunc2;
  PROCEDURE Trunc3(f: REAL): CARDINAL = BEGIN RETURN TRUNC(f); END Trunc3;
BEGIN
END Tests;

CONST CompareOpC = ARRAY CompareOp OF TEXT { "==", "!=", ">", ">=", "<", "<=" };
CONST ConvertOpName = ARRAY ConvertOp OF TEXT { "round", "trunc", "floor", "ceil" };
CONST CompareOpName = ARRAY CompareOp OF TEXT { "eq", "ne", "gt", "ge", "lt", "le" };

(*---------------------------------------------------------------------------*)

PROCEDURE paren(text: TEXT): TEXT =
BEGIN
(* It is possible we can reduce parens, but it isn't as simple as it seems. *)
    RETURN "(" & text & ")";
END paren;

PROCEDURE pop(u: U; n: CARDINAL := 1) =
BEGIN
    FOR i := 1 TO n DO
        EVAL u.stack.remlo();
    END;
END pop;

PROCEDURE push(u: U; <*UNUSED*>type: Type; expression: TEXT) =
BEGIN
    u.stack.addlo(expression);
END push;

PROCEDURE get(u: U; n: CARDINAL := 0): TEXT =
BEGIN
    RETURN u.stack.get(n);
END get;

PROCEDURE SuppressLineDirective(u: U; adjust: INTEGER; <*UNUSED*>reason: TEXT) =
BEGIN
  INC(u.suppress_line_directive, adjust);
  (*
  RTIO.PutText("suppress_line_directive now " & Fmt.Int(u.suppress_line_directive) & " due to " & reason & "\n");
  RTIO.Flush();
  *)
END SuppressLineDirective;

PROCEDURE print(u: U; text: TEXT) = <*FATAL ANY*>
  VAR length := Text.Length(text);
  BEGIN
    IF length = 0 THEN
      RETURN;
    END;
    IF output_extra_newlines AND Text.FindChar(text, '\n') = -1 THEN
      Wr.PutText(u.c, text & "\n");
      u.last_char_was_newline := TRUE;
    ELSE
      Wr.PutText(u.c, text);
    END;

    IF text = u.line_directive OR text = u.nl_line_directive THEN
      u.width := 0;
      u.last_char_was_newline := TRUE;
      RETURN;
    END;

    IF (*u.suppress_line_directive < 1 AND*) Text.GetChar(text, length - 1) = '\n' THEN
      Wr.PutText(u.c, u.line_directive);
      u.width := 0;
      u.last_char_was_newline := TRUE;
      RETURN;
    END;

    IF Text.FindChar(text, '\n') # -1 THEN
      u.width := 0; (* roughly *)
      Wr.PutText(u.c, u.nl_line_directive);
      u.last_char_was_newline := TRUE;
      RETURN;
    END;

    INC(u.width, length);
    IF u.width < 900 THEN
      u.last_char_was_newline := FALSE;
      RETURN;
    END;

    u.width := 0;
    IF u.last_char_was_newline THEN
      Wr.PutText(u.c, u.line_directive);
    ELSE
      Wr.PutText(u.c, u.nl_line_directive);
    END;
    u.last_char_was_newline := TRUE;
  END print;

(*---------------------------------------------------------------------------*)

PROCEDURE New (cfile: Wr.T): M3CG.T =
  VAR u := NEW (U);
  BEGIN
    u.wr := Wrx86.New (Stdio.stdout);
    (*u.debug := TRUE;*)
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

PROCEDURE next_label(u: U; n: INTEGER := 1): Label =
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

PROCEDURE F1(a: INTEGER): INTEGER = BEGIN RETURN F2(F3(F4(F5(F6(F7(F8(F9(F10(F11(F12(F13(F14(a))))))))))))); END F1;
PROCEDURE F2(a: INTEGER): INTEGER = BEGIN RETURN a; END F2;
PROCEDURE F3(a: INTEGER): INTEGER = BEGIN RETURN a; END F3;
PROCEDURE F4(a: INTEGER): INTEGER = BEGIN RETURN a; END F4;
PROCEDURE F5(a: INTEGER): INTEGER = BEGIN RETURN a; END F5;
PROCEDURE F6(a: INTEGER): INTEGER = BEGIN RETURN a; END F6;
PROCEDURE F7(a: INTEGER): INTEGER = BEGIN RETURN a; END F7;
PROCEDURE F8(a: INTEGER): INTEGER = BEGIN RETURN a; END F8;
PROCEDURE F9(a: INTEGER): INTEGER = BEGIN RETURN a; END F9;
PROCEDURE F10(a: INTEGER): INTEGER = BEGIN RETURN a; END F10;
PROCEDURE F11(a: INTEGER): INTEGER = BEGIN RETURN a; END F11;
PROCEDURE F12(a: INTEGER): INTEGER = BEGIN RETURN a; END F12;
PROCEDURE F13(a: INTEGER): INTEGER = BEGIN RETURN a; END F13;
PROCEDURE F14(a: INTEGER): INTEGER = BEGIN RETURN a; END F14;

PROCEDURE begin_unit(u: U; optimize: INTEGER) =
  (* called before any other method to initialize the compilation unit *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd ("begin_unit");
      u.wr.Int (optimize);
      u.wr.NL  ();
    END;
    print(u, " /* begin unit */\n");
    SuppressLineDirective(u, 1, "begin_unit");
    FOR i := FIRST(Prefix) TO LAST(Prefix) DO
      print(u, Prefix[i] & "\n");
    END;
    SuppressLineDirective(u, -1, "begin_unit");

    u.report_fault := NIL;
    u.in_proc_call := 0;

  END begin_unit;

PROCEDURE end_unit(u: U) =
  (* called after all other methods to finalize the unit and write the
     resulting object *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd ("end_unit");
      u.wr.NL  ();
    END;
    print(u, " /* end unit */\n");
    u.line_directive := ""; (* really suppress *)
    u.nl_line_directive := "\n"; (* really suppress *)
    SuppressLineDirective(u, 1, "end_unit");
    FOR i := FIRST(Suffix) TO LAST(Suffix) DO
      print(u, Suffix[i]);
      print(u, "\n");
    END;
    SuppressLineDirective(u, -1, "end_unit");
  END end_unit;

PROCEDURE import_unit(u: U; name: Name) =
  (* note that the current compilation unit imports the interface 'name' *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("import_unit");
      u.wr.ZName (name);
      u.wr.NL    ();
    END
  END import_unit;

PROCEDURE export_unit(u: U; name: Name) =
  (* note that the current compilation unit exports the interface 'name' *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("export_unit");
      u.wr.ZName (name);
      u.wr.NL    ();
    END
  END export_unit;

(*------------------------------------------------ debugging line numbers ---*)

PROCEDURE set_source_file(u: U; file: TEXT) =
  (* Sets the current source file name. Subsequent statements
     and expressions are associated with this source location. *)
  BEGIN
    IF u.debug THEN
      u.wr.OutT ("\t\t\t\t\t-----FILE ");
      u.wr.OutT (file);
      u.wr.OutT ("  -----");
      u.wr.NL ();
      print(u, " /* set_source_file */ ");
    END;
    u.file := file;
    SetLineDirective(u);
  END set_source_file;

PROCEDURE set_source_line(u: U; line: INTEGER) =
  (* Sets the current source line number. Subsequent statements
   and expressions are associated with this source location. *)
  BEGIN
    IF u.debug THEN
      u.wr.OutT ("\t\t\t\t\t-----LINE");
      u.wr.Int  (line);
      u.wr.OutT ("  -----");
      u.wr.NL ();
      print(u, " /* set_source_line */ ");
    END;
    u.line := line;
    SetLineDirective(u);
  END set_source_line;

(*------------------------------------------- debugging type declarations ---*)

PROCEDURE declare_typename(u: U; typeid: TypeUID; name: Name) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_typename");
      u.wr.Tipe  (typeid);
      u.wr.ZName (name);
      u.wr.NL    ();
      print(u, " /* declare_typename */ ");
    END;
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

PROCEDURE declare_array(u: U; typeid, index_typeid, element_typeid: TypeUID; total_bit_size: BitSize) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_array");
      u.wr.Tipe (typeid);
      u.wr.Tipe (index_typeid);
      u.wr.Tipe (element_typeid);
      u.wr.BInt (total_bit_size);
      u.wr.NL   ();
      print (u, " /* declare_array */ ");
    END;
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
      print(u, ";");
    END;
*)
  END declare_array;

PROCEDURE declare_open_array(u: U; typeid, element_typeid: TypeUID; bit_size: BitSize) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_open_array");
      u.wr.Tipe (typeid);
      u.wr.Tipe (element_typeid);
      u.wr.BInt (bit_size);
      u.wr.NL   ();
      print (u, " /* declare_open_array */ ");
    END;
    <* ASSERT bit_size MOD 32 = 0 *>
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
      print(u, ";}" & TypeIDToText(element_typeid) & ";");
      EVAL typeidToType.put(typeid, NEW(OpenArray_t,
              typeid := typeid,
              byte_size := bit_size DIV 8,
              bit_size := bit_size,
              element_typeid := element_typeid,
              element_type := element_type));
    END;
*)
  END declare_open_array;

PROCEDURE declare_enum(u: U; typeid: TypeUID; n_elts: INTEGER; bit_size: BitSize) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_enum");
      u.wr.Tipe (typeid);
      u.wr.Int  (n_elts);
      u.wr.BInt (bit_size);
      u.wr.NL   ();
      print (u, " /* declare_enum */ ");
    END;
    SuppressLineDirective(u, n_elts, "declare_enum n_elts");
    <* ASSERT bit_size = 8 OR bit_size = 16 OR bit_size = 32 *>
(*
    WITH type = NEW(Enum_t, typeid := typeid, max := n_elts - 1, cg_type := BitSizeToEnumCGType[bit_size]) DO
      <* ASSERT u.enum = NIL *>
      u.enum := type;
      EVAL Type_Init(type);
      u.enum_id := TypeIDToText(typeid);
      u.enum_value := 0;
      u.enum_type := "UINT" & Fmt.Int(bit_size);
      print(u, "typedef " & u.enum_type & " " & u.enum_id & ";");
    END;
*)
  END declare_enum;

PROCEDURE declare_enum_elt(u: U; name: Name) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_enum_elt");
      u.wr.ZName (name);
      u.wr.NL    ();
      print (u, " /* declare_enum_elt */ ");
    END;
    SuppressLineDirective(u, -1, "declare_enum_elt");
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

PROCEDURE declare_packed(u: U; typeid: TypeUID; bit_size: BitSize; base: TypeUID) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_packed");
      u.wr.Tipe (typeid);
      u.wr.BInt (bit_size);
      u.wr.Tipe (base);
      u.wr.NL   ();
      print (u, " /* declare_packed */ ");
    END;
  END declare_packed;

PROCEDURE declare_record(u: U; typeid: TypeUID; bit_size: BitSize; n_fields: INTEGER) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_record");
      u.wr.Tipe (typeid);
      u.wr.BInt (bit_size);
      u.wr.Int  (n_fields);
      u.wr.NL   ();
      print (u, " /* declare_record */ ");
    END;
    SuppressLineDirective(u, n_fields, "declare_record n_fields");
  END declare_record;

PROCEDURE declare_field(u: U; name: Name; offset: BitOffset; bit_size: BitSize; typeid: TypeUID) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_field");
      u.wr.ZName (name);
      u.wr.BInt  (offset);
      u.wr.BInt  (bit_size);
      u.wr.Tipe  (typeid);
      u.wr.NL    ();
      print (u, " /* declare_field */ ");
    END;
    SuppressLineDirective(u, -1, "declare_field");
  END declare_field;

PROCEDURE declare_set(u: U; typeid, domain: TypeUID; bit_size: BitSize) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_set");
      u.wr.Tipe (typeid);
      u.wr.Tipe (domain);
      u.wr.BInt (bit_size);
      print (u, " /* declare_set */ ");
    END;
  END declare_set;

PROCEDURE declare_subrange(u: U; typeid, domain: TypeUID;
                           READONLY min, max: Target.Int;
                           bit_size: BitSize) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_subrange");
      u.wr.Tipe (typeid);
      u.wr.Tipe (domain);
      u.wr.TInt (TIntN.FromTargetInt(min, NUMBER(min))); (* What about size? *)
      u.wr.TInt (TIntN.FromTargetInt(max, NUMBER(max))); (* What about size? *)
      u.wr.BInt (bit_size);
      print (u, " /* declare_subrange */ ");
    END;
  END declare_subrange;

PROCEDURE declare_pointer(u: U; typeid, target: TypeUID; brand: TEXT; traced: BOOLEAN) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_pointer");
      u.wr.Tipe (typeid);
      u.wr.Tipe (target);
      u.wr.Txt  (brand);
      u.wr.Bool (traced);
      u.wr.NL   ();
      print (u, " /* declare_pointer */ ");
    END;
  END declare_pointer;

PROCEDURE declare_indirect(u: U; typeid, target: TypeUID) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd  ("declare_indirect");
      u.wr.Tipe (typeid);
      u.wr.Tipe (target);
      u.wr.NL   ();
      print(u, " /* declare_indirect */ ");
    END;
  END declare_indirect;

PROCEDURE declare_proctype(u: U; typeid: TypeUID; n_formals: INTEGER;
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
      print (u, " /* declare_proctype */ ");
    END;
    SuppressLineDirective(u, n_formals + (ORD(n_raises >= 0) * n_raises), "declare_proctype n_formals + n_raises");
  END declare_proctype;

PROCEDURE declare_formal(u: U; name: Name; typeid: TypeUID) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_formal");
      u.wr.ZName (name);
      u.wr.Tipe  (typeid);
      u.wr.NL    ();
      print(u, " /* declare_formal */ ");
    END;
    SuppressLineDirective(u, -1, "declare_formal");
  END declare_formal;

PROCEDURE declare_raises(u: U; name: Name) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_raises");
      u.wr.ZName (name);
      u.wr.NL    ();
      print (u, " /* declare_raises */ ");
    END;
    SuppressLineDirective(u, -1, "declare_raises");
  END declare_raises;

PROCEDURE declare_object(u: U; typeid, super: TypeUID;
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
      print (u, " /* declare_object */ ");
    END;
    SuppressLineDirective(u, n_fields + n_methods, "declare_object n_fields + n_methods");
  END declare_object;

PROCEDURE declare_method(u: U; name: Name; signature: TypeUID) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_method");
      u.wr.ZName (name);
      u.wr.Tipe  (signature);
      u.wr.NL    ();
      print (u, " /* declare_method */ ");
    END;
    SuppressLineDirective(u, -1, "declare_method");
  END declare_method;

PROCEDURE declare_opaque(u: U; typeid, super: TypeUID) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_opaque");
      u.wr.Tipe  (typeid);
      u.wr.Tipe  (super);
      u.wr.NL    ();
      print (u, " /* declare_opaque */ ");
    END;
  END declare_opaque;

PROCEDURE reveal_opaque(u: U; lhs, rhs: TypeUID) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("reveal_opaque");
      u.wr.Tipe  (lhs);
      u.wr.Tipe  (rhs);
      u.wr.NL    ();
      print (u, " /* reveal_opaque */ ");
    END;
  END reveal_opaque;

PROCEDURE declare_exception(u: U; name: Name; arg_type: TypeUID;
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
      print (u, " /* declare_exception */ ");
    END;
  END declare_exception;

(*--------------------------------------------------------- runtime hooks ---*)

PROCEDURE set_runtime_proc(u: U; name: Name; p: Proc) =
  VAR proc := NARROW(p, CProc);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("set_runtime_proc");
      u.wr.ZName (name);
      u.wr.PName (proc);
      u.wr.NL    ();
      print (u, " /* set_runtime_proc */ ");
    END;
  END set_runtime_proc;

(*------------------------------------------------- variable declarations ---*)

PROCEDURE import_global(u: U; name: Name; byte_size: ByteSize; alignment: Alignment; type: Type; typeid: TypeUID): Var =
  VAR var := NEW(CVar, type := type, name := FixName(name));
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("import_global");
      u.wr.ZName (name);
      u.wr.Int   (byte_size);
      u.wr.Int   (alignment);
      u.wr.TName (type);
      u.wr.Tipe  (typeid);
      u.wr.VName (var);
      u.wr.NL    ();
      print (u, " /* import_global */ ");
    END;
    RETURN var;
  END import_global;

PROCEDURE declare_segment(u: U; name: Name; typeid: TypeUID; is_const: BOOLEAN): Var =
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
    print (u, " /* declare_segment */ ");
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
    print(u, "struct " & text & "_t;");
    IF is_const THEN
      print(u, "const ");
    END;
    print(u, "static struct " & text & "_t " & text & ";");

    IF u.report_fault = NIL AND NOT is_const THEN (* See M3x86.m3 *)
      u.report_fault := M3ID.ToText(var.name) & "_CRASH";
      print(u, "void __stdcall " & u.report_fault & "(UINT32 code) { RTHooks__ReportFault(&" & M3ID.ToText(var.name) & ",code);}");
    END;

    RETURN var;
  END declare_segment;

PROCEDURE bind_segment(u: U; v: Var; byte_size: ByteSize; alignment: Alignment;
                       type: Type; exported, inited: BOOLEAN) =
  VAR var := NARROW(v, CVar);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("bind_segment");
      u.wr.VName (var);
      u.wr.Int   (byte_size);
      u.wr.Int   (alignment);
      u.wr.TName (type);
      u.wr.Bool  (exported);
      u.wr.Bool  (inited);
      u.wr.NL    ();
    END;
    print (u, " /* bind_segment */ ");
  END bind_segment;

PROCEDURE declare_global(u: U; name: Name; byte_size: ByteSize; alignment: Alignment;
                         type: Type; typeid: TypeUID; exported, inited: BOOLEAN): Var =
  BEGIN
    print (u, " /* declare_global */ ");
    RETURN DeclareGlobal(u, name, byte_size, alignment, type, typeid, exported, inited, FALSE);
  END declare_global;

PROCEDURE declare_constant(u: U; name: Name; byte_size: ByteSize; alignment: Alignment;
                           type: Type; typeid: TypeUID; exported, inited: BOOLEAN): Var =
  BEGIN
    print (u, " /* declare_constant */ ");
    RETURN DeclareGlobal(u, name, byte_size, alignment, type, typeid, exported, inited, TRUE);
  END declare_constant;

PROCEDURE DeclareGlobal(u: U; name: Name; byte_size: ByteSize; alignment: Alignment;
                        type: Type; typeid: TypeUID;
                        exported, inited, is_const: BOOLEAN): Var =
  CONST DeclTag = ARRAY BOOLEAN OF TEXT { "declare_global", "declare_constant" };
  VAR var := NEW(CVar, name := FixName(name));
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   (DeclTag [is_const]);
      u.wr.ZName (name);
      u.wr.Int   (byte_size);
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

PROCEDURE declare_local(u: U; name: Name; byte_size: ByteSize; alignment: Alignment;
                        type: Type; typeid: TypeUID; in_memory, up_level: BOOLEAN;
                        frequency: Frequency): Var =
VAR var := NEW(CVar, type := type, name := FixName(name));
    text: TEXT;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_local");
      u.wr.ZName (name);
      u.wr.Int   (byte_size);
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
    print (u, " /* declare_local */ ");
    IF type = Type.Struct THEN
      text := "struct{char a[" & Fmt.Int(byte_size) & "];}";
    ELSE
      text := typeToText[type];
    END;
    text := text & " " & M3ID.ToText(var.name) & ";";
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
  SuppressLineDirective(u, 1, "funtion_prototype");
  print(u, typeToText[proc.return_type] & " __stdcall " & M3ID.ToText(proc.name));
  IF NUMBER (params^) = 0 THEN
    print(u, "(void)");
  ELSE
    print(u, "(");
    FOR i := FIRST(params^) TO LAST(params^) DO
      WITH param = params[i] DO
        print(u, typeToText[param.type]);
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
  SuppressLineDirective(u, -1, "funtion_prototype");
END function_prototype;

PROCEDURE declare_param(u: U; name: Name; byte_size: ByteSize; alignment: Alignment;
                        type: Type; typeid: TypeUID; in_memory, up_level: BOOLEAN;
                        frequency: Frequency): Var =
VAR var := NEW(CVar, type := type, name := FixName(name), type := type);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_param");
      u.wr.ZName (name);
      u.wr.Int   (byte_size);
      u.wr.Int   (alignment);
      u.wr.TName (type);
      u.wr.Tipe  (typeid);
      u.wr.Bool  (in_memory);
      u.wr.Bool  (up_level);
      u.wr.Int   (frequency);
      u.wr.VName (var);
      (*u.wr.Int   (var.offset);*)
      u.wr.NL    ();
      print (u, " /* declare_param */ ");
    END;
    u.function.params[u.param_count] := var;
    SuppressLineDirective(u, -1, "declare_param");
    INC(u.param_count);
    IF u.param_count = NUMBER(u.function.params^) THEN
      function_prototype(u, u.function);
      print(u, ";");
      u.param_count := -1000; (* catch bugs *)
    END;
    RETURN var;
  END declare_param;

PROCEDURE declare_temp(u: U; byte_size: ByteSize; alignment: Alignment; type: Type; in_memory:BOOLEAN): Var =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("declare_temp");
      u.wr.Int   (byte_size);
      u.wr.Int   (alignment);
      u.wr.TName (type);
      u.wr.Bool  (in_memory);
      (*u.wr.VName (var);*)
      (*u.wr.Int   (var.offset);*)
      u.wr.NL    ();
    END;
    print (u, " /* declare_temp => declare_local */ ");
    RETURN declare_local(u, 0, byte_size, alignment, type, -1, in_memory, FALSE, M3CG.Always);
  END declare_temp;

PROCEDURE free_temp(u: U; v: Var) =
  VAR var := NARROW(v, CVar);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("free_temp");
      u.wr.VName (var);
      u.wr.NL    ();
      print (u, " /* free_temp */ ");
    END;
  END free_temp;

(*---------------------------------------- static variable initialization ---*)

PROCEDURE begin_init(u: U; v: Var) =
  VAR var := NARROW(v, CVar);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("begin_init");
      u.wr.VName (var);
      u.wr.NL    ();
    END;
    print (u, " /* begin_init */ ");
    u.current_init_offset := 0;
    SuppressLineDirective(u, 1, "begin_init");
  END begin_init;

PROCEDURE end_init(u: U; v: Var) =
  VAR var := NARROW(v, CVar);
      init_fields := u.init_fields;
      initializer := u.initializer;
      var_name := M3ID.ToText(var.name);
      comma := "";
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("end_init");
      u.wr.VName (var);
      u.wr.NL    ();
    END;
    print (u, " /* end_init */ ");
    end_init_helper(u);
    IF var.is_const THEN
      print(u, "const ");
    END;
    print(u, "static struct " & var_name & "_t{");
    WHILE init_fields.size() > 0 DO
      print (u, init_fields.remlo());
    END;
    print(u, "}" & var_name & "={");
    WHILE initializer.size() > 0 DO
      print(u, comma);
      print(u, initializer.remlo());
      comma := ",";
    END;
    print (u, "};");
    SuppressLineDirective(u, -1, "end_init");
  END end_init;

PROCEDURE init_to_offset(u: U; offset: ByteOffset) =
  VAR pad := offset - u.current_init_offset;
      init_fields := u.init_fields;
      initializer := u.initializer;
  BEGIN
    <* ASSERT offset >= u.current_init_offset *>
    <* ASSERT pad >= 0 *>
    <* ASSERT u.current_init_offset >= 0 *>
    IF pad > 0 THEN
      end_init_helper(u);
      init_fields.addhi ("char " & M3ID.ToText(FixName(0)) & "[" & Fmt.Int(pad) & "];");
      initializer.addhi("{0}");
    END;
  END init_to_offset;

PROCEDURE end_init_helper(u: U) =
BEGIN
  IF u.init_type_count > 0 THEN
    u.init_fields.addhi("[" & Fmt.Int(u.init_type_count) & "];");
  END;
  u.init_type_count := 0;
END end_init_helper;

PROCEDURE init_helper(u: U; offset: ByteOffset; type: Type) =
BEGIN
    init_to_offset (u, offset);
    IF offset = 0 OR u.init_type # type OR offset # u.current_init_offset THEN
      end_init_helper(u);
      u.init_fields.addhi(typeToText[type] & " " & M3ID.ToText(FixName(0)));
    END;
    INC(u.init_type_count);
    u.init_type := type;
    u.current_init_offset := offset + TargetMap.CG_Bytes[type];
END init_helper;

PROCEDURE init_int(u: U; offset: ByteOffset; READONLY value: Target.Int; type: Type) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("init_int");
      u.wr.Int   (offset);
      u.wr.TInt  (TIntN.FromTargetInt(value, CG_Bytes[type]));
      u.wr.TName (type);
      u.wr.NL    ();
      print (u, " /* init_int */ ");
    END;
    init_helper(u, offset, type);
    u.initializer.addhi(TInt.ToText(value));
  END init_int;

PROCEDURE init_proc(u: U; offset: ByteOffset; p: Proc) =
  VAR proc := NARROW(p, CProc);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("init_proc");
      u.wr.Int   (offset);
      u.wr.PName (proc);
      u.wr.NL    ();
      print (u, " /* init_proc */ ");
    END;
    init_helper(u, offset, Type.Addr); (* FUTURE: better typing *)
    u.initializer.addhi("(ADDRESS)&" & M3ID.ToText(proc.name));
  END init_proc;

PROCEDURE init_label(u: U; offset: ByteOffset; value: Label) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("init_label");
      u.wr.Int   (offset);
      u.wr.Lab   (value);
      u.wr.NL    ();
    END;
    print (u, " /* init_label */ ");
    <* ASSERT FALSE *>
  END init_label;

PROCEDURE init_var(u: U; offset: ByteOffset; v: Var; bias: ByteOffset) =
  VAR var := NARROW(v, CVar);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("init_var");
      u.wr.Int   (offset);
      u.wr.VName (var);
      u.wr.Int   (bias);
      u.wr.NL    ();
      print(u, " /* init_var */ ");
    END;
    init_helper(u, offset, Type.Addr); (* FUTURE: better typing *)
    IF bias # 0 THEN
      u.initializer.addhi(Fmt.Int(bias) & "+"& "(ADDRESS)&" & M3ID.ToText(var.name));
    ELSE
      u.initializer.addhi("(ADDRESS)&" & M3ID.ToText(var.name));
    END;
  END init_var;

PROCEDURE init_offset(u: U; offset: ByteOffset; value: Var) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("init_offset");
      u.wr.Int   (offset);
      u.wr.VName (value);
      u.wr.NL    ();
    END;
    print (u, " /* init_offset */ ");
    <* ASSERT FALSE *>
  END init_offset;

PROCEDURE init_chars(u: U; offset: ByteOffset; value: TEXT) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("init_chars");
      u.wr.Int   (offset);
      u.wr.Txt   (value);
      u.wr.NL    ();
    END;
    print (u, " /* init_chars */ ");
  END init_chars;

PROCEDURE init_float(u: U; offset: ByteOffset; READONLY float: Target.Float) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("init_float");
      u.wr.Int   (offset);
      u.wr.Flt   (float);
      u.wr.NL    ();
    END;
    print (u, " /* init_float */ ");
  END init_float;

(*------------------------------------------------------------ PROCEDUREs ---*)

PROCEDURE import_procedure(u: U; name: Name; n_params: INTEGER;
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
      print (u, " /* import_procedure */ ");
    END;
    SuppressLineDirective(u, n_params, "import_procedure n_params");
    u.param_count := 0;
    u.function := proc;
    IF n_params = 0 THEN
      function_prototype(u, proc);
      print(u, ";");
    END;
    RETURN proc;
  END import_procedure;

PROCEDURE declare_procedure(u: U; name: Name; n_params: INTEGER;
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
      print (u, " /* declare_procedure */ ");
    END;
    SuppressLineDirective(u, n_params, "declare_procedure n_params");
    u.param_count := 0;
    u.function := proc;
    RETURN proc;
  END declare_procedure;

PROCEDURE begin_procedure(u: U; p: Proc) =
  VAR proc := NARROW(p, CProc);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("begin_procedure");
      u.wr.PName (proc);
      u.wr.NL    ();
    END;
    print (u, " /* begin_procedure */ ");
    INC(u.in_procedure);
    u.function := proc;
    function_prototype(u, proc);
    print(u, "{");
    WHILE proc.locals.size() > 0 DO
      print(u, proc.locals.remlo());
    END;
  END begin_procedure;

PROCEDURE end_procedure(u: U; p: Proc) =
  VAR proc := NARROW(p, CProc);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("end_procedure");
      u.wr.PName (proc);
      u.wr.NL    ();
    END;
    print (u, " /* end_procedure */ ");
    DEC(u.in_procedure);
    print(u, "}");
  END end_procedure;

PROCEDURE begin_block(u: U) =
  (* marks the beginning of a nested anonymous block *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("begin_block");
      u.wr.NL    ();
    END;
    print (u, " /* begin_block */ ");
    INC(u.in_block);
    print (u, "{");
  END begin_block;

PROCEDURE end_block(u: U) =
  (* marks the ending of a nested anonymous block *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("end_block");
      u.wr.NL    ();
    END;
    print (u, " /* end_block */ ");
    DEC(u.in_block);
    print (u, "}");
  END end_block;

PROCEDURE note_procedure_origin(u: U; p: Proc) =
  VAR proc := NARROW(p, CProc);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("note_procedure_origin");
      u.wr.PName (proc);
      u.wr.NL    ();
    END;
    print (u, " /* note_procedure_origin */ ");
  END note_procedure_origin;

(*------------------------------------------------------------ statements ---*)

PROCEDURE set_label(u: U; label: Label; <*UNUSED*> barrier: BOOLEAN) =
  (* define 'label' to be at the current pc *)
  BEGIN
    print (u, " /* set_label */ ");
    print(u, "L" & Fmt.Unsigned(label) & ":;");
  END set_label;

PROCEDURE jump(u: U; label: Label) =
  (* GOTO label *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("jump");
      u.wr.Lab   (label);
      u.wr.NL    ();
    END;
    print (u, " /* jump */ ");
    print(u, "goto L" & Fmt.Unsigned(label) & ";");
  END jump;

PROCEDURE if_true(u: U; itype: IType; label: Label; <*UNUSED*> frequency: Frequency) =
  (* IF (s0.itype # 0) GOTO label; pop *)
  VAR s0 := cast(get(u, 0), itype);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("if_true");
      u.wr.TName (itype);
      u.wr.Lab   (label);
      u.wr.NL    ();
      print (u, " /* if_true */ ");
    END;
    print(u, "if(" & s0 & ")goto L" & Fmt.Unsigned(label) & ";");
    pop(u);
  END if_true;

PROCEDURE if_false(u: U; itype: IType; label: Label; <*UNUSED*> frequency: Frequency) =
  (* IF (s0.itype = 0) GOTO label; pop *)
  VAR s0 := cast(get(u, 0), itype);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("if_false");
      u.wr.TName (itype);
      u.wr.Lab   (label);
      u.wr.NL    ();
      print (u, " /* if_false */ ");
    END;
    print(u, "if(!" & paren(s0) & ")goto L" & Fmt.Unsigned(label) & ";");
    pop(u);
  END if_false;

PROCEDURE if_compare(u: U; ztype: ZType; op: CompareOp; label: Label;
                     <*UNUSED*> frequency: Frequency) =
  (* IF (s1.ztype op s0.ztype) GOTO label; pop(2) *)
  VAR s0 := cast(get(u, 0), ztype);
      s1 := cast(get(u, 1), ztype);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("if_compare");
      u.wr.TName (ztype);
      u.wr.OutT  (" " & CompareOpName[op]);
      u.wr.Lab   (label);
      u.wr.NL    ();
      print(u, " /* if_compare */ ");
    END;
    pop(u, 2);
    print(u, "if(" & paren(s1) & CompareOpC[op] & paren(s0) & ")goto L" & Fmt.Unsigned(label) & ";");
  END if_compare;

PROCEDURE case_jump(u: U; itype: IType; READONLY labels: ARRAY OF Label) =
  (* "GOTO labels[s0.itype]; pop" with no range checking on s0.itype *)
  (*VAR s0 := get(u);*)
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
    print(u, " /* case_jump */ ");
    pop(u);
  END case_jump;

PROCEDURE exit_proc(u: U; type: Type) =
  (* Returns s0.type if type is not Void, otherwise returns no value. *)
  VAR s0: TEXT;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("exit_proc");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, " /* exit_proc */ ");
    IF type = Type.Void THEN
      print(u, "return;");
    ELSE
      s0 := get(u);
      IF type = Type.Addr THEN
        s0 := "(ADDRESS)" & s0;
      END;
      print(u, "return " & s0 & ";");
      pop(u);
    END;
  END exit_proc;

(*------------------------------------------------------------ load/store ---*)

PROCEDURE address_plus_offset(in: TEXT; in_offset: INTEGER): TEXT =
BEGIN
  IF in_offset # 0 THEN
    in := "(" & Fmt.Int(in_offset) & "+(ADDRESS)" & in & ")";
  END;
  RETURN in;
END address_plus_offset;

PROCEDURE load_helper(u: U; in: TEXT; in_offset: INTEGER; in_mtype: MType; out_ztype: ZType) =
  VAR text: TEXT := NIL;
  BEGIN
    <* ASSERT CG_Bytes[out_ztype] >= CG_Bytes[in_mtype] *>
    text := "*(volatile " & typeToText[in_mtype] & "*)" & address_plus_offset(in, in_offset);
    IF in_mtype # out_ztype THEN
      text := "((" & typeToText[out_ztype] & ")(" & text & "))";
    END;
    push(u, out_ztype, text);
  END load_helper;

PROCEDURE load(u: U; v: Var; offset: ByteOffset; mtype: MType; ztype: ZType) =
(* push; s0.ztype := Mem [ ADR(var) + offset ].mtype; The only allowed (mtype->ztype) conversions
   are {Int,Word}{8,16} -> {Int,Word}{32,64} and {Int,Word}32 -> {Int,Word}64.
   The source type, mtype, determines whether the value is sign-extended or
   zero-extended. *)
  VAR var := NARROW(v, CVar);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load");
      u.wr.VName (var);
      u.wr.Int   (offset);
      u.wr.TName (mtype);
      u.wr.TName (ztype);
      u.wr.NL    ();
    END;
    print(u, " /* load */ ");
    load_helper(u, "&" & M3ID.ToText(var.name), offset, mtype, ztype);
  END load;

PROCEDURE store_helper(u: U; in: TEXT; in_ztype: ZType; out_address: TEXT; out_offset: INTEGER; out_mtype: MType) =
  BEGIN
    <* ASSERT CG_Bytes[in_ztype] >= CG_Bytes[out_mtype] *>
    print(u, "(*(volatile " & typeToText[out_mtype] & "*)" & address_plus_offset(out_address, out_offset) & ")=(" & typeToText[in_ztype] & ")(" & in & ");");
  END store_helper;

PROCEDURE store(u: U; v: Var; offset: ByteOffset; ztype: ZType; mtype: MType) =
(* Mem [ ADR(var) + offset ].mtype := s0.ztype; pop *)
  VAR var := NARROW(v, CVar);
      s0 := cast(get(u, 0), ztype);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("store");
      u.wr.VName (var);
      u.wr.Int   (offset);
      u.wr.TName (ztype);
      u.wr.TName (mtype);
      u.wr.NL    ();
    END;
    print(u, " /* store */ ");
    pop(u);
    store_helper(u, s0, ztype, "&" & M3ID.ToText(var.name), offset, mtype);
  END store;

PROCEDURE load_address(u: U; v: Var; offset: ByteOffset) =
(* push; s0.A := ADR(var) + offset *)
  VAR var := NARROW(v, CVar);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_address");
      u.wr.VName (var);
      u.wr.Int   (offset);
      u.wr.NL    ();
    END;
    print(u, " /* load_address */ ");
    push(u, Type.Addr, address_plus_offset("&" & M3ID.ToText (var.name), offset));
  END load_address;

PROCEDURE load_indirect(u: U; offset: ByteOffset; mtype: MType; ztype: ZType) =
(* s0.ztype := Mem [s0.A + offset].mtype  *)
  VAR s0 := get(u);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_indirect");
      u.wr.Int   (offset);
      u.wr.TName (mtype);
      u.wr.TName (ztype);
      u.wr.NL    ();
    END;
    print(u, " /* load_indirect */ ");
    <* ASSERT CG_Bytes[ztype] >= CG_Bytes[mtype] *>
    pop(u);
    load_helper(u, s0, offset, mtype, ztype);
  END load_indirect;

PROCEDURE store_indirect(u: U; offset: ByteOffset; ztype: ZType; mtype: MType) =
(* Mem [s1.A + offset].mtype := s0.ztype; pop (2) *)
  VAR s0 := cast(get(u, 0), ztype);
      s1 := get(u, 1);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("store_indirect");
      u.wr.Int   (offset);
      u.wr.TName (ztype);
      u.wr.TName (mtype);
      u.wr.NL    ();
    END;
    print(u, " /* store_indirect */ ");
    pop(u, 2);
    store_helper(u, s0, ztype, s1, offset, mtype);
  END store_indirect;

(*-------------------------------------------------------------- literals ---*)

PROCEDURE load_nil(u: U) =
  (* push; s0.A := NIL *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_nil");
      u.wr.NL    ();
    END;
    print(u, " /* load_nil */ ");
    push(u, Type.Addr, "0"); (* UNDONE NULL or (ADDRESS)0? *)
  END load_nil;

PROCEDURE load_integer(u: U; type: IType; READONLY i: Target.Int) =
  (* push; s0.type := i *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_integer");
      u.wr.TName (type);
      u.wr.TInt  (TIntN.FromTargetInt(i, CG_Bytes[type])); (* UNDONE? *)
      u.wr.NL    ();
    END;
    print(u, " /* load_integer */ ");
    (* TODO: use suffixes L, U, UL, ULL, i64, ui64 via #ifdef and macro *)
    push(u, type, "((" & typeToText[type] & ")" & TInt.ToText(i) & ")");
  END load_integer;

PROCEDURE load_float(u: U; type: RType; READONLY float: Target.Float) =
  (* push; s0.type := float *)
  VAR buffer: ARRAY [0..BITSIZE(EXTENDED)] OF CHAR;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_float");
      u.wr.TName (type);
      u.wr.Flt   (float);
      u.wr.NL    ();
    END;
    print(u, " /* load_float */ ");
    (* TODO: use suffixes *)
    push(u, type, "((" & typeToText[type] & ")" & Text.FromChars(SUBARRAY(buffer, 0, TFloat.ToChars(float, buffer))) & ")");
  END load_float;

(*------------------------------------------------------------ arithmetic ---*)

PROCEDURE cast(expr: TEXT; type: Type): TEXT =
BEGIN
  RETURN paren("(" & typeToText[type] & ")" & paren(expr));
END cast;

PROCEDURE compare(u: U; ztype: ZType; itype: IType; op: CompareOp) =
  (* s1.itype := (s1.ztype op s0.ztype); pop *)
  VAR s0 := cast(get(u, 0), ztype);
      s1 := cast(get(u, 1), ztype);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("compare");
      u.wr.TName (ztype);
      u.wr.TName (itype);
      u.wr.OutT  (" " & CompareOpName[op]);
      u.wr.NL    ();
      print(u, " /* compare */ ");
    END;
    (* ASSERT cond # Cond.Z AND cond # Cond.NZ *)
      pop(u);
      push(u, itype, cast(s1 & CompareOpC[op] & s0, itype));
  END compare;

PROCEDURE add(u: U; type: AType) =
  (* s1.type := s1.type + s0.type; pop *)
  VAR s0 := cast(get(u, 0), type);
      s1 := cast(get(u, 1), type);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("add");
      u.wr.TName (type);
      u.wr.NL    ();
      print(u, " /* add */ ");
    END;
    pop(u, 2);
    push(u, type, cast(s1 & "+" & s0, type));
  END add;

PROCEDURE subtract(u: U; type: AType) =
  (* s1.type := s1.type - s0.type; pop *)
  VAR s0 := cast(get(u, 0), type);
      s1 := cast(get(u, 1), type);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("subtract");
      u.wr.TName (type);
      u.wr.NL    ();
      print(u, " /* subtract */ ");
    END;
    pop(u, 2);
    push(u, type, cast(s1 & "-" & s0, type));
  END subtract;

PROCEDURE multiply(u: U; type: AType) =
  (* s1.type := s1.type * s0.type; pop *)
  VAR s0 := cast(get(u, 0), type);
      s1 := cast(get(u, 1), type);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("multiply");
      u.wr.TName (type);
      u.wr.NL    ();
      print(u, " /* multiply */ ");
    END;
    pop(u, 2);
    push(u, type, cast(s1 & "*" & s0, type));
  END multiply;

PROCEDURE divide(u: U; type: RType) =
  (* s1.type := s1.type / s0.type; pop *)
  VAR s0 := cast(get(u, 0), type);
      s1 := cast(get(u, 1), type);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("divide");
      u.wr.TName (type);
      u.wr.NL    ();
      print(u, " /* divide */ ");
    END;
    pop(u, 2);
    push(u, type, cast(s1 & "/" & s0, type));
  END divide;

CONST SignName = ARRAY Sign OF TEXT { " P", " N", " X" };

PROCEDURE div(u: U; type: IType; a, b: Sign) =
  (* s1.type := s1.type DIV s0.type; pop *)
  VAR s0 := cast(get(u, 0), type);
      s1 := cast(get(u, 1), type);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("div");
      u.wr.TName (type);
      u.wr.OutT  (SignName[a]);
      u.wr.OutT  (SignName[b]);
      u.wr.NL    ();
      print(u, " /* div */ ");
    END;
    pop(u, 2);
    push(u, type, cast(s1 & "/" & s0, type));
  END div;

PROCEDURE mod(u: U; type: IType; a, b: Sign) =
  (* s1.type := s1.type MOD s0.type; pop *)
  VAR s0 := cast(get(u, 0), type);
      s1 := cast(get(u, 1), type);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("mod");
      u.wr.TName (type);
      u.wr.OutT  (SignName [a]);
      u.wr.OutT  (SignName [b]);
      u.wr.NL    ();
      print(u, " /* mod */ ");
    END;
    pop(u, 2);
    push(u, type, cast(s1 & "%" & s0, type));
  END mod;

PROCEDURE negate(u: U; type: AType) =
  (* s0.type := - s0.type *)
  VAR s0 := cast(get(u, 0), type);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("negate");
      u.wr.TName (type);
      u.wr.NL    ();
      print(u, " /* negate */ ");
    END;
    pop(u);
    push(u, type, cast("-" & s0, type));
  END negate;

PROCEDURE abs(u: U; type: AType) =
  (* s0.type := ABS (s0.type) (noop on Words) *)
  VAR s0 := cast(get(u, 0), type);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("abs");
      u.wr.TName (type);
      u.wr.NL    ();
      print(u, " /* abs */");
    END;
    pop(u);
    push(u, type, cast("m3_abs_" & typeToText[type] & "(" & s0 & ")", type));
  END abs;

PROCEDURE max(u: U; type: ZType) =
  (* s1.type := MAX (s1.type, s0.type); pop *)
  VAR s0 := cast(get(u, 0), type);
      s1 := cast(get(u, 1), type);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("max");
      u.wr.TName (type);
      u.wr.NL    ();
      print(u, " /* max */ ");
    END;
    pop(u, 2);
    push(u, type, cast("m3_max_" & typeToText[type] & "(" & s0 & "," & s1 & ")", type));
  END max;

PROCEDURE min(u: U; type: ZType) =
  (* s1.type := MIN (s1.type, s0.type); pop *)
  VAR s0 := cast(get(u, 0), type);
      s1 := cast(get(u, 1), type);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("min");
      u.wr.TName (type);
      u.wr.NL    ();
      print(u, " /* min */ ");
    END;
    pop(u, 2);
    push(u, type, cast("m3_min_" & typeToText[type] & "(" & s0 & "," & s1 & ")", type));
  END min;

PROCEDURE cvt_int(u: U; from_float_type: RType; to_integer_type: IType; op: ConvertOp) =
  (* s0.itype := ROUND(s0.rtype) *)
  VAR s0 := cast(get(u, 0), from_float_type);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("cvt_int");
      u.wr.TName (from_float_type);
      u.wr.TName (to_integer_type);
      u.wr.OutT  (" " & ConvertOpName[op]);
      u.wr.NL    ();
      print(u, " /* cvt_int */ ");
    END;
    pop(u);
    push(u, to_integer_type, cast("m3_" & ConvertOpName[op] & "(" & s0 & ")", to_integer_type));
  END cvt_int;

PROCEDURE cvt_float(u: U; from_arithmetic_type: AType; to_float_type: RType) =
  (* s0.rtype := ROUND(s0.atype) *)
  VAR s0 := cast(get(u, 0), from_arithmetic_type);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("cvt_float");
      u.wr.TName (from_arithmetic_type);
      u.wr.TName (to_float_type);
      u.wr.NL    ();
      print(u, " /* cvt_float */ ");
    END;
    (* UNDONE is this correct? *)
    pop(u);
    push(u, to_float_type, cast(s0, to_float_type));
  END cvt_float;

(*------------------------------------------------------------------ sets ---*)

PROCEDURE set_op3(u: U; byte_size: ByteSize; op: TEXT) =
  (* s2.B := s1.B op s0.B; pop(3) *)
  VAR s0 := cast(get(u, 0), Type.Addr);
      s1 := cast(get(u, 1), Type.Addr);
      s2 := cast(get(u, 1), Type.Addr);
  BEGIN
    IF u.debug THEN
      (*u.wr.Cmd   (BuiltinDesc[builtin].name);*)
      u.wr.Int   (byte_size);
      u.wr.NL    ();
      print(u, " /* " & op & " */ ");
    END;
    pop(u, 3);
    print(u, op & "(" & s2 & "," & s1 & "," & s0 & ")");
  END set_op3;

PROCEDURE set_union(u: U; byte_size: ByteSize) =
  (* s2.B := s1.B + s0.B; pop(2) *)
  BEGIN
    set_op3(u, byte_size, "set_union");
  END set_union;

PROCEDURE set_difference(u: U; byte_size: ByteSize) =
  (* s2.B := s1.B - s0.B; pop(2) *)
  BEGIN
    set_op3(u, byte_size, "set_difference");
  END set_difference;

PROCEDURE set_intersection(u: U; byte_size: ByteSize) =
  (* s2.B := s1.B * s0.B; pop(2) *)
  BEGIN
    set_op3(u, byte_size, "set_intersection");
  END set_intersection;

PROCEDURE set_sym_difference(u: U; byte_size: ByteSize) =
  (* s2.B := s1.B / s0.B; pop(2) *)
  BEGIN
    set_op3(u, byte_size, "set_sym_difference");
  END set_sym_difference;

PROCEDURE set_member(u: U; byte_size: ByteSize; type: IType) =
  (* s1.type := (s0.type IN s1.B); pop *)
  VAR s0 := cast(get(u, 0), type);
      s1 := cast(get(u, 1), Type.Addr);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("set_member");
      u.wr.Int   (byte_size);
      u.wr.TName (type);
      u.wr.NL    ();
      print(u, " /* set_member */ ");
    END;
    pop(u, 2);
    push(u, type, cast("set_member(" & s0 & "," & s1 & ")", type));
  END set_member;

PROCEDURE set_compare(u: U; byte_size: ByteSize; op: CompareOp; type: IType) =
  (* s1.type := (s1.B op s0.B); pop *)
  VAR s0 := cast(get(u, 0), Type.Addr);
      s1 := cast(get(u, 1), Type.Addr);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("set_compare");
      u.wr.Int   (byte_size);
      u.wr.OutT  (" " & CompareOpName[op]);
      u.wr.TName (type);
      u.wr.NL    ();
      print(u, " /* set_compare */ ");
    END;
    pop(u, 2);
    push(u, type, cast("m3_set_" & CompareOpName[op] & "(" & s1 & "," & s0 & ")", type));
  END set_compare;

PROCEDURE set_range(u: U; byte_size: ByteSize; type: IType) =
  (* s2.A [s1.type .. s0.type] := 1's; pop(3) *)
  VAR s0 := get(u, 0);
      s1 := get(u, 1);
      s2 := get(u, 2);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("set_range");
      u.wr.Int   (byte_size);
      u.wr.TName (type);
      u.wr.NL    ();
      print(u, " /* set_range */ ");
    END;
    pop(u, 2);
    push(u, type, "m3_set_range(" & s2 & s1 & "," & s0 & ")");
  END set_range;

PROCEDURE set_singleton(u: U; byte_size: ByteSize; type: IType) =
  (* s1.A [s0.type] := 1; pop(2) *)
  VAR s0 := get(u, 0);
      s1 := get(u, 1);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("set_singleton");
      u.wr.Int   (byte_size);
      u.wr.TName (type);
      u.wr.NL    ();
      print(u, " /* set_singleton */ ");
    END;
    pop(u, 2);
    push(u, type, "m3_set_singleton(" & s0 & "," & s1 & ")");
  END set_singleton;

(*------------------------------------------------- Word.T bit operations ---*)

PROCEDURE not(u: U; type: IType) =
  (* s0.type := Word.Not (s0.type) *)
  VAR s0 := get(u);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("not");
      u.wr.TName (type);
      u.wr.NL    ();
      print(u, " /* not */ ");
    END;
    pop(u, 2);
    push(u, type, cast("~" & cast(s0, type), type));
  END not;

PROCEDURE and(u: U; type: IType) =
  (* s1.type := Word.And (s1.type, s0.type); pop *)
  VAR s0 := get(u, 0);
      s1 := get(u, 1);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("and");
      u.wr.TName (type);
      u.wr.NL    ();
      print(u, " /* and */ ");
    END;
    pop(u, 2);
    push(u, type, cast(cast(s1, type) & "&" & cast(s0, type), type));
  END and;

PROCEDURE or(u: U; type: IType) =
  (* s1.type := Word.Or  (s1.type, s0.type); pop *)
  VAR s0 := get(u, 0);
      s1 := get(u, 1);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("or");
      u.wr.TName (type);
      u.wr.NL    ();
      print(u, " /* or */ ");
    END;
    pop(u, 2);
    push(u, type, cast(cast(s1, type) & "|" & cast(s0, type), type));
  END or;

PROCEDURE xor(u: U; type: IType) =
  (* s1.type := Word.Xor (s1.type, s0.type); pop *)
  VAR s0 := get(u, 0);
      s1 := get(u, 1);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("xor");
      u.wr.TName (type);
      u.wr.NL    ();
      print(u, " /* xor */ ");
    END;
    pop(u, 2);
    push(u, type, cast(cast(s1, type) & "^" & cast(s0, type), type));
  END xor;

PROCEDURE shift_left(u: U; type: IType) =
  (* s1.type := Word.Shift  (s1.type, s0.type); pop *)
  VAR s0 := get(u, 0);
      s1 := get(u, 1);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("shift_left");
      u.wr.TName (type);
      u.wr.NL    ();
      print(u, " /* shift_left */ ");
    END;
    pop(u, 2);
    push(u, type, cast(cast(s1, type) & "<<" & cast(s0, type), type));
  END shift_left;

PROCEDURE shift_right(u: U; type: IType) =
  (* s1.type := Word.Shift  (s1.type, -s0.type); pop *)
  VAR s0 := get(u, 0);
      s1 := get(u, 1);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("shift_right");
      u.wr.TName (type);
      u.wr.NL    ();
      print(u, " /* shift_right */ ");
    END;
    pop(u, 2);
    push(u, type, cast(cast(s1, type) & ">>" & cast(s0, type), type));
  END shift_right;

PROCEDURE shift(u: U; type: IType) =
  (* s1.type := Word.Shift  (s1.type, s0.type); pop *)
  VAR s0 := get(u, 0);
      s1 := get(u, 1);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("shift");
      u.wr.TName (type);
      u.wr.NL    ();
      print(u, " /* shift */ ");
    END;
    pop(u, 2);
    push(u, type, "m3_shift_" & typeToText[type] & "(" & s1 & "," & s0 & ")");
  END shift;

PROCEDURE rotate(u: U; type: IType) =
  (* s1.type := Word.Rotate (s1.type, s0.type); pop *)
  VAR s0 := get(u, 0);
      s1 := get(u, 1);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("rotate");
      u.wr.TName (type);
      u.wr.NL    ();
      print(u, " /* rotate */ ");
    END;
    pop(u, 2);
    push(u, type, "m3_rotate_" & typeToText[type] & "(" & s1 & "," & s0 & ")");
  END rotate;

PROCEDURE rotate_left(u: U; type: IType) =
  (* s1.type := Word.Rotate (s1.type, s0.type); pop *)
  VAR s0 := get(u, 0);
      s1 := get(u, 1);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("rotate_left");
      u.wr.TName (type);
      u.wr.NL    ();
      print(u, " /* rotate_left */ ");
    END;
    pop(u, 2);
    push(u, type, "m3_rotate_left" & typeToText[type] & "(" & s1 & "," & s0 & ")");
  END rotate_left;

PROCEDURE rotate_right(u: U; type: IType) =
  (* s1.type := Word.Rotate (s1.type, -s0.type); pop *)
  VAR s0 := get(u, 0);
      s1 := get(u, 1);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("rotate_right");
      u.wr.TName (type);
      u.wr.NL    ();
      print(u, " /* rotate_right */ ");
    END;
    pop(u, 2);
    push(u, type, "m3_rotate_right" & typeToText[type] & "(" & s1 & "," & s0 & ")");
  END rotate_right;

PROCEDURE widen(u: U; sign_extend: BOOLEAN) =
  (* s0.I64 := s0.I32; IF sign_extend THEN SignExtend s0; *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("widen");
      u.wr.Bool  (sign_extend);
      u.wr.NL    ();
    END;
    <*ASSERT FALSE*>
  END widen;

PROCEDURE chop(u: U) =
  (* s0.I32 := Word.And (s0.I64, 16_ffffffff); *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("chop");
      u.wr.NL    ();
    END;
    <*ASSERT FALSE*>
  END chop;

PROCEDURE extract(u: U; type: IType; sign_extend: BOOLEAN) =
  (* s2.type := Word.Extract(s2.type, s1.type, s0.type);
     IF sign_extend THEN SignExtend s2 END; pop(2) *)
  VAR s0 := get(u, 0);
      s1 := get(u, 1);
      s2 := get(u, 2);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("extract");
      u.wr.TName (type);
      u.wr.Bool  (sign_extend);
      u.wr.NL    ();
      print(u, " /* extract */ ");
    END;
    pop(u, 3);
    <* ASSERT sign_extend = FALSE *>
    push(u, type, "m3_extract_" & typeToText[type] & "(" & s2 & "," & s1 & "," & s0 & ")");
  END extract;

PROCEDURE extract_n(u: U; type: IType; sign_extend: BOOLEAN; n: CARDINAL) =
  (* s1.type := Word.Extract(s1.type, s0.type, n);
     IF sign_extend THEN SignExtend s1 END; pop(1) *)
  VAR s0 := get(u, 0);
      s1 := get(u, 1);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("extract_n");
      u.wr.TName (type);
      u.wr.Bool  (sign_extend);
      u.wr.Int   (n);
      u.wr.NL    ();
      print(u, " /* extract_m */ ");
    END;
    pop(u, 2);
    <* ASSERT sign_extend = FALSE *>
    push(u, type, "m3_extract_" & typeToText[type] & "(" & s1 & "," & s0 & "," & Fmt.Int(n) & ")");
  END extract_n;

PROCEDURE extract_mn(u: U; type: IType; sign_extend: BOOLEAN; m, n: CARDINAL) =
  (* s0.type := Word.Extract(s0.type, m, n);
     IF sign_extend THEN SignExtend s0 END; *)
  VAR s0 := get(u);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("extract_mn");
      u.wr.TName (type);
      u.wr.Bool  (sign_extend);
      u.wr.Int   (m);
      u.wr.Int   (n);
      u.wr.NL    ();
      print(u, " /* extract_mn */ ");
    END;
    pop(u);
    s0 := "m3_extract_" & typeToText[type] & "(" & s0 & "," & Fmt.Int(m) & "," & Fmt.Int(n) & ")";
    IF sign_extend THEN
      s0 := "m3_signextend_" & typeToText[type] & "(" & s0 & ")";
    END;
    push(u, type, s0);
  END extract_mn;

PROCEDURE insert(u: U; type: IType) =
  (* s3.type := Word.Insert (s3.type, s2.type, s1.type, s0.type); pop(3) *)
  VAR s0 := get(u, 0);
      s1 := get(u, 1);
      s2 := get(u, 2);
      s3 := get(u, 3);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("insert");
      u.wr.TName (type);
      u.wr.NL    ();
      print(u, " /* insert */ ");
    END;
    pop(u, 4);
    push(u, type, "m3_insert_" & typeToText[type] & "(" & s3 & "," & s2 & "," & s1 & "," & s0 & ")");
  END insert;

PROCEDURE insert_n(u: U; type: IType; n: CARDINAL) =
  (* s2.type := Word.Insert (s2.type, s1.type, s0.type, n); pop(2) *)
  VAR s0 := get(u, 0);
      s1 := get(u, 1);
      s2 := get(u, 2);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("insert_n");
      u.wr.TName (type);
      u.wr.Int   (n);
      u.wr.NL    ();
      print(u, " /* insert_n */ ");
    END;
    pop(u, 3);
    push(u, type, "m3_insert_" & typeToText[type] & "(" & s2 & "," & "," & s1 & "," & s0 & "," & Fmt.Int(n) & ")");
  END insert_n;

PROCEDURE insert_mn(u: U; type: IType; m, n: CARDINAL) =
  (* s1.type := Word.Insert (s1.type, s0.type, m, n); pop(2) *)
  VAR s0 := get(u, 0);
      s1 := get(u, 1);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("insert_mn");
      u.wr.TName (type);
      u.wr.Int   (m);
      u.wr.Int   (n);
      u.wr.NL    ();
      print(u, " /* insert_mn */ ");
    END;
    pop(u, 2);
    push(u, type, "m3_insert_" & typeToText[type] & "(" & s1 & "," & s0 & "," & Fmt.Int(m) & "," & Fmt.Int(n) & ")");
  END insert_mn;

(*------------------------------------------------ misc. stack/memory ops ---*)

PROCEDURE swap(u: U; a, b: Type) =
  (* tmp := s1; s1 := s0; s0 := tmp *)
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

PROCEDURE cg_pop(u: U; type: Type) =
  (* pop(1) (i.e. discard s0) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("pop");
      u.wr.TName (type);
      u.wr.NL    ();
      print(u, " /* pop */ ");
    END;
    pop(u);
  END cg_pop;

PROCEDURE copy_n(u: U; itype: IType; mtype: MType; overlap: BOOLEAN) =
  (* Mem[s2.A:s0.ztype] := Mem[s1.A:s0.ztype]; pop(3)*)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("copy_n");
      u.wr.TName (itype);
      u.wr.TName (mtype);
      u.wr.Bool  (overlap);
      u.wr.NL    ();
      print(u, " /* copy_n */ ");
    END;
    (* UNDONE *)
  END copy_n;

PROCEDURE copy(u: U; n: INTEGER; type: MType; overlap: BOOLEAN) =
  (* Mem[s1.A:sz] := Mem[s0.A:sz]; pop(2)*)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("copy");
      u.wr.Int   (n);
      u.wr.TName (type);
      u.wr.Bool  (overlap);
      u.wr.NL    ();
      print(u, " /* copy */ ");
    END;
    (* UNDONE *)
  END copy;

PROCEDURE zero_n(u: U; itype: IType; mtype: MType) =
  (* Mem[s1.A:s0.itype] := 0; pop(2) *)
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

PROCEDURE zero(u: U; n: INTEGER; type: MType) =
  (* Mem[s0.A:sz] := 0; pop(1) *)
  VAR s0 := get(u, 0);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("zero");
      u.wr.Int   (n);
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    pop(u);
    print(u, " /* zero */ ");
    print(u, "memset(" & s0 & ",0," & Fmt.Int(n) & "*" & Fmt.Int(CG_Bytes[type]) & ");");
  END zero;

(*----------------------------------------------------------- conversions ---*)

PROCEDURE loophole(u: U; from, to: ZType) =
  (* s0.to := LOOPHOLE(s0.from, to) *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("loophole");
      u.wr.TName (from);
      u.wr.TName (to);
      u.wr.NL    ();
    END;
    print(u, " /* loophole */ ");
    (* If type is already a pointer, then we should not add pointer here.
     * As well, if type does not contain a pointer, then we should store the
     * value in a non-stack-packed temporary and use its address.
     * We don't have code to queue up temporary declarations.
     * (for that matter, to limit/reuse temporaries)
     *)
    u.stack.put(0, "*(" & typeToText[to] & "*)&" & get(u, 0));
  END loophole;

(*------------------------------------------------ traps & runtime checks ---*)

PROCEDURE abort(u: U; code: RuntimeError) =
  VAR t: TEXT;
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("abort");
      u.wr.Int   (ORD (code));
      u.wr.NL    ();
      print(u, " /* abort */ ");
    END;
    reportfault(u, code);
  END abort;

PROCEDURE check_nil(u: U; code: RuntimeError) =
  (* IF (s0.A = NIL) THEN abort(code) *)
  VAR s0 := get(u);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("check_nil");
      u.wr.Int   (ORD (code));
      u.wr.NL    ();
      print(u, " /* check_nil */ ");
    END;
    print(u, "if(!" & paren(s0) & ")");
    reportfault(u, code);
  END check_nil;

PROCEDURE check_lo(u: U; type: IType; READONLY i: Target.Int; code: RuntimeError) =
  (* IF (s0.type < i) THEN abort(code) *)
  VAR s0 := cast(get(u), type);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("check_lo");
      u.wr.TName (type);
      u.wr.TInt  (TIntN.FromTargetInt(i, CG_Bytes[type]));
      u.wr.Int   (ORD (code));
      u.wr.NL    ();
      print(u, " /* check_lo */ ");
    END;
    print(u, "if(" & paren(s0) & "<" & TInt.ToText(i) & ")");
    reportfault(u, code);
  END check_lo;

PROCEDURE check_hi(u: U; type: IType; READONLY i: Target.Int; code: RuntimeError) =
  (* IF (i < s0.type) THEN abort(code) *)
  VAR s0 := cast(get(u), type);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("check_hi");
      u.wr.TName (type);
      u.wr.TInt  (TIntN.FromTargetInt(i, CG_Bytes[type]));
      u.wr.Int   (ORD (code));
      u.wr.NL    ();
      print(u, " /* check_hi */ ");
    END;
    print(u, "if(" & TInt.ToText(i) & "<" & paren(s0) & ")");
    reportfault(u, code);
  END check_hi;

PROCEDURE check_range(u: U; type: IType; READONLY a, b: Target.Int; code: RuntimeError) =
  (* IF (s0.type < a) OR (b < s0.type) THEN abort(code) *)
  VAR s0 := cast(get(u), type);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("check_range");
      u.wr.TInt  (TIntN.FromTargetInt(a, CG_Bytes[type]));
      u.wr.TInt  (TIntN.FromTargetInt(b, CG_Bytes[type]));
      u.wr.Int   (ORD(code));
      u.wr.NL    ();
      print(u, " /* check_range */ ");
    END;
    print(u, "if(" & paren(s0) & "<" & TInt.ToText(a) & "||" & TInt.ToText(b) & "<"  & paren(s0) & ")");
    reportfault(u, code);
  END check_range;

PROCEDURE check_index(u: U; type: IType; code: RuntimeError) =
  (* IF NOT (0 <= s1.type < s0.type) THEN
       abort(code)
     END;
     pop *)
  (* s0.type is guaranteed to be positive so the unsigned
     check (s0.W <= s1.W) is sufficient. *)
  VAR s0 := cast(get(u, 0), type);
      s1 := cast(get(u, 1), type);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("check_index");
      u.wr.TName (type);
      u.wr.Int   (ORD(code));
      u.wr.NL    ();
      print(u, " /* check_index */ ");
    END;
    print(u, "if(" & paren(s0) & "<=" & paren(s1) & ")");
    reportfault(u, code);
    pop(u);
  END check_index;

PROCEDURE check_eq(u: U; type: IType; code: RuntimeError) =
  (* IF (s0.type # s1.type) THEN
       abort(code);
       Pop (2) *)
  VAR s0 := cast(get(u, 0), type);
      s1 := cast(get(u, 1), type);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("check_eq");
      u.wr.TName (type);
      u.wr.Int   (ORD(code));
      u.wr.NL    ();
      print(u, " /* check_eq */ ");
    END;
    print(u, "if(" & paren(s0) & "!=" & paren(s1) & ")");
    reportfault(u, code);
  END check_eq;

PROCEDURE reportfault(u: U; code: RuntimeError) =
  (* 32: see M3CG.RuntimeError, RuntimeError.T *)
  VAR info := ORD (code) + u.line * 32;
  BEGIN
    <* ASSERT ORD (code) < 32 *> (* lose fault code not ok *)
    (* ASSERT u.line <= (LAST(INTEGER) DIV 32) *) (* losing line number ok *)
    print(u, u.report_fault & "(" & Fmt.Int(info) & ");");
  END reportfault;

(*---------------------------------------------------- address arithmetic ---*)

PROCEDURE add_offset(u: U; offset: INTEGER) =
  (* s0.A := s0.A + offset *)
  VAR s0 := cast(get(u, 0), Type.Addr);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("add_offset");
      u.wr.Int   (offset);
      u.wr.NL    ();
      print(u, " /* add_offset */ ");
    END;
    pop(u);
    push(u, Type.Addr, address_plus_offset(s0, offset));
  END add_offset;

PROCEDURE index_address(u: U; type: IType; size: INTEGER) =
  (* s1.A := s1.A + s0.type * size; pop *)
  VAR s0 := cast(get(u, 0), type);
      s1 := cast(get(u, 1), Type.Addr);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("index_address");
      u.wr.TName (type);
      u.wr.Int   (size);
      u.wr.NL    ();
      print(u, " /* index_address */ ");
    END;
    IF size = 0 THEN
      pop(u);
      <* ASSERT FALSE *>
    ELSE
      pop(u, 2);
      push(u, Type.Addr, "(((ADDRESS)" & paren(s1) & ") + (" & Fmt.Int(size) & "*" & paren(s0) & "))");
    END;
  END index_address;

(*------------------------------------------------------- PROCEDURE calls ---*)

PROCEDURE start_call(u: U) =
  BEGIN
    u.param_comma := "";
    u.static_link[u.in_proc_call] := NIL;
    INC(u.in_proc_call);
  END start_call;

PROCEDURE start_call_direct(u: U; p: Proc; level: INTEGER; type: Type) =
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
    print(u, " /* start_call_direct */ ");
    start_call(u);
    print(u, M3ID.ToText(proc.name) &  "(" (* ) *) );
  END start_call_direct;

PROCEDURE start_call_indirect(u: U; type: Type; callingConvention: CallingConvention) =
  (* begin a procedure call to a procedure at static level 'level'. *)
  VAR s0 := get(u, 0);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("start_call_indirect");
      u.wr.TName (type);
      u.wr.Txt   (callingConvention.name);
      u.wr.NL    ();
    END;
    print(u, " /* start_call_indirect */ ");
    start_call(u);
    print(u, s0 &  "(" (* ) *) );
  END start_call_indirect;

PROCEDURE pop_param(u: U; type: MType) =
  (* pop s0 and make it the "next" parameter in the current call *)
  VAR s0 := cast(get(u, 0), type);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("pop_param");
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, " /* pop_param */ ");
    print(u, u.param_comma);
    print(u, s0);
    u.param_comma := ",";
  END pop_param;

PROCEDURE pop_struct(u: U; typeid: TypeUID; byte_size: ByteSize; alignment: Alignment) =
  (* pop s0 and make it the "next" parameter in the current call
   * NOTE: it is passed by value *)
  VAR s0 := get(u, 0);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("pop_struct");
      u.wr.Tipe  (typeid);
      u.wr.Int   (byte_size);
      u.wr.Int   (alignment);
      u.wr.NL    ();
    END;
    print(u, " /* pop_struct */ ");
    print(u, u.param_comma);
    print(u, s0);
    u.param_comma := ",";
  END pop_struct;

PROCEDURE pop_static_link(u: U) =
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("pop_static_link");
      u.wr.NL    ();
    END;
    print(u, " /* pop_static_link */ ");
    (* UNDONE *)
  END pop_static_link;

PROCEDURE call_direct(u: U; p: Proc; type: Type) =
  (* call the procedure identified by Proc p. The procedure
     returns a value of type type. *)
  VAR proc := NARROW(p, CProc);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("call_direct");
      u.wr.PName (proc);
      u.wr.TName (type);
      u.wr.NL    ();
    END;
    print(u, " /* call_direct */ ");
    print(u, ")");
    DEC(u.in_proc_call);
  END call_direct;

PROCEDURE call_indirect(u: U; type: Type; callingConvention: CallingConvention) =
  (* call the procedure whose address is in s0.A and pop s0. The
     procedure returns a value of type type. *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("call_indirect");
      u.wr.TName (type);
      u.wr.Txt   (callingConvention.name);
      u.wr.NL    ();
    END;
    print(u, " /* call_indirect */ ");
    DEC(u.in_proc_call);
    print(u, ")");
  END call_indirect;

(*------------------------------------------- procedure and closure types ---*)

PROCEDURE load_procedure(u: U; p: Proc) =
  (* push; s0.A := ADDR (proc's body) *)
  VAR proc := NARROW(p, CProc);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_procedure");
      u.wr.PName (proc);
      u.wr.NL    ();
    END;
    print(u, " /* load_procedure */ ");
    push(u, Type.Addr, "0/*UNDONE*/");
    (* UNDONE *)
  END load_procedure;

PROCEDURE load_static_link(u: U; p: Proc) =
  (* push; s0.A := (static link needed to call proc, NIL for top-level procs) *)
  VAR proc := NARROW(p, CProc);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_static_link");
      u.wr.PName (proc);
      u.wr.NL    ();
    END;
    print(u, " /* load_static_link */ ");
    push(u, Type.Addr, "0/*UNDONE*/");
    (* UNDONE *)
  END load_static_link;

(*----------------------------------------------------------------- misc. ---*)

PROCEDURE comment(u: U; a, b, c, d: TEXT := NIL) =
  VAR i: INTEGER := -1;
  BEGIN
    Cmt2 (u, a, i);
    Cmt2 (u, b, i);
    Cmt2 (u, c, i);
    Cmt2 (u, d, i);
    Cmt1 (u, "\n", i);
  END comment;

PROCEDURE Cmt1(u: U; text: TEXT; VAR width: INTEGER) =
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

PROCEDURE Cmt2(u: U; text: TEXT; VAR width: INTEGER) =
  BEGIN
    IF (NOT u.debug OR text = NIL) THEN RETURN END;
    IF text = NIL THEN
      RETURN
    END;
    Cmt1 (u, text, width);
    print(u, " /* comment: " & text & " */\n");
  END Cmt2;

(*--------------------------------------------------------------- atomics ---*)

PROCEDURE store_ordered(u: U; ztype: ZType; mtype: MType; <*UNUSED*>order: MemoryOrder) =
(* Mem [s1.A].mtype := s0.ztype;
   pop (2) *)
  VAR s0 := get(u, 0);
      s1 := get(u, 1);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("store_ordered");
      u.wr.TName (ztype);
      u.wr.TName (mtype);
      u.wr.NL    ();
    END;
    print(u, " /* store_ordered => store */ ");
    store_helper(u, s0, ztype, s1, 0, mtype);
  END store_ordered;

PROCEDURE load_ordered(u: U; mtype: MType; ztype: ZType; <*UNUSED*>order: MemoryOrder) =
(* s0.ztype := Mem [s0.A].mtype  *)
  VAR s0 := get(u);
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("load_ordered");
      u.wr.TName (mtype);
      u.wr.TName (ztype);
      u.wr.NL    ();
    END;
    print(u, " /* load_ordered */ ");
    pop(u);
    load_helper(u, s0, 0, mtype, ztype);
  END load_ordered;

PROCEDURE exchange(u: U; mtype: MType; ztype: ZType; <*UNUSED*>order: MemoryOrder) =
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
    print(u, " /* exchange */ ");
  END exchange;

PROCEDURE compare_exchange(u: U; mtype: MType; ztype: ZType; result_type: IType;
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
    print(u, " /* compare_exchange */ ");
  END compare_exchange;

PROCEDURE fence(u: U; <*UNUSED*>order: MemoryOrder) =
(*
 * x86: Exchanging any memory with any register is a serializing instruction.
 *)
  BEGIN
    IF u.debug THEN
      u.wr.Cmd   ("fence");
      u.wr.NL    ();
    END;
    print(u, " /* fence */ ");
  END fence;

CONST AtomicOpName = ARRAY AtomicOp OF TEXT { "add", "sub", "or", "and", "xor" };

PROCEDURE fetch_and_op(u: U; atomic_op: AtomicOp; mtype: MType; ztype: ZType;
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
    print(u, " /* fetch_and_op */ ");
  END fetch_and_op;

BEGIN
(*
  BitSizeToEnumCGType[8] := M3CG.Type.Word8;
  BitSizeToEnumCGType[16] := M3CG.Type.Word16;
  BitSizeToEnumCGType[32] := M3CG.Type.Word32;
*)
END M3C.
