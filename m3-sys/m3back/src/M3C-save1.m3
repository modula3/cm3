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
CONST output_line_directives = TRUE;
  CONST output_extra_newlines = FALSE;

(* ztype: zero extended type -- a "larger" type that is a multiple of 32 bits in size
 *                              a type to store in registers, a type
 *                              to store on the compile-time or runtime stack
 *                              a type to pass a parameter as
 * mtype: memory type -- a "smaller" type that is possibly truncated to fit
 *        an in-memory layout
 *)

TYPE U = T;

REVEAL
  T = M3CG.T BRANDED "M3C.T" OBJECT
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

PROCEDURE SetLineDirective(this: T) =
BEGIN
    IF output_line_directives = FALSE THEN
        print(this, "\n");
        RETURN;
    END;
    IF this.line > 0 AND this.file # NIL THEN
        this.line_directive := "#line " & Fmt.Int(this.line) & " \"" & this.file & "\"\n";
        this.nl_line_directive := "\n" & this.line_directive;
        IF this.last_char_was_newline THEN
            print(this, this.line_directive);
        ELSE
            print(this, this.nl_line_directive);
        END;
    ELSE
        this.line_directive := "";
        this.nl_line_directive := "\n";
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

PROCEDURE Type_Init(type: Type_t): Type_t =
BEGIN
    IF type.bit_size = 0 THEN
       type.bit_size := TargetMap.CG_Size[type.cg_type];
    END;
    IF type.byte_size = 0 THEN
        type.byte_size := TargetMap.CG_Bytes[type.cg_type];
    END;
    EVAL typeidToType.put(type.typeid, type);
    RETURN type;
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

PROCEDURE pop(this: T; n: CARDINAL := 1) =
BEGIN
    FOR i := 1 TO n DO
        EVAL this.stack.remlo();
    END;
END pop;

PROCEDURE push(this: T; <*UNUSED*>type: Type; expression: TEXT) =
BEGIN
    this.stack.addlo(expression);
END push;

PROCEDURE get(this: T; n: CARDINAL := 0): TEXT =
BEGIN
    RETURN this.stack.get(n);
END get;

PROCEDURE SuppressLineDirective(this: T; adjust: INTEGER; <*UNUSED*>reason: TEXT) =
BEGIN
  INC(this.suppress_line_directive, adjust);
  (*
  RTIO.PutText("suppress_line_directive now " & Fmt.Int(this.suppress_line_directive) & " due to " & reason & "\n");
  RTIO.Flush();
  *)
END SuppressLineDirective;

PROCEDURE print(this: T; text: TEXT) = <*FATAL ANY*>
  VAR length := Text.Length(text);
  BEGIN
    IF length = 0 THEN
      RETURN;
    END;
    IF output_extra_newlines THEN
      Wr.PutText(this.c, text & "\n");
    ELSE
      Wr.PutText(this.c, text);
    END;

    IF text = this.line_directive OR text = this.nl_line_directive THEN
      this.width := 0;
      this.last_char_was_newline := TRUE;
      RETURN;
    END;

    IF (*this.suppress_line_directive < 1 AND*) Text.GetChar(text, length - 1) = '\n' THEN
      Wr.PutText(this.c, this.line_directive);
      this.width := 0;
      this.last_char_was_newline := TRUE;
      RETURN;
    END;

    IF Text.FindChar(text, '\n') # -1 THEN
      this.width := 0; (* roughly *)
      Wr.PutText(this.c, this.nl_line_directive);
      this.last_char_was_newline := TRUE;
      RETURN;
    END;

    INC(this.width, length);
    IF this.width < 900 THEN
      this.last_char_was_newline := FALSE;
      RETURN;
    END;

    this.width := 0;
    IF this.last_char_was_newline THEN
      Wr.PutText(this.c, this.line_directive);
    ELSE
      Wr.PutText(this.c, this.nl_line_directive);
    END;
    this.last_char_was_newline := TRUE;
  END print;

(*---------------------------------------------------------------------------*)

PROCEDURE New (cfile: Wr.T): M3CG.T =
  VAR this := NEW (T);
  BEGIN
    this.c := cfile;
    this.init_fields := NEW(TextSeq.T).init();
    this.initializer := NEW(TextSeq.T).init();
    this.stack := NEW(TextSeq.T).init();
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
    RETURN this;
  END New;

(*----------------------------------------------------------- ID counters ---*)

PROCEDURE next_label (this: T; n: INTEGER := 1): Label =
VAR label := this.label;
  BEGIN
    INC(this.label, n);
    RETURN label;
  END next_label;

(*------------------------------------------------ READONLY configuration ---*)

<*NOWARN*>PROCEDURE set_error_handler (this: T; p: ErrorHandler) =
  BEGIN
  END set_error_handler;

(*----------------------------------------------------- compilation units ---*)

<*NOWARN*>PROCEDURE begin_unit (this: T; optimize: INTEGER) =
(* called before any other method to initialize the compilation unit *)
BEGIN
    print(this, "/* begin unit */\n");
    SuppressLineDirective(this, 1, "begin_unit");
    FOR i := FIRST(Prefix) TO LAST(Prefix) DO
        print(this, Prefix[i]);
        print(this, "\n");
    END;
    SuppressLineDirective(this, -1, "begin_unit");

    this.report_fault := NIL;
    this.in_proc_call := 0;
END begin_unit;

PROCEDURE end_unit(this: T) =
(* called after all other methods to finalize the unit and write the
   resulting object *)
BEGIN
    print(this, "/* end unit */\n");
    this.line_directive := ""; (* really suppress *)
    this.nl_line_directive := "\n"; (* really suppress *)
    SuppressLineDirective(this, 1, "end_unit");
    FOR i := FIRST(Suffix) TO LAST(Suffix) DO
        print(this, Suffix[i]);
        print(this, "\n");
    END;
    SuppressLineDirective(this, -1, "end_unit");
END end_unit;

<*NOWARN*>PROCEDURE import_unit(this: T; name: Name) =
  (* note that the current compilation unit imports the interface 'name' *)
BEGIN
    (* UNDONE? *)  
END import_unit;

<*NOWARN*>PROCEDURE export_unit(this: T; name: Name) =
(* note that the current compilation unit exports the interface 'name' *)
BEGIN
    (* UNDONE? *)  
END export_unit;

(*------------------------------------------------ debugging line numbers ---*)

PROCEDURE set_source_file (this: T; file: TEXT) =
  (* Sets the current source file name. Subsequent statements
     and expressions are associated with this source location. *)
BEGIN
    this.file := file;
    SetLineDirective(this);
END set_source_file;

PROCEDURE set_source_line (this: T; line: INTEGER) =
(* Sets the current source line number. Subsequent statements
   and expressions are associated with this source location. *)
BEGIN
    this.line := line;
    SetLineDirective(this);
END set_source_line;

(*------------------------------------------- debugging type declarations ---*)

<*NOWARN*>PROCEDURE declare_typename (this: T; typeid: TypeUID; name: Name) =
BEGIN
(*
    print(this, "typedef M" & Fmt.Unsigned(typeid) & " " & M3ID.ToText(name) & ";\n");
*)
END declare_typename;

(*
PROCEDURE TypeIDToText(x: INTEGER): TEXT =
BEGIN
    RETURN "M" & Fmt.Unsigned(x);
END TypeIDToText;
*)

<*NOWARN*>PROCEDURE declare_array (this: T; typeid, index_typeid, element_typeid: TypeUID; total_bit_size: BitSize) =
BEGIN
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

      print(this, "typedef struct{");
      print(this, TypeIDToText(element_typeid));
      print(this, " _elts[");
      print(this, Fmt.Int(total_bit_size DIV element_type.bit_size));
      print(this, "];}");
      print(this, TypeIDToText(typeid));
      print(this, ";");
    END;
*)
END declare_array;

<*NOWARN*>PROCEDURE declare_open_array (this: T; typeid, element_typeid: TypeUID; bit_size: BitSize) =
BEGIN
    <* ASSERT bit_size MOD 32 = 0 *>
(*
    WITH element_type = TypeidToType_Get(element_typeid) DO
      IF element_type = NIL THEN
        RTIO.PutText("declare_array nil element_type\n");
        RTIO.Flush();
      END;
      print(this, "typedef struct { ");
      print(this, TypeIDToText(element_typeid));
      print(this, "* _elts; CARDINAL _size");
      IF bit_size > Target.Integer.size * 2 THEN
        print(this, "s[");
        print(this, Fmt.Int((bit_size - Target.Integer.size) DIV Target.Integer.size));
        print(this, "]");
      END;
      print(this, ";}" & TypeIDToText(element_typeid) & ";");
      EVAL typeidToType.put(typeid, NEW(OpenArray_t,
              typeid := typeid,
              byte_size := bit_size DIV 8,
              bit_size := bit_size,
              element_typeid := element_typeid,
              element_type := element_type));
    END;
*)
END declare_open_array;

<*NOWARN*>PROCEDURE declare_enum (this: T; typeid: TypeUID; n_elts: INTEGER; bit_size: BitSize) =
  BEGIN
    SuppressLineDirective(this, n_elts, "declare_enum n_elts");
    <* ASSERT bit_size = 8 OR bit_size = 16 OR bit_size = 32 *>
(*
    WITH type = NEW(Enum_t, typeid := typeid, max := n_elts - 1, cg_type := BitSizeToEnumCGType[bit_size]) DO
      <* ASSERT this.enum = NIL *>
      this.enum := type;
      EVAL Type_Init(type);
      this.enum_id := TypeIDToText(typeid);
      this.enum_value := 0;
      this.enum_type := "UINT" & Fmt.Int(bit_size);
      print(this, "typedef " & this.enum_type & " " & this.enum_id & ";");
    END;
*)
  END declare_enum;

<*NOWARN*>PROCEDURE declare_enum_elt (this: T; name: Name) =
BEGIN
    SuppressLineDirective(this, -1, "declare_enum_elt");
(*
    print(this, "#define " & this.enum_id & "_" & M3ID.ToText(name) & " ((" & this.enum_type & ")" & Fmt.Int(this.enum_value) & ")\n");
    INC (this.enum_value);
    IF this.enum_value = this.enum.max + 1 THEN
        this.enum := NIL;
        this.enum_id := NIL;
        this.enum_type := NIL;
        this.enum_value := 10000;
    END;
*)
END declare_enum_elt;

<*NOWARN*>PROCEDURE declare_packed  (this: T; typeid: TypeUID; bit_size: BitSize; base: TypeUID) =
BEGIN
END declare_packed;

<*NOWARN*>PROCEDURE declare_record (this: T; typeid: TypeUID; bit_size: BitSize; n_fields: INTEGER) =
BEGIN
    SuppressLineDirective(this, n_fields, "declare_record n_fields");
END declare_record;

<*NOWARN*>PROCEDURE declare_field (this: T; name: Name; offset: BitOffset; bit_size: BitSize; typeid: TypeUID) =
BEGIN
    SuppressLineDirective(this, -1, "declare_field");
END declare_field;

<*NOWARN*>PROCEDURE declare_set(this: T; typeid, domain: TypeUID; bit_size: BitSize) =
BEGIN
    (* UNDONE? *)  
END declare_set;

<*NOWARN*>PROCEDURE declare_subrange(
    this: T;
    typeid, domain: TypeUID;
    READONLY min, max: Target.Int;
    bit_size: BitSize
    ) =
BEGIN
    (* UNDONE? *)  
END declare_subrange;

<*NOWARN*>PROCEDURE declare_pointer(this: T; typeid, target: TypeUID; brand: TEXT; traced: BOOLEAN) =
BEGIN
    (* UNDONE? *)  
END declare_pointer;

<*NOWARN*>PROCEDURE declare_indirect(this: T; typeid, target: TypeUID; <*UNUSED*>target_typename: M3ID.T) =
BEGIN
    (* UNDONE? *)  
END declare_indirect;

<*NOWARN*>PROCEDURE declare_proctype(
    this: T; typeid: TypeUID; n_formals: INTEGER;
    result: TypeUID; n_raises: INTEGER;
    callingConvention: CallingConvention; result_typename: M3ID.T) =
BEGIN
    SuppressLineDirective(this, n_formals + (ORD(n_raises >= 0) * n_raises), "declare_proctype n_formals + n_raises");
END declare_proctype;

<*NOWARN*>PROCEDURE declare_formal(this: T; name: Name; typeid: TypeUID; typename: M3ID.T) =
BEGIN
    print(this, "/* declare formal: " & M3ID.ToText(name) & " */\n");
    SuppressLineDirective(this, -1, "declare_formal");
    (* UNDONE? *)  
END declare_formal;

<*NOWARN*>PROCEDURE declare_raises(this: T; name: Name) =
BEGIN
    SuppressLineDirective(this, -1, "declare_raises");
    (* UNDONE? *)  
END declare_raises;

<*NOWARN*>PROCEDURE declare_object(
    this: T; typeid, super: TypeUID;
    brand: TEXT; traced: BOOLEAN;
    n_fields, n_methods: INTEGER;
    field_size: BitSize) =
BEGIN
    SuppressLineDirective(this, n_fields + n_methods, "declare_object n_fields + n_methods");
    (* UNDONE? *)  
END declare_object;

<*NOWARN*>PROCEDURE declare_method(this: T; name: Name; signature: TypeUID) =
BEGIN
    print(this, "/* declare formal: " & M3ID.ToText(name) & " */\n");
    SuppressLineDirective(this, -1, "declare_method");
    (* UNDONE? *)  
END declare_method;

<*NOWARN*>PROCEDURE declare_opaque (this: T; typeid, super: TypeUID) =
BEGIN
    (* UNDONE? *)  
END declare_opaque;

<*NOWARN*>PROCEDURE reveal_opaque (this: T; lhs, rhs: TypeUID) =
BEGIN
    (* UNDONE? *)  
END reveal_opaque;

<*NOWARN*>PROCEDURE declare_exception(
    this: T; name: Name; arg_type: TypeUID;
    raise_proc: BOOLEAN; base: Var; offset: INTEGER) =
BEGIN
    (* UNDONE? *)  
END declare_exception;

(*--------------------------------------------------------- runtime hooks ---*)

<*NOWARN*>PROCEDURE set_runtime_proc(this: T; name: Name; p: Proc) =
VAR proc := NARROW(p, CProc);
BEGIN
    (* UNDONE? *)  
END set_runtime_proc;

(*------------------------------------------------- variable declarations ---*)

<*NOWARN*>PROCEDURE import_global(this: T; name: Name; byte_size: ByteSize; alignment: Alignment; type: Type; typeid: TypeUID): Var =
VAR var := NEW(CVar, type := type, name := FixName(name));
BEGIN
    RETURN var;
END import_global;

<*NOWARN*>PROCEDURE declare_segment (this: T; name: Name; typeid: TypeUID; is_const: BOOLEAN): Var =
VAR fixed_name := FixName(name);
    var := NEW(CVar, name := fixed_name, is_const := is_const);
    text: TEXT := NIL;
    length := 0;
BEGIN
    print (this, "/* declare_segment */ ");
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
            this.unit_name := text;
        END;
    END;
    text := M3ID.ToText(fixed_name);
    print(this, "struct " & text & "_t;");
    IF is_const THEN
        print(this, "const ");
    END;
    print(this, "static struct " & text & "_t " & text & ";");
    
    IF this.report_fault = NIL AND NOT is_const THEN (* See M3x86.m3 *)
        this.report_fault := M3ID.ToText(var.name) & "_CRASH";
        print(this, "void __stdcall " & this.report_fault & "(UINT32 code) { RTHooks__ReportFault((ADDRESS)&" & M3ID.ToText(var.name) & ",code);}");
    END;
    
    RETURN var;
END declare_segment;

PROCEDURE bind_segment (this: T; v: Var; byte_size: ByteSize; alignment: Alignment;
                        type: Type; exported, inited: BOOLEAN) =
VAR var := NARROW(v, CVar);
BEGIN
    print (this, "/* bind_segment */ ");
END bind_segment;

PROCEDURE declare_global (this: T; name: Name; byte_size: ByteSize; alignment: Alignment;
                     type: Type; typeid: TypeUID; exported, inited: BOOLEAN): Var =
BEGIN
    print (this, "/* declare_global */ ");
    RETURN DeclareGlobal(this, name, byte_size, alignment, type, typeid, exported, inited, FALSE);
END declare_global;

PROCEDURE
declare_constant(
    this: T;
    name: Name;
    byte_size: ByteSize;
    alignment: Alignment;
    type: Type;
    typeid: TypeUID;
    exported: BOOLEAN;
    inited: BOOLEAN
    ): Var =
BEGIN
    print (this, "/* declare_constant */ ");
    RETURN DeclareGlobal(this, name, byte_size, alignment, type, typeid, exported, inited, TRUE);
END declare_constant;

<*NOWARN*>PROCEDURE
DeclareGlobal(
    this: T;
    name: Name;
    byte_size: ByteSize;
    alignment: Alignment;
    type: Type;
    typeid: TypeUID;
    exported: BOOLEAN;
    inited: BOOLEAN;
    is_const: BOOLEAN;
    ): Var =
BEGIN
    RETURN NEW(CVar, name := FixName(name));
END DeclareGlobal;

PROCEDURE
declare_local(
    this: T;
    name: Name;
    byte_size: ByteSize;
    alignment: Alignment;
    type: Type;
    typeid: TypeUID;
    in_memory: BOOLEAN;
    up_level: BOOLEAN;
    frequency: Frequency): Var =
VAR var := NEW(CVar, type := type, name := FixName(name));
    text: TEXT;
BEGIN
    IF type = Type.Struct THEN
        text := "struct{char a[" & Fmt.Int(byte_size) & "];}";
    ELSE
        text := typeToText[type];
    END;
    text := text & " " & M3ID.ToText(var.name) & ";";
    IF this.in_procedure > 0 OR this.in_block > 0 THEN
        print(this, text);
    ELSE
        this.function.locals.addhi(text);
    END;
    RETURN var;
END declare_local;

PROCEDURE
function_prototype(this: T; proc: CProc) =
VAR params := proc.params;
BEGIN
    SuppressLineDirective(this, 1, "funtion_prototype");
    print(this, typeToText[proc.return_type] & " __stdcall " & M3ID.ToText(proc.name));
    IF NUMBER (params^) = 0 THEN
        print(this, "(void)");
    ELSE
        print(this, "(");
        FOR i := FIRST(params^) TO LAST(params^) DO
            WITH param = params[i] DO
                print(this, typeToText[param.type]);
                print(this, " ");
                print(this, M3ID.ToText(param.name));
                IF i # LAST(params^) THEN
                    print(this, ",");
                ELSE
                    print(this, ")");
                END;
            END;
        END;
    END;
    SuppressLineDirective(this, -1, "funtion_prototype");
END function_prototype;

PROCEDURE
declare_param(
    this: T;
    name: Name;
    byte_size: ByteSize;
    alignment: Alignment;
    type: Type;
    typeid: TypeUID;
    in_memory: BOOLEAN;
    up_level: BOOLEAN;
    frequency: Frequency
    ): Var =
VAR var := NEW(CVar, type := type, name := FixName(name), type := type);
BEGIN
    this.function.params[this.param_count] := var;
    SuppressLineDirective(this, -1, "declare_param");
    INC(this.param_count);
    IF this.param_count = NUMBER(this.function.params^) THEN
        function_prototype(this, this.function);
        print(this, ";");
        this.param_count := -1000; (* catch bugs *)
    END;
    RETURN var;
END declare_param;

PROCEDURE declare_temp (this: T; byte_size: ByteSize; alignment: Alignment; type: Type; in_memory:BOOLEAN): Var =
BEGIN
    RETURN declare_local(this, 0, byte_size, alignment, type, -1, in_memory, FALSE, M3CG.Always);
END declare_temp;

PROCEDURE free_temp (this: T; v: Var) =
BEGIN
END free_temp;

(*---------------------------------------- static variable initialization ---*)

PROCEDURE begin_init (this: T; v: Var) =
VAR var := NARROW(v, CVar);
BEGIN
    print (this, "/* begin_init */ ");
    this.current_init_offset := 0;
    SuppressLineDirective(this, 1, "begin_init");
END begin_init;

PROCEDURE end_init (this: T; v: Var) =
VAR var := NARROW(v, CVar);
    init_fields := this.init_fields;
    initializer := this.initializer;
    var_name := M3ID.ToText(var.name);
    comma := "";
BEGIN
    print (this, "/* end_init */ ");
    end_init_helper(this);
    IF var.is_const THEN
        print(this, "const ");
    END;
    print(this, "static struct " & var_name & "_t{");
    WHILE init_fields.size() > 0 DO
        print (this, init_fields.remlo());
    END;
    print(this, "}" & var_name & "={");
    WHILE initializer.size() > 0 DO
        print(this, comma);
        print(this, initializer.remlo());
        comma := ",";
    END;
    print (this, "};");
    SuppressLineDirective(this, -1, "end_init");
END end_init;

PROCEDURE init_to_offset (this: T; offset: ByteOffset) =
VAR pad := offset - this.current_init_offset;
    init_fields := this.init_fields;
    initializer := this.initializer;
BEGIN
    <* ASSERT offset >= this.current_init_offset *>
    <* ASSERT pad >= 0 *>
    <* ASSERT this.current_init_offset >= 0 *>
    IF pad > 0 THEN
        end_init_helper(this);
        init_fields.addhi ("char " & M3ID.ToText(FixName(0)) & "[" & Fmt.Int(pad) & "];");
        initializer.addhi("{0}");
    END;
END init_to_offset;

PROCEDURE end_init_helper(this: T) =
BEGIN
    IF this.init_type_count > 0 THEN
    this.init_fields.addhi("[" & Fmt.Int(this.init_type_count) & "];");
    END;
    this.init_type_count := 0;
END end_init_helper;

PROCEDURE init_helper(this: T; offset: ByteOffset; type: Type) =
BEGIN
    init_to_offset (this, offset);
    IF offset = 0 OR this.init_type # type OR offset # this.current_init_offset THEN
      end_init_helper(this);
      this.init_fields.addhi(typeToText[type] & " " & M3ID.ToText(FixName(0)));
    END;
    INC(this.init_type_count);
    this.init_type := type;
    this.current_init_offset := offset + TargetMap.CG_Bytes[type];
END init_helper;

PROCEDURE init_int (this: T; offset: ByteOffset; READONLY value: Target.Int; type: Type) =
BEGIN
    init_helper(this, offset, type);
    this.initializer.addhi(TInt.ToText(value));
END init_int;

PROCEDURE init_proc (this: T; offset: ByteOffset; p: Proc) =
VAR proc := NARROW(p, CProc);
BEGIN
    init_helper(this, offset, Type.Addr); (* FUTURE: better typing *)
    this.initializer.addhi("(ADDRESS)&" & M3ID.ToText(proc.name));
END init_proc;

PROCEDURE init_label (this: T; offset: ByteOffset; value: Label) =
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("init_label");
      this.wr.Int   (offset);
      this.wr.Lab   (value);
      this.wr.NL    ();
    END;
    print (this, "/* init_label */ ");
    <* ASSERT FALSE *>
  END init_label;

PROCEDURE init_var (this: T; offset: ByteOffset; v: Var; bias: ByteOffset) =
  VAR var := NARROW(v, CVar);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("init_var");
      this.wr.Int   (offset);
      this.wr.VName (var);
      this.wr.Int   (bias);
      this.wr.NL    ();
      print(this, "/* init_var */ ");
    END;
    init_helper(this, offset, Type.Addr); (* FUTURE: better typing *)
    IF bias # 0 THEN
      this.initializer.addhi(Fmt.Int(bias) & "+"& "(ADDRESS)&" & M3ID.ToText(var.name));
    ELSE
      this.initializer.addhi("(ADDRESS)&" & M3ID.ToText(var.name));
    END;
  END init_var;

PROCEDURE init_offset (this: T; offset: ByteOffset; value: Var) =
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("init_offset");
      this.wr.Int   (offset);
      this.wr.VName (value);
      this.wr.NL    ();
    END;
    print (this, "/* init_offset */ ");
    <* ASSERT FALSE *>
  END init_offset;

PROCEDURE init_chars (this: T; offset: ByteOffset; value: TEXT) =
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("init_chars");
      this.wr.Int   (offset);
      this.wr.Txt   (value);
      this.wr.NL    ();
    END;
    print (this, "/* init_chars */ ");
  END init_chars;

PROCEDURE init_float (this: T; offset: ByteOffset; READONLY float: Target.Float) =
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("init_float");
      this.wr.Int   (offset);
      this.wr.Flt   (float);
      this.wr.NL    ();
    END;
    print (this, "/* init_float */ ");
  END init_float;

(*------------------------------------------------------------ PROCEDUREs ---*)

PROCEDURE import_procedure (...): Proc =
VAR proc := NEW(CProc, name := FixName(name), n_params := n_params,
                return_type := return_type,
                callingConvention := callingConvention,
                locals := NEW(TextSeq.T).init(),
                params := NEW(REF ARRAY OF CVar, n_params));
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("import_procedure");
      this.wr.ZName (name);
      this.wr.Int   (n_params);
      this.wr.TName (return_type);
      this.wr.Txt   (callingConvention.name);
      this.wr.PName (proc);
      this.wr.NL    ();
      print (this, "/* import_procedure */ ");
    END;
    SuppressLineDirective(this, n_params, "import_procedure n_params");
    this.param_count := 0;
    this.function := proc;
    IF n_params = 0 THEN
      function_prototype(this, proc);
      print(this, ";");
    END;
    RETURN proc;
  END import_procedure;

PROCEDURE declare_procedure (this: T; name: Name; n_params: INTEGER;
                             return_type: Type; level: INTEGER;
                             callingConvention: CallingConvention;
                             exported: BOOLEAN; parent: Proc;
                             <*UNUSED*>return_typeid: TypeUID;
                             <*UNUSED*>return_typename: M3ID.T): Proc =
VAR proc := NEW(CProc, name := FixName(name), n_params := n_params,
                return_type := return_type, level := level,
                callingConvention := callingConvention, exported := exported,
                parent := parent,
                locals := NEW(TextSeq.T).init(),
                params := NEW(REF ARRAY OF CVar, n_params));
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("declare_procedure");
      this.wr.ZName (name);
      this.wr.Int   (n_params);
      this.wr.TName (return_type);
      this.wr.Int   (level);
      this.wr.Txt   (callingConvention.name);
      this.wr.Bool  (exported);
      this.wr.PName (parent);
      this.wr.PName (proc);
      this.wr.NL    ();
      print (this, "/* declare_procedure */ ");
    END;
    SuppressLineDirective(this, n_params, "declare_procedure n_params");
    this.param_count := 0;
    this.function := proc;
    RETURN proc;
  END declare_procedure;

PROCEDURE begin_procedure (this: T; p: Proc) =
  VAR proc := NARROW(p, CProc);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("begin_procedure");
      this.wr.PName (proc);
      this.wr.NL    ();
    END;
    print (this, "/* begin_procedure */ ");
    INC(this.in_procedure);
    this.function := proc;
    function_prototype(this, proc);
    print(this, "{");
    WHILE proc.locals.size() > 0 DO
      print(this, proc.locals.remlo());
    END;
  END begin_procedure;

PROCEDURE end_procedure (this: T; p: Proc) =
  VAR proc := NARROW(p, CProc);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("end_procedure");
      this.wr.PName (proc);
      this.wr.NL    ();
    END;
    print (this, "/* end_procedure */ ");
    DEC(this.in_procedure);
    print(this, "}");
  END end_procedure;

PROCEDURE begin_block (this: T) =
  (* marks the beginning of a nested anonymous block *)
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("begin_block");
      this.wr.NL    ();
    END;
    print (this, "/* begin_block */ ");
    INC(this.in_block);
    print (this, "{");
  END begin_block;

PROCEDURE end_block (this: T) =
  (* marks the ending of a nested anonymous block *)
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("end_block");
      this.wr.NL    ();
    END;
    print (this, "/* end_block */ ");
    DEC(this.in_block);
    print (this, "}");
  END end_block;

PROCEDURE note_procedure_origin (this: T; p: Proc) =
  VAR proc := NARROW(p, CProc);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("note_procedure_origin");
      this.wr.PName (proc);
      this.wr.NL    ();
    END;
    print (this, "/* note_procedure_origin */ ");
  END note_procedure_origin;

(*------------------------------------------------------------ statements ---*)

PROCEDURE set_label (this: T; label: Label; <*UNUSED*> barrier: BOOLEAN) =
  (* define 'label' to be at the current pc *)
  BEGIN
    print (this, "/* set_label */ ");
    print(this, "L" & Fmt.Unsigned(label) & ":;");
  END set_label;

PROCEDURE jump (this: T; label: Label) =
  (* GOTO label *)
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("jump");
      this.wr.Lab   (label);
      this.wr.NL    ();
    END;
    print (this, "/* jump */ ");
    print(this, "goto L" & Fmt.Unsigned(label) & ";");
  END jump;

PROCEDURE if_true  (this: T; itype: IType; label: Label; <*UNUSED*> frequency: Frequency) =
  (* IF (s0.itype # 0) GOTO label; pop *)
  VAR s0 := cast(get(this, 0), itype);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("if_true");
      this.wr.TName (itype);
      this.wr.Lab   (label);
      this.wr.NL    ();
      print (this, "/* if_true */ ");
    END;
    print(this, "if(" & s0 & ")goto L" & Fmt.Unsigned(label) & ";");
    pop(this);
  END if_true;

PROCEDURE if_false (this: T; itype: IType; label: Label; <*UNUSED*> frequency: Frequency) =
  (* IF (s0.itype = 0) GOTO label; pop *)
  VAR s0 := cast(get(this, 0), itype);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("if_false");
      this.wr.TName (itype);
      this.wr.Lab   (label);
      this.wr.NL    ();
      print (this, "/* if_false */ ");
    END;
    print(this, "if(!" & paren(s0) & ")goto L" & Fmt.Unsigned(label) & ";");
    pop(this);
  END if_false;

PROCEDURE if_compare (this: T; ztype: ZType; op: CompareOp; label: Label;
                      <*UNUSED*> frequency: Frequency) =
  (* IF (s1.ztype op s0.ztype) GOTO label; pop(2) *)
  VAR s0 := cast(get(this, 0), ztype);
      s1 := cast(get(this, 1), ztype);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("if_compare");
      this.wr.TName (ztype);
      this.wr.OutT  (" " & CompareOpName[op]);
      this.wr.Lab   (label);
      this.wr.NL    ();
      print(this, "/* if_compare */ ");
    END;
    pop(this, 2);
    print(this, "if(" & paren(s1) & CompareOpC[op] & paren(s0) & ")goto L" & Fmt.Unsigned(label) & ";");
  END if_compare;

PROCEDURE case_jump (this: T; itype: IType; READONLY labels: ARRAY OF Label) =
  (* "GOTO labels[s0.itype]; pop" with no range checking on s0.itype *)
  (*VAR s0 := get(this);*)
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("case_jump");
      this.wr.TName (itype);
      this.wr.Int   (NUMBER(labels));
      FOR i := FIRST (labels) TO LAST (labels) DO
        this.wr.Lab (labels [i]);
      END;
      this.wr.NL    ();
    END;
    print(this, "/* case_jump */ ");
    pop(this);
  END case_jump;

PROCEDURE exit_proc (this: T; type: Type) =
  (* Returns s0.type if type is not Void, otherwise returns no value. *)
  VAR s0: TEXT;
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("exit_proc");
      this.wr.TName (type);
      this.wr.NL    ();
    END;
    print(this, "/* exit_proc */ ");
    IF type = Type.Void THEN
      print(this, "return;");
    ELSE
      s0 := get(this);
      IF type = Type.Addr THEN
        s0 := "(ADDRESS)" & s0;
      END;
      print(this, "return " & s0 & ";");
      pop(this);
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

PROCEDURE load_helper  (this: T; in: TEXT; in_offset: INTEGER; in_mtype: MType; out_ztype: ZType) =
  VAR text: TEXT := NIL;
  BEGIN
    <* ASSERT CG_Bytes[out_ztype] >= CG_Bytes[in_mtype] *>
    text := "*(volatile " & typeToText[in_mtype] & "*)" & address_plus_offset(in, in_offset);
    IF in_mtype # out_ztype THEN
      text := "((" & typeToText[out_ztype] & ")(" & text & "))";
    END;
    push(this, out_ztype, text);
  END load_helper;

PROCEDURE load  (this: T; v: Var; offset: ByteOffset; mtype: MType; ztype: ZType) =
(* push; s0.ztype := Mem [ ADR(var) + offset ].mtype; The only allowed (mtype->ztype) conversions
   are {Int,Word}{8,16} -> {Int,Word}{32,64} and {Int,Word}32 -> {Int,Word}64.
   The source type, mtype, determines whether the value is sign-extended or
   zero-extended. *)
  VAR var := NARROW(v, CVar);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("load");
      this.wr.VName (var);
      this.wr.Int   (offset);
      this.wr.TName (mtype);
      this.wr.TName (ztype);
      this.wr.NL    ();
      print(this, "/* load */ ");
    END;
    load_helper(this, "&" & M3ID.ToText(var.name), offset, mtype, ztype);
  END load;

PROCEDURE store_helper(this: T; in: TEXT; in_ztype: ZType; out_address: TEXT; out_offset: INTEGER; out_mtype: MType) =
  BEGIN
    <* ASSERT CG_Bytes[in_ztype] >= CG_Bytes[out_mtype] *>
    print(this, "(*(volatile " & typeToText[out_mtype] & "*)" & address_plus_offset(out_address, out_offset) & ")=(" & typeToText[in_ztype] & ")(" & in & ");");
  END store_helper;

PROCEDURE store (this: T; v: Var; offset: ByteOffset; ztype: ZType; mtype: MType) =
(* Mem [ ADR(var) + offset ].mtype := s0.ztype; pop *)
  VAR var := NARROW(v, CVar);
      s0 := cast(get(this, 0), ztype);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("store");
      this.wr.VName (var);
      this.wr.Int   (offset);
      this.wr.TName (ztype);
      this.wr.TName (mtype);
      this.wr.NL    ();
      print(this, "/* store */ ");
    END;
    pop(this);
    store_helper(this, s0, ztype, "&" & M3ID.ToText(var.name), offset, mtype);
  END store;

PROCEDURE load_address (this: T; v: Var; offset: ByteOffset) =
(* push; s0.A := ADR(var) + offset *)
  VAR var := NARROW(v, CVar);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("load_address");
      this.wr.VName (var);
      this.wr.Int   (offset);
      this.wr.NL    ();
      print(this, "/* load_address */ ");
    END;
    push(this, Type.Addr, address_plus_offset("&" & M3ID.ToText (var.name), offset));
  END load_address;

PROCEDURE load_indirect (this: T; offset: ByteOffset; mtype: MType; ztype: ZType) =
(* s0.ztype := Mem [s0.A + offset].mtype  *)
  VAR s0 := get(this);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("load_indirect");
      this.wr.Int   (offset);
      this.wr.TName (mtype);
      this.wr.TName (ztype);
      this.wr.NL    ();
      print(this, "/* load_indirect */ ");
    END;
    <* ASSERT CG_Bytes[ztype] >= CG_Bytes[mtype] *>
    pop(this);
    load_helper(this, s0, offset, mtype, ztype);
  END load_indirect;

PROCEDURE store_indirect (this: T; offset: ByteOffset; ztype: ZType; mtype: MType) =
(* Mem [s1.A + offset].mtype := s0.ztype; pop (2) *)
  VAR s0 := cast(get(this, 0), ztype);
      s1 := get(this, 1);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("store_indirect");
      this.wr.Int   (offset);
      this.wr.TName (ztype);
      this.wr.TName (mtype);
      this.wr.NL    ();
      print(this, "/* store_indirect */ ");
    END;
    pop(this, 2);
    store_helper(this, s0, ztype, s1, offset, mtype);
  END store_indirect;

(*-------------------------------------------------------------- literals ---*)

PROCEDURE load_nil (this: T) =
  (* push; s0.A := NIL *)
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("load_nil");
      this.wr.NL    ();
      print(this, "/* load_nil */ ");
    END;
    push(this, Type.Addr, "0"); (* UNDONE NULL or (ADDRESS)0? *)
  END load_nil;

PROCEDURE load_integer  (this: T; type: IType; READONLY i: Target.Int) =
  (* push; s0.type := i *)
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("load_integer");
      this.wr.TName (type);
      this.wr.TInt  (TIntN.FromTargetInt(i, CG_Bytes[type])); (* UNDONE? *)
      this.wr.NL    ();
      print(this, "/* load_integer */ ");
    END;
    (* TODO: use suffixes L, U, UL, ULL, i64, ui64 via #ifdef and macro *)
    push(this, type, "((" & typeToText[type] & ")" & TInt.ToText(i) & ")");
  END load_integer;

PROCEDURE load_float    (this: T; type: RType; READONLY float: Target.Float) =
  (* push; s0.type := float *)
  VAR buffer: ARRAY [0..BITSIZE(EXTENDED)] OF CHAR;
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("load_float");
      this.wr.TName (type);
      this.wr.Flt   (float);
      this.wr.NL    ();
    END;
    print(this, "/* load_float */ ");
    (* TODO: use suffixes *)
    push(this, type, "((" & typeToText[type] & ")" & Text.FromChars(SUBARRAY(buffer, 0, TFloat.ToChars(float, buffer))) & ")");
  END load_float;

(*------------------------------------------------------------ arithmetic ---*)

PROCEDURE cast (expr: TEXT; type: Type): TEXT =
BEGIN
  RETURN "((" & typeToText[type] & ")(" & expr & "))";
END cast;

PROCEDURE compare (this: T; ztype: ZType; itype: IType; op: CompareOp) =
  (* s1.itype := (s1.ztype op s0.ztype); pop *)
  VAR s0 := cast(get(this, 0), ztype);
      s1 := cast(get(this, 1), ztype);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("compare");
      this.wr.TName (ztype);
      this.wr.TName (itype);
      this.wr.OutT  (" " & CompareOpName[op]);
      this.wr.NL    ();
      print(this, "/* compare */ ");
    END;
    (* ASSERT cond # Cond.Z AND cond # Cond.NZ *)
      pop(this);
      push(this, itype, cast(s1 & CompareOpC[op] & s0, itype));
  END compare;

PROCEDURE add (this: T; type: AType) =
  (* s1.type := s1.type + s0.type; pop *)
  VAR s0 := cast(get(this, 0), type);
      s1 := cast(get(this, 1), type);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("add");
      this.wr.TName (type);
      this.wr.NL    ();
      print(this, "/* add */ ");
    END;
    pop(this, 2);
    push(this, type, cast(s1 & "+" & s0, type));
  END add;

PROCEDURE subtract (this: T; type: AType) =
  (* s1.type := s1.type - s0.type; pop *)
  VAR s0 := cast(get(this, 0), type);
      s1 := cast(get(this, 1), type);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("subtract");
      this.wr.TName (type);
      this.wr.NL    ();
      print(this, "/* subtract */ ");
    END;
    pop(this, 2);
    push(this, type, cast(s1 & "-" & s0, type));
  END subtract;

PROCEDURE multiply (this: T; type: AType) =
  (* s1.type := s1.type * s0.type; pop *)
  VAR s0 := cast(get(this, 0), type);
      s1 := cast(get(this, 1), type);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("multiply");
      this.wr.TName (type);
      this.wr.NL    ();
      print(this, "/* multiply */ ");
    END;
    pop(this, 2);
    push(this, type, cast(s1 & "*" & s0, type));
  END multiply;

PROCEDURE divide (this: T; type: RType) =
  (* s1.type := s1.type / s0.type; pop *)
  VAR s0 := cast(get(this, 0), type);
      s1 := cast(get(this, 1), type);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("divide");
      this.wr.TName (type);
      this.wr.NL    ();
      print(this, "/* divide */ ");
    END;
    pop(this, 2);
    push(this, type, cast(s1 & "/" & s0, type));
  END divide;

CONST SignName = ARRAY Sign OF TEXT { " P", " N", " X" };

PROCEDURE div (this: T; type: IType; a, b: Sign) =
  (* s1.type := s1.type DIV s0.type; pop *)
  VAR s0 := cast(get(this, 0), type);
      s1 := cast(get(this, 1), type);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("div");
      this.wr.TName (type);
      this.wr.OutT  (SignName[a]);
      this.wr.OutT  (SignName[b]);
      this.wr.NL    ();
      print(this, "/* div */ ");
    END;
    pop(this, 2);
    push(this, type, cast(s1 & "/" & s0, type));
  END div;

PROCEDURE mod (this: T; type: IType; a, b: Sign) =
  (* s1.type := s1.type MOD s0.type; pop *)
  VAR s0 := cast(get(this, 0), type);
      s1 := cast(get(this, 1), type);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("mod");
      this.wr.TName (type);
      this.wr.OutT  (SignName [a]);
      this.wr.OutT  (SignName [b]);
      this.wr.NL    ();
      print(this, "/* mod */ ");
    END;
    pop(this, 2);
    push(this, type, cast(s1 & "%" & s0, type));
  END mod;

PROCEDURE negate (this: T; type: AType) =
  (* s0.type := - s0.type *)
  VAR s0 := cast(get(this, 0), type);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("negate");
      this.wr.TName (type);
      this.wr.NL    ();
      print(this, "/* negate */ ");
    END;
    pop(this);
    push(this, type, cast("-" & s0, type));
  END negate;

PROCEDURE abs (this: T; type: AType) =
  (* s0.type := ABS (s0.type) (noop on Words) *)
  VAR s0 := cast(get(this, 0), type);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("abs");
      this.wr.TName (type);
      this.wr.NL    ();
      print(this, "/* abs */");
    END;
    pop(this);
    push(this, type, "m3_abs_" & typeToText[type] & "(" & s0 & ")");
  END abs;

PROCEDURE max (this: T; type: ZType) =
  (* s1.type := MAX (s1.type, s0.type); pop *)
  VAR s0 := cast(get(this, 0), type);
      s1 := cast(get(this, 1), type);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("max");
      this.wr.TName (type);
      this.wr.NL    ();
      print(this, "/* max */ ");
    END;
    pop(this, 2);
    push(this, type, "m3_max_" & typeToText[type] & "(" & s0 & "," & s1 & ")");
  END max;

PROCEDURE min (this: T; type: ZType) =
  (* s1.type := MIN (s1.type, s0.type); pop *)
  VAR s0 := cast(get(this, 0), type);
      s1 := cast(get(this, 1), type);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("min");
      this.wr.TName (type);
      this.wr.NL    ();
      print(this, "/* min */ ");
    END;
    pop(this, 2);
    push(this, type, "m3_min_" & typeToText[type] & "(" & s0 & "," & s1 & ")");
  END min;

PROCEDURE cvt_int (this: T; rtype: RType; itype: IType; op: ConvertOp) =
  (* s0.itype := ROUND (s0.rtype) *)
  VAR s0 := cast(get(this, 0), rtype);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("cvt_int");
      this.wr.TName (rtype);
      this.wr.TName (itype);
      this.wr.OutT  (" " & ConvertOpName[op]);
      this.wr.NL    ();
      print(this, "/* cvt_int */ ");
    END;
    pop(this);
    push(this, itype, cast("m3_" & ConvertOpName[op] & "(" & s0 & ")", itype));
  END cvt_int;

PROCEDURE cvt_float (this: T; atype: AType; rtype: RType) =
  (* s0.rtype := ROUND (s0.atype) *)
  VAR s0 := cast(get(this, 0), atype);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("cvt_float");
      this.wr.TName (atype);
      this.wr.TName (rtype);
      this.wr.NL    ();
      print(this, "/* cvt_float */ ");
    END;
    (* UNDONE is this correct? *)
    pop(this);
    push(this, rtype, cast(s0, rtype));
  END cvt_float;

(*------------------------------------------------------------------ sets ---*)

PROCEDURE set_op3(this: T; byte_size: ByteSize; op: TEXT) =
  (* s2.B := s1.B op s0.B; pop(3) *)
  VAR s0 := cast(get(this, 0), Type.Addr);
      s1 := cast(get(this, 1), Type.Addr);
      s2 := cast(get(this, 1), Type.Addr);
  BEGIN
    IF this.debug THEN
      (*this.wr.Cmd   (BuiltinDesc[builtin].name);*)
      this.wr.Int   (byte_size);
      this.wr.NL    ();
      print(this, "/* " & op & " */ ");
    END;
    pop(this, 3);
    print(this, op & "(" & s2 & "," & s1 & "," & s0 & ")");
  END set_op3;

PROCEDURE set_union (this: T; byte_size: ByteSize) =
  (* s2.B := s1.B + s0.B; pop(2) *)
  BEGIN
    set_op3(this, byte_size, "set_union");
  END set_union;

PROCEDURE set_difference (this: T; byte_size: ByteSize) =
  (* s2.B := s1.B - s0.B; pop(2) *)
  BEGIN
    set_op3(this, byte_size, "set_difference");
  END set_difference;

PROCEDURE set_intersection (this: T; byte_size: ByteSize) =
  (* s2.B := s1.B * s0.B; pop(2) *)
  BEGIN
    set_op3(this, byte_size, "set_intersection");
  END set_intersection;

PROCEDURE set_sym_difference (this: T; byte_size: ByteSize) =
  (* s2.B := s1.B / s0.B; pop(2) *)
  BEGIN
    set_op3(this, byte_size, "set_sym_difference");
  END set_sym_difference;

PROCEDURE set_member (this: T; byte_size: ByteSize; type: IType) =
  (* s1.type := (s0.type IN s1.B); pop *)
  VAR s0 := cast(get(this, 0), type);
      s1 := cast(get(this, 1), Type.Addr);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("set_member");
      this.wr.Int   (byte_size);
      this.wr.TName (type);
      this.wr.NL    ();
      print(this, "/* set_member */ ");
    END;
    pop(this, 2);
    push(this, type, cast("set_member(" & s0 & "," & s1 & ")", type));
  END set_member;

PROCEDURE set_compare (this: T; byte_size: ByteSize; op: CompareOp; type: IType) =
  (* s1.type := (s1.B op s0.B); pop *)
  VAR s0 := cast(get(this, 0), Type.Addr);
      s1 := cast(get(this, 1), Type.Addr);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("set_compare");
      this.wr.Int   (byte_size);
      this.wr.OutT  (" " & CompareOpName[op]);
      this.wr.TName (type);
      this.wr.NL    ();
      print(this, "/* set_compare */ ");
    END;
    pop(this, 2);
    push(this, type, cast("m3_set_" & CompareOpName[op] & "(" & s1 & "," & s0 & ")", type));
  END set_compare;

PROCEDURE set_range (this: T; byte_size: ByteSize; type: IType) =
  (* s2.A [s1.type .. s0.type] := 1's; pop(3) *)
  VAR s0 := get(this, 0);
      s1 := get(this, 1);
      s2 := get(this, 2);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("set_range");
      this.wr.Int   (byte_size);
      this.wr.TName (type);
      this.wr.NL    ();
      print(this, "/* set_range */ ");
    END;
    pop(this, 2);
    push(this, type, "m3_set_range(" & s2 & s1 & "," & s0 & ")");
  END set_range;

PROCEDURE set_singleton (this: T; byte_size: ByteSize; type: IType) =
  (* s1.A [s0.type] := 1; pop(2) *)
  VAR s0 := get(this, 0);
      s1 := get(this, 1);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("set_singleton");
      this.wr.Int   (byte_size);
      this.wr.TName (type);
      this.wr.NL    ();
      print(this, "/* set_singleton */ ");
    END;
    pop(this, 2);
    push(this, type, "m3_set_singleton(" & s0 & "," & s1 & ")");
  END set_singleton;

(*------------------------------------------------- Word.T bit operations ---*)

PROCEDURE not (this: T; type: IType) =
  (* s0.type := Word.Not (s0.type) *)
  VAR s0 := get(this);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("not");
      this.wr.TName (type);
      this.wr.NL    ();
      print(this, "/* not */ ");
    END;
    pop(this, 2);
    push(this, type, cast("~" & cast(s0, type), type));
  END not;

PROCEDURE and (this: T; type: IType) =
  (* s1.type := Word.And (s1.type, s0.type); pop *)
  VAR s0 := get(this, 0);
      s1 := get(this, 1);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("and");
      this.wr.TName (type);
      this.wr.NL    ();
      print(this, "/* and */ ");
    END;
    pop(this, 2);
    push(this, type, cast(cast(s1, type) & "&" & cast(s0, type), type));
  END and;

PROCEDURE or  (this: T; type: IType) =
  (* s1.type := Word.Or  (s1.type, s0.type); pop *)
  VAR s0 := get(this, 0);
      s1 := get(this, 1);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("or");
      this.wr.TName (type);
      this.wr.NL    ();
      print(this, "/* or */ ");
    END;
    pop(this, 2);
    push(this, type, cast(cast(s1, type) & "|" & cast(s0, type), type));
  END or;

PROCEDURE xor (this: T; type: IType) =
  (* s1.type := Word.Xor (s1.type, s0.type); pop *)
  VAR s0 := get(this, 0);
      s1 := get(this, 1);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("xor");
      this.wr.TName (type);
      this.wr.NL    ();
      print(this, "/* xor */ ");
    END;
    pop(this, 2);
    push(this, type, cast(cast(s1, type) & "^" & cast(s0, type), type));
  END xor;

PROCEDURE shift_left   (this: T; type: IType) =
  (* s1.type := Word.Shift  (s1.type, s0.type); pop *)
  VAR s0 := get(this, 0);
      s1 := get(this, 1);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("shift_left");
      this.wr.TName (type);
      this.wr.NL    ();
      print(this, "/* shift_left */ ");
    END;
    pop(this, 2);
    push(this, type, cast(cast(s1, type) & "<<" & cast(s0, type), type));
  END shift_left;

PROCEDURE shift_right  (this: T; type: IType) =
  (* s1.type := Word.Shift  (s1.type, -s0.type); pop *)
  VAR s0 := get(this, 0);
      s1 := get(this, 1);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("shift_right");
      this.wr.TName (type);
      this.wr.NL    ();
      print(this, "/* shift_right */ ");
    END;
    pop(this, 2);
    push(this, type, cast(cast(s1, type) & ">>" & cast(s0, type), type));
  END shift_right;

PROCEDURE shift (this: T; type: IType) =
  (* s1.type := Word.Shift  (s1.type, s0.type); pop *)
  VAR s0 := get(this, 0);
      s1 := get(this, 1);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("shift");
      this.wr.TName (type);
      this.wr.NL    ();
      print(this, "/* shift */ ");
    END;
    pop(this, 2);
    push(this, type, "m3_shift_" & typeToText[type] & "(" & s1 & "," & s0 & ")");
  END shift;

PROCEDURE rotate (this: T; type: IType) =
  (* s1.type := Word.Rotate (s1.type, s0.type); pop *)
  VAR s0 := get(this, 0);
      s1 := get(this, 1);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("rotate");
      this.wr.TName (type);
      this.wr.NL    ();
      print(this, "/* rotate */ ");
    END;
    pop(this, 2);
    push(this, type, "m3_rotate_" & typeToText[type] & "(" & s1 & "," & s0 & ")");
  END rotate;

PROCEDURE rotate_left  (this: T; type: IType) =
  (* s1.type := Word.Rotate (s1.type, s0.type); pop *)
  VAR s0 := get(this, 0);
      s1 := get(this, 1);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("rotate_left");
      this.wr.TName (type);
      this.wr.NL    ();
      print(this, "/* rotate_left */ ");
    END;
    pop(this, 2);
    push(this, type, "m3_rotate_left" & typeToText[type] & "(" & s1 & "," & s0 & ")");
  END rotate_left;

PROCEDURE rotate_right (this: T; type: IType) =
  (* s1.type := Word.Rotate (s1.type, -s0.type); pop *)
  VAR s0 := get(this, 0);
      s1 := get(this, 1);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("rotate_right");
      this.wr.TName (type);
      this.wr.NL    ();
      print(this, "/* rotate_right */ ");
    END;
    pop(this, 2);
    push(this, type, "m3_rotate_right" & typeToText[type] & "(" & s1 & "," & s0 & ")");
  END rotate_right;

PROCEDURE widen (this: T; sign_extend: BOOLEAN) =
  (* s0.I64 := s0.I32; IF sign_extend THEN SignExtend s0; *)
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("widen");
      this.wr.Bool  (sign_extend);
      this.wr.NL    ();
    END;
    <*ASSERT FALSE*>
  END widen;

PROCEDURE chop (this: T) =
  (* s0.I32 := Word.And (s0.I64, 16_ffffffff); *)
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("chop");
      this.wr.NL    ();
    END;
    <*ASSERT FALSE*>
  END chop;

PROCEDURE extract (this: T; type: IType; sign_extend: BOOLEAN) =
  (* s2.type := Word.Extract(s2.type, s1.type, s0.type);
     IF sign_extend THEN SignExtend s2 END; pop(2) *)
  VAR s0 := get(this, 0);
      s1 := get(this, 1);
      s2 := get(this, 2);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("extract");
      this.wr.TName (type);
      this.wr.Bool  (sign_extend);
      this.wr.NL    ();
      print(this, "/* extract */ ");
    END;
    pop(this, 3);
    <* ASSERT sign_extend = FALSE *>
    push(this, type, "m3_extract_" & typeToText[type] & "(" & s2 & "," & s1 & "," & s0 & ")");
  END extract;

PROCEDURE extract_n (this: T; type: IType; sign_extend: BOOLEAN; n: CARDINAL) =
  (* s1.type := Word.Extract(s1.type, s0.type, n);
     IF sign_extend THEN SignExtend s1 END; pop(1) *)
  VAR s0 := get(this, 0);
      s1 := get(this, 1);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("extract_n");
      this.wr.TName (type);
      this.wr.Bool  (sign_extend);
      this.wr.Int   (n);
      this.wr.NL    ();
      print(this, "/* extract_m */ ");
    END;
    pop(this, 2);
    <* ASSERT sign_extend = FALSE *>
    push(this, type, "m3_extract_" & typeToText[type] & "(" & s1 & "," & s0 & "," & Fmt.Int(n) & ")");
  END extract_n;

PROCEDURE extract_mn (this: T; type: IType; sign_extend: BOOLEAN; m, n: CARDINAL) =
  (* s0.type := Word.Extract(s0.type, m, n);
     IF sign_extend THEN SignExtend s0 END; *)
  VAR s0 := get(this);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("extract_mn");
      this.wr.TName (type);
      this.wr.Bool  (sign_extend);
      this.wr.Int   (m);
      this.wr.Int   (n);
      this.wr.NL    ();
      print(this, "/* extract_mn */ ");
    END;
    pop(this);
    s0 := "m3_extract_" & typeToText[type] & "(" & s0 & "," & Fmt.Int(m) & "," & Fmt.Int(n) & ")";
    IF sign_extend THEN
      s0 := "m3_signextend_" & typeToText[type] & "(" & s0 & ")";
    END;
    push(this, type, s0);
  END extract_mn;

PROCEDURE insert  (this: T; type: IType) =
  (* s3.type := Word.Insert (s3.type, s2.type, s1.type, s0.type); pop(3) *)
  VAR s0 := get(this, 0);
      s1 := get(this, 1);
      s2 := get(this, 2);
      s3 := get(this, 3);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("insert");
      this.wr.TName (type);
      this.wr.NL    ();
      print(this, "/* insert */ ");
    END;
    pop(this, 4);
    push(this, type, "m3_insert_" & typeToText[type] & "(" & s3 & "," & s2 & "," & s1 & "," & s0 & ")");
  END insert;

PROCEDURE insert_n  (this: T; type: IType; n: CARDINAL) =
  (* s2.type := Word.Insert (s2.type, s1.type, s0.type, n); pop(2) *)
  VAR s0 := get(this, 0);
      s1 := get(this, 1);
      s2 := get(this, 2);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("insert_n");
      this.wr.TName (type);
      this.wr.Int   (n);
      this.wr.NL    ();
      print(this, "/* insert_n */ ");
    END;
    pop(this, 3);
    push(this, type, "m3_insert_" & typeToText[type] & "(" & s2 & "," & "," & s1 & "," & s0 & "," & Fmt.Int(n) & ")");
  END insert_n;

PROCEDURE insert_mn  (this: U; type: IType; m, n: CARDINAL) =
  (* s1.type := Word.Insert (s1.type, s0.type, m, n); pop(2) *)
  VAR s0 := get(this, 0);
      s1 := get(this, 1);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("insert_mn");
      this.wr.TName (type);
      this.wr.Int   (m);
      this.wr.Int   (n);
      this.wr.NL    ();
      print(this, "/* insert_mn */ ");
    END;
    pop(this, 2);
    push(this, type, "m3_insert_" & typeToText[type] & "(" & s1 & "," & s0 & "," & Fmt.Int(m) & "," & Fmt.Int(n) & ")");
  END insert_mn;

(*------------------------------------------------ misc. stack/memory ops ---*)

PROCEDURE swap (this: U; a, b: Type) =
  (* tmp := s1; s1 := s0; s0 := tmp *)
  VAR temp := get(this, 1);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("swap");
      this.wr.TName (a);
      this.wr.TName (b);
      this.wr.NL    ();
    END;
    this.stack.put(1, get(this, 0));
    this.stack.put(0, temp);
  END swap;

PROCEDURE cg_pop (this: U; type: Type) =
  (* pop(1) (i.e. discard s0) *)
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("pop");
      this.wr.TName (type);
      this.wr.NL    ();
      print(this, "/* pop */ ");
    END;
    pop(this);
  END cg_pop;

PROCEDURE copy_n (this: U; itype: IType; mtype: MType; overlap: BOOLEAN) =
  (* Mem[s2.A:s0.ztype] := Mem[s1.A:s0.ztype]; pop(3)*)
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("copy_n");
      this.wr.TName (itype);
      this.wr.TName (mtype);
      this.wr.Bool  (overlap);
      this.wr.NL    ();
      print(this, "/* copy_n */ ");
    END;
    (* UNDONE *)
  END copy_n;

PROCEDURE copy (this: U; n: INTEGER; type: MType; overlap: BOOLEAN) =
  (* Mem[s1.A:sz] := Mem[s0.A:sz]; pop(2)*)
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("copy");
      this.wr.Int   (n);
      this.wr.TName (type);
      this.wr.Bool  (overlap);
      this.wr.NL    ();
      print(this, "/* copy */ ");
    END;
    (* UNDONE *)
  END copy;

PROCEDURE zero_n (this: U; itype: IType; mtype: MType) =
  (* Mem[s1.A:s0.itype] := 0; pop(2) *)
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("zero_n");
      this.wr.TName (itype);
      this.wr.TName (mtype);
      this.wr.NL    ();
    END;

    <* ASSERT FALSE *>

    (* zero_n is implemented incorrectly in the gcc backend,
     * therefore it must not be used.
     *)
  END zero_n;

PROCEDURE zero (this: U; n: INTEGER; type: MType) =
  (* Mem[s0.A:sz] := 0; pop(1) *)
  VAR s0 := get(this, 0);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("zero");
      this.wr.Int   (n);
      this.wr.TName (type);
      this.wr.NL    ();
      print(this, "/* zero */ ");
    END;
    pop(this);
    print(this, "memset(" & s0 & ",0," & Fmt.Int(n) & "*" & Fmt.Int(CG_Bytes[type]) & ");");
  END zero;

(*----------------------------------------------------------- conversions ---*)

PROCEDURE loophole (this: U; from, to: ZType) =
  (* s0.to := LOOPHOLE(s0.from, to) *)
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("loophole");
      this.wr.TName (from);
      this.wr.TName (to);
      this.wr.NL    ();
    END;
    print(this, "/* loophole */ ");
    (* If type is already a pointer, then we should not add pointer here.
     * As well, if type does not contain a pointer, then we should store the
     * value in a non-stack-packed temporary and use its address.
     * We don't have code to queue up temporary declarations.
     * (for that matter, to limit/reuse temporaries)
     *)
    this.stack.put(0, "*(" & typeToText[to] & "*)&" & get(this, 0));
  END loophole;

(*------------------------------------------------ traps & runtime checks ---*)

PROCEDURE abort (this: U; code: RuntimeError) =
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("abort");
      this.wr.Int   (ORD (code));
      this.wr.NL    ();
      print(this, "/* abort */ ");
    END;
    reportfault(this, code);
  END abort;

PROCEDURE check_nil (this: U; code: RuntimeError) =
  (* IF (s0.A = NIL) THEN abort(code) *)
  VAR s0 := get(this);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("check_nil");
      this.wr.Int   (ORD (code));
      this.wr.NL    ();
      print(this, "/* check_nil */ ");
    END;
    print(this, "if(!" & paren(s0) & ")");
    reportfault(this, code);
  END check_nil;

PROCEDURE check_lo (this: U; type: IType; READONLY i: Target.Int; code: RuntimeError) =
  (* IF (s0.type < i) THEN abort(code) *)
  VAR s0 := cast(get(this), type);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("check_lo");
      this.wr.TName (type);
      this.wr.TInt  (TIntN.FromTargetInt(i, CG_Bytes[type]));
      this.wr.Int   (ORD (code));
      this.wr.NL    ();
      print(this, "/* check_lo */ ");
    END;
    print(this, "if(" & paren(s0) & "<" & TInt.ToText(i) & ")");
    reportfault(this, code);
  END check_lo;

PROCEDURE check_hi (this: U; type: IType; READONLY i: Target.Int; code: RuntimeError) =
  (* IF (i < s0.type) THEN abort(code) *)
  VAR s0 := cast(get(this), type);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("check_hi");
      this.wr.TName (type);
      this.wr.TInt  (TIntN.FromTargetInt(i, CG_Bytes[type]));
      this.wr.Int   (ORD (code));
      this.wr.NL    ();
      print(this, "/* check_hi */ ");
    END;
    print(this, "if(" & TInt.ToText(i) & "<" & paren(s0) & ")");
    reportfault(this, code);
  END check_hi;

PROCEDURE check_range (this: U; type: IType; READONLY a, b: Target.Int; code: RuntimeError) =
  (* IF (s0.type < a) OR (b < s0.type) THEN abort(code) *)
  VAR s0 := cast(get(this), type);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("check_range");
      this.wr.TInt  (TIntN.FromTargetInt(a, CG_Bytes[type]));
      this.wr.TInt  (TIntN.FromTargetInt(b, CG_Bytes[type]));
      this.wr.Int   (ORD(code));
      this.wr.NL    ();
      print(this, "/* check_range */ ");
    END;
    print(this, "if(" & paren(s0) & "<" & TInt.ToText(a) & "||" & TInt.ToText(b) & "<"  & paren(s0) & ")");
    reportfault(this, code);
  END check_range;

PROCEDURE check_index (this: U; type: IType; code: RuntimeError) =
  (* IF NOT (0 <= s1.type < s0.type) THEN
       abort(code)
     END;
     pop *)
  (* s0.type is guaranteed to be positive so the unsigned
     check (s0.W <= s1.W) is sufficient. *)
  VAR s0 := cast(get(this, 0), type);
      s1 := cast(get(this, 1), type);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("check_index");
      this.wr.TName (type);
      this.wr.Int   (ORD(code));
      this.wr.NL    ();
      print(this, "/* check_index */ ");
    END;
    print(this, "if(" & paren(s0) & "<=" & paren(s1) & ")");
    reportfault(this, code);
    pop(this);
  END check_index;

PROCEDURE check_eq (this: U; type: IType; code: RuntimeError) =
  (* IF (s0.type # s1.type) THEN
       abort(code);
       Pop (2) *)
  VAR s0 := cast(get(this, 0), type);
      s1 := cast(get(this, 1), type);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("check_eq");
      this.wr.TName (type);
      this.wr.Int   (ORD(code));
      this.wr.NL    ();
      print(this, "/* check_eq */ ");
    END;
    print(this, "if(" & paren(s0) & "!=" & paren(s1) & ")");
    reportfault(this, code);
  END check_eq;

PROCEDURE reportfault (this: U; code: RuntimeError) =
  (* 32: see M3CG.RuntimeError, RuntimeError.T *)
  VAR info := ORD (code) + this.line * 32;
  BEGIN
    <* ASSERT ORD (code) < 32 *> (* lose fault code not ok *)
    (* ASSERT this.line <= (LAST(INTEGER) DIV 32) *) (* losing line number ok *)
    print(this, this.report_fault & "(" & Fmt.Int(info) & ");");
  END reportfault;

(*---------------------------------------------------- address arithmetic ---*)

PROCEDURE add_offset (this: U; offset: INTEGER) =
  (* s0.A := s0.A + offset *)
  VAR s0 := cast(get(this, 0), Type.Addr);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("add_offset");
      this.wr.Int   (offset);
      this.wr.NL    ();
      print(this, "/* add_offset */ ");
    END;
    pop(this);
    push(this, Type.Addr, address_plus_offset(s0, offset));
  END add_offset;

PROCEDURE index_address (this: U; type: IType; size: INTEGER) =
  (* s1.A := s1.A + s0.type * size; pop *)
  VAR s0 := cast(get(this, 0), type);
      s1 := cast(get(this, 1), Type.Addr);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("index_address");
      this.wr.TName (type);
      this.wr.Int   (size);
      this.wr.NL    ();
      print(this, "/* index_address */ ");
    END;
    IF size = 0 THEN
      pop(this);
      <* ASSERT FALSE *>
    ELSE
      pop(this, 2);
      push(this, Type.Addr, "(((ADDRESS)(" & s1 & ")) + (" & Fmt.Int(size) & "*(" & s0 & ")))");
    END;
  END index_address;

(*------------------------------------------------------- PROCEDURE calls ---*)

PROCEDURE start_call (this: U) =
  BEGIN
    this.param_comma := "";
    this.static_link[this.in_proc_call] := NIL;
    INC(this.in_proc_call);
  END start_call;

PROCEDURE start_call_direct (this: U; p: Proc; level: INTEGER; type: Type) =
  (* begin a procedure call to a procedure at static level 'level'. *)
  VAR proc := NARROW(p, CProc);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("start_call_direct");
      this.wr.PName (proc);
      this.wr.Int   (level);
      this.wr.TName (type);
      this.wr.NL    ();
      print(this, "/* start_call_direct */ ");
    END;
    start_call(this);
    print(this, M3ID.ToText(proc.name));
  END start_call_direct;

PROCEDURE start_call_indirect (this: U; type: Type; callingConvention: CallingConvention) =
  (* begin a procedure call to a procedure at static level 'level'. *)
  VAR s0 := get(this, 0);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("start_call_indirect");
      this.wr.TName (type);
      this.wr.Txt   (callingConvention.name);
      this.wr.NL    ();
      print(this, "/* start_call_indirect */ ");
    END;
    start_call(this);
  END start_call_indirect;

PROCEDURE pop_param (this: U; type: MType) =
  (* pop s0 and make it the "next" parameter in the current call *)
  VAR s0 := cast(get(this, 0), type);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("pop_param");
      this.wr.TName (type);
      this.wr.NL    ();
      print(this, "/* pop_param */ ");
    END;
    print(this, this.param_comma);
    print(this, s0);
    this.param_comma := "";
  END pop_param;

PROCEDURE pop_struct (this: U; typeid: TypeUID; byte_size: ByteSize; alignment: Alignment) =
  (* pop s0 and make it the "next" parameter in the current call
   * NOTE: it is passed by value *)
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("pop_struct");
      this.wr.Tipe  (typeid);
      this.wr.Int   (byte_size);
      this.wr.Int   (alignment);
      this.wr.NL    ();
    END;
    print(this, "/* pop_struct */ ");
    (* UNDONE *)
  END pop_struct;

PROCEDURE pop_static_link (this: U) =
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("pop_static_link");
      this.wr.NL    ();
    END;
    print(this, "/* pop_static_link */ ");
    (* UNDONE *)
  END pop_static_link;

PROCEDURE call_direct (this: U; p: Proc; type: Type) =
  (* call the procedure identified by Proc p. The procedure
     returns a value of type type. *)
  VAR proc := NARROW(p, CProc);
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("call_direct");
      this.wr.PName (proc);
      this.wr.TName (type);
      this.wr.NL    ();
    END;
    print(this, "/* call_direct */ ");
    (* UNDONE *)
    DEC(this.in_proc_call);
  END call_direct;

PROCEDURE call_indirect (this: U; type: Type; callingConvention: CallingConvention) =
  (* call the procedure whose address is in s0.A and pop s0. The
     procedure returns a value of type type. *)
  BEGIN
    IF this.debug THEN
      this.wr.Cmd   ("call_indirect");
      this.wr.TName (type);
      this.wr.Txt   (callingConvention.name);
      this.wr.NL    ();
    END;
    print(this, "/* call_indirect */ ");
    (* UNDONE *)
    DEC(this.in_proc_call);
  END call_indirect;

(*------------------------------------------- procedure and closure types ---*)

PROCEDURE load_procedure (this: U; p: Proc) =
(* push; s0.A := ADDR (proc's body) *)
VAR proc := NARROW(p, CProc);
BEGIN
    print(this, "/* load_procedure */ ");
    push(this, Type.Addr, "0/*UNDONE*/");
    (* UNDONE *)
END load_procedure;

PROCEDURE load_static_link (this: U; p: Proc) =
  (* push; s0.A := (static link needed to call proc, NIL for top-level procs) *)
VAR proc := NARROW(p, CProc);
BEGIN
    print(this, "/* load_static_link */ ");
    push(this, Type.Addr, "0/*UNDONE*/");
    (* UNDONE *)
END load_static_link;

(*----------------------------------------------------------------- misc. ---*)

PROCEDURE comment (this: U; a, b, c, d: TEXT := NIL) =
VAR i: INTEGER := -1;
BEGIN
    Cmt2 (this, a, i);
    Cmt2 (this, b, i);
    Cmt2 (this, c, i);
    Cmt2 (this, d, i);
    Cmt1 (this, "\n", i);
END comment;

PROCEDURE Cmt1 (this: U; text: TEXT; VAR width: INTEGER) =
VAR ch: CHAR;
BEGIN
    IF (NOT this.debug OR text = NIL) THEN
        RETURN
    END;
    FOR i := 0 TO Text.Length (text) - 1 DO
        ch := Text.GetChar (text, i);
        IF (ch = '\n' OR ch = '\r') THEN
            this.wr.OutC (ch);
            width := -1;
        ELSE
            IF (width = -1) THEN
                this.wr.OutT ("\t# ");
                width := 0;
            END;
            this.wr.OutC (ch);
        END
    END;
END Cmt1;

PROCEDURE Cmt2 (this: U; text: TEXT; VAR width: INTEGER) =
BEGIN
    IF (NOT this.debug OR text = NIL) THEN
        RETURN
    END;
    IF text = NIL THEN
        RETURN
    END;
    Cmt1 (this, text, width);
    print(this, "/* comment: " & text & " */\n");
END Cmt2;

(*--------------------------------------------------------------- atomics ---*)

PROCEDURE store_ordered (this: U; ztype: ZType; mtype: MType; <*UNUSED*>order: MemoryOrder) =
(* Mem [s1.A].mtype := s0.ztype;
   pop (2) *)
VAR s0 := get(this, 0);
    s1 := get(this, 1);
BEGIN
    IF this.debug THEN
        this.wr.Cmd   ("store_ordered");
        this.wr.TName (ztype);
        this.wr.TName (mtype);
        this.wr.NL    ();
    END;
    print(this, "/* store_ordered => store */ ");
    store_helper(this, s0, ztype, s1, 0, mtype);
END store_ordered;

PROCEDURE load_ordered (this: U; mtype: MType; ztype: ZType; <*UNUSED*>order: MemoryOrder) =
(* s0.ztype := Mem [s0.A].mtype  *)
VAR s0 := get(this);
BEGIN
    IF this.debug THEN
        this.wr.Cmd   ("load_ordered");
        this.wr.TName (mtype);
        this.wr.TName (ztype);
        this.wr.NL    ();
    END;
    print(this, "/* load_ordered */ ");
    pop(this);
    load_helper(this, s0, 0, mtype, ztype);
END load_ordered;

PROCEDURE exchange (this: U; mtype: MType; ztype: ZType; <*UNUSED*>order: MemoryOrder) =
(* tmp := Mem [s1.A + offset].mtype;
   Mem [s1.A + offset].mtype := s0.ztype;
   s0.ztype := tmp;
   pop *)
BEGIN
    IF this.debug THEN
        this.wr.Cmd   ("exchange");
        this.wr.TName (mtype);
        this.wr.TName (ztype);
        this.wr.NL    ();
    END;
    print(this, "/* exchange */ ");
END exchange;

PROCEDURE compare_exchange (this: U; mtype: MType; ztype: ZType; result_type: IType;
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
    IF this.debug THEN
        this.wr.Cmd   ("compare_exchange");
        this.wr.TName (mtype);
        this.wr.TName (ztype);
        this.wr.TName (result_type);
        this.wr.NL    ();
    END;
    print(this, "/* compare_exchange */ ");
END compare_exchange;

PROCEDURE fence (this: U; <*UNUSED*>order: MemoryOrder) =
(*
 * x86: Exchanging any memory with any register is a serializing instruction.
 *)
BEGIN
    IF this.debug THEN
        this.wr.Cmd   ("fence");
        this.wr.NL    ();
    END;
    print(this, "/* fence */ ");
END fence;

CONST AtomicOpName = ARRAY AtomicOp OF TEXT { "add", "sub", "or", "and", "xor" };

PROCEDURE fetch_and_op (this: U; atomic_op: AtomicOp; mtype: MType; ztype: ZType;
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
    IF this.debug THEN
        this.wr.Cmd   ("fetch_and_op");
        this.wr.OutT  (AtomicOpName[atomic_op]);
        this.wr.TName (mtype);
        this.wr.TName (ztype);
        this.wr.NL    ();
    END;
    print(this, "/* fetch_and_op */ ");
END fetch_and_op;

BEGIN
(*
    BitSizeToEnumCGType[8] := M3CG.Type.Word8;
    BitSizeToEnumCGType[16] := M3CG.Type.Word16;
    BitSizeToEnumCGType[32] := M3CG.Type.Word32;
*)
END M3C.
