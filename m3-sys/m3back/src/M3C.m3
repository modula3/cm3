MODULE M3C;

IMPORT RefSeq, TextSeq, Wr, Text;
IMPORT M3CG, M3CG_Ops, Target, TFloat, TargetMap;
IMPORT RTIO, M3ID, TInt, ASCII, TextUtils, Cstdint, Long;
FROM TargetMap IMPORT CG_Bytes;
FROM M3CG IMPORT Name, ByteOffset, TypeUID, CallingConvention;
FROM M3CG IMPORT BitSize, ByteSize, Alignment, Frequency;
FROM M3CG IMPORT Label, Sign, BitOffset;
FROM M3CG IMPORT Type, ZType, AType, RType, IType, MType;
FROM M3CG IMPORT CompareOp, ConvertOp, RuntimeError, MemoryOrder, AtomicOp;
FROM Target IMPORT CGType;
FROM M3CG_Ops IMPORT ErrorHandler;

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

REVEAL
  U = Public BRANDED "M3C.U" OBJECT
        Err    : ErrorHandler := NIL;
        anonymousCounter := -1;
        c      : Wr.T := NIL;
        debug  := FALSE;
        stack  : TextSeq.T := NIL;
        params : TextSeq.T := NIL;

        (* import_procedure, import_global happens both outside and inside procedurs;
           When it happens inside, it presumed already imported in later ones.
           Record them here for later repetition. Hack. *)
        import_repeat : TextSeq.T := NIL; (* hack *)
        enum_type: TEXT := NIL;
        extra_scope_close_braces := ""; (* hack to account for locals/temps within code *)
        last_char_was_open_brace := FALSE;
        (*enum: Enum_t := NIL;*)
        enum_id: TEXT := NIL;
        enum_value: CARDINAL := 0;
        unit_name := "L_";
        handler_name_prefixes := ARRAY [FIRST(HandlerNamePieces) .. LAST(HandlerNamePieces)] OF TEXT{NIL, ..};
        param_count := 0;
        label := 0;
        static_link_id: M3ID.T;

        (* Every time we see a struct size, append it here.
           Later we will sort/unique and typedef them all.
           typedef struct { char a[size]; } m3struct_size_t;
        *)
        (* struct_sizes: IntSeq.T; FUTURE *)

        (* initialization support *)

        init_fields: TextSeq.T := NIL;
        current_init_offset: INTEGER := 0;
        initializer: TextSeq.T := NIL;

        (* initializers are aggregated into arrays to avoid
           redeclaring the types and generating new field names *)

        init_type := Type.Void;
        init_type_count := 0;

        (* line directive support *)
        file: TEXT := NIL;
        line: INTEGER := 0;
        line_directive := ""; (* combination of file/line *)
        nl_line_directive := "\n"; (* line_directive + "\n" *)
        last_char_was_newline := FALSE;
        suppress_line_directive: INTEGER := 0;

        static_link     := ARRAY [0..1] OF Var_t {NIL, NIL}; (* based on M3x86 *)
        current_proc    : Proc_t := NIL; (* based on M3x86 *)
        param_proc      : Proc_t := NIL; (* based on M3x86 *)
        in_proc         : BOOLEAN;      (* based on M3x86 *)
        in_proc_call : [0 .. 1] := 0; (* based on M3x86 *)
        report_fault: TEXT := NIL; (* based on M3x86 -- reportlabel, global_var *)
        width := 0;

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
        load_integer  := load_target_integer;
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

CONST HandlerNamePieces = ARRAY OF TEXT { "_M3_LINE_", "_I3_LINE_" };

(*
VAR BitSizeToEnumCGType := ARRAY [0..32] OF M3CG.Type { M3CG.Type.Void, .. };
*)

PROCEDURE SetLineDirective(u: U) =
VAR start := ARRAY BOOLEAN OF TEXT{" /* ", "#"}[output_line_directives];
    newline := ARRAY BOOLEAN OF TEXT{"", "\n"}[output_line_directives];
    end := ARRAY BOOLEAN OF TEXT{" */ ", "\n"}[output_line_directives];
BEGIN
    IF u.line > 0 AND u.file # NIL THEN
        u.line_directive := start & "line " & IntToDec(u.line) & " \"" & u.file & "\"" & end;
        u.nl_line_directive := newline & u.line_directive;
        IF u.last_char_was_newline THEN
            print(u, u.line_directive);
        ELSE
            print(u, u.nl_line_directive);
        END;
    ELSE
        u.line_directive := "";
        u.nl_line_directive := newline;
    END;
END SetLineDirective;

TYPE ReplacementName_t = RECORD
    id: M3ID.T;
    replacement_id: M3ID.T;
END;

<*UNUSED*>PROCEDURE Reverse(VAR a: ARRAY OF CHAR) =
VAR i := FIRST(a);
    j := LAST(a);
    t: CHAR;
BEGIN
    WHILE i < j DO
        t := a[i];
        a[i] := a[j];
        a[j] := t;
        INC(i);
        DEC(j);
    END;
END Reverse;

TYPE  INT32 = Cstdint.int32_t;
TYPE  INT64 = LONGINT;
TYPE UINT32 = Cstdint.uint32_t;
TYPE UINT64 = Long.T;
TYPE Base_t = [2..36];

PROCEDURE UInt64ToText(a: UINT64; base: Base_t): TEXT =
VAR buf: ARRAY [0..BITSIZE(a) + 1] OF CHAR;
    i := LAST(buf);
    c: UINT64;
    d: CHAR;
BEGIN
    REPEAT
        c := a MOD VAL(base, UINT64);
        IF c <= 9L THEN
            d := VAL(c + VAL(ORD('0'), UINT64), CHAR);
        ELSE
            d := VAL(c - 10L + VAL(ORD('A'), UINT64), CHAR);
        END;
        buf[i] := d;
        a := a DIV VAL(base, UINT64);
        DEC(i);
    UNTIL a = 0L;
    RETURN Text.FromChars(SUBARRAY(buf, i + 1, LAST(buf) - i));
END UInt64ToText;

PROCEDURE Int64ToText(a: INT64; base: Base_t): TEXT =
BEGIN
    IF a >= 0L THEN
        RETURN UInt64ToText(a, base);
    END;
    RETURN "-" & UInt64ToText((-(a + 1L)) + 1L, base);
END Int64ToText;

<*UNUSED*>PROCEDURE UInt64ToDec(a: UINT64): TEXT = BEGIN RETURN UInt64ToText(a, 10); END UInt64ToDec;
PROCEDURE UInt64ToHex(a: UINT64): TEXT = BEGIN RETURN UInt64ToText(a, 16); END UInt64ToHex;
<*UNUSED*>PROCEDURE  Int64ToDec(a:  INT64): TEXT = BEGIN RETURN  Int64ToText(a, 10); END Int64ToDec;
<*UNUSED*>PROCEDURE  Int32ToDec(a:  INT32): TEXT = BEGIN RETURN  Int64ToText(VAL(a, INT64), 10); END Int32ToDec;
PROCEDURE UInt32ToHex(a: UINT32): TEXT = BEGIN RETURN UInt64ToText(VAL(a, UINT64), 16); END UInt32ToHex;
PROCEDURE IntToHex(a: INTEGER): TEXT = BEGIN RETURN Int64ToText(VAL(a, INT64), 16); END IntToHex;
PROCEDURE IntToDec(a: INTEGER): TEXT = BEGIN RETURN Int64ToText(VAL(a, INT64), 10); END IntToDec;

<*UNUSED*>CONST Int32ToHex = UInt32ToHex;
<*UNUSED*>CONST Int64ToHex = UInt64ToHex;
CONST LabelToText = IntToHex;
CONST LabelToHex = IntToHex;

CONST reservedWords = ARRAY OF TEXT{
"__int8", "__int16", "__int32","__int64","__try","__except", "__finally",
"_cdecl","_stdcall","_fastcall","__cdecl","__stdcall","__fastcall",
"auto","const","double","float","int","short","struct","unsigned",
"break","continue","else","for","long","signed","switch","void",
"case","default","enum","goto","register","sizeof","typedef","volatile",
"char","do","extern","if","return","static","union","while",
"asm","dynamic_cast","namespace","reinterpret_cast","try",
"bool","explicit","new","static_cast","typeid",
"catch","false","operator","template","typename",
"class","friend","private","this","using",
"const_cast","inline","public","throw","virtual",
"delete","mutable","protected","true","wchar_t",
"and","bitand","compl","not_eq","or_eq","xor_eq",
"and_eq","bitor","not","or","xor",
"size_t","ptrdiff_t",
(* hack
The right fix here includes multiple passes.
  - import_procedure is called on unused function
    only declare them if they are otherwise referenced
Cstring.i3 declares strcpy and strcat incorrectly..on purpose.
*)
"strcpy", "strcat",

(* more incorrect declarations *)
"signgam", "cabs", "frexp", "modf"
};

(*
CONST suppressImports = ARRAY OF TEXT{
"strcpy", "strcat",
"signgam", "cabs", "frexp", "modf"
};
*)

VAR replacementNames_Inited := FALSE;
VAR replacementNames: ARRAY [FIRST(reservedWords) .. LAST(reservedWords)] OF ReplacementName_t;

PROCEDURE ReplaceName(id: M3ID.T): M3ID.T =
(* TODO: This is inefficient linear search. Use qsort. *)
BEGIN
    IF replacementNames_Inited = FALSE THEN
        FOR i := FIRST(reservedWords) TO LAST(reservedWords) DO
            WITH text = reservedWords[i] DO
                replacementNames[i] := ReplacementName_t{id := M3ID.Add(text), replacement_id := M3ID.Add("m3_" & text)};
            END;
        END;
        replacementNames_Inited := TRUE;
    END;
    FOR i := FIRST(replacementNames) TO LAST(replacementNames) DO
        IF replacementNames[i].id = id THEN
            RETURN replacementNames[i].replacement_id;
        END;
    END;
    RETURN id;
END ReplaceName;

PROCEDURE AnonymousCounter(u: U): INTEGER =
BEGIN
    INC(u.anonymousCounter, 1 + ORD(u.anonymousCounter = 385)); (* avoid "i386" -- really, it happened *)
    RETURN u.anonymousCounter;
END AnonymousCounter;

PROCEDURE GenerateName(u: U): Name =
BEGIN
    RETURN M3ID.Add("L_" & IntToDec(AnonymousCounter(u)));
END GenerateName;

PROCEDURE Proc_FixName(u: U; name: Name): Name =
BEGIN
    IF name = 0 OR Text.GetChar (M3ID.ToText (name), 0) = '*' THEN
        RETURN GenerateName(u);
    END;
    (* rename C names like int, short, void *)
    RETURN ReplaceName(name);
END Proc_FixName;

PROCEDURE Var_FixName(u: U; name: Name; imported_or_exported: BOOLEAN): Name =
VAR name2: Name;
BEGIN
    (* First try Proc_FixName; if it changes it, done. *)
    name2 := Proc_FixName(u, name);
    IF name2 # name THEN
        RETURN name2;
    END;
    (* workaround: begin/end_block should keep the names separate,
     * but they do nothing until import_procedure all moved up.
     * e.g. ETimer__Push() has parameter and local "t"
     *)
    IF NOT imported_or_exported AND name # u.static_link_id THEN
        name := M3ID.Add(M3ID.ToText(name) & "_L_" & IntToDec(AnonymousCounter(u)));
    END;
    RETURN name;
END Var_FixName;

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

TYPE expr_t = OBJECT
    is_const := FALSE;
    int_value: Target.Int;
    float_value: Target.Float;
    text_value: TEXT;
    type: M3CG.Type;
    ctext: TEXT;
    METHODS
        Add(right: expr_t): expr_t (*:= expr_add*);
        Sub(right: expr_t): expr_t (*:= expr_sub*);
        Mult(right: expr_t): expr_t (*:= expr_mult*);
        CText(): TEXT;
END;

(*
PROECURE Expr_FromTInt(i: Target.Int): expr_t;
PROECURE Expr_FromInt(i: INTEGER): expr_t;
PROECURE Expr_FromTFloat(f: Target.Float): expr_t;
PROECURE Expr_FromText(t: TEXT): expr_t;

PROCEDURE expr_add(<*UNUSED*>left, right: expr_t): expr_t = BEGIN RETURN NIL; END expr_add;
PROCEDURE expr_sub(<*UNUSED*>left, right: expr_t): expr_t = BEGIN RETURN NIL; END expr_sub;
PROCEDURE expr_mult(<*UNUSED*>left, right: expr_t): expr_t = BEGIN RETURN NIL; END expr_mult;
*)

TYPE Var_t = M3CG.Var OBJECT
    u: U;
    name: Name;
    type: Type;
    type_text: TEXT;
    const := FALSE;
    imported := FALSE;
    exported := FALSE;
    global := FALSE;
    proc: Proc_t;
    byte_size := -1; (* esp. for structs *)
    up_level := FALSE; (* local accessed from nested function *)
    is_static_link := FALSE; (* micro optimization -- uplevel but accessed directly *)

    METHODS
        Declare(): TEXT := Var_Declare;
        Name(): TEXT := Var_Name;
        Type(): TEXT := Var_Type;
        DeclareAndInitStructParamLocalValue(): TEXT := Var_DeclareAndInitStructParamLocalValue;
        Init(): Var_t := Var_Init;
END;

TYPE Param_t = Var_t OBJECT
    OVERRIDES
        Name := Param_Name;
        Type := Param_Type;
END;

(* TYPE LocalVar = Var_t;  FUTURE *)
(* TYPE GlobalVar = Var_t;  FUTURE *)

PROCEDURE Var_Init(var: Var_t): Var_t =
BEGIN
    var.name := Var_FixName(var.u, var.name, var.exported OR var.imported);
    RETURN var;
END Var_Init;

TYPE Proc_t = M3CG.Proc OBJECT
    name: Name;
    n_params: INTEGER := 0; (* FUTURE: remove this (same as NUMBER(params^)) *)
    n_params_without_static_link: INTEGER := 0; (* FUTURE: remove this (same as NUMBER(params^) - ORD(add_static_link)) *)
    return_type: Type;
    level: INTEGER := 0;
    callingConvention: CallingConvention;
    exported := FALSE;
    imported := FALSE;
    parent: Proc_t := NIL;
    params: REF ARRAY OF Var_t(*Param_t*);
    locals: RefSeq.T := NIL; (* Var_t *)
    uplevels := FALSE;
    is_exception_handler := FALSE;
    add_static_link := FALSE;
    declared_frame_type := FALSE;
    forward_declared_frame_type := FALSE;
    u: U;

    METHODS
        Locals_Size(): INTEGER := Proc_Locals_Size;
        Locals(i: INTEGER): Var_t := Proc_Locals;
        FrameName(): TEXT := Proc_FrameName;
        FrameType(): TEXT := Proc_FrameType;
        ForwardDeclareFrameType() := Proc_ForwardDeclareFrameType;
        Init(u: U): Proc_t := Proc_Init;
END;

PROCEDURE Proc_ForwardDeclareFrameType(proc: Proc_t) =
VAR u := proc.u;
BEGIN
    IF proc.forward_declared_frame_type THEN
        RETURN;
    END;
    print(u, "struct " & proc.FrameType() & ";");
    print(u, "typedef struct " & proc.FrameType() & " " & proc.FrameType() & ";");
    proc.forward_declared_frame_type := TRUE;
END Proc_ForwardDeclareFrameType;

PROCEDURE IsNameExceptionHandler(u: U; name: TEXT): BOOLEAN =
(* Is the name of the form unit_name + special + number + optionally underscore and another number ?
   See TryFinStmt.m3 *)
VAR ch: CHAR;
    underscore := FALSE;
    length, prefix_length := 0;
    prefix, end := "";
BEGIN
    length := Text.Length(name);
    FOR i := FIRST(u.handler_name_prefixes) TO LAST(u.handler_name_prefixes) DO
        prefix := u.handler_name_prefixes[i];
        prefix_length := Text.Length(prefix);
        IF length > prefix_length AND TextUtils.StartsWith(name, prefix) THEN
            end := Text.Sub(name, prefix_length);
            FOR i := 0 TO Text.Length(end) - 1 DO
                ch := Text.GetChar(end, i);
                IF ch = '_' THEN
                    IF underscore THEN
                        RETURN FALSE;
                    END;
                    underscore := TRUE;
                ELSIF NOT ch IN ASCII.Digits THEN
                    RETURN FALSE;
                END;
            END;
            RETURN TRUE;
        END;
    END;
    RETURN FALSE;
END IsNameExceptionHandler;

PROCEDURE Proc_Init(proc: Proc_t; u: U): Proc_t =
BEGIN
    proc.u := u;
    proc.name := Proc_FixName(proc.u, proc.name);
    proc.is_exception_handler := proc.level > 0 AND proc.n_params = 1 AND IsNameExceptionHandler(u, M3ID.ToText(proc.name));
    proc.n_params_without_static_link := proc.n_params;
    proc.add_static_link := proc.level > 0;
    INC(proc.n_params, ORD(proc.add_static_link));
    proc.locals := NEW(RefSeq.T).init();
    proc.params := NEW(REF ARRAY OF Var_t, proc.n_params);
    proc.ForwardDeclareFrameType(); (* TODO don't always do this *)
    RETURN proc;
END Proc_Init;

(*PROCEDURE Proc_FrameName(p: Proc_t): TEXT = BEGIN RETURN M3ID.ToText(p.name) & "_Frame"; END Proc_FrameName;*)
PROCEDURE Proc_FrameName(<*UNUSED*>p: Proc_t): TEXT = BEGIN RETURN "_frame"; END Proc_FrameName;
PROCEDURE Proc_FrameType(p: Proc_t): TEXT = BEGIN RETURN M3ID.ToText(p.name) & "_Frame_t"; END Proc_FrameType;
PROCEDURE Proc_Locals_Size(p: Proc_t): INTEGER = BEGIN RETURN p.locals.size(); END Proc_Locals_Size;
PROCEDURE Proc_Locals(p: Proc_t; i: INTEGER): Var_t = BEGIN RETURN NARROW(p.locals.get(i), Var_t); END Proc_Locals;

(*---------------------------------------------------------------------------*)


CONST Prefix = ARRAY OF TEXT {
(* It is unfortunate to #include anything -- slows down compilation;
   try to minimize/eliminate it. *)
"typedef char* ADDRESS;",
"typedef signed char INT8;",
"typedef unsigned char UINT8;",
"typedef short INT16;",
"typedef unsigned short UINT16;",
"typedef int INT32;",
"typedef unsigned int UINT32;",
"#if defined(_MSC_VER) || defined(__DECC) || defined(__int64)",
"typedef __int64 INT64;",
"typedef unsigned __int64 UINT64;",
"#define  M3_INT64(x) x##I64",
"#define M3_UINT64(x) x##UI64",
"#else",
"typedef long long INT64;",
"typedef unsigned long long UINT64;",
"#define  M3_INT64(x) x##LL",
"#define M3_UINT64(x) x##ULL",
"#endif",

"#if defined(_WIN64)",
"typedef __int64 ptrdiff_t;",
"typedef unsigned __int64 size_t;",
"#elif defined(_WIN32)",
"typedef int ptrdiff_t;",
"typedef unsigned size_t;",
"#elif defined(__APPLE__)",
"typedef unsigned long size_t;",
"#ifdef __LP64__",
"typedef long ptrdiff_t;",
"#else",
"typedef int ptrdiff_t;",
"#endif",
"#else",
"#include <stddef.h>", (* try to remove this, it is slow -- need size_t, ptrdiff_t *)
"#endif",

"#ifdef __cplusplus",
"extern \"C\" {",
"#endif",


"#if !(defined(_MSC_VER) || defined(__cdecl))",
"#define __cdecl /* nothing */",
"#endif",
"#if !defined(_MSC_VER) && !defined(__stdcall)",
"#define __stdcall /* nothing */",
"#endif",

(* string.h *)
"void* __cdecl memcpy(void*, const void*, size_t);",
"void* __cdecl memmove(void*, const void*, size_t);",
"void* __cdecl memset(void*, int, size_t);",

(* math.h *)
"double __cdecl floor(double);",
"double __cdecl ceil(double);",

(*"#include <limits.h>",*)

(* need multiple passes and forward declare all structs; temporary partial solution *)
"#define M3STRUCT(n) m3struct_##n##_t",
"typedef struct { volatile UINT8 a1;     } M3STRUCT(1);",
"typedef struct { volatile UINT16 a2;    } M3STRUCT(2);",
"typedef struct { volatile UINT8 a3[3];  } M3STRUCT(3);",
"typedef struct { volatile UINT32 a4;    } M3STRUCT(4);",
"typedef struct { volatile UINT8 a5[5];  } M3STRUCT(5);",
"typedef struct { volatile UINT16 a6[3]; } M3STRUCT(6);",
"typedef struct { volatile UINT8 a7[7];  } M3STRUCT(7);",
"typedef struct { volatile UINT64 a8;    } M3STRUCT(8);",
"#define M3STRUCT1(n) typedef struct { volatile UINT8 a[n]; } M3STRUCT(n);",
"#define M3STRUCT2(n) typedef struct { volatile UINT16 a[(n)/2]; } M3STRUCT(n);",
"#define M3STRUCT4(n) typedef struct { volatile UINT32 a[(n)/4]; } M3STRUCT(n);",
"#define M3STRUCT8(n) typedef struct { volatile UINT64 a[(n)/8]; } M3STRUCT(n);",
"M3STRUCT1(9)M3STRUCT2(10)M3STRUCT1(11)M3STRUCT4(12)",
"M3STRUCT1(13)M3STRUCT2(14)M3STRUCT1(15)M3STRUCT8(16)",
"M3STRUCT1(17)M3STRUCT2(18)M3STRUCT1(19)M3STRUCT4(20)",
"M3STRUCT1(21)M3STRUCT2(22)M3STRUCT1(23)M3STRUCT8(24)",
"M3STRUCT1(25)M3STRUCT2(26)M3STRUCT1(27)M3STRUCT4(28)",
"M3STRUCT1(29)M3STRUCT2(30)M3STRUCT1(31)M3STRUCT8(32)",
"M3STRUCT1(33)M3STRUCT2(34)M3STRUCT1(35)M3STRUCT4(36)",
"M3STRUCT1(37)M3STRUCT2(38)M3STRUCT1(39)M3STRUCT8(40)",
"#ifdef __cplusplus",
"#define M3_INIT",
"#define new m3_new",
"#define M3_DOTDOTDOT ...",
"#else",
"#define M3_INIT ={0}",
"#define M3_DOTDOTDOT",
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
(*
"/* WORD_T/INTEGER are always exactly the same size as a pointer.",
" * VMS sometimes has 32bit size_t/ptrdiff_t but 64bit pointers. */",
We really should not have this #ifdef, esp. the big list of architectures.
*)
"#if __INITIAL_POINTER_SIZE == 64 || defined(_WIN64) || defined(__LP64__) \\",
"    || defined(__alpha__) || defined(__arm64__) || defined(__hppa64__) \\",
"    || defined(__ia64__) || defined(__mips64__) || defined(__ppc64__) \\",
"    || defined(__s390x__) || defined(__sparcv9__) || defined(__x86_64__)",
"typedef INT64 INTEGER;",
"typedef UINT64 WORD_T;",
"void __cdecl RTHooks__ReportFault(ADDRESS module, INT64 code);",
"#else",
"typedef ptrdiff_t INTEGER;",
"typedef size_t WORD_T;",
"void __cdecl RTHooks__ReportFault(ADDRESS module, INT32 code);",
"#endif",
(* problem: size_t/ptrdiff_t could be int or long or long long or __int64 *)
(* RTHooks__ReportFault's signature varies, but it isn't imported *)
(*
"#if __INITIAL_POINTER_SIZE == 64",
"typedef __int64 INTEGER;",
"typedef unsigned __int64 WORD_T;",
"#else",
"typedef ptrdiff_t INTEGER;",
"typedef size_t WORD_T;",
"#endif",
*)

"typedef WORD_T* SET;",
"#define NIL ((ADDRESS)0)",
"typedef float REAL;",
"typedef double LONGREAL;",
"typedef /*long*/ double EXTENDED;",
"#define m3_extract_T(T) static T __stdcall m3_extract_##T(T value,WORD_T offset,WORD_T count){return((value>>offset)&~(((~(T)0))<<count));}",
"#define m3_insert_T(T) static T __stdcall m3_insert_##T(T x,T y,WORD_T offset,WORD_T count){T mask=(~((~(T)0)<<count))<<offset;return(((y<<offset)&mask)|(x&~mask));}",
"#define m3_sign_extend_T(T) static T __stdcall m3_sign_extend_##T(T value,WORD_T count){return(value|((value&(((T)-1)<<(count-1)))?(((T)-1)<<(count-1)):0));}",
"m3_sign_extend_T(INT32)",
"m3_sign_extend_T(INT64)",
"m3_sign_extend_T(UINT32)",
"m3_sign_extend_T(UINT64)",
"m3_extract_T(UINT32)",
"m3_extract_T(UINT64)",
"m3_insert_T(INT32)",
"m3_insert_T(INT64)",
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
"static void __stdcall m3_fence(void){ }",
(*"static void __stdcall m3_abort(const char* message, size_t length){fflush(NIL);write(2, message, length);abort();}\n",*)
"static WORD_T __stdcall m3_set_member(WORD_T elt,WORD_T*set){return(set[elt/SET_GRAIN]&(((WORD_T)1)<<(elt%SET_GRAIN)))!=0;}",
"static void __stdcall m3_set_union(WORD_T n_bits,WORD_T*c,WORD_T*b,WORD_T*a){WORD_T i,n_words = n_bits / SET_GRAIN;for (i = 0; i < n_words; ++i)a[i] = b[i] | c[i];}",
"static void __stdcall m3_set_intersection(WORD_T n_bits, WORD_T* c, WORD_T* b, WORD_T* a){WORD_T i,n_words = n_bits / SET_GRAIN;for (i = 0; i < n_words; ++i)a[i] = b[i] & c[i];}",
"static void __stdcall m3_set_difference(WORD_T n_bits,WORD_T*c,WORD_T*b,WORD_T*a){WORD_T i,n_words=n_bits/SET_GRAIN;for(i=0;i<n_words;++i)a[i]=b[i]&(~c[i]);}",
"static void __stdcall m3_set_sym_difference(WORD_T n_bits,WORD_T*c,WORD_T*b,WORD_T*a){WORD_T i,n_words=n_bits/SET_GRAIN;for(i=0;i<n_words;++i)a[i]=b[i]^c[i];}",
"static WORD_T __stdcall m3_set_eq(WORD_T n_bits,WORD_T*b,WORD_T*a){return(memcmp(a,b,n_bits/8)==0);}",
"static WORD_T __stdcall m3_set_ne(WORD_T n_bits,WORD_T*b,WORD_T*a){return(memcmp(a,b,n_bits/8)!=0);}",
"static WORD_T __stdcall m3_set_le(WORD_T n_bits,WORD_T*b,WORD_T*a){WORD_T n_words=n_bits/SET_GRAIN;WORD_T i;for(i=0;i<n_words;++i)if(a[i]&(~b[i]))return 0;return 1;}",
"static WORD_T __stdcall m3_set_lt(WORD_T n_bits,WORD_T*b,WORD_T*a){WORD_T n_words=n_bits/SET_GRAIN;WORD_T i,eq=0;for(i=0;i<n_words;++i)if(a[i]&(~b[i]))return 0;else eq|=(a[i]^b[i]);return(eq!=0);}",
"static WORD_T __stdcall m3_set_ge(WORD_T n_bits,WORD_T*b,WORD_T*a){return m3_set_le(n_bits,a,b);}",
"static WORD_T __stdcall m3_set_gt(WORD_T n_bits,WORD_T*b,WORD_T*a){return m3_set_lt(n_bits,a,b);}",
"#define M3_HIGH_BITS(a) ((~(WORD_T)0) << (a))",
"#define M3_LOW_BITS(a)  ((~(WORD_T)0) >> (SET_GRAIN - (a) - 1))",
"static void __stdcall m3_set_range(WORD_T b, WORD_T a, WORD_T*s){if(a>=b){WORD_T i,a_word=a/SET_GRAIN,b_word=b/SET_GRAIN,high_bits=M3_HIGH_BITS(a%SET_GRAIN),low_bits=M3_LOW_BITS(b%SET_GRAIN);if(a_word==b_word){s[a_word]|=(high_bits&low_bits);}else{s[a_word]|=high_bits;for(i=a_word+1;i<b_word;++i)s[i]=~(WORD_T)0;s[b_word]|=low_bits;}}}",
"static void __stdcall m3_set_singleton(WORD_T a,WORD_T*s){s[a/SET_GRAIN]|=(((WORD_T)1)<<(a%SET_GRAIN));}",
"#define m3_shift_T(T) static T m3_shift_##T(T value,INTEGER shift){if((shift>=(INTEGER)(sizeof(T)*8))||(shift<=(INTEGER)-(sizeof(T)*8)))value=0;else if(shift>0)value<<=shift;else if(shift<0)value>>=-shift;return value;}",
"m3_shift_T(UINT32)",
"m3_shift_T(UINT64)",
"/* return positive form of a negative value, avoiding overflow */",
"/* T should be an unsigned type */",
"#define M3_POS(T, a) (((T)-((a) + 1)) + 1)",
"#define m3_rotate_left_T(T)  static T __stdcall m3_rotate_left_##T (T a, int b) { return ((a << b) | (a >> ((sizeof(a) * 8) - b))); }",
"#define m3_rotate_right_T(T) static T __stdcall m3_rotate_right_##T(T a, int b) { return ((a >> b) | (a << ((sizeof(a) * 8) - b))); }",
"#define m3_rotate_T(T)       static T __stdcall m3_rotate_##T(T a, int b) { b &= ((sizeof(a) * 8) - 1); if (b > 0) a = m3_rotate_left_##T(a, b); else if (b < 0) a = m3_rotate_right_##T(a, -b); return a; }",
"#define m3_abs_T(T) static T __stdcall m3_abs_##T(T a) { return ((a < 0) ? (-a) : a); }",
"#define m3_min_T(T) static T __stdcall m3_min_##T(T a, T b) { return ((a < b) ? a : b); }",
"#define m3_max_T(T) static T __stdcall m3_max_##T(T a, T b) { return ((a > b) ? a : b); }",
"#define m3_min_max_abs_T(T) m3_min_T(T) m3_max_T(T) m3_abs_T(T)",
"#define m3_div_T(T) static T __stdcall m3_div_##T(T a, T b) \\",
"{ \\",
"  int aneg = (a < 0); \\",
"  int bneg = (b < 0); \\",
"  if (aneg == bneg || a == 0 || b == 0) \\",
"    return (a / b); \\",
"  else \\",
"  { \\",
"    /* round negative result down by rounding positive result up */ \\",
"    /* unsigned math is much better defined, see gcc -Wstrict-overflow=4 */ \\",
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
"m3_div_T(INT64)",
"m3_mod_T(INT64)",
"m3_rotate_left_T(UINT64)",
"m3_rotate_right_T(UINT64)",
"m3_rotate_T(UINT64)",
"m3_min_max_abs_T(INT32)",
"m3_min_max_abs_T(UINT32)",
"m3_min_max_abs_T(INT64)",
"m3_min_max_abs_T(UINT64)",
"m3_min_max_abs_T(REAL)",
"m3_min_max_abs_T(LONGREAL)",
"m3_min_max_abs_T(EXTENDED)",
"double __cdecl floor(double);",
"double __cdecl ceil(double);",
"INT64 __cdecl llroundl(long double);",
"static INT64 __stdcall m3_floor(EXTENDED f) { return floor(f); }",
"static INT64 __stdcall m3_ceil(EXTENDED f) { return ceil(f); }",
"static INT64 __stdcall m3_trunc(EXTENDED f) { return (INT64)f; }",
"static INT64 __stdcall m3_round(EXTENDED f) { return (INT64)llroundl(f); }",

"#define m3_pop_T(T) static void __stdcall m3_pop_##T(volatile T a) { }",
"static void __stdcall m3_pop_STRUCT(volatile const void* a) { }",
"m3_pop_T(INT8)",
"m3_pop_T(INT16)",
"m3_pop_T(INT32)",
"m3_pop_T(INT64)",
"m3_pop_T(UINT8)",
"m3_pop_T(UINT16)",
"m3_pop_T(UINT32)",
"m3_pop_T(UINT64)",
"m3_pop_T(ADDRESS)",
"m3_pop_T(SET)",
"m3_pop_T(REAL)",
"m3_pop_T(LONGREAL)",
"m3_pop_T(EXTENDED)",
""};

<*NOWARN*>CONST Suffix = ARRAY OF TEXT {
"#ifdef __cplusplus",
"} /* extern \"C\" */",
"#endif"
};

CONST intLiteralPrefix = ARRAY CGType OF TEXT {
    "",  "",
    "", "",
    "", "",
    "M3_UINT64(", "M3_INT64(",
    NIL, ..
};

CONST intLiteralSuffix = ARRAY CGType OF TEXT {
    "U",  "",
    "U", "",
    "U", "",
    ")", ")",
    NIL, ..
};

CONST typeToText = ARRAY CGType OF TEXT {
    "UINT8",  "INT8",
    "UINT16", "INT16",
    "UINT32", "INT32",
    "UINT64", "INT64",
    "REAL", "LONGREAL", "EXTENDED",
    "ADDRESS",
    "STRUCT",
    "void"
};

TYPE IntegerTypes = [Type.Word8 .. Type.Int64];

CONST typeIsUnsigned = ARRAY IntegerTypes OF BOOLEAN {
    TRUE, FALSE,
    TRUE, FALSE,
    TRUE, FALSE,
    TRUE, FALSE
};

CONST typeToUnsigned = ARRAY IntegerTypes OF IntegerTypes {
    Type.Word8, Type.Word8,
    Type.Word16, Type.Word16,
    Type.Word32, Type.Word32,
    Type.Word64, Type.Word64
};

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
    IF u.debug THEN
        RTIO.PutText("suppress_line_directive now " & IntToDec(u.suppress_line_directive) & " due to " & reason & "\n");
        RTIO.Flush();
    END
    *)
END SuppressLineDirective;

PROCEDURE print(u: U; text: TEXT) = <*FATAL ANY*>
VAR length: INTEGER;
    text_last_char := '\000';
BEGIN
    IF text = NIL THEN
        RETURN;
    END;

    length := Text.Length(text);
    IF length = 0 THEN
        RETURN;
    END;

    text_last_char := Text.GetChar(text, length - 1);
    u.last_char_was_open_brace := text_last_char = '{';

    IF output_extra_newlines AND Text.FindChar(text, '\n') = -1 THEN
        Wr.PutText(u.c, text & "\n");
        u.last_char_was_newline := TRUE;
    ELSE
        Wr.PutText(u.c, text);
        u.last_char_was_newline := FALSE;
    END;

    IF text = u.line_directive OR text = u.nl_line_directive THEN
        u.width := 0;
        u.last_char_was_newline := TRUE;
        RETURN;
    END;

    IF (*u.suppress_line_directive < 1 AND*) text_last_char = '\n' THEN
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
    IF u.width < 1000 THEN
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
    (* u.debug := TRUE; *)
    u.c := cfile;
    u.init_fields := NEW(TextSeq.T).init();
    u.initializer := NEW(TextSeq.T).init();
    u.stack := NEW(TextSeq.T).init();
    u.params := NEW(TextSeq.T).init();
    u.import_repeat := NEW(TextSeq.T).init();
    (* u.struct_sizes := NEW(IntSeq.T).init(); FUTURE *)
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
    u.Err := p;
END set_error_handler;

(*----------------------------------------------------- compilation units ---*)

PROCEDURE begin_unit(u: U; <*UNUSED*>optimize: INTEGER) =
(* called before any other method to initialize the compilation unit *)
BEGIN
    print(u, " /* begin unit */\n");
    print(u, "/* M3_TARGET = " & Target.System_name & " */ ");
    print(u, "/* M3_WORDSIZE = " & IntToDec(Target.Word.size) & " */ ");
    u.static_link_id := M3ID.Add("_static_link");
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

PROCEDURE import_unit(u: U; <*UNUSED*>name: Name) =
(* note that the current compilation unit imports the interface 'name' *)
BEGIN
    u.comment("import_unit");
END import_unit;

PROCEDURE export_unit(u: U; <*UNUSED*>name: Name) =
(* note that the current compilation unit exports the interface 'name' *)
BEGIN
    u.comment("export_unit");
END export_unit;

(*------------------------------------------------ debugging line numbers ---*)

PROCEDURE set_source_file(u: U; file: TEXT) =
(* Sets the current source file name. Subsequent statements
   and expressions are associated with this source location. *)
BEGIN
    u.comment("set_source_file");
    u.file := file;
    SetLineDirective(u);
END set_source_file;

PROCEDURE set_source_line(u: U; line: INTEGER) =
(* Sets the current source line number. Subsequent statements
and expressions are associated with this source location. *)
BEGIN
    u.comment("set_source_line");
    u.line := line;
    SetLineDirective(u);
END set_source_line;

(*------------------------------------------- debugging type declarations ---*)

<*NOWARN*>PROCEDURE declare_typename(u: U; typeid: TypeUID; name: Name) =
BEGIN
    u.comment("declare_typename");
    (*
    print(u, "typedef M" & TypeidToHex(typeid) & " " & M3ID.ToText(name) & ";\n");
    *)
END declare_typename;

(*
PROCEDURE TypeIDToText(x: INTEGER): TEXT =
BEGIN
    RETURN "M" & Fmt.Unsigned(x);
END TypeIDToText;
*)

<*NOWARN*>PROCEDURE declare_array(u: U; typeid, index_typeid, element_typeid: TypeUID; total_bit_size: BitSize) =
BEGIN
    u.comment("declare_array");
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
      print(u, IntToDec(total_bit_size DIV element_type.bit_size));
      print(u, "];}");
      print(u, TypeIDToText(typeid));
      print(u, ";");
    END;
*)
  END declare_array;

<*NOWARN*>PROCEDURE declare_open_array(u: U; typeid, element_typeid: TypeUID; bit_size: BitSize) =
BEGIN
    u.comment("declare_open_array");
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
            print(u, IntToDec((bit_size - Target.Integer.size) DIV Target.Integer.size));
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

<*NOWARN*>PROCEDURE declare_enum(u: U; typeid: TypeUID; n_elts: INTEGER; bit_size: BitSize) =
BEGIN
    u.comment("declare_enum");
    SuppressLineDirective(u, n_elts, "declare_enum n_elts");
    <* ASSERT bit_size = 8 OR bit_size = 16 OR bit_size = 32 *>
    (*
    WITH type = NEW(Enum_t, typeid := typeid, max := n_elts - 1, cg_type := BitSizeToEnumCGType[bit_size]) DO
        <* ASSERT u.enum = NIL *>
        u.enum := type;
        EVAL Type_Init(type);
        u.enum_id := TypeIDToText(typeid);
        u.enum_value := 0;
        u.enum_type := "UINT" & IntToDec(bit_size);
        print(u, "typedef " & u.enum_type & " " & u.enum_id & ";");
    END;
*)
END declare_enum;

<*NOWARN*>PROCEDURE declare_enum_elt(u: U; name: Name) =
BEGIN
    u.comment("declare_enum_elt");
    SuppressLineDirective(u, -1, "declare_enum_elt");
(*
    print(u, "#define " & u.enum_id & "_" & M3ID.ToText(name) & " ((" & u.enum_type & ")" & IntToDec(u.enum_value) & ")\n");
    INC(u.enum_value);
    IF u.enum_value = u.enum.max + 1 THEN
        u.enum := NIL;
        u.enum_id := NIL;
        u.enum_type := NIL;
        u.enum_value := 10000;
    END;
*)
  END declare_enum_elt;

<*NOWARN*>PROCEDURE declare_packed(u: U; typeid: TypeUID; bit_size: BitSize; base: TypeUID) =
BEGIN
    u.comment("declare_packed");
END declare_packed;

<*NOWARN*>PROCEDURE declare_record(u: U; typeid: TypeUID; bit_size: BitSize; n_fields: INTEGER) =
BEGIN
    u.comment("declare_record");
    SuppressLineDirective(u, n_fields, "declare_record n_fields");
END declare_record;

<*NOWARN*>PROCEDURE declare_field(u: U; name: Name; offset: BitOffset; bit_size: BitSize; typeid: TypeUID) =
BEGIN
    u.comment("declare_field");
    SuppressLineDirective(u, -1, "declare_field");
END declare_field;

<*NOWARN*>PROCEDURE declare_set(u: U; typeid, domain: TypeUID; bit_size: BitSize) =
BEGIN
    u.comment("declare_set");
END declare_set;

<*NOWARN*>PROCEDURE declare_subrange(u: U; typeid, domain: TypeUID; READONLY min, max: Target.Int; bit_size: BitSize) =
BEGIN
    u.comment("declare_subrange");
END declare_subrange;

<*NOWARN*>PROCEDURE declare_pointer(u: U; typeid, target: TypeUID; brand: TEXT; traced: BOOLEAN) =
BEGIN
    u.comment("declare_pointer");
END declare_pointer;

<*NOWARN*>PROCEDURE declare_indirect(u: U; typeid, target: TypeUID) =
BEGIN
    u.comment("declare_indirect");
END declare_indirect;

<*NOWARN*>PROCEDURE declare_proctype(u: U; typeid: TypeUID; n_formals: INTEGER; result: TypeUID; n_raises: INTEGER; callingConvention: CallingConvention) =
BEGIN
    u.comment("declare_proctype");
    (* SuppressLineDirective(u, n_formals + (ORD(n_raises >= 0) * n_raises), "declare_proctype n_formals + n_raises"); *)
END declare_proctype;

<*NOWARN*>PROCEDURE declare_formal(u: U; name: Name; typeid: TypeUID) =
BEGIN
    u.comment("declare_formal");
    (* SuppressLineDirective(u, -1, "declare_formal"); *)
END declare_formal;

<*NOWARN*>PROCEDURE declare_raises(u: U; name: Name) =
BEGIN
    u.comment("declare_raises");
    (* SuppressLineDirective(u, -1, "declare_raises"); *)
END declare_raises;

<*NOWARN*>PROCEDURE declare_object(u: U; typeid, super: TypeUID; brand: TEXT; traced: BOOLEAN; n_fields, n_methods: INTEGER; field_size: BitSize) =
BEGIN
    u.comment("declare_object");
    (* SuppressLineDirective(u, n_fields + n_methods, "declare_object n_fields + n_methods"); *)
END declare_object;

<*NOWARN*>PROCEDURE declare_method(u: U; name: Name; signature: TypeUID) =
BEGIN
    u.comment("declare_method");
    SuppressLineDirective(u, -1, "declare_method");
END declare_method;

<*NOWARN*>PROCEDURE declare_opaque(u: U; typeid, super: TypeUID) =
BEGIN
    u.comment("declare_opaque");
END declare_opaque;

<*NOWARN*>PROCEDURE reveal_opaque(u: U; lhs, rhs: TypeUID) =
BEGIN
    u.comment("reveal_opaque");
END reveal_opaque;

<*NOWARN*>PROCEDURE declare_exception(u: U; name: Name; arg_type: TypeUID; raise_proc: BOOLEAN; base: M3CG.Var; offset: INTEGER) =
BEGIN
    u.comment("declare_exception");
END declare_exception;

(*--------------------------------------------------------- runtime hooks ---*)

<*NOWARN*>PROCEDURE set_runtime_proc(u: U; name: Name; p: M3CG.Proc) =
BEGIN
    u.comment("set_runtime_proc");
END set_runtime_proc;

(*------------------------------------------------- variable declarations ---*)

PROCEDURE import_global(u: U; name: Name; <*UNUSED*>byte_size: ByteSize; <*UNUSED*>alignment: Alignment; type: Type; <*UNUSED*>typeid: TypeUID): M3CG.Var =
VAR var := NEW(Var_t, u := u, type := type, name := name, imported := TRUE).Init();
    declaration: TEXT;
BEGIN
    u.comment("import_global");
    declaration := "extern " & typeToText[type] & " " & M3ID.ToText(var.name) & ";";
    IF u.in_proc THEN
        u.import_repeat.addhi(declaration);
        ExtraScope_Open(u);
    END;
    print(u, declaration);
    RETURN var;
END import_global;

CONST Const = ARRAY BOOLEAN OF TEXT{"", " const "};

PROCEDURE declare_segment(u: U; name: Name; <*UNUSED*>typeid: TypeUID; const: BOOLEAN): M3CG.Var =
VAR var := NEW(Var_t, u := u, name := name, const := const).Init();
    fixed_name := var.name;
    text: TEXT := NIL;
    length := 0;
    const_text := Const[const];
BEGIN
    u.comment("declare_segment");
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
            FOR i := FIRST(HandlerNamePieces) TO LAST(HandlerNamePieces) DO
                u.handler_name_prefixes[i] := text & HandlerNamePieces[i];
            END;
        END;
    END;
    text := M3ID.ToText(fixed_name);
    print(u, "struct " & text & "_t;");
    print(u, "typedef struct " & text & "_t " & text & "_t;");
    print(u, const_text & "static " & text & "_t " & text & ";");

    IF u.report_fault = NIL AND NOT const THEN (* See M3x86.m3 *)
        u.report_fault := M3ID.ToText(var.name) & "_CRASH";
        print(u, "void __cdecl " & u.report_fault & "(UINT32 code) { RTHooks__ReportFault((ADDRESS)&" & M3ID.ToText(var.name) & ",code);}");
    END;

    RETURN var;
  END declare_segment;

PROCEDURE bind_segment(u: U; v: M3CG.Var; byte_size: ByteSize; <*UNUSED*>alignment: Alignment; <*UNUSED*>type: Type; <*UNUSED*>exported, inited: BOOLEAN) =
VAR var := NARROW(v, Var_t);
BEGIN
    u.comment("bind_segment");
    print(u, " /* bind_segment */ ");
    var.byte_size := byte_size;
END bind_segment;

PROCEDURE declare_global(u: U; name: Name; byte_size: ByteSize; alignment: Alignment;
                         type: Type; typeid: TypeUID; exported, inited: BOOLEAN): M3CG.Var =
BEGIN
    print(u, " /* declare_global */ ");
    RETURN DeclareGlobal(u, name, byte_size, alignment, type, typeid, exported, inited, FALSE);
END declare_global;

PROCEDURE declare_constant(u: U; name: Name; byte_size: ByteSize; alignment: Alignment;
                           type: Type; typeid: TypeUID; exported, inited: BOOLEAN): M3CG.Var =
BEGIN
    print(u, " /* declare_constant */ ");
    RETURN DeclareGlobal(u, name, byte_size, alignment, type, typeid, exported, inited, TRUE);
END declare_constant;

PROCEDURE DeclareGlobal(u: U; name: Name; byte_size: ByteSize; <*UNUSED*>alignment: Alignment; type: Type; <*UNUSED*>typeid: TypeUID; exported: BOOLEAN; <*UNUSED*>inited: BOOLEAN; const: BOOLEAN): M3CG.Var =
CONST DeclTag = ARRAY BOOLEAN OF TEXT { "declare_global", "declare_constant" };
VAR   var := NEW(Var_t, u := u, type := type, name := name, const := const,
                 (*inited := inited, typeid := typeid, alignment := alignment,*)
                 exported := exported, global := TRUE,
                 proc := u.current_proc, byte_size := byte_size).Init();
BEGIN
    u.comment(DeclTag [const]);
    print(u, var.Declare() & " M3_INIT;");
    RETURN var;
END DeclareGlobal;

PROCEDURE ExtraScope_Open(u: U) =
BEGIN
    IF NOT u.last_char_was_open_brace THEN
      print(u, "{");
      u.extra_scope_close_braces := u.extra_scope_close_braces & "}";
    END;
END ExtraScope_Open;

PROCEDURE ExtraScope_Close(u: U) =
BEGIN
    print(u, u.extra_scope_close_braces);
    u.extra_scope_close_braces := "";
END ExtraScope_Close;

PROCEDURE Struct(size: INTEGER): TEXT =
BEGIN
    IF size <= 40 THEN
        RETURN "M3STRUCT(" & IntToDec(size) & ")";
    END;
    RETURN "struct{char a[" & IntToDec(size) & "];}";
END Struct;

PROCEDURE Var_DeclareAndInitStructParamLocalValue(var: Var_t): TEXT =
BEGIN
    IF var.type # Type.Struct THEN
        RETURN NIL;
    END;
    RETURN Struct(var.byte_size) & " " & M3ID.ToText(var.name) & "=*_param_struct_pointer_" & M3ID.ToText(var.name) & ";";
END Var_DeclareAndInitStructParamLocalValue;

PROCEDURE Param_Type(var: Var_t): TEXT =
BEGIN
    IF var.type_text # NIL THEN RETURN var.type_text; END;
    IF var.type # Type.Struct THEN RETURN typeToText[var.type]; END;
    RETURN Struct(var.byte_size) & "*";
END Param_Type;

PROCEDURE Var_Type(var: Var_t): TEXT =
BEGIN
    IF var.type_text # NIL THEN RETURN var.type_text; END;
    IF var.type # Type.Struct THEN RETURN typeToText[var.type]; END;
    RETURN Struct(var.byte_size);
END Var_Type;

PROCEDURE Param_Name(var: Var_t): TEXT =
BEGIN
    IF var.type # Type.Struct THEN RETURN M3ID.ToText(var.name); END;
    RETURN "_param_struct_pointer_" & M3ID.ToText(var.name);
END Param_Name;

PROCEDURE Var_Name(var: Var_t): TEXT =
BEGIN
    RETURN M3ID.ToText(var.name);
END Var_Name;

PROCEDURE Var_Declare(var: Var_t): TEXT =
VAR static := ARRAY BOOLEAN OF TEXT{"", "static "}[var.global AND NOT var.exported];
BEGIN
    RETURN static & var.Type() & " " & var.Name();
END Var_Declare;

PROCEDURE declare_local(u: U; name: Name; byte_size: ByteSize; <*UNUSED*>alignment: Alignment; type: Type; <*UNUSED*>typeid: TypeUID; <*UNUSED*>in_memory: BOOLEAN; up_level: BOOLEAN; <*UNUSED*>frequency: Frequency): M3CG.Var =
VAR var := NEW(Var_t, u := u, type := type, name := name, up_level := up_level,
               byte_size := byte_size, proc := u.current_proc).Init();
BEGIN
    u.comment("declare_local");
    IF u.in_proc THEN
        ExtraScope_Open(u);
        print(u, var.Declare() & " M3_INIT;");
        <* ASSERT up_level = FALSE *>
    ELSE
        IF up_level THEN
            u.current_proc.uplevels := TRUE;
        END;
        u.current_proc.locals.addhi(var);
    END;
    RETURN var;
END declare_local;

TYPE FunctionPrototype_t = { Declare, Define };

PROCEDURE function_prototype(proc: Proc_t; kind: FunctionPrototype_t): TEXT =
VAR params := proc.params;
    (* is_exception_handler := proc.is_exception_handler; *)
    text := typeToText[proc.return_type] & " __cdecl " & M3ID.ToText(proc.name);
    after_param: TEXT;
    ansi := TRUE (*NOT is_exception_handler*);
    define_kr := NOT ansi AND kind = FunctionPrototype_t.Define;
    kr_part2 := "";
BEGIN
    IF NUMBER (params^) = 0 THEN
        text := text & "(void)";
    ELSIF NOT ansi AND NOT define_kr THEN
        text := text & "()";
    ELSE
        text := text & "(";
        FOR i := FIRST(params^) TO LAST(params^) DO
            WITH param = params[i] DO
                IF i # LAST(params^) THEN
                    after_param := ",";
                ELSE
                    after_param := ")";
                END;
                IF ansi THEN
                    text := text & param.Declare() & after_param;
                ELSIF define_kr THEN
                    text := text & param.Name() & after_param;
                    kr_part2 := kr_part2 & "    " & param.Type() & " " & param.Name() & ";\n";
                END;
            END;
        END;
    END;
    RETURN text & kr_part2;
END function_prototype;

PROCEDURE last_param(u: U) =
VAR proc := u.param_proc;
     prototype: TEXT;
     param: Var_t;
BEGIN
    IF u.debug THEN
        RTIO.PutText("last_param in " & M3ID.ToText(u.param_proc.name) & "\n");
        RTIO.Flush();
    END;

    IF proc.add_static_link THEN
        param := NARROW(internal_declare_param(
            u,
            u.static_link_id,
            CG_Bytes[Type.Addr], (* size *)
            CG_Bytes[Type.Addr], (* alignment *)
            Type.Addr,
            UID_ADDR,
            FALSE(*?*), (* in memory *)
            TRUE, (* up_level, sort of -- needs to be stored, but is never written, can be read from direct parameter
                     This get it stored in begin_function. *)
            M3CG.Never,
            NARROW(proc.parent, Proc_t).FrameType() & "*"), Var_t);
        (* static link is first instead of last
         * it is always present, but activation isn't
        IF proc.is_exception_handler THEN
            <* ASSERT NUMBER(proc.params^) = 2 *>
            proc.params[1] := proc.params[0];
            proc.params[0] := param;
        END;
        *)
    END;

    prototype := function_prototype(u.param_proc, FunctionPrototype_t.Declare) & ";";
    IF u.in_proc THEN
        u.import_repeat.addhi(prototype);
        ExtraScope_Open(u);
    END;
    print(u, prototype);
END last_param;

PROCEDURE internal_declare_param(u: U; name: Name; byte_size: ByteSize; <*UNUSED*>alignment: Alignment;
                                 type: Type; <*UNUSED*>typeid: TypeUID; <*UNUSED*>in_memory: BOOLEAN; up_level: BOOLEAN;
                                 <*UNUSED*>frequency: Frequency; type_text: TEXT): M3CG.Var =
VAR function := u.param_proc;
    var := NEW(Param_t, u := u, type := type, name := name, byte_size := byte_size, up_level := up_level, proc := function, type_text := type_text).Init();
BEGIN
    u.comment("declare_param");
    function.params[u.param_count] := var;
    function.uplevels := function.uplevels OR up_level;
    SuppressLineDirective(u, -1, "declare_param");
    INC(u.param_count);
    IF name # u.static_link_id AND u.param_count = function.n_params_without_static_link THEN
        last_param(u);
    END;
    RETURN var;
END internal_declare_param;

PROCEDURE declare_param(u: U; name: Name; byte_size: ByteSize; alignment: Alignment;
                        type: Type; typeid: TypeUID; in_memory, up_level: BOOLEAN;
                        frequency: Frequency): M3CG.Var =
BEGIN
    RETURN internal_declare_param(u, name, byte_size, alignment, type, typeid, in_memory, up_level, frequency, NIL);
END declare_param;

PROCEDURE declare_temp(u: U; byte_size: ByteSize; alignment: Alignment; type: Type; in_memory:BOOLEAN): M3CG.Var =
BEGIN
    u.comment("declare_temp");
    RETURN declare_local(u, 0, byte_size, alignment, type, -1, in_memory, FALSE, M3CG.Always);
END declare_temp;

PROCEDURE free_temp(u: U; <*NOWARN*>v: M3CG.Var) =
BEGIN
    u.comment("free_temp");
END free_temp;

(*---------------------------------------- static variable initialization ---*)

PROCEDURE begin_init(u: U; <*UNUSED*>v: M3CG.Var) =
BEGIN
    u.comment("begin_init");
    u.current_init_offset := 0;
    SuppressLineDirective(u, 1, "begin_init");
END begin_init;

PROCEDURE end_init(u: U; v: M3CG.Var) =
VAR var := NARROW(v, Var_t);
    init_fields := u.init_fields;
    initializer := u.initializer;
    var_name := M3ID.ToText(var.name);
    comma := "";
    const := Const[var.const];
BEGIN
    u.comment("end_init");
    init_to_offset(u, var.byte_size);
    end_init_helper(u);

    print(u, "struct " & var_name & "_t{");
    WHILE init_fields.size() > 0 DO
        print(u, init_fields.remlo());
    END;
    print(u, "};");

    print(u, "static " & const & var_name & "_t " & var_name & "={");
    WHILE initializer.size() > 0 DO
        print(u, comma & initializer.remlo());
        comma := ",";
    END;
    print(u, "};");
    SuppressLineDirective(u, -1, "end_init");
END end_init;

PROCEDURE init_to_offset(u: U; offset: ByteOffset) =
VAR pad := offset - u.current_init_offset;
    init_fields := u.init_fields;
    initializer := u.initializer;
    comment := "";
BEGIN
    IF u.debug THEN
        comment := " /* padding_intializer start=" & IntToDec(u.current_init_offset) & " size=" & IntToDec(pad) & " end=" & IntToDec(offset) & " */ ";
    END;
    (* print(u, " /* init_to_offset offset=" & IntToDec(offset) & " */ "); *)
    <* ASSERT offset >= u.current_init_offset *>
    <* ASSERT pad >= 0 *>
    <* ASSERT u.current_init_offset >= 0 *>
    IF pad > 0 THEN
        end_init_helper(u);
        init_fields.addhi(comment & "char " & M3ID.ToText(GenerateName(u)) & "[" & IntToDec(pad) & "];");
        initializer.addhi(comment & "{0}");
    END;
END init_to_offset;

PROCEDURE end_init_helper(u: U) =
BEGIN
    IF u.init_type_count > 0 THEN
        u.init_fields.addhi("[" & IntToDec(u.init_type_count) & "];");
    END;
    u.init_type_count := 0;
END end_init_helper;

PROCEDURE init_helper(u: U; offset: ByteOffset; type: Type; comment: TEXT := "") =
BEGIN
    init_to_offset(u, offset);
    IF offset = 0 OR u.init_type # type OR offset # u.current_init_offset THEN
        end_init_helper(u);
        u.init_fields.addhi(typeToText[type] & " " & M3ID.ToText(GenerateName(u)) & comment);
    END;
    INC(u.init_type_count);
    u.init_type := type;
    u.current_init_offset := offset + TargetMap.CG_Bytes[type];
END init_helper;

PROCEDURE init_int(u: U; offset: ByteOffset; READONLY value: Target.Int; type: Type) =
VAR comment := "";
BEGIN
    u.comment("init_int");
    init_helper(u, offset, type, comment);
    (* TIntLiteral includes suffixes like U, ULL, UI64, etc. *)
    u.initializer.addhi(TIntLiteral(type, value));
END init_int;

PROCEDURE init_proc(u: U; offset: ByteOffset; p: M3CG.Proc) =
VAR proc := NARROW(p, Proc_t);
    comment := "";
BEGIN
    u.comment("init_proc");
    init_helper(u, offset, Type.Addr, comment); (* FUTURE: better typing *)
    u.initializer.addhi(comment & "(ADDRESS)&" & M3ID.ToText(proc.name));
END init_proc;

<*NOWARN*>PROCEDURE init_label(u: U; offset: ByteOffset; value: Label) =
BEGIN
    u.comment("init_label");
    <* ASSERT FALSE *>
END init_label;

PROCEDURE init_var(u: U; offset: ByteOffset; v: M3CG.Var; bias: ByteOffset) =
VAR var := NARROW(v, Var_t);
    comment := "";
BEGIN
    u.comment("init_var");
    init_helper(u, offset, Type.Addr, comment); (* FUTURE: better typing *)
    IF bias # 0 THEN
        u.initializer.addhi(comment & IntToDec(bias) & "+"& "(ADDRESS)&" & M3ID.ToText(var.name));
    ELSE
        u.initializer.addhi(comment & "(ADDRESS)&" & M3ID.ToText(var.name));
    END;
END init_var;

<*NOWARN*>PROCEDURE init_offset(u: U; offset: ByteOffset; value: M3CG.Var) =
BEGIN
    u.comment("init_offset");
    <* ASSERT FALSE *>
END init_offset;

CONST Printable = ASCII.AlphaNumerics
        + ASCII.Set{' ','!','@','#','$','%','^','&','*','(',')','-','_','='}
        + ASCII.Set{'+','[','{',']','}','|',';',':','"',',','<','.','>','/'}
        + ASCII.Set{'?','`','~' };
PROCEDURE init_chars(u: U; offset: ByteOffset; value: TEXT) =
VAR length := Text.Length(value);
ch: CHAR;
BEGIN
    u.comment("init_chars");
    IF length = 0 THEN
        RETURN;
    END;
    FOR i := 0 TO length - 1 DO
        init_helper(u, offset + i, Type.Word8);
        ch := Text.GetChar(value, i);
        IF ch IN Printable THEN
            u.initializer.addhi("'" & Text.Sub(value, i, 1) & "'");
        ELSE
            u.initializer.addhi(IntToDec(ORD(ch)));
        END;
    END;
END init_chars;

PROCEDURE TIntLiteral(type: Type; READONLY i: Target.Int): TEXT =
BEGIN
    IF TInt.EQ(i, TInt.Min32) THEN
        RETURN "-" & intLiteralPrefix[type] & TInt.ToText(TInt.Max32) & intLiteralSuffix[type] & "-1";
    ELSIF TInt.EQ(i, TInt.Min64) THEN
        RETURN "-" & intLiteralPrefix[type] & TInt.ToText(TInt.Max64) & intLiteralSuffix[type] & "-1";
    ELSE
        RETURN intLiteralPrefix[type] & TInt.ToText(i) & intLiteralSuffix[type];
    END;
END TIntLiteral;

PROCEDURE IntLiteral(u: U; type: Type; i: INTEGER): TEXT =
BEGIN
    RETURN TIntLiteral(type, IntToTarget(u, i));
END IntLiteral;

PROCEDURE FloatLiteral(READONLY float: Target.Float): TEXT =
VAR suffix := '\000';
    cBuf, modulaBuf: ARRAY [0..BITSIZE(EXTENDED) + 1] OF CHAR;
    len := TFloat.ToChars(float, modulaBuf);
    j := 0;
    ch: CHAR;
BEGIN
(* 1.2e3 => 1.2e3F float/REAL
   1.2d3 => 1.2e3  double/LONGREAL
   1.2x3 => 1.2e3L long double/EXTENDED
*)
    FOR i := 0 TO len - 1 DO
        ch := modulaBuf[i];
        IF ch = 'e' OR ch = 'E' THEN
            suffix := 'F';
        ELSIF ch = 'd' OR ch = 'D' THEN
            suffix := '\000';
            ch := 'e';
        ELSIF ch = 'x' OR ch = 'X' THEN
            suffix := 'L';
            ch := 'e';
        END;
        cBuf[j] := ch;
        INC(j);
    END;
    IF suffix # '\000' THEN
        cBuf[j] := suffix;
        INC(j);
    END;
    RETURN Text.FromChars(SUBARRAY(cBuf, 0, j));
END FloatLiteral;

PROCEDURE init_float(u: U; offset: ByteOffset; READONLY float: Target.Float) =
BEGIN
    u.comment("init_float");
    init_helper(u, offset, TargetMap.Float_types[TFloat.Prec(float)].cg_type);
    u.initializer.addhi(FloatLiteral(float));
END init_float;

(*------------------------------------------------------------ PROCEDUREs ---*)

PROCEDURE import_procedure(u: U; name: Name; n_params: INTEGER;
                           return_type: Type; callingConvention: CallingConvention): M3CG.Proc =
VAR proc := NEW(Proc_t, name := name, n_params := n_params,
                return_type := return_type, imported := TRUE,
                callingConvention := callingConvention).Init(u);
BEGIN
    u.comment("import_procedure");
    SuppressLineDirective(u, n_params, "import_procedure n_params");
    u.param_proc := proc;
    u.param_count := 0;
    IF n_params = 0 THEN
        last_param(u);
    END;
    RETURN proc;
END import_procedure;

PROCEDURE declare_procedure(u: U; name: Name; n_params: INTEGER;
                            return_type: Type; level: INTEGER;
                            callingConvention: CallingConvention;
                            exported: BOOLEAN; parent: M3CG.Proc): M3CG.Proc =
VAR proc := NEW(Proc_t, name := name, n_params := n_params,
                return_type := return_type, level := level,
                callingConvention := callingConvention, exported := exported,
                parent := parent).Init(u);
BEGIN
    u.comment("declare_procedure");
    u.param_proc := proc;
    u.param_count := 0;
    IF NOT u.in_proc THEN
        u.current_proc := proc;
    END;
    IF n_params = 0 THEN
        last_param(u);
    END;

    SuppressLineDirective(u, n_params, "declare_procedure n_params");
    RETURN proc;
END declare_procedure;

PROCEDURE begin_procedure(u: U; p: M3CG.Proc) =
VAR proc := NARROW(p, Proc_t);
    frame_name := proc.FrameName();
    frame_type := proc.FrameType();
    var: Var_t;
    params := proc.params;
BEGIN
    u.comment("begin_procedure");

    <* ASSERT NOT u.in_proc *>
    u.in_proc := TRUE;
    u.current_proc := proc;

    (* declare frame type *)

    IF proc.forward_declared_frame_type THEN
        print(u, "struct " & frame_type & " {");

        (* add field to ensure frame not empty *)
        (* TODO only do this if necessary *)

        print(u, "void* _unused;");

        (* uplevel locals in frame *) (* structs? *)

        FOR i := 0 TO proc.Locals_Size() - 1 DO
            var := proc.Locals(i);
            IF var.up_level THEN
                print(u, var.Declare() & ";");
            END;
        END;

        (* uplevel params in frame *) (* structs? *)

        FOR i := FIRST(params^) TO LAST(params^) DO
            var := params[i];
            IF var.up_level THEN
                print(u, var.Declare() & ";");
            END;
        END;
        print(u, "};");
    END;

    print(u, function_prototype(proc, FunctionPrototype_t.Define));
    print(u, "{");

    (* copy structs from pointers to local value *) (* uplevels? *)

    FOR i := FIRST(params^) TO LAST(params^) DO
        print(u, params[i].DeclareAndInitStructParamLocalValue());
    END;

    (* declare and initialize non-uplevel locals *)

    FOR i := 0 TO proc.Locals_Size() - 1 DO
        WITH L = proc.Locals(i) DO
            IF NOT L.up_level THEN
                print(u, L.Declare() & "={0};");
            END;
        END;
    END;

    (* declare and initialize frame of uplevels *)

    IF proc.forward_declared_frame_type THEN
        print(u, frame_type & " " & frame_name & "={0};");

        (* capture uplevel parameters and static_link *)

        FOR i := FIRST(params^) TO LAST(params^) DO
            WITH param = params[i] DO
                IF param.up_level THEN
                    WITH param_name = M3ID.ToText(param.name) DO
                        print(u, frame_name & "." & param_name & "=" & param_name & ";");
                    END;
                END;
            END;
        END;

        (* quash unused warning *)

        print(u, frame_name & "._unused=&" & frame_name & ";");

    END;
END begin_procedure;

PROCEDURE end_procedure(u: U; <*UNUSED*>p: M3CG.Proc) =
BEGIN
    u.comment("end_procedure");
    u.in_proc := FALSE;
    print(u, "}");
    ExtraScope_Close(u);
    WHILE u.import_repeat.size() > 0 DO
        print(u, u.import_repeat.remlo());
    END;
END end_procedure;

PROCEDURE begin_block(u: U) =
(* marks the beginning of a nested anonymous block *)
BEGIN
    u.comment("begin_block");
(* pending import_procedure all moved up to global scope
    print(u, "{");
*)
END begin_block;

PROCEDURE end_block(u: U) =
(* marks the ending of a nested anonymous block *)
BEGIN
    u.comment("end_block");
(* pending import_procedure all moved up to global scope
    print(u, "}");
*)
END end_block;

PROCEDURE note_procedure_origin(u: U; <*UNUSED*>p: M3CG.Proc) =
BEGIN
    u.comment("note_procedure_origin");
END note_procedure_origin;

(*------------------------------------------------------------ statements ---*)

PROCEDURE set_label(u: U; label: Label; <*UNUSED*>barrier: BOOLEAN) =
(* define 'label' to be at the current pc *)
BEGIN
    IF u.debug THEN
        print(u, " /* set_label */ ");
    END;
    (* semicolon in case we fall off the function here:
       void F() { L: } is not legal but
       void F() { L:; } is, and means what you'd think the first means *)
    print(u, "L" & LabelToHex(label) & ":;");
END set_label;

PROCEDURE jump(u: U; label: Label) =
(* GOTO label *)
BEGIN
    u.comment("jump");
    print(u, "goto L" & LabelToHex(label) & ";");
END jump;

PROCEDURE if_true(u: U; itype: IType; label: Label; <*UNUSED*>frequency: Frequency) =
(* IF (s0.itype # 0) GOTO label; pop *)
VAR s0 := cast(get(u, 0), itype);
BEGIN
    u.comment("if_true");
    print(u, "if(" & s0 & ")goto L" & LabelToText(label) & ";");
    pop(u);
END if_true;

PROCEDURE if_false(u: U; itype: IType; label: Label; <*UNUSED*>frequency: Frequency) =
(* IF (s0.itype = 0) GOTO label; pop *)
VAR s0 := cast(get(u, 0), itype);
BEGIN
    u.comment("if_false");
    print(u, "if(!" & s0 & ")goto L" & LabelToText(label) & ";");
    pop(u);
END if_false;

PROCEDURE if_compare(u: U; ztype: ZType; op: CompareOp; label: Label;
                     <*UNUSED*>frequency: Frequency) =
(* IF (s1.ztype op s0.ztype) GOTO label; pop(2) *)
VAR s0 := cast(get(u, 0), ztype);
    s1 := cast(get(u, 1), ztype);
BEGIN
    u.comment("if_compare");
    pop(u, 2);
    print(u, "if(" & s1 & CompareOpC[op] & s0 & ")goto L" & LabelToText(label) & ";");
END if_compare;

PROCEDURE case_jump(u: U; itype: IType; READONLY labels: ARRAY OF Label) =
(* "GOTO labels[s0.itype]; pop" with no range checking on s0.itype *)
VAR s0 := cast(get(u, 0), itype);
BEGIN
    u.comment("case_jump");
    print(u, "switch(" & s0 & "){");
    FOR i := FIRST(labels) TO LAST(labels) DO
        print(u, "case " & IntToDec(i) & ":goto L" & LabelToText(labels[i]) & ";");
    END;
    print(u, "}");
    pop(u);
END case_jump;

PROCEDURE exit_proc(u: U; type: Type) =
(* Returns s0.type if type is not Void, otherwise returns no value. *)
VAR s0: TEXT;
BEGIN
    u.comment("exit_proc");
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
    in := paren(in);
    IF in_offset # 0 THEN
        in := paren(paren(IntToDec(in_offset)) & "+(ADDRESS)" & in);
    END;
    RETURN in;
END address_plus_offset;

PROCEDURE load_helper(u: U; in: TEXT; in_offset: INTEGER; in_mtype: MType; out_ztype: ZType) =
VAR text: TEXT := NIL;
BEGIN
    <* ASSERT CG_Bytes[out_ztype] >= CG_Bytes[in_mtype] *>
    text := "*(" & typeToText[in_mtype] & "*)" & address_plus_offset(in, in_offset);
    IF in_mtype # out_ztype THEN
        text := "((" & typeToText[out_ztype] & ")(" & text & "))";
    END;
    push(u, out_ztype, text);
END load_helper;

PROCEDURE follow_static_link(u: U; var: Var_t): TEXT =
VAR current_level := u.current_proc.level;
    var_proc := var.proc;
    var_level := 0;
    var_name := M3ID.ToText(var.name);
    static_link := "";
BEGIN
    (* RTIO.PutText("1 " & M3ID.ToText(var.name) & "\n"); RTIO.Flush(); *)
    IF var_proc = NIL OR var.up_level = FALSE OR var.is_static_link THEN
        RETURN  "";
    END;
    var_level := var_proc.level;
    IF u.debug THEN
        static_link := " /* accessing var " & var_name & " at level " & IntToDec(var_level) & " from level " & IntToDec(current_level) & " */ ";
    END;
    IF u.debug THEN
        RTIO.PutText("1 accessing " & M3ID.ToText(var.name) & " from " & IntToDec(current_level) & " to " & IntToDec(var_level) & "\n");
        RTIO.Flush();
    END;
    IF current_level = var_level THEN
        IF u.debug THEN
            RTIO.PutText("2 accessing " & M3ID.ToText(var.name) & " from " & IntToDec(current_level) & " to " & IntToDec(var_level) & "\n");
            RTIO.Flush();
        END;
        RETURN var_proc.FrameName() & ".";
    END;
    IF u.debug THEN
        RTIO.PutText("3 accessing " & M3ID.ToText(var.name) & " from " & IntToDec(current_level) & " to " & IntToDec(var_level) & "\n");
        RTIO.Flush();
    END;
    (* You cannot access the variables of a nested function, only a containing function. *)
    <* ASSERT var_level <= current_level *>
    FOR i := var_level + 1 TO current_level DO
        static_link := static_link & "_static_link->";
    END;
    RETURN static_link;
END follow_static_link;

PROCEDURE load(u: U; v: M3CG.Var; offset: ByteOffset; mtype: MType; ztype: ZType) =
(* push; s0.ztype := Mem [ ADR(var) + offset ].mtype; The only allowed (mtype->ztype) conversions
   are {Int,Word}{8,16} -> {Int,Word}{32,64} and {Int,Word}32 -> {Int,Word}64.
   The source type, mtype, determines whether the value is sign-extended or
   zero-extended. *)
VAR var := NARROW(v, Var_t);
BEGIN
    u.comment("load");
    load_helper(u, "&" & follow_static_link(u, var) & M3ID.ToText(var.name), offset, mtype, ztype);
END load;

PROCEDURE store_helper(u: U; in: TEXT; in_ztype: ZType; out_address: TEXT; out_offset: INTEGER; out_mtype: MType) =
BEGIN
    <* ASSERT CG_Bytes[in_ztype] >= CG_Bytes[out_mtype] *>
    print(u, "(*(" & typeToText[out_mtype] & "*)" & address_plus_offset(out_address, out_offset) & ")=(" & typeToText[in_ztype] & ")(" & in & ");");
END store_helper;

PROCEDURE store(u: U; v: M3CG.Var; offset: ByteOffset; ztype: ZType; mtype: MType) =
(* Mem [ ADR(var) + offset ].mtype := s0.ztype; pop *)
VAR var := NARROW(v, Var_t);
    s0 := cast(get(u, 0), ztype);
BEGIN
    u.comment("store");
    pop(u);
    store_helper(u, s0, ztype, "&" & follow_static_link(u, var) & M3ID.ToText(var.name), offset, mtype);
  END store;

PROCEDURE load_address(u: U; v: M3CG.Var; offset: ByteOffset) =
(* push; s0.A := ADR(var) + offset *)
VAR var := NARROW(v, Var_t);
BEGIN
    u.comment("load_address");
    push(u, Type.Addr, address_plus_offset("&" & follow_static_link(u, var) & M3ID.ToText (var.name), offset));
END load_address;

PROCEDURE load_indirect(u: U; offset: ByteOffset; mtype: MType; ztype: ZType) =
(* s0.ztype := Mem [s0.A + offset].mtype  *)
VAR s0 := get(u);
BEGIN
    u.comment("load_indirect");
    <* ASSERT CG_Bytes[ztype] >= CG_Bytes[mtype] *>
    pop(u);
    load_helper(u, s0, offset, mtype, ztype);
END load_indirect;

PROCEDURE store_indirect(u: U; offset: ByteOffset; ztype: ZType; mtype: MType) =
(* Mem [s1.A + offset].mtype := s0.ztype; pop (2) *)
VAR s0 := cast(get(u, 0), ztype);
    s1 := get(u, 1);
BEGIN
    u.comment("store_indirect");
    pop(u, 2);
    store_helper(u, s0, ztype, s1, offset, mtype);
END store_indirect;

(*-------------------------------------------------------------- literals ---*)

PROCEDURE load_nil(u: U) =
(* push; s0.A := NIL *)
BEGIN
    u.comment("load_nil");
    push(u, Type.Addr, "0"); (* UNDONE NULL or (ADDRESS)0? *)
END load_nil;

PROCEDURE IntToTarget(u: U; i: INTEGER): Target.Int =
VAR j: Target.Int;
BEGIN
    IF NOT TInt.FromInt(i, j) THEN
        u.Err ("failed to convert host integer to target integer");
    END;
    RETURN j;
END IntToTarget;

PROCEDURE load_host_integer(u: U; type: IType; i: INTEGER) =
BEGIN
    comment(u, "load_host_integer");
    load_target_integer(u, type, IntToTarget(u, i));
END load_host_integer;

PROCEDURE load_target_integer(u: U; type: IType; READONLY i: Target.Int) =
(* push; s0.type := i *)
BEGIN
    u.comment("load_integer");
    (* TIntLiteral includes suffixes like U, ULL, UI64, etc. *)
    push(u, type, cast(TIntLiteral(type, i), type));
END load_target_integer;

PROCEDURE load_float(u: U; type: RType; READONLY float: Target.Float) =
(* push; s0.type := float *)
BEGIN
    u.comment("load_float");
    (* FloatLiteral includes suffixes like "F" for float, "" for double, "L" for long double *)
    push(u, type, cast(FloatLiteral(float), type));
END load_float;

(*------------------------------------------------------------ arithmetic ---*)

PROCEDURE cast(expr: TEXT; type: Type := Type.Void; type_text: TEXT := NIL): TEXT =
BEGIN
    <* ASSERT (type = Type.Void) # (type_text = NIL) *>
    IF type_text = NIL THEN
        type_text := typeToText[type];
    END;
    RETURN paren("(" & type_text & ")" & paren(expr));
END cast;

PROCEDURE op1(u: U; type: Type; name, op: TEXT) =
(* unary operation *)
VAR s0 := cast(get(u, 0), type);
BEGIN
    u.comment(name);
    pop(u, 1);
    push(u, type, cast(op & s0, type));
END op1;

PROCEDURE op2(u: U; type: Type; name, op: TEXT) =
(* binary operation *)
VAR s0 := cast(get(u, 0), type);
    s1 := cast(get(u, 1), type);
BEGIN
    u.comment(name);
    pop(u, 2);
    push(u, type, cast(s1 & op & s0, type));
END op2;

PROCEDURE compare(u: U; ztype: ZType; itype: IType; op: CompareOp) =
(* s1.itype := (s1.ztype op s0.ztype); pop *)
VAR s0 := cast(get(u, 0), ztype);
    s1 := cast(get(u, 1), ztype);
BEGIN
    u.comment("compare");
    (* ASSERT cond # Cond.Z AND cond # Cond.NZ *)
    pop(u, 2);
    push(u, itype, cast(s1 & CompareOpC[op] & s0, itype));
END compare;

PROCEDURE add(u: U; type: AType) =
(* s1.type := s1.type + s0.type; pop *)
BEGIN
    op2(u, type, "add", "+");
END add;

PROCEDURE subtract(u: U; type: AType) =
(* s1.type := s1.type - s0.type; pop *)
BEGIN
    op2(u, type, "subtract", "-");
END subtract;

PROCEDURE multiply(u: U; type: AType) =
(* s1.type := s1.type * s0.type; pop *)
BEGIN
    op2(u, type, "multiply", "*");
END multiply;

PROCEDURE divide(u: U; type: RType) =
(* s1.type := s1.type / s0.type; pop *)
BEGIN
    op2(u, type, "divide", "/");
END divide;

PROCEDURE div(u: U; type: IType; <*UNUSED*>a, b: Sign) =
(* s1.type := s1.type DIV s0.type; pop *)
VAR s0 := cast(get(u, 0), type);
    s1 := cast(get(u, 1), type);
BEGIN
    u.comment("div");
    pop(u, 2);
    IF typeIsUnsigned[type] THEN
        push(u, type, cast(s1 & "/" & s0, type));
    ELSE
        push(u, type, cast("m3_div_" & typeToText[type] & "(" & s1 & "," & s0 & ")", type));
    END;
END div;

PROCEDURE mod(u: U; type: IType; <*UNUSED*>a, b: Sign) =
(* s1.type := s1.type MOD s0.type; pop *)
VAR s0 := cast(get(u, 0), type);
    s1 := cast(get(u, 1), type);
BEGIN
    u.comment("mod");
    pop(u, 2);
    IF typeIsUnsigned[type] THEN
        push(u, type, cast(s1 & "%" & s0, type));
    ELSE
        push(u, type, cast("m3_mod_" & typeToText[type] & "(" & s1 & "," & s0 & ")", type));
    END;
END mod;

PROCEDURE negate(u: U; type: AType) =
(* s0.type := - s0.type *)
BEGIN
    op1(u, type, "negate", "-");
END negate;

PROCEDURE abs(u: U; type: AType) =
(* s0.type := ABS (s0.type) (noop on Words) *)
VAR s0 := cast(get(u, 0), type);
BEGIN
    u.comment("abs");
    pop(u);
    push(u, type, "m3_abs_" & typeToText[type] & "(" & s0 & ")");
END abs;

PROCEDURE max(u: U; type: ZType) =
(* s1.type := MAX (s1.type, s0.type); pop *)
VAR s0 := cast(get(u, 0), type);
    s1 := cast(get(u, 1), type);
BEGIN
    u.comment("max");
    pop(u, 2);
    push(u, type, "m3_max_" & typeToText[type] & "(" & s0 & "," & s1 & ")");
END max;

PROCEDURE min(u: U; type: ZType) =
(* s1.type := MIN (s1.type, s0.type); pop *)
VAR s0 := cast(get(u, 0), type);
    s1 := cast(get(u, 1), type);
BEGIN
    u.comment("min");
    pop(u, 2);
    push(u, type, "m3_min_" & typeToText[type] & "(" & s0 & "," & s1 & ")");
END min;

PROCEDURE cvt_int(u: U; from_float_type: RType; to_integer_type: IType; op: ConvertOp) =
(* s0.itype := ROUND(s0.rtype) *)
VAR s0 := cast(get(u, 0), from_float_type);
BEGIN
    u.comment("cvt_int");
    pop(u);
    push(u, to_integer_type, cast("m3_" & ConvertOpName[op] & "(" & s0 & ")", to_integer_type));
END cvt_int;

PROCEDURE cvt_float(u: U; from_arithmetic_type: AType; to_float_type: RType) =
(* s0.rtype := ROUND(s0.atype) *)
VAR s0 := cast(get(u, 0), from_arithmetic_type);
BEGIN
    u.comment("cvt_float");
    (* UNDONE is this correct? *)
    pop(u);
    push(u, to_float_type, cast(s0, to_float_type));
END cvt_float;

(*------------------------------------------------------------------ sets ---*)

PROCEDURE set_op3(u: U; byte_size: ByteSize; op: TEXT) =
(* s2.B := s1.B op s0.B; pop(3) *)
VAR s0 := cast(get(u, 0), Type.Addr);
    s1 := cast(get(u, 1), Type.Addr);
    s2 := cast(get(u, 2), Type.Addr);
BEGIN
    u.comment(op);
    pop(u, 3);
    print(u, "m3_" & op & "(" & IntToDec(byte_size * 8) & ",(WORD_T*)" & s2 & ",(WORD_T*)" & s1 & ",(WORD_T*)" & s0 & ");");
END set_op3;

PROCEDURE set_union(u: U; byte_size: ByteSize) =
(* s2.B := s1.B + s0.B; pop(3) *)
BEGIN
    set_op3(u, byte_size, "set_union");
END set_union;

PROCEDURE set_difference(u: U; byte_size: ByteSize) =
(* s2.B := s1.B - s0.B; pop(3) *)
BEGIN
    set_op3(u, byte_size, "set_difference");
END set_difference;

PROCEDURE set_intersection(u: U; byte_size: ByteSize) =
(* s2.B := s1.B * s0.B; pop(3) *)
BEGIN
    set_op3(u, byte_size, "set_intersection");
END set_intersection;

PROCEDURE set_sym_difference(u: U; byte_size: ByteSize) =
(* s2.B := s1.B / s0.B; pop(3) *)
BEGIN
    set_op3(u, byte_size, "set_sym_difference");
END set_sym_difference;

PROCEDURE set_member(u: U; <*UNUSED*>byte_size: ByteSize; type: IType) =
(* s1.type := (s0.type IN s1.B); pop *)
VAR s0 := cast(get(u, 0), type);
    s1 := cast(get(u, 1), Type.Void, "SET");
BEGIN
    u.comment("set_member");
    pop(u, 2);
    push(u, type, cast("m3_set_member(" & s0 & "," & s1 & ")", type));
END set_member;

PROCEDURE set_compare(u: U; byte_size: ByteSize; op: CompareOp; type: IType) =
(* s1.type := (s1.B op s0.B); pop *)
VAR s0 := cast(get(u, 0), Type.Void, "SET");
    s1 := cast(get(u, 1), Type.Void, "SET");
BEGIN
    u.comment("set_compare");
    pop(u, 2);
    push(u, type, cast("m3_set_" & CompareOpName[op] & "(" & IntLiteral(u, Target.Word.cg_type, byte_size * 8) & "," & s1 & "," & s0 & ")", type));
END set_compare;

PROCEDURE set_range(u: U; <*UNUSED*>byte_size: ByteSize; type: IType) =
(* s2.A [s1.type .. s0.type] := 1's; pop(3) *)
VAR s0 := cast(get(u, 0), type);
    s1 := cast(get(u, 1), type);
    s2 := cast(get(u, 2), Type.Void, "SET");
BEGIN
    u.comment("set_range");
    pop(u, 3);
    print(u, "m3_set_range(" & s0 & "," & s1 & "," & s2 & ");");
END set_range;

PROCEDURE set_singleton(u: U; <*UNUSED*>byte_size: ByteSize; type: IType) =
(* s1.A [s0.type] := 1; pop(2) *)
VAR s0 := cast(get(u, 0), type);
    s1 := cast(get(u, 1), Type.Void, "SET");
BEGIN
    u.comment("set_singleton");
    pop(u, 2);
    print(u, "m3_set_singleton(" & s0 & "," & s1 & ");");
END set_singleton;

(*------------------------------------------------- Word.T bit operations ---*)

PROCEDURE not(u: U; type: IType) =
(* s0.type := Word.Not (s0.type) *)
BEGIN
    op1(u, type, "not", "~");
END not;

PROCEDURE and(u: U; type: IType) =
(* s1.type := Word.And (s1.type, s0.type); pop *)
BEGIN
    op2(u, type, "and", "&");
END and;

PROCEDURE or(u: U; type: IType) =
(* s1.type := Word.Or  (s1.type, s0.type); pop *)
BEGIN
    op2(u, type, "or", "|");
END or;

PROCEDURE xor(u: U; type: IType) =
(* s1.type := Word.Xor (s1.type, s0.type); pop *)
BEGIN
    op2(u, type, "xor", "^");
END xor;

PROCEDURE shift_left_or_right(u: U; type: IType; name, op: TEXT) =
VAR s0 := cast(get(u, 0), Target.Word.cg_type);
    s1 := cast(get(u, 1), type);
BEGIN
    u.comment(name);
    pop(u, 2);
    push(u, type, s1 & op & s0);
END shift_left_or_right;

PROCEDURE shift_left(u: U; type: IType) =
(* s1.type := Word.Shift  (s1.type, s0.Word); pop *)
BEGIN
    shift_left_or_right(u, type, "shift_left", "<<");
END shift_left;

PROCEDURE shift_right(u: U; type: IType) =
(* s1.type := Word.Shift  (s1.type, -s0.type); pop *)
BEGIN
    <* ASSERT typeIsUnsigned[type] *>
    shift_left_or_right(u, type, "shift_right", ">>");
END shift_right;

PROCEDURE shift_or_rotate(u: U; type: IType; which: TEXT; count_type: Type) =
VAR s0 := cast(get(u, 0), count_type);
    s1 := cast(get(u, 1), type);
BEGIN
    u.comment(which);
    pop(u, 2);
    push(u, type, "m3_" & which & "_" & typeToText[type] & "(" & s1 & "," & s0 & ")");
END shift_or_rotate;

PROCEDURE shift(u: U; type: IType) =
(* s1.type := Word.Shift  (s1.type, s0.type); pop *)
BEGIN
    shift_or_rotate(u, type, "shift", Target.Integer.cg_type);
END shift;

PROCEDURE rotate(u: U; type: IType) =
(* s1.type := Word.Rotate (s1.type, s0.type); pop *)
BEGIN
    shift_or_rotate(u, type, "rotate", Target.Integer.cg_type);
END rotate;

PROCEDURE rotate_left(u: U; type: IType) =
(* s1.type := Word.Rotate (s1.type, s0.type); pop *)
BEGIN
    shift_or_rotate(u, type, "rotate_left", Target.Word.cg_type);
END rotate_left;

PROCEDURE rotate_right(u: U; type: IType) =
(* s1.type := Word.Rotate (s1.type, -s0.type); pop *)
BEGIN
    shift_or_rotate(u, type, "rotate_right", Target.Word.cg_type);
END rotate_right;

PROCEDURE widen(u: U; <*UNUSED*>sign_extend: BOOLEAN) =
(* s0.I64 := s0.I32; IF sign_extend THEN SignExtend s0; *)
BEGIN
    u.comment("widen");
    <*ASSERT FALSE*>
END widen;

PROCEDURE chop(u: U) =
(* s0.I32 := Word.And (s0.I64, 16_FFFFFFFF); *)
BEGIN
    u.comment("chop");
    <*ASSERT FALSE*>
END chop;

PROCEDURE extract(u: U; type: IType; sign_extend: BOOLEAN) =
(* s2.type := Word.Extract(s2.type, s1.type, s0.type);
  IF sign_extend THEN SignExtend s2 END; pop(2) *)
VAR count := cast(get(u, 0), Target.Word.cg_type);
    offset := cast(get(u, 1), Target.Word.cg_type);
    value := cast(get(u, 2), type);
BEGIN
    u.comment("extract");
    <* ASSERT sign_extend = FALSE *>
    pop(u, 3);
    push(u, type, "m3_extract_" & typeToText[typeToUnsigned[type]] & "(" & value & "," & offset & "," & count & ")");
END extract;

PROCEDURE extract_n(u: U; type: IType; sign_extend: BOOLEAN; count: CARDINAL) =
(* s1.type := Word.Extract(s1.type, s0.type, count);
   IF sign_extend THEN SignExtend s1 END; pop(1) *)
BEGIN
    u.comment("extract_n");
    load_host_integer(u, Target.Word.cg_type, count);
    u.extract(type, sign_extend);
END extract_n;

PROCEDURE do_sign_extend(u: U; type: IType) =
VAR count := cast(get(u, 0), Target.Word.cg_type);
    value := cast(get(u, 1), type);
BEGIN
    pop(u, 2);
    push(u, type, "m3_sign_extend_" & typeToText[type] & "(" & value & "," & count & ")");
END do_sign_extend;

PROCEDURE extract_mn(u: U; type: IType; sign_extend: BOOLEAN; offset, count: CARDINAL) =
(* s0.type := Word.Extract(s0.type, offset, count);
    IF sign_extend THEN SignExtend s0 END; *)
BEGIN
    u.comment("extract_mn");
    load_host_integer(u, Target.Word.cg_type, offset);
    load_host_integer(u, Target.Word.cg_type, count);
    u.extract(type, FALSE);
    IF sign_extend THEN
        load_host_integer(u, Target.Word.cg_type, count);
        do_sign_extend(u, type);
    END;
END extract_mn;

PROCEDURE insert(u: U; type: IType) =
(* s3.type := Word.Insert (s3.type, s2.type, s1.type, s0.type); pop(3) *)
VAR count := cast(get(u, 0), Target.Word.cg_type);
    offset := cast(get(u, 1), Target.Word.cg_type);
    from := cast(get(u, 2), type);
    to := cast(get(u, 3), type);
BEGIN
    u.comment("insert");
    pop(u, 4);
    push(u, type, "m3_insert_" & typeToText[type] & "(" & to & "," & from & "," & offset & "," & count & ")");
END insert;

PROCEDURE insert_n(u: U; type: IType; count: CARDINAL) =
(* s2.type := Word.Insert (s2.type, s1.type, s0.type, count); pop(2) *)
BEGIN
    u.comment("insert_n");
    load_host_integer(u, Target.Word.cg_type, count);
    u.insert(type);
END insert_n;

PROCEDURE insert_mn(u: U; type: IType; offset, count: CARDINAL) =
(* s1.type := Word.Insert (s1.type, s0.type, offset, count); pop(2) *)
BEGIN
    u.comment("insert_mn");
    load_host_integer(u, Target.Word.cg_type, offset);
    load_host_integer(u, Target.Word.cg_type, count);
    u.insert(type);
END insert_mn;

(*------------------------------------------------ misc. stack/memory ops ---*)

PROCEDURE swap(u: U; <*UNUSED*>a, b: Type) =
(* tmp := s1; s1 := s0; s0 := tmp *)
VAR temp := get(u, 1);
BEGIN
    u.comment("swap");
    u.stack.put(1, get(u, 0));
    u.stack.put(0, temp);
END swap;

PROCEDURE cg_pop(u: U; type: Type) =
(* pop(1) (i.e. discard s0) *)
VAR s0 := cast(get(u, 0), type);
BEGIN
    u.comment("pop");
    pop(u);
    print(u, "m3_pop_" & typeToText[type] & "(" & s0 & ");");
END cg_pop;

CONST MemCopyOrMove = ARRAY OF TEXT{"memcpy", "memmove"};

PROCEDURE copy_n(u: U; itype: IType; mtype: MType; overlap: BOOLEAN) =
(* Mem[s2.A:s0.ztype] := Mem[s1.A:s0.ztype]; pop(3)*)
VAR s0 := cast(get(u, 0), itype);
    s1 := get(u, 1);
    s2 := get(u, 2);
BEGIN
    u.comment("copy_n");
    pop(u, 3);
    print(u, MemCopyOrMove[ORD(overlap)] & "(" & s2 & "," & s1 & "," & IntToDec(CG_Bytes[mtype]) & "*(size_t)" & s0 & ");");
END copy_n;

PROCEDURE copy(u: U; n: INTEGER; mtype: MType; overlap: BOOLEAN) =
(* Mem[s1.A:sz] := Mem[s0.A:sz]; pop(2)*)
VAR s0 := get(u, 0);
    s1 := get(u, 1);
BEGIN
    u.comment("copy");
    pop(u, 2);
    print(u, MemCopyOrMove[ORD(overlap)] & "(" & s1 & "," & s0 & "," & IntToDec(CG_Bytes[mtype] * n) & ");");
END copy;

<*NOWARN*>PROCEDURE zero_n(u: U; itype: IType; mtype: MType) =
(* Mem[s1.A:s0.itype] := 0; pop(2) *)
BEGIN
    u.comment("zero_n");

    <* ASSERT FALSE *>

    (* zero_n is implemented incorrectly in the gcc backend,
     * therefore it must not be used.
     *)
END zero_n;

PROCEDURE zero(u: U; n: INTEGER; type: MType) =
(* Mem[s0.A:sz] := 0; pop(1) *)
VAR s0 := get(u, 0);
BEGIN
    u.comment("zero");
    pop(u);
    print(u, "memset(" & s0 & ",0," & IntToDec(n) & "*(size_t)" & IntToDec(CG_Bytes[type]) & ");");
END zero;

(*----------------------------------------------------------- conversions ---*)

PROCEDURE loophole(u: U; from, to: ZType) =
(* s0.to := LOOPHOLE(s0.from, to) *)
VAR s0 := cast(cast(get(u, 0), from), to);
BEGIN
    u.comment("loophole");
    u.stack.put(0, s0);
END loophole;

(*------------------------------------------------ traps & runtime checks ---*)

PROCEDURE abort(u: U; code: RuntimeError) =
BEGIN
    u.comment("abort");
    reportfault(u, code);
END abort;

PROCEDURE check_nil(u: U; code: RuntimeError) =
(* IF (s0.A = NIL) THEN abort(code) *)
VAR s0 := get(u);
BEGIN
    u.comment("check_nil");
    print(u, "if(!" & paren(s0) & ")");
    reportfault(u, code);
END check_nil;

PROCEDURE check_lo(u: U; type: IType; READONLY i: Target.Int; code: RuntimeError) =
(* IF (s0.type < i) THEN abort(code) *)
VAR s0 := cast(get(u), type);
BEGIN
    u.comment("check_lo");
    print(u, "if(" & s0 & "<" & TIntLiteral(type, i) & ")");
    reportfault(u, code);
END check_lo;

PROCEDURE check_hi(u: U; type: IType; READONLY i: Target.Int; code: RuntimeError) =
(* IF (i < s0.type) THEN abort(code) *)
VAR s0 := cast(get(u), type);
BEGIN
    u.comment("check_hi");
    print(u, "if(" & TIntLiteral(type, i) & "<" & s0 & ")");
    reportfault(u, code);
END check_hi;

PROCEDURE check_range(u: U; type: IType; READONLY a, b: Target.Int; code: RuntimeError) =
(* IF (s0.type < a) OR (b < s0.type) THEN abort(code) *)
VAR s0 := cast(get(u), type);
    a_text := TIntLiteral(type, a);
    b_text := TIntLiteral(type, b);
BEGIN
    u.comment("check_range");
    print(u, "if((" & s0 & "<" & cast(a_text, type) & ")||(" & cast(b_text, type) & "<" & s0 & "))");
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
    u.comment("check_index");
    print(u, "if(" & s0 & "<=" & s1 & ")");
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
    u.comment("check_eq");
    print(u, "if(" & s0 & "!=" & s1 & ")");
    reportfault(u, code);
END check_eq;

PROCEDURE reportfault(u: U; code: RuntimeError) =
(* 32: see M3CG.RuntimeError, RuntimeError.T *)
VAR info := ORD (code) + u.line * 32;
BEGIN
    <* ASSERT ORD (code) < 32 *> (* lose fault code not ok *)
    (* ASSERT u.line <= (LAST(INTEGER) DIV 32) *) (* losing line number ok *)
    print(u, u.report_fault & "(" & IntToDec(info) & ");");
END reportfault;

(*---------------------------------------------------- address arithmetic ---*)

PROCEDURE add_offset(u: U; offset: INTEGER) =
(* s0.A := s0.A + offset *)
VAR s0 := cast(get(u, 0), Type.Addr);
BEGIN
    u.comment("add_offset");
    pop(u);
    push(u, Type.Addr, address_plus_offset(s0, offset));
END add_offset;

PROCEDURE index_address(u: U; type: IType; size: INTEGER) =
(* s1.A := s1.A + s0.type * size; pop *)
VAR s0 := cast(get(u, 0), type);
    s1 := cast(get(u, 1), Type.Addr);
    size_text := IntToDec(size);
BEGIN
    u.comment("index_address");
    IF size = 0 THEN
        pop(u);
        <* ASSERT FALSE *>
    ELSE
        pop(u, 2);
        push(u, Type.Addr, paren(s1 & "+" & paren(size_text & "*" & s0)));
    END;
END index_address;

(*------------------------------------------------------- PROCEDURE calls ---*)

PROCEDURE start_call_helper(u: U) =
BEGIN
    u.static_link[u.in_proc_call] := NIL;
    <* ASSERT u.params.size() = 0 *>
    INC(u.in_proc_call);
END start_call_helper;

PROCEDURE start_call_direct(u: U; p: M3CG.Proc; <*UNUSED*>level: INTEGER; <*UNUSED*>type: Type) =
(* begin a procedure call to a procedure at static level 'level'. *)
VAR proc := NARROW(p, Proc_t);
BEGIN
    u.comment("start_call_direct");
    start_call_helper(u);
    (* workaround frontend bug? *)
    IF proc.is_exception_handler THEN
        push(u, Type.Addr, "0");
        pop_parameter_helper(u, "0");
    END;
    (*
    IF level > u.current_proc.level THEN
        static_link := "(&_static_link)";
        FOR i := u.current_proc.level TO level DO
            static_link := static_link & "->_static_link";
        END;
        push(u, Type.Addr, static_link);
        pop_parameter_helper(u, static_link);
    END;
    *)
END start_call_direct;

PROCEDURE start_call_indirect(u: U; <*UNUSED*>type: Type; <*UNUSED*>callingConvention: CallingConvention) =
(* begin a procedure call to a procedure at static level 'level'. *)
BEGIN
    u.comment("start_call_indirect");
    start_call_helper(u);
END start_call_indirect;

PROCEDURE pop_parameter_helper(u: U; param: TEXT) =
BEGIN
    <* ASSERT u.in_proc_call > 0 *>
    u.params.addhi(param);
    pop(u);
END pop_parameter_helper;

PROCEDURE pop_param(u: U; type: MType) =
(* pop s0 and make it the "next" parameter in the current call *)
VAR s0 := cast(get(u, 0), type);
BEGIN
    u.comment("pop_param");
    pop_parameter_helper(u, s0);
END pop_param;

PROCEDURE pop_struct(u: U; <*UNUSED*>typeid: TypeUID; <*UNUSED*>byte_size: ByteSize; <*UNUSED*>alignment: Alignment) =
(* pop s0 and make it the "next" parameter in the current call
* NOTE: it is passed by value *)
VAR s0 := get(u, 0);
BEGIN
    u.comment("pop_struct");
    (* pop_parameter_helper(u, "*(M3STRUCT(" & IntToDec(byte_size) & ") * )" & s0); *)
    (* pop_parameter_helper(u, s0); *)
    (* BUG? local procedure take struct pointers, imported ones take ADDRESS? *)
    pop_parameter_helper(u, "(void * )" & s0);
END pop_struct;

PROCEDURE pop_static_link(u: U) =
VAR var := declare_temp(u, CG_Bytes[Type.Addr], CG_Bytes[Type.Addr], Type.Addr, FALSE);
BEGIN
    u.comment("pop_static_link");
    <* ASSERT u.in_proc_call > 0 *>
    u.static_link[u.in_proc_call - 1] := var;
    u.store(var, 0, Type.Addr, Type.Addr);
END pop_static_link;

PROCEDURE call_helper(u: U; type: Type; proc: TEXT) =
VAR comma := "";
BEGIN
    <* ASSERT u.in_proc_call > 0 *>
    DEC(u.in_proc_call);
    proc := proc & "(";
    WHILE u.params.size() > 0 DO
      proc := proc & comma & u.params.remlo();
      comma := ",";
    END;
    proc := proc & ")";
    IF type = Type.Void THEN
        print(u, proc & ";");
    ELSE
        push(u, type, proc);
    END;
END call_helper;

PROCEDURE get_static_link(u: U; target: Proc_t): TEXT =
VAR target_level := target.level;
    current := u.current_proc;
    current_level := current.level;
    static_link := "";
BEGIN
    u.comment("get_static_link");
    IF target_level = 0 THEN
        RETURN "((ADDRESS)0)";
    END;
    IF current_level = target_level THEN
        RETURN "_static_link";
    END;
    IF current_level < target_level THEN
        RETURN "&_frame";
    END;
    static_link := "_frame._static_link";
    WHILE current_level > target_level DO
        static_link := static_link & "->_static_link";
        DEC(current_level);
    END;
    RETURN static_link;
END get_static_link;

PROCEDURE call_direct(u: U; p: M3CG.Proc; type: Type) =
(* call the procedure identified by M3CG.Proc p. The procedure
   returns a value of type type. *)
VAR proc := NARROW(p, Proc_t);
BEGIN
    u.comment("call_direct");

    <* ASSERT u.in_proc_call > 0 *>

    IF proc.level # 0 THEN
        u.params.addhi(get_static_link(u, proc));
    END;

    call_helper(u, type, M3ID.ToText(proc.name));
END call_direct;

PROCEDURE call_indirect(u: U; type: Type; <*UNUSED*>callingConvention: CallingConvention) =
(* call the procedure whose address is in s0.A and pop s0. The
   procedure returns a value of type type. *)
VAR s0 := get(u, 0);
    static_link := u.static_link[u.in_proc_call - 1];
BEGIN
    u.comment("call_indirect");

    pop(u);

    <* ASSERT u.in_proc_call > 0 *>

    IF static_link # NIL THEN
        u.params.addhi(M3ID.ToText(static_link.name));
        free_temp(u, static_link);
        u.static_link[u.in_proc_call - 1] := NIL;
    END;

    (* UNDONE: cast to more accurate function type *)
    call_helper(u, type, "((" & typeToText[type] & " (__cdecl*)(M3_DOTDOTDOT))" & s0 & ")");

END call_indirect;

(*------------------------------------------- procedure and closure types ---*)

PROCEDURE load_procedure(u: U; p: M3CG.Proc) =
(* push; s0.A := ADDR (proc's body) *)
VAR proc := NARROW(p, Proc_t);
BEGIN
    u.comment("load_procedure");
    (* UNDONE? typeing? *)
    push(u, Type.Addr, M3ID.ToText(proc.name));
END load_procedure;

PROCEDURE load_static_link(u: U; p: M3CG.Proc) =
(* push; s0.A := (static link needed to call proc, NIL for top-level procs) *)
VAR target := NARROW(p, Proc_t);
BEGIN
    u.comment("load_static_link");
    IF target.level = 0 THEN
        u.load_nil();
        RETURN;
    END;
    push(u, Type.Addr, get_static_link(u, target));
END load_static_link;

(*----------------------------------------------------------------- misc. ---*)

PROCEDURE comment_1(VAR text: TEXT; VAR length: INTEGER) = 
BEGIN
    IF text = NIL THEN
        text := "";
        RETURN;
    END;
    INC(length, Text.Length(text));
END comment_1;

PROCEDURE comment(u: U; a, b, c, d: TEXT := NIL) =
VAR length := 0;
BEGIN
    IF NOT u.debug THEN
        RETURN;
    END;
    comment_1(a, length);
    comment_1(b, length);
    comment_1(c, length);
    comment_1(d, length);
    IF length = 0 THEN
        RETURN;
    END;
    print(u, " /* " & a & b & c & d & " */\n");
END comment;

(*--------------------------------------------------------------- atomics ---*)

PROCEDURE store_ordered(u: U; ztype: ZType; mtype: MType; <*UNUSED*>order: MemoryOrder) =
(* Mem [s1.A].mtype := s0.ztype;
   pop (2) *)
VAR s0 := get(u, 0);
    s1 := get(u, 1);
BEGIN
    u.comment("store_ordered");
    print(u, " /* store_ordered => store */ ");
    store_helper(u, s0, ztype, s1, 0, mtype);
END store_ordered;

PROCEDURE load_ordered(u: U; mtype: MType; ztype: ZType; <*UNUSED*>order: MemoryOrder) =
(* s0.ztype := Mem [s0.A].mtype  *)
VAR s0 := get(u);
BEGIN
    u.comment("load_ordered");
    print(u, " /* load_ordered */ ");
    pop(u);
    load_helper(u, s0, 0, mtype, ztype);
END load_ordered;

<*NOWARN*>PROCEDURE exchange(u: U; mtype: MType; ztype: ZType; <*UNUSED*>order: MemoryOrder) =
(* tmp := Mem [s1.A + offset].mtype;
   Mem [s1.A + offset].mtype := s0.ztype;
   s0.ztype := tmp;
   pop *)
BEGIN
    u.comment("exchange");
    print(u, " /* exchange */ ");
END exchange;

<*NOWARN*>PROCEDURE compare_exchange(u: U; mtype: MType; ztype: ZType; result_type: IType;
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
    u.comment("compare_exchange");
    print(u, " /* compare_exchange */ ");
END compare_exchange;

PROCEDURE fence(u: U; <*UNUSED*>order: MemoryOrder) =
(*
 * x86: Exchanging any memory with any register is a serializing instruction.
 *)
BEGIN
    u.comment("fence");
    print(u, " /* fence */ ");
    <* ASSERT u.in_proc *>
    <* ASSERT u.current_proc # NIL *>
    print(u, "m3_fence();");
END fence;

<*NOWARN*>CONST AtomicOpName = ARRAY AtomicOp OF TEXT { "add", "sub", "or", "and", "xor" };

<*NOWARN*>PROCEDURE fetch_and_op(u: U; atomic_op: AtomicOp; mtype: MType; ztype: ZType;
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
    u.comment("fetch_and_op");
    print(u, " /* fetch_and_op */ ");
    <* ASSERT CG_Bytes[ztype] >= CG_Bytes[mtype] *>
END fetch_and_op;

BEGIN
(*
    BitSizeToEnumCGType[8] := M3CG.Type.Word8;
    BitSizeToEnumCGType[16] := M3CG.Type.Word16;
    BitSizeToEnumCGType[32] := M3CG.Type.Word32;
*)
END M3C.
