MODULE M3C;

IMPORT RefSeq, TextSeq, Wr, Text, IntRefTbl, SortedIntRefTbl;
IMPORT M3CG, M3CG_Ops, Target, TFloat, TargetMap, IntArraySort;
IMPORT M3ID, TInt, ASCII, TextUtils, Cstdint, Long;
FROM TargetMap IMPORT CG_Bytes;
FROM M3CG IMPORT Name, ByteOffset, TypeUID, CallingConvention;
FROM M3CG IMPORT BitSize, ByteSize, Alignment, Frequency;
FROM M3CG IMPORT Label, Sign, BitOffset;
FROM M3CG IMPORT Type, ZType, AType, RType, IType, MType;
FROM M3CG IMPORT CompareOp, ConvertOp, RuntimeError, MemoryOrder, AtomicOp;
FROM Target IMPORT CGType;
FROM M3CG_Ops IMPORT ErrorHandler;
IMPORT M3CG_MultiPass, M3CG_DoNothing, M3CG_Binary, RTIO;

(* Taken together, these help debugging, as you get more lines in the
C and the error messages reference C line numbers *)
  CONST output_line_directives = TRUE;
  CONST output_extra_newlines = FALSE;
  CONST inline_extract = FALSE;
  
(* ztype: zero extended type -- a "larger" type that is a multiple of 32 bits in size
 *                              a type to store in registers, a type
 *                              to store on the compile-time or runtime stack
 *                              a type to pass a parameter as
 * mtype: memory type -- a "smaller" type that is possibly truncated to fit
 *        an in-memory layout
 *)

TYPE Multipass_t = M3CG_MultiPass.T BRANDED "M3C.Multipass_t" OBJECT
        self: T;
    OVERRIDES
        end_unit := multipass_end_unit;
    END;

TYPE
T = M3CG_DoNothing.T BRANDED "M3C.T" OBJECT

        typeidToType: IntRefTbl.T := NIL; (* FUTURE INTEGER => Type_t *)
        vars: REF ARRAY OF Var_t;
        procs: REF ARRAY OF Proc_t;
        current_block: Block_t := NIL;
    
        multipass: Multipass_t := NIL;
        Err    : ErrorHandler := NIL;
        anonymousCounter := -1;
        c      : Wr.T := NIL;
        debug  := 1;
        stack  : TextSeq.T := NIL;
        params : TextSeq.T := NIL;
        pop_static_link_temp_vars : RefSeq.T := NIL;
        pop_static_link_temp_vars_index := 0;
        
        enum_type: TEXT := NIL;
        extra_scope_close_braces := ""; (* hack to account for locals/temps within code *)
        last_char_was_open_brace := FALSE;
        (*enum: Enum_t := NIL;*)
        enum_id: TEXT := NIL;
        enum_value: CARDINAL := 0;
        unit_name := "L_";
        handler_name_prefixes := ARRAY [FIRST(HandlerNamePieces) .. LAST(HandlerNamePieces)] OF TEXT{NIL, ..};
        param_count := 0;
        label := 1000;
        static_link_id: M3ID.T;
        
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

    METHODS
        Type_Init(type: Type_t): Type_t := Type_Init;

    OVERRIDES
        end_unit   := end_unit;

        next_label := next_label;
        set_error_handler := set_error_handler;
        begin_unit := begin_unit;
        import_unit := import_unit;
        export_unit := export_unit;
        set_source_file := set_source_file;
        set_source_line := set_source_line;
        set_runtime_proc := set_runtime_proc;
        bind_segment := bind_segment;
        declare_global := declare_global;
        declare_constant := declare_constant;
        begin_init := begin_init;
        end_init := end_init;
        init_int := init_int;
        init_proc := init_proc;
        init_label := init_label;
        init_var := init_var;
        init_offset := init_offset;
        init_chars := init_chars;
        init_float := init_float;
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
        store := store;
        load_address := load_address;
        load_indirect := load_indirect;
        store_indirect := store_indirect;
        load_nil := load_nil;
        load_integer := load_target_integer;
        load_float := load_float;
        compare := compare;
        add := add;
        subtract := subtract;
        multiply := multiply;
        divide := divide;
        div := div;
        mod := mod;
        negate := negate;
        abs := abs;
        max := max;
        min := min;
        cvt_int := cvt_int;
        cvt_float := cvt_float;
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
        pop := cg_pop;
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

PROCEDURE SetLineDirective(self: T) =
VAR start := ARRAY BOOLEAN OF TEXT{" /* ", "#"}[output_line_directives];
    newline := ARRAY BOOLEAN OF TEXT{"", "\n"}[output_line_directives];
    end := ARRAY BOOLEAN OF TEXT{" */ ", "\n"}[output_line_directives];
BEGIN
    IF self.line > 0 AND self.file # NIL THEN
        self.line_directive := start & "line " & IntToDec(self.line) & " \"" & self.file & "\"" & end;
        self.nl_line_directive := newline & self.line_directive;
        IF self.last_char_was_newline THEN
            print(self, self.line_directive);
        ELSE
            print(self, self.nl_line_directive);
        END;
    ELSE
        self.line_directive := "";
        self.nl_line_directive := newline;
    END;
END SetLineDirective;

TYPE ReplacementName_t = RECORD
    id: M3ID.T;
    replacement_id: M3ID.T;
END;

<*UNUSED*>PROCEDURE Reverse(VAR a: ARRAY OF CHAR) =
VAR i := FIRST(a);
    j := LAST(a);
    self: CHAR;
BEGIN
    WHILE i < j DO
        self := a[i];
        a[i] := a[j];
        a[j] := self;
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

PROCEDURE AnonymousCounter(self: T): INTEGER =
BEGIN
    INC(self.anonymousCounter, 1 + ORD(self.anonymousCounter = 385)); (* avoid "i386" -- really, it happened *)
    RETURN self.anonymousCounter;
END AnonymousCounter;

PROCEDURE GenerateName(self: T): Name =
BEGIN
    RETURN M3ID.Add("L_" & IntToDec(AnonymousCounter(self)));
END GenerateName;

PROCEDURE Proc_FixName(self: T; name: Name): Name =
VAR text: TEXT;
BEGIN
    IF name = M3ID.NoID THEN
        RETURN GenerateName(self);
    END;
    text := M3ID.ToText (name);
    IF Text.GetChar (text, 0) = '*' THEN
        <* ASSERT Text.Length(text) = 1 *>
        RETURN GenerateName(self);
    END;
    (* rename C names like int, short, void *)
    RETURN ReplaceName(name);
END Proc_FixName;

PROCEDURE Var_FixName(self: T; name: Name; imported_or_exported: BOOLEAN): Name =
BEGIN
    name := Proc_FixName(self, name);
    (* workaround: begin/end_block should keep the names separate,
     * but they do nothing until import_procedure all moved up.
     * e.g. ETimer__Push() has parameter and local "self"
     *)
    IF NOT imported_or_exported AND name # self.static_link_id THEN
        name := M3ID.Add(M3ID.ToText(name) & "_L_" & IntToDec(AnonymousCounter(self)));
    END;
    RETURN name;
END Var_FixName;

TYPE Type_t = OBJECT
    bit_size: INTEGER := 0;  (* FUTURE Target.Int or LONGINT *)
    byte_size: INTEGER := 0; (* FUTURE Target.Int or LONGINT *)
    typeid: INTEGER := 0;
    cg_type: M3CG.Type := M3CG.Type.Addr;
    (*name_id: INTEGER;
    name_text: TEXT;*)
END;

(*
TYPE CField = M3CField.T;
TYPE CFieldSeq = M3CFieldSeq.T;

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

PROCEDURE TypeidToType_Get(typeid: TypeUID): Type_t =
VAR type: REFANY := NIL;
BEGIN
    EVAL typeidToType.get(typeid, type);
    RETURN NARROW(type, Type_t);
END TypeidToType_Get;
*)

PROCEDURE Type_Init(self: T; type: Type_t): Type_t =
BEGIN
    IF type.bit_size = 0 THEN
        type.bit_size := TargetMap.CG_Size[type.cg_type];
    END;
    IF type.byte_size = 0 THEN
        type.byte_size := TargetMap.CG_Bytes[type.cg_type];
    END;
    EVAL self.typeidToType.put(type.typeid, type);
    RETURN type;
END Type_Init;

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
PROECURE Expr_FromText(self: TEXT): expr_t;

PROCEDURE expr_add(<*UNUSED*>left, right: expr_t): expr_t = BEGIN RETURN NIL; END expr_add;
PROCEDURE expr_sub(<*UNUSED*>left, right: expr_t): expr_t = BEGIN RETURN NIL; END expr_sub;
PROCEDURE expr_mult(<*UNUSED*>left, right: expr_t): expr_t = BEGIN RETURN NIL; END expr_mult;
*)

TYPE Var_t = M3CG.Var OBJECT
    self: T := NIL;
    name: Name := 0;
    name_in_frame: Name := 0; (* if up_level, e.g. ".block1.foo" *)
    type: Type;
    type_text: TEXT;
    const := FALSE;
    imported := FALSE;
    exported := FALSE;
    global := FALSE;
    byte_size := -1; (* esp. for structs *)
    up_level := FALSE; (* local accessed from nested function *)
    in_memory := FALSE; (* ? *)
    is_static_link := FALSE; (* micro optimization -- uplevel but accessed directly *)
    next_in_block: Var_t := NIL;
    proc: Proc_t := NIL;
    block: Block_t := NIL;

    METHODS
        Declare(): TEXT := Var_Declare;
        InFrameDeclare(): TEXT := Var_InFrameDeclare;
        Name(): TEXT := Var_Name;
        InFrameName(): TEXT := Var_Name;
        Type(): TEXT := Var_Type;
        InFrameType(): TEXT := Var_Type;
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
    var.name := Var_FixName(var.self, var.name, var.exported OR var.imported);
    RETURN var;
END Var_Init;

TYPE Block_t = OBJECT
    sibling: Block_t;
    parent: Block_t;
    child: Block_t;
    proc: Proc_t;
    vars: Var_t;
    top_in_proc: Block_t;
    name: TEXT; (* for frame *)
END;

TYPE Proc_t = M3CG.Proc OBJECT
    name: Name := 0;
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
    self: T := NIL;
    blocks: RefSeq.T := NIL; (* Block_t *)
    block_stack: RefSeq.T := NIL; (* Block_t *)
    current_block: Block_t := NIL;

    METHODS
        Locals_Size(): INTEGER := Proc_Locals_Size;
        Locals(i: INTEGER): Var_t := Proc_Locals;
        FrameName(): TEXT := Proc_FrameName;
        FrameType(): TEXT := Proc_FrameType;
        ForwardDeclareFrameType() := Proc_ForwardDeclareFrameType;
        Init(self: T): Proc_t := Proc_Init;
END;

PROCEDURE Proc_ForwardDeclareFrameType(proc: Proc_t) =
VAR self := proc.self;
BEGIN
    IF proc.forward_declared_frame_type THEN
        RETURN;
    END;
    print(self, "struct " & proc.FrameType() & ";");
    print(self, "typedef struct " & proc.FrameType() & " " & proc.FrameType() & ";");
    proc.forward_declared_frame_type := TRUE;
END Proc_ForwardDeclareFrameType;

PROCEDURE IsNameExceptionHandler(self: T; name: TEXT): BOOLEAN =
(* Is the name of the form unit_name + special + number + optionally underscore and another number ?
   See TryFinStmt.m3 *)
VAR ch: CHAR;
    underscore := FALSE;
    length, prefix_length := 0;
    prefix, end := "";
BEGIN
    length := Text.Length(name);
    FOR i := FIRST(self.handler_name_prefixes) TO LAST(self.handler_name_prefixes) DO
        prefix := self.handler_name_prefixes[i];
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

PROCEDURE Proc_Init(proc: Proc_t; self: T): Proc_t =
BEGIN
    proc.self := self;
    proc.name := Proc_FixName(proc.self, proc.name);
    proc.is_exception_handler := proc.level > 0 AND proc.n_params = 1 AND IsNameExceptionHandler(self, M3ID.ToText(proc.name));
    proc.n_params_without_static_link := proc.n_params;
    proc.add_static_link := proc.level > 0;
    INC(proc.n_params, ORD(proc.add_static_link));
    proc.locals := NEW(RefSeq.T).init();
    proc.blocks := NEW(RefSeq.T).init();
    proc.block_stack := NEW(RefSeq.T).init();
    proc.params := NEW(REF ARRAY OF Var_t, proc.n_params);
    proc.ForwardDeclareFrameType(); (* TODO don'self always do this *)
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
"int __cdecl memcmp(const void*, const void*, size_t);",

(* math.h *)
"double __cdecl floor(double);",
"double __cdecl ceil(double);",

(*"#include <limits.h>",*)

(* need multiple passes and forward declare all structs; temporary partial solution *)
"#define M3STRUCT(n) m3struct_##n##_t",
"#define M3STRUCT1(n) typedef struct { volatile UINT8 a[n]; } M3STRUCT(n);",
"#define M3STRUCT2(n) typedef struct { volatile UINT16 a[(n)/2]; } M3STRUCT(n);",
"#define M3STRUCT4(n) typedef struct { volatile UINT32 a[(n)/4]; } M3STRUCT(n);",
"#define M3STRUCT8(n) typedef struct { volatile UINT64 a[(n)/8]; } M3STRUCT(n);",
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
(* RTHooks__ReportFault's signature varies, but it isn'self imported *)
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
"#define m3_extract(T, value, offset, count) ((((T)value)>>((WORD_T)offset))&~(((~(T)0))<<((WORD_T)count)))",
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
"static WORD_T m3_strlen(const char* s) { const char* self = s; while ( *self++) { /* nothing */ } return (WORD_T)(self - s - 1); }",
"static WORD_T m3_uint_to_dec_length(WORD_T i) { WORD_T length = 0; do { length += 1; i /= 10; } while (i); return length; }",
"static char* m3_strrev(char* s, unsigned length) { if (length > 1) { unsigned i = 0; unsigned j = length - 1; while (i < j) { \\",
"char self = s[i]; s[i] = s[j]; s[j] = self; i += 1; j -= 1; } } return s; }",
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
    "T",  "",
    "T", "",
    "T", "",
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
(* It is possible we can reduce parens, but it isn'self as simple as it seems. *)
    RETURN "(" & text & ")";
END paren;

PROCEDURE pop(self: T; n: CARDINAL := 1) =
BEGIN
    FOR i := 1 TO n DO
        EVAL self.stack.remlo();
    END;
END pop;

PROCEDURE push(self: T; <*UNUSED*>type: Type; expression: TEXT) =
BEGIN
    self.stack.addlo(expression);
END push;

PROCEDURE get(self: T; n: CARDINAL := 0): TEXT =
BEGIN
    RETURN self.stack.get(n);
END get;

PROCEDURE SuppressLineDirective(self: T; adjust: INTEGER; <*UNUSED*>reason: TEXT) =
BEGIN
    INC(self.suppress_line_directive, adjust);
END SuppressLineDirective;

PROCEDURE print(self: T; text: TEXT) = <*FATAL ANY*>
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
    self.last_char_was_open_brace := text_last_char = '{';

    IF output_extra_newlines AND Text.FindChar(text, '\n') = -1 THEN
        Wr.PutText(self.c, text & "\n");
        self.last_char_was_newline := TRUE;
    ELSE
        Wr.PutText(self.c, text);
        self.last_char_was_newline := FALSE;
    END;

    IF text = self.line_directive OR text = self.nl_line_directive THEN
        self.width := 0;
        self.last_char_was_newline := TRUE;
        RETURN;
    END;

    IF (*self.suppress_line_directive < 1 AND*) text_last_char = '\n' THEN
        Wr.PutText(self.c, self.line_directive);
        self.width := 0;
        self.last_char_was_newline := TRUE;
        RETURN;
    END;

    IF Text.FindChar(text, '\n') # -1 THEN
        self.width := 0; (* roughly *)
        Wr.PutText(self.c, self.nl_line_directive);
        self.last_char_was_newline := TRUE;
        RETURN;
    END;

    INC(self.width, length);
    IF self.width < 1000 THEN
        self.last_char_was_newline := FALSE;
        RETURN;
    END;

    self.width := 0;
    IF self.last_char_was_newline THEN
        Wr.PutText(self.c, self.line_directive);
    ELSE
        Wr.PutText(self.c, self.nl_line_directive);
    END;
    self.last_char_was_newline := TRUE;
END print;

(*---------------------------------------------------------------------------*)

PROCEDURE New (cfile: Wr.T): M3CG.T =
VAR self := NEW (T);
BEGIN
    self.typeidToType := NEW(SortedIntRefTbl.Default).init(); (* FUTURE *)
    self.multipass := NEW(Multipass_t).Init();
    self.multipass.reuse_refs := TRUE; (* TODO: change them all to integers *)
    self.multipass.self := self;
    self.c := cfile;
    self.init_fields := NEW(TextSeq.T).init();  (* CONSIDER compute size or maximum and use an array *)
    self.initializer := NEW(TextSeq.T).init();  (* CONSIDER compute size or maximum and use an array *)
    self.stack := NEW(TextSeq.T).init();        (* CONSIDER compute maximum depth and use an array *)
    self.params := NEW(TextSeq.T).init();       (* CONSIDER compute maximum and use an array *)
    self.pop_static_link_temp_vars := NEW(RefSeq.T).init(); (* CONSIDER compute size -- number of pop_static_link calls *)
    self.report_fault := NIL;
(*
    EVAL self.Type_Init(NEW(Integer_t, cg_type := Target.Integer.cg_type, typeid := UID_INTEGER));
    EVAL self.Type_Init(NEW(Integer_t, cg_type := Target.Word.cg_type, typeid := UID_WORD));
    EVAL self.Type_Init(NEW(Integer_t, cg_type := Target.Int64.cg_type, typeid := UID_LONGINT));
    EVAL self.Type_Init(NEW(Integer_t, cg_type := Target.Word64.cg_type, typeid := UID_LONGWORD));

    EVAL self.Type_Init(NEW(Float_t, cg_type := Target.Real.cg_type, typeid := UID_REEL));
    EVAL self.Type_Init(NEW(Float_t, cg_type := Target.Longreal.cg_type, typeid := UID_LREEL));
    EVAL self.Type_Init(NEW(Float_t, cg_type := Target.Extended.cg_type, typeid := UID_XREEL));

    EVAL self.Type_Init(NEW(Enum_t, cg_type := Target.Word8.cg_type, typeid := UID_BOOLEAN, max := 1));
    EVAL self.Type_Init(NEW(Enum_t, cg_type := Target.Word8.cg_type, typeid := UID_CHAR, max := 16_FF));
    EVAL self.Type_Init(NEW(Enum_t, cg_type := Target.Word16.cg_type, typeid := UID_WIDECHAR, max := 16_FFFF));

    EVAL self.Type_Init(NEW(Subrange_t, cg_type := Target.Integer.cg_type, typeid := UID_RANGE_0_31, min := 0, max := 31));
    EVAL self.Type_Init(NEW(Subrange_t, cg_type := Target.Integer.cg_type, typeid := UID_RANGE_0_63, min := 0, max := 31));

    EVAL self.Type_Init(NEW(Type_t, cg_type := Target.Address.cg_type, typeid := UID_MUTEX));
    EVAL self.Type_Init(NEW(Type_t, cg_type := Target.Address.cg_type, typeid := UID_TEXT));
    EVAL self.Type_Init(NEW(Type_t, cg_type := Target.Address.cg_type, typeid := UID_ROOT));
    EVAL self.Type_Init(NEW(Type_t, cg_type := Target.Address.cg_type, typeid := UID_REFANY));
    EVAL self.Type_Init(NEW(Type_t, cg_type := Target.Address.cg_type, typeid := UID_ADDR));
    EVAL self.Type_Init(NEW(Type_t, cg_type := Target.Address.cg_type, typeid := UID_PROC1));
    EVAL self.Type_Init(NEW(Type_t, cg_type := Target.Address.cg_type, typeid := UID_PROC2));
    EVAL self.Type_Init(NEW(Type_t, cg_type := Target.Address.cg_type, typeid := UID_PROC3));
    EVAL self.Type_Init(NEW(Type_t, cg_type := Target.Address.cg_type, typeid := UID_PROC4));
    EVAL self.Type_Init(NEW(Type_t, cg_type := Target.Address.cg_type, typeid := UID_PROC5));
    EVAL self.Type_Init(NEW(Type_t, cg_type := Target.Address.cg_type, typeid := UID_PROC6));
    EVAL self.Type_Init(NEW(Type_t, cg_type := Target.Address.cg_type, typeid := UID_PROC7));
    EVAL self.Type_Init(NEW(Type_t, cg_type := Target.Address.cg_type, typeid := UID_PROC8));
*)
    (* EVAL Type_Init(NEW(Type_t, bit_size := 0, byte_size := 0, typeid := UID_NULL)); *)
    RETURN self.multipass;
END New;

(*----------------------------------------------------------- ID counters ---*)

PROCEDURE next_label(self: T; n: INTEGER := 1): Label =
VAR label := self.label;
BEGIN
    INC(self.label, n);
    RETURN label;
END next_label;

(*------------------------------------------------ READONLY configuration ---*)

PROCEDURE set_error_handler (<*NOWARN*>self: T; <*NOWARN*>p: ErrorHandler) =
BEGIN
    self.Err := p;
END set_error_handler;

(*----------------------------------------------------- compilation units ---*)

PROCEDURE Prefix_Start(self: T) =
BEGIN
    print(self, " /* begin unit */\n");
    print(self, "/* M3_TARGET = " & Target.System_name & " */ ");
    print(self, "/* M3_WORDSIZE = " & IntToDec(Target.Word.size) & " */ ");
    self.static_link_id := M3ID.Add("_static_link");
    SuppressLineDirective(self, 1, "begin_unit");
    FOR i := FIRST(Prefix) TO LAST(Prefix) DO
        print(self, Prefix[i] & "\n");
    END;
END Prefix_Start;

PROCEDURE Prefix_End(self: T) =
BEGIN
    SuppressLineDirective(self, -1, "begin_unit");
END Prefix_End;

PROCEDURE multipass_end_unit(self: Multipass_t) =
(* called at the of the first pass -- we have everything
   in memory now, except for the end_unit itself.
   
   This function is in control of coordinating the passes.
 *)
BEGIN
    M3CG_MultiPass.end_unit(self); (* let M3CG_MultiPass do its usual last step *)
    self.Replay(self.self, self.op_data[M3CG_Binary.Op.begin_unit]);
    Prefix_Start(self.self);
    GetStructSizes(self);
    Prefix_End(self.self);

    (* forward declare functions/variables in this module and imports *)
    
    self.self.comment("begin pass: imports");
    self.Replay(NEW(Imports_t).Init(self.self));
    self.self.comment("end pass: imports");

    (* discover all locals (including temps and params) *)

    self.self.comment("begin pass: locals");
    self.Replay(NEW(Locals_t).Init(self.self));
    self.self.comment("emd pass: locals");

    (* last pass *)

    self.Replay(self.self);
END multipass_end_unit;

PROCEDURE begin_unit(self: T; <*UNUSED*>optimize: INTEGER) =
(* The first call in a particular pass. *)
BEGIN
    self.in_proc := FALSE;
    self.current_proc := NIL;
    self.in_proc_call := 0;
    self.pop_static_link_temp_vars_index := 0;
END begin_unit;

PROCEDURE end_unit(self: T) =
(* The last call in a particular pass. *)
BEGIN
    print(self, " /* end unit */\n");
    self.line_directive := ""; (* really suppress *)
    self.nl_line_directive := "\n"; (* really suppress *)
    SuppressLineDirective(self, 1, "end_unit");
    FOR i := FIRST(Suffix) TO LAST(Suffix) DO
        print(self, Suffix[i]);
        print(self, "\n");
    END;
    SuppressLineDirective(self, -1, "end_unit");
END end_unit;

PROCEDURE import_unit(self: T; <*UNUSED*>name: Name) =
(* note that the current compilation unit imports the interface 'name' *)
BEGIN
    self.comment("import_unit");
END import_unit;

PROCEDURE export_unit(self: T; <*UNUSED*>name: Name) =
(* note that the current compilation unit exports the interface 'name' *)
BEGIN
    self.comment("export_unit");
END export_unit;

(*------------------------------------------------ debugging line numbers ---*)

PROCEDURE set_source_file(self: T; file: TEXT) =
(* Sets the current source file name. Subsequent statements
   and expressions are associated with this source location. *)
BEGIN
    self.comment("set_source_file");
    self.file := file;
    SetLineDirective(self);
END set_source_file;

PROCEDURE set_source_line(self: T; line: INTEGER) =
(* Sets the current source line number. Subsequent statements
and expressions are associated with this source location. *)
BEGIN
    self.comment("set_source_line");
    self.line := line;
    SetLineDirective(self);
END set_source_line;

(*------------------------------------------- debugging type declarations ---*)

<*NOWARN*>PROCEDURE declare_typename(self: T; typeid: TypeUID; name: Name) =
BEGIN
    self.comment("declare_typename");
    (*
    print(self, "typedef M" & TypeidToHex(typeid) & " " & M3ID.ToText(name) & ";\n");
    *)
END declare_typename;

(*
PROCEDURE TypeIDToText(x: INTEGER): TEXT =
BEGIN
    RETURN "M" & Fmt.Unsigned(x);
END TypeIDToText;
*)

<*NOWARN*>PROCEDURE declare_array(self: T; typeid, index_typeid, element_typeid: TypeUID; total_bit_size: BitSize) =
BEGIN
    self.comment("declare_array");
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

      print(self, "typedef struct{");
      print(self, TypeIDToText(element_typeid));
      print(self, " _elts[");
      print(self, IntToDec(total_bit_size DIV element_type.bit_size));
      print(self, "];}");
      print(self, TypeIDToText(typeid));
      print(self, ";");
    END;
*)
  END declare_array;

<*NOWARN*>PROCEDURE declare_open_array(self: T; typeid, element_typeid: TypeUID; bit_size: BitSize) =
BEGIN
    self.comment("declare_open_array");
    <* ASSERT bit_size MOD 32 = 0 *>
(*
    WITH element_type = TypeidToType_Get(element_typeid) DO
        IF element_type = NIL THEN
            RTIO.PutText("declare_array nil element_type\n");
            RTIO.Flush();
        END;
        print(self, "typedef struct { ");
        print(self, TypeIDToText(element_typeid));
        print(self, "* _elts; CARDINAL _size");
        IF bit_size > Target.Integer.size * 2 THEN
            print(self, "s[");
            print(self, IntToDec((bit_size - Target.Integer.size) DIV Target.Integer.size));
            print(self, "]");
        END;
        print(self, ";}" & TypeIDToText(element_typeid) & ";");
        EVAL typeidToType.put(typeid, NEW(OpenArray_t,
        typeid := typeid,
        byte_size := bit_size DIV 8,
        bit_size := bit_size,
        element_typeid := element_typeid,
        element_type := element_type));
    END;
*)
  END declare_open_array;

<*NOWARN*>PROCEDURE declare_enum(self: T; typeid: TypeUID; n_elts: INTEGER; bit_size: BitSize) =
BEGIN
    self.comment("declare_enum");
    SuppressLineDirective(self, n_elts, "declare_enum n_elts");
    <* ASSERT bit_size = 8 OR bit_size = 16 OR bit_size = 32 *>
    (*
    WITH type = NEW(Enum_t, typeid := typeid, max := n_elts - 1, cg_type := BitSizeToEnumCGType[bit_size]) DO
        <* ASSERT self.enum = NIL *>
        self.enum := type;
        EVAL Type_Init(type);
        self.enum_id := TypeIDToText(typeid);
        self.enum_value := 0;
        self.enum_type := "UINT" & IntToDec(bit_size);
        print(self, "typedef " & self.enum_type & " " & self.enum_id & ";");
    END;
*)
END declare_enum;

<*NOWARN*>PROCEDURE declare_enum_elt(self: T; name: Name) =
BEGIN
    self.comment("declare_enum_elt");
    SuppressLineDirective(self, -1, "declare_enum_elt");
(*
    print(self, "#define " & self.enum_id & "_" & M3ID.ToText(name) & " ((" & self.enum_type & ")" & IntToDec(self.enum_value) & ")\n");
    INC(self.enum_value);
    IF self.enum_value = self.enum.max + 1 THEN
        self.enum := NIL;
        self.enum_id := NIL;
        self.enum_type := NIL;
        self.enum_value := 10000;
    END;
*)
  END declare_enum_elt;

<*NOWARN*>PROCEDURE declare_packed(self: T; typeid: TypeUID; bit_size: BitSize; base: TypeUID) =
BEGIN
    self.comment("declare_packed");
END declare_packed;

<*NOWARN*>PROCEDURE declare_record(self: T; typeid: TypeUID; bit_size: BitSize; n_fields: INTEGER) =
BEGIN
    self.comment("declare_record");
    SuppressLineDirective(self, n_fields, "declare_record n_fields");
END declare_record;

<*NOWARN*>PROCEDURE declare_field(self: T; name: Name; offset: BitOffset; bit_size: BitSize; typeid: TypeUID) =
BEGIN
    self.comment("declare_field");
    SuppressLineDirective(self, -1, "declare_field");
END declare_field;

<*NOWARN*>PROCEDURE declare_set(self: T; typeid, domain: TypeUID; bit_size: BitSize) =
BEGIN
    self.comment("declare_set");
END declare_set;

<*NOWARN*>PROCEDURE declare_subrange(self: T; typeid, domain: TypeUID; READONLY min, max: Target.Int; bit_size: BitSize) =
BEGIN
    self.comment("declare_subrange");
END declare_subrange;

<*NOWARN*>PROCEDURE declare_pointer(self: T; typeid, target: TypeUID; brand: TEXT; traced: BOOLEAN) =
BEGIN
    self.comment("declare_pointer");
END declare_pointer;

<*NOWARN*>PROCEDURE declare_indirect(self: T; typeid, target: TypeUID) =
BEGIN
    self.comment("declare_indirect");
END declare_indirect;

<*NOWARN*>PROCEDURE declare_proctype(self: T; typeid: TypeUID; n_formals: INTEGER; result: TypeUID; n_raises: INTEGER; callingConvention: CallingConvention) =
BEGIN
    self.comment("declare_proctype");
    (* SuppressLineDirective(self, n_formals + (ORD(n_raises >= 0) * n_raises), "declare_proctype n_formals + n_raises"); *)
END declare_proctype;

<*NOWARN*>PROCEDURE declare_formal(self: T; name: Name; typeid: TypeUID) =
BEGIN
    self.comment("declare_formal");
    (* SuppressLineDirective(self, -1, "declare_formal"); *)
END declare_formal;

<*NOWARN*>PROCEDURE declare_raises(self: T; name: Name) =
BEGIN
    self.comment("declare_raises");
    (* SuppressLineDirective(self, -1, "declare_raises"); *)
END declare_raises;

<*NOWARN*>PROCEDURE declare_object(self: T; typeid, super: TypeUID; brand: TEXT; traced: BOOLEAN; n_fields, n_methods: INTEGER; field_size: BitSize) =
BEGIN
    self.comment("declare_object");
    (* SuppressLineDirective(self, n_fields + n_methods, "declare_object n_fields + n_methods"); *)
END declare_object;

<*NOWARN*>PROCEDURE declare_method(self: T; name: Name; signature: TypeUID) =
BEGIN
    self.comment("declare_method");
    SuppressLineDirective(self, -1, "declare_method");
END declare_method;

<*NOWARN*>PROCEDURE declare_opaque(self: T; typeid, super: TypeUID) =
BEGIN
    self.comment("declare_opaque");
END declare_opaque;

<*NOWARN*>PROCEDURE reveal_opaque(self: T; lhs, rhs: TypeUID) =
BEGIN
    self.comment("reveal_opaque");
END reveal_opaque;

<*NOWARN*>PROCEDURE declare_exception(self: T; name: Name; arg_type: TypeUID; raise_proc: BOOLEAN; base: M3CG.Var; offset: INTEGER) =
BEGIN
    self.comment("declare_exception");
END declare_exception;

(*--------------------------------------------------------- runtime hooks ---*)

<*NOWARN*>PROCEDURE set_runtime_proc(self: T; name: Name; p: M3CG.Proc) =
BEGIN
    self.comment("set_runtime_proc");
END set_runtime_proc;

(*------------------------------------------------- variable declarations ---*)

PROCEDURE import_global(self: T; name: Name; byte_size: ByteSize; alignment: Alignment; type: Type; <*UNUSED*>typeid: TypeUID): M3CG.Var =
VAR var := NEW(Var_t, self := self, type := type, name := name, imported := TRUE).Init();
    declaration: TEXT;
BEGIN
    self.comment("import_global");
    declaration := "extern " & typeToText[type] & " " & M3ID.ToText(var.name) & ";";

    <* ASSERT (byte_size MOD alignment) = 0 *>
    <* ASSERT NOT self.in_proc *>
    print(self, declaration);
    RETURN var;
END import_global;

PROCEDURE Locals_DeclareSegment(
    self: Locals_t;
    name: Name;
    typeid: TypeUID;
    const: BOOLEAN): M3CG.Var =
BEGIN
    RETURN declare_segment(self.self, name, typeid, const);
END Locals_DeclareSegment;

CONST ConstText = ARRAY BOOLEAN OF TEXT{"", " const "};

PROCEDURE declare_segment(self: T; name: Name; <*UNUSED*>typeid: TypeUID; const: BOOLEAN): M3CG.Var =
VAR var := NEW(Var_t, self := self, name := name, const := const).Init();
    fixed_name := var.name;
    text: TEXT := NIL;
    length := 0;
    const_text := ConstText[const];
BEGIN
    self.comment("declare_segment");
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
            self.unit_name := text;
            FOR i := FIRST(HandlerNamePieces) TO LAST(HandlerNamePieces) DO
                self.handler_name_prefixes[i] := text & HandlerNamePieces[i];
            END;
        END;
    END;
    text := M3ID.ToText(fixed_name);
    print(self, "struct " & text & "_t;");
    print(self, "typedef struct " & text & "_t " & text & "_t;");
    print(self, const_text & "static " & text & "_t " & text & ";");

    IF self.report_fault = NIL AND NOT const THEN (* See M3x86.m3 *)
        self.report_fault := M3ID.ToText(var.name) & "_CRASH";
        print(self, "void __cdecl " & self.report_fault & "(UINT32 code) { RTHooks__ReportFault((ADDRESS)&" & M3ID.ToText(var.name) & ",code);}");
    END;

    RETURN var;
  END declare_segment;

PROCEDURE bind_segment(
    self: T;
    v: M3CG.Var;
    byte_size: ByteSize;
    alignment: Alignment;
    <*UNUSED*>type: Type;
    <*UNUSED*>exported: BOOLEAN;
    <*UNUSED*>inited: BOOLEAN) =
VAR var := NARROW(v, Var_t);
BEGIN
    self.comment("bind_segment");
    <* ASSERT (byte_size MOD alignment) = 0 *>
    print(self, " /* bind_segment */ ");
    var.byte_size := byte_size;
END bind_segment;

PROCEDURE declare_global(self: T; name: Name; byte_size: ByteSize; alignment: Alignment;
                         type: Type; typeid: TypeUID; exported: BOOLEAN; inited: BOOLEAN): M3CG.Var =
BEGIN
    print(self, " /* declare_global */ ");
    RETURN DeclareGlobal(self, name, byte_size, alignment, type, typeid, exported, inited, FALSE);
END declare_global;

PROCEDURE declare_constant(
    self: T;
    name: Name;
    byte_size: ByteSize;
    alignment: Alignment;
    type: Type;
    typeid: TypeUID;
    exported: BOOLEAN;
    inited: BOOLEAN): M3CG.Var =
BEGIN
    print(self, " /* declare_constant */ ");
    RETURN DeclareGlobal(self, name, byte_size, alignment, type, typeid, exported, inited, TRUE);
END declare_constant;

PROCEDURE DeclareGlobal(self: T; name: Name; byte_size: ByteSize; alignment: Alignment; type: Type; <*UNUSED*>typeid: TypeUID; exported: BOOLEAN; <*UNUSED*>inited: BOOLEAN; const: BOOLEAN): M3CG.Var =
CONST DeclTag = ARRAY BOOLEAN OF TEXT { "declare_global", "declare_constant" };
VAR   var := NEW(Var_t, self := self, type := type, name := name, const := const,
                 (*inited := inited, typeid := typeid, alignment := alignment,*)
                 exported := exported, global := TRUE,
                 proc := self.current_proc, byte_size := byte_size).Init();
BEGIN
    self.comment(DeclTag [const]);
    <* ASSERT (byte_size MOD alignment) = 0 *>
    print(self, var.Declare() & " M3_INIT;");
    RETURN var;
END DeclareGlobal;

TYPE Locals_t = M3CG_DoNothing.T BRANDED "M3C.Locals_t" OBJECT
    self: T := NIL;
METHODS
    Init(outer: T): Locals_t := Locals_Init;
OVERRIDES
    declare_segment := Locals_DeclareSegment; (* declare_segment is needed, to get the unit name, to check for exception handlers *)
    declare_procedure := Locals_DeclareProcedure;
    begin_procedure := Locals_BeginProcedure;
    end_procedure := Locals_EndProcedure;
    declare_param := Locals_DeclareParam;
    declare_local := Locals_DeclareLocal;
    declare_temp := Locals_DeclareTemp;
    begin_block := Locals_BeginBlock;   (* FUTURE: for unions in frame struct *)
    end_block := Locals_EndBlock;       (* FUTURE: for unions in frame struct *)
    pop_static_link := Locals_PopStaticLink; (* pop_static_link is needed because it calls declare_temp *)
END;

PROCEDURE Locals_Init(self: Locals_t; outer: T): Locals_t =
BEGIN
    self.self := outer;
    RETURN self;
END Locals_Init;

PROCEDURE Locals_DeclareParam(
    self: Locals_t;
    name: Name;
    byte_size: ByteSize;
    alignment: Alignment;
    type: Type;
    typeid: TypeUID;
    in_memory: BOOLEAN;
    up_level: BOOLEAN;
    frequency: Frequency): M3CG.Var =
BEGIN
    RETURN declare_param(self.self, name, byte_size, alignment, type, typeid,
        in_memory, up_level, frequency);
END Locals_DeclareParam;

PROCEDURE Locals_DeclareLocal(
    self: Locals_t;
    name: Name;
    byte_size: ByteSize;
    alignment: Alignment;
    type: Type;
    typeid: TypeUID;
    in_memory: BOOLEAN;
    up_level: BOOLEAN;
    frequency: Frequency): M3CG.Var =
BEGIN
    RETURN declare_local(self.self, name, byte_size, alignment, type, typeid, in_memory, up_level, frequency);
END Locals_DeclareLocal;

TYPE Imports_t = M3CG_DoNothing.T BRANDED "M3C.Imports_t" OBJECT
    self: T;
METHODS
    Init(outer: T): Imports_t := Imports_Init;
OVERRIDES
    import_procedure := Imports_ImportProcedure;
    declare_param := Imports_DeclareParam;
    import_global := Imports_ImportGlobal;
END;

PROCEDURE Imports_Init(self: Imports_t; outer: T): Imports_t =
BEGIN
    self.self := outer;
    RETURN self;
END Imports_Init;

PROCEDURE Imports_ImportProcedure(
    self: Imports_t;
    name: Name;
    n_params: INTEGER;
    return_type: Type;
    callingConvention: CallingConvention): M3CG.Proc =
BEGIN
    RETURN import_procedure(self.self, name, n_params, return_type, callingConvention);
END Imports_ImportProcedure;

PROCEDURE Imports_DeclareParam(
    self: Imports_t;
    name: Name;
    byte_size: ByteSize;
    alignment: Alignment;
    type: Type;
    typeid: TypeUID;
    in_memory: BOOLEAN;
    up_level: BOOLEAN;
    frequency: Frequency): M3CG.Var =
BEGIN
    RETURN declare_param(self.self, name, byte_size, alignment, type, typeid,
        in_memory, up_level, frequency);
END Imports_DeclareParam;

PROCEDURE Imports_ImportGlobal(
    self: Imports_t;
    name: Name;
    byte_size: ByteSize; 
    alignment: Alignment;
    type: Type;
    typeid: TypeUID): M3CG.Var =
BEGIN
    RETURN import_global(self.self, name, byte_size, alignment, type, typeid);
END Imports_ImportGlobal;

TYPE GetStructSizes_t = M3CG_DoNothing.T BRANDED "M3C.GetStructSizes_t" OBJECT
    sizes: REF ARRAY OF INTEGER := NIL;
    count := 0;
METHODS
    Declare(type: Type; byte_size: ByteSize; alignment: Alignment): M3CG.Var := GetStructSizes_Declare;
OVERRIDES
    declare_constant := GetStructSizes_DeclareConstant;
    declare_global := GetStructSizes_DeclareGlobal;
    declare_local := GetStructSizes_DeclareLocalOrParam;
    declare_param := GetStructSizes_DeclareLocalOrParam;
    declare_temp := GetStructSizes_DeclareTemp;
    import_global := GetStructSizes_ImportGlobal;
END;

PROCEDURE GetStructSizes(self: Multipass_t) =
CONST Ops = ARRAY OF M3CG_Binary.Op{
    (* These are all the operations that can introduce a struct size. *)
        M3CG_Binary.Op.declare_constant,
        M3CG_Binary.Op.declare_global,
        M3CG_Binary.Op.declare_local,
        M3CG_Binary.Op.declare_param,
        M3CG_Binary.Op.declare_temp,
        M3CG_Binary.Op.import_global};
CONST units = ARRAY OF INTEGER{8,4,2,1};
VAR size := 0;
    count := 0;
    prev := 0;
    getStructSizes := NEW(GetStructSizes_t);
    sizes: REF ARRAY OF INTEGER := NIL;
BEGIN
    (* count up how many ops we are going to walk *)

    FOR i := FIRST(Ops) TO LAST(Ops) DO
        INC(count, NUMBER(self.op_data[Ops[i]]^));
    END;
    
    (* make worst case array -- if all the ops declare a struct *)
    
    sizes := NEW(REF ARRAY OF INTEGER, count);
    
    (* replay the ops through this little pass *)
    
    getStructSizes.sizes := sizes;
    FOR i := FIRST(Ops) TO LAST(Ops) DO
        self.Replay(getStructSizes, self.op_data[Ops[i]]);
    END;

    (* sort, unique, output *)
    
    IntArraySort.Sort(SUBARRAY(sizes^, 0, getStructSizes.count));
    prev := -1;
    FOR i := 0 TO getStructSizes.count - 1 DO
        size := sizes[i];
        <* ASSERT (size > 0) AND (size >= prev) *>
        IF (size > 0) AND (size # prev) THEN
            prev := size;
            FOR unit := FIRST(units) TO LAST(units) DO
                IF (size MOD units[unit]) = 0 THEN
                    print(self.self, "M3STRUCT" & IntToDec(units[unit]) & "(" & IntToDec(size) & ")\n");
                    EXIT;
                END;
            END;
        END;
    END;

END GetStructSizes;

PROCEDURE GetStructSizes_Declare(self: GetStructSizes_t; type: Type; byte_size: ByteSize; alignment: Alignment): M3CG.Var =
BEGIN
    <* ASSERT byte_size >= 0 *>
    <* ASSERT (byte_size MOD alignment) = 0 *>
    byte_size := MAX(byte_size, 1);
    IF type = Type.Struct THEN
        self.sizes[self.count] := byte_size;
        INC(self.count);
    END;
    RETURN NIL;
END GetStructSizes_Declare;

PROCEDURE GetStructSizes_DeclareTemp(
    self: GetStructSizes_t;
    byte_size: ByteSize;
    alignment: Alignment;
    type: Type;
    <*UNUSED*>in_memory:BOOLEAN): M3CG.Var =
BEGIN
    RETURN self.Declare(type, byte_size, alignment);
END GetStructSizes_DeclareTemp;

PROCEDURE GetStructSizes_DeclareGlobal(
    self: GetStructSizes_t;
    <*UNUSED*>name: Name;
    byte_size: ByteSize;
    alignment: Alignment;
    type: Type;
    <*UNUSED*>typeid: TypeUID;
    <*UNUSED*>exported: BOOLEAN;
    <*UNUSED*>inited: BOOLEAN): M3CG.Var =
BEGIN
    RETURN self.Declare(type, byte_size, alignment);
END GetStructSizes_DeclareGlobal;

PROCEDURE GetStructSizes_ImportGlobal(
    self: GetStructSizes_t;
    <*UNUSED*>name: Name;
    byte_size: ByteSize;
    alignment: Alignment;
    type: Type;
    <*UNUSED*>typeid: TypeUID): M3CG.Var =
BEGIN
    RETURN self.Declare(type, byte_size, alignment);
END GetStructSizes_ImportGlobal;

PROCEDURE GetStructSizes_DeclareConstant(
    self: GetStructSizes_t;
    <*UNUSED*>name: Name;
    byte_size: ByteSize;
    alignment: Alignment;
    type: Type;
    <*UNUSED*>typeid: TypeUID;
    <*UNUSED*>exported: BOOLEAN;
    <*UNUSED*>inited: BOOLEAN): M3CG.Var =
BEGIN
    RETURN self.Declare(type, byte_size, alignment);
END GetStructSizes_DeclareConstant;

PROCEDURE GetStructSizes_DeclareLocalOrParam(
    self: GetStructSizes_t;
    <*UNUSED*>name: Name;
    byte_size: ByteSize;
    alignment: Alignment;
    type: Type;
    <*UNUSED*>typeid: TypeUID;
    <*UNUSED*>in_memory: BOOLEAN;
    <*UNUSED*>up_level: BOOLEAN;
    <*UNUSED*>frequency: Frequency): M3CG.Var =
BEGIN
    RETURN self.Declare(type, byte_size, alignment);
END GetStructSizes_DeclareLocalOrParam;

PROCEDURE Struct(size: INTEGER): TEXT =
BEGIN
    <* ASSERT size >= 0 *>
    size := MAX(size, 1);
    RETURN "M3STRUCT(" & IntToDec(size) & ")";
END Struct;

PROCEDURE Var_DeclareAndInitStructParamLocalValue(var: Var_t): TEXT =
VAR static_link := "";
BEGIN
    IF var.type # Type.Struct THEN
        RETURN NIL;
    END;
    (* TODO clean this up.. *)
    static_link := follow_static_link(var.self, var);
    IF Text.Length(static_link) = 0 THEN
        RETURN Struct(var.byte_size) & " " & M3ID.ToText(var.name) & "=*_param_struct_pointer_" & M3ID.ToText(var.name) & ";";
    ELSE
        RETURN static_link & M3ID.ToText(var.name) & "=*_param_struct_pointer_" & M3ID.ToText(var.name) & ";";
    END;
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
    IF var.type # Type.Struct THEN
        RETURN M3ID.ToText(var.name);
    END;
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

PROCEDURE Var_InFrameDeclare(var: Var_t): TEXT =
VAR static := ARRAY BOOLEAN OF TEXT{"", "static "}[var.global AND NOT var.exported];
BEGIN
    RETURN static & var.InFrameType() & " " & var.InFrameName();
END Var_InFrameDeclare;

PROCEDURE declare_local(
    self: T;
    name: Name;
    byte_size: ByteSize;
    <*UNUSED*>alignment: Alignment;
    type: Type;
    <*UNUSED*>typeid: TypeUID;
    in_memory: BOOLEAN;
    up_level: BOOLEAN;
    <*UNUSED*>frequency: Frequency): M3CG.Var =
VAR var := NEW(Var_t, self := self, type := type, name := name, up_level := up_level,
               in_memory := in_memory, byte_size := byte_size, proc := self.current_proc).Init();
BEGIN
    self.comment("declare_local");
    (* self.comment("declare_local " & M3ID.ToText(var.name)); *)
    IF up_level THEN
        self.current_proc.uplevels := TRUE;
    END;
    self.current_proc.locals.addhi(var);
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

PROCEDURE last_param(self: T) =
VAR proc := self.param_proc;
    prototype: TEXT;
    param: Var_t;
BEGIN
    IF proc.add_static_link THEN
        param := NARROW(internal_declare_param(
            self,
            self.static_link_id,
            CG_Bytes[Type.Addr], (* size *)
            CG_Bytes[Type.Addr], (* alignment *)
            Type.Addr,
            UID_ADDR,
            FALSE(*?*), (* in memory *)
            TRUE, (* up_level, sort of -- needs to be stored, but is never written, can be read from direct parameter
                     This gets it stored in begin_function. *)
            M3CG.Never,
            NARROW(proc.parent, Proc_t).FrameType() & "*"), Var_t);
    END;

    prototype := function_prototype(self.param_proc, FunctionPrototype_t.Declare) & ";";
    <* ASSERT NOT self.in_proc *>
    print(self, prototype);
    self.param_proc := NIL;
END last_param;

PROCEDURE internal_declare_param(
    self: T;
    name: Name;
    byte_size: ByteSize;
    <*UNUSED*>alignment: Alignment;
    type: Type;
    <*UNUSED*>typeid: TypeUID;
    in_memory: BOOLEAN;
    up_level: BOOLEAN;
    <*UNUSED*>frequency: Frequency;
    type_text: TEXT): M3CG.Var =
VAR function := self.param_proc;
    var := NEW(Param_t, self := self, type := type, name := name, byte_size := byte_size,
               in_memory := in_memory, up_level := up_level, proc := function, type_text := type_text).Init();
BEGIN
    self.comment("declare_param");
    (* self.comment("declare_param " & M3ID.ToText(var.name)); *)
    function.params[self.param_count] := var;
    function.uplevels := function.uplevels OR up_level;
    SuppressLineDirective(self, -1, "declare_param");
    INC(self.param_count);
    IF name # self.static_link_id AND self.param_count = function.n_params_without_static_link THEN
        last_param(self);
    END;
    RETURN var;
END internal_declare_param;

PROCEDURE declare_param(self: T; name: Name; byte_size: ByteSize; alignment: Alignment;
                        type: Type; typeid: TypeUID; in_memory, up_level: BOOLEAN;
                        frequency: Frequency): M3CG.Var =
BEGIN
    IF self.param_proc = NIL THEN
        RETURN NIL;
    END;
    RETURN internal_declare_param(self, name, byte_size, alignment, type, typeid, in_memory, up_level, frequency, NIL);
END declare_param;

PROCEDURE declare_temp(self: T; byte_size: ByteSize; alignment: Alignment; type: Type; in_memory:BOOLEAN): M3CG.Var =
BEGIN
    self.comment("declare_temp");
    RETURN declare_local(self, 0, byte_size, alignment, type, -1, in_memory, FALSE, M3CG.Always);
END declare_temp;

PROCEDURE Locals_DeclareTemp(self: Locals_t; byte_size: ByteSize; alignment: Alignment; type: Type; in_memory:BOOLEAN): M3CG.Var =
BEGIN
    RETURN declare_temp(self.self, byte_size, alignment, type, in_memory);
END Locals_DeclareTemp;

PROCEDURE free_temp(self: T; <*NOWARN*>v: M3CG.Var) =
BEGIN
    self.comment("free_temp");
END free_temp;

(*---------------------------------------- static variable initialization ---*)

PROCEDURE begin_init(self: T; <*UNUSED*>v: M3CG.Var) =
BEGIN
    self.comment("begin_init");
    self.current_init_offset := 0;
    SuppressLineDirective(self, 1, "begin_init");
END begin_init;

PROCEDURE end_init(self: T; v: M3CG.Var) =
VAR var := NARROW(v, Var_t);
    init_fields := self.init_fields;
    initializer := self.initializer;
    var_name := M3ID.ToText(var.name);
    comma := "";
    const := ConstText[var.const];
BEGIN
    self.comment("end_init");
    init_to_offset(self, var.byte_size);
    end_init_helper(self);

    print(self, "struct " & var_name & "_t{");
    WHILE init_fields.size() > 0 DO
        print(self, init_fields.remlo());
    END;
    print(self, "};");

    print(self, "static " & const & var_name & "_t " & var_name & "={");
    WHILE initializer.size() > 0 DO
        print(self, comma & initializer.remlo());
        comma := ",";
    END;
    print(self, "};");
    SuppressLineDirective(self, -1, "end_init");
END end_init;

PROCEDURE init_to_offset(self: T; offset: ByteOffset) =
VAR pad := offset - self.current_init_offset;
    init_fields := self.init_fields;
    initializer := self.initializer;
    comment := "";
BEGIN
    (* print(self, " /* init_to_offset offset=" & IntToDec(offset) & " */ "); *)
    <* ASSERT offset >= self.current_init_offset *>
    <* ASSERT pad >= 0 *>
    <* ASSERT self.current_init_offset >= 0 *>
    IF pad > 0 THEN
        end_init_helper(self);
        init_fields.addhi(comment & "char " & M3ID.ToText(GenerateName(self)) & "[" & IntToDec(pad) & "];");
        initializer.addhi(comment & "{0}");
    END;
END init_to_offset;

PROCEDURE end_init_helper(self: T) =
BEGIN
    IF self.init_type_count > 0 THEN
        self.init_fields.addhi("[" & IntToDec(self.init_type_count) & "];");
    END;
    self.init_type_count := 0;
END end_init_helper;

PROCEDURE init_helper(self: T; offset: ByteOffset; type: Type; comment: TEXT := "") =
BEGIN
    init_to_offset(self, offset);
    IF offset = 0 OR self.init_type # type OR offset # self.current_init_offset THEN
        end_init_helper(self);
        self.init_fields.addhi(typeToText[type] & " " & M3ID.ToText(GenerateName(self)) & comment);
    END;
    INC(self.init_type_count);
    self.init_type := type;
    self.current_init_offset := offset + TargetMap.CG_Bytes[type];
END init_helper;

PROCEDURE init_int(self: T; offset: ByteOffset; READONLY value: Target.Int; type: Type) =
VAR comment := "";
BEGIN
    self.comment("init_int");
    init_helper(self, offset, type, comment);
    (* TIntLiteral includes suffixes like T, ULL, UI64, etc. *)
    self.initializer.addhi(TIntLiteral(type, value));
END init_int;

PROCEDURE init_proc(self: T; offset: ByteOffset; p: M3CG.Proc) =
VAR proc := NARROW(p, Proc_t);
    comment := "";
BEGIN
    self.comment("init_proc");
    init_helper(self, offset, Type.Addr, comment); (* FUTURE: better typing *)
    self.initializer.addhi(comment & "(ADDRESS)&" & M3ID.ToText(proc.name));
END init_proc;

<*NOWARN*>PROCEDURE init_label(self: T; offset: ByteOffset; value: Label) =
BEGIN
    self.comment("init_label");
    <* ASSERT FALSE *>
END init_label;

PROCEDURE init_var(self: T; offset: ByteOffset; v: M3CG.Var; bias: ByteOffset) =
VAR var := NARROW(v, Var_t);
    comment := "";
BEGIN
    self.comment("init_var");
    init_helper(self, offset, Type.Addr, comment); (* FUTURE: better typing *)
    IF bias # 0 THEN
        self.initializer.addhi(comment & IntToDec(bias) & "+"& "(ADDRESS)&" & M3ID.ToText(var.name));
    ELSE
        self.initializer.addhi(comment & "(ADDRESS)&" & M3ID.ToText(var.name));
    END;
END init_var;

<*NOWARN*>PROCEDURE init_offset(self: T; offset: ByteOffset; value: M3CG.Var) =
BEGIN
    self.comment("init_offset");
    <* ASSERT FALSE *>
END init_offset;

CONST Printable = ASCII.AlphaNumerics
        + ASCII.Set{' ','!','@','#','$','%','^','&','*','(',')','-','_','='}
        + ASCII.Set{'+','[','{',']','}','|',';',':','"',',','<','.','>','/'}
        + ASCII.Set{'?','`','~' };
PROCEDURE init_chars(self: T; offset: ByteOffset; value: TEXT) =
VAR length := Text.Length(value);
ch: CHAR;
BEGIN
    self.comment("init_chars");
    IF length = 0 THEN
        RETURN;
    END;
    FOR i := 0 TO length - 1 DO
        init_helper(self, offset + i, Type.Word8);
        ch := Text.GetChar(value, i);
        IF ch IN Printable THEN
            self.initializer.addhi("'" & Text.Sub(value, i, 1) & "'");
        ELSE
            self.initializer.addhi(IntToDec(ORD(ch)));
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

PROCEDURE IntLiteral(self: T; type: Type; i: INTEGER): TEXT =
BEGIN
    RETURN TIntLiteral(type, IntToTarget(self, i));
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

PROCEDURE init_float(self: T; offset: ByteOffset; READONLY float: Target.Float) =
BEGIN
    self.comment("init_float");
    init_helper(self, offset, TargetMap.Float_types[TFloat.Prec(float)].cg_type);
    self.initializer.addhi(FloatLiteral(float));
END init_float;

(*------------------------------------------------------------ PROCEDUREs ---*)

PROCEDURE import_procedure(self: T; name: Name; n_params: INTEGER;
                           return_type: Type; callingConvention: CallingConvention): M3CG.Proc =
VAR proc := NEW(Proc_t, name := name, n_params := n_params,
                return_type := return_type, imported := TRUE,
                callingConvention := callingConvention).Init(self);
BEGIN
    self.comment("import_procedure");
    SuppressLineDirective(self, n_params, "import_procedure n_params");
    self.param_proc := proc;
    self.param_count := 0;
    IF n_params = 0 THEN
        last_param(self);
    END;
    RETURN proc;
END import_procedure;

PROCEDURE Locals_DeclareProcedure(
    self: Locals_t;
    name: Name;
    n_params: INTEGER;
    return_type: Type;
    level: INTEGER;
    callingConvention: CallingConvention;
    exported: BOOLEAN;
    parent: M3CG.Proc): M3CG.Proc =
BEGIN
    RETURN declare_procedure(
        self.self,
        name,
        n_params,
        return_type,
        level,
        callingConvention,
        exported,
        parent);
END Locals_DeclareProcedure;

PROCEDURE declare_procedure(self: T; name: Name; n_params: INTEGER;
                            return_type: Type; level: INTEGER;
                            callingConvention: CallingConvention;
                            exported: BOOLEAN; parent: M3CG.Proc): M3CG.Proc =
VAR proc := NEW(Proc_t, name := name, n_params := n_params,
                return_type := return_type, level := level,
                callingConvention := callingConvention, exported := exported,
                parent := parent).Init(self);
BEGIN
    self.comment("declare_procedure");
    self.param_proc := proc;
    self.param_count := 0;
    IF NOT self.in_proc THEN
        self.current_proc := proc;
    END;
    IF n_params = 0 THEN
        last_param(self);
    END;
    SuppressLineDirective(self, n_params, "declare_procedure n_params");
    RETURN proc;
END declare_procedure;

PROCEDURE Locals_BeginProcedure(self: Locals_t; p: M3CG.Proc) =
VAR proc := NARROW(p, Proc_t);
BEGIN
    <* ASSERT NOT self.self.in_proc *>
    self.self.in_proc := TRUE;
    self.self.current_proc := proc;
    self.begin_block();
END Locals_BeginProcedure;

PROCEDURE Locals_EndProcedure(self: Locals_t; <*UNUSED*>p: M3CG.Proc) =
BEGIN
    self.end_block();
    self.self.in_proc := FALSE;
    self.self.current_proc := NIL;
END Locals_EndProcedure;

PROCEDURE Locals_BeginBlock(self: Locals_t) =
VAR proc := self.self.current_proc;
    (*block := NEW(Block_t, parent_block := proc.current_block);*)
    block: Block_t;
BEGIN
    proc.blocks.addhi(block);
    proc.block_stack.addhi(block);
    proc.current_block := block;
END Locals_BeginBlock;

PROCEDURE Locals_EndBlock(self: Locals_t) =
VAR proc := self.self.current_proc;
BEGIN
    EVAL proc.block_stack.remhi();
    IF proc.block_stack.size() > 0 THEN
        proc.current_block := NARROW(proc.block_stack.gethi(), Block_t);
    ELSE
        proc.current_block := NIL;
    END;
END Locals_EndBlock;

PROCEDURE begin_procedure(self: T; p: M3CG.Proc) =
VAR proc := NARROW(p, Proc_t);
    frame_name := proc.FrameName();
    frame_type := proc.FrameType();
    param_name: TEXT := NIL;
    var_name: TEXT := NIL;
    var: Var_t := NIL;
    params := proc.params;
BEGIN
    self.comment("begin_procedure");
    (* self.comment("begin_procedure " & M3ID.ToText(proc.name)); *)

    <* ASSERT NOT self.in_proc *>
    self.in_proc := TRUE;
    self.current_proc := proc;

    (* declare frame type *)

    IF proc.forward_declared_frame_type THEN
        print(self, "struct " & frame_type & " {");

        (* add field to ensure frame not empty *)
        (* TODO only do this if necessary *)

        print(self, "void* _unused;");

        (* uplevel locals in frame *)

        FOR i := 0 TO proc.Locals_Size() - 1 DO
            var := proc.Locals(i);
            IF var.up_level THEN
                print(self, var.InFrameDeclare() & ";");
            END;
        END;

        (* uplevel params in frame *) (* structs? *)

        FOR i := FIRST(params^) TO LAST(params^) DO
            var := params[i];
            IF var.up_level THEN
                print(self, var.InFrameDeclare() & ";");
            END;
        END;
        print(self, "};");
    END;

    print(self, function_prototype(proc, FunctionPrototype_t.Define));
    print(self, "{");

    (* declare and initialize non-uplevel locals *)

    FOR i := 0 TO proc.Locals_Size() - 1 DO
        WITH L = proc.Locals(i) DO
            IF NOT L.up_level THEN
                print(self, L.Declare() & "={0};");
            END;
        END;
    END;

    (* declare and initialize frame of uplevels *)

    IF proc.forward_declared_frame_type THEN
        print(self, frame_type & " " & frame_name & "={0};");

        (* capture uplevel parameters and static_link *)

        FOR i := FIRST(params^) TO LAST(params^) DO
            WITH param = params[i] DO
                IF param.up_level THEN
                    param_name := Param_Name(param);
                    var_name := Var_Name(param);
                    IF param.type # Type.Struct THEN
                        print(self, frame_name & "." & var_name & "=" & param_name & ";");
                    ELSE
                        print(self, frame_name & "." & var_name & "=*" & param_name & ";");
                    END;
                END;
            END;
        END;

        (* quash unused warning *)

        print(self, frame_name & "._unused=&" & frame_name & ";");

    END;

    (* copy structs from pointers to local value *) (* uplevels? *)

    FOR i := FIRST(params^) TO LAST(params^) DO
        IF NOT params[i].up_level THEN
            print(self, params[i].DeclareAndInitStructParamLocalValue());
        END;
    END;

END begin_procedure;

PROCEDURE end_procedure(self: T; <*UNUSED*>p: M3CG.Proc) =
(*VAR proc := NARROW(p, Proc_t);*)
BEGIN
    self.comment("end_procedure");
    (*self.comment("end_procedure " & M3ID.ToText(proc.name));*)
    self.in_proc := FALSE;
    self.current_proc := NIL;
    print(self, "}");
END end_procedure;

PROCEDURE begin_block(self: T) =
(* marks the beginning of a nested anonymous block *)
BEGIN
    self.comment("begin_block");
(* pending import_procedure all moved up to global scope
    print(self, "{");
*)
END begin_block;

PROCEDURE end_block(self: T) =
(* marks the ending of a nested anonymous block *)
BEGIN
    self.comment("end_block");
(* pending import_procedure all moved up to global scope
    print(self, "}");
*)
END end_block;

PROCEDURE note_procedure_origin(self: T; <*UNUSED*>p: M3CG.Proc) =
BEGIN
    self.comment("note_procedure_origin");
END note_procedure_origin;

(*------------------------------------------------------------ statements ---*)

PROCEDURE set_label(self: T; label: Label; <*UNUSED*>barrier: BOOLEAN) =
(* define 'label' to be at the current pc *)
BEGIN
    self.comment("set_label");
    (* semicolon in case we fall off the function here:
       void F() { L: } is not legal but
       void F() { L:; } is, and means what you'd think the first means *)
    print(self, "L" & LabelToHex(label) & ":;");
END set_label;

PROCEDURE jump(self: T; label: Label) =
(* GOTO label *)
BEGIN
    self.comment("jump");
    print(self, "goto L" & LabelToHex(label) & ";");
END jump;

PROCEDURE if_true(self: T; itype: IType; label: Label; <*UNUSED*>frequency: Frequency) =
(* IF (s0.itype # 0) GOTO label; pop *)
VAR s0 := cast(get(self, 0), itype);
BEGIN
    self.comment("if_true");
    print(self, "if(" & s0 & ")goto L" & LabelToText(label) & ";");
    pop(self);
END if_true;

PROCEDURE if_false(self: T; itype: IType; label: Label; <*UNUSED*>frequency: Frequency) =
(* IF (s0.itype = 0) GOTO label; pop *)
VAR s0 := cast(get(self, 0), itype);
BEGIN
    self.comment("if_false");
    print(self, "if(!" & s0 & ")goto L" & LabelToText(label) & ";");
    pop(self);
END if_false;

PROCEDURE if_compare(self: T; ztype: ZType; op: CompareOp; label: Label;
                     <*UNUSED*>frequency: Frequency) =
(* IF (s1.ztype op s0.ztype) GOTO label; pop(2) *)
VAR s0 := cast(get(self, 0), ztype);
    s1 := cast(get(self, 1), ztype);
BEGIN
    self.comment("if_compare");
    pop(self, 2);
    print(self, "if(" & s1 & CompareOpC[op] & s0 & ")goto L" & LabelToText(label) & ";");
END if_compare;

PROCEDURE case_jump(self: T; itype: IType; READONLY labels: ARRAY OF Label) =
(* "GOTO labels[s0.itype]; pop" with no range checking on s0.itype *)
VAR s0 := cast(get(self, 0), itype);
BEGIN
    self.comment("case_jump");
    print(self, "switch(" & s0 & "){");
    FOR i := FIRST(labels) TO LAST(labels) DO
        print(self, "case " & IntToDec(i) & ":goto L" & LabelToText(labels[i]) & ";");
    END;
    print(self, "}");
    pop(self);
END case_jump;

PROCEDURE exit_proc(self: T; type: Type) =
(* Returns s0.type if type is not Void, otherwise returns no value. *)
VAR s0: TEXT;
BEGIN
    self.comment("exit_proc");
    IF type = Type.Void THEN
        print(self, "return;");
    ELSE
        s0 := get(self);
        IF type = Type.Addr THEN
            s0 := "(ADDRESS)" & s0;
        END;
        print(self, "return " & s0 & ";");
        pop(self);
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

PROCEDURE load_helper(self: T; in: TEXT; in_offset: INTEGER; in_mtype: MType; out_ztype: ZType) =
VAR text: TEXT := NIL;
BEGIN
    <* ASSERT CG_Bytes[out_ztype] >= CG_Bytes[in_mtype] *>
    text := "*(" & typeToText[in_mtype] & "*)" & address_plus_offset(in, in_offset);
    IF in_mtype # out_ztype THEN
        text := "((" & typeToText[out_ztype] & ")(" & text & "))";
    END;
    push(self, out_ztype, text);
END load_helper;

PROCEDURE follow_static_link(self: T; var: Var_t): TEXT =
VAR current_level := self.current_proc.level;
    var_proc := var.proc;
    var_level := 0;
    static_link := "";
BEGIN
    IF var_proc = NIL OR var.up_level = FALSE OR var.is_static_link THEN
        RETURN  "";
    END;
    var_level := var_proc.level;
    IF current_level = var_level THEN
        RETURN var_proc.FrameName() & ".";
    END;
    (* You cannot access the variables of a nested function, only a containing function. *)
    <* ASSERT var_level <= current_level *>
    FOR i := var_level + 1 TO current_level DO
        static_link := static_link & "_static_link->";
    END;
    RETURN static_link;
END follow_static_link;

PROCEDURE load(self: T; v: M3CG.Var; offset: ByteOffset; mtype: MType; ztype: ZType) =
(* push; s0.ztype := Mem [ ADR(var) + offset ].mtype; The only allowed (mtype->ztype) conversions
   are {Int,Word}{8,16} -> {Int,Word}{32,64} and {Int,Word}32 -> {Int,Word}64.
   The source type, mtype, determines whether the value is sign-extended or
   zero-extended. *)
VAR var := NARROW(v, Var_t);
BEGIN
    self.comment("load");
    load_helper(self, "&" & follow_static_link(self, var) & M3ID.ToText(var.name), offset, mtype, ztype);
END load;

PROCEDURE store_helper(self: T; in: TEXT; in_ztype: ZType; out_address: TEXT; out_offset: INTEGER; out_mtype: MType) =
BEGIN
    <* ASSERT CG_Bytes[in_ztype] >= CG_Bytes[out_mtype] *>
    print(self, "(*(" & typeToText[out_mtype] & "*)" & address_plus_offset(out_address, out_offset) & ")=(" & typeToText[in_ztype] & ")(" & in & ");");
END store_helper;

PROCEDURE store(self: T; v: M3CG.Var; offset: ByteOffset; ztype: ZType; mtype: MType) =
(* Mem [ ADR(var) + offset ].mtype := s0.ztype; pop *)
VAR var := NARROW(v, Var_t);
    s0 := cast(get(self, 0), ztype);
BEGIN
    self.comment("store");
    pop(self);
    store_helper(self, s0, ztype, "&" & follow_static_link(self, var) & M3ID.ToText(var.name), offset, mtype);
  END store;

PROCEDURE load_address(self: T; v: M3CG.Var; offset: ByteOffset) =
(* push; s0.A := ADR(var) + offset *)
VAR var := NARROW(v, Var_t);
BEGIN
    self.comment("load_address");
    push(self, Type.Addr, address_plus_offset("&" & follow_static_link(self, var) & M3ID.ToText (var.name), offset));
END load_address;

PROCEDURE load_indirect(self: T; offset: ByteOffset; mtype: MType; ztype: ZType) =
(* s0.ztype := Mem [s0.A + offset].mtype  *)
VAR s0 := get(self);
BEGIN
    self.comment("load_indirect");
    <* ASSERT CG_Bytes[ztype] >= CG_Bytes[mtype] *>
    pop(self);
    load_helper(self, s0, offset, mtype, ztype);
END load_indirect;

PROCEDURE store_indirect(self: T; offset: ByteOffset; ztype: ZType; mtype: MType) =
(* Mem [s1.A + offset].mtype := s0.ztype; pop (2) *)
VAR s0 := cast(get(self, 0), ztype);
    s1 := get(self, 1);
BEGIN
    self.comment("store_indirect");
    pop(self, 2);
    store_helper(self, s0, ztype, s1, offset, mtype);
END store_indirect;

(*-------------------------------------------------------------- literals ---*)

PROCEDURE load_nil(self: T) =
(* push; s0.A := NIL *)
BEGIN
    self.comment("load_nil");
    push(self, Type.Addr, "0"); (* UNDONE NULL or (ADDRESS)0? *)
END load_nil;

PROCEDURE IntToTarget(self: T; i: INTEGER): Target.Int =
VAR j: Target.Int;
BEGIN
    IF NOT TInt.FromInt(i, j) THEN
        self.Err ("failed to convert host integer to target integer");
    END;
    RETURN j;
END IntToTarget;

PROCEDURE load_host_integer(self: T; type: IType; i: INTEGER) =
BEGIN
    comment(self, "load_host_integer");
    load_target_integer(self, type, IntToTarget(self, i));
END load_host_integer;

PROCEDURE load_target_integer(self: T; type: IType; READONLY i: Target.Int) =
(* push; s0.type := i *)
BEGIN
    self.comment("load_integer");
    (* TIntLiteral includes suffixes like T, ULL, UI64, etc. *)
    push(self, type, cast(TIntLiteral(type, i), type));
END load_target_integer;

PROCEDURE load_float(self: T; type: RType; READONLY float: Target.Float) =
(* push; s0.type := float *)
BEGIN
    self.comment("load_float");
    (* FloatLiteral includes suffixes like "F" for float, "" for double, "L" for long double *)
    push(self, type, cast(FloatLiteral(float), type));
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

PROCEDURE op1(self: T; type: Type; name, op: TEXT) =
(* unary operation *)
VAR s0 := cast(get(self, 0), type);
BEGIN
    self.comment(name);
    pop(self, 1);
    push(self, type, cast(op & s0, type));
END op1;

PROCEDURE op2(self: T; type: Type; name, op: TEXT) =
(* binary operation *)
VAR s0 := cast(get(self, 0), type);
    s1 := cast(get(self, 1), type);
BEGIN
    self.comment(name);
    pop(self, 2);
    push(self, type, cast(s1 & op & s0, type));
END op2;

PROCEDURE compare(self: T; ztype: ZType; itype: IType; op: CompareOp) =
(* s1.itype := (s1.ztype op s0.ztype); pop *)
VAR s0 := cast(get(self, 0), ztype);
    s1 := cast(get(self, 1), ztype);
BEGIN
    self.comment("compare");
    (* ASSERT cond # Cond.Z AND cond # Cond.NZ *)
    pop(self, 2);
    push(self, itype, cast(s1 & CompareOpC[op] & s0, itype));
END compare;

PROCEDURE add(self: T; type: AType) =
(* s1.type := s1.type + s0.type; pop *)
BEGIN
    op2(self, type, "add", "+");
END add;

PROCEDURE subtract(self: T; type: AType) =
(* s1.type := s1.type - s0.type; pop *)
BEGIN
    op2(self, type, "subtract", "-");
END subtract;

PROCEDURE multiply(self: T; type: AType) =
(* s1.type := s1.type * s0.type; pop *)
BEGIN
    op2(self, type, "multiply", "*");
END multiply;

PROCEDURE divide(self: T; type: RType) =
(* s1.type := s1.type / s0.type; pop *)
BEGIN
    op2(self, type, "divide", "/");
END divide;

PROCEDURE div(self: T; type: IType; <*UNUSED*>a, b: Sign) =
(* s1.type := s1.type DIV s0.type; pop *)
VAR s0 := cast(get(self, 0), type);
    s1 := cast(get(self, 1), type);
BEGIN
    self.comment("div");
    pop(self, 2);
    IF typeIsUnsigned[type] THEN
        push(self, type, cast(s1 & "/" & s0, type));
    ELSE
        push(self, type, cast("m3_div_" & typeToText[type] & "(" & s1 & "," & s0 & ")", type));
    END;
END div;

PROCEDURE mod(self: T; type: IType; <*UNUSED*>a, b: Sign) =
(* s1.type := s1.type MOD s0.type; pop *)
VAR s0 := cast(get(self, 0), type);
    s1 := cast(get(self, 1), type);
BEGIN
    self.comment("mod");
    pop(self, 2);
    IF typeIsUnsigned[type] THEN
        push(self, type, cast(s1 & "%" & s0, type));
    ELSE
        push(self, type, cast("m3_mod_" & typeToText[type] & "(" & s1 & "," & s0 & ")", type));
    END;
END mod;

PROCEDURE negate(self: T; type: AType) =
(* s0.type := - s0.type *)
BEGIN
    op1(self, type, "negate", "-");
END negate;

PROCEDURE abs(self: T; type: AType) =
(* s0.type := ABS (s0.type) (noop on Words) *)
VAR s0 := cast(get(self, 0), type);
BEGIN
    self.comment("abs");
    pop(self);
    push(self, type, "m3_abs_" & typeToText[type] & "(" & s0 & ")");
END abs;

PROCEDURE max(self: T; type: ZType) =
(* s1.type := MAX (s1.type, s0.type); pop *)
VAR s0 := cast(get(self, 0), type);
    s1 := cast(get(self, 1), type);
BEGIN
    self.comment("max");
    pop(self, 2);
    push(self, type, "m3_max_" & typeToText[type] & "(" & s0 & "," & s1 & ")");
END max;

PROCEDURE min(self: T; type: ZType) =
(* s1.type := MIN (s1.type, s0.type); pop *)
VAR s0 := cast(get(self, 0), type);
    s1 := cast(get(self, 1), type);
BEGIN
    self.comment("min");
    pop(self, 2);
    push(self, type, "m3_min_" & typeToText[type] & "(" & s0 & "," & s1 & ")");
END min;

PROCEDURE cvt_int(self: T; from_float_type: RType; to_integer_type: IType; op: ConvertOp) =
(* s0.itype := ROUND(s0.rtype) *)
VAR s0 := cast(get(self, 0), from_float_type);
BEGIN
    self.comment("cvt_int");
    pop(self);
    push(self, to_integer_type, cast("m3_" & ConvertOpName[op] & "(" & s0 & ")", to_integer_type));
END cvt_int;

PROCEDURE cvt_float(self: T; from_arithmetic_type: AType; to_float_type: RType) =
(* s0.rtype := ROUND(s0.atype) *)
VAR s0 := cast(get(self, 0), from_arithmetic_type);
BEGIN
    self.comment("cvt_float");
    (* UNDONE is this correct? *)
    pop(self);
    push(self, to_float_type, cast(s0, to_float_type));
END cvt_float;

(*------------------------------------------------------------------ sets ---*)

PROCEDURE set_op3(self: T; byte_size: ByteSize; op: TEXT) =
(* s2.B := s1.B op s0.B; pop(3) *)
VAR s0 := cast(get(self, 0), Type.Addr);
    s1 := cast(get(self, 1), Type.Addr);
    s2 := cast(get(self, 2), Type.Addr);
BEGIN
    self.comment(op);
    pop(self, 3);
    print(self, "m3_" & op & "(" & IntToDec(byte_size * 8) & ",(WORD_T*)" & s0 & ",(WORD_T*)" & s1 & ",(WORD_T*)" & s2 & ");");
END set_op3;

PROCEDURE set_union(self: T; byte_size: ByteSize) =
(* s2.B := s1.B + s0.B; pop(3) *)
BEGIN
    set_op3(self, byte_size, "set_union");
END set_union;

PROCEDURE set_difference(self: T; byte_size: ByteSize) =
(* s2.B := s1.B - s0.B; pop(3) *)
BEGIN
    set_op3(self, byte_size, "set_difference");
END set_difference;

PROCEDURE set_intersection(self: T; byte_size: ByteSize) =
(* s2.B := s1.B * s0.B; pop(3) *)
BEGIN
    set_op3(self, byte_size, "set_intersection");
END set_intersection;

PROCEDURE set_sym_difference(self: T; byte_size: ByteSize) =
(* s2.B := s1.B / s0.B; pop(3) *)
BEGIN
    set_op3(self, byte_size, "set_sym_difference");
END set_sym_difference;

PROCEDURE set_member(self: T; <*UNUSED*>byte_size: ByteSize; type: IType) =
(* s1.type := (s0.type IN s1.B); pop *)
VAR s0 := cast(get(self, 0), type);
    s1 := cast(get(self, 1), Type.Void, "SET");
BEGIN
    self.comment("set_member");
    pop(self, 2);
    push(self, type, cast("m3_set_member(" & s0 & "," & s1 & ")", type));
END set_member;

PROCEDURE set_compare(self: T; byte_size: ByteSize; op: CompareOp; type: IType) =
(* s1.type := (s1.B op s0.B); pop *)
VAR s0 := cast(get(self, 0), Type.Void, "SET");
    s1 := cast(get(self, 1), Type.Void, "SET");
BEGIN
    self.comment("set_compare");
    pop(self, 2);
    push(self, type, cast("m3_set_" & CompareOpName[op] & "(" & IntLiteral(self, Target.Word.cg_type, byte_size * 8) & "," & s1 & "," & s0 & ")", type));
END set_compare;

PROCEDURE set_range(self: T; <*UNUSED*>byte_size: ByteSize; type: IType) =
(* s2.A [s1.type .. s0.type] := 1's; pop(3) *)
VAR s0 := cast(get(self, 0), type);
    s1 := cast(get(self, 1), type);
    s2 := cast(get(self, 2), Type.Void, "SET");
BEGIN
    self.comment("set_range");
    pop(self, 3);
    print(self, "m3_set_range(" & s0 & "," & s1 & "," & s2 & ");");
END set_range;

PROCEDURE set_singleton(self: T; <*UNUSED*>byte_size: ByteSize; type: IType) =
(* s1.A [s0.type] := 1; pop(2) *)
VAR s0 := cast(get(self, 0), type);
    s1 := cast(get(self, 1), Type.Void, "SET");
BEGIN
    self.comment("set_singleton");
    pop(self, 2);
    print(self, "m3_set_singleton(" & s0 & "," & s1 & ");");
END set_singleton;

(*------------------------------------------------- Word.T bit operations ---*)

PROCEDURE not(self: T; type: IType) =
(* s0.type := Word.Not (s0.type) *)
BEGIN
    op1(self, type, "not", "~");
END not;

PROCEDURE and(self: T; type: IType) =
(* s1.type := Word.And (s1.type, s0.type); pop *)
BEGIN
    op2(self, type, "and", "&");
END and;

PROCEDURE or(self: T; type: IType) =
(* s1.type := Word.Or  (s1.type, s0.type); pop *)
BEGIN
    op2(self, type, "or", "|");
END or;

PROCEDURE xor(self: T; type: IType) =
(* s1.type := Word.Xor (s1.type, s0.type); pop *)
BEGIN
    op2(self, type, "xor", "^");
END xor;

PROCEDURE shift_left_or_right(self: T; type: IType; name, op: TEXT) =
VAR s0 := cast(get(self, 0), Target.Word.cg_type);
    s1 := cast(get(self, 1), type);
BEGIN
    self.comment(name);
    pop(self, 2);
    push(self, type, s1 & op & s0);
END shift_left_or_right;

PROCEDURE shift_left(self: T; type: IType) =
(* s1.type := Word.Shift  (s1.type, s0.Word); pop *)
BEGIN
    shift_left_or_right(self, type, "shift_left", "<<");
END shift_left;

PROCEDURE shift_right(self: T; type: IType) =
(* s1.type := Word.Shift  (s1.type, -s0.type); pop *)
BEGIN
    <* ASSERT typeIsUnsigned[type] *>
    shift_left_or_right(self, type, "shift_right", ">>");
END shift_right;

PROCEDURE shift_or_rotate(self: T; type: IType; which: TEXT; count_type: Type) =
VAR s0 := cast(get(self, 0), count_type);
    s1 := cast(get(self, 1), type);
BEGIN
    self.comment(which);
    pop(self, 2);
    push(self, type, "m3_" & which & "_" & typeToText[type] & "(" & s1 & "," & s0 & ")");
END shift_or_rotate;

PROCEDURE shift(self: T; type: IType) =
(* s1.type := Word.Shift  (s1.type, s0.type); pop *)
BEGIN
    shift_or_rotate(self, type, "shift", Target.Integer.cg_type);
END shift;

PROCEDURE rotate(self: T; type: IType) =
(* s1.type := Word.Rotate (s1.type, s0.type); pop *)
BEGIN
    shift_or_rotate(self, type, "rotate", Target.Integer.cg_type);
END rotate;

PROCEDURE rotate_left(self: T; type: IType) =
(* s1.type := Word.Rotate (s1.type, s0.type); pop *)
BEGIN
    shift_or_rotate(self, type, "rotate_left", Target.Word.cg_type);
END rotate_left;

PROCEDURE rotate_right(self: T; type: IType) =
(* s1.type := Word.Rotate (s1.type, -s0.type); pop *)
BEGIN
    shift_or_rotate(self, type, "rotate_right", Target.Word.cg_type);
END rotate_right;

PROCEDURE widen(self: T; <*UNUSED*>sign_extend: BOOLEAN) =
(* s0.I64 := s0.I32; IF sign_extend THEN SignExtend s0; *)
BEGIN
    self.comment("widen");
    <*ASSERT FALSE*>
END widen;

PROCEDURE chop(self: T) =
(* s0.I32 := Word.And (s0.I64, 16_FFFFFFFF); *)
BEGIN
    self.comment("chop");
    <*ASSERT FALSE*>
END chop;

PROCEDURE extract(self: T; type: IType; sign_extend: BOOLEAN) =
(* s2.type := Word.Extract(s2.type, s1.type, s0.type);
  IF sign_extend THEN SignExtend s2 END; pop(2) *)
VAR count := cast(get(self, 0), Target.Word.cg_type);
    offset := cast(get(self, 1), Target.Word.cg_type);
    value := cast(get(self, 2), type);
BEGIN
    self.comment("extract");
    <* ASSERT sign_extend = FALSE *>
    pop(self, 3);
    IF inline_extract THEN
        push(self, type, "m3_extract(" & typeToText[typeToUnsigned[type]] & "," & value & "," & offset & "," & count & ")");
    ELSE
        push(self, type, "m3_extract_" & typeToText[typeToUnsigned[type]] & "(" & value & "," & offset & "," & count & ")");
    END;
END extract;

PROCEDURE extract_n(self: T; type: IType; sign_extend: BOOLEAN; count: CARDINAL) =
(* s1.type := Word.Extract(s1.type, s0.type, count);
   IF sign_extend THEN SignExtend s1 END; pop(1) *)
BEGIN
    self.comment("extract_n");
    load_host_integer(self, Target.Word.cg_type, count);
    self.extract(type, sign_extend);
END extract_n;

PROCEDURE do_sign_extend(self: T; type: IType) =
VAR count := cast(get(self, 0), Target.Word.cg_type);
    value := cast(get(self, 1), type);
BEGIN
    pop(self, 2);
    push(self, type, "m3_sign_extend_" & typeToText[type] & "(" & value & "," & count & ")");
END do_sign_extend;

PROCEDURE extract_mn(self: T; type: IType; sign_extend: BOOLEAN; offset, count: CARDINAL) =
(* s0.type := Word.Extract(s0.type, offset, count);
    IF sign_extend THEN SignExtend s0 END; *)
BEGIN
    self.comment("extract_mn");
    load_host_integer(self, Target.Word.cg_type, offset);
    load_host_integer(self, Target.Word.cg_type, count);
    self.extract(type, FALSE);
    IF sign_extend THEN
        load_host_integer(self, Target.Word.cg_type, count);
        do_sign_extend(self, type);
    END;
END extract_mn;

PROCEDURE insert(self: T; type: IType) =
(* s3.type := Word.Insert (s3.type, s2.type, s1.type, s0.type); pop(3) *)
VAR count := cast(get(self, 0), Target.Word.cg_type);
    offset := cast(get(self, 1), Target.Word.cg_type);
    from := cast(get(self, 2), type);
    to := cast(get(self, 3), type);
BEGIN
    self.comment("insert");
    pop(self, 4);
    push(self, type, "m3_insert_" & typeToText[type] & "(" & to & "," & from & "," & offset & "," & count & ")");
END insert;

PROCEDURE insert_n(self: T; type: IType; count: CARDINAL) =
(* s2.type := Word.Insert (s2.type, s1.type, s0.type, count); pop(2) *)
BEGIN
    self.comment("insert_n");
    load_host_integer(self, Target.Word.cg_type, count);
    self.insert(type);
END insert_n;

PROCEDURE insert_mn(self: T; type: IType; offset, count: CARDINAL) =
(* s1.type := Word.Insert (s1.type, s0.type, offset, count); pop(2) *)
BEGIN
    self.comment("insert_mn");
    load_host_integer(self, Target.Word.cg_type, offset);
    load_host_integer(self, Target.Word.cg_type, count);
    self.insert(type);
END insert_mn;

(*------------------------------------------------ misc. stack/memory ops ---*)

PROCEDURE swap(self: T; <*UNUSED*>a, b: Type) =
(* tmp := s1; s1 := s0; s0 := tmp *)
VAR temp := get(self, 1);
BEGIN
    self.comment("swap");
    self.stack.put(1, get(self, 0));
    self.stack.put(0, temp);
END swap;

PROCEDURE cg_pop(self: T; type: Type) =
(* pop(1) (i.e. discard s0) *)
VAR s0 := cast(get(self, 0), type);
BEGIN
    self.comment("pop");
    pop(self);
    print(self, "m3_pop_" & typeToText[type] & "(" & s0 & ");");
END cg_pop;

CONST MemCopyOrMove = ARRAY OF TEXT{"memcpy", "memmove"};

PROCEDURE copy_n(self: T; itype: IType; mtype: MType; overlap: BOOLEAN) =
(* Mem[s2.A:s0.ztype] := Mem[s1.A:s0.ztype]; pop(3)*)
VAR s0 := cast(get(self, 0), itype);
    s1 := get(self, 1);
    s2 := get(self, 2);
BEGIN
    self.comment("copy_n");
    pop(self, 3);
    print(self, MemCopyOrMove[ORD(overlap)] & "(" & s2 & "," & s1 & "," & IntToDec(CG_Bytes[mtype]) & "*(size_t)" & s0 & ");");
END copy_n;

PROCEDURE copy(self: T; n: INTEGER; mtype: MType; overlap: BOOLEAN) =
(* Mem[s1.A:sz] := Mem[s0.A:sz]; pop(2)*)
VAR s0 := get(self, 0);
    s1 := get(self, 1);
BEGIN
    self.comment("copy");
    pop(self, 2);
    print(self, MemCopyOrMove[ORD(overlap)] & "(" & s1 & "," & s0 & "," & IntToDec(CG_Bytes[mtype] * n) & ");");
END copy;

<*NOWARN*>PROCEDURE zero_n(self: T; itype: IType; mtype: MType) =
(* Mem[s1.A:s0.itype] := 0; pop(2) *)
BEGIN
    self.comment("zero_n");

    <* ASSERT FALSE *>

    (* zero_n is implemented incorrectly in the gcc backend,
     * therefore it must not be used.
     *)
END zero_n;

PROCEDURE zero(self: T; n: INTEGER; type: MType) =
(* Mem[s0.A:sz] := 0; pop(1) *)
VAR s0 := get(self, 0);
BEGIN
    self.comment("zero");
    pop(self);
    print(self, "memset(" & s0 & ",0," & IntToDec(n) & "*(size_t)" & IntToDec(CG_Bytes[type]) & ");");
END zero;

(*----------------------------------------------------------- conversions ---*)

PROCEDURE loophole(self: T; from, to: ZType) =
(* s0.to := LOOPHOLE(s0.from, to) *)
VAR s0 := cast(cast(get(self, 0), from), to);
BEGIN
    self.comment("loophole");
    self.stack.put(0, s0);
END loophole;

(*------------------------------------------------ traps & runtime checks ---*)

PROCEDURE abort(self: T; code: RuntimeError) =
BEGIN
    self.comment("abort");
    reportfault(self, code);
END abort;

PROCEDURE check_nil(self: T; code: RuntimeError) =
(* IF (s0.A = NIL) THEN abort(code) *)
VAR s0 := get(self);
BEGIN
    self.comment("check_nil");
    print(self, "if(!" & paren(s0) & ")");
    reportfault(self, code);
END check_nil;

PROCEDURE check_lo(self: T; type: IType; READONLY i: Target.Int; code: RuntimeError) =
(* IF (s0.type < i) THEN abort(code) *)
VAR s0 := cast(get(self), type);
BEGIN
    self.comment("check_lo");
    print(self, "if(" & s0 & "<" & TIntLiteral(type, i) & ")");
    reportfault(self, code);
END check_lo;

PROCEDURE check_hi(self: T; type: IType; READONLY i: Target.Int; code: RuntimeError) =
(* IF (i < s0.type) THEN abort(code) *)
VAR s0 := cast(get(self), type);
BEGIN
    self.comment("check_hi");
    print(self, "if(" & TIntLiteral(type, i) & "<" & s0 & ")");
    reportfault(self, code);
END check_hi;

PROCEDURE check_range(self: T; type: IType; READONLY a, b: Target.Int; code: RuntimeError) =
(* IF (s0.type < a) OR (b < s0.type) THEN abort(code) *)
VAR s0 := cast(get(self), type);
    a_text := TIntLiteral(type, a);
    b_text := TIntLiteral(type, b);
BEGIN
    self.comment("check_range");
    print(self, "if((" & s0 & "<" & cast(a_text, type) & ")||(" & cast(b_text, type) & "<" & s0 & "))");
    reportfault(self, code);
END check_range;

PROCEDURE check_index(self: T; type: IType; code: RuntimeError) =
(* IF NOT (0 <= s1.type < s0.type) THEN
     abort(code)
   END;
   pop *)
(* s0.type is guaranteed to be positive so the unsigned
   check (s0.W <= s1.W) is sufficient. *)
VAR s0 := cast(get(self, 0), type);
    s1 := cast(get(self, 1), type);
BEGIN
    self.comment("check_index");
    print(self, "if(" & s0 & "<=" & s1 & ")");
    reportfault(self, code);
    pop(self);
END check_index;

PROCEDURE check_eq(self: T; type: IType; code: RuntimeError) =
(* IF (s0.type # s1.type) THEN
     abort(code);
     Pop (2) *)
VAR s0 := cast(get(self, 0), type);
    s1 := cast(get(self, 1), type);
BEGIN
    self.comment("check_eq");
    print(self, "if(" & s0 & "!=" & s1 & ")");
    reportfault(self, code);
END check_eq;

PROCEDURE reportfault(self: T; code: RuntimeError) =
(* 32: see M3CG.RuntimeError, RuntimeError.T *)
VAR info := ORD (code) + self.line * 32;
BEGIN
    <* ASSERT ORD (code) < 32 *> (* lose fault code not ok *)
    (* ASSERT self.line <= (LAST(INTEGER) DIV 32) *) (* losing line number ok *)
    print(self, self.report_fault & "(" & IntToDec(info) & ");");
END reportfault;

(*---------------------------------------------------- address arithmetic ---*)

PROCEDURE add_offset(self: T; offset: INTEGER) =
(* s0.A := s0.A + offset *)
VAR s0 := cast(get(self, 0), Type.Addr);
BEGIN
    self.comment("add_offset");
    pop(self);
    push(self, Type.Addr, address_plus_offset(s0, offset));
END add_offset;

PROCEDURE index_address(self: T; type: IType; size: INTEGER) =
(* s1.A := s1.A + s0.type * size; pop *)
VAR s0 := cast(get(self, 0), type);
    s1 := cast(get(self, 1), Type.Addr);
    size_text := IntToDec(size);
BEGIN
    self.comment("index_address");
    IF size = 0 THEN
        pop(self);
        <* ASSERT FALSE *>
    ELSE
        pop(self, 2);
        push(self, Type.Addr, paren(s1 & "+" & paren(size_text & "*" & s0)));
    END;
END index_address;

(*------------------------------------------------------- PROCEDURE calls ---*)

PROCEDURE start_call_helper(self: T) =
BEGIN
    self.static_link[self.in_proc_call] := NIL;
    <* ASSERT self.params.size() = 0 *>
    INC(self.in_proc_call);
END start_call_helper;

PROCEDURE start_call_direct(self: T; p: M3CG.Proc; <*UNUSED*>level: INTEGER; <*UNUSED*>type: Type) =
(* begin a procedure call to a procedure at static level 'level'. *)
VAR proc := NARROW(p, Proc_t);
BEGIN
    self.comment("start_call_direct");
    start_call_helper(self);
    (* workaround frontend bug? *)
    IF proc.is_exception_handler THEN
        push(self, Type.Addr, "0");
        pop_parameter_helper(self, "0");
    END;
    (* This code would pass static_link first.
       But we pass it last now, because we don't
       know when to pass it, e.g. on indirect calls,
       and we pass it when it is not expected,
       which works for putting it at the end,
       and doesn't work for at the start.
    IF level > self.current_proc.level THEN
        static_link := "(&_static_link)";
        FOR i := self.current_proc.level TO level DO
            static_link := static_link & "->_static_link";
        END;
        push(self, Type.Addr, static_link);
        pop_parameter_helper(self, static_link);
    END;
    *)
END start_call_direct;

PROCEDURE start_call_indirect(self: T; <*UNUSED*>type: Type; <*UNUSED*>callingConvention: CallingConvention) =
(* begin a procedure call to a procedure at static level 'level'. *)
BEGIN
    self.comment("start_call_indirect");
    start_call_helper(self);
END start_call_indirect;

PROCEDURE pop_parameter_helper(self: T; param: TEXT) =
BEGIN
    <* ASSERT self.in_proc_call > 0 *>
    self.params.addhi(param);
    pop(self);
END pop_parameter_helper;

PROCEDURE pop_param(self: T; type: MType) =
(* pop s0 and make it the "next" parameter in the current call *)
VAR s0 := cast(get(self, 0), type);
BEGIN
    self.comment("pop_param");
    pop_parameter_helper(self, s0);
END pop_param;

PROCEDURE pop_struct(self: T; <*UNUSED*>typeid: TypeUID; byte_size: ByteSize; alignment: Alignment) =
(* pop s0 and make it the "next" parameter in the current call
* NOTE: it is passed by value *)
VAR s0 := get(self, 0);
BEGIN
    self.comment("pop_struct");
    <* ASSERT (byte_size MOD alignment) = 0 *>
    <* ASSERT byte_size >= 0 *>
    (* pop_parameter_helper(self, "*(M3STRUCT(" & IntToDec(byte_size) & ") * )" & s0); *)
    (* pop_parameter_helper(self, s0); *)
    (* BUG? local procedure take struct pointers, imported ones take ADDRESS? *)
    pop_parameter_helper(self, "(void * )" & s0);
END pop_struct;

PROCEDURE pop_static_link(self: T) =
VAR var := self.pop_static_link_temp_vars.get(self.pop_static_link_temp_vars_index);
BEGIN
    self.comment("pop_static_link");
    INC(self.pop_static_link_temp_vars_index);
    <* ASSERT self.in_proc_call > 0 *>
    self.static_link[self.in_proc_call - 1] := var;
    self.store(var, 0, Type.Addr, Type.Addr);
END pop_static_link;

PROCEDURE Locals_PopStaticLink(self: Locals_t) =
BEGIN
    self.self.pop_static_link_temp_vars.addhi(declare_temp(self.self, CG_Bytes[Type.Addr], CG_Bytes[Type.Addr], Type.Addr, FALSE));
END Locals_PopStaticLink;

PROCEDURE call_helper(self: T; type: Type; proc: TEXT) =
VAR comma := "";
BEGIN
    <* ASSERT self.in_proc_call > 0 *>
    DEC(self.in_proc_call);
    proc := proc & "(";
    WHILE self.params.size() > 0 DO
      proc := proc & comma & self.params.remlo();
      comma := ",";
    END;
    proc := proc & ")";
    IF type = Type.Void THEN
        print(self, proc & ";");
    ELSE
        push(self, type, proc);
    END;
END call_helper;

PROCEDURE get_static_link(self: T; target: Proc_t): TEXT =
VAR target_level := target.level;
    current := self.current_proc;
    current_level := current.level;
    static_link := "";
BEGIN
    self.comment("get_static_link");
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

PROCEDURE call_direct(self: T; p: M3CG.Proc; type: Type) =
(* call the procedure identified by M3CG.Proc p. The procedure
   returns a value of type type. *)
VAR proc := NARROW(p, Proc_t);
BEGIN
    self.comment("call_direct");

    <* ASSERT self.in_proc_call > 0 *>

    IF proc.level # 0 THEN
        self.params.addhi(get_static_link(self, proc));
    END;

    call_helper(self, type, M3ID.ToText(proc.name));
END call_direct;

PROCEDURE call_indirect(self: T; type: Type; <*UNUSED*>callingConvention: CallingConvention) =
(* call the procedure whose address is in s0.A and pop s0. The
   procedure returns a value of type type. *)
VAR s0 := get(self, 0);
    static_link := self.static_link[self.in_proc_call - 1];
BEGIN
    self.comment("call_indirect");

    pop(self);

    <* ASSERT self.in_proc_call > 0 *>

    IF static_link # NIL THEN
        self.params.addhi(M3ID.ToText(static_link.name));
        free_temp(self, static_link);
        self.static_link[self.in_proc_call - 1] := NIL;
    END;

    (* UNDONE: cast to more accurate function type *)
    call_helper(self, type, "((" & typeToText[type] & " (__cdecl*)(M3_DOTDOTDOT))" & s0 & ")");

END call_indirect;

(*------------------------------------------- procedure and closure types ---*)

PROCEDURE load_procedure(self: T; p: M3CG.Proc) =
(* push; s0.A := ADDR (proc's body) *)
VAR proc := NARROW(p, Proc_t);
BEGIN
    self.comment("load_procedure");
    (* UNDONE? typeing? *)
    push(self, Type.Addr, M3ID.ToText(proc.name));
END load_procedure;

PROCEDURE load_static_link(self: T; p: M3CG.Proc) =
(* push; s0.A := (static link needed to call proc, NIL for top-level procs) *)
VAR target := NARROW(p, Proc_t);
BEGIN
    self.comment("load_static_link");
    IF target.level = 0 THEN
        self.load_nil();
        RETURN;
    END;
    push(self, Type.Addr, get_static_link(self, target));
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

PROCEDURE comment(self: T; a, b, c, d: TEXT := NIL) =
VAR length := 0;
BEGIN
    IF self.debug < 1 THEN
        RETURN;
    END;
    comment_1(a, length);
    comment_1(b, length);
    comment_1(c, length);
    comment_1(d, length);
    IF length < 1 THEN
        RETURN;
    END;
    print(self, " /* " & a & b & c & d & " */\n");
END comment;

(*--------------------------------------------------------------- atomics ---*)

PROCEDURE store_ordered(self: T; ztype: ZType; mtype: MType; <*UNUSED*>order: MemoryOrder) =
(* Mem [s1.A].mtype := s0.ztype;
   pop (2) *)
VAR s0 := get(self, 0);
    s1 := get(self, 1);
BEGIN
    self.comment("store_ordered");
    print(self, " /* store_ordered => store */ ");
    store_helper(self, s0, ztype, s1, 0, mtype);
END store_ordered;

PROCEDURE load_ordered(self: T; mtype: MType; ztype: ZType; <*UNUSED*>order: MemoryOrder) =
(* s0.ztype := Mem [s0.A].mtype  *)
VAR s0 := get(self);
BEGIN
    self.comment("load_ordered");
    print(self, " /* load_ordered */ ");
    pop(self);
    load_helper(self, s0, 0, mtype, ztype);
END load_ordered;

<*NOWARN*>PROCEDURE exchange(self: T; mtype: MType; ztype: ZType; <*UNUSED*>order: MemoryOrder) =
(* tmp := Mem [s1.A + offset].mtype;
   Mem [s1.A + offset].mtype := s0.ztype;
   s0.ztype := tmp;
   pop *)
BEGIN
    self.comment("exchange");
    print(self, " /* exchange */ ");
END exchange;

<*NOWARN*>PROCEDURE compare_exchange(self: T; mtype: MType; ztype: ZType; result_type: IType;
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
    self.comment("compare_exchange");
    print(self, " /* compare_exchange */ ");
END compare_exchange;

PROCEDURE fence(self: T; <*UNUSED*>order: MemoryOrder) =
(*
 * x86: Exchanging any memory with any register is a serializing instruction.
 *)
BEGIN
    self.comment("fence");
    print(self, " /* fence */ ");
    <* ASSERT self.in_proc *>
    <* ASSERT self.current_proc # NIL *>
    print(self, "m3_fence();");
END fence;

<*NOWARN*>CONST AtomicOpName = ARRAY AtomicOp OF TEXT { "add", "sub", "or", "and", "xor" };

<*NOWARN*>PROCEDURE fetch_and_op(self: T; atomic_op: AtomicOp; mtype: MType; ztype: ZType;
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
    self.comment("fetch_and_op");
    print(self, " /* fetch_and_op */ ");
    <* ASSERT CG_Bytes[ztype] >= CG_Bytes[mtype] *>
END fetch_and_op;

BEGIN
(*
    BitSizeToEnumCGType[8] := M3CG.Type.Word8;
    BitSizeToEnumCGType[16] := M3CG.Type.Word16;
    BitSizeToEnumCGType[32] := M3CG.Type.Word32;
*)
END M3C.
