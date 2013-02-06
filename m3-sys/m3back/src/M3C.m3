MODULE M3C;

IMPORT RefSeq, TextSeq, Wr, Text, IntRefTbl, SortedIntRefTbl, TIntN;
IMPORT M3CG, M3CG_Ops, Target, TFloat, TargetMap, IntArraySort, Process;
IMPORT M3ID, TInt, TWord, ASCII, TextUtils, Fmt, Thread, Stdio;
FROM TargetMap IMPORT CG_Bytes;
FROM M3CG IMPORT Name, ByteOffset, CallingConvention;
FROM M3CG IMPORT BitSize, ByteSize, Alignment, Frequency;
FROM M3CG IMPORT Label, Sign, BitOffset, TypeUID;
FROM M3CG IMPORT Type, ZType, AType, RType, IType, MType;
FROM M3CG IMPORT CompareOp, ConvertOp, RuntimeError, MemoryOrder, AtomicOp;
FROM Target IMPORT CGType;
FROM M3CG_Ops IMPORT ErrorHandler;
IMPORT M3CG_MultiPass, M3CG_DoNothing, M3CG_Binary, RTIO;
FROM M3CC IMPORT INT32, INT64, UINT32, UINT64, Base_t, UInt64ToText;

(* comparison is always false due to limited range of data type *)
VAR AvoidGccTypeRangeWarnings := TRUE;

VAR CaseDefaultAssertFalse := FALSE;

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

        no_return := FALSE; (* are there any no_return functions -- i.e. #include <sys/cdefs.h on Darwon for __dead2 *)

        typeidToType: IntRefTbl.T := NIL; (* FUTURE INTEGER => Type_t *)
        temp_vars: REF ARRAY OF Var_t := NIL; (* for check_* to avoid double evaluation, and pop_static_link *)
        current_block: Block_t := NIL;
    
        multipass: Multipass_t := NIL;
        Err    : ErrorHandler := DefaultErrorHandler;
        anonymousCounter := -1;
        c      : Wr.T := NIL;
        debug  := 0;
        stack  : RefSeq.T := NIL;
        params : TextSeq.T := NIL;
        op_index: INTEGER := 0;
        
        enum_type: TEXT := NIL;
        (*enum: Enum_t := NIL;*)
        enum_id: TEXT := NIL;
        enum_value: CARDINAL := 0;
        unit_name := "L_";
        handler_name_prefixes := ARRAY [FIRST(HandlerNamePieces) .. LAST(HandlerNamePieces)] OF TEXT{NIL, ..};
        param_count := 0;
        static_link_id: M3ID.T := 0;
        RTException_Raise_id: M3ID.T := 0;
        RTHooks_AssertFailed_id: M3ID.T := 0;
        RTHooks_Raise_id: M3ID.T := 0;
        RTHooks_ReportFault_id: M3ID.T := 0;
        RTHooks_ReportFault_imported_or_declared := FALSE;
        
        (* labels *)
        labels_min := FIRST(Label);
        labels_max := LAST(Label);
        labels: REF ARRAY (*Label=INTEGER*) OF BOOLEAN := NIL;
        
        (* initialization support *)
        
        init_fields: TextSeq.T := NIL;
        current_init_offset: INTEGER := 0;
        initializer: TextSeq.T := NIL;
        initializer_comma: TEXT := "";
        
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
        
        static_link     : Var_t := NIL; (* based on M3x86 *)
        current_proc    : Proc_t := NIL; (* based on M3x86 *)
        param_proc      : Proc_t := NIL; (* based on M3x86 *)
        in_proc         : BOOLEAN := FALSE; (* based on M3x86 *)
        in_proc_call    : BOOLEAN := FALSE; (* based on M3x86 *)
        proc_being_called : Proc_t := NIL;
        report_fault: TEXT := NIL; (* based on M3x86 -- reportlabel, global_var *)
        report_fault_used := FALSE;
        width := 0;

    METHODS
        Type_Init(type: Type_t): Type_t := Type_Init;

    OVERRIDES
        end_unit   := end_unit;

        set_error_handler := set_error_handler;
        begin_unit := begin_unit;
        set_source_file := set_source_file;
        set_source_line := set_source_line;
        set_runtime_proc := set_runtime_proc;
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
VAR start := ARRAY BOOLEAN OF TEXT{" /* ", (*"#"*)"//"}[output_line_directives];
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

CONST BoolToText = ARRAY BOOLEAN OF TEXT{"FALSE", "TRUE"};

CONST reservedWords = ARRAY OF TEXT{
"__cdecl", "__except", "__fastcall", "__finally",
"__int16", "__int32", "__int64", "__int8",
"__stdcall", "__try", "_cdecl", "_fastcall",
"_stdcall", "and", "and_eq", "asm",
"auto", "bitand", "bitor", "bool",
"break", "case", "catch", "char",
"class", "compl", "const", "const_cast",
"continue", "default", "delete", "do",
"double", "dynamic_cast", "else", "enum",
"explicit", "extern", "false", "float",
"for", "friend", "goto", "if",
"inline", "int", "long", "mutable",
"namespace", "new", "not", "not_eq",
"operator", "or", "or_eq", "private",
"protected", "ptrdiff_t", "public", "register",
"reinterpret_cast", "return", "short", "signed",
"size_t", "sizeof", "static", "static_cast",
"struct", "switch", "template", "this",
"throw", "true", "try", "typedef",
"typeid", "typename", "union", "unsigned",
"using", "virtual", "void", "volatile",
"wchar_t", "while", "xor", "xor_eq",
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
    min: Target.Int; (* alwways zero *)
    max: Target.Int;
END;

TYPE Subrange_t  = Type_t OBJECT
    min: Target.Int;
    max: Target.Int;
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

TYPE ExprType = {
    Invalid,
    Variable,
    LoadIndirect,
    LoadAddress,
    ConstantInt,
    ConstantFloat,
    ConstantNil,
    AddressOf,
    Cast,
    Deref,
    Compare,
    Add,
    Subtract,
    Multiply,
    FloatDivide,
    IntDiv,
    IntMod,
    Negate,
    Abs,
    Max,
    Min,
    FloatToInt,
    ToFloat,
    SetUnion,
    SetDifference,
    SetIntersection,
    SetSymDifference,
    SetMember,
    SetCompare,
    SetRange,
    SetSingletone,
    Not,
    And,
    Or,
    Xor,
    ShiftLeft,
    ShiftRight,
    Shift,
    Rotate,
    RotateLeft,
    RotateRight,
    Widen,
    Chop,
    Extract,
    Insert,
    SignExtend,
    AddOffset,
    IndexAddress,
    FieldRef,
    AsText
};

TYPE Expr_t = OBJECT
    self: T := NIL;
    expr_type := ExprType.Invalid;
    current_proc: Proc_t := NIL;
    points_to_m3cgtype: M3CG.Type := M3CG.Type.Void;
    points_to_typeid: TypeUID := 0;
    float_value: Target.Float;
    text_value: TEXT := NIL;
    (* The right generalization here is a set of values the expression
    could possibly have, represented by range lists, not just one range. *)
    minmax_valid := minMaxFalse;
    minmax := int64MinMax;
    m3cgtype: M3CG.Type := M3CG.Type.Void;
    typeid: TypeUID;
    type: Type_t := NIL;
    c_text: TEXT := NIL;
    c_unop_text: TEXT := NIL;  (* e.g. ~, -, ! *)
    c_binop_text: TEXT := NIL; (* e.g. +, -, *, / *)
    left: Expr_t := NIL;
    right: Expr_t := NIL;
    type_text: TEXT := NIL;
    METHODS
        CText(): TEXT := Expr_CText;
        Add(right: Expr_t): Expr_t (*:= expr_add*);
        Sub(right: Expr_t): Expr_t (*:= expr_sub*);
        Mult(right: Expr_t): Expr_t (*:= expr_mult*);
END;

TYPE Expr_Constant_t = Expr_t OBJECT END;
TYPE Expr_AddressOf_t = Expr_t OBJECT END;
TYPE Expr_Ordinal_t = Expr_t OBJECT END;
TYPE Expr_Deref_t = Expr_t OBJECT END;

TYPE Expr_Variable_t = Expr_t OBJECT
    var: Var_t := NIL;
    OVERRIDES
        CText := Expr_Variable_CText;
END;
PROCEDURE Expr_Variable_CText(self: Expr_Variable_t): TEXT =
VAR var := self.var;
BEGIN
    RETURN follow_static_link(self.current_proc, var) & M3ID.ToText(var.name);
END Expr_Variable_CText;

TYPE Expr_Unary_t  = Expr_t OBJECT (* x: ARRAY [0..0] OF Expr_t; *) END;
TYPE Expr_Binary_t = Expr_t OBJECT (* x: ARRAY [0..1] OF Expr_t; *) END;

TYPE Expr_ConstantInt_t = Expr_t OBJECT OVERRIDES CText := Expr_ConstantInt_CText; END;
PROCEDURE Expr_ConstantInt_CText(self: Expr_ConstantInt_t): TEXT =
BEGIN
    (* ASSERT self.minmax[Min] = self.minmax[Max] *)
    RETURN TIntLiteral(self.m3cgtype, self.minmax[Min]);
END Expr_ConstantInt_CText;

TYPE Expr_Cast_t = Expr_Unary_t OBJECT
    force := FALSE;
    OVERRIDES
        CText := Expr_Cast_CText;
END;
PROCEDURE Expr_Cast_CText(self: Expr_Cast_t): TEXT =
VAR type_text := self.type_text;
    m3cgtype := self.m3cgtype;
    force := self.force;
    left := self.left;
    left_text := left.CText();
    remove := 0;
    lparen := "";
    rparen := "";
BEGIN
    <* ASSERT (m3cgtype = M3CG.Type.Void) # (self.type_text = NIL) *>
    IF NOT force THEN
        (* We might need "force_cast":
        INT16 a, b, c = a + b;
        => a + b is "int" and needs cast to INT16
        *)
        IF type_text # NIL THEN
            IF left.type_text # NIL AND (left.type_text = type_text OR Text.Equal(left.type_text, type_text)) THEN
                remove := 1; (* I've never seen this -- so skipped below *)
            END;
        ELSE
            type_text := typeToText[m3cgtype];
            lparen := "(";
            rparen := ")";
            IF left.m3cgtype = m3cgtype AND m3cgtype # M3CG.Type.Addr THEN
                remove := 2; (* This happens fairly often. *)
            END;
        END;
    END;
    IF remove > 0 AND remove # 1 THEN
        RETURN " /* cast_removed" & Fmt.Int(remove) & ": " & type_text & " */ " & left_text;
    ELSE
        RETURN "(" & lparen & type_text & rparen & "(" & left_text & "))";
    END;
END Expr_Cast_CText;

TYPE Expr_Add_t = Expr_Binary_t OBJECT OVERRIDES CText := Expr_Add_CText; END;
PROCEDURE Expr_Add_CText(self: Expr_Add_t): TEXT = BEGIN RETURN self.left.CText() & "+" & self.right.CText(); END Expr_Add_CText;

TYPE Expr_Subtract_t = Expr_Binary_t OBJECT OVERRIDES CText := Expr_Subtract_CText; END;
PROCEDURE Expr_Subtract_CText(self: Expr_Subtract_t): TEXT = BEGIN RETURN self.left.CText() & "-" & self.right.CText(); END Expr_Subtract_CText;

TYPE Expr_Negate_t = Expr_Unary_t OBJECT OVERRIDES CText := Expr_Negate_CText; END;
PROCEDURE Expr_Negate_CText(self: Expr_Negate_t): TEXT = BEGIN RETURN "-" & self.left.CText(); END Expr_Negate_CText;

PROCEDURE CTextToExpr(c_text: TEXT): Expr_t =
BEGIN
    RETURN NEW(Expr_t, c_text := c_text);
END CTextToExpr;

PROCEDURE TextOrNil(a: TEXT): TEXT =
BEGIN
    RETURN ARRAY BOOLEAN OF TEXT{"NIL", a}[a # NIL];
END TextOrNil;

PROCEDURE Expr_Assert(self: Expr_t) =
VAR type_text := self.type_text;
    m3cgtype := self.m3cgtype;
    ok := FALSE;
BEGIN
    IF FALSE THEN
        IF NOT ((m3cgtype = M3CG.Type.Void) # (type_text = NIL)) THEN
            RTIO.PutText("m3cgtype:" & typeToText[m3cgtype] & " type_text:" & TextOrNil(type_text) & "\n");
            RTIO.Flush();
            <* ASSERT (m3cgtype = M3CG.Type.Void) # (type_text = NIL) *>
        END;
    END;
    IF self.expr_type = ExprType.Variable THEN
        ok := TRUE;
    END;
    IF self.expr_type = ExprType.Cast THEN
        ok := TRUE;
    END;
    IF self.c_text # NIL THEN
        ok := TRUE;
    END;
    IF self.c_unop_text # NIL THEN
        <* ASSERT self.left # NIL *>
        <* ASSERT self.right = NIL *>
        <* ASSERT self.c_binop_text = NIL *>
        ok := TRUE;
    END;
    IF self.c_binop_text # NIL THEN
        <* ASSERT self.left # NIL *>
        <* ASSERT self.right # NIL *>
        <* ASSERT self.c_unop_text = NIL *>
        ok := TRUE;
    END;
    (* ASSERT ok *)
END Expr_Assert;

PROCEDURE Expr_CText(self: Expr_t): TEXT =
VAR left := self.left;
    right := self.right;
    c_text := self.c_text;
    c_unop_text := self.c_unop_text;
    c_binop_text := self.c_binop_text;
BEGIN
    Expr_Assert(self);
    IF c_text # NIL THEN
        RETURN c_text;
    END;
    IF c_unop_text # NIL THEN
        RETURN c_unop_text & left.CText();
    END;
    IF c_binop_text # NIL THEN
        RETURN left.CText() & c_binop_text & right.CText();
    END;
    RETURN NIL;
END Expr_CText;

(*
PROCEDURE Expr_FromInt(i: INTEGER): Expr_t;
PROCEDURE Expr_FromTFloat(f: Target.Float): Expr_t;
PROCEDURE Expr_FromText(self: TEXT): Expr_t;

PROCEDURE Expr_add(<*UNUSED*>left, right: Expr_t): Expr_t = BEGIN RETURN NIL; END expr_add;
PROCEDURE Expr_sub(<*UNUSED*>left, right: Expr_t): Expr_t = BEGIN RETURN NIL; END expr_sub;
PROCEDURE Expr_mult(<*UNUSED*>left, right: Expr_t): Expr_t = BEGIN RETURN NIL; END expr_mult;
*)

TYPE Var_t = M3CG.Var OBJECT
    self: T := NIL;
    used := FALSE;
    name: Name := 0;
    name_in_frame: Name := 0; (* if up_level, e.g. ".block1.foo" *)
    m3cgtype: M3CG.Type;
    typeid: TypeUID;
    points_to_m3cgtype: M3CG.Type; (* future *)
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
    var.is_static_link := (var.name = var.self.static_link_id);
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
    return_type: M3CG.Type;
    level: INTEGER := 0;
    callingConvention: CallingConvention;
    exported := FALSE;
    imported := FALSE;
    parent: Proc_t := NIL;
    params: REF ARRAY OF Var_t(*Param_t*);
    locals: RefSeq.T := NIL; (* Var_t *)
    uplevels := FALSE;
    is_exception_handler := FALSE;
    is_RTHooks_Raise := FALSE;
    is_RTException_Raise := FALSE;
    no_return := FALSE;
    exit_proc_skipped := 0;
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
    print(self, "struct " & proc.FrameType() & ";\n");
    print(self, "typedef struct " & proc.FrameType() & " " & proc.FrameType() & ";\n");
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
VAR is_common := (proc.parent = NIL
                  AND (proc.exported = TRUE OR proc.imported = TRUE)
                  AND proc.level = 0
                  AND proc.return_type = M3CG.Type.Void);
    is_RTHooks_ReportFault := (is_common
                               AND proc.name = self.RTHooks_ReportFault_id
                               AND proc.n_params = 2);
    is_RTHooks_AssertFailed := (is_common
                                AND proc.name = self.RTHooks_AssertFailed_id
                                AND proc.n_params = 3);
BEGIN
    proc.is_RTHooks_Raise := (is_common
                              AND proc.name = self.RTHooks_Raise_id
                              AND proc.n_params = 4);
    proc.is_RTException_Raise := (is_common
                                  AND proc.name = self.RTException_Raise_id
                                  AND proc.n_params = 1);
    IF is_RTHooks_ReportFault THEN
        self.RTHooks_ReportFault_imported_or_declared := TRUE;
    END;
    proc.no_return := is_RTHooks_AssertFailed OR is_RTHooks_ReportFault OR proc.is_RTException_Raise OR proc.is_RTHooks_Raise;
    IF proc.no_return THEN
        no_return(self);
    END;
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
    proc.ForwardDeclareFrameType(); (* TODO do not always do this *)
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
"#ifdef __SUNPRO_C",
(*"#pragma error_messages(off, E_INIT_DOES_NOT_FIT)",*)
"#pragma error_messages(off, E_STATEMENT_NOT_REACHED)",
"#endif",
"typedef char* ADDRESS;",
"typedef char* STRUCT;",
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

"#if !defined(_MSC_VER) && !defined(__cdecl)",
"#define __cdecl /* nothing */",
"#endif",
"#if !defined(_MSC_VER) && !defined(__stdcall)",
"#define __stdcall /* nothing */",
"#endif",

"#define M3STRUCT(n) m3struct_##n##_t",                                             (* TODO prune if not used *)
"#define M3STRUCT1(n) typedef struct { volatile UINT8 a[n]; } M3STRUCT(n);",        (* TODO prune if not used *)
"#define M3STRUCT2(n) typedef struct { volatile UINT16 a[(n)/2]; } M3STRUCT(n);",   (* TODO prune if not used *)
"#define M3STRUCT4(n) typedef struct { volatile UINT32 a[(n)/4]; } M3STRUCT(n);",   (* TODO prune if not used *)
"#define M3STRUCT8(n) typedef struct { volatile UINT64 a[(n)/8]; } M3STRUCT(n);",   (* TODO prune if not used *)
"#ifdef __cplusplus",
"#define M3_DOTDOTDOT ...",
"#else",
"#define M3_DOTDOTDOT",
"#endif",

(* WORD_T/INTEGER are always exactly the same size as a pointer.
 * VMS sometimes has 32bit size_t/ptrdiff_t but 64bit pointers. *)
"#if __INITIAL_POINTER_SIZE == 64",
"typedef __int64 INTEGER;",
"typedef unsigned __int64 WORD_T;",
"#else",
"typedef ptrdiff_t INTEGER;",
"typedef size_t WORD_T;",
"#endif",

"typedef /*long*/ double EXTENDED;",
"typedef WORD_T* SET;",
"#define SET_GRAIN (sizeof(WORD_T)*8)",

""};

<*NOWARN*>CONST Suffix = ARRAY OF TEXT {
"\n#ifdef __cplusplus",
"} /* extern \"C\" */",
"#endif"
};

CONST intLiteralPrefix = ARRAY CGType OF TEXT {
    "",  "((INT8)",
    "",  "((INT16)",
    "", "",
    "M3_UINT64(", "M3_INT64(",
    NIL, ..
};

CONST intLiteralSuffix = ARRAY CGType OF TEXT {
    "U",  ")",
    "U",  ")",
    "U", "",
    ")", ")",
    NIL, ..
};

CONST typeToText = ARRAY CGType OF TEXT {
    "UINT8",  "INT8",
    "UINT16", "INT16",
    "UINT32", "INT32",
    "UINT64", "INT64",
    "float",  (* REAL *)
    "double", (* LONGREAL *)
    "EXTENDED",
    "ADDRESS",
    "STRUCT",
    "void"
};

TYPE IntegerTypes = [M3CG.Type.Word8 .. M3CG.Type.Int64];

CONST typeIsInteger = ARRAY CGType OF BOOLEAN {
    TRUE, TRUE, (* 8 *)
    TRUE, TRUE, (* 16 *)
    TRUE, TRUE, (* 32 *)
    TRUE, TRUE, (* 64 *)
    FALSE, FALSE, FALSE, (* real, longreal, extended *)
    FALSE, FALSE, FALSE (* address, struct, void *)
};
CONST minMaxPossiblyValidForType = ARRAY CGType OF MinMaxBool_t {
    minMaxTrue, minMaxTrue, (* 8 *)
    minMaxTrue, minMaxTrue, (* 16 *)
    minMaxTrue, minMaxTrue, (* 32 *)
    minMaxTrue, minMaxTrue, (* 64 *)
    minMaxFalse, minMaxFalse, minMaxFalse, (* real, longreal, extended *)
    minMaxFalse, minMaxFalse, minMaxFalse (* address, struct, void *)
};
CONST typeIsUnsignedInt = ARRAY CGType OF BOOLEAN {
    TRUE, FALSE, (* 8 *)
    TRUE, FALSE, (* 16 *)
    TRUE, FALSE, (* 32 *)
    TRUE, FALSE, (* 64 *)
    FALSE, FALSE, FALSE, (* real, longreal, extended *)
    FALSE, FALSE, FALSE (* address, struct, void *)
};
CONST typeIsSignedInt = ARRAY CGType OF BOOLEAN {
    FALSE, TRUE, (* 8 *)
    FALSE, TRUE, (* 16 *)
    FALSE, TRUE, (* 32 *)
    FALSE, TRUE, (* 64 *)
    FALSE, FALSE, FALSE, (* real, longreal, extended *)
    FALSE, FALSE, FALSE (* address, struct, void *)
};
CONST typeSizeBits = ARRAY CGType OF [0..64] {
    8, 8, 16, 16, 32, 32, 64, 64,
    32, 64, 64, (* real, longreal, extended *)
    0, 0, 0 (* address, struct, void *)
};
CONST typeSizeBytes = ARRAY CGType OF [0..8] {
    1, 1, 2, 2, 4, 4, 8, 8,
    4, 8, 8, (* real, longreal, extended *)
    0, 0, 0 (* address, struct, void *)
};

CONST Min = 0;
CONST Max = 1;
TYPE MinOrMax = [Min..Max];
TYPE MinMaxInt_t = ARRAY MinOrMax OF Target.Int;
TYPE MinMaxBool_t = ARRAY MinOrMax OF BOOLEAN;

CONST minMaxFalse = MinMaxBool_t { FALSE, FALSE };
CONST minMaxTrue = MinMaxBool_t { TRUE, TRUE };
CONST int32MinMax = MinMaxInt_t{TInt.Min32, TInt.Max32};
CONST int64MinMax = MinMaxInt_t{TInt.Min64, TInt.Max64};

CONST typeMinMax = ARRAY CGType OF MinMaxInt_t {
    MinMaxInt_t{ TInt.Zero,  TWord.Max8  }, (* UINT8 *)
    MinMaxInt_t{ TInt.Min8,  TInt.Max8   }, (*  INT8 *)
    MinMaxInt_t{ TInt.Zero,  TWord.Max16 }, (* UINT16 *)
    MinMaxInt_t{ TInt.Min16, TInt.Max16  }, (*  INT16 *)
    MinMaxInt_t{ TInt.Zero,  TWord.Max32 }, (* UINT32 *)
    int32MinMax,
    MinMaxInt_t{ TInt.Zero,  TWord.Max64 }, (* UINT64 *)
    int64MinMax,
    (* The rest are not used, but provided for convenience. *)
    MinMaxInt_t{ TInt.Zero,  TInt.Zero   }, (* real *)
    MinMaxInt_t{ TInt.Zero,  TInt.Zero   }, (* longreal *)
    MinMaxInt_t{ TInt.Zero,  TInt.Zero   }, (* extended  *)
    MinMaxInt_t{ TInt.Zero,  TInt.Zero   }, (* address  *)
    MinMaxInt_t{ TInt.Zero,  TInt.Zero   }, (* struct  *)
    MinMaxInt_t{ TInt.Zero,  TInt.Zero   }  (* void  *)
};
CONST typeToUnsigned = ARRAY IntegerTypes OF IntegerTypes {
    M3CG.Type.Word8, M3CG.Type.Word8,
    M3CG.Type.Word16, M3CG.Type.Word16,
    M3CG.Type.Word32, M3CG.Type.Word32,
    M3CG.Type.Word64, M3CG.Type.Word64
};
CONST CompareOpC = ARRAY CompareOp OF TEXT { "==", "!=", ">", ">=", "<", "<=" };
CONST ConvertOpName = ARRAY ConvertOp OF TEXT { "round", "trunc", "floor", "ceil" };
CONST CompareOpName = ARRAY CompareOp OF TEXT { "eq", "ne", "gt", "ge", "lt", "le" };

(*---------------------------------------------------------------------------*)

PROCEDURE paren(expr: Expr_t): Expr_t =
BEGIN
(* It is possible we can reduce parens, but it is not as simple as it seems.
e.g. naive approach:
   ((ADDRESS)(1 + 2))
=> (ADDRESS)(1 + 2)
=> ADDRESS)(1 + 2
oops
The correct approach requires verifying the parens match.
*)
    RETURN CTextToExpr("(" & expr.CText() & ")");
END paren;

PROCEDURE pop(self: T; n: CARDINAL := 1) =
BEGIN
    FOR i := 1 TO n DO
        EVAL self.stack.remlo();
    END;
END pop;

PROCEDURE push(self: T; <*UNUSED*>type: M3CG.Type; expression: Expr_t) =
BEGIN
    self.stack.addlo(expression);
END push;

PROCEDURE get(self: T; n: CARDINAL := 0): Expr_t =
BEGIN
    RETURN NARROW(self.stack.get(n), Expr_t);
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
    self.stack := NEW(RefSeq.T).init();         (* CONSIDER compute maximum depth and use an array *)
    self.params := NEW(TextSeq.T).init();       (* CONSIDER compute maximum and use an array *)
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

(*------------------------------------------------ READONLY configuration ---*)

PROCEDURE DefaultErrorHandler (msg: TEXT) =
<*FATAL Wr.Failure, Thread.Alerted*>
BEGIN
    Wr.PutText (Stdio.stdout, Wr.EOL & "** ERROR in M3C: " & msg & " **" & Wr.EOL);
    Process.Exit (1);
END DefaultErrorHandler;

PROCEDURE set_error_handler (<*NOWARN*>self: T; <*NOWARN*>p: ErrorHandler) =
BEGIN
    self.Err := p;
END set_error_handler;

(*----------------------------------------------------- compilation units ---*)

PROCEDURE Prefix_Start(self: T; multipass: Multipass_t) =
BEGIN
    self.comment("begin unit");
    self.comment("M3_TARGET = ", Target.System_name);
    self.comment("M3_WORDSIZE = ", IntToDec(Target.Word.size));
    self.static_link_id := M3ID.Add("_static_link");
    self.RTHooks_ReportFault_id := M3ID.Add("RTHooks__ReportFault");
    self.RTHooks_Raise_id := M3ID.Add("RTHooks__Raise");
    self.RTException_Raise_id := M3ID.Add("RTException__Raise");
    self.RTHooks_AssertFailed_id := M3ID.Add("RTHooks__AssertFailed");
    SuppressLineDirective(self, 1, "begin_unit");
    IF NUMBER(multipass.op_data[M3CG_Binary.Op.case_jump]^) > 0 THEN
        IF CaseDefaultAssertFalse THEN
            print(self, "#include <assert.h>\n");
        END;
    END;
    FOR i := FIRST(Prefix) TO LAST(Prefix) DO
        print(self, Prefix[i] & "\n");
    END;
END Prefix_Start;

PROCEDURE Prefix_End(self: T) =
BEGIN
    SuppressLineDirective(self, -1, "begin_unit");
END Prefix_End;

PROCEDURE multipass_end_unit(self: Multipass_t) =
(* called at the end of the first pass -- we have everything
   in memory now, except for the end_unit itself.
   
   This function is in control of coordinating the passes.
 *)
VAR x := self.self;
    index: INTEGER;
BEGIN
    M3CG_MultiPass.end_unit(self); (* let M3CG_MultiPass do its usual last step *)
    self.Replay(x, index, self.op_data[M3CG_Binary.Op.begin_unit]);
    self.Replay(x, index, self.op_data[M3CG_Binary.Op.set_error_handler]);
    Prefix_Start(x, self);
    HelperFunctions(self);
    GetStructSizes(self);
    Prefix_End(x);

    (* forward declare functions/variables in this module and imports *)
    
    x.comment("begin pass: imports");
    self.Replay(NEW(Imports_t, self := x), index);
    x.comment("end pass: imports");

    (* discover all locals, including temps and params and check_* *)

    x.comment("begin pass: locals");
    x.temp_vars := NEW(REF ARRAY OF Var_t, NUMBER(self.data^));
    self.Replay(NEW(Locals_t, self := x), x.op_index);
    x.comment("end pass: locals");

    (* segments/globals *)

    x.comment("begin pass: segments/globals");
    self.Replay(NEW(Segments_t, self := x), index);
    x.comment("end pass: segments/globals");
    
    (* labels -- which are used *)
    DiscoverUsedLabels(self);
    
    (* variables -- which are used *)
    DiscoverUsedVariables(self);

    (* last pass *)

    self.Replay(x, x.op_index);
END multipass_end_unit;

PROCEDURE begin_unit(self: T; <*UNUSED*>optimize: INTEGER) =
(* The first call in a particular pass. *)
BEGIN
    self.in_proc := FALSE;
    self.current_proc := NIL;
    self.in_proc_call := FALSE;
END begin_unit;

PROCEDURE end_unit(self: T) =
(* The last call in a particular pass. *)
BEGIN
    self.comment("end unit");
    self.line_directive := ""; (* really suppress *)
    self.nl_line_directive := "\n"; (* really suppress *)
    SuppressLineDirective(self, 1, "end_unit");
    FOR i := FIRST(Suffix) TO LAST(Suffix) DO
        print(self, Suffix[i]);
        print(self, "\n");
    END;
    SuppressLineDirective(self, -1, "end_unit");
END end_unit;

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
    IF self.debug > 0 THEN
        self.comment("set_source_line " & Fmt.Int(line));
    ELSE
        self.comment("set_source_line");
    END;
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

PROCEDURE import_global(self: T; name: Name; byte_size: ByteSize; alignment: Alignment; type: M3CG.Type; <*UNUSED*>typeid: TypeUID): M3CG.Var =
VAR var := NEW(Var_t, self := self, m3cgtype := type, name := name, imported := TRUE).Init();
BEGIN
    self.comment("import_global");
    <* ASSERT (byte_size MOD alignment) = 0 *>
    <* ASSERT NOT self.in_proc *>
    print(self, "extern " & typeToText[type] & " " & M3ID.ToText(var.name) & ";\n");
    RETURN var;
END import_global;

PROCEDURE Locals_declare_segment(
    self: Locals_t;
    name: Name;
    typeid: TypeUID;
    const: BOOLEAN): M3CG.Var =
BEGIN
    RETURN declare_segment(self.self, name, typeid, const);
END Locals_declare_segment;

PROCEDURE declare_segment(self: T; name: Name; <*UNUSED*>typeid: TypeUID; const: BOOLEAN): M3CG.Var =
VAR var := NEW(Var_t, self := self, name := name, const := const).Init();
    fixed_name := var.name;
    text: TEXT := NIL;
    length := 0;
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
    print(self, "struct " & text & "_t;\n");
    print(self, "typedef struct " & text & "_t " & text & "_t;\n");
    RETURN var;
  END declare_segment;

PROCEDURE bind_segment(
    self: T;
    v: M3CG.Var;
    byte_size: ByteSize;
    alignment: Alignment;
    <*UNUSED*>type: M3CG.Type;
    <*UNUSED*>exported: BOOLEAN;
    <*UNUSED*>inited: BOOLEAN) =
VAR var := NARROW(v, Var_t);
BEGIN
    self.comment("bind_segment");
    <* ASSERT (byte_size MOD alignment) = 0 *>
    var.byte_size := byte_size;
END bind_segment;

PROCEDURE Segments_bind_segment(
    self: Segments_t;
    var: M3CG.Var;
    byte_size: ByteSize;
    alignment: Alignment;
    type: M3CG.Type;
    exported: BOOLEAN;
    inited: BOOLEAN) =
BEGIN
    bind_segment(self.self, var, byte_size, alignment, type, exported, inited);
END Segments_bind_segment;

PROCEDURE declare_global(self: T; name: Name; byte_size: ByteSize; alignment: Alignment;
                         type: M3CG.Type; typeid: TypeUID; exported: BOOLEAN; inited: BOOLEAN): M3CG.Var =
BEGIN
    self.comment("declare_global");
    RETURN DeclareGlobal(self, name, byte_size, alignment, type, typeid, exported, inited, FALSE);
END declare_global;

PROCEDURE Segments_declare_global(self: Segments_t; name: Name; byte_size: ByteSize; alignment: Alignment;
                                  type: M3CG.Type; typeid: TypeUID; exported: BOOLEAN; inited: BOOLEAN): M3CG.Var =
BEGIN
    RETURN declare_global(self.self, name, byte_size, alignment, type, typeid, exported, inited);
END Segments_declare_global;

PROCEDURE declare_constant(
    self: T;
    name: Name;
    byte_size: ByteSize;
    alignment: Alignment;
    type: M3CG.Type;
    typeid: TypeUID;
    exported: BOOLEAN;
    inited: BOOLEAN): M3CG.Var =
BEGIN
    self.comment("declare_constant");
    RETURN DeclareGlobal(self, name, byte_size, alignment, type, typeid, exported, inited, TRUE);
END declare_constant;

PROCEDURE Segments_declare_constant(
    self: Segments_t;
    name: Name;
    byte_size: ByteSize;
    alignment: Alignment;
    type: M3CG.Type;
    typeid: TypeUID;
    exported: BOOLEAN;
    inited: BOOLEAN): M3CG.Var =
BEGIN
    RETURN declare_constant(self.self, name, byte_size, alignment, type, typeid, exported, inited);
END Segments_declare_constant;

PROCEDURE DeclareGlobal(self: T; name: Name; byte_size: ByteSize; alignment: Alignment; type: M3CG.Type; <*UNUSED*>typeid: TypeUID; exported: BOOLEAN; <*UNUSED*>inited: BOOLEAN; const: BOOLEAN): M3CG.Var =
CONST DeclTag = ARRAY BOOLEAN OF TEXT { "declare_global", "declare_constant" };
VAR   var := NEW(Var_t, self := self, m3cgtype := type, name := name, const := const,
                 (*inited := inited, typeid := typeid, alignment := alignment,*)
                 exported := exported, global := TRUE,
                 proc := self.current_proc, byte_size := byte_size).Init();
BEGIN
    self.comment(DeclTag [const]);
    <* ASSERT (byte_size MOD alignment) = 0 *>
    print(self, var.Declare() & ";\n");
    RETURN var;
END DeclareGlobal;

TYPE DiscoverUsedVariables_t = M3CG_DoNothing.T BRANDED "M3C.DiscoverUsedVariables_t" OBJECT
OVERRIDES
    load := DiscoverUsedVariables_load;
    load_address := DiscoverUsedVariables_load_address;
    store := DiscoverUsedVariables_store;
END;

PROCEDURE DiscoverUsedVariables_common(var: M3CG.Var) =
BEGIN
    NARROW(var, Var_t).used := TRUE;
END DiscoverUsedVariables_common;

PROCEDURE DiscoverUsedVariables_load(
    <*UNUSED*>self: DiscoverUsedVariables_t;
    var: M3CG.Var;
    <*UNUSED*>offset: ByteOffset;
    <*UNUSED*>mtype: MType;
    <*UNUSED*>ztype: ZType) =
BEGIN
    DiscoverUsedVariables_common(var);
END DiscoverUsedVariables_load;

PROCEDURE DiscoverUsedVariables_load_address(
    <*UNUSED*>self: DiscoverUsedVariables_t;
    var: M3CG.Var;
    <*UNUSED*>offset: ByteOffset) =
BEGIN
    DiscoverUsedVariables_common(var);
END DiscoverUsedVariables_load_address;

PROCEDURE DiscoverUsedVariables_store(
    <*UNUSED*>self: DiscoverUsedVariables_t;
    var: M3CG.Var;
    <*UNUSED*>offset: ByteOffset;
    <*UNUSED*>ztype: ZType;
    <*UNUSED*>mtype: MType) =
BEGIN
    DiscoverUsedVariables_common(var);
END DiscoverUsedVariables_store;

PROCEDURE DiscoverUsedVariables(self: Multipass_t) =
(* frontend creates unreferenced variables, that gcc -Wall complains about;
   the point of this pass is to discover which variables are actually used,
   so that later code ignores unused variables
*)
TYPE Op = M3CG_Binary.Op;
CONST Ops = ARRAY OF Op{
    (* operands that use a variable -- marking the variable as used *)
        Op.load,
        Op.load_address,
        Op.store
    };
VAR x := self.self;
    pass := NEW(DiscoverUsedVariables_t);
    index: INTEGER;
BEGIN
    x.comment("begin pass: discover used variables");
    FOR i := FIRST(Ops) TO LAST(Ops) DO
        self.Replay(pass, index, self.op_data[Ops[i]]);
    END;
   x.comment("end pass: discover used variables");
END DiscoverUsedVariables;

TYPE CountUsedLabels_t = M3CG_DoNothing.T BRANDED "M3C.CountUsedLabels_t" OBJECT
    count := 0;
OVERRIDES
    case_jump := CountUsedLabels_case_jump;
END;

TYPE DiscoverUsedLabels_t = M3CG_DoNothing.T BRANDED "M3C.DiscoverUsedLabels_t" OBJECT
    array: REF ARRAY OF Label := NIL;
    index := 0;
    min := LAST(Label);
    max := FIRST(Label);
OVERRIDES
    jump := DiscoverUsedLabels_jump;
    if_true := DiscoverUsedLabels_if_true;
    if_false := DiscoverUsedLabels_if_true;
    if_compare := DiscoverUsedLabels_if_compare;
    case_jump := DiscoverUsedLabels_case_jump;
END;

PROCEDURE DiscoverUsedLabels_jump(self: DiscoverUsedLabels_t; label: Label) =
BEGIN
    self.min := MIN(self.min, label);
    self.max := MAX(self.max, label);
    self.array[self.index] := label;
    INC(self.index);
END DiscoverUsedLabels_jump;

PROCEDURE DiscoverUsedLabels_if_true(self: DiscoverUsedLabels_t; <*UNUSED*>itype: IType; label: Label; <*UNUSED*>frequency: Frequency) =
BEGIN
    DiscoverUsedLabels_jump(self, label);
END DiscoverUsedLabels_if_true;

PROCEDURE DiscoverUsedLabels_if_compare(
    self: DiscoverUsedLabels_t;
    <*UNUSED*>ztype: ZType;
    <*UNUSED*>op: CompareOp;
    label: Label;
    <*UNUSED*>frequency: Frequency) =
BEGIN
    DiscoverUsedLabels_jump(self, label);
END DiscoverUsedLabels_if_compare;

PROCEDURE DiscoverUsedLabels_case_jump(self: DiscoverUsedLabels_t; <*UNUSED*>itype: IType; READONLY labels: ARRAY OF Label) =
BEGIN
    FOR i := FIRST(labels) TO LAST(labels) DO
        DiscoverUsedLabels_jump(self, labels[i]);
    END;
END DiscoverUsedLabels_case_jump;

PROCEDURE CountUsedLabels_case_jump(self: CountUsedLabels_t; <*UNUSED*>itype: IType; READONLY labels: ARRAY OF Label) =
BEGIN
    INC(self.count, NUMBER(labels));
END CountUsedLabels_case_jump;

PROCEDURE DiscoverUsedLabels(self: Multipass_t) =
(* frontend creates unreferenced labels, that gcc -Wall complains about;
   the point of this pass is to discover which labels are actually used,
   so that later set_label ignores unused labels
*)
TYPE Op = M3CG_Binary.Op;
CONST Ops = ARRAY OF Op{
    (* operands that goto a label -- marking the label as used
       Except for case_jump, these ops use one label each.
       case_jump requires more specific analysis
    *)
        Op.jump,
        Op.if_true,
        Op.if_false,
        Op.if_compare,
        Op.case_jump
    };
VAR x := self.self;
    pass := NEW(DiscoverUsedLabels_t);
    count_pass := NEW(CountUsedLabels_t);
    index: INTEGER;
BEGIN
    x.comment("begin pass: discover used labels");
    
    (* First estimate label count via op count.
       This is correct, except for case_jump.
    *)
    FOR i := FIRST(Ops) TO LAST(Ops) DO
        INC(pass.index, self.op_counts[Ops[i]]);
    END;
    (* Subtract off case_jump, and then count it accurately. *)
    DEC(pass.index, self.op_counts[Op.case_jump]);
    self.Replay(count_pass, index, self.op_data[Op.case_jump]);
    INC(pass.index, count_pass.count);

    IF pass.index # 0 THEN
        pass.array := NEW(REF ARRAY OF Label, pass.index);
        pass.index := 0;
        FOR i := FIRST(Ops) TO LAST(Ops) DO
            self.Replay(pass, index, self.op_data[Ops[i]]);
        END;
        x.labels := NEW(REF ARRAY OF BOOLEAN, pass.max - pass.min + 1);
        x.labels_min := pass.min;
        x.labels_max := pass.max;
        FOR i := FIRST(x.labels^) TO LAST(x.labels^) DO
            x.labels[i] := FALSE;
        END;
        FOR i := 0 TO pass.index - 1 DO
            x.labels[pass.array[i] - pass.min] := TRUE;
        END;
    END;

   x.comment("end pass: discover used labels");
END DiscoverUsedLabels;

TYPE Segments_t = M3CG_DoNothing.T BRANDED "M3C.Segments_t" OBJECT
(* The goal of this pass is to build up segments/globals before they are used.
   Specifically, we used to do this:
       struct foo_t; typedef struct foo_t foo_t;
       [const] [static] foo_t foo; /* foo_t not yet defined */
       void F1(int);
       void F2(void) { F1(*(int*)(8 + &foo)); }
       struct foo_t { int a,b,c,d; };
       [const] [static] foo_t foo = { 1,2,3,4 };
    which is legal C but invalid C++.

    It is, I believe, sufficient to:
       struct foo_t { int a,b,c,d; };
       typedef struct foo_t foo_t;
       [const] [static] foo_t foo;
       void F1(int);
       void F2(void) { F1(*(int* )(8 + (char* )&foo)); } /* F1(foo.b); */
       [const] [static] foo_t foo = { 1,2,3,4 };

    However we now produce the clearly valid:
       struct foo_t { int a,b,c,d; };
       typedef struct foo_t foo_t;
       [const] [static] foo_t foo = { 1,2,3,4 };
       void F1(int);
       void F2(void) { F1(*(int*)(8 + (char*)&foo)); } /* F1(foo.b); */
*)
    self: T := NIL;
OVERRIDES
    bind_segment := Segments_bind_segment;
    begin_init := Segments_begin_init;
    end_init := Segments_end_init;
    init_int := Segments_init_int;
    init_proc := Segments_init_proc;
    init_label := Segments_init_label;
    init_var := Segments_init_var;
    init_offset := Segments_init_offset;
    init_chars := Segments_init_chars;
    init_float := Segments_init_float;
    declare_constant := Segments_declare_constant;
    declare_global := Segments_declare_global;
END;

PROCEDURE HelperFunctions(self: Multipass_t) =
(* We output several helper functions into the .c files.
   They are static.
   gcc -Wall complains about unused static functions.
   The goal of this pass is to discover exactly which
   functions we use and output only those.
*)
TYPE Op = M3CG_Binary.Op;
CONST Ops = ARRAY OF Op{
    (* These are operations that use helper functions. *)
        Op.div,
        Op.mod,
        Op.abs,
        Op.max,
        Op.min,
        Op.cvt_int,
        Op.set_compare,
        Op.shift,
        Op.shift_left,
        Op.shift_right,
        Op.rotate,
        Op.rotate_left,
        Op.rotate_right,
        Op.extract,
        Op.extract_n,
        Op.extract_mn,
        Op.insert,
        Op.insert_n,
        Op.insert_mn,
        Op.pop,
        Op.copy,
        Op.copy_n,
        Op.fence,
        Op.zero,
        Op.check_range, (* bug *)
        Op.xor,         (* bug *)
        Op.compare,     (* bug *)
        Op.if_compare,  (* bug *)
        Op.if_true,     (* bug *)
        Op.if_false     (* bug *)
    };
CONST OpsThatCanFault = ARRAY OF Op{
        Op.abort,
        Op.check_nil,
        Op.check_lo,
        Op.check_hi,
        Op.check_range,
        Op.check_index,
        Op.check_eq
    };
TYPE T1 = RECORD op: Op; text: TEXT END;
CONST data = ARRAY OF T1{
    T1{Op.set_union, "static void __stdcall m3_set_union(WORD_T n_words,WORD_T*c,WORD_T*b,WORD_T*a){WORD_T i=0;for(;i<n_words;++i)a[i]=b[i]|c[i];}"},
    T1{Op.set_difference, "static void __stdcall m3_set_difference(WORD_T n_words,WORD_T*c,WORD_T*b,WORD_T*a){WORD_T i=0;for(;i<n_words;++i)a[i]=b[i]&(~c[i]);}"},
    T1{Op.set_intersection, "static void __stdcall m3_set_intersection(WORD_T n_words,WORD_T*c,WORD_T*b,WORD_T*a){WORD_T i=0;for(;i<n_words;++i)a[i]=b[i]&c[i];}"},
    T1{Op.set_sym_difference, "static void __stdcall m3_set_sym_difference(WORD_T n_words,WORD_T*c,WORD_T*b,WORD_T*a){WORD_T i=0;for(;i<n_words;++i)a[i]=b[i]^c[i];}"},
    T1{Op.set_range,
          "\n#define M3_HIGH_BITS(a) ((~(WORD_T)0) << (a))\n"
        & "#define M3_LOW_BITS(a)  ((~(WORD_T)0) >> (SET_GRAIN - (a) - 1))\n"
        & "static void __stdcall m3_set_range(WORD_T b, WORD_T a, WORD_T* s)\n"
        & "{\n"
        & "  if (a >= b)\n"
        & "  {\n"
        & "    WORD_T i = 0;\n"
        & "    WORD_T const a_word = a / SET_GRAIN;\n"
        & "    WORD_T const b_word = b / SET_GRAIN;\n"
        & "    WORD_T const high_bits = M3_HIGH_BITS(a % SET_GRAIN);\n"
        & "    WORD_T const low_bits = M3_LOW_BITS(b % SET_GRAIN);\n"
        & "    if (a_word == b_word)\n"
        & "    {\n"
        & "      s[a_word] |= (high_bits & low_bits);\n"
        & "    }\n"
        & "    else\n"
        & "    {\n"
        & "      s[a_word] |= high_bits;\n"
        & "      for (i = a_word + 1; i < b_word; ++i)\n" (* could use memset here *)
        & "        s[i] = ~(WORD_T)0;\n"
        & "      s[b_word] |= low_bits;\n"
        & "    }\n"
        & "  }\n"
        & "}\n"
        },
    T1{Op.set_singleton, "static void __stdcall m3_set_singleton(WORD_T a,WORD_T*s){s[a/SET_GRAIN]|=(((WORD_T)1)<<(a%SET_GRAIN));}"},
    T1{Op.set_member, "static WORD_T __stdcall m3_set_member(WORD_T elt,WORD_T*set){return(set[elt/SET_GRAIN]&(((WORD_T)1)<<(elt%SET_GRAIN)))!=0;}"}
    };
VAR x := self.self;
    helperFunctions := NEW(HelperFunctions_t).Init(x);
    index: INTEGER;
BEGIN
    x.comment("begin pass: helper functions");
     
    FOR i := FIRST(data) TO LAST(data) DO
        IF self.op_counts[data[i].op] > 0 THEN
            print(x, data[i].text);
        END;
    END;
    FOR i := FIRST(OpsThatCanFault) TO LAST(OpsThatCanFault) DO
        IF self.op_counts[OpsThatCanFault[i]] > 0 THEN
            no_return(x);
            x.report_fault_used := TRUE;
            EXIT;
        END;
    END;
    FOR i := FIRST(Ops) TO LAST(Ops) DO
        self.Replay(helperFunctions, index, self.op_data[Ops[i]]);
    END;
    x.comment("end pass: helper functions");
END HelperFunctions;

TYPE HelperFunctions_t = M3CG_DoNothing.T BRANDED "M3C.HelperFunctions_t" OBJECT
    self: T := NIL;
    data: RECORD
        pos, memcmp, memcpy, memmove, memset, floor, ceil, fence,
        set_le, set_lt, extract_inline := FALSE;
        cvt_int := ARRAY ConvertOp OF BOOLEAN{FALSE, ..};
        shift, rotate, rotate_left, rotate_right, div, mod, min, max, abs,
        extract, insert, sign_extend, pop, check_range, xor := SET OF CGType{};
        compare := ARRAY CompareOp OF SET OF CGType{ SET OF CGType{}, .. };
    END;
METHODS
    Init(outer: T): HelperFunctions_t := HelperFunctions_Init;
OVERRIDES
    div := HelperFunctions_div;
    mod := HelperFunctions_mod;
    abs := HelperFunctions_abs;
    max := HelperFunctions_max;
    min := HelperFunctions_min;
    cvt_int := HelperFunctions_cvt_int;
    set_compare := HelperFunctions_set_compare;
    shift := HelperFunctions_shift;
    rotate := HelperFunctions_rotate;
    rotate_left := HelperFunctions_rotate_left;
    rotate_right := HelperFunctions_rotate_right;
    extract := HelperFunctions_extract;
    extract_n := HelperFunctions_extract_n;
    extract_mn := HelperFunctions_extract_mn;
    insert := HelperFunctions_insert;
    insert_n := HelperFunctions_insert_n;
    insert_mn := HelperFunctions_insert_mn;
    pop := HelperFunctions_pop;
    copy := HelperFunctions_copy;
    copy_n := HelperFunctions_copy_n;
    zero := HelperFunctions_zero;
    fence := HelperFunctions_fence;
    check_range := HelperFunctions_check_range;
    xor := HelperFunctions_xor;
    compare := HelperFunctions_compare;
    if_compare := HelperFunctions_if_compare;
    if_true := HelperFunctions_if_true;
    if_false := HelperFunctions_if_false;
END;

PROCEDURE HelperFunctions_Init(self: HelperFunctions_t; outer: T): HelperFunctions_t =
BEGIN
    self.self := outer;
    RETURN self;
END HelperFunctions_Init;

PROCEDURE HelperFunctions_print_array(self: HelperFunctions_t; READONLY text: ARRAY OF TEXT) =
VAR x := self.self;
BEGIN
    IF NOT x.last_char_was_newline THEN
        print(x, "\n");
    END;
    FOR i := FIRST(text) TO LAST(text) DO
        print(x, text[i]);
        print(x, "\n");
    END;
END HelperFunctions_print_array;

PROCEDURE HelperFunctions_helper_with_boolean_and_array(self: HelperFunctions_t; VAR boolean: BOOLEAN; READONLY text: ARRAY OF TEXT) =
BEGIN
    IF NOT boolean THEN
        HelperFunctions_print_array(self, text);
        boolean := TRUE;
    END;
END HelperFunctions_helper_with_boolean_and_array;

PROCEDURE HelperFunctions_helper_with_boolean(self: HelperFunctions_t; VAR boolean: BOOLEAN; text: TEXT) =
BEGIN
    HelperFunctions_helper_with_boolean_and_array(self, boolean, ARRAY OF TEXT{text});
END HelperFunctions_helper_with_boolean;

PROCEDURE HelperFunctions_helper_with_type_and_array(self: HelperFunctions_t; op: TEXT; type: CGType; VAR types: SET OF CGType; READONLY first: ARRAY OF TEXT) =
BEGIN
    IF types = SET OF CGType{} THEN
        HelperFunctions_print_array(self, first);
    END;
    IF NOT type IN types THEN
        print(self.self, "m3_" & op & "_T(" & typeToText[type] & ")");
        types := types + SET OF CGType{type};
    END;
END HelperFunctions_helper_with_type_and_array;

PROCEDURE HelperFunctions_helper_with_type(self: HelperFunctions_t; op: TEXT; type: CGType; VAR types: SET OF CGType; first: TEXT) =
BEGIN
    HelperFunctions_helper_with_type_and_array(self, op, type, types, ARRAY OF TEXT{first});
END HelperFunctions_helper_with_type;

PROCEDURE HelperFunctions_memset(self: HelperFunctions_t) =
CONST text = "void* __cdecl memset(void*, int, size_t); /* string.h */";
BEGIN
    HelperFunctions_helper_with_boolean(self, self.data.memset, text);
END HelperFunctions_memset;

PROCEDURE HelperFunctions_memmove(self: HelperFunctions_t) =
CONST text = "void* __cdecl memmove(void*, const void*, size_t); /* string.h */";
BEGIN
    HelperFunctions_helper_with_boolean(self, self.data.memmove, text);
END HelperFunctions_memmove;

PROCEDURE HelperFunctions_memcpy(self: HelperFunctions_t) =
CONST text = "void* __cdecl memcpy(void*, const void*, size_t); /* string.h */";
BEGIN
    HelperFunctions_helper_with_boolean(self, self.data.memcpy, text);
END HelperFunctions_memcpy;

PROCEDURE HelperFunctions_memcmp(self: HelperFunctions_t) =
CONST text = "int __cdecl memcmp(const void*, const void*, size_t); /* string.h */";
BEGIN
    HelperFunctions_helper_with_boolean(self, self.data.memcmp, text);
END HelperFunctions_memcmp;

PROCEDURE HelperFunctions_pos(self: HelperFunctions_t) =
CONST text = ARRAY OF TEXT{
"/* return positive form of a negative value, avoiding overflow */",
"/* T should be an unsigned type */",
"#define M3_POS(T, a) (((T)-((a) + 1)) + 1)"
};
BEGIN
    HelperFunctions_helper_with_boolean_and_array(self, self.data.pos, text);
END HelperFunctions_pos;

PROCEDURE HelperFunctions_div(self: HelperFunctions_t; type: IType; a, b: Sign) =
CONST text = ARRAY OF TEXT{
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
"}"
};
BEGIN
    IF NOT (((a = b) AND (a # Sign.Unknown)) OR typeIsUnsignedInt[type]) THEN
        HelperFunctions_pos(self);
        HelperFunctions_helper_with_type_and_array(self, "div", type, self.data.div, text);
    END;
END HelperFunctions_div;

PROCEDURE HelperFunctions_mod(self: HelperFunctions_t; type: IType; a, b: Sign) =
CONST text = ARRAY OF TEXT{
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
"}"
};
BEGIN
    IF NOT (((a = b) AND (a # Sign.Unknown)) OR typeIsUnsignedInt[type]) THEN
        HelperFunctions_pos(self);
        HelperFunctions_helper_with_type_and_array(self, "mod", type, self.data.mod, text);
    END;
END HelperFunctions_mod;

PROCEDURE HelperFunctions_abs(self: HelperFunctions_t; type: AType) =
CONST text = "#define m3_abs_T(T) static T __stdcall m3_abs_##T(T a) { return ((a < 0) ? (-a) : a); }";
BEGIN
    HelperFunctions_helper_with_type(self, "abs", type, self.data.abs, text);
END HelperFunctions_abs;

PROCEDURE HelperFunctions_max(self: HelperFunctions_t; type: ZType) =
CONST text = "#define m3_max_T(T) static T __stdcall m3_max_##T(T a, T b) { return ((a > b) ? a : b); }";
BEGIN
    HelperFunctions_helper_with_type(self, "max", type, self.data.max, text);
END HelperFunctions_max;

PROCEDURE HelperFunctions_min(self: HelperFunctions_t; type: ZType) =
CONST text = "#define m3_min_T(T) static T __stdcall m3_min_##T(T a, T b) { return ((a < b) ? a : b); }";
BEGIN
    HelperFunctions_helper_with_type(self, "min", type, self.data.min, text);
END HelperFunctions_min;

PROCEDURE HelperFunctions_cvt_int(self: HelperFunctions_t; <*UNUSED*>rtype: RType; <*UNUSED*>itype: IType; op: ConvertOp) =
CONST text = ARRAY ConvertOp OF TEXT{
    "INT64 __cdecl llroundl(long double);\nstatic INT64 __stdcall m3_round(EXTENDED f) { return (INT64)llroundl(f); }",
    "static INT64 __stdcall m3_trunc(EXTENDED f) { return (INT64)f; }",
    "double __cdecl floor(double);\nstatic INT64 __stdcall m3_floor(EXTENDED f) { return floor(f); } /* math.h */",
    "double __cdecl ceil(double);\nstatic INT64 __stdcall m3_ceil(EXTENDED f) { return ceil(f); } /* math.h */"
    };
BEGIN
    HelperFunctions_helper_with_boolean(self, self.data.cvt_int[op], text[op]);
END HelperFunctions_cvt_int;

PROCEDURE HelperFunctions_set_compare(self: HelperFunctions_t; <*UNUSED*>byte_size: ByteSize; op: CompareOp; <*UNUSED*>type: IType) =
CONST text_le = "static WORD_T __stdcall m3_set_le(WORD_T n_words,WORD_T*b,WORD_T*a){WORD_T i=0;for(;i<n_words;++i)if(a[i]&(~b[i]))return 0;return 1;}";
CONST text_lt = "static WORD_T __stdcall m3_set_lt(WORD_T n_words,WORD_T*b,WORD_T*a){WORD_T i=0,eq=0;for(;i<n_words;++i)if(a[i]&(~b[i]))return 0;else eq|=(a[i]^b[i]);return(eq!=0);}";
BEGIN
    CASE op OF
        | CompareOp.EQ, CompareOp.NE =>
            HelperFunctions_memcmp(self);
        | CompareOp.GE, CompareOp.LE =>
            HelperFunctions_helper_with_boolean(self, self.data.set_le, text_le);
        | CompareOp.GT, CompareOp.LT =>
            HelperFunctions_helper_with_boolean(self, self.data.set_lt, text_lt);
    END;
END HelperFunctions_set_compare;

PROCEDURE HelperFunctions_shift(self: HelperFunctions_t; type: IType) =
CONST text = "#define m3_shift_T(T) static T m3_shift_##T(T value,INTEGER shift){if((shift>=(INTEGER)(sizeof(T)*8))||(shift<=(INTEGER)-(sizeof(T)*8)))value=0;else if(shift>0)value<<=shift;else if(shift<0)value>>=-shift;return value;}";
BEGIN
    HelperFunctions_helper_with_type(self, "shift", type, self.data.shift, text);
END HelperFunctions_shift;

PROCEDURE HelperFunctions_rotate(self: HelperFunctions_t; type: IType) =
CONST text = "#define m3_rotate_T(T) static T __stdcall m3_rotate_##T(T a,int b){b&=((sizeof(a)*8)-1);if(b>0)a=m3_rotate_left_##T(a,b);else if(b<0)a=m3_rotate_right_##T(a,-b);return a;}";
BEGIN
    HelperFunctions_rotate_left(self, type);
    HelperFunctions_rotate_right(self, type);
    HelperFunctions_helper_with_type(self, "rotate", type, self.data.rotate, text);
END HelperFunctions_rotate;

PROCEDURE HelperFunctions_rotate_left(self: HelperFunctions_t; type: IType) =
CONST text = "#define m3_rotate_left_T(T) static T __stdcall m3_rotate_left_##T(T a,int b){return((a<<b)|(a>>((sizeof(a)*8)-b)));}";
BEGIN
    HelperFunctions_helper_with_type(self, "rotate_left", type, self.data.rotate_left, text);
END HelperFunctions_rotate_left;

PROCEDURE HelperFunctions_rotate_right(self: HelperFunctions_t; type: IType) =
CONST text = "#define m3_rotate_right_T(T) static T __stdcall m3_rotate_right_##T(T a,int b){return((a>>b)|(a<<((sizeof(a)*8)-b)));}";
BEGIN
    HelperFunctions_helper_with_type(self, "rotate_right", type, self.data.rotate_right, text);
END HelperFunctions_rotate_right;

PROCEDURE HelperFunctions_extract(self: HelperFunctions_t; type: IType; sign_extend: BOOLEAN) =
CONST text = "#define m3_extract_T(T) static T __stdcall m3_extract_##T(T value,WORD_T offset,WORD_T count){return((value>>offset)&~(((~(T)0))<<count));}";
CONST text_inline = "#define m3_extract(T, value, offset, count) ((((T)value)>>((WORD_T)offset))&~(((~(T)0))<<((WORD_T)count)))";
CONST text_sign_extend = "#define m3_sign_extend_T(T) static T __stdcall m3_sign_extend_##T(T value,WORD_T count){return(value|((value&(((T)-1)<<(count-1)))?(((T)-1)<<(count-1)):0));}";
BEGIN
    IF inline_extract THEN
        HelperFunctions_helper_with_boolean(self, self.data.extract_inline, text_inline);
    ELSE
        HelperFunctions_helper_with_type(self, "extract", typeToUnsigned[type], self.data.extract, text);
    END;
    IF sign_extend THEN
        HelperFunctions_helper_with_type(self, "sign_extend", type, self.data.sign_extend, text_sign_extend);
    END;
END HelperFunctions_extract;

PROCEDURE HelperFunctions_extract_n(self: HelperFunctions_t; type: IType; sign_extend: BOOLEAN; <*UNUSED*>count: CARDINAL) =
BEGIN
    HelperFunctions_extract(self, type, sign_extend);
END HelperFunctions_extract_n;

PROCEDURE HelperFunctions_extract_mn(self: HelperFunctions_t; type: IType; sign_extend: BOOLEAN; <*UNUSED*>offset: CARDINAL; <*UNUSED*>count: CARDINAL) =
BEGIN
    HelperFunctions_extract(self, type, sign_extend);
END HelperFunctions_extract_mn;

PROCEDURE HelperFunctions_insert(self: HelperFunctions_t; type: IType) =
CONST text = "#define m3_insert_T(T) static T __stdcall m3_insert_##T(T x,T y,WORD_T offset,WORD_T count){T mask=(~((~(T)0)<<count))<<offset;return(((y<<offset)&mask)|(x&~mask));}";
BEGIN
    HelperFunctions_helper_with_type(self, "insert", type, self.data.insert, text);
END HelperFunctions_insert;

PROCEDURE HelperFunctions_insert_n(self: HelperFunctions_t; type: IType; <*UNUSED*>count: CARDINAL) =
BEGIN
    HelperFunctions_insert(self, type);
END HelperFunctions_insert_n;

PROCEDURE HelperFunctions_insert_mn(self: HelperFunctions_t; type: IType; <*UNUSED*>offset: CARDINAL; <*UNUSED*>count: CARDINAL) =
BEGIN
    HelperFunctions_insert(self, type);
END HelperFunctions_insert_mn;

PROCEDURE HelperFunctions_pop(self: HelperFunctions_t; type: M3CG.Type) =
CONST text = "#define m3_pop_T(T) static void __stdcall m3_pop_##T(volatile T a) { }";
BEGIN
    HelperFunctions_helper_with_type(self, "pop", type, self.data.pop, text);
END HelperFunctions_pop;

PROCEDURE HelperFunctions_check_range(self: HelperFunctions_t; type: IType; <*UNUSED*>READONLY low, high: Target.Int; <*UNUSED*>code: RuntimeError) =
(* Ideally the helper would call report_fault but I do not think we have the name yet.
 * This is a helper function to avoid a warning from gcc about the range check
 * being redundant.
 *)
CONST text = "#define m3_check_range_T(T) static int __stdcall m3_check_range_##T(T value, T low, T high){return value<low||high<value;}";
BEGIN
    HelperFunctions_helper_with_type(self, "check_range", type, self.data.check_range, text);
END HelperFunctions_check_range;

PROCEDURE HelperFunctions_xor(self: HelperFunctions_t; type: IType) =
(* Ideally this is not a helper function. This is a workaround to prevent warnings from gcc.
 * We could limit it to certain versions of gcc. *)
CONST text = "#define m3_xor_T(T) static int __stdcall m3_xor_##T(T x, T y){return x ^ y;}";
BEGIN
    IF NOT AvoidGccTypeRangeWarnings THEN
        RETURN;
    END;
    HelperFunctions_helper_with_type(self, "xor", type, self.data.xor, text);
END HelperFunctions_xor;

PROCEDURE HelperFunctions_if_true(self: HelperFunctions_t; itype: IType; <*UNUSED*>label: Label; <*UNUSED*>frequency: Frequency) =
(* IF (s0.itype # 0) GOTO label; pop *)
BEGIN
    HelperFunctions_internal_compare(self, itype, CompareOp.NE);
END HelperFunctions_if_true;

PROCEDURE HelperFunctions_if_false(self: HelperFunctions_t; itype: IType; <*UNUSED*>label: Label; <*UNUSED*>frequency: Frequency) =
(* IF (s0.itype = 0) GOTO label; pop *)
BEGIN
    HelperFunctions_internal_compare(self, itype, CompareOp.EQ);
END HelperFunctions_if_false;

PROCEDURE HelperFunctions_internal_compare(
    self: HelperFunctions_t;
    type: Type;
    op: CompareOp) =
(* Ideally this is not a helper function. This is a workaround to prevent warnings from gcc.
 * We could limit it to certain versions of gcc. And do better with constants esp. 0. *)
CONST text = ARRAY CompareOp OF TEXT {
    "#define m3_eq_T(T) static WORD_T __stdcall m3_eq_##T(T x, T y){return x==y;}",
    "#define m3_ne_T(T) static WORD_T __stdcall m3_ne_##T(T x, T y){return x!=y;}",
    "#define m3_gt_T(T) static WORD_T __stdcall m3_gt_##T(T x, T y){return x>y;}",
    "#define m3_ge_T(T) static WORD_T __stdcall m3_ge_##T(T x, T y){return x>=y;}",
    "#define m3_lt_T(T) static WORD_T __stdcall m3_lt_##T(T x, T y){return x<y;}",
    "#define m3_le_T(T) static WORD_T  __stdcall m3_le_##T(T x, T y){return x<=y;}" };
BEGIN
    IF NOT AvoidGccTypeRangeWarnings THEN
        RETURN;
    END;
    HelperFunctions_helper_with_type(self, CompareOpName[op], type, self.data.compare[op], text[op]);
END HelperFunctions_internal_compare;

PROCEDURE HelperFunctions_compare(
    self: HelperFunctions_t;
    ztype: ZType;
    <*UNUSED*>itype: IType;
    op: CompareOp) =
BEGIN
    HelperFunctions_internal_compare(self, ztype, op);
END HelperFunctions_compare;

PROCEDURE HelperFunctions_if_compare(
    self: HelperFunctions_t;
    ztype: ZType;
    op: CompareOp;
    <*UNUSED*>label: Label;
    <*UNUSED*>frequency: Frequency) =
BEGIN
    HelperFunctions_internal_compare(self, ztype, op);
END HelperFunctions_if_compare;

PROCEDURE HelperFunctions_copy_common(self: HelperFunctions_t; overlap: BOOLEAN) =
BEGIN
    IF overlap THEN
        HelperFunctions_memmove(self);
    ELSE
        HelperFunctions_memcpy(self);
    END
END HelperFunctions_copy_common;

PROCEDURE HelperFunctions_copy_n(self: HelperFunctions_t; <*UNUSED*>itype: IType; <*UNUSED*>mtype: MType; overlap: BOOLEAN) =
BEGIN
    HelperFunctions_copy_common(self, overlap);
END HelperFunctions_copy_n;

PROCEDURE HelperFunctions_copy(self: HelperFunctions_t; <*UNUSED*>n: INTEGER; <*UNUSED*>mtype: MType; overlap: BOOLEAN) =
BEGIN
    HelperFunctions_copy_common(self, overlap);
END HelperFunctions_copy;

PROCEDURE HelperFunctions_zero(self: HelperFunctions_t; <*UNUSED*>n: INTEGER; <*UNUSED*>type: MType) =
BEGIN
    HelperFunctions_memset(self);
END HelperFunctions_zero;

PROCEDURE HelperFunctions_fence(self: HelperFunctions_t; <*UNUSED*>order: MemoryOrder) =
CONST text = ARRAY OF TEXT{
"#ifdef _MSC_VER",
"long __cdecl _InterlockedExchange(volatile long*, long);",
"#pragma instrinsic(_InterlockedExchange)",
"static volatile long m3_fence_var;",
"#define m3_fence() _InterlockedExchange(&m3_fence_var, 0)",
"#else",
"static void __stdcall m3_fence(void){}", (* not yet implemented *)
"#endif"
};
BEGIN
    HelperFunctions_helper_with_boolean_and_array(self, self.data.fence, text);
END HelperFunctions_fence;

TYPE Locals_t = M3CG_DoNothing.T BRANDED "M3C.Locals_t" OBJECT
    self: T := NIL;
OVERRIDES
    declare_segment := Locals_declare_segment; (* declare_segment is needed, to get the unit name, to check for exception handlers *)
    declare_procedure := Locals_declare_procedure;
    begin_procedure := Locals_begin_procedure;
    end_procedure := Locals_end_procedure;
    declare_param := Locals_declare_param;
    declare_local := Locals_declare_local;
    declare_temp := Locals_declare_temp;
    begin_block := Locals_begin_block;   (* FUTURE: for unions in frame struct *)
    end_block := Locals_end_block;       (* FUTURE: for unions in frame struct *)
    pop_static_link := Locals_pop_static_link;  (* pop_static_link is needed because it calls declare_temp *)

    check_lo := AllocateTemps_check_lo;
    check_hi := AllocateTemps_check_hi;
    check_index := AllocateTemps_check_index;
    check_range := AllocateTemps_check_range;
    check_nil := AllocateTemps_check_nil;
END;

PROCEDURE Locals_declare_param(
    self: Locals_t;
    name: Name;
    byte_size: ByteSize;
    alignment: Alignment;
    type: M3CG.Type;
    typeid: TypeUID;
    in_memory: BOOLEAN;
    up_level: BOOLEAN;
    frequency: Frequency): M3CG.Var =
BEGIN
    RETURN declare_param(
        self.self,
        name,
        byte_size,
        alignment,
        type,
        typeid,
        in_memory,
        up_level,
        frequency);
END Locals_declare_param;

PROCEDURE Locals_declare_local(
    self: Locals_t;
    name: Name;
    byte_size: ByteSize;
    alignment: Alignment;
    type: M3CG.Type;
    typeid: TypeUID;
    in_memory: BOOLEAN;
    up_level: BOOLEAN;
    frequency: Frequency): M3CG.Var =
BEGIN
    RETURN declare_local(self.self, name, byte_size, alignment, type, typeid, in_memory, up_level, frequency);
END Locals_declare_local;

TYPE AllocateTemps_t = Locals_t;

PROCEDURE AllocateTemps_common(self: AllocateTemps_t; type: M3CG.Type) =
VAR x := self.self;
BEGIN
    x.comment("AllocateTemps_common");
    WITH t = internal_declare_temp(x, CG_Bytes[type], CG_Bytes[type], type, FALSE) DO
        t.used := TRUE;
        x.temp_vars[x.op_index] := t;
    END;
END AllocateTemps_common;

PROCEDURE AllocateTemps_check_nil(self: AllocateTemps_t; <*UNUSED*>code: RuntimeError) =
VAR x := self.self;
BEGIN
    x.comment("AllocateTemps_check_nil");
    AllocateTemps_common(self, M3CG.Type.Addr);
END AllocateTemps_check_nil;

PROCEDURE AllocateTemps_check_lo(self: AllocateTemps_t; type: IType; <*UNUSED*>READONLY i: Target.Int; <*UNUSED*>code: RuntimeError) =
VAR x := self.self;
BEGIN
    x.comment("AllocateTemps_check_lo");
    AllocateTemps_common(self, type);
END AllocateTemps_check_lo;

PROCEDURE AllocateTemps_check_hi(self: AllocateTemps_t; type: IType; <*UNUSED*>READONLY i: Target.Int; <*UNUSED*>code: RuntimeError) =
VAR x := self.self;
BEGIN
    x.comment("AllocateTemps_check_hi");
    AllocateTemps_common(self, type);
END AllocateTemps_check_hi;

PROCEDURE AllocateTemps_check_range(self: AllocateTemps_t; type: IType; <*UNUSED*>READONLY low: Target.Int; <*UNUSED*>READONLY high: Target.Int; <*UNUSED*>code: RuntimeError) =
VAR x := self.self;
BEGIN
    x.comment("AllocateTemps_check_range");
    AllocateTemps_common(self, type);
END AllocateTemps_check_range;

PROCEDURE AllocateTemps_check_index(self: AllocateTemps_t; type: IType; <*UNUSED*>code: RuntimeError) =
VAR x := self.self;
BEGIN
    x.comment("AllocateTemps_check_index");
    AllocateTemps_common(self, type);
END AllocateTemps_check_index;

TYPE Imports_t = M3CG_DoNothing.T BRANDED "M3C.Imports_t" OBJECT
    self: T;
OVERRIDES
    import_procedure := Imports_import_procedure;
    declare_param := Imports_declare_param;
    import_global := Imports_import_global;
END;

PROCEDURE Imports_import_procedure(
    self: Imports_t;
    name: Name;
    n_params: INTEGER;
    return_type: M3CG.Type;
    callingConvention: CallingConvention): M3CG.Proc =
BEGIN
    RETURN import_procedure(self.self, name, n_params, return_type, callingConvention);
END Imports_import_procedure;

PROCEDURE Imports_declare_param(
    self: Imports_t;
    name: Name;
    byte_size: ByteSize;
    alignment: Alignment;
    type: M3CG.Type;
    typeid: TypeUID;
    in_memory: BOOLEAN;
    up_level: BOOLEAN;
    frequency: Frequency): M3CG.Var =
BEGIN
    RETURN declare_param(self.self, name, byte_size, alignment, type, typeid,
        in_memory, up_level, frequency);
END Imports_declare_param;

PROCEDURE Imports_import_global(
    self: Imports_t;
    name: Name;
    byte_size: ByteSize; 
    alignment: Alignment;
    type: M3CG.Type;
    typeid: TypeUID): M3CG.Var =
BEGIN
    RETURN import_global(self.self, name, byte_size, alignment, type, typeid);
END Imports_import_global;

TYPE GetStructSizes_t = M3CG_DoNothing.T BRANDED "M3C.GetStructSizes_t" OBJECT
    sizes: REF ARRAY OF INTEGER := NIL;
    count := 0;
METHODS
    Declare(type: M3CG.Type; byte_size: ByteSize; alignment: Alignment): M3CG.Var := GetStructSizes_Declare;
OVERRIDES
    declare_constant := GetStructSizes_declare_constant;
    declare_global := GetStructSizes_declare_global;
    declare_local := GetStructSizes_declare_local_or_param;
    declare_param := GetStructSizes_declare_local_or_param;
    declare_temp := GetStructSizes_declare_temp;
    import_global := GetStructSizes_import_global;
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
    x := self.self;
    index: INTEGER;
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
        self.Replay(getStructSizes, index, self.op_data[Ops[i]]);
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
                    print(x, "M3STRUCT" & IntToDec(units[unit]) & "(" & IntToDec(size) & ")\n");
                    EXIT;
                END;
            END;
        END;
    END;

END GetStructSizes;

PROCEDURE GetStructSizes_Declare(self: GetStructSizes_t; type: M3CG.Type; byte_size: ByteSize; alignment: Alignment): M3CG.Var =
BEGIN
    <* ASSERT byte_size >= 0 *>
    <* ASSERT (byte_size MOD alignment) = 0 *>
    byte_size := MAX(byte_size, 1);
    IF type = M3CG.Type.Struct THEN
        self.sizes[self.count] := byte_size;
        INC(self.count);
    END;
    RETURN NIL;
END GetStructSizes_Declare;

PROCEDURE GetStructSizes_declare_temp(
    self: GetStructSizes_t;
    byte_size: ByteSize;
    alignment: Alignment;
    type: M3CG.Type;
    <*UNUSED*>in_memory:BOOLEAN): M3CG.Var =
BEGIN
    RETURN self.Declare(type, byte_size, alignment);
END GetStructSizes_declare_temp;

PROCEDURE GetStructSizes_declare_global(
    self: GetStructSizes_t;
    <*UNUSED*>name: Name;
    byte_size: ByteSize;
    alignment: Alignment;
    type: M3CG.Type;
    <*UNUSED*>typeid: TypeUID;
    <*UNUSED*>exported: BOOLEAN;
    <*UNUSED*>inited: BOOLEAN): M3CG.Var =
BEGIN
    RETURN self.Declare(type, byte_size, alignment);
END GetStructSizes_declare_global;

PROCEDURE GetStructSizes_import_global(
    self: GetStructSizes_t;
    <*UNUSED*>name: Name;
    byte_size: ByteSize;
    alignment: Alignment;
    type: M3CG.Type;
    <*UNUSED*>typeid: TypeUID): M3CG.Var =
BEGIN
    RETURN self.Declare(type, byte_size, alignment);
END GetStructSizes_import_global;

PROCEDURE GetStructSizes_declare_constant(
    self: GetStructSizes_t;
    <*UNUSED*>name: Name;
    byte_size: ByteSize;
    alignment: Alignment;
    type: M3CG.Type;
    <*UNUSED*>typeid: TypeUID;
    <*UNUSED*>exported: BOOLEAN;
    <*UNUSED*>inited: BOOLEAN): M3CG.Var =
BEGIN
    RETURN self.Declare(type, byte_size, alignment);
END GetStructSizes_declare_constant;

PROCEDURE GetStructSizes_declare_local_or_param(
    self: GetStructSizes_t;
    <*UNUSED*>name: Name;
    byte_size: ByteSize;
    alignment: Alignment;
    type: M3CG.Type;
    <*UNUSED*>typeid: TypeUID;
    <*UNUSED*>in_memory: BOOLEAN;
    <*UNUSED*>up_level: BOOLEAN;
    <*UNUSED*>frequency: Frequency): M3CG.Var =
BEGIN
    RETURN self.Declare(type, byte_size, alignment);
END GetStructSizes_declare_local_or_param;

PROCEDURE Struct(size: INTEGER): TEXT =
BEGIN
    <* ASSERT size >= 0 *>
    size := MAX(size, 1);
    RETURN "M3STRUCT(" & IntToDec(size) & ")";
END Struct;

PROCEDURE Param_Type(var: Var_t): TEXT =
BEGIN
    IF var.type_text # NIL THEN RETURN var.type_text; END;
    IF TRUE THEN
        RETURN typeToText[var.m3cgtype];
    ELSE
        IF var.m3cgtype # M3CG.Type.Struct THEN
            RETURN typeToText[var.m3cgtype];
        END;
        RETURN Struct(var.byte_size) & "*";
    END;
END Param_Type;

PROCEDURE Var_Type(var: Var_t): TEXT =
BEGIN
    IF var.type_text # NIL THEN
        RETURN var.type_text;
    END;
    IF var.m3cgtype # M3CG.Type.Struct THEN
        RETURN typeToText[var.m3cgtype];
    END;
    RETURN Struct(var.byte_size);
END Var_Type;

PROCEDURE Param_Name(var: Var_t): TEXT =
BEGIN
    RETURN ARRAY BOOLEAN OF TEXT{"","_param_struct_pointer_"}[var.m3cgtype = M3CG.Type.Struct] & M3ID.ToText(var.name);
END Param_Name;

PROCEDURE Var_Name(var: Var_t): TEXT =
BEGIN
    RETURN M3ID.ToText(var.name);
END Var_Name;

PROCEDURE Var_Declare(var: Var_t): TEXT =
BEGIN
    RETURN ARRAY BOOLEAN OF TEXT{"", "static "}[var.global AND NOT var.exported] & var.Type() & " " & var.Name();
END Var_Declare;

PROCEDURE Var_InFrameDeclare(var: Var_t): TEXT =
BEGIN
    RETURN ARRAY BOOLEAN OF TEXT{"", "static "}[var.global AND NOT var.exported] & var.InFrameType() & " " & var.InFrameName();
END Var_InFrameDeclare;

PROCEDURE internal_declare_local(
(* returns derived Var_t instead of base M3CG.Var *)
    self: T;
    name: Name;
    byte_size: ByteSize;
    <*UNUSED*>alignment: Alignment;
    type: M3CG.Type;
    <*UNUSED*>typeid: TypeUID;
    in_memory: BOOLEAN;
    up_level: BOOLEAN;
    <*UNUSED*>frequency: Frequency): Var_t =
VAR var := NEW(Var_t, self := self, m3cgtype := type, name := name, up_level := up_level,
               in_memory := in_memory, byte_size := byte_size, proc := self.current_proc).Init();
BEGIN
    IF self.debug > 1 THEN
        self.comment("declare_local " & M3ID.ToText(var.name));
    ELSE
        self.comment("declare_local");
    END;
    <* ASSERT self.current_proc # NIL *>
    <* ASSERT self.current_proc.locals # NIL *>
    IF up_level THEN
        self.current_proc.uplevels := TRUE;
    END;
    self.current_proc.locals.addhi(var);
    RETURN var;
END internal_declare_local;

PROCEDURE declare_local(
(* returns derived Var_t instead of base M3CG.Var *)
    self: T;
    name: Name;
    byte_size: ByteSize;
    alignment: Alignment;
    type: M3CG.Type;
    typeid: TypeUID;
    in_memory: BOOLEAN;
    up_level: BOOLEAN;
    frequency: Frequency): Var_t =
BEGIN
    RETURN internal_declare_local(self, name, byte_size, alignment, type, typeid, in_memory, up_level, frequency);
END declare_local;

TYPE FunctionPrototype_t = { Declare, Define };

PROCEDURE function_prototype(proc: Proc_t; kind: FunctionPrototype_t): TEXT =
VAR params := proc.params;
    text := typeToText[proc.return_type] & "\n__cdecl\n" & M3ID.ToText(proc.name);
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
        text := text & "(\n  ";
        FOR i := FIRST(params^) TO LAST(params^) DO
            WITH param = params[i] DO
                IF i # LAST(params^) THEN
                    after_param := ",\n  ";
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

PROCEDURE no_return(self: T) =
(* note that a no_return function has been seen *)
CONST text = ARRAY OF TEXT {
(* see Darwin /usr/include/stdlib.h abort exit /usr/include/sys/cdefs.h __dead2 *)
"\n#if __GNUC__ > 2 || __GNUC__ == 2 && __GNUC_MINOR__ >= 5",
"#define M3_ATTRIBUTE_NO_RETURN __attribute__((__noreturn__))",
"#else",
"#define M3_ATTRIBUTE_NO_RETURN",
"#endif"
};
BEGIN
    IF NOT self.no_return THEN
        self.no_return := TRUE;
        FOR i := FIRST(text) TO LAST(text) DO
            print(self, text[i] & "\n");
        END;
    END;
END no_return;

PROCEDURE last_param(self: T) =
VAR proc := self.param_proc;
    prototype: TEXT;
    param: Var_t;
BEGIN
    IF proc.no_return THEN
        no_return(self);
    END;
    IF proc.add_static_link THEN
        param := NARROW(internal_declare_param(
            self,
            self.static_link_id,
            CG_Bytes[M3CG.Type.Addr], (* size *)
            CG_Bytes[M3CG.Type.Addr], (* alignment *)
            M3CG.Type.Addr,
            UID_ADDR,
            FALSE(*?*), (* in memory *)
            TRUE, (* up_level, sort of -- needs to be stored, but is never written, can be read from direct parameter
                     This gets it stored in begin_function. *)
            M3CG.Never,
            NARROW(proc.parent, Proc_t).FrameType() & "*"), Var_t);
        param.used := TRUE;
    END;

    prototype := function_prototype(proc, FunctionPrototype_t.Declare) & ARRAY BOOLEAN OF TEXT{";\n", " M3_ATTRIBUTE_NO_RETURN;\n"}[proc.no_return];
    <* ASSERT NOT self.in_proc *>
    print(self, prototype);
    self.param_proc := NIL;
END last_param;

PROCEDURE internal_declare_param(
    self: T;
    name: Name;
    byte_size: ByteSize;
    <*UNUSED*>alignment: Alignment;
    type: M3CG.Type;
    <*UNUSED*>typeid: TypeUID;
    in_memory: BOOLEAN;
    up_level: BOOLEAN;
    <*UNUSED*>frequency: Frequency;
    type_text: TEXT): M3CG.Var =
VAR function := self.param_proc;
    var := NEW(Param_t, self := self, m3cgtype := type, name := name, byte_size := byte_size,
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

PROCEDURE
declare_param(
    self: T;
    name: Name;
    byte_size: ByteSize;
    alignment: Alignment;
    type: M3CG.Type;
    typeid: TypeUID;
    in_memory: BOOLEAN;
    up_level: BOOLEAN;
    frequency: Frequency): M3CG.Var =
BEGIN
    IF self.param_proc = NIL THEN
        RETURN NIL;
    END;
    RETURN internal_declare_param(self, name, byte_size, alignment, type, typeid, in_memory, up_level, frequency, NIL);
END declare_param;

PROCEDURE
internal_declare_temp(
(* returns derived Var_t instead of base M3CG.Var *)
    self: T;
    byte_size: ByteSize;
    alignment: Alignment;
    type: M3CG.Type;
    in_memory:BOOLEAN): Var_t =
BEGIN
    self.comment("declare_temp");
    RETURN declare_local(self, 0, byte_size, alignment, type, -1, in_memory, FALSE, M3CG.Always);
END internal_declare_temp;

PROCEDURE declare_temp(self: T; byte_size: ByteSize; alignment: Alignment; type: M3CG.Type; in_memory:BOOLEAN): M3CG.Var =
BEGIN
    RETURN internal_declare_temp(self, byte_size, alignment, type, in_memory);
END declare_temp;

PROCEDURE Locals_declare_temp(self: Locals_t; byte_size: ByteSize; alignment: Alignment; type: M3CG.Type; in_memory:BOOLEAN): M3CG.Var =
BEGIN
    RETURN internal_declare_temp(self.self, byte_size, alignment, type, in_memory);
END Locals_declare_temp;

PROCEDURE free_temp(self: T; <*NOWARN*>v: M3CG.Var) =
BEGIN
    self.comment("free_temp");
END free_temp;

(*---------------------------------------- static variable initialization ---*)

PROCEDURE begin_init(self: T; <*UNUSED*>v: M3CG.Var) =
BEGIN
    self.comment("begin_init");
    self.current_init_offset := 0;
    self.initializer_comma := "";
    SuppressLineDirective(self, 1, "begin_init");
END begin_init;

PROCEDURE Segments_begin_init(self: Segments_t; v: M3CG.Var) =
BEGIN
    begin_init(self.self, v);
END Segments_begin_init;

CONST CONST_TEXT_LEFT_BRACE = "{";

PROCEDURE initializer_addhi(self: T; text: TEXT) =
VAR initializer := self.initializer;
BEGIN
    initializer.addhi(self.initializer_comma);
    initializer.addhi(text);
    IF text = CONST_TEXT_LEFT_BRACE THEN
        self.initializer_comma := "";
    ELSE
        self.initializer_comma := ",";
    END;
END initializer_addhi;

PROCEDURE end_init(self: T; v: M3CG.Var) =
VAR var := NARROW(v, Var_t);
    init_fields := self.init_fields;
    initializer := self.initializer;
    var_name := M3ID.ToText(var.name);
    const := ARRAY BOOLEAN OF TEXT{"", " const "}[var.const];
BEGIN
    self.comment("end_init");
    init_to_offset(self, var.byte_size);
    end_init_helper(self);

    print(self, "struct " & var_name & "_t{");
    WHILE init_fields.size() > 0 DO
        print(self, init_fields.remlo());
    END;
    print(self, "};\n");

    print(self, "static " & const & var_name & "_t " & var_name & "={");
    WHILE initializer.size() > 0 DO
        print(self, initializer.remlo());
    END;
    print(self, "};\n");

    IF NOT var.const AND self.report_fault_used THEN (* See M3x86.m3 *)
        <* ASSERT self.no_return *>
        no_return(self);
        self.report_fault := M3ID.ToText(var.name) & "_CRASH";
        IF NOT self.RTHooks_ReportFault_imported_or_declared THEN
            print(self, "void __cdecl RTHooks__ReportFault(ADDRESS, WORD_T) M3_ATTRIBUTE_NO_RETURN;\n");
        END;
        print(self, "static void __cdecl " & self.report_fault & "(WORD_T code) M3_ATTRIBUTE_NO_RETURN;\n");
        print(self, "static void __cdecl " & self.report_fault & "(WORD_T code){RTHooks__ReportFault((ADDRESS)&" & M3ID.ToText(var.name) & ",code);}");
    END;

    SuppressLineDirective(self, -1, "end_init");
END end_init;

PROCEDURE Segments_end_init(self: Segments_t; v: M3CG.Var) =
BEGIN
    end_init(self.self, v);
END Segments_end_init;

PROCEDURE init_to_offset(self: T; offset: ByteOffset) =
VAR pad := offset - self.current_init_offset;
BEGIN
    (* self.comment("init_to_offset offset=", IntToDec(offset)); *)
    <* ASSERT offset >= self.current_init_offset *>
    <* ASSERT pad >= 0 *>
    <* ASSERT self.current_init_offset >= 0 *>
    IF pad > 0 THEN
        end_init_helper(self);
        self.init_fields.addhi("char " & M3ID.ToText(GenerateName(self)) & "[" & IntToDec(pad) & "];\n");
        initializer_addhi(self, CONST_TEXT_LEFT_BRACE);
        FOR i := 1 TO pad DO
            initializer_addhi(self, "0 /* " & Fmt.Int(i) & " */ ");
        END;
        initializer_addhi(self, "}");
    END;
END init_to_offset;

PROCEDURE end_init_helper(self: T) =
BEGIN
    IF self.init_type_count > 0 THEN
        self.init_fields.addhi("[" & IntToDec(self.init_type_count) & "];\n");
        self.initializer.addhi("}");
    END;
    self.init_type_count := 0;
END end_init_helper;

PROCEDURE init_helper(self: T; offset: ByteOffset; type: M3CG.Type) =
BEGIN
    init_to_offset(self, offset);
    IF offset = 0 OR self.init_type # type OR offset # self.current_init_offset THEN
        end_init_helper(self);
        self.init_fields.addhi(typeToText[type] & " " & M3ID.ToText(GenerateName(self)));
        initializer_addhi(self, CONST_TEXT_LEFT_BRACE);
    END;
    INC(self.init_type_count);
    self.init_type := type;
    self.current_init_offset := offset + TargetMap.CG_Bytes[type];
END init_helper;

PROCEDURE init_int(self: T; offset: ByteOffset; READONLY value: Target.Int; type: M3CG.Type) =
BEGIN
    self.comment("init_int");
    init_helper(self, offset, type);
    (* TIntLiteral includes suffixes like T, ULL, UI64, etc. *)
    initializer_addhi(self, TIntLiteral(type, value));
END init_int;

PROCEDURE Segments_init_int(self: Segments_t; offset: ByteOffset; READONLY value: Target.Int; type: M3CG.Type) =
BEGIN
    init_int(self.self, offset, value, type);
END Segments_init_int;

PROCEDURE init_proc(self: T; offset: ByteOffset; p: M3CG.Proc) =
VAR proc := NARROW(p, Proc_t);
BEGIN
    self.comment("init_proc");
    init_helper(self, offset, M3CG.Type.Addr); (* FUTURE: better typing *)
    initializer_addhi(self, "(ADDRESS)&" & M3ID.ToText(proc.name));
END init_proc;

PROCEDURE Segments_init_proc(self: Segments_t; offset: ByteOffset; p: M3CG.Proc) =
BEGIN
    init_proc(self.self, offset, p);
END Segments_init_proc;

PROCEDURE init_label(self: T; <*UNUSED*>offset: ByteOffset; <*UNUSED*>value: Label) =
BEGIN
    self.comment("init_label");
    <* ASSERT FALSE *>
END init_label;

PROCEDURE Segments_init_label(self: Segments_t; offset: ByteOffset; value: Label) =
BEGIN
    init_label(self.self, offset, value);
    <* ASSERT FALSE *>
END Segments_init_label;

PROCEDURE init_var(self: T; offset: ByteOffset; v: M3CG.Var; bias: ByteOffset) =
VAR var := NARROW(v, Var_t);
    bias_text := "";
BEGIN
    self.comment("init_var");
    init_helper(self, offset, M3CG.Type.Addr); (* FUTURE: better typing *)
    IF bias # 0 THEN
        bias_text := IntToDec(bias) & "+";
    END;
    initializer_addhi(self, bias_text & "(ADDRESS)&" & M3ID.ToText(var.name));
END init_var;

PROCEDURE Segments_init_var(self: Segments_t; offset: ByteOffset; v: M3CG.Var; bias: ByteOffset) =
BEGIN
    init_var(self.self, offset, v, bias);
END Segments_init_var;

PROCEDURE init_offset(self: T; <*UNUSED*>offset: ByteOffset; <*UNUSED*>value: M3CG.Var) =
BEGIN
    self.comment("init_offset");
    <* ASSERT FALSE *>
END init_offset;

PROCEDURE Segments_init_offset(self: Segments_t; offset: ByteOffset; value: M3CG.Var) =
BEGIN
    init_offset(self.self, offset, value);
END Segments_init_offset;

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
        init_helper(self, offset + i, M3CG.Type.Word8);
        ch := Text.GetChar(value, i);
        IF ch IN Printable THEN
            initializer_addhi(self, "'" & Text.Sub(value, i, 1) & "'");
        ELSE
            initializer_addhi(self, IntToDec(ORD(ch)));
        END;
    END;
END init_chars;

PROCEDURE Segments_init_chars(self: Segments_t; offset: ByteOffset; value: TEXT) =
BEGIN
    init_chars(self.self, offset, value);
END Segments_init_chars;

PROCEDURE TIntToExpr(self: T; READONLY i: Target.Int): Expr_t =
VAR e := NEW(Expr_ConstantInt_t,
             expr_type := ExprType.ConstantInt,
             self := self,
             m3cgtype := Target.Integer.cg_type);
BEGIN
    e.minmax[Min] := i;
    e.minmax[Max] := i;
    e.minmax_valid := minMaxTrue;
    RETURN e;
END TIntToExpr;

PROCEDURE IntToExpr(self: T; i: INTEGER): Expr_t =
BEGIN
    RETURN TIntToExpr(self, IntToTInt(self, i));
END IntToExpr;

PROCEDURE TIntLiteral(type: M3CG.Type; READONLY i: Target.Int): TEXT =
VAR ok1 := TRUE;
    ok2 := TRUE;
BEGIN
    CASE type OF
        | M3CG.Type.Int8   => ok1 := TInt.GE(i, TInt.Min8 );
                              (*ok2 := TInt.LE(i, TInt.Max8);*)
        | M3CG.Type.Int16  => ok1 := TInt.GE(i, TInt.Min16);
                              (*ok2 := TInt.LE(i, TInt.Max16);*)
        | M3CG.Type.Int32  => ok1 := TInt.GE(i, TInt.Min32);
                              (*ok2 := TInt.LE(i, TInt.Max32);*)
        | M3CG.Type.Int64  => ok1 := TInt.GE(i, TInt.Min64);
                              ok2 := TInt.LE(i, TInt.Max64);
        | M3CG.Type.Word8  => ok1 := TWord.LE(i, TWord.Max8);
        | M3CG.Type.Word16 => ok1 := TWord.LE(i, TWord.Max16);
        | M3CG.Type.Word32 => ok1 := TWord.LE(i, TWord.Max32);
        | M3CG.Type.Word64 => ok1 := TWord.LE(i, TWord.Max64);
        ELSE
            RTIO.PutText("TIntLiteral:invalid type=" & typeToText[type] & "\n");
            RTIO.Flush();
            <* ASSERT FALSE *>
    END;
    IF NOT ok1 OR NOT ok2 THEN
        RTIO.PutText("TIntLiteral:type=" & typeToText[type]
                     & " i=" & TInt.ToText(i)
                     & " ok1=" & BoolToText[ok1]
                     & " ok2=" & BoolToText[ok2] & "\n"
                     );
        RTIO.Flush();
        <* ASSERT FALSE *>
    END;
    IF type = M3CG.Type.Int32 AND TInt.EQ(i, TInt.Min32) THEN
        RETURN "-" & intLiteralPrefix[type] & TInt.ToText(TInt.Max32) & intLiteralSuffix[type] & "-1";
    ELSIF type = M3CG.Type.Int64 AND TInt.EQ(i, TInt.Min64) THEN
        RETURN "-" & intLiteralPrefix[type] & TInt.ToText(TInt.Max64) & intLiteralSuffix[type] & "-1";
    ELSE
        RETURN intLiteralPrefix[type] & TInt.ToText(i) & intLiteralSuffix[type];
    END;
END TIntLiteral;

PROCEDURE IntLiteral(self: T; type: M3CG.Type; i: INTEGER): TEXT =
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
            (* suffix := 'L'; if actually using long double *)
            suffix := '\000';
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
    initializer_addhi(self, FloatLiteral(float));
END init_float;

PROCEDURE Segments_init_float(self: Segments_t; offset: ByteOffset; READONLY float: Target.Float) =
BEGIN
    init_float(self.self, offset, float);
END Segments_init_float;

(*------------------------------------------------------------ PROCEDUREs ---*)

PROCEDURE import_procedure(self: T; name: Name; n_params: INTEGER;
                           return_type: M3CG.Type; callingConvention: CallingConvention): M3CG.Proc =
VAR proc := NEW(Proc_t, name := name, n_params := n_params,
                return_type := return_type, imported := TRUE,
                callingConvention := callingConvention).Init(self);
BEGIN
    self.comment("import_procedure");
    IF name = self.RTHooks_ReportFault_id THEN
        self.RTHooks_ReportFault_imported_or_declared := TRUE;
        no_return(self);
    END;
    SuppressLineDirective(self, n_params, "import_procedure n_params");
    self.param_proc := proc;
    self.param_count := 0;
    IF n_params = 0 THEN
        last_param(self);
    END;
    RETURN proc;
END import_procedure;

PROCEDURE Locals_declare_procedure(
    self: Locals_t;
    name: Name;
    n_params: INTEGER;
    return_type: M3CG.Type;
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
END Locals_declare_procedure;

PROCEDURE declare_procedure(self: T; name: Name; n_params: INTEGER;
                            return_type: M3CG.Type; level: INTEGER;
                            callingConvention: CallingConvention;
                            exported: BOOLEAN; parent: M3CG.Proc): M3CG.Proc =
VAR proc := NEW(Proc_t, name := name, n_params := n_params,
                return_type := return_type, level := level,
                callingConvention := callingConvention, exported := exported,
                parent := parent).Init(self);
BEGIN
    self.comment("declare_procedure");
    IF name = self.RTHooks_ReportFault_id THEN
        self.RTHooks_ReportFault_imported_or_declared := TRUE;
    END;
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

PROCEDURE Locals_begin_procedure(self: Locals_t; p: M3CG.Proc) =
BEGIN
    internal_begin_procedure(self.self, p);
END Locals_begin_procedure;

PROCEDURE Locals_end_procedure(self: Locals_t; p: M3CG.Proc) =
BEGIN
    internal_end_procedure(self.self, p);
END Locals_end_procedure;

PROCEDURE Locals_begin_block(self: Locals_t) =
VAR proc := self.self.current_proc;
    (*block := NEW(Block_t, parent_block := proc.current_block);*)
    block: Block_t;
BEGIN
    proc.blocks.addhi(block);
    proc.block_stack.addhi(block);
    proc.current_block := block;
END Locals_begin_block;

PROCEDURE Locals_end_block(self: Locals_t) =
VAR proc := self.self.current_proc;
BEGIN
    EVAL proc.block_stack.remhi();
    IF proc.block_stack.size() > 0 THEN
        proc.current_block := NARROW(proc.block_stack.gethi(), Block_t);
    ELSE
        proc.current_block := NIL;
    END;
END Locals_end_block;

PROCEDURE internal_begin_procedure(self: T; p: M3CG.Proc) =
VAR proc := NARROW(p, Proc_t);
BEGIN
    self.comment("internal_begin_procedure");
    IF self.in_proc THEN (* TODO don't require this *)
        self.Err ("internal_begin_procedure: in_proc; C backend requires "
            & "M3_FRONT_FLAGS = [\"-unfold_nested_procs\"] in config file");
    END;
    self.in_proc := TRUE;
    self.current_proc := proc;
    self.begin_block();
    <* ASSERT proc # NIL *>
    <* ASSERT p # NIL *>
    <* ASSERT self.current_proc = proc *>
END internal_begin_procedure;

PROCEDURE internal_end_procedure(self: T; p: M3CG.Proc) =
BEGIN
    self.comment("internal_end_procedure");
    <* ASSERT self.in_proc *>
    <* ASSERT self.current_proc = p *>
    self.end_block();
    self.in_proc := FALSE;
    self.current_proc := NIL;
END internal_end_procedure;

PROCEDURE begin_procedure(self: T; p: M3CG.Proc) =
VAR proc := NARROW(p, Proc_t);
    frame_name := proc.FrameName();
    frame_type := proc.FrameType();
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

        print(self, "ADDRESS _unused;\n");

        (* uplevel locals in frame *)

        FOR i := 0 TO proc.Locals_Size() - 1 DO
            WITH var = proc.Locals(i) DO
                IF var.up_level AND var.used THEN
                    print(self, var.InFrameDeclare() & ";\n");
                END;
            END;
        END;

        (* uplevel params in frame *) (* structs? *)

        FOR i := FIRST(params^) TO LAST(params^) DO
            WITH param = params[i] DO
                IF param.up_level AND param.used THEN
                    print(self, param.InFrameDeclare() & ";\n");
                END;
            END;
        END;
        print(self, "};\n");
    END;

    print(self, function_prototype(proc, FunctionPrototype_t.Define) & "\n{\n");

    (* declare and zero non-uplevel locals (including temporaries) *)

    FOR i := 0 TO proc.Locals_Size() - 1 DO
        WITH local = proc.Locals(i) DO
            IF (NOT local.up_level) AND local.used THEN
                IF local.m3cgtype = M3CG.Type.Struct THEN
                    print(self, local.Declare() & ";\n");
                ELSE
                    print(self, local.Declare() & " = 0;\n");
                END;
            END;
        END;
    END;
    
    (* declare and zero non-uplevel struct param values (uplevels are in the frame struct) *)

    FOR i := FIRST(params^) TO LAST(params^) DO
        WITH param = params[i] DO
            IF (NOT param.up_level) AND (param.m3cgtype = M3CG.Type.Struct) AND param.used THEN
                print(self, Struct(param.byte_size) & " " & Var_Name(param) & ";\n");
            END;
        END;
    END;
    
    (* declare frame of uplevels *)

    IF proc.forward_declared_frame_type THEN
        print(self, frame_type & " " & frame_name & ";\n");
    END;

    (* init/capture uplevel parameters and static_link (including struct values) *)

    IF proc.forward_declared_frame_type THEN
        FOR i := FIRST(params^) TO LAST(params^) DO
            WITH param = params[i] DO
                IF param.up_level AND param.used THEN
                    IF param.m3cgtype # M3CG.Type.Struct THEN
                        print(self, frame_name & "." & Var_Name(param) & "=" & Param_Name(param) & ";\n");
                    ELSE
                        print(self, frame_name & "." & Var_Name(param) & "=*(" & Struct(param.byte_size) & "*" & ")(" & Param_Name(param) & ");\n");
                    END;
                END;
            END;
        END;

        (* quash unused warning *) (* TODO cleanup -- only declare if needed *)
        print(self, frame_name & "._unused=(ADDRESS)&" & frame_name & ";\n");
    END;

    (* copy non-uplevel struct params from pointers to local value *)

    FOR i := FIRST(params^) TO LAST(params^) DO
        WITH param = params[i] DO
            IF (NOT param.up_level) AND param.m3cgtype = M3CG.Type.Struct AND param.used THEN
                print(self, Var_Name(param) & "=*(" & Struct(param.byte_size) & "*" & ")(" & Param_Name(param) & ");\n");
            END;
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
VAR min := self.labels_min;
BEGIN
    self.comment("set_label");
    IF self.labels # NIL AND label >= self.labels_min AND label <= self.labels_max AND self.labels[label - min] THEN
        (* semicolon in case we fall off the function here:
           void F() { L: } is not legal but
           void F() { L:; } is, and means what you'd think the first means *)
        print(self, "L" & LabelToHex(label) & ":;\n");
    END;
END set_label;

PROCEDURE jump(self: T; label: Label) =
(* GOTO label *)
BEGIN
    self.comment("jump");
    print(self, "goto L" & LabelToHex(label) & ";\n");
END jump;

TYPE internal_compare_t = RECORD
    op: CompareOp;
    left: Expr_t := NIL;
    right: Expr_t := NIL;
    value_valid := FALSE;
    value := FALSE;
    comment: TEXT := NIL;
    text: TEXT := NIL;
    m3cgtype := CGType.Void;
END;

PROCEDURE remove_comments(text: TEXT): TEXT =
BEGIN
    RETURN TextUtils.Substitute(TextUtils.Substitute(TextUtils.Substitute(text, "/*", ""), "*/", ""), "  ", " ");
END remove_comments;

PROCEDURE if_true(self: T; itype: IType; label: Label; frequency: Frequency) =
(* IF (s0.itype # 0) GOTO label; pop *)
BEGIN
    if_true_or_false(self, itype, label, frequency, TRUE);
END if_true;

PROCEDURE if_false(self: T; itype: IType; label: Label; frequency: Frequency) =
(* IF (s0.itype = 0) GOTO label; pop *)
BEGIN
    if_true_or_false(self, itype, label, frequency, FALSE);
END if_false;

PROCEDURE if_true_or_false(self: T; itype: IType; label: Label; frequency: Frequency; value: BOOLEAN) =
(* IF (s0.itype # 0) GOTO label; pop
   OR IF (s0.itype = 0) GOTO label; pop
*)
VAR s0 := cast(get(self, 0), itype);
BEGIN
    self.comment("if_true_or_false");
    IF AvoidGccTypeRangeWarnings THEN
        load_host_integer(self, itype, 0);
        self.if_compare(itype, ARRAY BOOLEAN OF CompareOp{CompareOp.EQ, CompareOp.NE}[value], label, frequency);
    ELSE
        pop(self);
        print(self, "if(" & ARRAY BOOLEAN OF TEXT{"!", ""}[value] & s0.CText() & ")goto L" & LabelToText(label) & ";\n");
    END;
END if_true_or_false;

PROCEDURE if_compare(self: T; ztype: ZType; op: CompareOp; label: Label; <*UNUSED*>frequency: Frequency) =
(* IF (s1.ztype op s0.ztype) GOTO label; pop(2) *)
VAR s0 := cast(get(self, 0), ztype);
    s1 := cast(get(self, 1), ztype);
BEGIN
    self.comment("if_compare");
    pop(self, 2);
    IF AvoidGccTypeRangeWarnings THEN
        print(self, "if(m3_" & CompareOpName[op] & "_" & typeToText[ztype] & "(" & s1.CText() & "," & s0.CText() & "))goto L" & LabelToText(label) & ";\n");
    ELSE
        print(self, "if(" & s1.CText() & CompareOpC[op] & s0.CText() & ")goto L" & LabelToText(label) & ";\n");
    END;
END if_compare;

PROCEDURE case_jump(self: T; itype: IType; READONLY labels: ARRAY OF Label) =
(* "GOTO labels[s0.itype]; pop" with no range checking on s0.itype *)
VAR s0 := cast(get(self, 0), itype);
BEGIN
    self.comment("case_jump");
    IF CaseDefaultAssertFalse THEN
        print(self, "switch(" & s0.CText() & "){\ndefault:assert(!\"case_jump hit default\");\n");
    ELSE
        print(self, "switch(" & s0.CText() & "){");
    END;
    FOR i := FIRST(labels) TO LAST(labels) DO
        print(self, "case " & IntToDec(i) & ":goto L" & LabelToText(labels[i]) & ";\n");
    END;
    print(self, "}");
    pop(self);
END case_jump;

PROCEDURE exit_proc(self: T; type: M3CG.Type) =
(* Returns s0.type if type is not Void, otherwise returns no value. *)
VAR proc := self.current_proc;
BEGIN
    self.comment("exit_proc");
    <* ASSERT self.in_proc *>
    <* ASSERT proc # NIL *>
    <* ASSERT NOT proc.is_RTException_Raise *>
    IF proc.no_return THEN
        <* ASSERT proc.exit_proc_skipped = 0 *>
        <* ASSERT type = M3CG.Type.Void  *>
        INC(proc.exit_proc_skipped);
        RETURN;
    END;
    IF type = M3CG.Type.Void THEN
        IF NOT proc.is_RTHooks_Raise THEN
            print(self, "return;\n");
        END;
    ELSE
        print(self, "return " & get(self).CText() & ";\n");
        pop(self);
    END;
END exit_proc;

(*------------------------------------------------------------ load/store ---*)

PROCEDURE address_plus_offset(in: TEXT; in_offset: INTEGER): Expr_t =
BEGIN
    IF in_offset # 0 THEN
        RETURN CTextToExpr("((" & IntToDec(in_offset) & ")+(ADDRESS)(" & in & "))");
    ELSE
        RETURN CTextToExpr("(" & in & ")");
    END;
END address_plus_offset;

PROCEDURE Variable(self: T; var: Var_t): Expr_t =
VAR expr := NEW(Expr_Variable_t,
                expr_type := ExprType.Variable,
                var := var,
                m3cgtype := var.m3cgtype,
                typeid := var.typeid,
                current_proc := self.current_proc);
BEGIN
(* TODO: subranges *)
    expr.minmax_valid := minMaxPossiblyValidForType[var.m3cgtype];
    expr.minmax := typeMinMax[var.m3cgtype];
    RETURN expr;
END Variable;

PROCEDURE AddressOf(expr: Expr_t): Expr_t =
BEGIN
    RETURN NEW(Expr_t,
               expr_type := ExprType.AddressOf,
               m3cgtype := M3CG.Type.Addr,
               points_to_m3cgtype := expr.m3cgtype,
               points_to_typeid := expr.typeid,
               left := expr,
               c_unop_text := "(ADDRESS)&");
END AddressOf;

PROCEDURE Deref(expr: Expr_t): Expr_t =
BEGIN
    RETURN
        NEW(Expr_t,
            expr_type := ExprType.Deref,
            m3cgtype := expr.points_to_m3cgtype,
            typeid := expr.points_to_typeid,
            left := expr,
            c_unop_text := "*");
END Deref;

PROCEDURE follow_static_link(current_proc: Proc_t; var: Var_t): TEXT =
VAR current_level := 0;
    var_proc: Proc_t := NIL;
    var_level := 0;
    static_link := "";
BEGIN
    <* ASSERT var # NIL *>
    <* ASSERT current_proc # NIL *>
    current_level := current_proc.level;
    var_proc := var.proc;

    IF var_proc = NIL OR var.up_level = FALSE (*OR var.is_static_link*) THEN
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

PROCEDURE load(self: T; v: M3CG.Var; offset: ByteOffset; in_mtype: MType; out_ztype: ZType) =
(* push; s0.ztype := Mem [ ADR(var) + offset ].mtype; The only allowed (mtype->ztype) conversions
   are {Int,Word}{8,16} -> {Int,Word}{32,64} and {Int,Word}32 -> {Int,Word}64.
   The source type, mtype, determines whether the value is sign-extended or
   zero-extended. *)
VAR var := NARROW(v, Var_t);
    expr := Variable(self, var);
BEGIN
    self.comment("load");
    IF FALSE THEN
        IF NOT in_mtype = var.m3cgtype THEN
            RTIO.PutText("load in_mtype:" & typeToText[in_mtype] & " var.m3cgtype:" & typeToText[var.m3cgtype]);
            RTIO.Flush();
            <* ASSERT in_mtype = var.m3cgtype *>
        END;
    END;
    IF offset # 0 OR var.m3cgtype # in_mtype THEN
        expr := AddressOf(expr);
        IF offset # 0 THEN
            expr := cast(expr, type := M3CG.Type.Addr);
            expr := NEW(Expr_t, right := expr, left := IntToExpr(self, offset), c_binop_text := "+");
        END;
        expr := cast(expr, type_text := "(" & typeToText[in_mtype] & " * )");
        expr := Deref(expr);
    END;
    IF in_mtype # out_ztype THEN
        expr := cast(expr, out_ztype);
    END;
    push(self, out_ztype, expr);
END load;

PROCEDURE store_helper(self: T; in: TEXT; in_ztype: ZType; out_address: TEXT; out_offset: INTEGER; out_mtype: MType) =
BEGIN
    <* ASSERT CG_Bytes[in_ztype] >= CG_Bytes[out_mtype] *>
    print(self, "(*(" & typeToText[out_mtype] & "*)" & address_plus_offset(out_address, out_offset).CText() & ")=(" & typeToText[in_ztype] & ")(" & in & ");\n");
END store_helper;

PROCEDURE store(self: T; v: M3CG.Var; offset: ByteOffset; ztype: ZType; mtype: MType) =
(* Mem [ ADR(var) + offset ].mtype := s0.ztype; pop *)
VAR var := NARROW(v, Var_t);
    s0 := cast(get(self, 0), ztype);
BEGIN
    self.comment("store");
    pop(self);
    store_helper(self, s0.CText(), ztype, "&" & follow_static_link(self.current_proc, var) & M3ID.ToText(var.name), offset, mtype);
END store;

PROCEDURE load_address(self: T; v: M3CG.Var; offset: ByteOffset) =
(* push; s0.A := ADR(var) + offset *)
VAR var := NARROW(v, Var_t);
    expr: Expr_t := NIL;
BEGIN
    self.comment("load_address");
    expr := AddressOf(Variable(self, var));
    IF offset # 0 THEN
        expr := cast(expr, type := M3CG.Type.Addr);
        expr := NEW(Expr_t, right := expr, left := IntToExpr(self, offset), c_binop_text := "+");
    END;
    push(self, M3CG.Type.Addr, expr);
END load_address;

PROCEDURE load_indirect(self: T; offset: ByteOffset; mtype: MType; ztype: ZType) =
(* s0.ztype := Mem [s0.A + offset].mtype  *)
VAR expr := get(self);
BEGIN
    self.comment("load_indirect");
    <* ASSERT CG_Bytes[ztype] >= CG_Bytes[mtype] *>
    pop(self);
    IF offset # 0 THEN
        expr := cast(expr, type := M3CG.Type.Addr); (* might be redundant *)
        expr := NEW(Expr_t, right := expr, left := IntToExpr(self, offset), c_binop_text := "+");
    END;
    expr := CastAndDeref(expr, type_text := "(" & typeToText[mtype] & "*)"); (* cast might be redundant *)
    IF mtype # ztype THEN
        expr := cast(expr, ztype);
    END;
    expr.m3cgtype := ztype;
    push(self, ztype, expr);
END load_indirect;

PROCEDURE store_indirect(self: T; offset: ByteOffset; ztype: ZType; mtype: MType) =
(* Mem [s1.A + offset].mtype := s0.ztype; pop (2) *)
VAR s0 := cast(get(self, 0), ztype);
    s1 := get(self, 1);
BEGIN
    self.comment("store_indirect");
    pop(self, 2);
    store_helper(self, s0.CText(), ztype, s1.CText(), offset, mtype);
END store_indirect;

(*-------------------------------------------------------------- literals ---*)

PROCEDURE load_nil(self: T) =
(* push; s0.A := NIL *)
BEGIN
    self.comment("load_nil");
    push(self, M3CG.Type.Addr, CTextToExpr("0")); (* UNDONE NULL or (ADDRESS)0? *)
END load_nil;

CONST IntToTInt = IntToTarget;

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
    IF self.debug > 1 THEN
        comment(self, "load_host_integer:" & Fmt.Int(i));
    ELSE
        comment(self, "load_host_integer");
    END;
    self.load_integer(type, IntToTarget(self, i));
END load_host_integer;

PROCEDURE load_target_integer(self: T; type: IType; READONLY readonly_i: Target.Int) =
(* push; s0.type := i *)
VAR i := readonly_i;
    expr := CTextToExpr(TIntLiteral(type, i));
    size := typeSizeBytes[type];
    signed   := typeIsSignedInt[type];
    unsigned := typeIsUnsignedInt[type];
BEGIN
    self.comment("load_integer");
    <* ASSERT signed OR unsigned *>
    expr.minmax_valid[Min] := TRUE;
    expr.minmax_valid[Max] := TRUE;
    expr.m3cgtype := type;
    IF typeIsUnsignedInt[type] THEN TIntN.ZeroExtend(i, size)
    ELSIF typeIsSignedInt[type] THEN TIntN.SignExtend(i, size) END;
    <* ASSERT readonly_i = i *>
    expr.minmax[Min] := i;
    expr.minmax[Max] := i;
    expr := cast(expr, type);
    expr.m3cgtype := type;
    (* TIntLiteral includes suffixes like U, ULL, UI64, etc. *)
    push(self, type, expr);
END load_target_integer;

PROCEDURE load_float(self: T; type: RType; READONLY float: Target.Float) =
(* push; s0.type := float *)
BEGIN
    self.comment("load_float");
    (* FloatLiteral includes suffixes like "F" for float, "" for double, "L" for long double *)
    push(self, type, cast(CTextToExpr(FloatLiteral(float)), type));
END load_float;

(*------------------------------------------------------------ arithmetic ---*)

PROCEDURE InternalTransferMinMax2(
    from: Expr_t;
    from_valid: BOOLEAN;
    READONLY from_value: Target.Int;
    to: Expr_t;
    VAR to_valid: BOOLEAN;
    VAR to_value: Target.Int): BOOLEAN =
VAR from_type     := from.m3cgtype;
    from_signed   := typeIsSignedInt[from_type];
    from_unsigned := typeIsUnsignedInt[from_type];
    to_type       := to.m3cgtype;
    to_signed     := typeIsSignedInt[to_type];
    to_unsigned   := typeIsUnsignedInt[to_type];
BEGIN
    RETURN FALSE;

    <* ASSERT NOT (from_signed AND from_unsigned) *>
    <* ASSERT NOT (to_signed AND to_unsigned) *>
    <* ASSERT to_signed OR to_unsigned *>
    to_valid := from_valid;
    IF NOT from_valid THEN
        to_value := from_value;
        RETURN TRUE; (* micro optimize; FALSE makes more sense but TRUE is ok *)
    END;
    <* ASSERT from_signed OR from_unsigned *>
    IF NOT TIntExtendOrTruncate(from_value, to_type, to_value) THEN
        RETURN FALSE;
    END;
    IF from_unsigned AND to_signed THEN
        RETURN TRUE;
    END;
END InternalTransferMinMax2;

PROCEDURE InternalTransferMinMax1(from, to: Expr_t; minOrMax: MinOrMax): BOOLEAN =
BEGIN
    RETURN FALSE;
    RETURN InternalTransferMinMax2(
        from,
        from.minmax_valid[minOrMax],
        from.minmax[minOrMax],
        to,
        to.minmax_valid[minOrMax],
        to.minmax[minOrMax]);
END InternalTransferMinMax1;

PROCEDURE TransferMinMax(from, to: Expr_t) =
BEGIN
    to.minmax_valid := minMaxFalse;
    RETURN;
    IF NOT typeIsInteger[to.m3cgtype] THEN
        to.minmax_valid := minMaxFalse;
        RETURN;
    END;
    IF from.m3cgtype = to.m3cgtype THEN
        to.minmax := from.minmax;
        to.minmax_valid := from.minmax_valid;
        RETURN;
    END;
    IF NOT InternalTransferMinMax1(from, to, Min)
            OR NOT InternalTransferMinMax1(from, to, Max) THEN
        (* punt and extend range arbitrarily; this could be better *)
        to.minmax := typeMinMax[to.m3cgtype];
    END;
END TransferMinMax;

PROCEDURE cast(expr: Expr_t; type: M3CG.Type := M3CG.Type.Void; type_text: TEXT := NIL): Expr_t =
VAR e := NEW(Expr_Cast_t, m3cgtype := type, type_text := type_text, left := expr);
BEGIN
    <* ASSERT (type = M3CG.Type.Void) # (type_text = NIL) *>
    (* casts are either truncating or sign extending or zero extending *)
    TransferMinMax(expr, e);
    RETURN e;
END cast;

PROCEDURE old_Cast(expr: Expr_t; type: M3CG.Type := M3CG.Type.Void; type_text: TEXT := NIL): Expr_t =
BEGIN
    <* ASSERT (type = M3CG.Type.Void) # (type_text = NIL) *>
    RETURN NEW(Expr_Cast_t, m3cgtype := type, type_text := type_text, left := expr);
END old_Cast;

PROCEDURE CastAndDeref(expr: Expr_t; type: M3CG.Type := M3CG.Type.Void; type_text: TEXT := NIL): Expr_t =
BEGIN
    RETURN Deref(cast(expr, type, type_text));
END CastAndDeref;

PROCEDURE op1(self: T; type: M3CG.Type; name, op: TEXT) =
(* unary operation *)
VAR s0 := cast(get(self, 0), type);
BEGIN
    self.comment(name);
    pop(self, 1);
    push(self, type, cast(NEW(Expr_t, left := s0, c_unop_text := op), type));
END op1;

TYPE TIntOp2_t = PROCEDURE (READONLY a, b: Target.Int; VAR i: Target.Int): BOOLEAN;
TYPE TFloatOp2_t = PROCEDURE (READONLY a, b: Target.Float; VAR f: Target.Float): BOOLEAN;

TYPE TIntExtendOrTruncate_t = PROCEDURE (READONLY in: Target.Int; byte_size: CARDINAL; VAR out: Target.Int): BOOLEAN;

PROCEDURE TIntExtendOrTruncate(READONLY in: Target.Int; type: CGType; VAR out: Target.Int): BOOLEAN =
VAR size := typeSizeBytes[type];
    signed   := typeIsSignedInt[type];
    unsigned := typeIsUnsignedInt[type];
BEGIN
    <* ASSERT signed OR unsigned *>
    RETURN ((ARRAY BOOLEAN OF TIntExtendOrTruncate_t{TWord.Truncate, TInt.Extend})[signed])(in, size, out);
END TIntExtendOrTruncate;

PROCEDURE
op2(
    self: T; 
    type: M3CG.Type;
    name: TEXT;
    op: TEXT;
    <*UNUSED*>intOp: TIntOp2_t := TInt.Add;
    <*UNUSED*>floatOp: TFloatOp2_t := TFloat.Add
    ) =
(* binary operation *)
VAR s0 := cast(get(self, 0), type);
    s1 := cast(get(self, 1), type);
    expr: Expr_t;
BEGIN
    self.comment(name);
    pop(self, 2);
    expr := NEW(Expr_t, left := s1, right := s0, c_binop_text := op);
    expr := cast(expr, type);
    expr.minmax_valid := minMaxPossiblyValidForType[type];
    expr.minmax := typeMinMax[type];
    push(self, type, expr);
END op2;

PROCEDURE compare(self: T; ztype: ZType; itype: IType; op: CompareOp) =
(* s1.itype := (s1.ztype op s0.ztype); pop *)
VAR s0 := get(self, 0);
    s1 := get(self, 1);
BEGIN
    self.comment("compare");
    pop(self, 2);
    IF AvoidGccTypeRangeWarnings THEN
        push(self, itype, cast(CTextToExpr("m3_" & CompareOpName[op] & "_" & typeToText[ztype] & "(" & s1.CText() & "," & s0.CText() & ")"), itype));
    ELSE
        push(self, itype, cast(CTextToExpr(s1.CText() & CompareOpC[op] & s0.CText()), itype));
    END;
END compare;

PROCEDURE add(self: T; type: AType) =
(* s1.type := s1.type + s0.type; pop *)
BEGIN
    op2(self, type, "add", "+", TInt.Add, TFloat.Add);
END add;

PROCEDURE subtract(self: T; type: AType) =
(* s1.type := s1.type - s0.type; pop *)
BEGIN
    op2(self, type, "subtract", "-", TInt.Subtract, TFloat.Subtract);
END subtract;

PROCEDURE multiply(self: T; type: AType) =
(* s1.type := s1.type * s0.type; pop *)
BEGIN
    op2(self, type, "multiply", "*", TInt.Multiply, TFloat.Multiply);
END multiply;

PROCEDURE divide(self: T; type: RType) =
(* s1.type := s1.type / s0.type; pop *)
BEGIN
    op2(self, type, "divide", "/");
END divide;

PROCEDURE div(self: T; type: IType; a, b: Sign) =
(* s1.type := s1.type DIV s0.type; pop *)
VAR s0 := cast(get(self, 0), type);
    s1 := cast(get(self, 1), type);
BEGIN
    self.comment("div");
    pop(self, 2);
    IF ((a = b) AND (a # Sign.Unknown)) OR typeIsUnsignedInt[type] THEN
        push(self, type, cast(CTextToExpr(s1.CText() & "/" & s0.CText()), type));
    ELSE
        push(self, type, cast(CTextToExpr("m3_div_" & typeToText[type] & "(" & s1.CText() & "," & s0.CText() & ")"), type));
    END;
END div;

PROCEDURE mod(self: T; type: IType; a, b: Sign) =
(* s1.type := s1.type MOD s0.type; pop *)
VAR s0 := cast(get(self, 0), type);
    s1 := cast(get(self, 1), type);
BEGIN
    self.comment("mod");
    pop(self, 2);
    IF ((a = b) AND (a # Sign.Unknown)) OR typeIsUnsignedInt[type] THEN
        push(self, type, cast(CTextToExpr(s1.CText() & "%" & s0.CText()), type));
    ELSE
        push(self, type, cast(CTextToExpr("m3_mod_" & typeToText[type] & "(" & s1.CText() & "," & s0.CText() & ")"), type));
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
    push(self, type, CTextToExpr("m3_abs_" & typeToText[type] & "(" & s0.CText() & ")"));
END abs;

PROCEDURE max(self: T; type: ZType) =
(* s1.type := MAX (s1.type, s0.type); pop *)
VAR s0 := cast(get(self, 0), type);
    s1 := cast(get(self, 1), type);
BEGIN
    self.comment("max");
    pop(self, 2);
    push(self, type, CTextToExpr("m3_max_" & typeToText[type] & "(" & s0.CText() & "," & s1.CText() & ")"));
END max;

PROCEDURE min(self: T; type: ZType) =
(* s1.type := MIN (s1.type, s0.type); pop *)
VAR s0 := cast(get(self, 0), type);
    s1 := cast(get(self, 1), type);
BEGIN
    self.comment("min");
    pop(self, 2);
    push(self, type, CTextToExpr("m3_min_" & typeToText[type] & "(" & s0.CText() & "," & s1.CText() & ")"));
END min;

PROCEDURE cvt_int(self: T; from_float_type: RType; to_integer_type: IType; op: ConvertOp) =
(* s0.itype := ROUND(s0.rtype) *)
VAR s0 := cast(get(self, 0), from_float_type);
BEGIN
    self.comment("cvt_int");
    pop(self);
    push(self, to_integer_type, cast(CTextToExpr("m3_" & ConvertOpName[op] & "(" & s0.CText() & ")"), to_integer_type));
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
VAR s0 := cast(get(self, 0), M3CG.Type.Addr);
    s1 := cast(get(self, 1), M3CG.Type.Addr);
    s2 := cast(get(self, 2), M3CG.Type.Addr);
    target_word_bytes := Target.Word.bytes;
BEGIN
    self.comment(op);
    <* ASSERT (byte_size MOD target_word_bytes) = 0 *>
    pop(self, 3);
    print(self, "m3_" & op & "(" & IntToDec(byte_size DIV target_word_bytes) & ",(WORD_T*)" & s0.CText() & ",(WORD_T*)" & s1.CText() & ",(WORD_T*)" & s2.CText() & ");\n");
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
    s1 := cast(get(self, 1), M3CG.Type.Void, "(SET)");
BEGIN
    self.comment("set_member");
    pop(self, 2);
    push(self, type, cast(CTextToExpr("m3_set_member(" & s0.CText() & "," & s1.CText() & ")"), type));
END set_member;

PROCEDURE set_compare(self: T; byte_size: ByteSize; op: CompareOp; type: IType) =
(* s1.type := (s1.B op s0.B); pop *)
VAR swap := (op IN SET OF CompareOp{CompareOp.GT, CompareOp.GE});
    s0 := cast(get(self, ORD(swap)), M3CG.Type.Void, "(SET)");
    s1 := cast(get(self, ORD(NOT swap)), M3CG.Type.Void, "(SET)");
    target_word_bytes := Target.Word.bytes;
    eq := ARRAY BOOLEAN OF TEXT{"==0", "!=0"}[op = CompareOp.EQ];
BEGIN
    self.comment("set_compare");
    <* ASSERT (byte_size MOD target_word_bytes) = 0 *>
    pop(self, 2);
    IF swap THEN
        IF op = CompareOp.GE THEN
            op := CompareOp.LE;
        ELSIF op = CompareOp.GT THEN
            op := CompareOp.LT;
        END;
    END;
    IF op IN SET OF CompareOp{CompareOp.LT, CompareOp.LE} THEN
        byte_size := byte_size DIV target_word_bytes;
        push(self, type, cast(CTextToExpr("m3_set_" & CompareOpName[op] & "(" & IntLiteral(self, Target.Word.cg_type, byte_size) & "," & s1.CText() & "," & s0.CText() & ")"), type));
    ELSE
        <* ASSERT op IN SET OF CompareOp{CompareOp.EQ, CompareOp.NE} *>
        push(self, type, cast(CTextToExpr("memcmp(" & s1.CText() & "," & s0.CText() & "," & IntLiteral(self, Target.Word.cg_type, byte_size) & ")" & eq), type));
    END;
END set_compare;

PROCEDURE set_range(self: T; <*UNUSED*>byte_size: ByteSize; type: IType) =
(* s2.A [s1.type .. s0.type] := 1's; pop(3) *)
VAR s0 := cast(get(self, 0), type);
    s1 := cast(get(self, 1), type);
    s2 := cast(get(self, 2), M3CG.Type.Void, "(SET)");
BEGIN
    self.comment("set_range");
    pop(self, 3);
    print(self, "m3_set_range(" & s0.CText() & "," & s1.CText() & "," & s2.CText() & ");\n");
END set_range;

PROCEDURE set_singleton(self: T; <*UNUSED*>byte_size: ByteSize; type: IType) =
(* s1.A [s0.type] := 1; pop(2) *)
VAR s0 := cast(get(self, 0), type);
    s1 := cast(get(self, 1), M3CG.Type.Void, "(SET)");
BEGIN
    self.comment("set_singleton");
    pop(self, 2);
    print(self, "m3_set_singleton(" & s0.CText() & "," & s1.CText() & ");\n");
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
VAR s0 := cast(get(self, 0), type);
    s1 := cast(get(self, 1), type);
BEGIN
    self.comment("xor");
    IF AvoidGccTypeRangeWarnings THEN
        pop(self, 2);
        push(self, type, CTextToExpr("m3_xor_" & typeToText[type] & "(" & s1.CText() & "," & s0.CText() & ")"));
    ELSE
        op2(self, type, "xor", "^");
    END;
END xor;

PROCEDURE shift_left_or_right(self: T; type: IType; name, op: TEXT) =
VAR s0 := cast(get(self, 0), Target.Word.cg_type);
    s1 := cast(get(self, 1), type);
BEGIN
    self.comment(name);
    pop(self, 2);
    push(self, type, CTextToExpr(s1.CText() & op & s0.CText()));
END shift_left_or_right;

PROCEDURE shift_left(self: T; type: IType) =
(* s1.type := Word.Shift  (s1.type, s0.Word); pop *)
BEGIN
    shift_left_or_right(self, type, "shift_left", "<<");
END shift_left;

PROCEDURE shift_right(self: T; type: IType) =
(* s1.type := Word.Shift  (s1.type, -s0.type); pop *)
BEGIN
    <* ASSERT typeIsUnsignedInt[type] *>
    shift_left_or_right(self, type, "shift_right", ">>");
END shift_right;

PROCEDURE shift_or_rotate(self: T; type: IType; which: TEXT; count_type: M3CG.Type) =
VAR s0 := cast(get(self, 0), count_type);
    s1 := cast(get(self, 1), type);
BEGIN
    self.comment(which);
    pop(self, 2);
    push(self, type, CTextToExpr("m3_" & which & "_" & typeToText[type] & "(" & s1.CText() & "," & s0.CText() & ")"));
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
        push(self, type, CTextToExpr("m3_extract(" & typeToText[typeToUnsigned[type]] & "," & value.CText() & "," & offset.CText() & "," & count.CText() & ")"));
    ELSE
        push(self, type, CTextToExpr("m3_extract_" & typeToText[typeToUnsigned[type]] & "(" & value.CText() & "," & offset.CText() & "," & count.CText() & ")"));
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
    push(self, type, CTextToExpr("m3_sign_extend_" & typeToText[type] & "(" & value.CText() & "," & count.CText() & ")"));
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
    push(self, type, CTextToExpr("m3_insert_" & typeToText[type] & "(" & to.CText() & "," & from.CText() & "," & offset.CText() & "," & count.CText() & ")"));
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

PROCEDURE swap(self: T; <*UNUSED*>a, b: M3CG.Type) =
(* tmp := s1; s1 := s0; s0 := tmp *)
VAR temp := get(self, 1);
BEGIN
    self.comment("swap");
    self.stack.put(1, get(self, 0));
    self.stack.put(0, temp);
END swap;

PROCEDURE cg_pop(self: T; type: M3CG.Type) =
(* pop(1) (i.e. discard s0) *)
VAR s0 := cast(get(self, 0), type);
BEGIN
    self.comment("pop");
    pop(self);
    print(self, "m3_pop_" & typeToText[type] & "(" & s0.CText() & ");\n");
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
    print(self, MemCopyOrMove[ORD(overlap)] & "(" & s2.CText() & "," & s1.CText() & "," & IntToDec(CG_Bytes[mtype]) & "*(size_t)" & s0.CText() & ");\n");
END copy_n;

PROCEDURE copy(self: T; n: INTEGER; mtype: MType; overlap: BOOLEAN) =
(* Mem[s1.A:sz] := Mem[s0.A:sz]; pop(2)*)
VAR s0 := get(self, 0);
    s1 := get(self, 1);
BEGIN
    self.comment("copy");
    pop(self, 2);
    print(self, MemCopyOrMove[ORD(overlap)] & "(" & s1.CText() & "," & s0.CText() & "," & IntToDec(CG_Bytes[mtype] * n) & ");\n");
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
    print(self, "memset(" & s0.CText() & ",0," & IntToDec(n) & "*(size_t)" & IntToDec(CG_Bytes[type]) & ");\n");
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
VAR t := self.temp_vars[self.op_index];
BEGIN
    self.comment("check_nil");
    self.store(t, 0, M3CG.Type.Addr, M3CG.Type.Addr);
    self.load(t, 0, M3CG.Type.Addr, M3CG.Type.Addr);
    print(self, "/*check_nil*/if(!" & get(self).CText() & ")");
    reportfault(self, code);
END check_nil;

PROCEDURE check_lo(self: T; type: IType; READONLY i: Target.Int; code: RuntimeError) =
(* IF (s0.type < i) THEN abort(code) *)
VAR t := self.temp_vars[self.op_index];
BEGIN
    self.comment("check_lo");
    self.store(t, 0, type, type);
    self.load(t, 0, type, type);
    print(self, "/*check_lo*/if(" & get(self).CText() & "<" & TIntLiteral(type, i) & ")");
    reportfault(self, code);
END check_lo;

PROCEDURE check_hi(self: T; type: IType; READONLY i: Target.Int; code: RuntimeError) =
(* IF (i < s0.type) THEN abort(code) *)
VAR t := self.temp_vars[self.op_index];
BEGIN
    self.comment("check_hi");
    self.store(t, 0, type, type);
    self.load(t, 0, type, type);
    print(self, "/*check_hi*/if(" & TIntLiteral(type, i) & "<" & get(self).CText() & ")");
    reportfault(self, code);
END check_hi;

PROCEDURE check_range(self: T; type: IType; READONLY low, high: Target.Int; code: RuntimeError) =
(* IF (s0.type < low) OR (high < s0.type) THEN abort(code) *)
(* TODO capture into temporaries. *)
VAR t := self.temp_vars[self.op_index];
    low_expr := CTextToExpr(TIntLiteral(type, low));
    high_expr := CTextToExpr(TIntLiteral(type, high));
BEGIN
    self.comment("check_range");
    self.store(t, 0, type, type);
    self.load(t, 0, type, type);
    print(self, "if(m3_check_range_" & typeToText[type] & "(" & get(self).CText() & "," & low_expr.CText() & "," & high_expr.CText() & "))");
    reportfault(self, code);
END check_range;

PROCEDURE check_index(self: T; type: IType; code: RuntimeError) =
(* IF NOT (0 <= s1.type < s0.type) THEN
     abort(code)
   END;
   pop
   s0.type is guaranteed to be positive so the unsigned
   check (s0.W <= s1.W) is sufficient. *)
VAR s0: Expr_t;
    s1: Expr_t;
    t := self.temp_vars[self.op_index];
BEGIN
    self.comment("check_index");
    <* ASSERT type = Target.Integer.cg_type *>
    (* ASSERT (NOT s0.is_const) OR TInt.GE(s0.int_value, TInt.Zero) *)
    
    self.swap(type, type);
    self.store(t, 0, type, type);
    self.load(t, 0, type, type);
    self.swap(type, type);
    s0 := cast(get(self, 0), Target.Word.cg_type);
    s1 := cast(get(self, 1), Target.Word.cg_type);
    print(self, "/*check_index*/if(" & s0.CText() & "<=" & s1.CText() & ")");
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
    print(self, "/*check_eq*/if(" & s0.CText() & "!=" & s1.CText() & ")");
    reportfault(self, code);
    pop(self, 2);
END check_eq;

PROCEDURE reportfault(self: T; code: RuntimeError) =
(* 32: see M3CG.RuntimeError, RuntimeError.T *)
VAR info := ORD (code) + self.line * 32;
BEGIN
    <* ASSERT ORD (code) < 32 *> (* lose fault code not ok *)
    (* ASSERT self.line <= (LAST(INTEGER) DIV 32) *) (* losing line number ok *)
    print(self, self.report_fault & "(" & IntToDec(info) & ");\n");
END reportfault;

(*---------------------------------------------------- address arithmetic ---*)

PROCEDURE add_offset(self: T; offset: INTEGER) =
(* s0.A := s0.A + offset *)
VAR s0 := cast(get(self, 0), M3CG.Type.Addr);
BEGIN
    self.comment("add_offset");
    pop(self);
    push(self, M3CG.Type.Addr, address_plus_offset(s0.CText(), offset));
END add_offset;

PROCEDURE index_address(self: T; type: IType; size: INTEGER) =
(* s1.A := s1.A + s0.type * size; pop *)
VAR s0 := cast(get(self, 0), type);
    s1 := cast(get(self, 1), M3CG.Type.Addr);
    size_text := IntToDec(size);
BEGIN
    self.comment("index_address");
    IF size = 0 THEN
        pop(self);
        <* ASSERT FALSE *>
    ELSE
        pop(self, 2);
        push(self, M3CG.Type.Addr, paren(CTextToExpr(s1.CText() & "+" & paren(CTextToExpr(size_text & "*" & s0.CText())).CText())));
    END;
END index_address;

(*------------------------------------------------------- PROCEDURE calls ---*)

PROCEDURE start_call_helper(self: T) =
BEGIN
    self.static_link := NIL;
    <* ASSERT self.params.size() = 0 *>
    <* ASSERT NOT self.in_proc_call *>
    self.in_proc_call := TRUE;
END start_call_helper;

PROCEDURE start_call_direct(self: T; p: M3CG.Proc; <*UNUSED*>level: INTEGER; <*UNUSED*>type: M3CG.Type) =
(* begin a procedure call to a procedure at static level 'level'. *)
VAR proc := NARROW(p, Proc_t);
BEGIN
    self.comment("start_call_direct");
    start_call_helper(self);
    (* workaround frontend bug? *)
    IF proc.is_exception_handler THEN
        push(self, M3CG.Type.Addr, CTextToExpr("0"));
        pop_parameter_helper(self, "0");
    END;
    self.proc_being_called := proc;
END start_call_direct;

PROCEDURE start_call_indirect(self: T; <*UNUSED*>type: M3CG.Type; <*UNUSED*>callingConvention: CallingConvention) =
(* begin a procedure call to a procedure at static level 'level'. *)
BEGIN
    self.comment("start_call_indirect");
    start_call_helper(self);
END start_call_indirect;

PROCEDURE pop_parameter_helper(self: T; param: TEXT) =
BEGIN
    <* ASSERT self.in_proc_call *>
    self.params.addhi(param);
    pop(self);
END pop_parameter_helper;

PROCEDURE pop_param(self: T; type: MType) =
(* pop s0 and make it the "next" parameter in the current call *)
VAR s0 := cast(get(self, 0), type);
BEGIN
    self.comment("pop_param");
    pop_parameter_helper(self, s0.CText());
END pop_param;

PROCEDURE pop_struct(self: T; <*UNUSED*>typeid: TypeUID; byte_size: ByteSize; alignment: Alignment) =
(* pop s0 and make it the "next" parameter in the current call
 * NOTE: it is passed by value *)
VAR s0 := cast(get(self, 0), M3CG.Type.Addr);
BEGIN
    self.comment("pop_struct");
    <* ASSERT (byte_size MOD alignment) = 0 *>
    <* ASSERT byte_size >= 0 *>
    pop_parameter_helper(self, s0.CText());
END pop_struct;

PROCEDURE pop_static_link(self: T) =
VAR var := self.temp_vars[self.op_index];
BEGIN
    self.comment("pop_static_link");
    <* ASSERT self.in_proc_call *>
    self.static_link := var;
    self.store(var, 0, M3CG.Type.Addr, M3CG.Type.Addr);
END pop_static_link;

PROCEDURE Locals_pop_static_link(self: Locals_t) =
VAR x := self.self;
    var := internal_declare_temp(x, CG_Bytes[M3CG.Type.Addr], CG_Bytes[M3CG.Type.Addr], M3CG.Type.Addr, FALSE);
BEGIN
    <* ASSERT x.temp_vars # NIL *>
    x.temp_vars[x.op_index] := var;
    var.used := TRUE;
END Locals_pop_static_link;

PROCEDURE call_helper(self: T; type: M3CG.Type; proc: TEXT) =
VAR comma := "";
BEGIN
    <* ASSERT self.in_proc_call *>
    self.proc_being_called := NIL;
    self.in_proc_call := FALSE;
    proc := proc & "(";
    WHILE self.params.size() > 0 DO
      proc := proc & comma & self.params.remlo();
      comma := ",";
    END;
    proc := proc & ")";
    IF type = M3CG.Type.Void THEN
        print(self, proc & ";\n");
    ELSE
        push(self, type, CTextToExpr(proc));
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

PROCEDURE call_direct(self: T; p: M3CG.Proc; type: M3CG.Type) =
(* call the procedure identified by M3CG.Proc p. The procedure
   returns a value of type type. *)
VAR proc := NARROW(p, Proc_t);
BEGIN
    self.comment("call_direct");

    <* ASSERT self.in_proc_call *>

    IF proc.level # 0 THEN
        self.params.addhi(get_static_link(self, proc));
    END;

    call_helper(self, type, M3ID.ToText(proc.name));
END call_direct;

PROCEDURE call_indirect(self: T; type: M3CG.Type; <*UNUSED*>callingConvention: CallingConvention) =
(* call the procedure whose address is in s0.A and pop s0. The
   procedure returns a value of type type. *)
VAR s0 := get(self, 0);
    static_link := self.static_link;
BEGIN
    self.comment("call_indirect");

    pop(self);

    <* ASSERT self.in_proc_call *>

    IF static_link # NIL THEN
        self.params.addhi(M3ID.ToText(static_link.name));
        free_temp(self, static_link);
        self.static_link := NIL;
    END;

    (* UNDONE: cast to more accurate function type *)
    call_helper(self, type, "((" & typeToText[type] & " (__cdecl*)(M3_DOTDOTDOT))" & s0.CText() & ")");

END call_indirect;

(*------------------------------------------- procedure and closure types ---*)

PROCEDURE load_procedure(self: T; p: M3CG.Proc) =
(* push; s0.A := ADDR (proc's body) *)
VAR proc := NARROW(p, Proc_t);
BEGIN
    self.comment("load_procedure");
    (* UNDONE? typeing? *)
    push(self, M3CG.Type.Addr, CTextToExpr(M3ID.ToText(proc.name)));
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
    push(self, M3CG.Type.Addr, CTextToExpr(get_static_link(self, target)));
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
    a := " /* " & a & b & c & d & " */\n";
    print(self, a);
    RTIO.PutText(a);
    RTIO.Flush();
END comment;

(*--------------------------------------------------------------- atomics ---*)

PROCEDURE store_ordered(self: T; ztype: ZType; mtype: MType; <*UNUSED*>order: MemoryOrder) =
(* Mem [s1.A].mtype := s0.ztype;
   pop (2) *)
VAR s0 := get(self, 0);
    s1 := get(self, 1);
BEGIN
    self.comment("store_ordered => store_helper");
    store_helper(self, s0.CText(), ztype, s1.CText(), 0, mtype);
END store_ordered;

PROCEDURE load_ordered(self: T; mtype: MType; ztype: ZType; <*UNUSED*>order: MemoryOrder) =
(* s0.ztype := Mem [s0.A].mtype  *)
BEGIN
    self.comment("load_ordered");
    load_indirect(self, 0, mtype, ztype);
END load_ordered;

<*NOWARN*>PROCEDURE exchange(self: T; mtype: MType; ztype: ZType; <*UNUSED*>order: MemoryOrder) =
(* tmp := Mem [s1.A + offset].mtype;
   Mem [s1.A + offset].mtype := s0.ztype;
   s0.ztype := tmp;
   pop *)
BEGIN
    self.comment("exchange");
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
END compare_exchange;

PROCEDURE fence(self: T; <*UNUSED*>order: MemoryOrder) =
(*
 * x86: Exchanging any memory with any register is a serializing instruction.
 *)
BEGIN
    self.comment("fence");
    <* ASSERT self.in_proc *>
    <* ASSERT self.current_proc # NIL *>
    print(self, "m3_fence();\n");
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
    <* ASSERT CG_Bytes[ztype] >= CG_Bytes[mtype] *>
END fetch_and_op;

BEGIN
(*
    BitSizeToEnumCGType[8] := M3CG.Type.Word8;
    BitSizeToEnumCGType[16] := M3CG.Type.Word16;
    BitSizeToEnumCGType[32] := M3CG.Type.Word32;
*)
END M3C.
