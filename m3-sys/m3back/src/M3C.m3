MODULE M3C;

IMPORT RefSeq, TextSeq, Wr, Text, IntRefTbl, SortedIntRefTbl, TIntN, IntIntTbl;
IMPORT M3CG, M3CG_Ops, Target, TFloat, TargetMap, IntArraySort, Process;
IMPORT M3ID, TInt, TWord, ASCII, Thread, Stdio, Word, TextUtils;
FROM TargetMap IMPORT CG_Bytes;
FROM M3CG IMPORT Name, ByteOffset, CallingConvention;
FROM M3CG IMPORT BitSize, ByteSize, Alignment, Frequency;
FROM M3CG IMPORT Label, Sign, BitOffset, TypeUID;
FROM M3CG IMPORT Type, ZType, AType, RType, IType, MType;
FROM M3CG IMPORT CompareOp, ConvertOp, RuntimeError, MemoryOrder, AtomicOp;
FROM Target IMPORT CGType;
FROM M3CG_Ops IMPORT ErrorHandler;
IMPORT M3CG_MultiPass, M3CG_DoNothing, M3CG_Binary, RTIO;
IMPORT CharSeq, CharSeqRep;
FROM M3CC IMPORT IntToDec, IntToHex, UIntToHex, INT32;
IMPORT TextSetDef;
CONST NameT = M3ID.ToText;

(* 
Something like:
int F(unsigned i) { return i < 0; }
gets a warning with gcc.
You can quash it with -Wno-type-limits in newer versions
but not older. We know 4.2 does not have the flag and 4.3 does.
*)
(* VAR AvoidGccTypeRangeWarnings := FALSE; comparison is always false due to limited range of data type *)
VAR PassStructsByValue := FALSE;    (* TODO change this *)
VAR ReturnStructsByValue := FALSE;  (* TODO change this *)
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

TYPE Multipass_t = M3CG_MultiPass.T OBJECT
        self: T;
    OVERRIDES
        end_unit := multipass_end_unit;
        set_runtime_proc := set_runtime_proc;
    END;

TYPE
T = M3CG_DoNothing.T OBJECT

        no_return := FALSE; (* are there any no_return functions -- i.e. #include <sys/cdefs.h on Darwin for __dead2 *)

        imported_procs: RefSeq.T := NIL; (*TODO*) (* Proc_t *)
        declared_procs: RefSeq.T := NIL; (*TODO*) (* Proc_t *)
        procs_pending_output: RefSeq.T := NIL; (*TODO*) (* Proc_t *)
        typeidToType: IntRefTbl.T := NIL;
        pendingTypes: RefSeq.T := NIL; (* Type_t *)
        temp_vars: REF ARRAY OF Var_t := NIL; (* for check_* to avoid double evaluation, and pop_static_link *)
        current_block: Block_t := NIL;

        multipass: Multipass_t := NIL;
        Err    : ErrorHandler := DefaultErrorHandler;
        anonymousCounter := -1;
        unique := "L_"; (* changed later *)
        c      : Wr.T := NIL;
        debug := 2; (* 1-5 >4 is to stdio *)
        stack  : RefSeq.T := NIL;
        params : TextSeq.T := NIL;
        op_index := 0;

        unit_name := "L_";
        handler_name_prefixes := ARRAY [FIRST(HandlerNamePieces) .. LAST(HandlerNamePieces)] OF TEXT{NIL, ..};
        param_count := 0;
        static_link_id := M3ID.NoID;
        RTException_Raise_id := M3ID.NoID;
        RTHooks_AssertFailed_id := M3ID.NoID;
        RTHooks_Raise_id := M3ID.NoID;
        RTHooks_ReportFault_id := M3ID.NoID;
        RTHooks_ReportFault_imported_or_declared := FALSE;
        alloca_id := M3ID.NoID;
        setjmp_id := M3ID.NoID;
        jmpbuf_size_id := M3ID.NoID;
        done_include_setjmp_h := FALSE;

        (* labels *)
        labels_min := FIRST(Label);
        labels_max := LAST(Label);
        labels: REF ARRAY (*Label=INTEGER*) OF BOOLEAN := NIL;

        (* initialization and record declaration support *)

        fields: TextSeq.T := NIL;
        current_offset := 0;
        initializer: TextSeq.T := NIL;
        debug_initializer: CharSeq.T := NIL;
        initializer_comma := "";

        (* initializers are aggregated into arrays to avoid
        redeclaring the types and generating new field names *)

        init_type := Type.Void;
        init_type_count := 0;

        (* line directive support *)
        file: TEXT := NIL;
        line := 0;
        line_directive := ""; (* combination of file/line *)
        nl_line_directive := "\n"; (* line_directive + "\n" *)
        last_char_was_newline := FALSE;
        suppress_line_directive := 0;

        static_link     : Var_t := NIL; (* based on M3x86 *)
        current_proc    : Proc_t := NIL; (* based on M3x86 *)
        param_proc      : Proc_t := NIL; (* based on M3x86 *)
        dummy_proc      : Proc_t := NIL; (* avoid null derefs for indirect calls *)
        in_proc         : BOOLEAN := FALSE; (* based on M3x86 *)
        in_proc_call    : BOOLEAN := FALSE; (* based on M3x86 *)
        abort_in_call   := FALSE;
        in_call_indirect := FALSE;
        proc_being_called : Proc_t := NIL;
        report_fault: TEXT := NIL; (* based on M3x86 -- reportlabel, global_var *)
        report_fault_used := FALSE;
        width := 0;
        typedef_defined: TextSetDef.T := NIL;

    METHODS
        Type_Init(type: Type_t; type_text_tail := "") := Type_Init;
        TIntLiteral(type: CGType; READONLY i: Target.Int): TEXT := TIntLiteral;

    OVERRIDES
        end_unit   := end_unit;

        set_error_handler := set_error_handler;
        begin_unit := begin_unit;
        set_source_file := set_source_file;
        set_source_line := set_source_line;
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

PROCEDURE DebugVerbose(self:T): BOOLEAN =
BEGIN
    RETURN self.debug > 1;
END DebugVerbose;

(*---------------------------------------------------------------------------*)

CONST HandlerNamePieces = ARRAY OF TEXT { "_M3_LINE_", "_I3_LINE_" };

CONST Text_left_brace = "{";
CONST Text_address = "ADDRESS";
CONST Text_int8 = "INT8";
CONST Text_uint8 = "UINT8";
CONST Text_int16 = "INT16";
CONST Text_uint16 = "UINT16";
CONST Text_int32 = "INT32";
CONST Text_uint32 = "UINT32";
CONST Text_int64 = "INT64";
CONST Text_uint64 = "UINT64";

TYPE BitSizeRange_t = [8..64];
(*TYPE BitSizeEnum_t = [8,16,32,64];*)
VAR BitsToCGInt := ARRAY BitSizeRange_t OF CGType { CGType.Void, .. };
VAR BitsToCGUInt := ARRAY BitSizeRange_t OF CGType { CGType.Void, .. };
VAR BitsToInt := ARRAY BitSizeRange_t OF TEXT {NIL, ..};    (* "INT8", "INT16", "INT32", "INT64" *)
VAR BitsToUInt := ARRAY BitSizeRange_t OF TEXT {NIL, ..};   (* "UINT8", "UINT16", "UINT32", "UINT64" *)
VAR SignedAndBitsToCGType: ARRAY BOOLEAN, BitSizeRange_t OF CGType;

PROCEDURE SetLineDirective(self: T) =
VAR start := ARRAY BOOLEAN OF TEXT{" /* ", "#"(*"//"*)}[output_line_directives];
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

CONST LabelToText = IntToHex;
CONST LabelToHex = IntToHex;

CONST BoolToText = ARRAY BOOLEAN OF TEXT{"FALSE", "TRUE"};

CONST reservedWords = ARRAY OF TEXT{
(* avoid using these identifiers for function names, local variables, parameter names, etc. *)
"ALPHA",
"AMD64",
"ARM",
"ARM32",
"ARM64",
"DBG",
"NDEBUG",
"NULL",
"RISCV",
"RISCV32",
"RISCV64",
"STRUCT",
"DOTDOTDOT",
"INT8",
"INT16",
"INT32",
"INT64",
"UINT8",
"UINT16",
"UINT32",
"UINT64",
(* TODO fill in more strings here like INT8, STRUCT, DOTDOTDOT that we use,
so i.e. they can be used for parameter, local, field names *)
(*
"_ANSI_SOURCE",
"_CHAR_UNSIGNED",
"_CLR_VER",
"_CPPRTTI",
"_CPPUNWIND",
"_DARWIN_C_SOURCE",
"_DEBUG",
"_DLL",
"_INTEGRAL_MAX_BITS",
"_MANAGED",
"_MSC_EXTENSIONS",
"_MSC_VER",
"_MT",
"_M_ALPHA",
"_M_CEE",
"_M_CEE_PURE",
"_M_CEE_SAFE",
"_M_IA64",
"_M_IX86",
"_M_IX86_FP",
"_M_MPPC",
"_M_MRX000",
"_M_PPC",
"_M_X64",
"_NATIVE_WCHAR_T_DEFINED",
"_OPENMP",
"_POSIX_C_SOURCE",
"_PTRDIFF_T",
"_SIZE_T",
"_VC_NODEFAULTLIB",
"_WCHAR_T",
"_WCHAR_T_DEFINED",
"_WIN32",
"_WIN64",
"_WINT_T",
"_Wp64",
"__CLR_VER",
"__COUNTER__",
"__DARWIN_NULL",
"__DATE__",
"__FILE__",
"__FUNCDNAME__",
"__FUNCSIG__",
"__FUNCTION__",
"__GNUC_MINOR__",
"__GNUC__",
"__LINE__",
"__MSVC_RUNTIME_CHECKS",
"__STDC__",
"__STDDEF_H__",
"__SUNPRO_C",
"__SUNPRO_CC",
"__TIMESTAMP__",
"__TIME__",
"__VA_ARGS__",
"__amd64",
"__based",
"__builtin_offsetof",
"__cdecl",
"__clrcall",
"__cplusplus",
"__cplusplus_cli",
"__darwin_ptrdiff_t",
"__darwin_size_t",
"__darwin_wchar_t",
"__darwin_wint_t",
"__declspec",
"__except",
"__fastcall",
"__finally",
"__forceinline",
"__i386",
"__inline",
"__int16",
"__int32",
"__int64",
"__int8",
"__need_NULL",
"__need_ptrdiff_t",
"__need_size_t",
"__need_wchar_t",
"__need_wint_t",
"__offsetof",
"__restrict",
"__sparc",
"__sparcv9",
"__sptr",
"__stdcall",
"__sun",
"__thiscall",
"__try",
"__unaligned",
"__uptr",
"__w64",
"__wchar_t",
"_based",
"_cdecl",
"_clrcall",
"_declspec",
"_fastcall",
"_restrict",
"_sptr",
"_stdcall",
"_thiscall",
"_unaligned",
"_uptr",
"_w64",
*)
"and",
"and_eq",
"asm",
"assert",
"auto",
"bitand",
"bitor",
"bool",
"break",
"case",
"catch",
"char",
"class",
"compl",
"const",
"const_cast",
"continue",
"default",
"delete",
"do",
"double",
"dynamic_cast",
"else",
"enum",
"except",
"explicit",
"export",
"extern",
"false",
"float",
"for",
"friend",
"goto",
"if",
"inline",
"int",
"long",
"main",
"mutable",
"namespace",
"new",
"not",
"not_eq",
"nullptr",
"offsetof",
"operator",
"or",
"or_eq",
"private",
"protected",
"ptrdiff_t",
"public",
"register",
"reinterpret_cast",
"return",
"short",
"signed",
"size_t",
"sizeof",
"static",
"static_cast",
"string",
"struct",
"switch",
"template",
"this",
"throw",
"true",
"try",
"typedef",
"typeid",
"typename",
"union",
"unsigned",
"using",
"virtual",
"void",
"volatile",
"wchar_t",
"while",
"wint_t",
"wmain",
"xor",
"xor_eq",

(* hack
The right fix here includes multiple passes.
  - import_procedure is called on unused function
    only declare them if they are otherwise referenced
Cstring.i3 declares strcpy and strcat incorrectly..on purpose.
*)
"strcpy", "strcat",

(* more incorrect declarations *)
"signgam", "cabs", "frexp", "modf",

(* brk, sbrk deprecated on MacOS X 10.10.4, ok on 10.5.8. *)
"brk", "sbrk",

(* st_mtime is a macro on Linux and cannot be used along with #include sys/stat.h *)
"st_mtime",

(* symbols internal to the generated code *)
"M3_HIGH_BITS",
"M3_LOW_BITS",
"m3_set_range",
"m3_set_intersection",
"m3_set_sym_difference",
"m3_set_union",
"WORD_T",
"CARDINAL",
"DOTDOTDOT",
"STRUCT",
"INT8", "UINT8", "INT16", "UINT16", "INT32", "UINT32", "INT64", "UINT64",
"m3_eq",
"m3_ne",
"m3_gt",
"m3_ge",
"m3_lt",
"m3_le",
"m3_xor",
"m3_check_range",
"GCC_VERSION"
};

(*
CONST suppressImports = ARRAY OF TEXT{
"strcpy", "strcat",
"signgam", "cabs", "frexp", "modf",
"brk", "sbrk",
};
*)

VAR replacementNames_Inited := FALSE;
VAR replacementNames_Table := NEW(IntIntTbl.Default).init(NUMBER(reservedWords));

PROCEDURE ReplaceName(id: M3ID.T): M3ID.T =
VAR replacement := 0;
BEGIN
    IF replacementNames_Inited = FALSE THEN
        FOR i := FIRST(reservedWords) TO LAST(reservedWords) DO
            WITH text = reservedWords[i] DO
                EVAL replacementNames_Table.put(M3ID.Add(text), M3ID.Add("m3_" & text));
            END;
        END;
        replacementNames_Inited := TRUE;
    END;
    IF replacementNames_Table.get(id, replacement) THEN
        RETURN replacement;
    END;
    RETURN id;
END ReplaceName;

PROCEDURE AnonymousCounter(self: T): INTEGER =
BEGIN
    INC(self.anonymousCounter, 1 + ORD(self.anonymousCounter = 385)); (* avoid "i386" -- really, it happened *)
    RETURN self.anonymousCounter;
END AnonymousCounter;

(* e.g. padding fields within a struct *)
PROCEDURE GenerateNameLocal(self: T): Name =
BEGIN
    RETURN M3ID.Add("L_" & IntToDec(AnonymousCounter(self)));
END GenerateNameLocal;

(* e.g. so if we concatenate many files, still unique *)
PROCEDURE GenerateNameGlobal(self: T): Name =
BEGIN
    RETURN M3ID.Add(self.unique & IntToDec(AnonymousCounter(self)));
END GenerateNameGlobal;

PROCEDURE GenerateNameLocalText(self: T): TEXT =
BEGIN
    RETURN NameT(GenerateNameLocal(self));
END GenerateNameLocalText;

PROCEDURE Assert(self: T; value: BOOLEAN; message: TEXT) =
BEGIN
  IF NOT value THEN
    RTIO.PutText("Assertion failure: " & message);
    RTIO.Flush();
    IF self.c # NIL THEN
      Wr.Flush(self.c);
    END;
  END;
  <* ASSERT value *>
END Assert;

(* This is not just for procedures. *)
PROCEDURE Proc_FixName(self: T; name: Name): Name =
VAR text: TEXT := NIL;
BEGIN
    IF name = M3ID.NoID THEN
        RETURN GenerateNameGlobal(self);
    END;
    text := NameT (name);
    IF Text.GetChar (text, 0) = '*' THEN
        Assert(self, Text.Length(text) = 1, "Text.Length(text) = 1");
        Err(self, "almost unnamed function");
        RETURN GenerateNameLocal(self);
    END;
    (* rename C names like int, short, void *)
    RETURN ReplaceName(name);
END Proc_FixName;

(* segment names are not unique if files are concatenated; this fixes that
   TODO: Handle it in m3front *)
PROCEDURE Segment_FixName(self: T; name: Name): Name =
VAR text := NameT(name);
    ch: CHAR;
BEGIN
  IF Text.Length(text) > 2 AND Text.GetChar(text, 1) = '_' THEN
    ch := Text.GetChar(text, 0);
    IF ch = 'M' OR ch = 'I' THEN
      name := M3ID.Add(self.unique & text);
    END;
  END;
  RETURN name;
END Segment_FixName;

PROCEDURE Var_FixName(self: T; name: Name; imported_or_exported: BOOLEAN): Name =
VAR name1 := name;
BEGIN
    name := Proc_FixName(self, name);
    (* workaround: begin/end_block should keep the names separate,
     * but they do nothing until import_procedure all moved up.
     * e.g. ETimer__Push() has parameter and local "self"
     *)
    IF NOT imported_or_exported AND name1 # self.static_link_id THEN
        name := M3ID.Add(NameT(name) & "_L_" & IntToDec(AnonymousCounter(self)));
    END;
    RETURN Segment_FixName(self, name);
END Var_FixName;

TYPE Type_State = {None, ForwardDeclared, CanBeDefined, Defined};

PROCEDURE TypeText (self: T; cgtype: CGType; typename: TEXT; typeid: TypeUID := 0; type_text: TEXT := NIL; name := M3ID.NoID): TEXT =
VAR type: Type_t := NIL;
BEGIN
  (* All typeids must be known, though this is maybe overkill. *)
  IF typeid # -1 AND typeid # 0 AND NOT ResolveType(self, typeid, type) THEN
    Err(self, "TypeText:"
              & " unknown typeid:" & TypeIDToText(typeid)
              & " type:" & cgtypeToText[cgtype]
              & " name:" & TextOrNil(NameT(name)) & "\n");
  END;

  IF type_text = NIL THEN
    IF type # NIL THEN
      type_text := type.text & " /* TypeText1 */ ";
      IF cgtype = CGType.Addr AND NOT PassStructsByValue AND (type.isRecord() OR type.isArray()) THEN (* TODO remove this *)
        type_text := type_text & " * " & " /* TypeText2 */ ";
      END;
    ELSE
      type_text := cgtypeToText[cgtype] & " /* TypeText3 */ ";
    END;
  END;

  (* If typename is already defined, such as from m3core.h, use that.
   * Else typedef it to be type_text, which is the resolved
   * lowlevel CG type like Int32, Int64, Address.
   * Gradually all types should be typename and never just CG.
   * Historically the other way around: There was only CG and no typename.
   *
   *)
  (* > 4 instead of != NULL, which occurs in elego\m3msh\src\M3MiniShell.m3 *)
  (* IF typename # NIL AND Text.Length (typename) > 0 AND Text.GetChar (typename, 0) IN ASCII.Letters AND NOT Text.Equal (typename, "NULL") THEN *)
  IF typename # NIL AND Text.Length (typename) > 4 AND Text.GetChar (typename, 0) IN ASCII.Letters THEN
    IF NOT self.typedef_defined.insert(typename) THEN
      ifndef(self, typename);
      print(self, "typedef " & type_text & " " & typename & ";");
      endif(self);
    END;
    RETURN typename;
  END;

  RETURN type_text;
END TypeText;

TYPE Type_t = OBJECT
    bit_size := 0;  (* FUTURE Target.Int or LONGINT *)
    typeid: TypeUID := 0;
    text: TEXT := NIL;
    cgtype: CGType := CGType.Addr;
    state := Type_State.None;
METHODS
(* public (Uppercase) *)
    Define(self: T) := Type_Define;
    CanBeDefined(self: T): BOOLEAN := Type_CanBeDefined;
    IsDefined(): BOOLEAN := Type_IsDefined;
    ForwardDeclare(self: T) := Type_ForwardDeclare; (* useful for structs *)
    IsForwardDeclared(): BOOLEAN := Type_IsForwardDeclared; (* useful for structs *)
    (* GetMinimumBitSize(): INTEGER FUTURE Target.Int := Type_GetMinimumBitSize; *)

(* protected lowercase) *)
    canBeForwardDeclared(self: T): BOOLEAN := type_canBeForwardDeclared_false; (* useful for structs *)
    forwardDeclare(self: T) := type_forwardDeclare; (* useful for structs *)
    define(self: T);
    canBeDefined(self: T): BOOLEAN := type_canBeDefined_false;
    (* not used isOrdinal(): BOOLEAN := type_isType_false; *)
    (* not used isInteger(): BOOLEAN := type_isType_false; *)
    (* not used isFloat(): BOOLEAN := type_isType_false; *)
    isRecord(): BOOLEAN := type_isType_false;
    isArray(): BOOLEAN := type_isType_false;
    (* not used isEnum(): BOOLEAN := type_isType_false; *)
    (* not used isSubrange(): BOOLEAN := type_isType_false; *)
    (* not used isPointer(): BOOLEAN := type_isType_false; *)
    isPacked(): BOOLEAN := type_isType_false;
    toPacked(): Packed_t := type_toPacked_nil;
    (* getMinimumBitSize(): INTEGER FUTURE Target.Int := type_getMinimumBitSize; *)
END;

(*
PROCEDURE Type_GetMinimumBitSize(type: Type_t): INTEGER FUTURE Target.Int =
BEGIN
    RETURN type.getMinimumBitSize();
END Type_GetMinimumBitSize;

PROCEDURE type_getMinimumBitSize(type: Type_t): INTEGER FUTURE Target.Int =
BEGIN
    RETURN type.bit_size;
END type_getMinimumBitSize;
*)

PROCEDURE type_toPacked_nil(<*UNUSED*>type: Type_t): Packed_t =
BEGIN
    RETURN NIL;
END type_toPacked_nil;

PROCEDURE type_isType_true(<*UNUSED*>type: Type_t): BOOLEAN =
BEGIN
    RETURN TRUE;
END type_isType_true;

PROCEDURE type_isType_false(<*UNUSED*>type: Type_t): BOOLEAN =
BEGIN
    RETURN FALSE;
END type_isType_false;

PROCEDURE Type_IsForwardDeclared(type: Type_t): BOOLEAN =
BEGIN
    RETURN type.state = Type_State.Defined OR type.state = Type_State.ForwardDeclared;
END Type_IsForwardDeclared;

PROCEDURE Type_IsDefined(type: Type_t): BOOLEAN =
BEGIN
    RETURN type.state = Type_State.Defined;
END Type_IsDefined;

PROCEDURE type_canBeForwardDeclared_false(<*UNUSED*>type: Type_t; <*UNUSED*>self: T): BOOLEAN =
BEGIN
    RETURN FALSE;
END type_canBeForwardDeclared_false;

PROCEDURE type_canBeForwardDeclared_true(<*UNUSED*>type: Type_t; <*UNUSED*>self: T): BOOLEAN =
BEGIN
    RETURN TRUE;
END type_canBeForwardDeclared_true;

PROCEDURE type_forwardDeclare(<*UNUSED*>type: Type_t; <*UNUSED*>self: T) =
BEGIN
END type_forwardDeclare;

PROCEDURE Type_CanBeDefined(type: Type_t; self: T): BOOLEAN =
BEGIN
    IF type.state = Type_State.Defined THEN
        RETURN FALSE;
    END;
    IF type.state = Type_State.CanBeDefined THEN
        RETURN TRUE;
    END;
    IF type.canBeDefined(self) THEN
        type.state := Type_State.CanBeDefined;
        RETURN TRUE;
    END;
    RETURN FALSE;
END Type_CanBeDefined;

PROCEDURE Type_Define(type: Type_t; self: T) =
BEGIN
    IF NOT type.CanBeDefined(self) THEN
        RETURN;
    END;
    type.define(self);
    type.state := Type_State.Defined;
END Type_Define;

PROCEDURE Type_ForwardDeclare(type: Type_t; self: T) =
BEGIN
    IF ORD(type.state) >= ORD(Type_State.ForwardDeclared) OR NOT type.canBeForwardDeclared(self) THEN
      RETURN;
    END;
    type.forwardDeclare(self);
    IF ORD(type.state) < ORD(Type_State.ForwardDeclared) THEN
        type.state := Type_State.ForwardDeclared;
    END;
END Type_ForwardDeclare;

TYPE Type_CanBeDefinedTrue_t = Type_t OBJECT
OVERRIDES
    canBeDefined := type_canBeDefined_true;
END;

PROCEDURE type_typedef(type: Type_t; self: T) =
(* A reusable value for Type_Define. *)
BEGIN
    print(self, "/*type_typedef*/typedef " & cgtypeToText[type.cgtype] & " " & type.text & ";\n");
END type_typedef;

PROCEDURE type_canBeDefined_true(<*UNUSED*>type: Type_CanBeDefinedTrue_t; <*UNUSED*>self: T): BOOLEAN =
BEGIN
    RETURN TRUE;
END type_canBeDefined_true;

PROCEDURE type_canBeDefined_false(<*UNUSED*>type: Type_t; <*UNUSED*>self: T): BOOLEAN =
BEGIN
    RETURN FALSE;
END type_canBeDefined_false;

TYPE PointerOrTypename_t = Type_t OBJECT
  refers_to_type: Type_t := NIL;
  refers_to_typeid: TypeUID := 0;
OVERRIDES
  canBeDefined := pointerOrTypename_canBeDefined;
END;

TYPE Pointer_t = PointerOrTypename_t OBJECT
  target_typename: TEXT := NIL; (* optional *)
  brand: TEXT := NIL;           (* optional *)
  traced := FALSE;              (* optional *)
OVERRIDES
  define := pointer_define;
END;

TYPE Typename_t = PointerOrTypename_t OBJECT
OVERRIDES
  define := typename_define;
END;

PROCEDURE pointer_define(type: Pointer_t; self: T) =
(* Does branding make a difference? *)
VAR x := self;
    target_typename := type.target_typename;
BEGIN
    <*ASSERT type.refers_to_type # NIL *> (* canBeDefined should have already set refers_to_type *)
    type.target_typename := NIL;

    (* We have recursive types TYPE FOO = UNTRACED REF FOO. Typos actually. *)
    IF type.refers_to_typeid = type.typeid THEN
      print(x, "typedef void* " & type.text & ";\n");
      RETURN;
    END;

    type.refers_to_type.ForwardDeclare(self);

    IF target_typename = NIL THEN
      (* If no typename given fallback to the existing name, usually a hash.
       * The cases are for example:
       * 1. Reference a hash:
       *    REF RECORD .. END; => *T123...
       * 2. Reference a name in context:
       *    REF T => *T;
       * 3. Reference a hash that has been given a name otherwise coincidentally:
       *    TYPE T = RECORD .. END;
       *    REF RECORD .. END; => *T
       *)
      target_typename := type.refers_to_type.text;
    END;
    target_typename := target_typename & "*";
    (* TODO This typedef is not likely needed, given the subsequent rendering as target*.
     * And then this replacement can be done immediately without waiting for target
     * to be defined. *)
    print(x, "typedef " & target_typename & TypeIDToText(type.typeid) & ";\n");
    type.text := target_typename; (* with the star *)
END pointer_define;

PROCEDURE typename_define(type: Typename_t; self: T) =
VAR x := self;
    typetext := type.text;
BEGIN
    <*ASSERT type.refers_to_type # NIL *> (* canBeDefined should have already set refers_to_type *)
    <*ASSERT type.refers_to_typeid # type.typeid *>

    type.refers_to_type.ForwardDeclare(self);

    IF NOT x.typedef_defined.insert(typetext) THEN
      ifndef (self, typetext);
      print(x, "typedef " & type.refers_to_type.text & " " & typetext & ";");
      endif (self);
    END;
END typename_define;

TYPE Packed_t = Type_t OBJECT
    base_typeid: TypeUID := 0;
    base_type: Type_t := NIL;
OVERRIDES
    define := packed_define;
    canBeDefined := packed_canBeDefined;
    isPacked := type_isType_true;
    toPacked := packed_toPacked;
(* not used
    isOrdinal := packed_isOrdinal;
    isInteger := packed_isInteger;
    isFloat := packed_isFloat; *)
    isRecord := packed_isRecord;
    isArray := packed_isArray;
(* not used
    isEnum := packed_isEnum;
    isSubrange := packed_isSubrange;
    isPointer := packed_isPointer; *)
END;

PROCEDURE packed_toPacked(type: Packed_t): Packed_t =
BEGIN
    RETURN type;
END packed_toPacked;

PROCEDURE packed_define(type: Packed_t; self: T) =
VAR x := self;
BEGIN
    type.base_type.Define(self);
    print(x, "typedef " & type.base_type.text & " " & type.text & ";\n");
END packed_define;

PROCEDURE packed_canBeDefined(type: Packed_t; self: T): BOOLEAN =
BEGIN
    RETURN ResolveType(self, type.base_typeid, type.base_type)
        AND (type.base_type.IsDefined() (*OR type.base_type.CanBeDefined(self)*));
END packed_canBeDefined;

PROCEDURE packed_isArray(type: Packed_t): BOOLEAN =
BEGIN
    RETURN type.base_type.isArray();
END packed_isArray;

PROCEDURE packed_isRecord(type: Packed_t): BOOLEAN =
BEGIN
    RETURN type.base_type.isRecord();
END packed_isRecord;

PROCEDURE ResolveType(self: T; typeid: INTEGER; VAR type: Type_t): BOOLEAN =
BEGIN
    IF type # NIL THEN
        (* self.comment("ResolveType1 TRUE typeid:" & TypeIDToText(typeid)); *)
        RETURN TRUE;
    END;
    type := TypeidToType_Get(self, typeid);
    (* self.comment("ResolveType2 " & BoolToText[type # NIL] & " typeid:" & TypeIDToText(typeid)); *)
    RETURN type # NIL;
END ResolveType;

PROCEDURE pointerOrTypename_canBeDefined(type: PointerOrTypename_t; self: T): BOOLEAN =
BEGIN
    (* We have recursive types TYPE FOO = UNTRACED REF FOO. Typos actually. *)
    IF type.refers_to_typeid = type.typeid THEN
      RETURN TRUE;
    END;
    RETURN ResolveType(self, type.refers_to_typeid, type.refers_to_type)
        AND (type.refers_to_type.IsForwardDeclared() OR type.refers_to_type.IsDefined() (*OR type.refers_to_type.CanBeDefined(self)*));
END pointerOrTypename_canBeDefined;

(* We need "Ordinal_t" as base for: Integer_t, Enum_t, Subrange_t *)

TYPE Ordinal_t = Type_CanBeDefinedTrue_t OBJECT
OVERRIDES
    define := type_typedef;
    (* not used isOrdinal := type_isType_true; *)
END;

TYPE Integer_t = Ordinal_t OBJECT
OVERRIDES
    define := type_typedef;
    (* not used isInteger := type_isType_true; *)
    canBeDefined := type_canBeDefined_true;
END;

TYPE Float_t = Type_CanBeDefinedTrue_t OBJECT
OVERRIDES
    define := type_typedef;
    (* not used isFloat := type_isType_true; *)
    canBeDefined := type_canBeDefined_true;
END;

TYPE Field_t = REF RECORD
    bit_offset, bit_size, typeid := -1;
    name: Name := 0;
    type: Type_t := NIL;
END;

TYPE Record_t = Type_t OBJECT
    fields: RefSeq.T := NIL; (* Field_t *)
OVERRIDES
    canBeForwardDeclared := type_canBeForwardDeclared_true;
    forwardDeclare := record_forwardDeclare;
    canBeDefined := record_canBeDefined;
    define := record_define;
    isRecord := type_isType_true;
END;

PROCEDURE record_forwardDeclare(type: Record_t; self: T) =
VAR id := type.text;
BEGIN
    (*  typedef struct foo foo is different than
        struct foo; typedef struct foo foo, in the presence of C++ namespaces?,
        so use the second form
    *)
    print(self, "/*record_forwardDeclare*/struct " & id & ";typedef struct " & id & " " & id & ";\n");
END record_forwardDeclare;

PROCEDURE record_canBeDefined(type: Record_t; self: T): BOOLEAN =
VAR field: Field_t := NIL;
    fields := type.fields;
BEGIN
    FOR i := 0 TO fields.size() - 1 DO
        field := NARROW(fields.get(i), Field_t);
        Assert(self, field # NIL, "field # NIL");
        IF NOT ResolveType(self, field.typeid, field.type) THEN
            RETURN FALSE;
        END;
        IF NOT ((field.type.IsDefined() (*OR field.type.CanBeDefined(self)*))) THEN
            RETURN FALSE;
        END;
    END;
    RETURN TRUE;
END record_canBeDefined;

(* ifndef so multiple files can be concatenated and compiled at once *)
PROCEDURE ifndef(self:T; id: TEXT) =
BEGIN
  print (self, "\n#ifndef " & id & "\n#define " & id & " " & id & "\n");
END ifndef;

PROCEDURE endif(self: T) =
BEGIN
  print(self, "\n#endif\n");
END endif;

PROCEDURE record_define(record: Record_t; self: T) =
VAR x := self;
    fields := record.fields;
    field_count := fields.size();
    bit_size := record.bit_size;
    int_type, i, bit_pad, bit_offset := 0;
    field: Field_t := NIL;
    name := "";
BEGIN
    FOR j := 0 TO field_count - 1 DO
        NARROW(record.fields.get(j), Field_t).type.Define(self);
    END;

    ifndef(x, record.text); (* ifdef so multiple files can be concatenated and compiled at once *)

    print(x, "/*record_define*/struct " & record.text & "{\n");

    FOR j := 0 TO field_count - 1 DO
        field := NARROW(record.fields.get(j), Field_t);
        name := NameT(field.name);
        IF field.bit_offset < bit_offset THEN
            Err(x, "fields not in offset order bit_offset:" & IntToDec(bit_offset) & " field.bit_offset:" &
                IntToDec(field.bit_offset) & " name:" & name);
        END;
        bit_pad := field.bit_offset - bit_offset;
        (* padding: array of bytes followed by bitfield *)
        IF bit_pad > 0 THEN

            (* Eat up bits, to the next byte boundary or up to the next field, whichever is earlier. *)
            IF (bit_offset MOD 8) # 0 THEN
                i := MIN(bit_pad, 8 - (bit_pad MOD 8));
                print(x, "UINT8 " & GenerateNameLocalText(x) & ":" & IntToDec(i) & ";\n");
                INC(bit_offset, i);
                DEC(bit_pad, i);
            END;

            (* Eat up bytes to the field. *)
            IF bit_pad >= 8 THEN
                i := bit_pad DIV 8;
                print(x, "UINT8 " & GenerateNameLocalText(x) & "[" & IntToDec(i) & "];\n");
                i := i * 8;
                INC(bit_offset, i);
                DEC(bit_pad, i);
            END;

            (* Eat up bits to the field. *)
            Assert(self, bit_pad < 8, "bit_pad < 8");
            IF bit_pad > 0 THEN
                i := bit_pad;
                print(x, "UINT8 " & GenerateNameLocalText(x) & ":" & IntToDec(i) & ";\n");
                INC(bit_offset, i);
                DEC(bit_pad, i);
            END;
        END;

        (* Handle bitfields specially. *)
        IF (field.bit_size MOD 8) # 0 THEN
            IF NOT field.type.isPacked() THEN
                Err(x, "bitfield is not packed?");
            END;
            IF field.bit_size > 64 THEN
                Err(x, "bitfield larger than 64 bits");
            ELSIF field.bit_size > 32 THEN
                int_type := 64;
            ELSIF field.bit_size > 16 THEN
                int_type := 32;
            ELSIF field.bit_size > 8 THEN
                int_type := 16;
            ELSE
                int_type := 8;
            END;
            print(x, BitsToUInt[int_type] & " " & name & ":" & IntToDec(field.bit_size) & ";\n");
        ELSE
            print(x, field.type.text & " " & name & ";\n");
        END;
        INC(bit_offset, field.bit_size);
    END;

    (* pad out end of record *)
    IF bit_offset > bit_size THEN
        Err(x, "record fields exceed record");
    END;
    bit_pad := bit_size - bit_offset;
    IF bit_pad > 0 THEN
        i := bit_pad MOD 8;
        IF i > 0 THEN
            (*i := 8 - i;*)
            print(x, "UINT8 " & GenerateNameLocalText(x) & ":" & IntToDec(i) & ";\n");
            INC(bit_offset, i);
            DEC(bit_pad, i);
        END;
        Assert (self, (bit_pad MOD 8) = 0, "(bit_pad MOD 8) = 0");
        IF bit_pad > 0 THEN
            i := bit_pad DIV 8;
            print(x, "UINT8 " & GenerateNameLocalText(x) & "[" & IntToDec(i) & "];\n");
            i := i * 8;
            INC(bit_offset, i);
            DEC(bit_pad, i);
        END;
    END;
    IF bit_offset # bit_size THEN
        Err(x, "failed to declare record to correct size");
    END;
    print(x, "};");
    endif(x);
END record_define;

TYPE Subrange_t = Ordinal_t OBJECT
    min: Target.Int := TInt.Zero;
    max: Target.Int := TInt.Zero;
    domain_type: Type_t := NIL;
    domain_typeid: TypeUID := 0;
OVERRIDES
    define := subrange_define;
    canBeDefined := subrange_canBeDefined;
    (* not used isSubrange := type_isType_true; *)
    (* getMinimumBitSize := subrange_getMinimumBitSize; *)
END;

(*
PROCEDURE subrange_getMinimumBitSize(subrange: Subrange_t): INTEGER FUTURE Target.Int =
VAR count := TInt.Zero;
    max_for_bits, temp := TInt.Two;
    min := subrange.min;
    max := subrange.max;
    i := 0;
BEGIN
    IF TInt.LE(min, max) THEN
        EVAL TInt.Subtract(max, min, count);
        EVAL TInt.Inc(count);
        WHILE i < 63 DO
            INC(i);
            IF TInt.LE(count, max_for_bits) THEN
                EXIT;
            END;
            EVAL TInt.Add(max_for_bits, max_for_bits, temp);
            max_for_bits := temp;
        END;
    END;
    RTIO.PutText("subrange_getMinimumBitSize: min:"
        & TInt.ToText(min)
        & " max:" & TInt.ToText(max)
        & " result:" & IntToDec(i));
    RTIO.Flush();
    RETURN i;
END subrange_getMinimumBitSize;
*)

PROCEDURE subrange_define(subrange: Subrange_t; self: T) =
VAR x := self;
    text := "";
BEGIN
    IF ResolveType(self, subrange.domain_typeid, subrange.domain_type)
            AND subrange.domain_type.bit_size = subrange.bit_size THEN
        subrange.domain_type.Define(self);
        text := subrange.domain_type.text;
    ELSE
        text := cgtypeToText[subrange.cgtype];
    END;
    print(x, "/*subrange_define*/typedef " & text & " " & subrange.text & ";\n");
    (* subrange.text := text; *)
END subrange_define;

PROCEDURE subrange_canBeDefined(type: Subrange_t; self: T): BOOLEAN =
BEGIN
    RETURN ResolveType(self, type.domain_typeid, type.domain_type) AND (type.domain_type.IsDefined() (*OR type.domain_type.CanBeDefined(self)*));
END subrange_canBeDefined;

TYPE Enum_t  = Subrange_t OBJECT
    names: REF ARRAY OF Name := NIL;
OVERRIDES
    define := enum_define;
    (* not used isEnum := type_isType_true; *)
    canBeDefined := enum_canBeDefined;
END;

PROCEDURE enum_canBeDefined(enum: Enum_t; <*UNUSED*> x: T): BOOLEAN =
BEGIN
    RETURN enum.names # NIL;
END enum_canBeDefined;

PROCEDURE enum_define(enum: Enum_t; x: T) =
VAR bit_size := enum.bit_size;
    id := enum.text;
    int_type := BitsToUInt[bit_size];
    start := "#define " & id & "_";
    cast := " ((" & int_type & ")";
    end := ") /*declare_enum_elt*/\n";
    names := enum.names;
    element_count := NUMBER(names^);
BEGIN
    SuppressLineDirective(x, element_count, "declare_enum element_count");
    (* TODO cplusplus and GNU C can do better *)
    print(x, "/*enum_define*/typedef " & int_type & " " & id & "; /*declare_enum*/\n");
    FOR i := 0 TO element_count - 1 DO
        Assert (x, NameT(names^[i]) # NIL, "NameT(names^[i]) # NIL");
        print(x, start & NameT(names^[i]) & cast & IntToDec(i) & end);
    END;
END enum_define;

TYPE Array_t = Type_t OBJECT
    index_typeid := 0;
    element_typeid := 0;
    index_type: Type_t := NIL;
    element_type: Type_t := NIL;
OVERRIDES
    isArray := type_isType_true;
    canBeForwardDeclared := type_canBeForwardDeclared_true;
    forwardDeclare := array_forwardDeclare;
END;

PROCEDURE array_forwardDeclare(type: Array_t; self: T) =
VAR id := type.text;
BEGIN
    (*  typedef struct foo foo is different than
        struct foo; typedef struct foo foo, in the presence of C++ namespaces?,
        so use the second form
    *)
    print(self, "/*array_forwardDeclare*/struct " & id & ";typedef struct " & id & " " & id & ";\n");
END array_forwardDeclare;

TYPE FixedArray_t = Array_t OBJECT
OVERRIDES
    define := fixedArray_define;
    canBeDefined := fixedArray_canBeDefined;
END;

PROCEDURE fixedArray_define(type: FixedArray_t; x: T) =
BEGIN
    type.element_type.Define(x);

    ifndef(x, type.text); (* ifdef so multiple files can be concatenated and compiled at once *)

    print(x, "/*fixedArray_define*/struct " & type.text & "{");
    print(x, type.element_type.text);
    print(x, " _elts[");
    print(x, IntToDec(type.bit_size DIV type.element_type.bit_size));
    print(x, "];};");
    endif(x);
END fixedArray_define;

PROCEDURE fixedArray_canBeDefined(type: Array_t; self: T): BOOLEAN =
BEGIN
    RETURN ResolveType(self, type.element_typeid, type.element_type) AND (type.element_type.IsDefined() (*OR type.element_type.CanBeDefined(self)*));
END fixedArray_canBeDefined;

TYPE OpenArray_t = Array_t OBJECT
OVERRIDES
    define := openArray_define;
    canBeDefined := openArray_canBeDefined;
END;

PROCEDURE openArray_canBeDefined(type: Array_t; self: T): BOOLEAN =
BEGIN
  IF NOT ResolveType(self, type.element_typeid, type.element_type) THEN
    RETURN FALSE;
  END;
  IF NOT Type_IsForwardDeclared(type.element_type) THEN
    RETURN FALSE;
  END;
  RETURN TRUE;
END openArray_canBeDefined;

PROCEDURE openArray_define(type: OpenArray_t; x: T) =
VAR text := "";
    element_type := type.element_type;
    element_type_text: TEXT := NIL;
    Integer_size := Target.Integer.size;
    dimensions := (type.bit_size - Integer_size) DIV Integer_size;
BEGIN
    Assert (x, dimensions >= 1, "dimensions >= 1");
    IF element_type # NIL THEN
        element_type_text := element_type.text;
    END;
    IF element_type_text = NIL THEN
        element_type_text := "char/*TODO*/";
    END;

    ifndef(x, type.text); (* ifdef so multiple files can be concatenated and compiled at once *)

    text := "/*openArray_define*/struct " & type.text & "{\n" & element_type_text;
    FOR i := 1 TO dimensions DO
        text := text & "*";
    END;
    text := text & "_elts;\nCARDINAL _size";
    IF dimensions > 1 THEN
        text := text & "s[" & IntToDec(dimensions) & "]";
    END;
    print(x, text & ";\n};");
    endif(x);
END openArray_define;

TYPE ProcType_t = Type_CanBeDefinedTrue_t OBJECT
  index := 0;
  types: REF ARRAY OF Type_t := NIL;
  typeids: REF ARRAY OF TypeUID := NIL;
  callingConvention: CallingConvention;
OVERRIDES
  canBeDefined := ProcType_canBeDefined;
  define := ProcType_define;
END;

PROCEDURE ProcType_canBeDefined(type: ProcType_t; self: T): BOOLEAN =
BEGIN
  IF type.index # NUMBER(type.types^) THEN
    RETURN FALSE;
  END;

  FOR i := 0 TO NUMBER(type.types^) - 1 DO
    IF type.types^[i] = NIL AND type.typeids^[i] # 0 THEN
      type.types^[i] := TypeidToType_Get(self, type.typeids^[i]);
      IF type.types^[i] = NIL THEN
        RETURN FALSE;
      END;
      IF NOT type.types^[i].IsDefined() THEN
        RETURN FALSE;
      END;
    END;
  END;

  type.typeids := NIL; (* no longer needed *)
  RETURN TRUE;

END ProcType_canBeDefined;

PROCEDURE ProcType_define(type: ProcType_t; self: T) =
VAR return := type.types[0];
BEGIN
  print(self, "typedef ");
  IF return = NIL THEN
    print(self, "void");
  ELSE
    print(self, return.text);
  END;
  (* Ifdef guard might be needed here, but it is also advantageous
   * to omit. If there are duplicates, the C compiler verifies
   * they are equivalent, else errors.
   *)
  print(self, "(");
  print(self, CallingConventionToText(type.callingConvention));
  print(self, "*");
  print(self, type.text);
  print(self, ")(");
  IF NUMBER(type.types^) = 1 THEN
    print(self, "void");
  ELSE
    FOR i := 1 TO NUMBER(type.types^) - 1 DO
      print(self, type.types[i].text);
      IF i # NUMBER(type.types^) - 1 THEN
        print(self, ",");
      END;
    END;
  END;
  print(self, ");");
END ProcType_define;

PROCEDURE TypeidToType_Get(self: T; typeid: TypeUID): Type_t =
VAR type: REFANY := NIL;
BEGIN
    IF typeid # -1 AND typeid # 0 THEN
        EVAL self.typeidToType.get(typeid, type);
    END;
    IF type = NIL THEN
        RETURN NIL;
    END;
    RETURN NARROW(type, Type_t);
END TypeidToType_Get;

PROCEDURE Type_Init(self: T; type: Type_t; type_text_tail := "") =
VAR cgtype := type.cgtype;
BEGIN
    (* TODO require bit_size be set *)
    IF type.bit_size = 0 THEN
        type.bit_size := TargetMap.CG_Size[cgtype];
    END;

    IF type.text = NIL THEN
        IF type.typeid = -1 OR type.typeid = 0 THEN
            IF cgtype = CGType.Struct THEN
                type.text := " /* Type_Init Struct */ " & Struct(type.bit_size DIV 8);
            ELSE
                type.text := cgtypeToText[cgtype];
            END;
        ELSE
            type.text := TypeIDToText(type.typeid) & type_text_tail;
        END;
    END;

    IF type.typeid # -1 AND type.typeid # 0 THEN
        EVAL self.typeidToType.put(type.typeid, type);
    END;

    Type_ForwardDeclare(type, self);
    IF Type_CanBeDefined(type, self) THEN
        type.Define(self);
    ELSE
        self.pendingTypes.addhi(type);
    END;
END Type_Init;

PROCEDURE SignExtend(a, b: INTEGER): INTEGER =
BEGIN
    b := Word.LeftShift(-1, b - 1);
    IF Word.And(a, b) # 0 THEN
        a := Word.Or(a, b);
    END;
    RETURN a;
END SignExtend;

PROCEDURE SignExtend32(a: INTEGER): INT32 =
BEGIN
    RETURN SignExtend(a, 32);
END SignExtend32;

CONST IntegerToTypeid = SignExtend32;

(* see RTBuiltin.mx
   see RT0.i3
   see output of m3front -vsdebug
   typeids are 32bit.
   16_FFFFFFFF on a 64bit system is a large 64bit value
   that does not fit in 32bits. It gets a range error at runtime.
   Two unsatisfactory solutions:
     - CONST foo = -1 (* 16_FFFFFFFF *);
     - VAR foo := IntegerToTypeid(16_FFFFFFFF);
*)
CONST UID_INTEGER = 16_195C2A74; (* INTEGER *)
CONST UID_LONGINT = 16_05562176; (* LONGINT *)
VAR UID_WORD := IntegerToTypeid(16_97E237E2); (* CARDINAL *)
VAR UID_LONGWORD := IntegerToTypeid(16_9CED36E7); (* LONGCARD *)
CONST UID_REEL = 16_48E16572; (* REAL *)
VAR UID_LREEL := IntegerToTypeid(16_94FE32F6); (* LONGREAL *)
VAR UID_XREEL := IntegerToTypeid(16_9EE024E3); (* EXTENDED *)
CONST UID_BOOLEAN = 16_1E59237D; (* BOOLEAN [0..1] *)
CONST UID_CHAR = 16_56E16863; (* CHAR [0..255] *)
VAR UID_WIDECHAR := IntegerToTypeid(16_88F439FC);
CONST UID_MUTEX = 16_1541F475; (* MUTEX *)
CONST UID_TEXT = 16_50F86574; (* TEXT *)
VAR UID_UNTRACED_ROOT := IntegerToTypeid(16_898EA789); (* UNTRACED ROOT *)
VAR UID_ROOT := IntegerToTypeid(16_9D8FB489); (* ROOT *)
CONST UID_REFANY = 16_1C1C45E6; (* REFANY *)
CONST UID_ADDR = 16_08402063; (* ADDRESS *)
(*VAR UID_RANGE_0_31 := 16_2DA6581D; [0..31] *)
(*VAR UID_RANGE_0_63 := 16_2FA3581D; [0..63] *)
VAR UID_PROC1 := IntegerToTypeid(16_9C9DE465); (* PROCEDURE (x, y: INTEGER): INTEGER *)
CONST UID_PROC2 = 16_20AD399F; (* PROCEDURE (x, y: INTEGER): BOOLEAN *)
(*CONST UID_PROC3 = 16_3CE4D13B;*) (* PROCEDURE (x: INTEGER): INTEGER *)
VAR UID_PROC4 :=  IntegerToTypeid(16_FA03E372); (* PROCEDURE (x, n: INTEGER): INTEGER *)
CONST UID_PROC5 = 16_509E4C68; (* PROCEDURE (x: INTEGER;  n: [0..31]): INTEGER *)
VAR UID_PROC6 := IntegerToTypeid(16_DC1B3625); (* PROCEDURE (x: INTEGER;  n: [0..63]): INTEGER *)
VAR UID_PROC7 := IntegerToTypeid(16_EE17DF2C); (* PROCEDURE (x: INTEGER;  i, n: CARDINAL): INTEGER *)
VAR UID_PROC8 := IntegerToTypeid(16_B740EFD0); (* PROCEDURE (x, y: INTEGER;  i, n: CARDINAL): INTEGER *)
CONST UID_NULL = 16_48EC756E; (* NULL *) (* Occurs in elego/graphicutils/src/RsrcFilter.m3 *)

(* Ctypes.i3.c suggests:
CONST UID_INT8   = 16_66A2A904;
CONST UID_INT16  = 16_7300E1E8;
 VAR UID_INT32  := IntegerToTypeid(16_ADC6066D);
 VAR UID_INT64  := IntegerToTypeid(16_839F750E);
CONST UID_UINT8  = 16_B5B30AA;
 VAR UID_UINT16 := IntegerToTypeid(16_A4B285DE);
CONST UID_UINT32 = 16_6FA2E87D; only on 64bit system, else use UID_INT32 or UID_WORD
*)

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
    points_to_cgtype: CGType := CGType.Void;
    refers_to_typeid: TypeUID := 0;
    (* TODO refers_to_type: Type_t := NIL; *)
    (* int_value := TInt.Zero *) (* TODO replace minMax with this *)
    float_value: Target.Float;
    text_value: TEXT := NIL;
    (* The right generalization here is a set of values the expression
    could possibly have, represented by range lists, not just one range. *)
    minmax_valid := minMaxFalse;
    minmax := int64MinMax;
    cgtype: CGType := CGType.Void;
    typeid: TypeUID := 0;
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

<*UNUSED*>TYPE Expr_Constant_t = Expr_t OBJECT END;
<*UNUSED*>TYPE Expr_AddressOf_t = Expr_t OBJECT END;
<*UNUSED*>TYPE Expr_Ordinal_t = Expr_t OBJECT END;
<*UNUSED*>TYPE Expr_Deref_t = Expr_t OBJECT END;

TYPE Expr_Variable_t = Expr_t OBJECT
    var: Var_t := NIL;
    OVERRIDES
        CText := Expr_Variable_CText;
END;
PROCEDURE Expr_Variable_CText(self: Expr_Variable_t): TEXT =
VAR var := self.var;
BEGIN
    RETURN follow_static_link(self.current_proc, var) & NameT(var.name);
END Expr_Variable_CText;

TYPE Expr_Unary_t  = Expr_t OBJECT (* x: ARRAY [0..0] OF Expr_t; *) END;
TYPE Expr_Binary_t = Expr_t OBJECT (* x: ARRAY [0..1] OF Expr_t; *) END;

TYPE Expr_ConstantInt_t = Expr_t OBJECT OVERRIDES CText := Expr_ConstantInt_CText; END;
PROCEDURE Expr_ConstantInt_CText(self: Expr_ConstantInt_t): TEXT =
BEGIN
    (* ASSERT self.minmax[Min] = self.minmax[Max] *)
    RETURN self.self.TIntLiteral(self.cgtype, self.minmax[Min]);
END Expr_ConstantInt_CText;

TYPE Expr_Cast_t = Expr_Unary_t OBJECT
    force := FALSE;
    OVERRIDES
        CText := Expr_Cast_CText;
END;

<*UNUSED*>PROCEDURE new_Expr_Cast_CText(self: Expr_Cast_t): TEXT =
VAR type_text := self.type_text; (* TODO self.type.text *)
    cgtype := self.cgtype;
    force := self.force;
    left := self.left;
    left_text := left.CText();
    left_type_text := left.type_text; (* TODO left.type.text *)
    remove := 0;
    lparen := "";
    rparen := "";
BEGIN
    <* ASSERT (cgtype = CGType.Void) # (self.type_text = NIL) *> (* TODO type_text *)
    IF NOT force THEN
        (* We might need "force_cast":
        INT16 a, b, c = a + b;
        => a + b is "int" and needs cast to INT16
        *)
        IF type_text # NIL
                AND left_type_text # NIL
                AND (left_type_text = type_text
                    OR Text.Equal(left_type_text, type_text)) THEN
            remove := 1; (* I've never seen this. *)
        ELSE
            type_text := cgtypeToText[cgtype];
            lparen := "(";
            rparen := ")";
            IF left.cgtype = cgtype AND cgtype # CGType.Addr THEN
                remove := 2; (* This happens fairly often. *)
            END;
        END;
    END;
    IF remove > 0 THEN
        RETURN " " & left_text;
    ELSE
        RETURN "(" & lparen & type_text & rparen & "(" & left_text & "))";
    END;
END new_Expr_Cast_CText;

PROCEDURE Expr_Cast_CText(self: Expr_Cast_t): TEXT =
VAR type_text := self.type_text;
    cgtype := self.cgtype;
    force := self.force;
    left := self.left;
    left_text := left.CText();
    remove := 0;
BEGIN
    <* ASSERT (cgtype = CGType.Void) # (self.type_text = NIL) *>
    IF NOT force THEN
        (* We might need "force_cast":
        INT16 a, b, c = a + b;
        => a + b is "int" and needs cast to INT16
        *)
        IF type_text # NIL THEN
            IF left.type_text # NIL AND (left.type_text = type_text OR Text.Equal(left.type_text, type_text)) THEN
                remove := 1; (* I've never seen this. *)
            END;
        ELSE
            type_text := cgtypeToText[cgtype];
            IF left.cgtype = cgtype AND cgtype # CGType.Addr THEN
                remove := 2; (* This happens fairly often. *)
            END;
        END;
    END;
    IF remove > 0 THEN
        RETURN " " & left_text;
    ELSE
        RETURN "((" & type_text & ")(" & left_text & "))";
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
    IF a = NIL THEN RETURN "NIL" END;
    RETURN a;
END TextOrNil;

PROCEDURE TextOrNIL(text: TEXT): TEXT =
BEGIN
    IF text = NIL THEN RETURN "<NIL>" END;
    RETURN text;
END TextOrNIL;

PROCEDURE Expr_Assert(self: Expr_t) =
VAR type_text := self.type_text;
    cgtype := self.cgtype;
    ok := FALSE;
BEGIN
    IF FALSE THEN
        IF NOT ((cgtype = CGType.Void) # (type_text = NIL)) THEN
            RTIO.PutText("cgtype:" & cgtypeToText[cgtype] & " type_text:" & TextOrNil(type_text) & "\n");
            RTIO.Flush();
            <* ASSERT (cgtype = CGType.Void) # (type_text = NIL) *>
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
    cgtype: CGType;
    typeid: TypeUID := -1;
    points_to_cgtype: CGType; (* future *)
    type_text: TEXT := NIL; (* TODO replace with type:Type_t.text *)
    const := FALSE;
    imported := FALSE;
    exported := FALSE;
    global := FALSE;
    byte_size := -1; (* esp. for structs *)
    up_level := FALSE; (* local accessed from nested function *)
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
        Declare := Param_Declare;
END;

(* TYPE LocalVar = Var_t;  FUTURE *)
(* TYPE GlobalVar = Var_t;  FUTURE *)

PROCEDURE Var_Init(var: Var_t): Var_t =
BEGIN
    var.is_static_link := (var.name = var.self.static_link_id);
    var.name := Var_FixName(var.self, var.name, var.exported OR var.imported);
    RETURN var;
END Var_Init;

PROCEDURE VarNameT(v: M3CG.Var): TEXT =
BEGIN
    TYPECASE v OF
        NULL => RETURN "NIL"
        | Var_t(var) => RETURN Var_Name(var);
        ELSE RETURN "VarNotType"
    END;
END VarNameT;

TYPE Block_t = OBJECT
    sibling: Block_t := NIL;
    parent: Block_t := NIL;
    child: Block_t := NIL;
    proc: Proc_t := NIL;
    vars: Var_t := NIL;
    top_in_proc: Block_t := NIL;
    name: TEXT := NIL; (* for frame *)
END;

TYPE Proc_t = M3CG.Proc OBJECT
    name: Name := 0;
    pending_output := FALSE;
    output := FALSE;
    op_start := 0; (* M3CG_MultiPass.Replay range *)
    op_end := 0;   (* M3CG_MultiPass.Replay range *)
    parameter_count := 0; (* FUTURE: remove this (same as NUMBER(params^)) *)
    parameter_count_without_static_link := 0; (* FUTURE: remove this (same as NUMBER(params^) - ORD(add_static_link)) *)
    return_type: CGType;
    return_type_text: TEXT := NIL;
    level := 0;
    callingConvention: CallingConvention;
    exported := FALSE;
    imported := FALSE;
    parent: Proc_t := NIL;
    params: REF ARRAY OF Var_t(*Param_t*);
    locals: RefSeq.T := NIL; (* Var_t *)
    uplevels := FALSE;
    is_exception_handler := FALSE;
    is_RTHooks_Raise := FALSE;
    is_setjmp := FALSE;
    omit_prototype := FALSE;
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
    id := proc.FrameType();
BEGIN
    IF proc.forward_declared_frame_type THEN
        RETURN;
    END;
    print(self, "/*Proc_ForwardDeclareFrameType*/struct " & id & ";typedef struct " & id & " " & id & ";\n");
    proc.forward_declared_frame_type := TRUE;
END Proc_ForwardDeclareFrameType;

(*---------------------------------------------------------------------------*)
PROCEDURE Text_Starts(s, t : TEXT): BOOLEAN =
(* based on m3-libs/sysutils TextUtils.StartsWith *)
  VAR tlen := Text.Length(t);
  BEGIN
    RETURN tlen <= Text.Length(s) AND Text.Equal(Text.Sub(s, 0, tlen), t);
  END Text_Starts;

(*---------------------------------------------------------------------------*)

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
        IF length > prefix_length AND Text_Starts(name, prefix) THEN
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
            comment(self, "IsNameExceptionHandler:" & name);
            RETURN TRUE;
        END;
    END;
    RETURN FALSE;
END IsNameExceptionHandler;

PROCEDURE Proc_Init(proc: Proc_t; self: T): Proc_t =
VAR name := proc.name;
    parameter_count := proc.parameter_count;
    is_common := proc.parent = NIL
                 AND (proc.exported = TRUE OR proc.imported = TRUE)
                 AND proc.level = 0;
    is_common_void := is_common AND proc.return_type = CGType.Void;
    is_common_not_void := is_common AND NOT is_common_void;
    is_RTHooks_ReportFault := is_common_void
                              AND name = self.RTHooks_ReportFault_id
                              AND parameter_count = 2;
    is_RTHooks_AssertFailed := is_common_void
                               AND name = self.RTHooks_AssertFailed_id
                               AND parameter_count = 3;
    is_setjmp := is_common_not_void AND parameter_count = 1 AND name = self.setjmp_id;
    is_alloca := is_common_not_void AND parameter_count = 1 AND name = self.alloca_id;
BEGIN
    (* Omit a few prototypes that the frontend might have slightly wrong,
       e.g. alloca(unsigned int vs. unsigned long vs. unsigned long long)
            vs. compiler intrinsic
       e.g. setjmp(void* ) vs. setjmp(array of something) vs. compiler intrinsic
       setjmp must be referenced "inline", cannot be delegated to a C helper (except for a macro).
    *)
    proc.is_setjmp := is_setjmp;
    proc.omit_prototype := is_setjmp OR is_alloca;
                           (* TODO
                           - add CGType.Jmpbuf
                           - #include <setjmp.h> if there are any
                             instances of CGType.Jmpbuf
                           - render CGType.Jmpbuf as "jmp_buf" *)
    proc.is_RTHooks_Raise := is_common_void
                             AND name = self.RTHooks_Raise_id
                             AND parameter_count = 4;
    proc.is_RTException_Raise := is_common_void
                                 AND name = self.RTException_Raise_id
                                 AND parameter_count = 1;
    IF is_RTHooks_ReportFault THEN
        self.RTHooks_ReportFault_imported_or_declared := TRUE;
    END;
    proc.no_return := is_RTHooks_AssertFailed OR is_RTHooks_ReportFault OR proc.is_RTException_Raise OR proc.is_RTHooks_Raise;
    IF proc.no_return THEN
        no_return(self);
    END;
    proc.self := self;
    proc.name := Proc_FixName(proc.self, name);
    name := proc.name;
    proc.is_exception_handler := proc.level > 0 AND parameter_count = 1 AND IsNameExceptionHandler(self, NameT(name));
    proc.parameter_count_without_static_link := parameter_count;
    proc.add_static_link := proc.level > 0;
    INC(proc.parameter_count, ORD(proc.add_static_link));
    proc.locals := NEW(RefSeq.T).init();
    proc.blocks := NEW(RefSeq.T).init();
    proc.block_stack := NEW(RefSeq.T).init();
    proc.params := NEW(REF ARRAY OF Var_t, proc.parameter_count);
    proc.ForwardDeclareFrameType(); (* TODO do not always do this *)

    IF is_setjmp THEN
      include_setjmp_h(self);
    END;

    RETURN proc;
END Proc_Init;

(*PROCEDURE Proc_FrameName(p: Proc_t): TEXT = BEGIN RETURN NameT(p.name) & "_Frame"; END Proc_FrameName;*)
PROCEDURE Proc_FrameName(<*UNUSED*>p: Proc_t): TEXT = BEGIN RETURN "_frame"; END Proc_FrameName;
PROCEDURE Proc_FrameType(p: Proc_t): TEXT = BEGIN RETURN NameT(p.name) & "_Frame_t"; END Proc_FrameType;
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

"#define m3_eq(T, x, y) (((T)(x)) == ((T)(y)))",
"#define m3_ne(T, x, y) (((T)(x)) != ((T)(y)))",
"#define m3_gt(T, x, y) (((T)(x)) > ((T)(y)))",
"#define m3_ge(T, x, y) (((T)(x)) >= ((T)(y)))",
"#define m3_lt(T, x, y) (((T)(x)) < ((T)(y)))",
"#define m3_le(T, x, y) (((T)(x)) <= ((T)(y)))",
"#define m3_check_range(T, value, low, high) (((T)(value)) < ((T)(low)) || ((T)(high)) < ((T)(value)))",
"#define m3_xor(T, x, y) (((T)(x)) ^ ((T)(y)))",

"#ifdef _MSC_VER",
"#define _CRT_SECURE_NO_DEPRECATE",
"#define _CRT_NONSTDC_NO_DEPRECATE",
"#pragma warning(disable:4616) /* there is no warning x (unavoidable if targeting multiple compiler versions) */",
"#pragma warning(disable:4619) /* there is no warning x (unavoidable if targeting multiple compiler versions) */",
"#pragma warning(disable:4100) /* unused parameter */",
"#pragma warning(disable:4115) /* named type definition in parentheses */",
"#pragma warning(disable:4127) /* conditional expression is constant */",
"#pragma warning(disable:4201) /* nonstandard extension: nameless struct/union */",
"#pragma warning(disable:4214) /* nonstandard extension: bitfield other than int */",
"#pragma warning(disable:4209) /* nonstandard extension: benign re-typedef */",
"#pragma warning(disable:4226) /* nonstandard extension: __export */",
"#pragma warning(disable:4242) /* 'return': conversion from '' to '', possible loss of data */",
"#pragma warning(disable:4255) /* () change to (void) */",
"#pragma warning(disable:4310) /* cast truncates constant value */", (* TODO fix these UINT64/INT64 confusion *)
"#pragma warning(disable:4514) /* unused inline function removed */",
"#pragma warning(disable:4668) /* #if of undefined symbol */",
"#pragma warning(disable:4705) /* statement has no effect for merely using assert() at -W4 */",
"#pragma warning(disable:4715) /* not all control paths return a value */",
"#pragma warning(disable:4716) /* must return a value */",
"#pragma warning(disable:4820) /* padding inserted */",
"#pragma warning(disable:5045) /* Compiler will insert Spectre mitigation for memory load if /Qspectre switch specified */", (* TODO fix *)
"#endif",
(* TODO ideally these are char* for K&R or ideally absent when strong
   typing and setjmp work done *)
"typedef char* ADDRESS;",
"typedef char* STRUCT;",
"typedef signed char INT8;", (* m3core.h is a bit more portable here, via limits.h; TODO: C99 *)
"typedef unsigned char UINT8;",
"typedef short INT16;",
"typedef unsigned short UINT16;",
"typedef int INT32;",
"typedef unsigned int UINT32;",
"#if defined(_MSC_VER) || defined(__DECC) || defined(__DECCXX) || defined(__int64)", (* matches m3core.h *)
"typedef __int64 INT64;",
"typedef unsigned __int64 UINT64;",
"#define  INT64_(x) x##I64",
"#define UINT64_(x) x##UI64",
"#else",
"typedef long long INT64;",
"typedef unsigned long long UINT64;",
"#define  INT64_(x) x##LL",
"#define UINT64_(x) x##ULL",
"#endif",

(* This chunk can/should be moved to HelperFunctions i.e. memcmp | memmove |
   memcpy | memset | copy_n | zero | set_compare, esp. to reduce #include
   <stddef.h>.
   NOTE: While the vast majority of systems, except NT and VMS, have unsigned
   long the same size as size_t, size_t could also be unsigned int on 32bit
   systems or unsigned long long on 64bit systems, and the correct type
   should be used, unless we have an intermediate function and cast.
   NOTE: NT and likely VMS are the exception the previous -- 32bit long always.
*)
"#if defined(_WIN64)",
"typedef UINT64 size_t;",
"#elif defined(_WIN32)",
"typedef unsigned size_t;",
"#elif defined(__SIZE_TYPE__)", (* gcc, clang *)
"typedef __SIZE_TYPE__ size_t;",
"#elif defined(__APPLE__) /*|| defined(_LP64) || defined(__LP64__)*/",
"typedef unsigned long size_t;",
"#else",
(*"typedef unsigned int size_t;",*)
"#include <stddef.h>", (* try to remove this, it is slow -- need size_t *)
"#endif",

"/* http://c.knowcoding.com/view/23699-portable-alloca.html */",
"/* Find a good version of alloca. */",
"#ifndef alloca",
"# ifdef __GNUC__",
"#  define alloca __builtin_alloca",
"# elif defined(__DECC) || defined(__DECCXX)",
"#  define alloca(x) __ALLOCA(x)",
"# elif defined(_MSC_VER)",
"   void * __cdecl _alloca(size_t size);",
"#  define alloca _alloca",
"# else",
(* This is verified correct for Solaris, documented correct for AIX. TODO: Irix, HP-UX, etc. *)
"#  include <alloca.h>",
"# endif",
"#endif",

"typedef float REAL;",
"typedef double LONGREAL;",
"typedef /*long*/ double EXTENDED;",

"#ifdef __cplusplus",
"extern \"C\" {",
"#endif",

"#if !defined(_MSC_VER) && !defined(__cdecl)",
"#define __cdecl /* nothing */",
"#endif",
"#if !defined(_MSC_VER) && !defined(__stdcall)",
"#define __stdcall /* nothing */",
"#endif",

"#define STRUCT(n) struct_##n##_t", (* TODO prune if not used *)
(* TODO struct1 and struct2 should not be needed.
   struct4 and struct8 can go away when we make open arrays and jmpbufs
   better typed, and then struct can go also *)
"#define STRUCT1(n) typedef struct { volatile char a[n]; }     STRUCT(n);", (* TODO prune if not used *)
"#define STRUCT2(n) typedef struct { volatile short a[n/2]; }  STRUCT(n);", (* TODO prune if not used *)
"#define STRUCT4(n) typedef struct { volatile int a[n/4]; }    STRUCT(n);", (* TODO prune if not used *)
"#define STRUCT8(n) typedef struct { volatile UINT64 a[n/8]; } STRUCT(n);", (* TODO prune if not used *)
"#ifdef __cplusplus",
"#define DOTDOTDOT ...",
"#else",
"#define DOTDOTDOT",
"#endif",

""};

<*NOWARN*>CONST Suffix = ARRAY OF TEXT {
"\n#ifdef __cplusplus",
"} /* extern \"C\" */",
"#endif"
};

CONST intLiteralPrefix = ARRAY CGType OF TEXT {
    "",  "((INT8)",             (* 0 1 *)
    "",  "((INT16)",            (* 2 2 *)
    "", "",                     (* 4 3 *)
    "UINT64_(", "INT64_(",      (* 6 7 *)
    NIL, ..  (* 8 REAL 9 LONGREAL A EXTENDED B ADDRESS C STRUCT D void *)
};

CONST intLiteralSuffix = ARRAY CGType OF TEXT {
    "U",  ")",  (* 0 1 UINT8  INT8 *)
    "U",  ")",  (* 2 3 UINT16 INT16 *)
    "U", "",    (* 4 5 UINT32 INT32 *)
    ")", ")",   (* 6 7 UINT64 INT64 *)
    NIL, ..     (* 8 REAL 9 LONGREAL A EXTENDED B ADDRESS C STRUCT D void *)
};

CONST cgtypeToText = ARRAY CGType OF TEXT {
    Text_uint8,  Text_int8,   (* 0 1 *)
    Text_uint16, Text_int16,  (* 2 3 *)
    Text_uint32, Text_int32,  (* 4 5 *)
    Text_uint64, Text_int64,  (* 6 7 *)
    "float",  (* REAL *)        (* 8 *)
    "double", (* LONGREAL *)    (* 9 *)
    "EXTENDED",                 (* A *)
    Text_address,               (* B *)
    "STRUCT",                   (* C *)
    "void"                      (* D *)
};

TYPE IntegerTypes = [CGType.Word8 .. CGType.Int64];

CONST cgtypeIsInteger = ARRAY CGType OF BOOLEAN {
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
CONST cgtypeIsUnsignedInt = ARRAY CGType OF BOOLEAN {
    TRUE, FALSE, (* 8 *)
    TRUE, FALSE, (* 16 *)
    TRUE, FALSE, (* 32 *)
    TRUE, FALSE, (* 64 *)
    FALSE, FALSE, FALSE, (* real, longreal, extended *)
    FALSE, FALSE, FALSE (* address, struct, void *)
};
CONST cgtypeIsSignedInt = ARRAY CGType OF BOOLEAN {
    FALSE, TRUE, (* 8 *)
    FALSE, TRUE, (* 16 *)
    FALSE, TRUE, (* 32 *)
    FALSE, TRUE, (* 64 *)
    FALSE, FALSE, FALSE, (* real, longreal, extended *)
    FALSE, FALSE, FALSE (* address, struct, void *)
};
<*UNUSED*>CONST cgtypeSizeBits = ARRAY CGType OF [0..64] {
    8, 8, 16, 16, 32, 32, 64, 64,
    32, 64, 64, (* real, longreal, extended *)
    0, 0, 0 (* address, struct, void *)
};
CONST cgtypeSizeBytes = ARRAY CGType OF [0..8] {
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
    CGType.Word8, CGType.Word8,
    CGType.Word16, CGType.Word16,
    CGType.Word32, CGType.Word32,
    CGType.Word64, CGType.Word64
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

PROCEDURE push(self: T; <*UNUSED*>type: CGType; expression: Expr_t) =
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
VAR length := 0;
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

PROCEDURE NewInternal (cfile: Wr.T): T =
VAR self := NEW (T);
BEGIN
    self.typeidToType := NEW(SortedIntRefTbl.Default).init(); (* FUTURE? *)
    self.multipass := NEW(Multipass_t).Init();
    self.multipass.reuse_refs := TRUE; (* TODO: change them all to integers *)
    self.multipass.self := self;
    self.c := cfile;
    self.fields := NEW(TextSeq.T).init();
    self.initializer := NEW(TextSeq.T).init();
    self.debug_initializer := NEW(CharSeq.T).init();
    self.stack := NEW(RefSeq.T).init(); (* Expr_t *)
    self.params := NEW(TextSeq.T).init(); (* TODO Expr_t *)
    self.pendingTypes := NEW(RefSeq.T).init();
    self.dummy_proc := NEW(Proc_t);             (* avoid null derefs for indirect calls *)
    self.proc_being_called := self.dummy_proc;  (* avoid null derefs for indirect calls *)
    self.imported_procs := NEW(RefSeq.T).init();
    self.declared_procs := NEW(RefSeq.T).init();
    self.procs_pending_output := NEW(RefSeq.T).init();
    RETURN self;
END NewInternal;

PROCEDURE New0 (cfile: Wr.T): M3CG.T =
VAR self := NewInternal (cfile);
BEGIN
    RETURN self.multipass;
END New0;

PROCEDURE TextRemoveAtEnd (VAR t: TEXT; a: TEXT): BOOLEAN =
VAR result := TextUtils.EndsWith (t, a);
BEGIN
  IF result THEN
    t := Text.Sub (t, 0, Text.Length (t) - Text.Length (a));
  END;
  RETURN result;
END TextRemoveAtEnd;

<*UNUSED*>PROCEDURE TextSubstituteAtEnd (VAR t: TEXT; a, b: TEXT): BOOLEAN =
VAR result := TextRemoveAtEnd (t, a);
BEGIN
  IF result THEN
    t := t & b;
  END;
  RETURN result;
END TextSubstituteAtEnd;

PROCEDURE TextToId (VAR t: TEXT) =
BEGIN
  t := TextUtils.Substitute (t, ".", "_");
  t := TextUtils.Substitute (t, "-", "_");
  (* TODO more ways to turn into valid identifier prefix? Handle in m3front. *)
END TextToId;

PROCEDURE New (<*UNUSED*>library (* or program *): TEXT;
               <*UNUSED*>source (* lacks extension .m3 or .i3 *): M3ID.T;
               cfile: Wr.T;
               target_name: TEXT): M3CG.T =
VAR self := NewInternal (cfile);
BEGIN
  EVAL TextRemoveAtEnd (target_name, "3.c") OR TextRemoveAtEnd (target_name, "3.cpp");
  TextToId (target_name);
  (* library is like "cm3"
   * source is like "Arg" unfortunately without module or interface indicated.
   * target_name is like "Arg.i3.c"
   *
   * We want to be able to concatenate m3c output files across a static link.
   * m3front gives us unnamed linked symbols like segments.
   * self.unique and an incrementing counter is used to generate unique symbols.
   *
   * TODO m3front should satisfy these requirements instead.
   *)
  self.unique := target_name & "_";
  RETURN self.multipass;
END New;

(*---------------------------------------------------------------------------*)

(* Poorly defined types that are just ADDRESS *)
(* TODO Some/all of these can be better typed *)

TYPE AddressType_t = Type_CanBeDefinedTrue_t OBJECT
OVERRIDES
    define := addressType_define;
END;

PROCEDURE addressType_define(type: AddressType_t; self: T) =
BEGIN
    print(self, "/*1addressType_define*/typedef ADDRESS " & type.text & ";\n");
    print(self, "/*2addressType_define*/typedef ADDRESS " & TypeIDToText(type.typeid) & ";\n");
END addressType_define;

(*---------------------------------------------------------------------------*)

PROCEDURE DeclareBuiltinTypes(self: T) =
VAR widechar_target_type: Target.CGType; 
    widechar_last := 0;
    type: Type_t := NIL;
BEGIN

    (* Builtin/base types start out as state := Type_State.CanBeDefined or Defined  *)

    ifndef (self, "INTEGER"); (* m3core.h interop *)
    type := NEW(Integer_t, state := Type_State.CanBeDefined, cgtype := Target.Integer.cg_type, typeid := UID_INTEGER, text := "INTEGER");
    self.Type_Init(type);
    endif (self);

    ifndef (self, "WORD_T"); (* m3core.h interop *)
    type := NEW(Integer_t, state := Type_State.CanBeDefined, cgtype := Target.Word.cg_type, typeid := UID_WORD, text := "WORD_T");
    self.Type_Init(type);
    endif (self);

    print(self, "typedef WORD_T CARDINAL;\n");

    type := NEW(Integer_t, state := Type_State.Defined, cgtype := Target.Int64.cg_type, typeid := UID_LONGINT, text := "INT64");
    self.Type_Init(type);

    type := NEW(Integer_t, state := Type_State.Defined, cgtype := Target.Word64.cg_type, typeid := UID_LONGWORD, text := "UINT64");
    self.Type_Init(type);

    type := NEW(Float_t, state := Type_State.CanBeDefined, cgtype := Target.Real.cg_type, typeid := UID_REEL, text := "REAL");
    self.Type_Init(type);

    type := NEW(Float_t, state := Type_State.CanBeDefined, cgtype := Target.Longreal.cg_type, typeid := UID_LREEL, text := "LONGREAL");
    self.Type_Init(type);

    type := NEW(Float_t, state := Type_State.CanBeDefined, cgtype := Target.Extended.cg_type, typeid := UID_XREEL, text := "EXTENDED");
    self.Type_Init(type);

(* Enum_t? *)
    type := NEW(Integer_t, state := Type_State.CanBeDefined, cgtype := Target.Word8.cg_type, typeid := UID_BOOLEAN, (* max := IntToTarget(self, 1), *) text := "BOOLEAN");
    self.Type_Init(type);

    type := NEW(Integer_t, state := Type_State.CanBeDefined, cgtype := Target.Word8.cg_type, typeid := UID_CHAR, (* max := IntToTarget(self, 16_FF), *) text := "UCHAR");
    self.Type_Init(type);

    widechar_target_type := Target.Word16.cg_type; 
    widechar_last := 16_FFFF; (* The defaults. *) 
    IF self.multipass.op_counts[M3CG_Binary.Op.widechar_size] > 0 THEN 
      WITH op_widechar_size_list = self.multipass.op_data[M3CG_Binary.Op.widechar_size] DO
        TYPECASE op_widechar_size_list[LAST(op_widechar_size_list^)] 
        OF NULL => 
        | M3CG_MultiPass.widechar_size_t (op_widechar_size) => 
          IF op_widechar_size.size = 32 THEN 
            widechar_target_type := Target.Word32.cg_type;
            widechar_last := 16_10FFFF;
          END;
        ELSE
        END;
      END;
    END;

(* Enum_t? *)
    type := NEW(Integer_t, state := Type_State.CanBeDefined, cgtype := widechar_target_type,
                typeid := UID_WIDECHAR, (* max := IntToTarget(self, widechar_last), *) text := "WIDECHAR");
    self.Type_Init(type);

    (* self.declareTypes.declare_subrange(UID_RANGE_0_31, UID_INTEGER, TInt.Zero, IntToTarget(self, 31), Target.Integer.size); *)
    (* self.declareTypes.declare_subrange(UID_RANGE_0_63, UID_INTEGER, TInt.Zero, IntToTarget(self, 63), Target.Integer.size); *)
    
    TYPE AddressTypeInit_t = RECORD
        typeid: TypeUID := 0;
        text: TEXT := NIL;
    END;
    VAR addressTypes := ARRAY [0..13] OF AddressTypeInit_t {
        AddressTypeInit_t {UID_MUTEX, "MUTEX"},
        AddressTypeInit_t {UID_TEXT, "TEXT"},
        AddressTypeInit_t {UID_ROOT, "ROOT"},
        AddressTypeInit_t {UID_UNTRACED_ROOT, "UNTRACED_ROOT"},
        AddressTypeInit_t {UID_REFANY, "REFANY"},
        AddressTypeInit_t {UID_ADDR, Text_address},
        AddressTypeInit_t {UID_PROC1, "PROC1"},
        AddressTypeInit_t {UID_PROC2, "PROC2"},
        (*AddressTypeInit_t {UID_PROC3, "PROC3"},*)
        AddressTypeInit_t {UID_PROC4, "PROC4"},
        AddressTypeInit_t {UID_PROC5, "PROC5"},
        AddressTypeInit_t {UID_PROC6, "PROC6"},
        AddressTypeInit_t {UID_PROC7, "PROC7"},
        AddressTypeInit_t {UID_PROC8, "PROC8"},
        AddressTypeInit_t {UID_NULL, "M3_NULL_T"}
    };
    BEGIN
        FOR i := FIRST(addressTypes) TO LAST(addressTypes) DO
            WITH a = addressTypes[i] DO
                type := NEW(AddressType_t, state := Type_State.CanBeDefined, cgtype := Target.Address.cg_type, typeid := a.typeid, text := a.text);
                self.Type_Init(type);
            END;
        END;
    END;
END DeclareBuiltinTypes;

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

PROCEDURE Prefix_Print(self: T; multipass: Multipass_t) =
BEGIN
    self.comment("begin unit");
(*  This breaks portable distribution format.
    self.comment("M3_TARGET = ", Target.System_name);
    self.comment("M3_WORDSIZE = ", IntToDec(Target.Word.size));
*)
    self.static_link_id := M3ID.Add("_static_link");
    self.alloca_id := M3ID.Add("alloca");
    self.setjmp_id := M3ID.Add("m3_setjmp");
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
END Prefix_Print;

PROCEDURE multipass_end_unit(self: Multipass_t) =
(* called at the end of the first pass -- we have everything
   in memory now, except for the end_unit itself.

   This function is in control of coordinating the passes.
 *)
VAR x := self.self;
    index := 0;
BEGIN
    x.typedef_defined := NEW(TextSetDef.T).init(251);
    M3CG_MultiPass.end_unit(self); (* let M3CG_MultiPass do its usual last step *)
    self.Replay(x, index, self.op_data[M3CG_Binary.Op.begin_unit]);
    self.Replay(x, index, self.op_data[M3CG_Binary.Op.set_error_handler]);
    Prefix_Print(x, self);

    (* declare/define types *)
    DeclareTypes(self);

    HelperFunctions(self);
    GetStructSizes(self); (* TODO remove this when we finish strong typing *)

    (* forward declare functions/variables in this module and imports *)

    x.comment("begin: imports");
    self.Replay(NEW(Imports_t, self := x), index);
    x.comment("end: imports");

    (* discover all locals, including temps and params and check_* *)

    x.comment("begin: locals");
    x.temp_vars := NEW(REF ARRAY OF Var_t, NUMBER(self.data^));
    self.Replay(NEW(Locals_t, self := x), x.op_index);
    x.comment("end: locals");

    (* segments/globals *)

    x.comment("begin: segments/globals");
    self.Replay(NEW(Segments_t, self := x), index);
    x.comment("end: segments/globals");

    (* labels, variables, maybe procedures -- which are used *)
    MarkUsed(self);

    (* last pass *)

    self.Replay(x, x.op_index);
END multipass_end_unit;

PROCEDURE begin_unit(self: T; <*UNUSED*>optimize: INTEGER) =
(* The first call in a particular pass. *)
BEGIN
    self.in_proc := FALSE;
    self.current_proc := NIL;
    self.in_proc_call := FALSE;
    self.abort_in_call := FALSE;
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
    file := TextUtils.SubstChar(file, '\\', '/');
    file := Target.CleanupSourcePath(file);

    IF DebugVerbose(self) THEN
        self.comment("set_source_file file:", file);
    ELSE
        self.comment("set_source_file");
    END;
    self.file := file;
    SetLineDirective(self);
END set_source_file;

PROCEDURE set_source_line(self: T; line: INTEGER) =
(* Sets the current source line number. Subsequent statements
and expressions are associated with this source location. *)
BEGIN
    IF self.debug > 3 THEN
        self.comment("set_source_line ", IntToDec(line));
    ELSIF self.debug > 2 THEN
        self.comment("set_source_line");
    END;
    self.line := line;
    SetLineDirective(self);
END set_source_line;

(*------------------------------------------- debugging type declarations ---*)

<*NOWARN*>PROCEDURE declare_typename(declareType: DeclareTypes_t; typeid: TypeUID; name: Name) =
VAR nameText := NameT(name);
    self := declareType.self;
BEGIN
  IF DebugVerbose(self) THEN
    self.comment("declare_typename typeid:", TypeIDToText(typeid), " name:" & TextOrNIL(nameText));
  ELSE
    self.comment("declare_typename");
  END;

  IF name = M3ID.NoID THEN
    RETURN;
  END;

  TextToId (nameText);
  (* typename is like pointer but without the star and without a hash in the name *)
  self.Type_Init (NEW (Typename_t, text := nameText, refers_to_typeid := typeid));

END declare_typename;

PROCEDURE TypeIDToText(x: M3CG.TypeUID): TEXT =
BEGIN
    RETURN "T" & UIntToHex(Word.And(16_FFFFFFFF, x));
END TypeIDToText;

PROCEDURE declare_array(self: DeclareTypes_t; typeid, index_typeid, element_typeid: TypeUID; bit_size: BitSize; <*UNUSED*>element_typename: Name) =
VAR x := self.self;
BEGIN
    IF DebugVerbose(x) THEN
        x.comment("declare_array typeid:" & TypeIDToText(typeid)
            & " index_typeid:" & TypeIDToText(index_typeid)
            & " element_typeid:" & TypeIDToText(element_typeid)
            & " bit_size:" & IntToDec(bit_size));
    ELSE
        x.comment("declare_array");
    END;

    x.Type_Init(NEW(FixedArray_t, typeid := typeid, index_typeid := index_typeid, element_typeid := element_typeid, bit_size := bit_size));
  END declare_array;

PROCEDURE declare_open_array(self: DeclareTypes_t; typeid, element_typeid: TypeUID; bit_size: BitSize; <*UNUSED*>element_typename: Name) =
VAR x := self.self;
BEGIN
    IF TRUE OR DebugVerbose(x) THEN
        x.comment("declare_open_array typeid:" & TypeIDToText(typeid)
            & " element_typeid:" & TypeIDToText(element_typeid)
            & " bit_size:" & IntToDec(bit_size));
    ELSE
        x.comment("declare_open_array");
    END;
    <* ASSERT bit_size MOD 32 = 0 *>
    x.Type_Init(NEW(OpenArray_t,
        typeid := typeid,
        element_typeid := element_typeid,
        bit_size := bit_size));
(*
    WITH element_type = TypeidToType_Get(element_typeid) DO
        IF element_type = NIL THEN
            RTIO.PutText("declare_array nil element_type\n");
            RTIO.Flush();
        END;
        ifndef (self, TypeIDToText (typeid)); (* ifdef so multiple files can be concatenated and compiled at once *)
        print(self, "/*declare_open_array*/typedef struct {");
        print(self, element_type.text);
        print(self, "* _elts; CARDINAL _size");
        IF bit_size > Target.Integer.size * 2 THEN
            print(self, "s[");
            print(self, IntToDec((bit_size - Target.Integer.size) DIV Target.Integer.size));
            print(self, "]");
        END;
        print(self, ";}" & element_type.text & ";");
        EVAL typeidToType.put(typeid, NEW(OpenArray_t,
                                          typeid := typeid,
                                          bit_size := bit_size,
                                          element_typeid := element_typeid,
                                          element_type := element_type));
        endif(self); (* ifdef so multiple files can be concatenated and compiled at once *)
    END;
*)
  END declare_open_array;

PROCEDURE declare_enum(self: DeclareTypes_t; typeid: TypeUID; element_count: INTEGER; bit_size: BitSize) =
VAR x := self.self;
    enum: Enum_t := NIL;
BEGIN
    IF DebugVerbose(x) THEN
        x.comment("declare_enum typeid:" & TypeIDToText(typeid)
            & " bit_size:" & IntToDec(bit_size)
            & " element_count:" & IntToDec(element_count));
    ELSE
        x.comment("declare_enum");
    END;
    Assert (x, bit_size = 8 OR bit_size = 16 OR bit_size = 32, "bit_size = 8 OR bit_size = 16 OR bit_size = 32");
    Assert (x, element_count > 0, "element_count > 0");
    Assert (x, self.enum = NIL, "self.enum = NIL");
    Assert (x, self.enum_value = -1, "self.enum_value = -1");

    enum := NEW(Enum_t,
                typeid := typeid,
                min := TInt.Zero,
                max := IntToTarget(x, element_count - 1),
                names := NEW(REF ARRAY OF Name, element_count),
                cgtype := BitsToCGUInt[bit_size],
                text := TypeIDToText(typeid));
    Assert (x, self.enum = NIL, "self.enum = NIL");
    Assert (x, self.enum_value = -1, "self.enum_value = -1");
    self.enum := enum;
    self.enum_element_count := element_count;
    self.enum_value := 0;
END declare_enum;

PROCEDURE declare_enum_elt(self: DeclareTypes_t; name: Name) =
VAR enum_value := self.enum_value;
    enum_element_count := self.enum_element_count;
    x := self.self;
BEGIN
    IF DebugVerbose(x) THEN
        x.comment("declare_enum_elt name:", NameT(name), "=", IntToDec(self.enum_value));
    ELSE
        x.comment("declare_enum_elt");
    END;

    IF enum_value < 0 OR enum_value >= enum_element_count THEN
        Err(x, "declare_enum_elt overflow");
        RETURN;
    END;

    Assert (x, self.enum # NIL, "self.enum # NIL");
    Assert (x, self.enum.names # NIL, "self.enum.names # NIL");

    x.comment("declare_enum_elt: NUMBER(self.enum.names^):", IntToDec(NUMBER(self.enum.names^)));
    x.comment("declare_enum_elt: enum_element_count:", IntToDec(enum_element_count));
    Assert (x, NUMBER(self.enum.names^) = enum_element_count, "NUMBER(self.enum.names^) = enum_element_count");

    self.enum.names[enum_value] := name;
    INC(enum_value);
    IF enum_value = enum_element_count THEN
        x.Type_Init(self.enum);
        self.enum := NIL;
        self.enum_value := -1;
        RETURN;
    END;
    self.enum_value := enum_value;
  END declare_enum_elt;

PROCEDURE declare_packed(self: DeclareTypes_t; typeid: TypeUID; bit_size: BitSize; base_typeid: TypeUID; <*UNUSED*>base_typename: Name) =
VAR x := self.self;
BEGIN
    IF DebugVerbose(x) THEN
        x.comment("declare_packed typeid:" & TypeIDToText(typeid)
            & " bit_size:" & IntToDec(bit_size)
            & " base:" & TypeIDToText(base_typeid));
    ELSE
        x.comment("declare_packed");
    END;
    x.Type_Init(NEW(Packed_t,
        typeid := typeid,
        base_typeid := base_typeid,
        bit_size := bit_size));
END declare_packed;

TYPE DeclareTypes_t = M3CG_DoNothing.T OBJECT
    self: T := NIL;

    (* declare_enum, declare_enum_elt *)
    enum: Enum_t := NIL;
    enum_value := -1;
    enum_element_count := -1;

    (* declare_record, declare_field *)
    record: Record_t := NIL;
    previous_field: Field_t := NIL;
    field_count, field_index := 0;
    procType: ProcType_t := NIL;

OVERRIDES
    declare_typename := declare_typename;
    declare_enum := declare_enum;
    declare_enum_elt := declare_enum_elt;
    declare_record := declare_record;
    declare_field := declare_field;
    declare_pointer := declare_pointer;
    declare_set := declare_set;
    declare_packed := declare_packed;
    declare_exception := declare_exception;
    reveal_opaque := reveal_opaque;
    declare_array := declare_array;
    declare_open_array := declare_open_array;
    declare_indirect := declare_indirect;
    declare_proctype := declare_proctype;
    declare_formal := declare_formal;
    declare_raises := declare_raises;
    declare_object := declare_object;
    declare_method := declare_method;
    declare_opaque := declare_opaque;
    declare_subrange := declare_subrange;
END;

PROCEDURE DeclareTypes_FlushOnce(x: T) =
VAR size := x.pendingTypes.size();
BEGIN
    FOR i := 1 TO size DO
        WITH type = NARROW(x.pendingTypes.remlo(), Type_t) DO
            IF type.IsDefined() THEN
                (* nothing *)
                x.comment("DeclareTypes_FlushOnce IsDefined:" & type.text);
            ELSIF type.CanBeDefined(x) THEN
                x.comment("DeclareTypes_FlushOnce CanBeDefined:" & type.text);
                type.Define(x);
            ELSE
                x.comment("DeclareTypes_FlushOnce pending:" & type.text);
                x.pendingTypes.addhi(type);
            END;
        END;
    END;
END DeclareTypes_FlushOnce;

PROCEDURE DeclareTypes(multipass: Multipass_t) =
VAR x := multipass.self;
    self := NEW(DeclareTypes_t, self := x);
    index := 0;
    size_before, size_after := 0;
BEGIN
    x.comment("begin: DeclareTypes");
    DeclareBuiltinTypes(x); (* This must be before replay. *)
    multipass.Replay(self, index);

    size_before := x.pendingTypes.size();
    WHILE size_before > 0 DO
        DeclareTypes_FlushOnce(x);
        size_after := x.pendingTypes.size();
        IF size_after >= size_before THEN
            Err(x, "DeclareTypes not progressing");
        END;
        size_before := size_after;
    END;

    x.comment("end: DeclareTypes");
END DeclareTypes;

PROCEDURE declare_record(self: DeclareTypes_t; typeid: TypeUID; bit_size: BitSize; field_count: INTEGER) =
VAR record: Record_t := NIL;
    x := self.self;
BEGIN
    IF DebugVerbose(x) THEN
        x.comment("declare_record typeid:" & TypeIDToText(typeid)
            & " bit_size:" & IntToDec(bit_size)
            & " field_count:" & IntToDec(field_count));
    ELSE
        x.comment("declare_record");
    END;
    IF typeid = -1 THEN
        RETURN;
    END;
    record := NEW(Record_t,
                  text := TypeIDToText(typeid),
                  typeid := typeid,
                  bit_size := bit_size,
                  fields := NEW(RefSeq.T).init(field_count));
    self.previous_field := NIL;
    self.field_count := field_count;
    self.field_index := 0;
    IF field_count = 0 THEN
        Type_Init(x, record);
        self.record := NIL;
    ELSE
        self.record := record;
    END;
END declare_record;

PROCEDURE declare_field(self: DeclareTypes_t; name: Name; bit_offset: BitOffset; bit_size: BitSize; typeid: TypeUID; <*UNUSED*>typename: Name) =
VAR field: Field_t := NIL;
    previous_field := self.previous_field;
    x := self.self;
BEGIN
    IF DebugVerbose(x) THEN
        x.comment("declare_field " & NameT(name)
            & " bit_size:" & IntToDec(bit_size)
            & " bit_offset:" & IntToDec(bit_offset)
            & " typeid:" & TypeIDToText(typeid));
    ELSE
        x.comment("declare_field");
    END;
    IF self.field_index < 0 OR self.field_index >= self.field_count OR self.record = NIL THEN
        (* skip objects for now
        Err(x, "declare_field overflow");
        *)
        RETURN;
    END;
    name := ReplaceName(name);
    field := NEW(Field_t, bit_offset := bit_offset, bit_size := bit_size, typeid := typeid, name := name);
    self.record.fields.addhi(field);
    IF previous_field # NIL AND previous_field.bit_offset + previous_field.bit_size > bit_offset THEN
        Err(x, "declare_field: fields out of order");
        RETURN;
    END;
    INC(self.field_index);
    IF previous_field # NIL AND previous_field.bit_offset + previous_field.bit_size > bit_offset THEN
        Err(x, "declare_field: fields out of order");
        RETURN;
    END;
    previous_field := field;
    IF self.field_index = self.field_count THEN
        Type_Init(x, self.record);
        self.record := NIL;
        self.previous_field := NIL;
    END;
END declare_field;

PROCEDURE declare_set(self: DeclareTypes_t; typeid, domain_type: TypeUID; bit_size: BitSize; <*UNUSED*>domain_typename: Name) =
VAR x := self.self;
    integers := ARRAY [0..3] OF Target.Int_type { Target.Word8,
                                                  Target.Word16,
                                                  Target.Word32,
                                                  Target.Word64 };
BEGIN
    IF DebugVerbose(x) THEN
        x.comment("declare_set typeid:" & TypeIDToText(typeid)
            & " domain_type:" & TypeIDToText(domain_type)
            & " bit_size:" & IntToDec(bit_size));
    ELSE
        x.comment("declare_set");
    END;

    (* Fit in a single integer or array of integers
     * or single integer of any size.
     * Array of any size integer would also make sense.
     *)
    IF bit_size = Target.Integer.size THEN
        x.Type_Init(NEW(Integer_t, cgtype := Target.Integer.cg_type, typeid := typeid));
    ELSIF bit_size > Target.Integer.size THEN
        <* ASSERT (bit_size MOD Target.Integer.size) = 0 *>
        self.declare_array(typeid, UID_WORD, UID_WORD, bit_size);
    ELSE
      (* Find smallest integer type that is larger than or equal in size.
       *)
      FOR i := FIRST(integers) TO LAST(integers) DO
        IF bit_size <= integers[i].size THEN
          x.Type_Init(NEW(Integer_t, cgtype := integers[i].cg_type, typeid := typeid));
          RETURN;
        END;
      END;
      Err(x, "declare_set not representable as single any size integer or array of integers");
    END;

END declare_set;

PROCEDURE SubrangeIsSigned(READONLY min, max: Target.Int): BOOLEAN =
(* slightly strange logic -- see m3front/src/types/SubrangeType *)
BEGIN
    RETURN TInt.LT(min, TInt.Zero) OR TInt.LT(max, TInt.Zero);
END SubrangeIsSigned;

PROCEDURE SubrangeCGType(READONLY min, max: Target.Int; bit_size: BitSize): CGType =
(* slightly strange logic -- see m3front/src/types/SubrangeType
   m3front should pass us down cgtype directly, and not
   bother with bit_size, domain *)
BEGIN
    RETURN SignedAndBitsToCGType[SubrangeIsSigned(min, max)][bit_size];
END SubrangeCGType;

PROCEDURE declare_subrange(self: DeclareTypes_t; typeid, domain_typeid: TypeUID; READONLY min, max: Target.Int; bit_size: BitSize; <*UNUSED*>domain_typename: Name) =
VAR x := self.self;
BEGIN
    IF DebugVerbose(x) THEN
        x.comment("declare_subrange typeid:" & TypeIDToText(typeid)
            & " domain_type:" & TypeIDToText(domain_typeid)
            & " min:" & TInt.ToText(min)
            & " max:" & TInt.ToText(max)
            & " bit_size:" & IntToDec(bit_size));
    ELSE
        x.comment("declare_subrange");
    END;
    <* ASSERT bit_size = 8 OR bit_size = 16 OR bit_size = 32 OR bit_size = 64 *>
    <* ASSERT typeid # domain_typeid *>
    x.Type_Init(NEW(Subrange_t,
        min := min,
        max := max,
        typeid := typeid,
        domain_typeid := domain_typeid,
        bit_size := bit_size,
        cgtype := SubrangeCGType(min, max, bit_size)),
        (* Subranges have colliding typeids. m3front bug?
         * For example:
         * libm3\Formatter.m3:
         *    Int   = REF INTEGER;
         *    ints: ARRAY [-256..256] OF Int;
         *    declare_subrange typeid:T69A2A904 domain_type:T195C2A74 min:-256 max:256 bit_size:16 */
         * libm3\Sx.m3
         *    MinBoxedInt   = -100;
         *    MaxBoxedInt   = 100;
         *    BoxedInts  := ARRAY [MinBoxedInt .. MaxBoxedInt] OF REF INTEGER {NIL, ..};
         *    declare_subrange typeid:T69A2A904 domain_type:T195C2A74 min:-100 max:100 bit_size:8 */
         *
         * Partial workaround it here by appending the size to the type text.
         * We could further workaround by deferring the typedef until use, as in this
         * case, neither type is used. Just defined.
         *)
        (*type_text_tail*) "_" & IntToDec(bit_size));
END declare_subrange;

PROCEDURE declare_pointer_no_trace(x: T; typeid, target: TypeUID; target_typename: TEXT; brand: TEXT := NIL; traced: BOOLEAN := FALSE) =
BEGIN
    x.Type_Init(
        NEW(Pointer_t,
            typeid := typeid,
            refers_to_typeid := target,
            brand := brand,
            target_typename := target_typename,
            traced := traced));
END declare_pointer_no_trace;

PROCEDURE declare_pointer(self: DeclareTypes_t; typeid, target: TypeUID; brand: TEXT; traced: BOOLEAN; target_typename: Name) =
VAR x := self.self;
    target_typename_text := NameT(target_typename);
BEGIN
    IF typeid = target OR DebugVerbose(x) THEN
        x.comment("declare_pointer typeid:" & TypeIDToText(typeid)
            & " target:" & TypeIDToText(target)
            & " brand:" & TextOrNIL(brand)
            & " traced:" & BoolToText[traced]
            & " target_typename:" & TextOrNil(target_typename_text));
    ELSE
        x.comment("declare_pointer");
    END;
    declare_pointer_no_trace(x, typeid, target, target_typename_text, brand, traced);
END declare_pointer;

PROCEDURE declare_indirect(self: DeclareTypes_t; typeid, target: TypeUID; target_typename: M3ID.T) =
VAR x := self.self;
    target_typename_text := NameT(target_typename);
BEGIN
    IF typeid = target OR DebugVerbose(x) THEN
        x.comment("declare_indirect typeid:", TypeIDToText(typeid),
            " target:" & TypeIDToText(target),
            " target_typename:" & TextOrNil(NameT(target_typename)));
    ELSE
        x.comment("declare_indirect");
    END;
    declare_pointer_no_trace(x, typeid, target, target_typename_text);
END declare_indirect;

PROCEDURE CallingConventionToText(callingConvention: CallingConvention): TEXT =
BEGIN
    IF callingConvention = NIL THEN RETURN "<NIL>" END;
    (* Return underlying __stdcall and __cdecl instead of aliases like WINAPI.
     * RETURN callingConvention.name;
     *)
    RETURN Target.ConventionFromID(callingConvention.m3cg_id).name;
END CallingConventionToText;

PROCEDURE declare_proctype(self: DeclareTypes_t; typeid: TypeUID; param_count: INTEGER; result: TypeUID; raise_count: INTEGER; callingConvention: CallingConvention; result_typename: M3ID.T) =
VAR x := self.self;
BEGIN
    IF DebugVerbose(x) THEN
        x.comment("declare_proctype typeid:" & TypeIDToText(typeid)
            & " param_count:" & IntToDec(param_count)
            & " result:" & TypeIDToText(result)
            & " raise_count:" & IntToDec(raise_count)
            & " callingConvention:" & CallingConventionToText(callingConvention)
            & " result_typename:" & TextOrNil(NameT(result_typename)));
    ELSE
        x.comment("declare_proctype");
    END;
    self.procType := NEW(ProcType_t,
                         cgtype := Target.Address.cg_type,
                         index := 1,
                         callingConvention := callingConvention,
                         types := NEW(REF ARRAY OF Type_t, param_count + 1),
                         typeids := NEW(REF ARRAY OF TypeUID, param_count + 1),
                         typeid := typeid);
    self.procType.typeids[0] := result;
    x.Type_Init(self.procType);
END declare_proctype;

PROCEDURE declare_formal(self: DeclareTypes_t; name: Name; typeid: TypeUID; typename: M3ID.T) =
VAR x := self.self;
    type := self.procType;
BEGIN
  IF DebugVerbose(x) THEN
    x.comment("declare_formal name:", NameT(name),
              " typeid:" & TypeIDToText(typeid),
              " typename:" & TextOrNil(NameT(typename)));
  ELSE
    x.comment("declare_formal");
  END;
  type.typeids[type.index] := typeid;
  INC(type.index);
  IF type.index = NUMBER(type.types^) THEN
    self.procType := NIL;
  END;
END declare_formal;

PROCEDURE declare_raises(self: DeclareTypes_t; name: Name) =
VAR x := self.self;
BEGIN
    IF DebugVerbose(x) THEN
        x.comment("declare_raises name:", NameT(name));
    ELSE
        x.comment("declare_raises");
    END
    (* SuppressLineDirective(self, -1, "declare_raises"); *)
END declare_raises;

(* TODO check that objects are really described correctly; I think they are not,
 * in that base type layout is not always known at compile-time.
 *)
PROCEDURE declare_object(self: DeclareTypes_t; typeid, super: TypeUID; brand: TEXT; traced: BOOLEAN; field_count, method_count: INTEGER; field_size: BitSize; <*UNUSED*>super_typename: Name) =
VAR record: Record_t := NIL;
    x := self.self;
BEGIN
    IF DebugVerbose(x) THEN
        x.comment("declare_object typeid:" & TypeIDToText(typeid)
            & " super:" & TypeIDToText(super)
            & " brand:" & TextOrNIL(brand)
            & " traced:" & BoolToText[traced]
            & " field_count:" & IntToDec(field_count)
            & " method_count:" & IntToDec(method_count)
            & " field_size:" & IntToDec(field_size));
    ELSE
        x.comment("declare_object");
    END;
    (* SuppressLineDirective(self, field_count + method_count, "declare_object field_count + method_count"); *)

    record := NEW(Record_t,
                  text := TypeIDToText(typeid) & "_fields",
                  typeid := -1,
                  bit_size := field_size,
                  fields := NEW(RefSeq.T).init(field_count));
    self.previous_field := NIL;
    self.field_count := field_count;
    self.field_index := 0;
    IF field_count = 0 THEN
        Type_Init(x, record);
        self.record := NIL;
    ELSE
        self.record := record;
    END;
    x.Type_Init(
        NEW(Pointer_t,
            typeid := typeid,
            refers_to_type := record,
            refers_to_typeid := -1,
            brand := brand,
            traced := traced));
END declare_object;

PROCEDURE declare_method(self: DeclareTypes_t; name: Name; signature: TypeUID) =
VAR x := self.self;
BEGIN
    IF DebugVerbose(x) THEN
        x.comment("declare_method name:", NameT(name),
            " signature:", TypeIDToText(signature));
    ELSE
        x.comment("declare_method");
    END;
    SuppressLineDirective(x, -1, "declare_method");
END declare_method;

PROCEDURE declare_opaque(self: DeclareTypes_t; typeid, super: TypeUID) =
VAR x := self.self;
BEGIN
    IF DebugVerbose(x) THEN
        x.comment("declare_opaque typeid:", TypeIDToText(typeid),
            " super:", TypeIDToText(super));
    ELSE
        x.comment("declare_opaque");
    END;
    (* TODO Stronger types *)
    x.Type_Init(NEW(AddressType_t, cgtype := Target.Address.cg_type, typeid := typeid)); (* TODO? *)
END declare_opaque;

PROCEDURE reveal_opaque(self: DeclareTypes_t; lhs, rhs: TypeUID) =<*NOWARN*>
(* VAR x := self.self; *)
BEGIN
    (* m3front does not always reveal_opaque in the same order,
     * damaging ability to diff output. So do not output the comment.
     * This needs further attention.
     *
     * IF DebugVerbose(x) THEN
     *  x.comment("reveal_opaque lhs:", TypeIDToText(lhs),
     *      " rhs:" & TypeIDToText(rhs));
     * ELSE
     *  x.comment("reveal_opaque");
     * END;
     *)
END reveal_opaque;

PROCEDURE declare_exception(self: DeclareTypes_t; name: Name; arg_type: TypeUID; raise_proc: BOOLEAN; base: M3CG.Var; offset: INTEGER) =
VAR x := self.self;
BEGIN
    IF DebugVerbose(x) THEN
        x.comment("declare_exception name:" & NameT(name)
            & " arg_type:" & TypeIDToText(arg_type)
            & " raise_proc:" & BoolToText[raise_proc]
            & " base:" & VarNameT(base)
            & " offset:" & IntToDec(offset));
    ELSE
        x.comment("declare_exception");
    END;
END declare_exception;

PROCEDURE include_setjmp_h(self: T) =
(* Upon importing setjmp, include setjmp.h
 *
 * setjmp and maybe jmp_buf, longjmp must be properly declared.
 * For example Visual C++ has an intrinsic setjmp with a
 * compiler-produced second parameter that is only passed
 * if you include setjmp.h, and without it, longjmp crashes.
 *
 * Avoid including setjmp.h unless needed, to speed up compilation.
 *
 * In future, replace exception handling with optimized C++.
 * _setjmp sort of does not work in Solaris. The headers
 * put it in std:: for C++.
 *)
BEGIN
  (* This is messy. See CsetjmpC.c. *)
  IF NOT self.done_include_setjmp_h THEN
    print(self,
        "#include <setjmp.h>\n" &
        "#if defined(_WIN32) || defined(__CYGWIN__) || defined(__MINGW__)\n" &
        "#define m3_setjmp(env) (setjmp(env))\n" &
        "#elif defined(__sun)\n" &
        "#define m3_setjmp(env) (sigsetjmp((env), 0))\n" &
        "#else\n" &
        "#define m3_setjmp(env) (_setjmp(env))\n" &
        "#endif\n");
    self.done_include_setjmp_h := TRUE;
  END;
END include_setjmp_h;

(*--------------------------------------------------------- runtime hooks ---*)

PROCEDURE set_runtime_proc(multipass: Multipass_t; name: Name; <*UNUSED*>p: M3CG.Proc) =
VAR self := multipass.self;
BEGIN
    IF DebugVerbose(self) THEN
        self.comment("set_runtime_proc name:", NameT(name));
    ELSE
        self.comment("set_runtime_proc");
    END
END set_runtime_proc;

(*------------------------------------------------- variable declarations ---*)

PROCEDURE import_global(self: T; name: Name; byte_size: ByteSize; alignment: Alignment; cgtype: CGType; typeid: TypeUID; <*UNUSED*>typename: Name): M3CG.Var =
VAR var := NEW(Var_t,
        self := self,
        cgtype := cgtype,
        typeid := typeid,
        name := name,
        imported := TRUE).Init();
BEGIN
    self.comment("import_global");
    <* ASSERT (byte_size MOD alignment) = 0 *>
    <* ASSERT NOT self.in_proc *>
    print(self, "extern " & cgtypeToText[cgtype] & " " & NameT(var.name) & ";\n");
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

PROCEDURE declare_segment(self: T; name: Name; typeid: TypeUID; const: BOOLEAN): M3CG.Var =
VAR var := NEW(Var_t,
        self := self,
        name := name,
        typeid := typeid,
        const := const).Init();
    fixed_name := var.name;
    text: TEXT := NIL;
    length := 0;
BEGIN
    self.comment("declare_segment name:" & TextOrNIL(NameT(name))
        & " typeid:" & TypeIDToText(typeid)
        & " const:" & BoolToText[const]);

    (* TODO clean this up and/or document it *)
    IF name # 0 THEN
        text := NameT(name);
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
                comment(self, "handler_name_prefixes:" & self.handler_name_prefixes[i]);
            END;
        END;
    END;
    text := NameT(fixed_name);
    print(self, "/*declare_segment*/struct " & text & "_t;\n");
    print(self, "/*declare_segment*/typedef struct " & text & "_t " & text & "_t;\n");
    RETURN var;
  END declare_segment;

PROCEDURE bind_segment(
    self: T;
    v: M3CG.Var;
    byte_size: ByteSize;
    alignment: Alignment;
    cgtype: CGType;
    exported: BOOLEAN;
    inited: BOOLEAN) =
VAR var := NARROW(v, Var_t);
BEGIN
    IF DebugVerbose(self) THEN
        self.comment("bind_segment var:" & Var_Name(var)
            & " byte_size:" & IntToDec(byte_size)
            & " cgtype:" & cgtypeToText[cgtype]
            & " exported:" & BoolToText[exported]
            & " inited:" & BoolToText[inited]);
    ELSE
        self.comment("bind_segment");
    END;

    <* ASSERT (byte_size MOD alignment) = 0 *>
    var.byte_size := byte_size;
END bind_segment;

PROCEDURE Segments_bind_segment(
    self: Segments_t;
    var: M3CG.Var;
    byte_size: ByteSize;
    alignment: Alignment;
    type: CGType;
    exported: BOOLEAN;
    inited: BOOLEAN) =
BEGIN
    bind_segment(self.self, var, byte_size, alignment, type, exported, inited);
END Segments_bind_segment;

PROCEDURE declare_global(
    self: T;
    name: Name;
    byte_size: ByteSize;
    alignment: Alignment;
    cgtype: CGType;
    typeid: TypeUID;
    exported: BOOLEAN;
    inited: BOOLEAN;
    typename: Name): M3CG.Var =
BEGIN
    IF DebugVerbose(self) THEN
        self.comment("declare_global name:" & TextOrNIL(NameT(name))
            & " byte_size:" & IntToDec(byte_size)
            & " cgtype:" & cgtypeToText[cgtype]
            & " typeid:" & TypeIDToText(typeid)
            & " exported:" & BoolToText[exported]
            & " inited:" & BoolToText[inited]);
    ELSE
        self.comment("declare_global");
    END;
    RETURN DeclareGlobal(self, name, byte_size, alignment, cgtype, typeid, exported, inited, FALSE, typename);
END declare_global;

PROCEDURE Segments_declare_global(
    self: Segments_t;
    name: Name;
    byte_size: ByteSize;
    alignment: Alignment;
    cgtype: CGType;
    typeid: TypeUID;
    exported: BOOLEAN;
    inited: BOOLEAN;
    typename: Name): M3CG.Var =
BEGIN
    RETURN declare_global(self.self, name, byte_size, alignment, cgtype, typeid, exported, inited, typename);
END Segments_declare_global;

PROCEDURE declare_constant(
    self: T;
    name: Name;
    byte_size: ByteSize;
    alignment: Alignment;
    cgtype: CGType;
    typeid: TypeUID;
    exported: BOOLEAN;
    inited: BOOLEAN;
    typename: Name): M3CG.Var =
BEGIN
    IF DebugVerbose(self) THEN
        self.comment("declare_global name:" & NameT(name)
            & " byte_size:" & IntToDec(byte_size)
            & " cgtype:" & cgtypeToText[cgtype]
            & " typeid:" & TypeIDToText(typeid)
            & " exported:" & BoolToText[exported]
            & " inited:" & BoolToText[inited]);
    ELSE
        self.comment("declare_constant");
    END;
    RETURN DeclareGlobal(self, name, byte_size, alignment, cgtype, typeid, exported, inited, TRUE, typename);
END declare_constant;

PROCEDURE Segments_declare_constant(
    self: Segments_t;
    name: Name;
    byte_size: ByteSize;
    alignment: Alignment;
    cgtype: CGType;
    typeid: TypeUID;
    exported: BOOLEAN;
    inited: BOOLEAN;
    typename: Name): M3CG.Var =
BEGIN
    RETURN declare_constant(self.self, name, byte_size, alignment, cgtype, typeid, exported, inited, typename);
END Segments_declare_constant;

PROCEDURE DeclareGlobal(
    self: T;
    name: Name;
    byte_size: ByteSize;
    alignment: Alignment;
    cgtype: CGType;
    typeid: TypeUID;
    exported: BOOLEAN;
    <*UNUSED*>inited: BOOLEAN;
    const: BOOLEAN;
    <*UNUSED*>typename: Name): M3CG.Var =
CONST DeclTag = ARRAY BOOLEAN OF TEXT { "declare_global", "declare_constant" };
VAR var := NEW(Var_t,
        self := self,
        cgtype := cgtype,
        name := name,
        const := const,
        typeid := typeid,
        (*inited := inited, TODO?
        alignment := alignment, TODO? *)
        exported := exported,
        global := TRUE,
        proc := self.current_proc,
        byte_size := byte_size).Init();
BEGIN
    self.comment(DeclTag [const]);
    <* ASSERT (byte_size MOD alignment) = 0 *>
    print(self, var.Declare() & ";\n");
    RETURN var;
END DeclareGlobal;

PROCEDURE MarkUsed_var(var: M3CG.Var) =
BEGIN
    NARROW(var, Var_t).used := TRUE;
END MarkUsed_var;

PROCEDURE MarkUsed_load(
    <*UNUSED*>self: MarkUsed_t;
    var: M3CG.Var;
    <*UNUSED*>offset: ByteOffset;
    <*UNUSED*>mtype: MType;
    <*UNUSED*>ztype: ZType) =
BEGIN
    MarkUsed_var(var);
END MarkUsed_load;

PROCEDURE MarkUsed_load_address(
    <*UNUSED*>self: MarkUsed_t;
    var: M3CG.Var;
    <*UNUSED*>offset: ByteOffset) =
BEGIN
    MarkUsed_var(var);
END MarkUsed_load_address;

PROCEDURE MarkUsed_store(
    <*UNUSED*>self: MarkUsed_t;
    var: M3CG.Var;
    <*UNUSED*>offset: ByteOffset;
    <*UNUSED*>ztype: ZType;
    <*UNUSED*>mtype: MType) =
BEGIN
    MarkUsed_var(var);
END MarkUsed_store;

TYPE CountUsedLabels_t = M3CG_DoNothing.T OBJECT
    count := 0;
OVERRIDES
    case_jump := CountUsedLabels_case_jump;
END;

TYPE MarkUsed_t = M3CG_DoNothing.T OBJECT
    self: T;
    labels: REF ARRAY OF Label := NIL;
    index := 0;
    labels_min := LAST(Label);
    labels_max := FIRST(Label);
OVERRIDES
(* frontend creates unreferenced labels, that gcc -Wall complains about;
   the point of this pass is to mark which labels are actually used,
   so that later set_label ignores unused labels
*)
    jump := MarkUsed_label;
    if_true := MarkUsed_if_true;
    if_false := MarkUsed_if_true;
    if_compare := MarkUsed_if_compare;
    case_jump := MarkUsed_case_jump;

(* frontend creates unreferenced variables, that gcc -Wall complains about;
   the point of this pass is to mark which variables are actually used,
   so that later code ignores unused variables
*)
    load := MarkUsed_load;
    load_address := MarkUsed_load_address;
    store := MarkUsed_store;

    (* procedures *)
    init_proc := MarkUsed_init_proc;
    start_call_direct := MarkUsed_start_call_direct;
    load_procedure := MarkUsed_load_procedure;
END;

PROCEDURE MarkUsed_label(self: MarkUsed_t; label: Label) =
BEGIN
    self.labels_min := MIN(self.labels_min, label);
    self.labels_max := MAX(self.labels_max, label);
    self.labels[self.index] := label;
    INC(self.index);
END MarkUsed_label;

PROCEDURE MarkUsed_if_true(self: MarkUsed_t; <*UNUSED*>itype: IType; label: Label; <*UNUSED*>frequency: Frequency) =
BEGIN
    MarkUsed_label(self, label);
END MarkUsed_if_true;

PROCEDURE MarkUsed_if_compare(
    self: MarkUsed_t;
    <*UNUSED*>ztype: ZType;
    <*UNUSED*>op: CompareOp;
    label: Label;
    <*UNUSED*>frequency: Frequency) =
BEGIN
    MarkUsed_label(self, label);
END MarkUsed_if_compare;

PROCEDURE MarkUsed_case_jump(self: MarkUsed_t; <*UNUSED*>itype: IType; READONLY labels: ARRAY OF Label) =
BEGIN
    FOR i := FIRST(labels) TO LAST(labels) DO
        MarkUsed_label(self, labels[i]);
    END;
END MarkUsed_case_jump;

PROCEDURE CountUsedLabels_case_jump(self: CountUsedLabels_t; <*UNUSED*>itype: IType; READONLY labels: ARRAY OF Label) =
BEGIN
    INC(self.count, NUMBER(labels));
END CountUsedLabels_case_jump;

PROCEDURE MarkUsed(self: Multipass_t) =
(* frontend creates unreferenced labels and variables
   that gcc -Wall complains about; find out which are used and mark them,
   so we can skip others later
   for procedures, I'd just like to suppress declaring unused imports
*)
TYPE Op = M3CG_Binary.Op;
CONST LabelOps = ARRAY OF Op{
    (* operands that goto a label -- marking the label as used
       Except for case_jump, these ops use one label each.
       case_jump requires more specific analysis *)
        Op.jump,
        Op.if_true,
        Op.if_false,
        Op.if_compare,
        Op.case_jump
    };
CONST VarProcOps = ARRAY OF Op{
    (* operands that use a variable -- marking the variable as used *)
        Op.load,
        Op.load_address,
        Op.store,

    (* operands that use procedures *)
        Op.start_call_direct,
        Op.load_procedure,
        Op.init_proc
    };
VAR x := self.self;
    pass := NEW(MarkUsed_t, self := x);
    count_pass := NEW(CountUsedLabels_t);
    index := 0;
BEGIN
    x.comment("begin: mark used");

    (* First estimate label count via op count.
       This is correct, except for case_jump.
    *)
    FOR i := FIRST(LabelOps) TO LAST(LabelOps) DO
        INC(pass.index, self.op_counts[LabelOps[i]]);
    END;
    (* Subtract off case_jump, and then count it accurately. *)
    DEC(pass.index, self.op_counts[Op.case_jump]);
    index := 0;
    self.Replay(count_pass, index, self.op_data[Op.case_jump]);
    INC(pass.index, count_pass.count);

    IF pass.index # 0 THEN
        pass.labels := NEW(REF ARRAY OF Label, pass.index);
        pass.index := 0;
        FOR i := FIRST(LabelOps) TO LAST(LabelOps) DO
            index := 0;
            self.Replay(pass, index, self.op_data[LabelOps[i]]);
        END;
        x.labels := NEW(REF ARRAY OF BOOLEAN, pass.labels_max - pass.labels_min + 1);
        x.labels_min := pass.labels_min;
        x.labels_max := pass.labels_max;
        FOR i := FIRST(x.labels^) TO LAST(x.labels^) DO
            x.labels[i] := FALSE;
        END;
        FOR i := 0 TO pass.index - 1 DO
            x.labels[pass.labels[i] - pass.labels_min] := TRUE;
        END;
    END;
    
    index := 0;
    FOR i := FIRST(VarProcOps) TO LAST(VarProcOps) DO
        self.Replay(pass, index, self.op_data[VarProcOps[i]]);
    END;

   x.comment("end: mark used");
END MarkUsed;

TYPE Segments_t = M3CG_DoNothing.T OBJECT
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
        Op.check_range,
        Op.xor,
        Op.compare,
        Op.if_compare,
        Op.if_true,
        Op.if_false
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
CONST setData = ARRAY OF T1{
    T1{Op.set_union, "static void __stdcall m3_set_union(WORD_T n_words,WORD_T*c,WORD_T*b,WORD_T*a){WORD_T i=0;for(;i<n_words;++i)a[i]=b[i]|c[i];}"},
    T1{Op.set_difference, "static void __stdcall m3_set_difference(WORD_T n_words,WORD_T*c,WORD_T*b,WORD_T*a){WORD_T i=0;for(;i<n_words;++i)a[i]=b[i]&(~c[i]);}"},
    T1{Op.set_intersection, "static void __stdcall m3_set_intersection(WORD_T n_words,WORD_T*c,WORD_T*b,WORD_T*a){WORD_T i=0;for(;i<n_words;++i)a[i]=b[i]&c[i];}"},
    T1{Op.set_sym_difference, "static void __stdcall m3_set_sym_difference(WORD_T n_words,WORD_T*c,WORD_T*b,WORD_T*a){WORD_T i=0;for(;i<n_words;++i)a[i]=b[i]^c[i];}"},
    T1{Op.set_range,
          "#define M3_HIGH_BITS(a) ((~(WORD_T)0) << (a))\n"
        & "#define M3_LOW_BITS(a)  ((~(WORD_T)0) >> (SET_GRAIN - (a) - 1))\n"
        & "static void __stdcall m3_set_range(WORD_T b, WORD_T a, WORD_T* s)\n"
        & "{\n"
        & "  if (a > b) {\n"
        & "    /* no bits to set */\n"
        & "  } else {\n"
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
        & "}"
        },
    T1{Op.set_singleton, "static void __stdcall m3_set_singleton(WORD_T a,WORD_T*s){s[a/SET_GRAIN]|=(((WORD_T)1)<<(a%SET_GRAIN));}"},
    T1{Op.set_member, "static WORD_T __stdcall m3_set_member(WORD_T elt,WORD_T*set){return(set[elt/SET_GRAIN]&(((WORD_T)1)<<(elt%SET_GRAIN)))!=0;}"}
    };
VAR x := self.self;
    helperFunctions := NEW(HelperFunctions_t).Init(x);
    index := 0;
    setAny := FALSE;
BEGIN
    x.comment("begin: helper functions");

    FOR i := FIRST(setData) TO LAST(setData) DO
        IF self.op_counts[setData[i].op] > 0 THEN
            IF NOT setAny THEN
                print(x, "typedef WORD_T* SET;\n#define SET_GRAIN (sizeof(WORD_T)*8)\n");
                setAny := TRUE;
            END;
            ifndef(x, "m3set" & M3CG_Binary.OpText(setData[i].op));
            print(x, setData[i].text);
            endif(x);
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
    x.comment("end: helper functions");
END HelperFunctions;

(* Helper functions are only output if needed, to avoid warnings about unused functions.
 * This pass determines which helper functions are neded and outputs them. *)
TYPE HelperFunctions_t = M3CG_DoNothing.T OBJECT
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
        IF text[i] # NIL THEN
            print(x, text[i]);
            print(x, "\n");
        END;
    END;
END HelperFunctions_print_array;

PROCEDURE HelperFunctions_helper_with_boolean_and_array(self: HelperFunctions_t; VAR already_printed: BOOLEAN; READONLY text: ARRAY OF TEXT) =
BEGIN
    IF NOT already_printed THEN
        HelperFunctions_print_array(self, text);
        already_printed := TRUE;
    END;
END HelperFunctions_helper_with_boolean_and_array;

PROCEDURE HelperFunctions_helper_with_boolean(self: HelperFunctions_t; VAR already_printed: BOOLEAN; text: TEXT) =
BEGIN
    HelperFunctions_helper_with_boolean_and_array(self, already_printed, ARRAY OF TEXT{text});
END HelperFunctions_helper_with_boolean;

PROCEDURE HelperFunctions_helper_with_type_and_array(
    self: HelperFunctions_t; op: TEXT; type: CGType; VAR types_already_printed: SET OF CGType; READONLY first: ARRAY OF TEXT) =
BEGIN
    (* Print the prefix array before any type-specific content. *)
    IF types_already_printed = SET OF CGType{} THEN
        HelperFunctions_print_array(self, first);
    END;

    (* Print per-type content. Remember what types are printed to avoid duplication. *)
    IF NOT type IN types_already_printed THEN
        ifndef(self.self, "m3_" & op & "_" & cgtypeToText[type]);
        print(self.self, "m3_" & op & "_T(" & cgtypeToText[type] & ")");
        endif(self.self);
        types_already_printed := types_already_printed + SET OF CGType{type};
    END;
END HelperFunctions_helper_with_type_and_array;

PROCEDURE HelperFunctions_helper_with_type(self: HelperFunctions_t; op: TEXT; type: CGType; VAR types_already_printed: SET OF CGType; first: TEXT := NIL) =
BEGIN
    HelperFunctions_helper_with_type_and_array(self, op, type, types_already_printed, ARRAY OF TEXT{first});
END HelperFunctions_helper_with_type;

(* TODO give up and #include <string.h>? *)
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
    IF NOT (((a = b) AND (a # Sign.Unknown)) OR cgtypeIsUnsignedInt[type]) THEN
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
    IF NOT (((a = b) AND (a # Sign.Unknown)) OR cgtypeIsUnsignedInt[type]) THEN
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

    "#ifndef m3_round\n#define m3_round m3_round\n"
    & "#ifdef _WIN64 /* temporary workaround */\n"
    & "static INT64 __stdcall m3_round(EXTENDED f) { return (INT64)f; }\n"
    & "#else\n"
    & "INT64 __cdecl llroundl(long double);\nstatic INT64 __stdcall m3_round(EXTENDED f) { return (INT64)llroundl(f); }\n"
    & "#endif\n"
    & "#endif",

    "#ifndef m3_trunc\n#define m3_trunc m3_trunc\nstatic INT64 __stdcall m3_trunc(EXTENDED f) { return (INT64)f; }\n#endif\n",

    "#ifndef m3_floor\n#define m3_floor m3_floor\ndouble __cdecl floor(double);\nstatic INT64 __stdcall m3_floor(EXTENDED f) { return floor(f); } /* math.h */\n#endif\n",

    "#ifndef m3_ceil\n#define m3_ceil m3_ceil\ndouble __cdecl ceil(double);\nstatic INT64 __stdcall m3_ceil(EXTENDED f) { return ceil(f); } /* math.h */\n#endif\n"
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

PROCEDURE HelperFunctions_pop(self: HelperFunctions_t; type: CGType) =
CONST text = "#define m3_pop_T(T) static void __stdcall m3_pop_##T(volatile T a) { }";
BEGIN
    HelperFunctions_helper_with_type(self, "pop", type, self.data.pop, text);
END HelperFunctions_pop;

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
"#ifndef m3_fence",
"#ifdef _MSC_VER",
"long __cdecl _InterlockedExchange(volatile long*, long);",
"#pragma instrinsic(_InterlockedExchange)",
"static volatile long m3_fence_var;",
"#define m3_fence() _InterlockedExchange(&m3_fence_var, 0)",
"#else",
"#define m3_fence m3_fence",
"static void __stdcall m3_fence(void){}", (* not yet implemented *)
"#endif",
"#endif"
};
BEGIN
    HelperFunctions_helper_with_boolean_and_array(self, self.data.fence, text);
END HelperFunctions_fence;

TYPE Locals_t = M3CG_DoNothing.T OBJECT
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
    type: CGType;
    typeid: TypeUID;
    <*UNUSED*>in_memory: BOOLEAN;
    up_level: BOOLEAN;
    <*UNUSED*>frequency: Frequency;
    typename: M3ID.T): M3CG.Var =
BEGIN
    RETURN declare_param(
        self.self,
        name,
        byte_size,
        alignment,
        type,
        typeid,
        up_level,
        typename);
END Locals_declare_param;

PROCEDURE Locals_declare_local(
    self: Locals_t;
    name: Name;
    byte_size: ByteSize;
    alignment: Alignment;
    type: CGType;
    typeid: TypeUID;
    <*UNUSED*>in_memory: BOOLEAN;
    up_level: BOOLEAN;
    <*UNUSED*>frequency: Frequency;
    typename: Name): M3CG.Var =
BEGIN
    RETURN declare_local(self.self, name, byte_size, alignment, type, typeid, up_level, typename);
END Locals_declare_local;

TYPE AllocateTemps_t = Locals_t;

PROCEDURE AllocateTemps_common(self: AllocateTemps_t; type: CGType) =
VAR x := self.self;
BEGIN
    x.comment("AllocateTemps_common");
    WITH t = internal_declare_temp(x, CG_Bytes[type], CG_Bytes[type], type) DO
        t.used := TRUE;
        x.temp_vars[x.op_index] := t;
    END;
END AllocateTemps_common;

PROCEDURE AllocateTemps_check_nil(self: AllocateTemps_t; <*UNUSED*>code: RuntimeError) =
VAR x := self.self;
BEGIN
    x.comment("AllocateTemps_check_nil");
    AllocateTemps_common(self, CGType.Addr);
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

TYPE Imports_t = M3CG_DoNothing.T OBJECT
    self: T;
OVERRIDES
    import_procedure := Imports_import_procedure;
    declare_param := Imports_declare_param;
    import_global := Imports_import_global;
END;

PROCEDURE Imports_import_procedure(
    self: Imports_t;
    name: Name;
    parameter_count: INTEGER;
    return_type: CGType;
    callingConvention: CallingConvention;
    return_typeid: TypeUID;
    return_typename: M3ID.T): M3CG.Proc =
BEGIN
    RETURN import_procedure(self.self, name, parameter_count, return_type, callingConvention, return_typeid, return_typename);
END Imports_import_procedure;

PROCEDURE Imports_declare_param(
    self: Imports_t;
    name: Name;
    byte_size: ByteSize;
    alignment: Alignment;
    type: CGType;
    typeid: TypeUID;
    <*UNUSED*>in_memory: BOOLEAN;
    up_level: BOOLEAN;
    <*UNUSED*>frequency: Frequency;
    typename: M3ID.T): M3CG.Var =
BEGIN
    RETURN declare_param(
        self.self,
        name,
        byte_size,
        alignment,
        type,
        typeid,
        up_level,
        typename);
END Imports_declare_param;

PROCEDURE Imports_import_global(
    self: Imports_t;
    name: Name;
    byte_size: ByteSize;
    alignment: Alignment;
    type: CGType;
    typeid: TypeUID;
    typename: Name): M3CG.Var =
BEGIN
    RETURN import_global(self.self, name, byte_size, alignment, type, typeid, typename);
END Imports_import_global;

TYPE GetStructSizes_t = M3CG_DoNothing.T OBJECT
    sizes: REF ARRAY OF INTEGER := NIL;
    count := 0;
METHODS
    Declare(type: CGType; byte_size: ByteSize; alignment: Alignment): M3CG.Var := GetStructSizes_Declare;
OVERRIDES
    declare_constant := GetStructSizes_declare_constant;
    declare_global := GetStructSizes_declare_global;
    declare_local := GetStructSizes_declare_local;
    declare_param := GetStructSizes_declare_param;
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
    index := 0;
    sizestr: TEXT;
BEGIN

    (* RETURN; *) (* TODO *)

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
                    sizestr := IntToDec(size);
                    ifndef(x, "struct_" & sizestr & "_t"); (* see define STRUCT *)
                    print(x, "STRUCT" & IntToDec(units[unit]) & "(" & sizestr & ")");
                    endif(x);
                    EXIT;
                END;
            END;
        END;
    END;

END GetStructSizes;

PROCEDURE GetStructSizes_Declare(self: GetStructSizes_t; type: CGType; byte_size: ByteSize; alignment: Alignment): M3CG.Var =
BEGIN

    <* ASSERT byte_size >= 0 *>
    <* ASSERT (byte_size MOD alignment) = 0 *>

    byte_size := MAX(byte_size, 1);
    IF type = CGType.Struct THEN
        self.sizes[self.count] := byte_size;
        INC(self.count);
    END;
    RETURN NIL;
END GetStructSizes_Declare;

PROCEDURE GetStructSizes_declare_temp(
    self: GetStructSizes_t;
    byte_size: ByteSize;
    alignment: Alignment;
    type: CGType;
    <*UNUSED*>in_memory:BOOLEAN;
    <*UNUSED*>typename: Name): M3CG.Var =
BEGIN
    RETURN self.Declare(type, byte_size, alignment);
END GetStructSizes_declare_temp;

PROCEDURE GetStructSizes_declare_global(
    self: GetStructSizes_t;
    <*UNUSED*>name: Name;
    byte_size: ByteSize;
    alignment: Alignment;
    type: CGType;
    <*UNUSED*>typeid: TypeUID;
    <*UNUSED*>exported: BOOLEAN;
    <*UNUSED*>inited: BOOLEAN;
    <*UNUSED*>typename: Name): M3CG.Var =
BEGIN
    RETURN self.Declare(type, byte_size, alignment);
END GetStructSizes_declare_global;

PROCEDURE GetStructSizes_import_global(
    self: GetStructSizes_t;
    <*UNUSED*>name: Name;
    byte_size: ByteSize;
    alignment: Alignment;
    type: CGType;
    <*UNUSED*>typeid: TypeUID;
    <*UNUSED*>typename: Name): M3CG.Var =
BEGIN
    RETURN self.Declare(type, byte_size, alignment);
END GetStructSizes_import_global;

PROCEDURE GetStructSizes_declare_constant(
    self: GetStructSizes_t;
    <*UNUSED*>name: Name;
    byte_size: ByteSize;
    alignment: Alignment;
    type: CGType;
    <*UNUSED*>typeid: TypeUID;
    <*UNUSED*>exported: BOOLEAN;
    <*UNUSED*>inited: BOOLEAN;
    <*UNUSED*>typename: Name): M3CG.Var =
BEGIN
    RETURN self.Declare(type, byte_size, alignment);
END GetStructSizes_declare_constant;

PROCEDURE GetStructSizes_declare_local(
    self: GetStructSizes_t;
    <*UNUSED*>name: Name;
    byte_size: ByteSize;
    alignment: Alignment;
    type: CGType;
    <*UNUSED*>typeid: TypeUID;
    <*UNUSED*>in_memory: BOOLEAN;
    <*UNUSED*>up_level: BOOLEAN;
    <*UNUSED*>frequency: Frequency;
    <*UNUSED*>typename: Name): M3CG.Var =
BEGIN
    RETURN self.Declare(type, byte_size, alignment);
END GetStructSizes_declare_local;

PROCEDURE GetStructSizes_declare_param(
    self: GetStructSizes_t;
    <*UNUSED*>name: Name;
    byte_size: ByteSize;
    alignment: Alignment;
    type: CGType;
    <*UNUSED*>typeid: TypeUID;
    <*UNUSED*>in_memory: BOOLEAN;
    <*UNUSED*>up_level: BOOLEAN;
    <*UNUSED*>frequency: Frequency;
    <*UNUSED*>typename: M3ID.T): M3CG.Var =
BEGIN
    RETURN self.Declare(type, byte_size, alignment);
END GetStructSizes_declare_param;

PROCEDURE Struct(size: INTEGER): TEXT =
BEGIN
    <* ASSERT size >= 0 *>
    size := MAX(size, 1);
    RETURN "STRUCT(" & IntToDec(size) & ")";
END Struct;

PROCEDURE Var_Type(var: Var_t): TEXT =
BEGIN
    IF var.type_text # NIL THEN
        RETURN " /* Var_Type1 */ " & var.type_text;
    END;
    IF var.cgtype # CGType.Struct THEN
        RETURN " /* Var_Type2 */ " & cgtypeToText[var.cgtype];
    END;
    RETURN " /* Var_Type3 */ " & Struct(var.byte_size);
END Var_Type;

PROCEDURE Var_Name(var: Var_t): TEXT =
BEGIN
    RETURN NameT(var.name);
END Var_Name;

PROCEDURE Var_Declare(var: Var_t): TEXT =
BEGIN
    RETURN ARRAY BOOLEAN OF TEXT{"", "static "}[var.global AND NOT var.exported] & var.Type() & " " & var.Name();
END Var_Declare;

PROCEDURE Var_InFrameDeclare(var: Var_t): TEXT =
(* InFrame means in frame struct for uplevels. *)
VAR struct := " ";
BEGIN
    <* ASSERT NOT (var.global OR var.exported) *> (* only locals/params for now *)
    IF PassStructsByValue AND var.cgtype = CGType.Struct AND var.up_level THEN
        struct := "*";
    END;
    RETURN var.InFrameType() & struct & var.InFrameName();
END Var_InFrameDeclare;

PROCEDURE Param_Type(var: Var_t): TEXT =
BEGIN
    IF var.type_text # NIL THEN
        RETURN " /* Param_Type1 */ " & var.type_text;
    END;
    RETURN " /* Param_Type2 */ " & cgtypeToText[var.cgtype];
END Param_Type;

PROCEDURE Param_Name(var: Var_t): TEXT =
VAR struct := "";
BEGIN
    IF var.cgtype = CGType.Struct AND NOT PassStructsByValue THEN
        struct := "_param_struct_pointer_";
    END;
    RETURN struct & NameT(var.name);
END Param_Name;

PROCEDURE Param_Declare(var: Var_t): TEXT =
VAR struct := " ";
BEGIN
    IF NOT PassStructsByValue AND var.cgtype = CGType.Struct THEN
        struct := "*";
    END;
    RETURN var.Type() & struct & var.Name();
END Param_Declare;

PROCEDURE declare_local(
    self: T;
    name: Name;
    byte_size: ByteSize;
    <*UNUSED*>alignment: Alignment;
    cgtype: CGType;
    typeid: TypeUID;
    up_level: BOOLEAN;
    <*UNUSED*>typename := M3ID.NoID): Var_t =
VAR var := NEW(Var_t,
        self := self,
        cgtype := cgtype,
        name := name,
        up_level := up_level,
        byte_size := byte_size,
        typeid := typeid,
        proc := self.current_proc).Init();
    type: Type_t := NIL;
BEGIN
    IF DebugVerbose(self) THEN
        self.comment("declare_local name:" & NameT(var.name)
            & " typeid:" & TypeIDToText(typeid)
            & " cgtype:" & cgtypeToText[cgtype]
            & " up_level:" & BoolToText[up_level]
            & " byte_size:" & IntToDec(byte_size));
    ELSE
        self.comment("declare_local");
    END;

    <* ASSERT self.current_proc # NIL *>
    <* ASSERT self.current_proc.locals # NIL *>

    IF up_level THEN
        self.current_proc.uplevels := TRUE;
    END;
    IF ResolveType(self, typeid, type) AND type # NIL AND type.text # NIL THEN
        var.type_text := type.text;
    ELSE
        IF typeid # -1 AND typeid # 0 THEN
            Err(self, "declare_local: unknown typeid:" & TypeIDToText(typeid) & " type:" & cgtypeToText[cgtype] & "\n");
        ELSE
            (* RTIO.PutText("warning: declare_local: unknown typeid:" & TypeIDToText(typeid) & " type:" & cgtypeToText[cgtype] & "\n");
            RTIO.Flush(); *) (* this occurs frequently *)
        END;
    END;
    self.current_proc.locals.addhi(var);
    RETURN var;
END declare_local;

TYPE FunctionPrototype_t = { Declare, Define };

PROCEDURE function_prototype(proc: Proc_t; kind: FunctionPrototype_t): TEXT =
VAR params := proc.params;
    text := proc.return_type_text & "\n" &
            CallingConventionToText(proc.callingConvention) & "\n" &
            NameT(proc.name);
    after_param: TEXT := NIL;
    ansi := TRUE (*NOT is_exception_handler*);
    define_kr := NOT ansi AND kind = FunctionPrototype_t.Define;
    kr_part2 := "";
BEGIN
    IF proc.omit_prototype THEN
      RETURN "";
    END;
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
(* TODO do this later, after MarkUsed *)
VAR proc := self.param_proc;
    prototype: TEXT := NIL;
    param: Var_t := NIL;
BEGIN
    IF proc.no_return THEN
        no_return(self);
    END;
    IF proc.add_static_link THEN
        param := NARROW(internal_declare_param(
            self,
            self.static_link_id,
            CG_Bytes[CGType.Addr], (* size *)
            CG_Bytes[CGType.Addr], (* alignment *)
            CGType.Addr,
            -1, (* typeid *)
            TRUE, (* up_level, sort of -- needs to be stored, but is never written, can be read from direct parameter
                     This gets it stored in begin_function. *)
            NARROW(proc.parent, Proc_t).FrameType() & "*",
            M3ID.NoID (* typename TODO combine with previous *)), Var_t);
        param.used := TRUE;
    END;

    prototype := function_prototype(proc, FunctionPrototype_t.Declare) & ARRAY BOOLEAN OF TEXT{";\n", " M3_ATTRIBUTE_NO_RETURN;\n"}[proc.no_return];

    <* ASSERT NOT self.in_proc *>

    IF NOT proc.omit_prototype THEN
      print(self, prototype);
    END;
    self.param_proc := NIL;
END last_param;

PROCEDURE internal_declare_param(
    self: T;
    name: Name;
    byte_size: ByteSize;
    <*UNUSED*>alignment: Alignment;
    cgtype: CGType;
    typeid: TypeUID;
    up_level: BOOLEAN;
    type_text: TEXT;
    typename: M3ID.T): M3CG.Var =
VAR function := self.param_proc;
    var: Var_t := NIL;
    typename_text := NameT(typename);
BEGIN
    IF DebugVerbose(self) THEN
        self.comment("internal_declare_param name:" & TextOrNIL(NameT(name))
            & " cgtype:" & cgtypeToText[cgtype]
            & " typeid:" & TypeIDToText(typeid)
            & " up_level:" & BoolToText[up_level]
            & " type_text:" & TextOrNIL(type_text)
            & " typename:" & TextOrNil(typename_text));
    ELSE
        self.comment("internal_declare_param");
    END;

    type_text := TypeText (self, cgtype, typename_text, typeid, type_text, name);

    var := NEW(Param_t,
        self := self,
        name := name,
        cgtype := cgtype,
        typeid := typeid,
        byte_size := byte_size,
        up_level := up_level,
        proc := function,
        type_text := type_text).Init();
    function.params[self.param_count] := var;
    function.uplevels := function.uplevels OR up_level;
    SuppressLineDirective(self, -1, "declare_param");
    INC(self.param_count);
    IF name # self.static_link_id AND self.param_count = function.parameter_count_without_static_link THEN
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
    type: CGType;
    typeid: TypeUID;
    up_level: BOOLEAN;
    typename: M3ID.T): M3CG.Var =
BEGIN
    IF self.param_proc = NIL THEN
        RETURN NIL;
    END;
    RETURN internal_declare_param(
        self,
        name,
        byte_size,
        alignment,
        type,
        typeid,
        up_level,
        NIL, (* typetext *)
        typename);
END declare_param;

PROCEDURE
internal_declare_temp(
(* returns derived Var_t instead of base M3CG.Var *)
    self: T;
    byte_size: ByteSize;
    alignment: Alignment;
    type: CGType): Var_t =
BEGIN
    IF DebugVerbose(self) THEN
        self.comment("declare_temp byte_size:", IntToDec(byte_size),
            " cgtype:", cgtypeToText[type]);
    ELSE
        self.comment("declare_temp");
    END;
    RETURN declare_local(self, 0, byte_size, alignment, type, -1, FALSE);
END internal_declare_temp;

PROCEDURE Locals_declare_temp(
    self: Locals_t;
    byte_size: ByteSize;
    alignment: Alignment;
    type: CGType;
    <*UNUSED*>in_memory:BOOLEAN;
    <*UNUSED*>typename: Name): M3CG.Var =
BEGIN
    RETURN internal_declare_temp(self.self, byte_size, alignment, type);
END Locals_declare_temp;

PROCEDURE free_temp(self: T; <*NOWARN*>v: M3CG.Var) =
BEGIN
    self.comment("free_temp");
END free_temp;

(*---------------------------------------- static variable initialization ---*)

PROCEDURE begin_init(self: T; <*UNUSED*>v: M3CG.Var) =
BEGIN
    self.comment("begin_init");
    self.current_offset := 0;
    self.initializer_comma := "";
    SuppressLineDirective(self, 1, "begin_init");
END begin_init;

PROCEDURE Segments_begin_init(self: Segments_t; v: M3CG.Var) =
BEGIN
    begin_init(self.self, v);
END Segments_begin_init;

PROCEDURE initializer_addhi(self: T; text: TEXT) =
VAR initializer := self.initializer;
BEGIN
    initializer.addhi(self.initializer_comma);
    initializer.addhi(text);
    IF text = Text_left_brace THEN
        self.initializer_comma := "";
    ELSE
        self.initializer_comma := ",";
    END;
END initializer_addhi;

PROCEDURE end_init(self: T; v: M3CG.Var) =
VAR var := NARROW(v, Var_t);
    fields := self.fields;
    initializer := self.initializer;
    var_name := NameT(var.name);
    const := ARRAY BOOLEAN OF TEXT{"", " const "}[var.const];
BEGIN
    self.comment("end_init");
    init_to_offset(self, var.byte_size);
    end_init_helper(self);

    print(self, "struct " & var_name & "_t{");
    WHILE fields.size() > 0 DO
        print(self, fields.remlo());
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
        self.report_fault := NameT(var.name) & "_CRASH";
        IF NOT self.RTHooks_ReportFault_imported_or_declared THEN
            print(self, "void __cdecl RTHooks__ReportFault(ADDRESS, WORD_T) M3_ATTRIBUTE_NO_RETURN;\n");
        END;
        print(self, "static void __cdecl " & self.report_fault & "(WORD_T code) M3_ATTRIBUTE_NO_RETURN;\n");
        print(self, "static void __cdecl " & self.report_fault & "(WORD_T code){RTHooks__ReportFault((ADDRESS)&" & NameT(var.name) & ",code);}");
    END;

    SuppressLineDirective(self, -1, "end_init");
END end_init;

PROCEDURE Segments_end_init(self: Segments_t; v: M3CG.Var) =
BEGIN
    end_init(self.self, v);
END Segments_end_init;

PROCEDURE init_to_offset(self: T; offset: ByteOffset) =
VAR pad := offset - self.current_offset;
BEGIN
    (* self.comment("init_to_offset offset=", IntToDec(offset)); *)

    <* ASSERT offset >= self.current_offset *>
    <* ASSERT pad >= 0 *>
    <* ASSERT self.current_offset >= 0 *>

    IF pad > 0 THEN
        end_init_helper(self);
        self.fields.addhi("char " & GenerateNameLocalText(self) & "[" & IntToDec(pad) & "];\n");
        initializer_addhi(self, Text_left_brace);
        FOR i := 1 TO pad DO
            initializer_addhi(self, "0 /* " & IntToDec(i) & " */ ");
        END;
        initializer_addhi(self, "}");
    END;
END init_to_offset;

PROCEDURE end_init_helper(self: T) =
BEGIN
    IF self.init_type_count > 0 THEN
        self.fields.addhi("[" & IntToDec(self.init_type_count) & "];\n");
        self.initializer.addhi("}");
    END;
    self.init_type_count := 0;
END end_init_helper;

PROCEDURE init_helper(self: T; offset: ByteOffset; type: CGType) =
BEGIN
(*
    IF DebugVerbose(self) THEN
      self.comment("init_helper offset:" & IntToDec(offset) & " type:"
        & cgtypeToText[type]);
    ELSE
      self.comment("init_helper");
    END;
*)
    init_to_offset(self, offset);
    IF offset = 0 OR self.init_type # type OR offset # self.current_offset THEN
        end_init_helper(self);
        self.fields.addhi(cgtypeToText[type] & " " & GenerateNameLocalText(self));
        initializer_addhi(self, Text_left_brace);
    END;
    INC(self.init_type_count);
    self.init_type := type;
    self.current_offset := offset + TargetMap.CG_Bytes[type];
END init_helper;

PROCEDURE init_int(
    self: T;
    offset: ByteOffset;
    READONLY value: Target.Int;
    type: CGType) =
BEGIN
    IF DebugVerbose(self) THEN
      self.comment("init_int offset:" & IntToDec(offset)
        & " value:" & TInt.ToText(value) & " type:" & cgtypeToText[type]);
    ELSE
      self.comment("init_int");
    END;
    init_helper(self, offset, type);
    (* TIntLiteral includes suffixes like T, ULL, UI64, etc. *)
    initializer_addhi(self, self.TIntLiteral(type, value));
END init_int;

PROCEDURE Segments_init_int(
    self: Segments_t;
    offset: ByteOffset;
    READONLY value: Target.Int;
    type: CGType) =
BEGIN
    init_int(self.self, offset, value, type);
END Segments_init_int;

PROCEDURE init_proc(self: T; offset: ByteOffset; p: M3CG.Proc) =
VAR proc := NARROW(p, Proc_t);
BEGIN
    IF DebugVerbose(self) THEN
      self.comment("init_proc offset:" & IntToDec(offset));
    ELSE
      self.comment("init_proc");
    END;
    init_helper(self, offset, CGType.Addr); (* FUTURE: better typing *)
    initializer_addhi(self, "(ADDRESS)&" & NameT(proc.name));
END init_proc;

PROCEDURE MarkUsed_proc(self: T; p: M3CG.Proc) =
VAR proc: Proc_t;
BEGIN

    <* ASSERT p # NIL *>
    <* ASSERT self.procs_pending_output # NIL *>

    proc := NARROW(p, Proc_t);
    IF proc.pending_output OR proc.output THEN RETURN END;
    proc.pending_output := TRUE;
    self.procs_pending_output.addhi(proc);
END MarkUsed_proc;

PROCEDURE MarkUsed_init_proc(self: MarkUsed_t; <*UNUSED*>offset: ByteOffset;
    p: M3CG.Proc) =
BEGIN
    MarkUsed_proc(self.self, p);
END MarkUsed_init_proc;

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
    IF DebugVerbose(self) THEN
      self.comment("init_var offset:" & IntToDec(offset));
    ELSE
      self.comment("init_var");
    END;
    init_helper(self, offset, CGType.Addr); (* FUTURE: better typing *)
    IF bias # 0 THEN
        bias_text := IntToDec(bias) & "+";
    END;
    initializer_addhi(self, bias_text & "(char*)&" & NameT(var.name));
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
    debug_ch: CHAR;
    debug_initializer := self.debug_initializer;
BEGIN

    <* ASSERT debug_initializer # NIL *>
    <* ASSERT debug_initializer.st = 0 *>
    <* ASSERT debug_initializer.sz = 0 *>

    IF length # 0 THEN

      FOR i := 0 TO length - 1 DO
        init_helper(self, offset + i, CGType.Word8);
        ch := Text.GetChar(value, i);
        debug_ch := ch;
        IF ch IN Printable THEN
            IF ch = '*' OR ch = '/' THEN (* Avoid breaking comment. *)
              debug_ch := '.';
            END;
            initializer_addhi(self, "'" & Text.Sub(value, i, 1) & "'");
        ELSE
            debug_ch := '.';
            initializer_addhi(self, IntToDec(ORD(ch)));
        END;
        debug_initializer.addhi(debug_ch);
      END;
    END;

    IF DebugVerbose(self) THEN
      self.comment("init_chars offset:" & IntToDec(offset) &
        " length:" & IntToDec(length) &
        " value:" &  Text.FromChars(SUBARRAY(debug_initializer.elem^, 0, debug_initializer.sz)));
    ELSE
      self.comment("init_chars");
    END;

    debug_initializer.sz := 0;
    debug_initializer.st := 0;

END init_chars;

PROCEDURE Segments_init_chars(self: Segments_t; offset: ByteOffset; value: TEXT) =
BEGIN
    init_chars(self.self, offset, value);
END Segments_init_chars;

PROCEDURE TIntToExpr(self: T; READONLY i: Target.Int): Expr_t =
VAR e := NEW(Expr_ConstantInt_t,
             expr_type := ExprType.ConstantInt,
             self := self,
             cgtype := Target.Integer.cg_type);
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

PROCEDURE TIntLiteral(self: T; type: CGType; READONLY i: Target.Int): TEXT =
VAR ok1 := TRUE;
    ok2 := TRUE;
BEGIN
    CASE type OF
        | CGType.Int8   => ok1 := TInt.GE(i, TInt.Min8 );
                         (*ok2 := TInt.LE(i, TInt.Max8);*)
                           ok2 := TInt.LE(i, TWord.Max8);
        | CGType.Int16  => ok1 := TInt.GE(i, TInt.Min16);
                         (*ok2 := TInt.LE(i, TInt.Max16);*)
                           ok2 := TInt.LE(i, TWord.Max16);
        | CGType.Int32  => ok1 := TInt.GE(i, TInt.Min32);
                         (*ok2 := TInt.LE(i, TInt.Max32);*)
                           ok2 := TInt.LE(i, TWord.Max32);
        | CGType.Int64  => ok1 := TInt.GE(i, TInt.Min64);
                         (*ok2 := TInt.LE(i, TInt.Max64);*)
                           ok2 := TInt.LE(i, TWord.Max64);
        | CGType.Word8  => ok1 := TWord.LE(i, TWord.Max8);
        | CGType.Word16 => ok1 := TWord.LE(i, TWord.Max16);
        | CGType.Word32 => ok1 := TWord.LE(i, TWord.Max32);
        | CGType.Word64 => ok1 := TWord.LE(i, TWord.Max64);
        ELSE
            RTIO.PutText("TIntLiteral:invalid type=" & cgtypeToText[type] & "\n");
            RTIO.Flush();
            <* ASSERT FALSE *>
    END;
    IF NOT ok1 OR NOT ok2 THEN
        RTIO.PutText("TIntLiteral:type=" & cgtypeToText[type]
                     & " i=" & TInt.ToText(i)
                     & " ok1=" & BoolToText[ok1]
                     & " ok2=" & BoolToText[ok2] & "\n");
        RTIO.Flush();
        Wr.Flush(self.c);
        <* ASSERT ok1 *>
        (* There are tests that fail this and m3front allows it, with warnings. *)
        (* ASSERT ok2 *)
    END;
    IF type = CGType.Int32 AND TInt.EQ(i, TInt.Min32) THEN
        RETURN "-" & intLiteralPrefix[type] & TInt.ToText(TInt.Max32) & intLiteralSuffix[type] & "-1";
    ELSIF type = CGType.Int64 AND TInt.EQ(i, TInt.Min64) THEN
        RETURN "-" & intLiteralPrefix[type] & TInt.ToText(TInt.Max64) & intLiteralSuffix[type] & "-1";
    ELSE
        RETURN intLiteralPrefix[type] & TInt.ToText(i) & intLiteralSuffix[type];
    END;
END TIntLiteral;

PROCEDURE IntLiteral(self: T; type: CGType; i: INTEGER): TEXT =
BEGIN
    RETURN self.TIntLiteral(type, IntToTarget(self, i));
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
VAR cg_type := TargetMap.Float_types[TFloat.Prec(float)].cg_type;
BEGIN
    IF DebugVerbose(self) THEN
      self.comment("init_float offset:" & IntToDec(offset) & " type:"
        & cgtypeToText[cg_type]);
    ELSE
      self.comment("init_float");
    END;
    init_helper(self, offset, cg_type);
    initializer_addhi(self, FloatLiteral(float));
END init_float;

PROCEDURE Segments_init_float(
    self: Segments_t; offset: ByteOffset; READONLY float: Target.Float) =
BEGIN
    init_float(self.self, offset, float);
END Segments_init_float;

(*------------------------------------------------------------ PROCEDUREs ---*)

PROCEDURE import_procedure(
    self: T; name: Name; parameter_count: INTEGER;
    return_type: CGType; callingConvention: CallingConvention;
    return_typeid: TypeUID;
    return_typename: M3ID.T): M3CG.Proc =
VAR return_type_text := NameT(return_typename);
    proc := NEW(Proc_t, name := name, parameter_count := parameter_count,
                return_type := return_type, imported := TRUE,
                return_type_text := TypeText (self, return_type, return_type_text),
                callingConvention := callingConvention).Init(self);
BEGIN
    IF DebugVerbose(self) THEN
        self.comment("import_procedure name:" & NameT(name)
            & " parameter_count:" & IntToDec(parameter_count)
            & " return_type:" & cgtypeToText[return_type]
            & " return_typeid:" & TypeIDToText(return_typeid)
            & " return_typename:" & TextOrNil(return_type_text));
    ELSE
        self.comment("import_procedure");
    END;
    IF name = self.RTHooks_ReportFault_id THEN
        self.RTHooks_ReportFault_imported_or_declared := TRUE;
        no_return(self);
    END;
    SuppressLineDirective(self, parameter_count, "import_procedure parameter_count");
    self.param_proc := proc;
    self.param_count := 0;
    IF parameter_count = 0 THEN
        last_param(self);
    END;
    self.imported_procs.addhi(proc);
    RETURN proc;
END import_procedure;

PROCEDURE Locals_declare_procedure(
    self: Locals_t;
    name: Name;
    parameter_count: INTEGER;
    return_type: CGType;
    level: INTEGER;
    callingConvention: CallingConvention;
    exported: BOOLEAN;
    parent: M3CG.Proc;
    return_typeid: TypeUID;
    return_typename: M3ID.T): M3CG.Proc =
BEGIN
    RETURN declare_procedure(
        self.self,
        name,
        parameter_count,
        return_type,
        level,
        callingConvention,
        exported,
        parent,
        return_typeid,
        return_typename);
END Locals_declare_procedure;

PROCEDURE ProcNameOrNIL(proc: M3CG.Proc): TEXT =
BEGIN
    IF proc = NIL THEN
        RETURN "NIL";
    END;
    RETURN NameT(NARROW(proc, Proc_t).name);
END ProcNameOrNIL;

PROCEDURE declare_procedure(
    self: T; name: Name; parameter_count: INTEGER;
    return_type: CGType; level: INTEGER;
    callingConvention: CallingConvention;
    exported: BOOLEAN; parent: M3CG.Proc;
    return_typeid: TypeUID;
    return_typename: M3ID.T): M3CG.Proc =
VAR return_type_text := NameT(return_typename);
    proc := NEW(Proc_t, name := name, parameter_count := parameter_count,
                return_type := return_type, level := level,
                return_type_text := TypeText (self, return_type, return_type_text),
                callingConvention := callingConvention, exported := exported,
                parent := parent).Init(self);
BEGIN
    IF DebugVerbose(self) THEN
        self.comment("declare_procedure name:" & NameT(name)
            & " parameter_count:" & IntToDec(parameter_count)
            & " return_type:" & cgtypeToText[return_type]
            & " exported:" & BoolToText[exported]
            & " level:" & IntToDec(level)
            & " parent:" & ProcNameOrNIL(parent)
            & " return_typeid:" & TypeIDToText(return_typeid)
            & " return_typename:" & TextOrNil(return_type_text));
    ELSE
        self.comment("declare_procedure");
    END;
    IF name = self.RTHooks_ReportFault_id THEN
        self.RTHooks_ReportFault_imported_or_declared := TRUE;
    END;
    self.param_proc := proc;
    self.param_count := 0;
    IF NOT self.in_proc THEN
        self.current_proc := proc;
    END;
    IF parameter_count = 0 THEN
        last_param(self);
    END;
    SuppressLineDirective(self, parameter_count, "declare_procedure parameter_count");
    self.declared_procs.addhi(proc);
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
    IF DebugVerbose(self) THEN
        self.comment("internal_begin_procedure:", NameT(proc.name));
    ELSE
        self.comment("internal_begin_procedure");
    END;
    IF self.in_proc THEN (* TODO don't require this *)
        Err(self, "internal_begin_procedure: in_proc; C backend requires "
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
    struct := "";
BEGIN
    IF DebugVerbose(self) THEN
        self.comment("begin_procedure:", NameT(proc.name));
    ELSE
        self.comment("begin_procedure");
    END;

    <* ASSERT NOT self.in_proc *>
    self.in_proc := TRUE;
    self.current_proc := proc;

    (* declare frame type *)

    IF proc.forward_declared_frame_type THEN
        print(self, "struct " & frame_type & " {\n");

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
                print(self, local.Declare() & " = { 0 };\n");
            END;
        END;
    END;

    (* declare and zero non-uplevel struct param values (uplevels are in the frame struct) *)

    IF NOT PassStructsByValue THEN
        FOR i := FIRST(params^) TO LAST(params^) DO
            WITH param = params[i] DO
                IF (NOT param.up_level) AND (param.cgtype = CGType.Struct) AND param.used THEN
                    print(self, Var_Type(param) & " " & Var_Name(param) & ";\n");
                END;
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
                    struct := "";
                    IF param.cgtype = CGType.Struct THEN
                        IF PassStructsByValue THEN
                            struct := "&";
                        ELSE
                            struct := "*";
                        END;
                    END;
                    print(self, frame_name & "." & Var_Name(param) & "=" & struct & Param_Name(param) & ";\n");
                END;
            END;
        END;

        (* quash unused warning *) (* TODO cleanup -- only declare if needed *)
        print(self, frame_name & "._unused=(ADDRESS)&" & frame_name & ";\n");
    END;

    (* copy non-uplevel struct params from pointers to local value *)

    FOR i := FIRST(params^) TO LAST(params^) DO
        WITH param = params[i] DO
            IF (NOT PassStructsByValue) AND (NOT param.up_level) AND param.cgtype = CGType.Struct AND param.used THEN
                print(self, Var_Name(param) & "=*" & Param_Name(param) & ";\n");
            END;
        END;
    END;
END begin_procedure;

PROCEDURE end_procedure(self: T; <*UNUSED*>p: M3CG.Proc) =
(*VAR proc := NARROW(p, Proc_t);*)
BEGIN
    self.comment("end_procedure");
    (*self.comment("end_procedure " & NameT(proc.name));*)
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

<*UNUSED*>TYPE internal_compare_t = RECORD
    op: CompareOp;
    left: Expr_t := NIL;
    right: Expr_t := NIL;
    value_valid := FALSE;
    value := FALSE;
    comment: TEXT := NIL;
    text: TEXT := NIL;
    cgtype := CGType.Void;
END;

(*PROCEDURE remove_comments(text: TEXT): TEXT =
BEGIN
    RETURN TextUtils.Substitute(TextUtils.Substitute(TextUtils.Substitute(text, "/*", ""), "*/", ""), "  ", " ");
END remove_comments;*)

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
    IF DebugVerbose(self) THEN
        self.comment("if_true_or_false " & BoolToText[value] & " type:" & cgtypeToText[itype]);
    ELSE
        self.comment("if_true_or_false");
    END;
    IF TRUE (* AvoidGccTypeRangeWarnings  *) THEN
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
    IF DebugVerbose(self) THEN
        self.comment("if_compare " & "op:" & CompareOpName[op] & " type:" & cgtypeToText[ztype]);
    ELSE
        self.comment("if_compare");
    END;
    pop(self, 2);
    print(self, "if(m3_" & CompareOpName[op] & "(" & cgtypeToText[ztype]
        & ",\n " & s1.CText() & ",\n " & s0.CText()
        & "))goto L" & LabelToText(label) & ";\n");
END if_compare;

PROCEDURE case_jump(self: T; itype: IType; READONLY labels: ARRAY OF Label) =
(* "GOTO labels[s0.itype]; pop" with no range checking on s0.itype *)
VAR s0 := cast(get(self, 0), itype);
BEGIN
    self.comment("case_jump");
    print(self, "switch(" & s0.CText() & "){\n");
    IF CaseDefaultAssertFalse THEN
        print(self, "default:assert(!\"case_jump hit default\");\n");
    END;
    FOR i := FIRST(labels) TO LAST(labels) DO
        print(self, "case " & IntToDec(i) & ":goto L" & LabelToText(labels[i]) & ";\n");
    END;
    print(self, "}");
    pop(self);
END case_jump;

PROCEDURE exit_proc(self: T; type: CGType) =
(* Returns s0.type if type is not Void, otherwise returns no value. *)
VAR proc := self.current_proc;
    cast1, cast2, cast3, cast4 := "";
BEGIN
    self.comment("exit_proc");
    <* ASSERT self.in_proc *>
    <* ASSERT proc # NIL *>
    <* ASSERT NOT proc.is_RTException_Raise *>
    IF proc.no_return THEN
        <* ASSERT proc.exit_proc_skipped = 0 *>
        <* ASSERT type = CGType.Void  *>
        INC(proc.exit_proc_skipped);
        RETURN;
    END;
    IF type = CGType.Void THEN
        IF NOT proc.is_RTHooks_Raise THEN
            print(self, "return;\n");
        END;
    ELSE
        (* TODO Is the cast avoidable? *)
        IF type = CGType.Addr OR (NOT ReturnStructsByValue AND type = CGType.Struct) THEN
            cast1 := "(";
            cast2 := proc.return_type_text;
            cast3 := ")(";
            cast4 := ")";
        END;
        print(self, "return " & cast1 & cast2 & cast3 & get(self).CText() & cast4 & ";\n");
        pop(self);
    END;
END exit_proc;

(*------------------------------------------------------------ load/store ---*)

PROCEDURE address_plus_offset(in: TEXT; in_offset: INTEGER): Expr_t =
VAR pre := "("; post := ")";
BEGIN
    IF in_offset # 0 THEN
        pre := "((" & IntToDec(in_offset) & ")+(char*)(";
        post := "))";
    END;
    RETURN CTextToExpr(pre & in & post);
END address_plus_offset;

PROCEDURE Variable(self: T; var: Var_t): Expr_t =
VAR expr := NEW(Expr_Variable_t,
                expr_type := ExprType.Variable,
                var := var,
                cgtype := var.cgtype,
                typeid := var.typeid,
                current_proc := self.current_proc);
BEGIN
(* TODO: subranges *)
    expr.minmax_valid := minMaxPossiblyValidForType[var.cgtype];
    expr.minmax := typeMinMax[var.cgtype];
    RETURN expr;
END Variable;

PROCEDURE AddressOf(expr: Expr_t): Expr_t =
(* TODO VAR type: Type_t; *)
BEGIN
    (* TODO EVAL ResolveType(expr.typeid, type); *)
    RETURN NEW(Expr_t,
               expr_type := ExprType.AddressOf,
               cgtype := CGType.Addr,
               points_to_cgtype := expr.cgtype,
               refers_to_typeid := expr.typeid,
               (* TODO refers_to_type := expr.type, *)
               left := expr,
               c_unop_text := "&"
               (* TODO c_unop_text := "(void* )&" *)
               (* c_unop_text := "(ADDRESS)&" *)
               );
END AddressOf;

PROCEDURE Deref(expr: Expr_t): Expr_t =
BEGIN
    RETURN
        NEW(Expr_t,
            expr_type := ExprType.Deref,
            cgtype := expr.points_to_cgtype,
            typeid := expr.refers_to_typeid,
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
    IF DebugVerbose(self) THEN
      self.comment("load var:", Var_Name(var),
        " offset:" & IntToDec(offset),
        " in_mtype:" & cgtypeToText[in_mtype] &
        " out_ztype:" & cgtypeToText[out_ztype]);
    ELSE
      self.comment("load");
    END;

    IF FALSE THEN
        IF NOT in_mtype = var.cgtype THEN
            RTIO.PutText("load in_mtype:" & cgtypeToText[in_mtype] & " var.cgtype:" & cgtypeToText[var.cgtype]);
            RTIO.Flush();
            <* ASSERT in_mtype = var.cgtype *>
        END;
    END;
    IF offset # 0 OR var.cgtype # in_mtype THEN
        expr := AddressOf(expr);
        IF offset # 0 THEN
            expr := cast(expr, type := CGType.Addr);
            expr := NEW(Expr_t, right := expr, left := IntToExpr(self, offset), c_binop_text := "+");
        END;
        expr := cast(expr, type_text := cgtypeToText[in_mtype] & "*");
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
    print(self, "(*(" & cgtypeToText[out_mtype] & "*)" & address_plus_offset(out_address, out_offset).CText() & ")=(" & cgtypeToText[in_ztype] & ")(" & in & ");\n");
END store_helper;

PROCEDURE store(self: T; v: M3CG.Var; offset: ByteOffset; ztype: ZType; mtype: MType) =
(* Mem [ ADR(var) + offset ].mtype := s0.ztype; pop *)
VAR var := NARROW(v, Var_t);
    s0 := cast(get(self, 0), ztype);
BEGIN
    IF DebugVerbose(self) THEN
      self.comment("store var:", Var_Name(var),
        " offset:" & IntToDec(offset),
        " ztype:" & cgtypeToText[ztype] &
        " mtype:" & cgtypeToText[mtype]);
    ELSE
      self.comment("store");
    END;
    pop(self);
    store_helper(self, s0.CText(), ztype, "&" & follow_static_link(self.current_proc, var) & NameT(var.name), offset, mtype);
END store;

PROCEDURE load_address(self: T; v: M3CG.Var; offset: ByteOffset) =
(* push; s0.A := ADR(var) + offset *)
VAR var := NARROW(v, Var_t);
    expr: Expr_t := NIL;
BEGIN
    IF DebugVerbose(self) THEN
        self.comment("load_address var:", Var_Name(var),
            " offset:", IntToDec(offset));
    ELSE
        self.comment("load_address");
    END;
    expr := AddressOf(Variable(self, var));
    IF offset # 0 THEN
        expr := cast(expr, type := CGType.Addr);
        expr := NEW(Expr_t, right := expr, left := IntToExpr(self, offset), c_binop_text := "+");
    END;
    push(self, CGType.Addr, expr);
END load_address;

PROCEDURE load_indirect(self: T; offset: ByteOffset; mtype: MType; ztype: ZType) =
(* s0.ztype := Mem [s0.A + offset].mtype  *)
VAR expr := get(self);
BEGIN
    IF DebugVerbose(self) THEN
      self.comment("load_indirect",
        " offset:" & IntToDec(offset),
        " mtype:" & cgtypeToText[mtype] &
        " ztype:" & cgtypeToText[ztype]);
    ELSE
      self.comment("load_indirect");
    END;
    <* ASSERT CG_Bytes[ztype] >= CG_Bytes[mtype] *>
    pop(self);
    IF offset # 0 THEN
        expr := cast(expr, type := CGType.Addr); (* might be redundant *)
        expr := NEW(Expr_t, right := expr, left := IntToExpr(self, offset), c_binop_text := "+");
    END;
    expr := CastAndDeref(expr, type_text := cgtypeToText[mtype] & "*"); (* cast might be redundant *)
    IF mtype # ztype THEN
        expr := cast(expr, ztype);
    END;
    expr.cgtype := ztype;
    push(self, ztype, expr);
END load_indirect;

PROCEDURE store_indirect(self: T; offset: ByteOffset; ztype: ZType; mtype: MType) =
(* Mem [s1.A + offset].mtype := s0.ztype; pop (2) *)
VAR s0 := cast(get(self, 0), ztype);
    s1 := get(self, 1);
BEGIN
    IF DebugVerbose(self) THEN
      self.comment("store_indirect ",
        " offset:" & IntToDec(offset),
        " ztype:" & cgtypeToText[ztype],
        " mtype:" & cgtypeToText[mtype]);
    ELSE
      self.comment("store_indirect");
    END;

    pop(self, 2);
    store_helper(self, s0.CText(), ztype, s1.CText(), offset, mtype);
END store_indirect;

(*-------------------------------------------------------------- literals ---*)

PROCEDURE load_nil(self: T) =
(* push; s0.A := NIL *)
BEGIN
    self.comment("load_nil");
    push(self, CGType.Addr, CTextToExpr("0")); (* UNDONE NULL or (ADDRESS)0? *)
END load_nil;

<*UNUSED*>PROCEDURE TIntInc(self: T; i: Target.Int): Target.Int =
VAR j: Target.Int;
BEGIN
    IF NOT TInt.Add(i, TInt.One, j) THEN
        Err(self, "failed to increment target integer");
    END;
    RETURN j;
END TIntInc;

CONST IntToTInt = IntToTarget;

PROCEDURE IntToTarget(self: T; i: INTEGER): Target.Int =
VAR j: Target.Int;
BEGIN
    IF NOT TInt.FromInt(i, j) THEN
        Err(self, "failed to convert host integer to target integer");
    END;
    RETURN j;
END IntToTarget;

PROCEDURE load_host_integer(self: T; type: IType; i: INTEGER) =
BEGIN
    IF DebugVerbose(self) THEN
        comment(self, "load_host_integer:", IntToDec(i));
    ELSE
        comment(self, "load_host_integer");
    END;
    self.load_integer(type, IntToTarget(self, i));
END load_host_integer;

PROCEDURE load_target_integer(self: T; type: IType; READONLY readonly_i: Target.Int) =
(* push; s0.type := i *)
VAR i := readonly_i;
    expr := CTextToExpr(self.TIntLiteral(type, i));
    size := cgtypeSizeBytes[type];
    signed   := cgtypeIsSignedInt[type];
    unsigned := cgtypeIsUnsignedInt[type];
BEGIN
    self.comment("load_integer");
    <* ASSERT signed OR unsigned *>
    expr.minmax_valid[Min] := TRUE;
    expr.minmax_valid[Max] := TRUE;
    expr.cgtype := type;
    IF cgtypeIsUnsignedInt[type] THEN TIntN.ZeroExtend(i, size)
    ELSIF cgtypeIsSignedInt[type] THEN TIntN.SignExtend(i, size) END;
    <* ASSERT readonly_i = i *>
    expr.minmax[Min] := i;
    expr.minmax[Max] := i;
    expr := cast(expr, type);
    expr.cgtype := type;
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
VAR from_type     := from.cgtype;
    from_signed   := cgtypeIsSignedInt[from_type];
    from_unsigned := cgtypeIsUnsignedInt[from_type];
    to_type       := to.cgtype;
    to_signed     := cgtypeIsSignedInt[to_type];
    to_unsigned   := cgtypeIsUnsignedInt[to_type];
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
    IF NOT cgtypeIsInteger[to.cgtype] THEN
        to.minmax_valid := minMaxFalse;
        RETURN;
    END;
    IF from.cgtype = to.cgtype THEN
        to.minmax := from.minmax;
        to.minmax_valid := from.minmax_valid;
        RETURN;
    END;
    IF NOT InternalTransferMinMax1(from, to, Min)
            OR NOT InternalTransferMinMax1(from, to, Max) THEN
        (* punt and extend range arbitrarily; this could be better *)
        to.minmax := typeMinMax[to.cgtype];
    END;
END TransferMinMax;

PROCEDURE cast(expr: Expr_t; type: CGType := CGType.Void; type_text: TEXT := NIL): Expr_t =
VAR e := NEW(Expr_Cast_t, cgtype := type, type_text := type_text, left := expr);
BEGIN
    <* ASSERT (type = CGType.Void) # (type_text = NIL) *>
    (* casts are either truncating or sign extending or zero extending *)
    TransferMinMax(expr, e);
    RETURN e;
END cast;

<*UNUSED*>PROCEDURE old_Cast(expr: Expr_t; type: CGType := CGType.Void; type_text: TEXT := NIL): Expr_t =
BEGIN
    <* ASSERT (type = CGType.Void) # (type_text = NIL) *>
    RETURN NEW(Expr_Cast_t, cgtype := type, type_text := type_text, left := expr);
END old_Cast;

PROCEDURE CastAndDeref(expr: Expr_t; type: CGType := CGType.Void; type_text: TEXT := NIL): Expr_t =
BEGIN
    RETURN Deref(cast(expr, type, type_text));
END CastAndDeref;

PROCEDURE op1(self: T; type: CGType; name, op: TEXT) =
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
VAR size := cgtypeSizeBytes[type];
    signed   := cgtypeIsSignedInt[type];
    unsigned := cgtypeIsUnsignedInt[type];
BEGIN
    <* ASSERT signed OR unsigned *>
    RETURN ((ARRAY BOOLEAN OF TIntExtendOrTruncate_t{TWord.Truncate, TInt.Extend})[signed])(in, size, out);
END TIntExtendOrTruncate;

PROCEDURE
op2(
    self: T;
    type: CGType;
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
    cast1 := "";
    cast2 := "";
BEGIN
    IF DebugVerbose(self) THEN
        self.comment("compare " & "op:" & CompareOpName[op] & " type:" & cgtypeToText[ztype]);
    ELSE
        self.comment("compare");
    END;
    pop(self, 2);
    IF ztype = CGType.Addr THEN
        cast1 := "((ADDRESS)";
        cast2 := ")";
    END;
    IF TRUE (* AvoidGccTypeRangeWarnings *) THEN
        push(self, itype, cast(CTextToExpr("m3_" & CompareOpName[op] & "(" & cgtypeToText[ztype] & ",\n " & cast1 & s1.CText() & cast2 & ",\n " & cast1 & s0.CText() & cast2 & ")"), itype));
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
    IF ((a = b) AND (a # Sign.Unknown)) OR cgtypeIsUnsignedInt[type] THEN
        push(self, type, cast(CTextToExpr(s1.CText() & "/" & s0.CText()), type));
    ELSE
        push(self, type, cast(CTextToExpr("m3_div_" & cgtypeToText[type] & "(\n " & s1.CText() & ",\n " & s0.CText() & ")"), type));
    END;
END div;

PROCEDURE mod(self: T; type: IType; a, b: Sign) =
(* s1.type := s1.type MOD s0.type; pop *)
VAR s0 := cast(get(self, 0), type);
    s1 := cast(get(self, 1), type);
BEGIN
    self.comment("mod");
    pop(self, 2);
    IF ((a = b) AND (a # Sign.Unknown)) OR cgtypeIsUnsignedInt[type] THEN
        push(self, type, cast(CTextToExpr(s1.CText() & "%" & s0.CText()), type));
    ELSE
        push(self, type, cast(CTextToExpr("m3_mod_" & cgtypeToText[type] & "(\n " & s1.CText() & ",\n " & s0.CText() & ")"), type));
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
    push(self, type, CTextToExpr("m3_abs_" & cgtypeToText[type] & "(\n " & s0.CText() & ")"));
END abs;

PROCEDURE max(self: T; type: ZType) =
(* s1.type := MAX (s1.type, s0.type); pop *)
VAR s0 := cast(get(self, 0), type);
    s1 := cast(get(self, 1), type);
BEGIN
    self.comment("max");
    pop(self, 2);
    push(self, type, CTextToExpr("m3_max_" & cgtypeToText[type] & "(\n " & s0.CText() & ",\n " & s1.CText() & ")"));
END max;

PROCEDURE min(self: T; type: ZType) =
(* s1.type := MIN (s1.type, s0.type); pop *)
VAR s0 := cast(get(self, 0), type);
    s1 := cast(get(self, 1), type);
BEGIN
    self.comment("min");
    pop(self, 2);
    push(self, type, CTextToExpr("m3_min_" & cgtypeToText[type] & "(\n " & s0.CText() & ",\n " & s1.CText() & ")"));
END min;

PROCEDURE cvt_int(self: T; from_float_type: RType; to_integer_type: IType; op: ConvertOp) =
(* s0.itype := ROUND(s0.rtype) *)
VAR s0 := cast(get(self, 0), from_float_type);
BEGIN
    self.comment("cvt_int");
    pop(self);
    push(self, to_integer_type, cast(CTextToExpr("m3_" & ConvertOpName[op] & "(\n " & s0.CText() & ")"), to_integer_type));
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
VAR s0 := cast(get(self, 0), CGType.Addr);
    s1 := cast(get(self, 1), CGType.Addr);
    s2 := cast(get(self, 2), CGType.Addr);
    target_word_bytes := Target.Word.bytes;
BEGIN
    self.comment(op);
    <* ASSERT (byte_size MOD target_word_bytes) = 0 *>
    pop(self, 3);
    print(self, "m3_" & op & "(\n " & IntToDec(byte_size DIV target_word_bytes) & ",(WORD_T*)" & s0.CText() & ",(WORD_T*)" & s1.CText() & ",(WORD_T*)" & s2.CText() & ");\n");
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
    s1 := cast(get(self, 1), CGType.Void, "SET");
BEGIN
    self.comment("set_member");
    pop(self, 2);
    push(self, type, cast(CTextToExpr("m3_set_member(" & s0.CText() & ",\n " & s1.CText() & ")"), type));
END set_member;

PROCEDURE set_compare(self: T; byte_size: ByteSize; op: CompareOp; type: IType) =
(* s1.type := (s1.B op s0.B); pop *)
VAR swap := (op IN SET OF CompareOp{CompareOp.GT, CompareOp.GE});
    s0 := cast(get(self, ORD(swap)), CGType.Void, "SET");
    s1 := cast(get(self, ORD(NOT swap)), CGType.Void, "SET");
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
        push(self, type, cast(CTextToExpr("m3_set_" & CompareOpName[op] & "(\n " & IntLiteral(self, Target.Word.cg_type, byte_size) & ",\n " & s1.CText() & ",\n " & s0.CText() & ")"), type));
    ELSE
        <* ASSERT op IN SET OF CompareOp{CompareOp.EQ, CompareOp.NE} *>
        push(self, type, cast(CTextToExpr("memcmp(" & s1.CText() & ",\n " & s0.CText() & ",\n " & IntLiteral(self, Target.Word.cg_type, byte_size) & ")" & eq), type));
    END;
END set_compare;

PROCEDURE set_range(self: T; <*UNUSED*>byte_size: ByteSize; type: IType) =
(* s2.A [s1.type .. s0.type] := 1's; pop(3) *)
VAR s0 := cast(get(self, 0), type);
    s1 := cast(get(self, 1), type);
    s2 := cast(get(self, 2), CGType.Void, "SET");
BEGIN
    self.comment("set_range");
    pop(self, 3);
    print(self, "m3_set_range(" & s0.CText() & ",\n " & s1.CText() & ",\n " & s2.CText() & ");\n");
END set_range;

PROCEDURE set_singleton(self: T; <*UNUSED*>byte_size: ByteSize; type: IType) =
(* s1.A [s0.type] := 1; pop(2) *)
VAR s0 := cast(get(self, 0), type);
    s1 := cast(get(self, 1), CGType.Void, "SET");
BEGIN
    self.comment("set_singleton");
    pop(self, 2);
    print(self, "m3_set_singleton(" & s0.CText() & ",\n " & s1.CText() & ");\n");
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
    IF DebugVerbose(self) THEN
        self.comment("xor type:" & cgtypeToText[type]);
    ELSE
        self.comment("xor");
    END;
    pop(self, 2);
    push(self, type, CTextToExpr("m3_xor(" & cgtypeToText[type] & ",\n " & s1.CText() & ",\n " & s0.CText() & ")"));
END xor;

PROCEDURE shift_left_or_right(self: T; type: IType; name, op: TEXT) =
VAR s0 := cast(get(self, 0), Target.Word.cg_type);
    s1 := cast(get(self, 1), type);
BEGIN
    self.comment(name);
    pop(self, 2);
    push(self, type, CTextToExpr(s1.CText() & op & s0.CText()));
END shift_left_or_right;

PROCEDURE casted_shift_left(self: T; type: IType) =
(* s1.type := Word.Shift  (s1.type, s0.Word); pop *)
(* int f() { return -1 << 1; }
 * cc -c 1.c
 * warning: shifting a negative signed value is undefined [-Wshift-negative-value]
 * So cast to unsigned and back -- which also might warn about casting negative number.
 *)
VAR s0 := cast(get(self, 0), Target.Word.cg_type);
    s1 := cast(cast(get(self, 1), type), typeToUnsigned[type]);
BEGIN
    self.comment("shift_lexft");
    pop(self, 2);
    push(self, type, cast(CTextToExpr(s1.CText() & "<<" & s0.CText()), type));
END casted_shift_left;

PROCEDURE shift_left(self: T; type: IType) =
(* s1.type := Word.Shift  (s1.type, s0.Word); pop *)
BEGIN
    IF typeToUnsigned[type] # type THEN
      casted_shift_left(self, type);
    ELSE
      shift_left_or_right(self, type, "shift_left", "<<");
    END;
END shift_left;

PROCEDURE shift_right(self: T; type: IType) =
(* s1.type := Word.Shift  (s1.type, -s0.type); pop *)
BEGIN
    (* ASSERT cgtypeIsUnsignedInt[type]
    shift is unsigned, casts are applied on input and output
    int i; i << n; => (int) (((unsigned)i) << n)
    *)
    shift_left_or_right(self, type, "shift_right", ">>");
END shift_right;

PROCEDURE shift_or_rotate(self: T; type: IType; which: TEXT; count_type: CGType) =
VAR s0 := cast(get(self, 0), count_type);
    s1 := cast(get(self, 1), type);
BEGIN
    self.comment(which);
    pop(self, 2);
    push(self, type, CTextToExpr("m3_" & which & "_" & cgtypeToText[type] & "(\n " & s1.CText() & ",\n " & s0.CText() & ")"));
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
        push(self, type, CTextToExpr("m3_extract(\n " & cgtypeToText[typeToUnsigned[type]] & ",\n " & value.CText() & ",\n " & offset.CText() & ",\n " & count.CText() & ")"));
    ELSE
        push(self, type, CTextToExpr("m3_extract_" & cgtypeToText[typeToUnsigned[type]] & "(\n " & value.CText() & ",\n " & offset.CText() & ",\n " & count.CText() & ")"));
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
    push(self, type, CTextToExpr("m3_sign_extend_" & cgtypeToText[type] & "(\n " & value.CText() & ",\n " & count.CText() & ")"));
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
    push(self, type, CTextToExpr("m3_insert_" & cgtypeToText[type] & "(\n " & to.CText() & ",\n " & from.CText() & ",\n " & offset.CText() & ",\n " & count.CText() & ")"));
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

PROCEDURE swap(self: T; <*UNUSED*>a, b: CGType) =
(* tmp := s1; s1 := s0; s0 := tmp *)
VAR temp := get(self, 1);
BEGIN
    self.comment("swap");
    self.stack.put(1, get(self, 0));
    self.stack.put(0, temp);
END swap;

PROCEDURE cg_pop(self: T; type: CGType) =
(* pop(1) (i.e. discard s0) *)
VAR s0 := cast(get(self, 0), type);
BEGIN
    self.comment("pop");
    pop(self);
    print(self, "m3_pop_" & cgtypeToText[type] & "(" & s0.CText() & ");\n");
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
    print(self, MemCopyOrMove[ORD(overlap)] & "(\n " & s2.CText() & ",\n " & s1.CText() & ",\n " & IntToDec(CG_Bytes[mtype]) & "*(size_t)" & s0.CText() & ");\n");
END copy_n;

PROCEDURE copy(self: T; n: INTEGER; mtype: MType; overlap: BOOLEAN) =
(* Mem[s1.A:sz] := Mem[s0.A:sz]; pop(2)*)
VAR s0 := get(self, 0);
    s1 := get(self, 1);
BEGIN
    self.comment("copy");
    pop(self, 2);
    print(self, MemCopyOrMove[ORD(overlap)] & "(\n " & s1.CText() & ",\n " & s0.CText() & ",\n " & IntToDec(CG_Bytes[mtype] * n) & ");\n");
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
    self.abort_in_call := self.in_proc_call;
END abort;

PROCEDURE check_nil(self: T; code: RuntimeError) =
(* IF (s0.A = NIL) THEN abort(code) *)
VAR t := self.temp_vars[self.op_index];
BEGIN
    self.comment("check_nil");
    self.store(t, 0, CGType.Addr, CGType.Addr);
    self.load(t, 0, CGType.Addr, CGType.Addr);
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
    print(self, "/*check_lo*/if(" & get(self).CText() & "<" & self.TIntLiteral(type, i) & ")");
    reportfault(self, code);
END check_lo;

PROCEDURE check_hi(self: T; type: IType; READONLY i: Target.Int; code: RuntimeError) =
(* IF (i < s0.type) THEN abort(code) *)
VAR t := self.temp_vars[self.op_index];
BEGIN
    self.comment("check_hi");
    self.store(t, 0, type, type);
    self.load(t, 0, type, type);
    print(self, "/*check_hi*/if(" & self.TIntLiteral(type, i) & "<" & get(self).CText() & ")");
    reportfault(self, code);
END check_hi;

PROCEDURE check_range(self: T; type: IType; READONLY low, high: Target.Int; code: RuntimeError) =
(* IF (s0.type < low) OR (high < s0.type) THEN abort(code) *)
VAR t := self.temp_vars[self.op_index];
    low_expr := CTextToExpr(self.TIntLiteral(type, low));
    high_expr := CTextToExpr(self.TIntLiteral(type, high));
BEGIN
    self.comment("check_range");
    self.store(t, 0, type, type);
    self.load(t, 0, type, type);
    print(self, "if(m3_check_range(" & cgtypeToText[type] & ",\n" & get(self).CText() & ",\n " & low_expr.CText() & ",\n " & high_expr.CText() & "))");
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
VAR s0 := cast(get(self, 0), CGType.Addr);
BEGIN
    self.comment("add_offset");
    pop(self);
    push(self, CGType.Addr, address_plus_offset(s0.CText(), offset));
END add_offset;

PROCEDURE index_address(self: T; type: IType; size: INTEGER) =
(* s1.A := s1.A + s0.type * size; pop *)
VAR s0 := cast(get(self, 0), type);
    s1 := cast(get(self, 1), CGType.Addr);
BEGIN
    self.comment("index_address");
    IF size = 0 THEN
        pop(self);
        <* ASSERT FALSE *>
    ELSE
        pop(self, 2);
        IF size # 1 THEN
            s0 := CTextToExpr(IntToDec(size) & "*" & paren(s0).CText());
        END;
        push(self, CGType.Addr, paren(CTextToExpr(s1.CText() & "+" & paren(s0).CText())));
    END;
END index_address;

(*------------------------------------------------------- PROCEDURE calls ---*)

PROCEDURE start_call_helper(self: T) =
BEGIN
    self.static_link := NIL;
    <* ASSERT self.params.size() = 0 *>
    <* ASSERT NOT self.in_proc_call *>
    self.in_proc_call := TRUE; (* call cannot be nested *)
    self.abort_in_call := FALSE;
END start_call_helper;

PROCEDURE MarkUsed_start_call_direct(self: MarkUsed_t;
    p: M3CG.Proc; <*UNUSED*>level: INTEGER; <*UNUSED*>type: CGType) =
BEGIN
    MarkUsed_proc(self.self, p);
END MarkUsed_start_call_direct;

PROCEDURE start_call_direct(self: T; p: M3CG.Proc; <*UNUSED*>level: INTEGER; <*UNUSED*>type: CGType) =
(* begin a procedure call to a procedure at static level 'level'. *)
VAR proc := NARROW(p, Proc_t);
BEGIN
    self.comment("start_call_direct");
    start_call_helper(self);
    self.in_call_indirect := FALSE;
    self.proc_being_called := proc;

    (* workaround frontend bug? *)
    IF proc.is_exception_handler THEN
        push(self, CGType.Addr, CTextToExpr("0"));
        pop_parameter_helper(self, "0");
    END;
END start_call_direct;

PROCEDURE start_call_indirect(self: T; <*UNUSED*>type: CGType; <*UNUSED*>callingConvention: CallingConvention) =
(* begin a procedure call to a procedure at static level 'level'. *)
BEGIN
    self.comment("start_call_indirect");
    start_call_helper(self);
    self.in_call_indirect := TRUE;
END start_call_indirect;

PROCEDURE pop_parameter_helper(self: T; param: TEXT) =
BEGIN
    <* ASSERT self.in_proc_call *>
    self.params.addhi(param);
    pop(self);
END pop_parameter_helper;

PROCEDURE pop_param(self: T; type: MType) =
(* pop s0 and make it the "next" parameter in the current call *)
(* TODO try to remove cast *)
VAR s0 := cast(get(self, 0), type);
BEGIN
    self.comment("pop_param");
    pop_parameter_helper(self, s0.CText());
END pop_param;

PROCEDURE pop_struct(self: T; typeid: TypeUID; byte_size: ByteSize; alignment: Alignment) =
(* pop s0 and make it the "next" parameter in the current call
 * NOTE: it is passed by value *)
VAR s0 := get(self, 0);
    type: Type_t := NIL;
    type_text: TEXT := NIL;
BEGIN
    IF DebugVerbose(self) THEN
        self.comment("pop_struct typeid:" & TypeIDToText(typeid),
            " byte_size:", IntToDec(byte_size));
    ELSE
        self.comment("pop_struct");
    END;
    <* ASSERT (byte_size MOD alignment) = 0 *>
    <* ASSERT byte_size >= 0 *>

    (* type_text := type.text & "*"; TODO switch to this *)
    type_text := TypeIDToText(typeid) & "*";
    IF NOT ResolveType(self, typeid, type) THEN
        Err(self, "pop_struct: unknown typeid:" & type_text);
    END;
    s0 := cast(s0, type_text := type_text);
    IF PassStructsByValue THEN
        s0 := Deref(s0);
    END;
    pop_parameter_helper(self, s0.CText());
END pop_struct;

PROCEDURE pop_static_link(self: T) =
VAR var := self.temp_vars[self.op_index];
BEGIN
    self.comment("pop_static_link");
    <* ASSERT self.in_proc_call *>
    self.static_link := var;
    self.store(var, 0, CGType.Addr, CGType.Addr);
END pop_static_link;

PROCEDURE Locals_pop_static_link(self: Locals_t) =
VAR x := self.self;
    var := internal_declare_temp(x, CG_Bytes[CGType.Addr], CG_Bytes[CGType.Addr], CGType.Addr);
BEGIN
    <* ASSERT x.temp_vars # NIL *>
    x.temp_vars[x.op_index] := var;
    var.used := TRUE;
END Locals_pop_static_link;

PROCEDURE call_helper(self: T; type: CGType; proc: TEXT) =
VAR comma, t1, t2, t3, t4 := "";
    index := 0;
    direct := NOT self.in_call_indirect;
    types := self.proc_being_called.params;
    values := self.params;
    abort_in_call := self.abort_in_call;
BEGIN
  <* ASSERT self.in_proc_call *>

  self.abort_in_call := FALSE;

  (* m3front invalid IR:
   * m3front issues abort within calls, and then stops
   * issuing parameters, but does issue the call,
   * which the C compiler rejects, insufficient parameters to call.
   *
   * Evaluate whatever parameters we got and then skip the call,
   * which should be unreachable, due to following a fault.
   *
   * TODO: On the theory that abort is continuable in a debugger,
   * consider making this conditional on if sufficient parameters
   * have been seen, not if abort has been seen. They likely coincide.
   *
   * TODO: Consider requiring better IR from m3front.
   *)
  IF abort_in_call THEN
    proc := " /* abort_in_call */ ";
  ELSE
    proc := proc & "(\n ";
  END;

  WHILE values.size() > 0 DO
    t1 := "";
    t2 := "";
    t3 := "";
    t4 := "";
    (* Aborted calls evalute but ignore the parameters. *)
    IF abort_in_call THEN
      t1 := " (void)( ";
      t4 := " ) ";
    ELSIF direct THEN (* TODO? No casts for indirect calls? *)
      (* Fix for setjmp. *)
      IF index = 0 AND self.proc_being_called.is_setjmp THEN
        t1 := "*(jmp_buf*)"
      ELSE
        WITH param = types[index] DO
          IF param.cgtype # CGType.Struct OR param.typeid = UID_ADDR THEN
            t1 := " (";
            (* TODO type.text *)
            t2 := " " & param.type_text;
            t3 := " )(";
            t4 := " )";
          END;
        END;
      END;
      INC(index);
    END;
    proc := proc & comma & t1 & t2 & t3 & values.remlo() & t4;
    IF abort_in_call THEN
      comma := ";\n ";
    ELSE
      comma := ",\n ";
    END;
  END;
  self.proc_being_called := self.dummy_proc;
  self.in_proc_call := FALSE; (* call cannot be nested *)
  IF abort_in_call THEN
    print(self, proc & ";\n");
  ELSIF type = CGType.Void THEN
    print(self, proc & ");\n"); (* print, do not push *)
  ELSE
    push(self, type, CTextToExpr(proc & ")"));
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

PROCEDURE call_direct(self: T; p: M3CG.Proc; type: CGType) =
(* call the procedure identified by M3CG.Proc p. The procedure
   returns a value of type type. *)
VAR proc := NARROW(p, Proc_t);
BEGIN
    self.comment("call_direct");

    <* ASSERT self.in_proc_call *>

    IF proc.level # 0 THEN
        self.params.addhi(get_static_link(self, proc));
    END;

    call_helper(self, type, NameT(proc.name));
END call_direct;

PROCEDURE call_indirect(self: T; type: CGType; <*UNUSED*>callingConvention: CallingConvention) =
(* call the procedure whose address is in s0.A and pop s0. The
   procedure returns a value of type type. *)
VAR s0 := get(self, 0);
    static_link := self.static_link;
BEGIN
    self.comment("call_indirect");

    pop(self);

    <* ASSERT self.in_proc_call *>

    IF static_link # NIL THEN
        self.params.addhi(NameT(static_link.name));
        free_temp(self, static_link);
        self.static_link := NIL;
    END;

    (* UNDONE: cast to more accurate function type *)
    call_helper(
        self,
        type,
        "((" & cgtypeToText[type] & " (__cdecl*)(DOTDOTDOT))" & s0.CText() & ")");

END call_indirect;

(*------------------------------------------- procedure and closure types ---*)

PROCEDURE MarkUsed_load_procedure(self: MarkUsed_t; p: M3CG.Proc) =
BEGIN
    MarkUsed_proc(self.self, p);
END MarkUsed_load_procedure;

PROCEDURE load_procedure(self: T; p: M3CG.Proc) =
(* push; s0.A := ADDR (proc's body) *)
VAR proc := NARROW(p, Proc_t);
BEGIN
    self.comment("load_procedure");
    (* UNDONE? typeing? *)
    push(self, CGType.Addr, CTextToExpr(NameT(proc.name)));
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
    push(self, CGType.Addr, CTextToExpr(get_static_link(self, target)));
END load_static_link;

(*----------------------------------------------------------------- misc. ---*)

PROCEDURE Err(self: T; text: TEXT) =
BEGIN
    self.comment("ERROR:" & text);
    self.Err(text);
    Wr.Flush(self.c);
END Err;

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
    IF self.debug > 4 THEN
        RTIO.PutText(a);
        RTIO.Flush();
    END;
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
    BitsToCGUInt[8] := CGType.Word8;
    BitsToCGUInt[16] := CGType.Word16;
    BitsToCGUInt[32] := CGType.Word32;
    BitsToCGUInt[64] := CGType.Word64;
    BitsToCGInt[8] := CGType.Int8;
    BitsToCGInt[16] := CGType.Int16;
    BitsToCGInt[32] := CGType.Int32;
    BitsToCGInt[64] := CGType.Int64;
    BitsToInt[8] := Text_int8;
    BitsToInt[16] := Text_int16;
    BitsToInt[32] := Text_int32;
    BitsToInt[64] := Text_int64;
    BitsToUInt[8] := Text_uint8;
    BitsToUInt[16] := Text_uint16;
    BitsToUInt[32] := Text_uint32;
    BitsToUInt[64] := Text_uint64;
    SignedAndBitsToCGType[TRUE] := BitsToCGInt;
    SignedAndBitsToCGType[FALSE] := BitsToCGUInt;
END M3C.
