(* MSIR -- the Modula-3 Semantic IR: a typed SSA IR between m3front and
   the code generators.  It keeps the Modula-3 meaning that M3CG and LLVM
   IR lose: traced versus untraced references, EH scopes, and the runtime
   interactions (checks, type tests, allocation, RAISE, method lookup) are
   ops, not code (D20, D31).  Control flow is a flat graph of blocks; the
   structure is in the ops.  MSIR-design.md records the decisions (D1-D36)
   and open questions (O1-O21) cited here.

   Pipeline.  m3front builds one Module per unit as it compiles, through
   MSIRBuilder (m3front/src/msir), and MSIRVerifier checks it.  Each
   lowering first runs MSIRRuntime.Expand, which turns the runtime ops into
   compares, branches, loads and calls of the module's RtHook procedures,
   and then lowers the module:
     MSIRToLLVM  MSIREH.LowerModule (EH scopes to invoke/landingpad/resume),
                 then textual LLVM IR;
     MSIRToM3CG  its own EH lowering, then M3CG for the existing backends.
   MSIRPrinter writes the readable form (<unit>.msir).

   Abstractions and invariants.
   - T: types are structural (Equal); integers of one width are equal
     whatever their sign.  gc_ref T is a traced reference held in a
     register, gc_slot T is memory holding one (D5), ptr T is untraced.  A
     traced load is gc.load, or a plain load recording the front end's
     decision that it needs no barrier (D30); a traced store is gc.store.
   - Value: an SSA value: a constant, a procedure or block parameter, an
     instruction result, or a reference to a global symbol.
   - Insn: an Op, operands, an optional result and op-specific attributes
     (see the Insn accessors).  Each carries the source line current when
     it was built, or the one its builder was given.
   - Block: a label, typed parameters (no phi nodes), and instructions
     ending in one terminator.  Each block belongs to one EH scope.
   - Proc: parameters with modes (VAR and READONLY are ptr, D16), a result,
     and blocks; the first is the entry, and BuildAlloca puts every alloca
     there.  It also carries attributes (RAISES set, D10; linkage) and a
     scope tree.
   - Scope (D26): Except and Finally scopes protect blocks; Handler and
     FinallyCode scopes hold their code.  An edge leaving a scope is
     unwind_to or ret_through_envelope (D8).  invoke, landingpad,
     extractvalue and resume appear only after MSIREH.
   - Module: the procs and the unit's runtime data: the RT0 ModuleInfo with
     its embedded globals, typecells (TypeDesc), type links, revelations,
     exception descriptors, TEXT literals, constant arrays, import binders,
     gc maps, debug info, and the table of runtime procedures it calls
     (RtHook, D34).
   - Runtime ops (D31-D36): nil_check, range_check, subscript_check,
     shape_check, fault, istype, narrow, new, openarray.new, raise and
     method.  They exist so that passes can see their meaning; only
     MSIRRuntime expands them.

   Sections: types; values; debug types (DWARF only); procedures and
   attributes; blocks; EH scopes; the module and its runtime data; Insn
   (Op and accessors); builders by kind; rewriting primitives for passes. *)

INTERFACE MSIR;

IMPORT Target;

(*------------------------------------------------------------------- Types *)

TYPE T <: REFANY;                     (* opaque MSIR type *)

TYPE TypeKind = {
  Void,
  I1, I8, I16, I32, I64,              (* signed integer widths *)
  IWide,                               (* arbitrary-width signed integer (multi-word sets) *)
  W8, W16, W32, W64,                  (* unsigned (word) widths *)
  F32, F64, F128,
  Ptr,                                (* untraced pointer *)
  GcRef,                              (* register-resident traced ref *)
  GcSlot,                             (* memory-resident traced slot *)
  Struct,
  Object,                             (* class type with super, methods, descriptor *)
  OpenArray,                          (* fat pointer: { data, sizes... } *)
  FixedArray,                         (* ARRAY [lo..hi] OF T: contiguous storage *)
  Enum                                (* enumeration type: integer storage + label names for DWARF *)
};

TYPE Field  = RECORD name: TEXT;  type: T;  offset: INTEGER := 0  END;
                                             (* offset: bit offset of the field
                                                within its containing struct,
                                                as computed by M3 record layout.
                                                Used by MSIRToLLVM for DWARF
                                                DW_AT_data_member_location. *)

PROCEDURE TVoid(): T;
PROCEDURE TI1(): T;
PROCEDURE TI(bits: INTEGER): T;       (* bits in {8,16,32,64} or any multiple of 8 > 64 (IWide) *)
PROCEDURE TW(bits: INTEGER): T;
PROCEDURE TF(bits: INTEGER): T;       (* bits in {32,64,128} *)
PROCEDURE TPtr(elt: T): T;
PROCEDURE TGcRef(elt: T): T;
PROCEDURE TGcSlot(elt: T): T;
PROCEDURE TStruct(name: TEXT;  READONLY fields: ARRAY OF Field): T;
PROCEDURE TObject(name: TEXT;
                  super: T;                      (* NIL for root *)
                  READONLY fields:  ARRAY OF Field): T;
(* An object's data fields; methods are reached through `method` (D36),
   which needs no method records on the type. *)
PROCEDURE TOpenArray(rank: INTEGER;  elt: T): T;
PROCEDURE TFixedArray(len: INTEGER;  elt: T): T;
PROCEDURE TEnum(name: TEXT;  bits: INTEGER;  READONLY labels: ARRAY OF TEXT): T;
(* Enum: lowers to iN in LLVM IR; carries label names for DW_TAG_enumeration_type. *)

PROCEDURE EnumLabelCount(t: T): INTEGER;
PROCEDURE EnumLabel(t: T;  i: INTEGER): TEXT;

(* Landing-pad aggregate: { ptr, i32 } — the LLVM EH landingpad result type. *)
PROCEDURE TLandingPad(): T;

PROCEDURE Kind(t: T): TypeKind;
PROCEDURE Equal(a, b: T): BOOLEAN;
PROCEDURE EltType(t: T): T;           (* for Ptr / GcRef / GcSlot *)
PROCEDURE StructName(t: T): TEXT;
PROCEDURE StructFieldCount(t: T): INTEGER;
PROCEDURE StructField(t: T; i: INTEGER): Field;
PROCEDURE BitWidth(t: T): INTEGER;    (* for scalar kinds; -1 otherwise *)
PROCEDURE IsFloat(k: TypeKind): BOOLEAN;  (* F32, F64 or F128 *)

(* Type UID — the CM3 GlobalUID fingerprint, stored here so the lowering
   pass can deduplicate composite DWARF nodes across procs without pulling
   in m3front's Type module.  0 means "no UID assigned". *)
PROCEDURE SetTypeUID(t: T; uid: INTEGER);
PROCEDURE SetTypeAlign(t: T; bits: INTEGER);
PROCEDURE TypeAlign(t: T): INTEGER;
(* M3's alignment of a record or array type, in bits (0 if not stated).
   It can exceed what the type's representation implies: a packed array is
   [N x i8] but M3 may align it to its size. *)
PROCEDURE TypeUID(t: T): INTEGER;

PROCEDURE ObjectName(t: T): TEXT;
PROCEDURE ObjectSuper(t: T): T;       (* NIL if root *)
PROCEDURE ObjectFieldCount(t: T): INTEGER;
PROCEDURE ObjectField(t: T;  i: INTEGER): Field;

PROCEDURE OpenArrayRank(t: T): INTEGER;
PROCEDURE OpenArrayElt(t: T): T;


PROCEDURE FixedArrayLen(t: T): INTEGER;
PROCEDURE FixedArrayElt(t: T): T;
PROCEDURE FixedArrayLo(t: T): INTEGER;       (* lower bound of index type; 0 if zero-based *)
PROCEDURE SetFixedArrayLo(t: T; lo: INTEGER);



(*----------------------------------------------------------------- Values *)

TYPE Value <: REFANY;

TYPE ValueKind = {ConstInt, ConstFloat, ConstNil, ConstProc, ConstTextLit, ConstStruct, ConstAggArray, Param, BlockParam, InsnResult, GlobalRef,
                  StructFieldRef (* GEP into module's @Mod_M3_info struct *) };

PROCEDURE ConstInt(t: T;  v: INTEGER): Value;
PROCEDURE ConstFloat(t: T;  READONLY v: Target.Float): Value;
PROCEDURE GetFloatVal(v: Value;  VAR f: Target.Float);  (* ConstFloat only *)
PROCEDURE ConstBool(v: BOOLEAN): Value;
PROCEDURE ConstNil(t: T): Value;      (* t must be Ptr / GcRef *)
PROCEDURE ConstZero(t: T): Value;    (* zero / NIL / FALSE for scalars; NIL for unsupported types *)
PROCEDURE ConstProcRef(p: Proc): Value;
(* A pointer constant that represents the address of procedure p.
   Type is TPtr(TVoid()); in LLVM this lowers to ptr @procname. *)
PROCEDURE GetConstProc(v: Value): Proc;   (* ConstProc only *)
PROCEDURE RetypeValue(v: Value; t: T): Value;
PROCEDURE StructFieldRef(infoName: TEXT;  byteOffset: INTEGER;  t: T): Value;
(* A ptr/GcSlot value computed as getelementptr i8, ptr @infoName, i64 byteOffset.
   Used for module globals embedded in the @Mod_M3_info struct. *)
PROCEDURE GetStructFieldOffset(v: Value): INTEGER;
(* Return a new Value with the same name/kind as v but type t.
   Used to recast a GEP ptr result as GcSlot when the target field is traced. *)
PROCEDURE ConstStruct(t: T; READONLY fields: ARRAY OF Value): Value;
(* A compile-time constant struct value, used in global constant array initializers.
   t must be a Struct type; fields must all be constant-kind Values. *)
PROCEDURE GetConstStructField(v: Value; i: INTEGER): Value;
PROCEDURE GetConstStructFieldCount(v: Value): INTEGER;

(* Inline constant array literal (e.g. a nested fixed array used as an element
   of an outer CONST array).  Rendered as LLVM '[ <typedval>, ... ]'.  Shares
   the ConstStruct element accessors above. *)
PROCEDURE ConstAggArray(t: T; READONLY elts: ARRAY OF Value): Value;

PROCEDURE ConstTextLit(uid: INTEGER; chars: TEXT; cnt: INTEGER): Value;
(* A TEXT literal value. uid indexes @textlit_<uid> for LLVM lowering;
   chars/cnt are the string content for readable MSIR text output. *)
PROCEDURE GetTextLitUID  (v: Value): INTEGER;
PROCEDURE GetTextLitChars(v: Value): TEXT;
PROCEDURE GetTextLitCnt  (v: Value): INTEGER;

PROCEDURE ValueType(v: Value): T;
PROCEDURE ValueName(v: Value): TEXT;  (* SSA name, e.g. "%5" or "a" *)
PROCEDURE GetValueKind(v: Value): ValueKind;
PROCEDURE GetIntVal(v: Value): INTEGER;            (* for ConstInt only *)

(*------------------------------------------------------- Parameters / modes *)

TYPE ParamMode = {ByValue, Var, Readonly};
TYPE Param      = RECORD name: TEXT;  type: T;  mode: ParamMode  END;

(*------------------------------------------------------------ Debug types *)

(* A variable's type as a debugger should show it.  Several source types
   share one MSIR type (BOOLEAN, CHAR and an enumeration are all integers),
   so the front end describes the source type separately, DWARF-shaped but
   language-neutral, and attaches it to the variable's storage.  Debug
   types are debugger metadata only: they change no semantics, and only
   the LLVM lowering reads them. *)
TYPE DbgT <: REFANY;
TYPE DbgKind = {Basic, Enum, Alias, Pointer, Struct, Array, Proc, FromType};
TYPE DbgEncoding = {Signed, Unsigned, Boolean, Float, Char, Address};
TYPE DbgField = RECORD name: TEXT;  type: DbgT;  bitOffset, bitSize: INTEGER END;

PROCEDURE DbgBasic   (name: TEXT;  enc: DbgEncoding;  bits: INTEGER): DbgT;
PROCEDURE DbgEnum    (name: TEXT;  bits: INTEGER;  READONLY labels: ARRAY OF TEXT): DbgT;
PROCEDURE DbgAlias   (name: TEXT;  base: DbgT): DbgT;    (* a named type, a subrange *)
PROCEDURE DbgPointer (name: TEXT;  target: DbgT): DbgT;  (* target NIL: untyped *)
PROCEDURE DbgStruct  (name: TEXT;  bits: INTEGER): DbgT; (* fields set next *)
PROCEDURE DbgStructSetFields (d: DbgT;  READONLY fields: ARRAY OF DbgField);
PROCEDURE DbgSetBase (d: DbgT;  base: DbgT);  (* a Pointer's target, set late *)
PROCEDURE DbgSetName (d: DbgT;  name: TEXT);  (* a name learned late *)
PROCEDURE DbgSetUID (d: DbgT;  uid: INTEGER);  (* the source type's UID, 0: none *)
PROCEDURE DbgUID (d: DbgT): INTEGER;
PROCEDURE DbgArray   (elt: DbgT;  lo, count: INTEGER): DbgT;
PROCEDURE DbgProc    (name: TEXT): DbgT;                 (* a procedure value *)
PROCEDURE DbgFromType (t: T): DbgT;  (* as the lowering derives it from t *)

PROCEDURE DbgKindOf (d: DbgT): DbgKind;
PROCEDURE DbgName (d: DbgT): TEXT;
PROCEDURE DbgEnc (d: DbgT): DbgEncoding;
PROCEDURE DbgBits (d: DbgT): INTEGER;
PROCEDURE DbgBase (d: DbgT): DbgT;             (* Alias, Pointer, Array *)
PROCEDURE DbgLabelCount (d: DbgT): INTEGER;
PROCEDURE DbgLabel (d: DbgT;  i: INTEGER): TEXT;
PROCEDURE DbgFieldCount (d: DbgT): INTEGER;
PROCEDURE DbgFieldAt (d: DbgT;  i: INTEGER): DbgField;
PROCEDURE DbgArrayLo (d: DbgT): INTEGER;
PROCEDURE DbgArrayCount (d: DbgT): INTEGER;
PROCEDURE DbgMSIRType (d: DbgT): T;            (* FromType *)

(* The variable an alloca holds: its debug type, and its parameter number
   (1-based) when it is a VALUE formal's copy, 0 for a local. *)
PROCEDURE SetAllocaDbg (alloca: Value;  d: DbgT;  argNo: INTEGER := 0;
                        line: INTEGER := 0);
(* line: where the variable is declared (0: unknown, its procedure's) *)
PROCEDURE AllocaDbgType (i: Insn): DbgT;
PROCEDURE AllocaDbgArg (i: Insn): INTEGER;
PROCEDURE AllocaDbgLine (i: Insn): INTEGER;
(* A formal passed by reference (VAR, READONLY): the type of the variable
   the parameter points to. *)
PROCEDURE SetParamDbgType (p: Proc;  i: INTEGER;  d: DbgT;  line: INTEGER := 0);
PROCEDURE ParamDbgType (p: Proc;  i: INTEGER): DbgT;
PROCEDURE ParamDbgLine (p: Proc;  i: INTEGER): INTEGER;
TYPE BlockParam = RECORD name: TEXT;  type: T  END;

(*-------------------------------------------------------------- Procedure *)

TYPE Proc <: REFANY;

PROCEDURE NewProc(name: TEXT;
                  READONLY params: ARRAY OF Param;
                  result: T): Proc;

(* Replace p's parameter list and rebuild its param Values.  Used when a
   bodyless forward stub (built from source formals only) is reused as the real
   definition that also has lambda-lifted capture params. *)
PROCEDURE ProcSetParams(p: Proc;  READONLY params: ARRAY OF Param);

(*------------------------------------------------- procedure attributes *)

TYPE CallingConvention = {M3, C, WinAPI};
TYPE Linkage           = {External, Internal};

(* Default attributes after NewProc: no RAISES check, not noreturn,
   External linkage, M3 calling convention. *)

(* The procedure's RAISES clause, to be enforced (D10): an exception that
   would escape p and is not one of excs becomes a BlockedException runtime
   error.  The front end decides when a check is needed, as for CG (not for
   RAISES ANY, nor when no other exception can escape the body); if it is
   never called, p has no check.  Each lowering generates what its EH scheme
   needs. *)
PROCEDURE ProcSetRaises(p: Proc;  READONLY excs: ARRAY OF ExcDesc);
PROCEDURE ProcChecksRaises(p: Proc): BOOLEAN;
PROCEDURE ProcRaisesCount(p: Proc): INTEGER;
PROCEDURE ProcRaises(p: Proc;  i: INTEGER): ExcDesc;

PROCEDURE ProcSetNoReturn(p: Proc;  v: BOOLEAN);
PROCEDURE ProcIsNoReturn(p: Proc): BOOLEAN;

PROCEDURE ProcSetLinkage(p: Proc;  l: Linkage);
PROCEDURE ProcGetLinkage(p: Proc): Linkage;

PROCEDURE ProcSetCallingConvention(p: Proc;  cc: CallingConvention);
PROCEDURE ProcGetCallingConvention(p: Proc): CallingConvention;

PROCEDURE ProcSetSrcLoc(p: Proc;  file: TEXT;  line: INTEGER);
PROCEDURE ProcSrcFile(p: Proc): TEXT;      (* NIL if no location recorded *)
PROCEDURE ProcSrcLine(p: Proc): INTEGER;   (* 0 if no location recorded *)

(* Set the module-global current source line; all subsequent addInsn calls
   stamp this line on the instruction.  Call before each statement. *)
PROCEDURE SetCurrentSrcLine(line: INTEGER);

(* Return the source line recorded on instruction i; 0 if unknown. *)
PROCEDURE InsnSrcLine(i: Insn): INTEGER;
PROCEDURE InsnUnbarriered(i: Insn): BOOLEAN;
(* TRUE for a load built by BuildUnbarrieredLoad. *)

PROCEDURE HoldsGcRef(t: T): BOOLEAN;
(* TRUE iff t is a gc_ref, or a struct or fixed array with one inside. *)

PROCEDURE InsnCheckCode(i: Insn): INTEGER;
PROCEDURE InsnCheckUnsigned(i: Insn): BOOLEAN;  (* range_check *)
(* nil_check, range_check, subscript_check: the M3CG.RuntimeError code
   reported when the check fails (D32); InsnSrcLine is its line. *)

PROCEDURE InsnBlockMap(i: Insn): GcMapBytes;
PROCEDURE InsnBlockStride(i: Insn): INTEGER;
(* gc.load_block: the gc map of one element, and the bytes between
   elements; see BuildGcLoadBlock. *)

PROCEDURE ProcName(p: Proc): TEXT;
PROCEDURE ProcParamCount(p: Proc): INTEGER;
PROCEDURE ProcParam    (p: Proc;  i: INTEGER): Value;
PROCEDURE ProcParamName(p: Proc;  i: INTEGER): TEXT;
PROCEDURE ProcParamMode(p: Proc;  i: INTEGER): ParamMode;
PROCEDURE ProcResultType(p: Proc): T;
PROCEDURE ProcEntry(p: Proc): Block;               (* the first block *)
PROCEDURE ProcBlockCount(p: Proc): INTEGER;
PROCEDURE ProcBlock(p: Proc;  i: INTEGER): Block;
PROCEDURE ProcAddBlock(p: Proc;  b: Block);

(*------------------------------------------------------------------ Block *)

TYPE Block <: REFANY;

PROCEDURE NewBlock(label: TEXT;
                   READONLY params: ARRAY OF BlockParam): Block;
PROCEDURE BlockLabel(b: Block): TEXT;
PROCEDURE BlockParamCount(b: Block): INTEGER;
PROCEDURE BlockParamValue(b: Block;  i: INTEGER): Value;
PROCEDURE BlockInsnCount(b: Block): INTEGER;
PROCEDURE BlockInsn(b: Block;  i: INTEGER): Insn;
PROCEDURE BlockIsTerminated(b: Block): BOOLEAN;
                                             (* TRUE if last insn is a terminator *)

(* The EH scope b belongs to; a block not given one is in its procedure's
   root scope. *)
PROCEDURE BlockScope(b: Block): Scope;
PROCEDURE BlockSetScope(b: Block;  s: Scope);

(*-------------------------------------------------------------- EH scopes *)

(* D26.  The blocks of a procedure stay a flat list; each belongs to one EH
   scope, and the scopes form a tree rooted at the procedure's scope.
   - An Except scope (TRY/EXCEPT) protects its blocks; it has handlers, each
     an exception (NIL: ELSE) and the entry block of the handler's code,
     which takes the exception's argument as its one parameter if the
     handler uses it.
   - A Finally scope (TRY/FINALLY, LOCK) protects its blocks; it has the
     entry block of its FINALLY code.
   - A Handler or FinallyCode scope holds one handler's or the FINALLY's
     code.  Its parent is its TRY scope's parent (it is not protected by its
     own TRY), and the TRY scope is its owner.  It is entered only by the EH
     mechanism.  FINALLY code finishes with end_finally, which resumes the
     outcome that entered it; leaving it otherwise replaces that outcome.
   Control enters an Except or Finally scope by a branch from its parent to
   its entry block; any other edge between scopes leaves scopes and must be
   unwind_to or ret_through_envelope (D8). *)

TYPE Scope <: REFANY;
TYPE ScopeKind = {Proc, Except, Finally, Handler, FinallyCode};
TYPE Handler = RECORD
  exc:   ExcDesc;   (* NIL: the ELSE handler *)
  entry: Block;
END;

PROCEDURE ProcScope(p: Proc): Scope;                (* the root, kind Proc *)
PROCEDURE NewScope(p: Proc;  kind: ScopeKind;  parent: Scope;
                   owner: Scope := NIL): Scope;
                                             (* not Proc; owner: the TRY scope,
                                                for Handler and FinallyCode *)
PROCEDURE ProcScopeCount(p: Proc): INTEGER;         (* including the root *)
PROCEDURE ProcMayReraise(p: Proc): BOOLEAN;
(* p can re-raise an exception through the module's re-raise hook: it has a
   Finally scope (TRY FINALLY, LOCK), or an Except scope with no ELSE
   handler.  A TRY EXCEPT ELSE never re-raises. *)
PROCEDURE ProcScopeAt(p: Proc;  i: INTEGER): Scope; (* 0 is the root *)

PROCEDURE ScopeKindOf(s: Scope): ScopeKind;
PROCEDURE ScopeParent(s: Scope): Scope;             (* NIL for the root *)
PROCEDURE ScopeOwner(s: Scope): Scope;              (* Handler, FinallyCode *)
PROCEDURE ScopeIndex(s: Scope): INTEGER;            (* its ProcScopeAt index *)
PROCEDURE ScopeSetEntry(s: Scope;  b: Block);       (* the first protected block *)
PROCEDURE ScopeEntry(s: Scope): Block;
PROCEDURE ScopeAddHandler(s: Scope;  exc: ExcDesc;  entry: Block);
PROCEDURE ScopeHandlerCount(s: Scope): INTEGER;
PROCEDURE ScopeHandler(s: Scope;  i: INTEGER): Handler;
PROCEDURE ScopeSetFinally(s: Scope;  entry: Block);
PROCEDURE ScopeFinally(s: Scope): Block;            (* NIL unless Finally *)
PROCEDURE ScopeIsAncestor(a, s: Scope): BOOLEAN;    (* a = s or encloses s *)

(*--------------------------------------------------------------- Module *)

TYPE Module <: REFANY;

PROCEDURE NewModule(name: TEXT): Module;
PROCEDURE ModuleName(m: Module): TEXT;

PROCEDURE ModuleInfoName(m: Module): TEXT;
(* The RT0 ModuleInfo struct symbol: <Name>_I3_info for interfaces, <Name>_M3_info
   for modules — distinct so a MODULE Z EXPORTS Z pair does not collide. *)
PROCEDURE ModuleSrcFile(m: Module): TEXT;             (* RT0.ModuleInfo.file; NIL if unset *)
PROCEDURE ModuleSetSrcFile(m: Module;  f: TEXT);      (* records once (first non-NIL wins) *)
PROCEDURE ModuleSetIsInterface(m: Module;  isInterface: BOOLEAN);
PROCEDURE ModuleIsInterface(m: Module): BOOLEAN;
PROCEDURE SetModuleTarget(m: Module;  triple, datalayout: TEXT);
PROCEDURE ModuleTriple(m: Module): TEXT;
PROCEDURE ModuleDataLayout(m: Module): TEXT;

(* Every runtime (RTHooks) procedure the lowered module calls, bound per
   module (D34): the check fault (ReportFault, D32), type tests (D33), RAISE
   (D35), the allocators (D34), the GC barriers (D30), TYPECASE, the EH
   lowerings' re-raise and ex_frame hooks, and the TEXT literal methods.
   MSIRRuntime.Hooks says which a module needs; the front end binds each to
   its RTHooks procedure, so the names come from the compiler's view of
   RTHooks.  A NIL hook is one the module does not use. *)
TYPE RtHook = {ReportFault, CheckIsType, RaiseEx,
               NewTracedRef, NewUntracedRef, NewTracedObj, NewUntracedObj,
               NewTracedArray, NewUntracedArray,
               CheckLoadTracedRef, CheckStoreTraced, CheckLoadTracedBlock,
               ScanTypecase, ResumeRaiseEx, PushEFrame, PopEFrame,
               TextLitInfo, TextLitGetChar, TextLitGetWideChar,
               TextLitGetChars, TextLitGetWideChars};
PROCEDURE SetModuleRtHook(m: Module;  h: RtHook;  p: Proc);
PROCEDURE ModuleRtHook(m: Module;  h: RtHook): Proc;
PROCEDURE ModuleAddProc(m: Module;  p: Proc);
PROCEDURE ModuleProcCount(m: Module): INTEGER;
PROCEDURE ModuleProc(m: Module;  i: INTEGER): Proc;

(*-------------------------------------------------- exception descriptors *)

(* A per-exception static descriptor: { uid: i64, name: ptr, implicit: i64 }.
   Created by NewExcDesc; emitted as a module-level internal global.
   uid = M3FP.ToInt(fingerprint of exception global name) — 32-bit hash.
   The Value returned by ExcDescValue has ptr type; MSIRRuntime passes it
   as RTHooks.RaiseEx's exception argument when it expands raise (D35). *)
TYPE ExcDesc <: REFANY;

PROCEDURE NewExcDesc(name: TEXT;  uid: INTEGER;  display: TEXT := NIL;
                     argType: T := NIL;  argByRef := FALSE): ExcDesc;
PROCEDURE ExcDescArgType (d: ExcDesc): T;    (* the argument's type; NIL if none *)
(* TRUE if the raise passes the argument's address rather than its value in
   the activation's argument word (the front end's convention). *)
PROCEDURE ExcDescArgByRef (d: ExcDesc): BOOLEAN;
PROCEDURE SetExcDescArg (d: ExcDesc;  argType: T;  argByRef: BOOLEAN);
(* For an argument passed by reference: the typecell of a REF to the
   argument's type, for the heap box a RAISE copies it into (D35).  Set by
   the front end when it first raises the exception in a module. *)
PROCEDURE SetExcDescBoxCell (d: ExcDesc;  cell: Value);
PROCEDURE ExcDescBoxCell (d: ExcDesc): Value;
PROCEDURE ExcDescName (d: ExcDesc): TEXT;    (* LLVM global symbol *)
PROCEDURE ExcDescUID  (d: ExcDesc): INTEGER;
PROCEDURE ExcDescDisplay (d: ExcDesc): TEXT; (* qualified name, e.g. "Main.a"; NIL if none *)
PROCEDURE ExcDescValue(d: ExcDesc): Value;   (* ptr to descriptor *)

PROCEDURE ModuleAddExcDesc  (m: Module;  d: ExcDesc);
PROCEDURE ModuleExcDescCount(m: Module): INTEGER;
PROCEDURE ModuleExcDesc     (m: Module;  i: INTEGER): ExcDesc;

(*----------------------------------------------- type descriptors (type_cells) *)

(* A TypeCell or ObjectTypeCell static global, emitted into the .ll and
   linked into the RT0.ModuleInfo.type_cells list for RTLinker.FixTypes.
   For REF types: a base TypeCell (96 bytes on 64-bit).
   For OBJECT types: an ObjectTypeCell (152 bytes) with defaultMethods set
   so AllocateTracedObj -> InitObj stores the vtable without needing linkProc. *)
TYPE TypeDesc <: REFANY;

(* uid = Type.GlobalUID = M3FP.ToInt(fp), a 32-bit hash of the 64-bit fingerprint.
   kind: ORD(M3RT.TypeKind) — Ref=1, Obj=2, Array=3.
   dataSize and dataAlignment in bytes (TC_dataSize, TC_dataAlignment).
   methods: names of vtable function symbols, one per slot (OBJECT only).
   methodBytes: total vtable byte size; -1 means compute from methods array. *)
PROCEDURE NewTypeDesc(name: TEXT; uid: INTEGER; isTraced: BOOLEAN;
                      kind: INTEGER; dataSize: INTEGER;
                      dataAlignment: INTEGER;
                      parentUID: INTEGER := 0;
                      READONLY methods: ARRAY OF TEXT := ARRAY OF TEXT{};
                      methodBytes: INTEGER := -1): TypeDesc;
PROCEDURE TypeDescName        (d: TypeDesc): TEXT;
PROCEDURE TypeDescValue       (d: TypeDesc): Value;   (* ptr to the TypeCell global *)
PROCEDURE TypeDescUID         (d: TypeDesc): INTEGER;
(* Set / get the 64-bit Fingerprint2.T bytes (little-endian byte[0..7]).
   SetTypeDescFP must be called after NewTypeDesc with the actual fingerprint
   so that TC_fp in the emitted TypeCell matches the real backend. *)
PROCEDURE SetTypeDescFP(d: TypeDesc; READONLY b: ARRAY OF [0..255]);
PROCEDURE TypeDescFPByte(d: TypeDesc; i: INTEGER): INTEGER;
PROCEDURE TypeDescTraced      (d: TypeDesc): BOOLEAN;
PROCEDURE TypeDescKind        (d: TypeDesc): INTEGER; (* ORD(M3RT.TypeKind) *)
PROCEDURE TypeDescSize        (d: TypeDesc): INTEGER; (* dataSize in bytes *)
PROCEDURE TypeDescAlign       (d: TypeDesc): INTEGER; (* dataAlignment in bytes *)
PROCEDURE TypeDescParentUID   (d: TypeDesc): INTEGER; (* OBJ: parent fingerprint *)
PROCEDURE SetTypeDescUserName (d: TypeDesc; name: TEXT); (* user-visible name for TC_name *)
PROCEDURE SetTypeDescInitProc (d: TypeDesc; procName: TEXT); (* OBJ: field-default init proc *)
PROCEDURE TypeDescInitProcName(d: TypeDesc): TEXT;           (* NIL if none *)
PROCEDURE TypeDescUserName    (d: TypeDesc): TEXT;       (* user-visible name or NIL *)
PROCEDURE TypeDescMethodBytes (d: TypeDesc): INTEGER; (* OBJ: vtable byte size *)
PROCEDURE TypeDescMethodCount (d: TypeDesc): INTEGER;
PROCEDURE TypeDescMethod      (d: TypeDesc;  i: INTEGER): TEXT;
(* C-GenLinkProc-style method initialisers: one per method (own or override)
   with a proc default.  When useLinkProc is set the emitter produces
   defaultMethods=NIL + a linkProc that stores each proc at declBase+offBytes
   (declBase const if known, else runtime OTC_methodOffset of declUID's cell). *)
PROCEDURE AddTypeDescMethodInit(d: TypeDesc;  proc: TEXT;  offBytes: INTEGER;
                                declBaseKnown: BOOLEAN;  declBase: INTEGER;
                                declUID: INTEGER);
PROCEDURE TypeDescMethodInitCount   (d: TypeDesc): INTEGER;
PROCEDURE TypeDescMethodInitProc    (d: TypeDesc;  i: INTEGER): TEXT;
PROCEDURE TypeDescMethodInitOff     (d: TypeDesc;  i: INTEGER): INTEGER;
PROCEDURE TypeDescMethodInitBaseKnown(d: TypeDesc;  i: INTEGER): BOOLEAN;
PROCEDURE TypeDescMethodInitBase    (d: TypeDesc;  i: INTEGER): INTEGER;
PROCEDURE TypeDescMethodInitDeclUID (d: TypeDesc;  i: INTEGER): INTEGER;
PROCEDURE SetTypeDescUseLinkProc(d: TypeDesc; b: BOOLEAN);
PROCEDURE TypeDescUseLinkProc   (d: TypeDesc): BOOLEAN;
(* Array TypeCell extensions (kind = ORD(M3RT.TypeKind.Array)). *)
PROCEDURE TypeDescSetArrayInfo(d: TypeDesc; nDimensions, elementSize: INTEGER);
PROCEDURE TypeDescNDimensions (d: TypeDesc): INTEGER;
PROCEDURE TypeDescElementSize (d: TypeDesc): INTEGER;

(* Runtime type maps, built by the front end (TipeMap / TipeDesc) exactly as
   for the C backend, attached here as raw bytes and emitted verbatim into
   the TypeCell.  NIL/empty => the corresponding TypeCell field is NIL.
   - gc_map (TC_gc_map): refs-only RTMapOp program the collector interprets
     to trace/fix up references inside the referent during a moving GC.
   - type_map (TC_type_map): full RTMapOp program (every field, not just
     refs) interpreted by RTTypeMap.WalkRef — the pickle ver1 machinery.
   - type_desc (TC_type_desc): RTTipe type description (op count + preorder
     type AST) — the pickle ver2 / ConvertPacking machinery. *)
TYPE GcMapBytes = REF ARRAY OF [0..255];
(* Front-end TipeMap bytes covering the module's traced globals (RT0.ModuleInfo
   gc_map/var_map).  Offsets are module-segment-relative (t.offset). *)
PROCEDURE SetModuleGcMap (m: Module;  bytes: GcMapBytes);
PROCEDURE ModuleGcMap    (m: Module): GcMapBytes;
PROCEDURE SetModuleGcFlags (m: Module;  flags: INTEGER);
PROCEDURE ModuleGcFlags    (m: Module): INTEGER;
(* RT0.ModuleInfo.gc_flags: which barriers the module's code carries, set by
   the front end as for the C payload (Module.m3): GC_gen (1) for the write
   barriers, GC_inc (2) for the read barriers.  Default 3 (both). *)
PROCEDURE SetTypeDescGcMap    (d: TypeDesc;  bytes: GcMapBytes);
PROCEDURE TypeDescGcMap       (d: TypeDesc): GcMapBytes;
PROCEDURE SetTypeDescTypeMap  (d: TypeDesc;  bytes: GcMapBytes);
PROCEDURE TypeDescTypeMap     (d: TypeDesc): GcMapBytes;
PROCEDURE SetTypeDescPickleDesc (d: TypeDesc;  bytes: GcMapBytes);
PROCEDURE TypeDescPickleDesc    (d: TypeDesc): GcMapBytes;
(* BRANDED types: the brand's characters (target byte order, no terminating
   NUL) and the size of one character.  Lowered to the RT0 brand record
   (an INTEGER byte count, then the characters and a NUL) for TC_brand. *)
PROCEDURE SetTypeDescBrand (d: TypeDesc;  chars: GcMapBytes;  charBytes: INTEGER);
PROCEDURE TypeDescBrand    (d: TypeDesc;  VAR charBytes: INTEGER): GcMapBytes;

PROCEDURE ModuleAddTypeDesc  (m: Module;  d: TypeDesc);

(* Debug types the module's debug info must keep though no variable uses
   them (DICompileUnit retainedTypes): an object type with a typecell here,
   so a debugger can show an object under its dynamic type. *)
PROCEDURE ModuleRetainDbgType      (m: Module;  d: DbgT);

(* A global variable's debug info: its source name (without the module,
   which is its DWARF scope), its type and declaration line, and where it
   lives: byteOff bytes into the LLVM global sym. *)
PROCEDURE ModuleAddDbgGlobal (m: Module;  name: TEXT;  d: DbgT;
                              line: INTEGER;  sym: TEXT;  byteOff: INTEGER);
PROCEDURE ModuleDbgGlobalCount (m: Module): INTEGER;
PROCEDURE ModuleDbgGlobal (m: Module;  i: INTEGER;
                           VAR name: TEXT;  VAR d: DbgT;  VAR line: INTEGER;
                           VAR sym: TEXT;  VAR byteOff: INTEGER);
PROCEDURE ModuleRetainedDbgTypeCount(m: Module): INTEGER;
PROCEDURE ModuleRetainedDbgType    (m: Module;  i: INTEGER): DbgT;
PROCEDURE ModuleTypeDescCount(m: Module): INTEGER;
PROCEDURE ModuleTypeDesc     (m: Module;  i: INTEGER): TypeDesc;

(* Create a ptr-typed value that refers to the TypeCell global named 'name'.
   Used in proc bodies before the TypeCell global is registered (forward ref). *)
PROCEDURE TypeCellRef (name: TEXT): Value;

(*----------------------------------------------- type cell references (cell_ptrs) *)

(* Analog of RT0.TypeLink / CG cell_ptrs.  Each entry is a { ptr defn, i64 uid }
   global added to MI_type_cell_ptrs.  RTLinker.ResolveTypeLinks fills defn with
   the actual TypeCell pointer at module-init time.
   uid = Type.GlobalUID (32-bit hash of 64-bit FP); stored as i64 in LLVM IR.
   Loading defn at runtime (after RTLinker) gives the TypeCell pointer. *)
TYPE TypeLink <: REFANY;

PROCEDURE NewTypeLink (name: TEXT;  uid: INTEGER): TypeLink;
PROCEDURE TypeLinkName (tl: TypeLink): TEXT;
PROCEDURE TypeLinkUID  (tl: TypeLink): INTEGER;

PROCEDURE ModuleAddTypeLink  (m: Module;  tl: TypeLink);
PROCEDURE ModuleTypeLinkCount(m: Module): INTEGER;
PROCEDURE ModuleTypeLink     (m: Module;  i: INTEGER): TypeLink;

(* REVEAL records — analog of RT0.Revelation / CG full_rev array.
   Each entry records (lhs_id = opaque UID, rhs_id = revealed UID) so that
   RTLinker.NoteFullRevelation can map opaque TypeLinks to revealed TypeCells. *)
TYPE Revelation <: REFANY;

PROCEDURE NewRevelation (lhsUID, rhsUID: INTEGER): Revelation;
PROCEDURE RevelationLhsUID (r: Revelation): INTEGER;
PROCEDURE RevelationRhsUID (r: Revelation): INTEGER;

PROCEDURE ModuleAddRevelation  (m: Module;  r: Revelation);
PROCEDURE ModuleRevelationCount(m: Module): INTEGER;
PROCEDURE ModuleRevelation     (m: Module;  i: INTEGER): Revelation;

(* TEXT literals, added by the front end as it compiles them.  chars holds the
   characters (for a wide literal, cnt < 0 and chars holds each WIDECHAR as
   little-endian bytes); cnt is the length.  ModuleAddTextLit returns the
   literal's uid, its index here; an equal literal added again gets the same
   uid. *)
PROCEDURE ModuleAddTextLit  (m: Module;  chars: TEXT;  cnt: INTEGER): INTEGER;
PROCEDURE ModuleTextLitCount(m: Module): INTEGER;
PROCEDURE ModuleTextLitChars(m: Module;  uid: INTEGER): TEXT;
PROCEDURE ModuleTextLitCnt  (m: Module;  uid: INTEGER): INTEGER;




(*----------------------------------------------- import binders *)

(* Register an imported module's binder function name (e.g. "Fmt_M3").
   The emitter builds the RT0.ImportInfo chain from these. *)
PROCEDURE ModuleAddImportBinder  (m: Module;  binder: TEXT);
PROCEDURE ModuleImportBinderCount(m: Module): INTEGER;
PROCEDURE ModuleImportBinder     (m: Module;  i: INTEGER): TEXT;

(*-------------------------------------------------------- module globals *)

TYPE Global <: REFANY;

(* A module-level global. If isTraced, the slot must be registered as a
   GC root in the module descriptor (D14) and stores go through
   gc.store; loads go through gc.load. Otherwise plain load/store. *)
PROCEDURE NewGlobal(name: TEXT;  type: T;  isTraced: BOOLEAN;
                    isExternal: BOOLEAN := FALSE): Global;
PROCEDURE GlobalName       (g: Global): TEXT;
PROCEDURE GlobalType       (g: Global): T;
PROCEDURE GlobalSetWeak    (g: Global);          (* mark weak linkage *)
PROCEDURE GlobalIsWeak     (g: Global): BOOLEAN;
PROCEDURE GlobalSetPrivate (g: Global);          (* mark internal linkage *)
PROCEDURE GlobalIsPrivate  (g: Global): BOOLEAN;
PROCEDURE GlobalIsTraced   (g: Global): BOOLEAN;
PROCEDURE GlobalIsExternal    (g: Global): BOOLEAN;
PROCEDURE GlobalByteOffset    (g: Global): INTEGER; (* -1 for external/standalone *)

PROCEDURE GlobalSetStructField(g: Global;  byteOff: INTEGER;  ref: Value);
PROCEDURE GlobalValue(g: Global): Value;
PROCEDURE GlobalSetBackingBytes(g: Global;  n: INTEGER); (* indirect: set [n x i8] backing size *)
PROCEDURE GlobalBackingBytes   (g: Global): INTEGER;     (* 0 if not a backing-storage global *)
PROCEDURE GlobalAddrValue      (g: Global): Value;       (* @Name as a ptr constant *)
                                             (* an addressable Value:
                                                gc_slot type if traced,
                                                ptr type otherwise *)

PROCEDURE ModuleAddGlobal(m: Module;  g: Global);

(* Record a compile-time-constant initial value for an embedded user global at
   byteOff within @Mod_M3_info.  Emitted by an early global constructor (before
   any module body runs) because the user region is otherwise a zero blob and
   some globals (RTType's InfoMap tables) are read during RTLinker startup. *)
PROCEDURE ModuleAddGlobalInit(m: Module;  byteOff: INTEGER;  val: Value);
PROCEDURE ModuleGlobalInitCount(m: Module): INTEGER;
PROCEDURE ModuleGlobalInitOffset(m: Module;  i: INTEGER): INTEGER;
PROCEDURE ModuleGlobalInitValue(m: Module;  i: INTEGER): Value;

(* Number of fields in RT0.ModuleInfo.  Its byte size is M3RT.MI_SIZE;
   MSIRToLLVM checks its field table against M3RT's MI_* offsets (D24). *)
CONST MI_nFields = 13;

(* Record that a user global extends to endByteOff in the module struct (used
   when placing globals at the front-end's canonical Variable offset rather than
   MSIR's dense packing, so cross-module importers agree). *)
PROCEDURE ModuleNoteGlobal(m: Module;  endByteOff: INTEGER);
PROCEDURE ModuleGlobalStructSize(m: Module): INTEGER;

(* Mark m as not describing all of its unit's data: some part the unit
   lays out (for other units to read) has no MSIR form yet.  A backend that
   must reproduce the unit's data exactly (MSIRToM3CG) then declines it.
   The first reason is kept; NIL from ModuleIncomplete means complete. *)
PROCEDURE ModuleSetIncomplete(m: Module;  reason: TEXT);
PROCEDURE ModuleIncomplete(m: Module): TEXT;
(* Total byte size of the module struct = MI_SIZE + embedded user globals. *)
PROCEDURE ModuleGlobalCount(m: Module): INTEGER;
PROCEDURE ModuleGlobal(m: Module;  i: INTEGER): Global;

(*---------------------------------------------- constant array globals *)

(* A private constant [N x eltType] global, used to materialise
   CONST ARRAY OF T values for subscript LValue operations.
   Emitted as: @constarray_K = private constant [N x eltType] [...] *)
TYPE ConstArray <: REFANY;

PROCEDURE NewConstArray(name: TEXT; eltType: T;
                        READONLY elts: ARRAY OF Value): ConstArray;
PROCEDURE ConstArrayName    (ca: ConstArray): TEXT;
PROCEDURE ConstArrayEltType (ca: ConstArray): T;
PROCEDURE ConstArrayEltCount(ca: ConstArray): INTEGER;
PROCEDURE ConstArrayElt     (ca: ConstArray; i: INTEGER): Value;
PROCEDURE ConstArrayValue   (ca: ConstArray): Value;  (* ptr to the global *)

PROCEDURE ModuleAddConstArray  (m: Module; ca: ConstArray);
PROCEDURE ModuleConstArrayCount(m: Module): INTEGER;
PROCEDURE ModuleConstArray     (m: Module; i: INTEGER): ConstArray;

(*------------------------------------------------------------------- Insn *)

TYPE Insn <: REFANY;

TYPE Op = {
  (* memory *)
  Alloca,    (* static-count stack alloc: alloca T, i64 N    *)
  AllocaDyn, (* dynamic-count stack alloc: alloca i8, i64 %n — byteCount is operand 0 *)
  Load, Store, GcLoad, GcStore, GcDirty, GcLoadBlock,
  (* integer arithmetic *)
  IAdd, ISub, IMul, IDiv, IMod,
  IUDiv, IURem,                          (* unsigned divide / remainder *)
  IAnd, IOr, IXor, IShl, ILShr, IAShr,  (* bitwise / shift *)
  IRotL, IRotR,                          (* rotate left/right → llvm.fshl/fshr *)
  Select,                                (* conditional select: select i1, T, T → T *)
  (* float arithmetic *)
  FAdd, FSub, FMul, FDiv, FNeg,
  (* compare *)
  ICmp, FCmp,
  (* control *)
  Br, CondBr, Ret, Unreachable,
  UnwindTo, RetThroughEnvelope,        (* cross-envelope exits *)
  EndFinally,                          (* FINALLY code done: resume (D26) *)
  HandlerActivation,                   (* the handled exception's activation *)
  (* call / alloc *)
  Call, Invoke, New,
  (* M3 object/RTTI intrinsics *)
  Method, Narrow, Istype, Typecase,
  (* exception *)
  Raise,
  LandingPad,   (* LLVM landingpad instruction; cleanup or catch _ZTI6_M3Exc *)
  ExtractValue, (* extract field from a struct aggregate by index *)
  Resume,       (* LLVM resume — re-throw after landingpad (terminator) *)
  (* open arrays *)
  OpenArraySize, OpenArrayElemAddr,
  OpenArrayNew,
  (* fixed arrays *)
  ArrayElemAddr,
  (* runtime checks *)
  SubscriptCheck, NilCheck, RangeCheck,
  Fault,        (* a runtime error known to be inevitable (terminator) *)
  ShapeCheck,   (* two array dimension counts must be equal *)
  (* type conversion / casting *)
  Convert,
  SIToFP,     (* signed integer → float *)
  FPToSI,     (* float → signed integer *)
  FPExt,      (* float extension (narrower → wider) *)
  FPTrunc,    (* float truncation (wider → narrower) *)
  ZExt,       (* zero-extend integer *)
  SExt,       (* sign-extend integer *)
  Trunc,      (* integer truncation *)
  FPFloor,    (* floor(x): round toward -inf; lowers to llvm.floor.* *)
  FPCeil,     (* ceil(x):  round toward +inf; lowers to llvm.ceil.* *)
  FPRound,    (* round-half-to-even (FloatMode.RoundDefault=NearestElseEven); lowers to llvm.roundeven.* *)
  FPAbs,      (* fabs(x): floating-point absolute value; lowers to llvm.fabs.* *)
  (* sets (D19), on multi-word sets typed iN *)
  SetUnion, SetIntersect, SetDifference, SetMember,
  SetSymDifference, SetSingleton, SetRange,
  (* indirect dispatch *)
  PtrAdd,         (* getelementptr ptr, ptr %base, i64 N  — vtable slot address *)
  GepByte,        (* getelementptr i8, ptr %base, i64 %offset — dynamic byte-offset ptr arith *)
  CallIndirect,   (* call via function-pointer value (no static Proc target) *)
  InvokeIndirect, (* invoke via function-pointer value, with normal/unwind targets *)
  (* atomic memory operations *)
  AtomicFence,    (* fence <ordering> *)
  AtomicLoad,     (* %v = load atomic T, ptr %p <ordering>, align N; targetType = elem type *)
  AtomicStore,    (* store atomic T %v, ptr %p <ordering>, align N; no result *)
  AtomicRMW,      (* %old = atomicrmw <op> ptr %p, T %v <ordering>; targetType = elem type *)
  AtomicCmpXchg   (* %ok = cmpxchg expanded (load exp, cmpxchg, store back); result = i1 *)
};

TYPE CmpPred = {
  Eq, Ne,
  Slt, Sle, Sgt, Sge,
  Ult, Ule, Ugt, Uge
};

TYPE FCmpPred = {
  OEq, ONe,   (* ordered: both not NaN *)
  OLt, OLe, OGt, OGe,
  ORd,        (* ordered: neither NaN *)
  UNe,        (* unordered or not equal *)
  ULt, ULe, UGt, UGe
};

TYPE MemOrder = { Relaxed, Release, Acquire, AcqRel, SeqCst };

TYPE AtomicRMWOp = { Xchg, Add, Sub, And, Or, Xor };

PROCEDURE InsnOp(i: Insn): Op;
PROCEDURE InsnResult(i: Insn): Value;        (* NIL if Void-typed *)
PROCEDURE ValueDefInsn(v: Value): Insn;      (* defining insn for InsnResult
   values (survives RetypeValue); NIL for params/consts/globals.  Used by the
   verifier's read-barrier audit to walk address provenance. *)
PROCEDURE InsnOperandCount(i: Insn): INTEGER;
PROCEDURE InsnOperand(i: Insn;  k: INTEGER): Value;

(* Some opcodes carry extra data that isn't an SSA operand: *)
PROCEDURE InsnCmpPred(i: Insn): CmpPred;     (* ICmp only *)
PROCEDURE InsnFCmpPred(i: Insn): FCmpPred;   (* FCmp only *)
PROCEDURE InsnBrTarget(i: Insn;  k: INTEGER): Block;
                                             (* k=0 for Br, k∈{0,1} for CondBr *)
PROCEDURE InsnBrArgCount(i: Insn;  k: INTEGER): INTEGER;
PROCEDURE InsnBrArg(i: Insn;  k, j: INTEGER): Value;
PROCEDURE InsnCallee(i: Insn): Proc;         (* Call only *)
PROCEDURE InsnTargetType(i: Insn): T;        (* Alloca, New, Narrow, Istype, AtomicLoad/RMW/CmpXchg *)
PROCEDURE InsnSelector(i: Insn): TEXT;       (* method: the method's name *)
PROCEDURE InsnExtractIdx(i: Insn): INTEGER;  (* ExtractValue: field index *)
PROCEDURE InsnIsCleanup(i: Insn): BOOLEAN;   (* LandingPad: TRUE=cleanup, FALSE=catch *)
PROCEDURE InsnMemOrder(i: Insn): MemOrder;   (* Atomic ops *)
PROCEDURE InsnMemOrder2(i: Insn): MemOrder;  (* AtomicCmpXchg failure ordering *)
PROCEDURE InsnAtomicOp(i: Insn): AtomicRMWOp; (* AtomicRMW op code *)

(*---------------------------------------------------------------- Builders *)

PROCEDURE BuildIAdd(b: Block;  name: TEXT;  x, y: Value): Value;
PROCEDURE BuildISub(b: Block;  name: TEXT;  x, y: Value): Value;
PROCEDURE BuildIMul(b: Block;  name: TEXT;  x, y: Value): Value;
PROCEDURE BuildIDiv(b: Block;  name: TEXT;  x, y: Value): Value;
PROCEDURE BuildIMod(b: Block;  name: TEXT;  x, y: Value): Value;
PROCEDURE BuildIAnd(b: Block;  name: TEXT;  x, y: Value): Value;
PROCEDURE BuildIOr (b: Block;  name: TEXT;  x, y: Value): Value;
PROCEDURE BuildIXor(b: Block;  name: TEXT;  x, y: Value): Value;
PROCEDURE BuildIShl(b: Block;  name: TEXT;  x, y: Value): Value;
PROCEDURE BuildILShr(b: Block; name: TEXT;  x, y: Value): Value;
PROCEDURE BuildIAShr(b: Block; name: TEXT;  x, y: Value): Value;
PROCEDURE BuildIUDiv(b: Block; name: TEXT;  x, y: Value): Value;
PROCEDURE BuildIURem(b: Block; name: TEXT;  x, y: Value): Value;
PROCEDURE BuildIRotL(b: Block; name: TEXT;  x, y: Value): Value;
PROCEDURE BuildIRotR(b: Block; name: TEXT;  x, y: Value): Value;
PROCEDURE BuildSelect(b: Block; name: TEXT;  cond, ifTrue, ifFalse: Value): Value;
PROCEDURE BuildICmp(b: Block;  name: TEXT;  pred: CmpPred;  x, y: Value): Value;

PROCEDURE BuildFAdd(b: Block;  name: TEXT;  x, y: Value): Value;
PROCEDURE BuildFSub(b: Block;  name: TEXT;  x, y: Value): Value;
PROCEDURE BuildFMul(b: Block;  name: TEXT;  x, y: Value): Value;
PROCEDURE BuildFDiv(b: Block;  name: TEXT;  x, y: Value): Value;
PROCEDURE BuildFNeg(b: Block;  name: TEXT;  x: Value): Value;
PROCEDURE BuildFCmp(b: Block;  name: TEXT;  pred: FCmpPred;  x, y: Value): Value;

PROCEDURE BuildAtomicFence(b: Block; order: MemOrder);
PROCEDURE BuildAtomicLoad(b: Block; name: TEXT; elemType: T; ptr: Value; order: MemOrder): Value;
PROCEDURE BuildAtomicStore(b: Block; value: Value; ptr: Value; order: MemOrder;
                            container: Value := NIL);
PROCEDURE BuildAtomicRMW(b: Block; name: TEXT; op: AtomicRMWOp; ptr: Value;
                          val: Value; order: MemOrder;
                          container: Value := NIL): Value;
PROCEDURE BuildAtomicCmpXchg(b: Block; name: TEXT; elemType: T; varPtr: Value;
                              expectedPtr: Value; desired: Value;
                              succOrder: MemOrder; failOrder: MemOrder;
                              container: Value := NIL): Value;

(* Type-converting casts — result type is supplied explicitly. *)
PROCEDURE BuildSIToFP (b: Block;  name: TEXT;  x: Value;  dstType: T): Value;
PROCEDURE BuildFPToSI (b: Block;  name: TEXT;  x: Value;  dstType: T): Value;
PROCEDURE BuildFPExt  (b: Block;  name: TEXT;  x: Value;  dstType: T): Value;
PROCEDURE BuildFPTrunc(b: Block;  name: TEXT;  x: Value;  dstType: T): Value;
PROCEDURE BuildZExt       (b: Block;  name: TEXT;  x: Value;  dstType: T): Value;
PROCEDURE BuildSExt       (b: Block;  name: TEXT;  x: Value;  dstType: T): Value;
PROCEDURE BuildTrunc      (b: Block;  name: TEXT;  x: Value;  dstType: T): Value;
(* Float rounding — result type = input type; lower to llvm.floor/ceil/roundeven *)
PROCEDURE BuildFPFloor    (b: Block;  name: TEXT;  x: Value): Value;
PROCEDURE BuildFPCeil     (b: Block;  name: TEXT;  x: Value): Value;
PROCEDURE BuildFPRound    (b: Block;  name: TEXT;  x: Value): Value;
PROCEDURE BuildFPAbs      (b: Block;  name: TEXT;  x: Value): Value;

PROCEDURE BuildLoad (b: Block;  name: TEXT;  type: T;  addr: Value): Value;
PROCEDURE BuildUnbarrieredLoad (b: Block;  name: TEXT;  type: T;  addr: Value)
  : Value;
(* A load of the traced reference of type `type' (a gc_ref) that the front
   end decided needs no read barrier (D30: Expr.LoadNeedsBarrier): a plain
   load that records the decision, for AuditReadBarriers and the printer.
   Where the front end decides a barrier is needed, it builds BuildGcLoad. *)
PROCEDURE BuildStore(b: Block;  value: Value;  addr: Value);

PROCEDURE BuildBr(b: Block;  target: Block;
                  READONLY args: ARRAY OF Value);
PROCEDURE BuildCondBr(b: Block;  cond: Value;
                      ifTrue: Block;   READONLY trueArgs:  ARRAY OF Value;
                      ifFalse: Block;  READONLY falseArgs: ARRAY OF Value);

PROCEDURE BuildRet(b: Block;  value: Value);  (* pass NIL for void return *)
PROCEDURE BuildUnreachable(b: Block);

PROCEDURE BuildCall(b: Block;  name: TEXT;  callee: Proc;
                    READONLY args: ARRAY OF Value): Value;

(*-------------------------------------------------- Object / RTTI builders *)

PROCEDURE BuildAlloca(b: Block;  name: TEXT;  type: T): Value;

(* Dynamic stack alloc: alloca i8, i64 byteCount.  Returns ptr void.
   Used for runtime-sized copies of open-array VALUE formals. *)
PROCEDURE BuildAllocaDyn(b: Block;  name: TEXT;  byteCount: Value): Value;

                                             (* result: ptr type *)
(* Change an existing alloca's element count (for frame-size fixup).
   count >= 1; when count > 1 emits "alloca T, i64 count". *)
PROCEDURE AllocaSetCount(v: Value;  count: INTEGER);

PROCEDURE BuildGcLoad(b: Block;  name: TEXT;  slot: Value): Value;
                                             (* slot must have type gc_slot T *)
(* Store a traced reference into a gc_slot.
   container: the heap object that owns the slot; pass NIL for module globals
   (which are GC roots and don't need a write barrier).  When non-NIL, the
   lowering emits the CM3 dirty-bit check and a conditional call to
   RTHooks__CheckStoreTraced before the actual store. *)
(* Write-barrier ONLY (no store): mark `container` (a heap object) dirty so
   the incremental/generational collector re-scans it.  Used before AGGREGATE
   copies into heap objects (memcpy/array stores), which cannot go through
   BuildGcStore's single-slot barrier+store. *)
PROCEDURE BuildGcDirty(b: Block;  container: Value);

(* Read barrier on a block copy (D30, #1388): a copy of a record or array
   from global, heap or indirect storage has just stored traced references
   at dst (a ptr); give each of them the read barrier, as gc.load does for
   one.  The front end decides where it is needed (Expr.CopyNeedsBarrier)
   and places it after the copy.  'map' is the garbage collector's map of
   one element (TipeMap, refs only), and the block holds 'n' elements
   (an integer Value; NIL means 1), 'stride' bytes apart.  The lowerings
   call RTHooks.CheckLoadTracedBlock (dst, map, n, stride). *)
PROCEDURE BuildGcLoadBlock(b: Block;  dst: Value;  n: Value;
                           map: GcMapBytes;  stride: INTEGER);

PROCEDURE BuildGcStore(b: Block;  slot: Value;  value: Value;
                        container: Value := NIL);


(* `array.elem_addr arr, idx` — pointer arithmetic on a fixed array.
   arr must be ptr FixedArray (i.e. an alloca or by-ref).
   Result: ptr (element type). *)
PROCEDURE BuildArrayElemAddr(b: Block;  name: TEXT;
                             arr: Value;  idx: Value): Value;

(* Allocation (D34).  `new cell` allocates a reference or object of the
   type whose typecell is cell, by kind; the result has type t.  The
   lowerings call the module's allocator hook (an RTHooks.New proc); the typecell's
   initProc applies field defaults, as in the C backend.  uid is the type's
   GlobalUID, for reading the IR. *)
TYPE AllocKind = {TracedRef, UntracedRef, TracedObj, UntracedObj,
                  TracedArray, UntracedArray};
PROCEDURE BuildNew(b: Block;  name: TEXT;  cell: Value;  kind: AllocKind;
                   uid: INTEGER;  t: T): Value;
PROCEDURE InsnAllocKind(i: Insn): AllocKind;

(* `method obj, +off` (D36) is the procedure in the method suite of the
   object obj at byte offset off: the method-suite pointer is obj's first
   word.  With a holder typecell (`method obj, cell, +off`), the suite
   offset is cell's OTC_methodOffset plus off, for a method declared under
   an opaque supertype, whose offset is known only at run time.  The result
   is a ptr; the call is an ordinary call_indirect with obj first.  selector
   names the method, for reading the IR; uid is the GlobalUID of obj's
   static type, for analyses (the possible targets are its subtypes'
   overrides).  MSIRRuntime expands it into the
   loads; a pass that knows obj's exact type can replace it by the
   procedure (devirtualisation). *)
PROCEDURE BuildMethod(b: Block;  name: TEXT;  obj, holder: Value;
                      off: INTEGER;  selector: TEXT;  uid: INTEGER): Value;
PROCEDURE InsnMethodOffset(i: Insn): INTEGER;

(* Type tests (D33).  `istype v, cell` is TRUE if the reference v is of
   the type whose typecell is cell; `narrow v, cell` is v at type t, and
   faults with code (NarrowFailed) at line unless v is of the type.  NIL is of every reference type.
   Exact: a REF type, tested by its typecode (a tagged v has REFANY's).
   Subtype: an object or opaque type, tested by the typecode first, then by
   RTHooks.CheckIsType (the module's type-test hook).  Null: the type NULL,
   only NIL; cell is NIL.  uid is the type's Type.GlobalUID, for reading
   the IR.  MSIRRuntime expands both before lowering. *)
TYPE TypeTest = {Exact, Subtype, Null};
PROCEDURE BuildIstype(b: Block;  name: TEXT;  v, cell: Value;
                      test: TypeTest;  uid: INTEGER): Value;
PROCEDURE BuildNarrow(b: Block;  name: TEXT;  v, cell: Value;
                      test: TypeTest;  uid, code, line: INTEGER;  t: T): Value;
PROCEDURE InsnTypeTest(i: Insn): TypeTest;
PROCEDURE InsnTypeUid(i: Insn): INTEGER;

(* TYPECASE is a structured terminator op.  Each clause names a target type
   (or ELSE) and a parameterized block.  ELSE is mandatory at MSIR level.
   uid  = Type.GlobalUID (32-bit hash of the 64-bit FP) used to build the
          RTHooks__ScanTypecase type table; 0 for the ELSE terminator.
   targetType = MSIR type of the narrowed value (NIL if isElse). *)
TYPE TypecaseClause = RECORD
  isElse:     BOOLEAN;
  uid:        INTEGER;  (* M3 type UID; 0 for ELSE *)
  targetType: T;        (* NIL if isElse *)
  block:      Block;
END;

PROCEDURE BuildTypecase(b: Block;  value: Value;
                        clauses: REF ARRAY OF TypecaseClause);

PROCEDURE InsnTypecaseClauseCount(i: Insn): INTEGER;
PROCEDURE InsnTypecaseClause(i: Insn;  k: INTEGER): TypecaseClause;

(*--------------------------------------------------- EH builders / control *)

(* `invoke` is `call` that can unwind.  normalBlock is the block entered
   on normal return; unwindBlock is the landingpad block entered on exception.
   Both blocks must belong to the same proc as b. *)
PROCEDURE BuildInvoke(b: Block;  name: TEXT;  callee: Proc;
                      READONLY args: ARRAY OF Value;
                      normalBlock: Block;  unwindBlock: Block): Value;

(* `landingpad` begins the exception-handling preamble of an unwind block.
   Must be the first instruction in the block.
   isCleanup = TRUE  → `cleanup` clause (for TRY/FINALLY)
   isCleanup = FALSE → `catch ptr @_ZTI6_M3Exc` (for TRY/EXCEPT)
   Result type: TLandingPad() = { ptr, i32 }. *)
PROCEDURE BuildLandingPad(b: Block;  name: TEXT;  isCleanup: BOOLEAN): Value;

(* `extractvalue` — extract field idx from a struct aggregate.
   Result type = type of field idx in aggregate's struct type. *)
PROCEDURE BuildExtractValue(b: Block;  name: TEXT;
                             aggregate: Value;  idx: INTEGER): Value;

(* `resume` — re-throw the in-flight exception.  lp must have type
   TLandingPad().  Terminator. *)
PROCEDURE BuildResume(b: Block;  lp: Value);

(* `raise exc, arg` raises the exception exc at line (terminator, D35).
   arg is NIL for an exception without an argument.  For an argument
   passed by value, arg is the value (of ExcDescArgType); for one passed by
   reference, arg is the argument's address, and the lowering copies it to
   a heap box of the type ExcDescBoxCell names (the frame does not survive
   the unwinding).  MSIRRuntime expands it into RTHooks.RaiseEx (exc, arg,
   module, line). *)
PROCEDURE BuildRaise(b: Block;  exc: ExcDesc;  arg: Value;  line: INTEGER);
PROCEDURE InsnExcDesc(i: Insn): ExcDesc;     (* Raise only *)

(* `unwind_to target(args)` leaves one or more EH scopes for target, in an
   enclosing scope, running the FINALLY code of every Finally scope left
   (D8, D26).  The only way out of a scope other than ret_through_envelope
   and raising. *)
PROCEDURE BuildUnwindTo(b: Block;  target: Block;
                        READONLY args: ARRAY OF Value);

(* `ret_through_envelope v` returns from inside EH scopes, running the
   FINALLY code of every Finally scope left (D8, D26). *)
PROCEDURE BuildRetThroughEnvelope(b: Block;  value: Value);

(* `end_finally` ends FINALLY code (in a FinallyCode scope), resuming the
   outcome that entered it: the exit continues, the RETURN returns, or the
   exception is re-raised (D26).  Terminator. *)
PROCEDURE BuildEndFinally(b: Block);

(* `handler.activation` is the activation record (RT0.RaiseActivation, as an
   address) of the exception being handled by the nearest enclosing Handler
   scope; valid only in blocks with such a scope (Compiler.ThisException). *)
PROCEDURE BuildHandlerActivation(b: Block;  name: TEXT): Value;

(*-------------------------------------------------- rewriting (passes) *)

(* Primitives for passes that rewrite a procedure in place (MSIREH).
   BlockTakeInsns empties b and returns its instructions in order;
   BlockAppendInsn appends an existing instruction (one taken from a block)
   to b.  InsnMakeInvoke turns a call or call_indirect into an invoke with
   the given successors.  ProcClearScopes puts every block back in the
   root scope and forgets the others. *)
PROCEDURE BlockTakeInsns(b: Block): REF ARRAY OF Insn;
PROCEDURE BlockAppendInsn(b: Block;  i: Insn);
PROCEDURE InsnMakeInvoke(i: Insn;  normal, unwind: Block);
(* Turn i, whose result has v's type, into a copy of v (a no-op convert). *)
PROCEDURE InsnMakeCopy(i: Insn;  v: Value);
PROCEDURE ProcClearScopes(p: Proc);
(* Turn b's parameters into loads from slots[k] at the head of b, for
   lowerings that do not pass block arguments: the parameter Values become
   the loads' results, and b takes no parameters. *)
PROCEDURE BlockParamsToLoads(b: Block;  READONLY slots: ARRAY OF Value);

(*-------------------------------------- Open arrays / runtime-check builders *)

(* `openarray.size oa, k` — extract size at dimension k. Result: i64.
   k must be a non-negative integer constant; the verifier will check
   0 <= k < rank when added. *)
PROCEDURE BuildOpenArraySize(b: Block;  name: TEXT;
                             oa: Value;  dim: INTEGER): Value;

(* `openarray.elem_addr oa, idx0[, idx1...]` — pointer arithmetic.
   Result: ptr (element type).  Number of indices must equal rank. *)
PROCEDURE BuildOpenArrayElemAddr(b: Block;  name: TEXT;
                                 oa: Value;
                                 READONLY indices: ARRAY OF Value): Value;


(* `subscript_check idx, bound` — faults unless idx < bound, compared
   unsigned (so a negative idx faults too); idx and bound have one type. *)
PROCEDURE BuildSubscriptCheck(b: Block;  idx, bound: Value;
                              code, line: INTEGER);

(* `shape_check n, m` — faults unless dimension counts n and m (one
   integer type) are equal, as when an open array meets a fixed one. *)
PROCEDURE BuildShapeCheck(b: Block;  n, m: Value;  code, line: INTEGER);

(* `fault code` — the runtime error code (M3CG.RuntimeError) at line,
   statically inevitable; terminates b. *)
PROCEDURE BuildFault(b: Block;  code, line: INTEGER);

(* `nil_check ref` — raises if ref is nil. *)
PROCEDURE BuildNilCheck(b: Block;  ref: Value;  code, line: INTEGER);

(* `range_check value, lo, hi` — faults if value not in [lo..hi],
   compared unsigned if `unsigned`.  lo and hi have value's type and are
   typically constants; a one-sided check passes the type's extreme for
   the other bound, which the expansion omits. *)
PROCEDURE BuildRangeCheck(b: Block;  value, lo, hi: Value;
                          code, line: INTEGER;  unsigned := FALSE);

(*--------------------------------------------- Conversion / set builders *)

(* `convert value to targetType` — pure representation change (sign extend,
   truncate, sitofp, type-system narrowing, etc.). m3front emits a preceding
   range_check or other guard when narrowing semantics demand it. *)
PROCEDURE BuildConvert(b: Block;  name: TEXT;
                       value: Value;  targetType: T): Value;

(* Set intrinsics (D19).  m3front uses them for sets wider than a word,
   whose type is an integer iN (bit i is element FIRST + i).  An element
   operand is an i64 bit index, already biased by FIRST and checked by the
   front end: 0 <= i < N.  A set_range is empty when lo > hi; otherwise
   0 <= lo <= hi < N. *)
PROCEDURE BuildSetUnion     (b: Block;  name: TEXT;  x, y: Value): Value;
PROCEDURE BuildSetIntersect (b: Block;  name: TEXT;  x, y: Value): Value;
PROCEDURE BuildSetDifference(b: Block;  name: TEXT;  x, y: Value): Value;
PROCEDURE BuildSetSymDifference(b: Block;  name: TEXT;  x, y: Value): Value;
PROCEDURE BuildSetMember    (b: Block;  name: TEXT;
                             set: Value;  elt: Value): Value;
                             (* result: i1 *)
PROCEDURE BuildSetSingleton (b: Block;  name: TEXT;  t: T;  elt: Value): Value;
                             (* {elt}, of set type t *)
PROCEDURE BuildSetRange     (b: Block;  name: TEXT;  t: T;  lo, hi: Value): Value;
                             (* {lo..hi}, of set type t *)

(*----------------------------------------------- indirect dispatch *)

(* Advance a pointer by idx BYTES: `getelementptr i8, ptr %base, i64 idx`.
   General byte-offset GEP; result type = same as base.
   Used for vtable slot indexing (pass idx = method_slot * sizeof(ptr))
   and for heap object field access (pass idx = total byte offset). *)
PROCEDURE BuildPtrAdd(b: Block;  name: TEXT;  base: Value;  idx: INTEGER): Value;

(* Dynamic byte-offset GEP: getelementptr inbounds i8, ptr %base, i64 %offset.
   Used for ADDRESS arithmetic (INC/DEC on ADDRESS variables). *)
PROCEDURE BuildGepByte(b: Block;  name: TEXT;  base, offset: Value): Value;

(* Call through a function-pointer value.  fn must have ptr type.
   rtype is the MSIR return type (NIL for void).  Like BuildCall but the
   callee is a runtime value rather than a statically known Proc. *)
PROCEDURE BuildCallIndirect(b: Block;  name: TEXT;  fn: Value;  rtype: T;
                             READONLY args: ARRAY OF Value): Value;


(*------------------------------------------------- heap-array builders *)

(* `openarray.new cell, n0, n1, ...` allocates a REF to an open array
   (D34) with the given dimension sizes (integers), traced or not, of the
   type whose typecell is cell; the result has type t.  The lowerings pass
   the sizes in an open-array shape (M3RT.OA_) to RTHooks.NewTracedArray
   or NewUntracedArray. *)
PROCEDURE BuildOpenArrayNew(b: Block;  name: TEXT;  cell: Value;
                            traced: BOOLEAN;  uid: INTEGER;
                            READONLY sizes: ARRAY OF Value;  t: T): Value;


END MSIR.
