(* Modula-3 Semantic IR — typed-SSA IR sitting between m3front and LLVM.

   See repo-root MSIR.md (strategic narrative) and MSIR-design.md
   (decisions and open questions). This is the v0 skeleton: a minimal
   in-memory IR sufficient to construct and print small procedures
   programmatically. Verifier, parser, and LLVM emitter come next. *)

INTERFACE MSIR;

IMPORT Target;

(*------------------------------------------------------------------- Types *)

TYPE T <: REFANY;                     (* opaque MSIR type *)

TYPE TypeKind = {
  Void,
  I1, I8, I16, I32, I64,              (* signed integer widths *)
  W8, W16, W32, W64,                  (* unsigned (word) widths *)
  F32, F64, F128,
  Ptr,                                (* untraced pointer *)
  GcRef,                              (* register-resident traced ref *)
  GcSlot,                             (* memory-resident traced slot *)
  Struct,
  Object,                             (* class type with super, methods, descriptor *)
  OpenArray,                          (* fat pointer: { data, sizes... } *)
  HeapArray,                          (* REF ARRAY OF T: heap dope-prefix struct *)
  FixedArray,                         (* ARRAY [lo..hi] OF T: contiguous storage *)
  Subrange,                           (* parent type T constrained to [lo..hi] *)
  Set,                                (* set with element type and domain [lo..hi] *)
  ProcType
};

TYPE Field  = RECORD name: TEXT;  type: T  END;
TYPE Method = RECORD name: TEXT;  procType: T  END;

PROCEDURE TVoid(): T;
PROCEDURE TI1(): T;
PROCEDURE TI(bits: INTEGER): T;       (* bits in {8,16,32,64} *)
PROCEDURE TW(bits: INTEGER): T;
PROCEDURE TF(bits: INTEGER): T;       (* bits in {32,64,128} *)
PROCEDURE TPtr(elt: T): T;
PROCEDURE TGcRef(elt: T): T;
PROCEDURE TGcSlot(elt: T): T;
PROCEDURE TStruct(name: TEXT;  READONLY fields: ARRAY OF Field): T;
PROCEDURE TProc(READONLY params: ARRAY OF T;  result: T): T;
PROCEDURE TObject(name: TEXT;
                  super: T;                      (* NIL for root *)
                  READONLY fields:  ARRAY OF Field;
                  READONLY methods: ARRAY OF Method;
                  descriptorSym: TEXT): T;
PROCEDURE TOpenArray(rank: INTEGER;  elt: T): T;
PROCEDURE THeapArray(rank: INTEGER;  elt: T): T;
PROCEDURE TFixedArray(len: LONGINT;  elt: T): T;
PROCEDURE TSubrange(parent: T;  lo, hi: LONGINT): T;
PROCEDURE TSet(elt: T;  lo, hi: LONGINT): T;

(* Landing-pad aggregate: { ptr, i32 } — the LLVM EH landingpad result type. *)
PROCEDURE TLandingPad(): T;

PROCEDURE Kind(t: T): TypeKind;
PROCEDURE Equal(a, b: T): BOOLEAN;
PROCEDURE EltType(t: T): T;           (* for Ptr / GcRef / GcSlot *)
PROCEDURE StructName(t: T): TEXT;
PROCEDURE StructFieldCount(t: T): INTEGER;
PROCEDURE StructField(t: T; i: INTEGER): Field;
PROCEDURE BitWidth(t: T): INTEGER;    (* for scalar kinds; -1 otherwise *)

PROCEDURE ObjectName(t: T): TEXT;
PROCEDURE ObjectSuper(t: T): T;       (* NIL if root *)
PROCEDURE ObjectFieldCount(t: T): INTEGER;
PROCEDURE ObjectField(t: T;  i: INTEGER): Field;
PROCEDURE ObjectMethodCount(t: T): INTEGER;
PROCEDURE ObjectMethod(t: T;  i: INTEGER): Method;
PROCEDURE ObjectMethodIndex(t: T;  selector: TEXT): INTEGER;  (* -1 if not found *)
PROCEDURE ObjectFieldIndex(t: T;  name: TEXT): INTEGER;       (* -1 if not found *)
PROCEDURE ObjectDescriptorSym(t: T): TEXT;

PROCEDURE OpenArrayRank(t: T): INTEGER;
PROCEDURE OpenArrayElt(t: T): T;

PROCEDURE HeapArrayRank(t: T): INTEGER;
PROCEDURE HeapArrayElt(t: T): T;

PROCEDURE FixedArrayLen(t: T): LONGINT;
PROCEDURE FixedArrayElt(t: T): T;

PROCEDURE SubrangeParent(t: T): T;
PROCEDURE SubrangeLo(t: T): LONGINT;
PROCEDURE SubrangeHi(t: T): LONGINT;

PROCEDURE SetElt(t: T): T;
PROCEDURE SetLo(t: T): LONGINT;
PROCEDURE SetHi(t: T): LONGINT;

(*----------------------------------------------------------------- Values *)

TYPE Value <: REFANY;

TYPE ValueKind = {ConstInt, ConstFloat, ConstNil, ConstProc, ConstTextLit, Param, BlockParam, InsnResult, GlobalRef,
                  StructFieldRef (* GEP into module's @Mod_M3_info struct *) };

PROCEDURE ConstInt(t: T;  v: LONGINT): Value;
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
PROCEDURE ConstTextLit(uid: INTEGER; chars: TEXT; cnt: INTEGER): Value;
(* A TEXT literal value. uid indexes @textlit_<uid> for LLVM lowering;
   chars/cnt are the string content for readable MSIR text output. *)
PROCEDURE GetTextLitUID  (v: Value): INTEGER;
PROCEDURE GetTextLitChars(v: Value): TEXT;
PROCEDURE GetTextLitCnt  (v: Value): INTEGER;

PROCEDURE ValueType(v: Value): T;
PROCEDURE ValueName(v: Value): TEXT;  (* SSA name, e.g. "%5" or "a" *)
PROCEDURE GetValueKind(v: Value): ValueKind;
PROCEDURE GetIntVal(v: Value): LONGINT;            (* for ConstInt only *)

(*------------------------------------------------------- Parameters / modes *)

TYPE ParamMode = {ByValue, Var, Readonly};
TYPE Param      = RECORD name: TEXT;  type: T;  mode: ParamMode  END;
TYPE BlockParam = RECORD name: TEXT;  type: T  END;

(*-------------------------------------------------------------- Procedure *)

TYPE Proc <: REFANY;

PROCEDURE NewProc(name: TEXT;
                  READONLY params: ARRAY OF Param;
                  result: T): Proc;

(*------------------------------------------------- procedure attributes *)

TYPE CallingConvention = {M3, C, WinAPI};
TYPE Linkage           = {External, Internal};

(* Default attributes after NewProc: empty raises, not noreturn,
   External linkage, M3 calling convention. *)
PROCEDURE ProcSetRaises(p: Proc;  READONLY exceptionSyms: ARRAY OF TEXT);
PROCEDURE ProcRaisesCount(p: Proc): INTEGER;
PROCEDURE ProcRaises(p: Proc;  i: INTEGER): TEXT;

PROCEDURE ProcSetNoReturn(p: Proc;  v: BOOLEAN);
PROCEDURE ProcIsNoReturn(p: Proc): BOOLEAN;

PROCEDURE ProcSetLinkage(p: Proc;  l: Linkage);
PROCEDURE ProcGetLinkage(p: Proc): Linkage;

PROCEDURE ProcSetCallingConvention(p: Proc;  cc: CallingConvention);
PROCEDURE ProcGetCallingConvention(p: Proc): CallingConvention;

PROCEDURE ProcName(p: Proc): TEXT;
PROCEDURE ProcParamCount(p: Proc): INTEGER;
PROCEDURE ProcParam    (p: Proc;  i: INTEGER): Value;
PROCEDURE ProcParamName(p: Proc;  i: INTEGER): TEXT;
PROCEDURE ProcParamMode(p: Proc;  i: INTEGER): ParamMode;
PROCEDURE ProcResultType(p: Proc): T;
PROCEDURE ProcEntry(p: Proc): Block;
PROCEDURE ProcBlockCount(p: Proc): INTEGER;          (* counts blocks only *)
PROCEDURE ProcBlock(p: Proc;  i: INTEGER): Block;    (* i-th block, skipping envelopes *)
PROCEDURE ProcAddBlock(p: Proc;  b: Block);
PROCEDURE ProcAddEnvelope(p: Proc;  e: Envelope);

(* Top-level region: ordered list of blocks and envelopes. *)
PROCEDURE ProcItemCount(p: Proc): INTEGER;
PROCEDURE ProcItem(p: Proc;  i: INTEGER): REFANY;    (* Block or Envelope *)
PROCEDURE ProcItemIsBlock(p: Proc;  i: INTEGER): BOOLEAN;
PROCEDURE ProcItemIsEnvelope(p: Proc;  i: INTEGER): BOOLEAN;

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

(*--------------------------------------------------------------- Envelope *)

TYPE Envelope <: REFANY;

TYPE Handler = RECORD
  exceptionSym: TEXT;        (* M3 exception identity, e.g. "@NotFound" *)
  block:        Block;       (* parameterized for value-binding clauses *)
END;

PROCEDURE NewTryExcept(body: Block;
                       READONLY handlers: ARRAY OF Handler): Envelope;
PROCEDURE NewTryFinally(body: Block;  finallyBlock: Block): Envelope;

PROCEDURE IsTryExcept(e: Envelope): BOOLEAN;
PROCEDURE IsTryFinally(e: Envelope): BOOLEAN;
PROCEDURE EnvelopeBody(e: Envelope): Block;
PROCEDURE EnvelopeHandlerCount(e: Envelope): INTEGER;  (* TryExcept only *)
PROCEDURE EnvelopeHandler(e: Envelope;  i: INTEGER): Handler;
PROCEDURE EnvelopeFinally(e: Envelope): Block;        (* TryFinally only *)

(*--------------------------------------------------------------- Module *)

TYPE Module <: REFANY;

PROCEDURE NewModule(name: TEXT): Module;
PROCEDURE ModuleName(m: Module): TEXT;
PROCEDURE SetModuleTarget(m: Module;  triple, datalayout: TEXT);
PROCEDURE ModuleTriple(m: Module): TEXT;
PROCEDURE ModuleDataLayout(m: Module): TEXT;

(* Register the RunTyme hook procs used by the GC barriers and typecase.
   Called from MSIREmit after RunTyme lookup.  Pass NIL to use the fallback
   hardcoded symbol names in the LLVM emitter. *)
PROCEDURE SetModuleHooks(m: Module;
                          gcLoad, gcStore, scanTypecase: Proc);
PROCEDURE ModuleGCLoadBarrier  (m: Module): Proc;
PROCEDURE ModuleGCStoreBarrier (m: Module): Proc;
PROCEDURE ModuleScanTypecase   (m: Module): Proc;
PROCEDURE ModuleAddImport(m: Module;  name: TEXT);
PROCEDURE ModuleImportCount(m: Module): INTEGER;
PROCEDURE ModuleImport(m: Module;  i: INTEGER): TEXT;
PROCEDURE ModuleAddProc(m: Module;  p: Proc);
PROCEDURE ModuleProcCount(m: Module): INTEGER;
PROCEDURE ModuleProc(m: Module;  i: INTEGER): Proc;

(*-------------------------------------------------- exception descriptors *)

(* A per-exception static descriptor: { uid: i64, name: ptr, implicit: i64 }.
   Created by NewExcDesc; emitted as a module-level internal global.
   The Value returned by ExcDescValue has ptr type and may be passed
   directly as the 'ex' argument to RTHooks__Raise. *)
TYPE ExcDesc <: REFANY;

PROCEDURE NewExcDesc(name: TEXT;  uid: LONGINT): ExcDesc;
PROCEDURE ExcDescName (d: ExcDesc): TEXT;    (* LLVM global symbol *)
PROCEDURE ExcDescUID  (d: ExcDesc): LONGINT;
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

(* uid = M3FP fingerprint.  kind: ORD(M3RT.TypeKind) — Ref=1, Obj=2, Array=3.
   dataSize in bytes; dataAlignment in bits (matches M3RT TC_dataAlignment).
   methods: names of vtable function symbols, one per slot (OBJECT only).
   methodBytes: total vtable byte size; -1 means compute from methods array. *)
PROCEDURE NewTypeDesc(name: TEXT; uid: LONGINT; isTraced: BOOLEAN;
                      kind: INTEGER; dataSize: INTEGER;
                      dataAlignment: INTEGER;
                      parentUID: LONGINT := 0L;
                      dataOffset: INTEGER := 0;
                      READONLY methods: ARRAY OF TEXT := ARRAY OF TEXT{};
                      methodBytes: INTEGER := -1): TypeDesc;
PROCEDURE TypeDescName        (d: TypeDesc): TEXT;
PROCEDURE TypeDescValue       (d: TypeDesc): Value;   (* ptr to the TypeCell global *)
PROCEDURE TypeDescUID         (d: TypeDesc): LONGINT;
PROCEDURE TypeDescTraced      (d: TypeDesc): BOOLEAN;
PROCEDURE TypeDescKind        (d: TypeDesc): INTEGER; (* ORD(M3RT.TypeKind) *)
PROCEDURE TypeDescSize        (d: TypeDesc): INTEGER; (* dataSize in bytes *)
PROCEDURE TypeDescAlign       (d: TypeDesc): INTEGER; (* dataAlignment in bits *)
PROCEDURE TypeDescParentUID   (d: TypeDesc): LONGINT; (* OBJ: parent fingerprint *)
PROCEDURE TypeDescDataOffset  (d: TypeDesc): INTEGER; (* OBJ: field region byte offset *)
PROCEDURE TypeDescMethodBytes (d: TypeDesc): INTEGER; (* OBJ: vtable byte size *)
PROCEDURE TypeDescMethodCount (d: TypeDesc): INTEGER;
PROCEDURE TypeDescMethod      (d: TypeDesc;  i: INTEGER): TEXT;
(* Array TypeCell extensions (kind = ORD(M3RT.TypeKind.Array)). *)
PROCEDURE TypeDescSetArrayInfo(d: TypeDesc; nDimensions, elementSize: INTEGER);
PROCEDURE TypeDescNDimensions (d: TypeDesc): INTEGER;
PROCEDURE TypeDescElementSize (d: TypeDesc): INTEGER;

PROCEDURE ModuleAddTypeDesc  (m: Module;  d: TypeDesc);
PROCEDURE ModuleTypeDescCount(m: Module): INTEGER;
PROCEDURE ModuleTypeDesc     (m: Module;  i: INTEGER): TypeDesc;

(* Create a ptr-typed value that refers to the TypeCell global named 'name'.
   Used in proc bodies before the TypeCell global is registered (forward ref). *)
PROCEDURE TypeCellRef (name: TEXT): Value;

(*----------------------------------------------- type cell references (cell_ptrs) *)

(* Analog of RT0.TypeLink / CG cell_ptrs.  Each entry is a { ptr defn, i64 uid }
   global added to MI_type_cell_ptrs.  RTLinker.ResolveTypeLinks fills defn with
   the actual TypeCell pointer at module-init time.
   Loading defn at runtime (after RTLinker) gives the TypeCell pointer. *)
TYPE TypeLink <: REFANY;

PROCEDURE NewTypeLink (name: TEXT;  uid: LONGINT): TypeLink;
PROCEDURE TypeLinkName (tl: TypeLink): TEXT;
PROCEDURE TypeLinkUID  (tl: TypeLink): LONGINT;

PROCEDURE ModuleAddTypeLink  (m: Module;  tl: TypeLink);
PROCEDURE ModuleTypeLinkCount(m: Module): INTEGER;
PROCEDURE ModuleTypeLink     (m: Module;  i: INTEGER): TypeLink;

(* TEXT literals — populated by MSIREmit.EndUnit from TextExpr.Literal* *)
PROCEDURE ModuleAddTextLit  (m: Module;  chars: TEXT;  cnt: INTEGER): INTEGER;
PROCEDURE ModuleTextLitCount(m: Module): INTEGER;
PROCEDURE ModuleTextLitChars(m: Module;  uid: INTEGER): TEXT;
PROCEDURE ModuleTextLitCnt  (m: Module;  uid: INTEGER): INTEGER;

(* TextLiteral vtable method hook procs — the five RTHooks__TextLit* procs.
   Populated by MSIREmit.EndUnit via MSIRBuilder.HookProc + RunTyme.LookUpProc
   so that names come from the compiler's view of RTHooks rather than being
   hardcoded in the LLVM emitter.  NIL elements mean not yet registered. *)
PROCEDURE ModuleSetTextLitHooks(m: Module;
                                 READONLY hooks: ARRAY [0..4] OF Proc);
PROCEDURE ModuleGetTextLitHook (m: Module;  i: INTEGER): Proc;
(* indices: 0=TextLitInfo 1=TextLitGetChar 2=TextLitGetWideChar
            3=TextLitGetChars 4=TextLitGetWideChars *)

(* Build a getelementptr computing the TEXT reference:
   ptr to offset 8 of @textlit_<uid> (past the GC header to the vtable field).
   uid comes from TextExpr.SetUID via TextExpr.CompileMSIR. *)
PROCEDURE BuildTextLiteralRef(b: Block;  uid: INTEGER): Value;

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
PROCEDURE GlobalIsTraced   (g: Global): BOOLEAN;
PROCEDURE GlobalIsExternal (g: Global): BOOLEAN;
PROCEDURE GlobalByteOffset    (g: Global): INTEGER; (* -1 for external/standalone *)
PROCEDURE GlobalSetStructField(g: Global;  byteOff: INTEGER;  ref: Value);
PROCEDURE GlobalValue(g: Global): Value;
                                             (* an addressable Value:
                                                gc_slot type if traced,
                                                ptr type otherwise *)

PROCEDURE ModuleAddGlobal(m: Module;  g: Global);

(* Allocate space for a struct-embedded global in the module's @Mod_M3_info
   struct.  Returns the byte offset allocated; advances the internal cursor.
   byteSize is the size in bytes; byteAlign is the required byte alignment.
   After MI_SIZE bytes of standard ModuleInfo fields, user globals follow. *)
PROCEDURE ModuleAllocGlobal(m: Module;  byteSize: INTEGER;
                             byteAlign: INTEGER): INTEGER;
PROCEDURE ModuleGlobalStructSize(m: Module): INTEGER;
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
  Load, Store, GcLoad, GcStore, FieldAddr,
  (* integer arithmetic *)
  IAdd, ISub, IMul, IDiv, IMod,
  IAnd, IOr, IXor, IShl, ILShr, IAShr, (* bitwise / shift *)
  (* float arithmetic *)
  FAdd, FSub, FMul, FDiv, FNeg,
  (* compare *)
  ICmp, FCmp,
  (* control *)
  Br, CondBr, Ret, Unreachable,
  UnwindTo, RetThroughEnvelope,        (* cross-envelope exits *)
  (* call / alloc *)
  Call, Invoke, New,
  (* M3 object/RTTI intrinsics *)
  Dispatch, Narrow, Istype, Typecase,
  (* exception *)
  Raise,
  LandingPad,   (* LLVM landingpad instruction; cleanup or catch _ZTI6_M3Exc *)
  ExtractValue, (* extract field from a struct aggregate by index *)
  Resume,       (* LLVM resume — re-throw after landingpad (terminator) *)
  (* open arrays *)
  OpenArraySize, OpenArrayElemAddr, Subarray,
  OpenArrayNew, OpenArrayDeref,
  (* fixed arrays *)
  ArrayElemAddr,
  (* runtime checks *)
  SubscriptCheck, NilCheck, RangeCheck,
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
  (* sets *)
  SetUnion, SetIntersect, SetDifference, SetMember,
  (* indirect dispatch *)
  PtrAdd,         (* getelementptr ptr, ptr %base, i64 N  — vtable slot address *)
  GepByte,        (* getelementptr i8, ptr %base, i64 %offset — dynamic byte-offset ptr arith *)
  CallIndirect,   (* call via function-pointer value (no static Proc target) *)
  InvokeIndirect  (* invoke via function-pointer value, with normal/unwind targets *)
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

PROCEDURE InsnOp(i: Insn): Op;
PROCEDURE InsnResult(i: Insn): Value;        (* NIL if Void-typed *)
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
PROCEDURE InsnTargetType(i: Insn): T;        (* Alloca, New, Narrow, Istype *)
PROCEDURE InsnSelector(i: Insn): TEXT;       (* Dispatch: method name; FieldAddr: field name *)
PROCEDURE InsnExtractIdx(i: Insn): INTEGER;  (* ExtractValue: field index *)
PROCEDURE InsnIsCleanup(i: Insn): BOOLEAN;   (* LandingPad: TRUE=cleanup, FALSE=catch *)

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
PROCEDURE BuildICmp(b: Block;  name: TEXT;  pred: CmpPred;  x, y: Value): Value;

PROCEDURE BuildFAdd(b: Block;  name: TEXT;  x, y: Value): Value;
PROCEDURE BuildFSub(b: Block;  name: TEXT;  x, y: Value): Value;
PROCEDURE BuildFMul(b: Block;  name: TEXT;  x, y: Value): Value;
PROCEDURE BuildFDiv(b: Block;  name: TEXT;  x, y: Value): Value;
PROCEDURE BuildFNeg(b: Block;  name: TEXT;  x: Value): Value;
PROCEDURE BuildFCmp(b: Block;  name: TEXT;  pred: FCmpPred;  x, y: Value): Value;

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

PROCEDURE BuildLoad (b: Block;  name: TEXT;  type: T;  addr: Value): Value;
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
PROCEDURE BuildGcStore(b: Block;  slot: Value;  value: Value;
                        container: Value := NIL);

PROCEDURE BuildFieldAddr(b: Block;  name: TEXT;
                         obj: Value;  fieldName: TEXT): Value;
                                             (* obj: gc_ref Object | gc_ref Struct |
                                                ptr Struct; result: ptr fieldType *)

(* `array.elem_addr arr, idx` — pointer arithmetic on a fixed array.
   arr must be ptr FixedArray (i.e. an alloca or by-ref).
   Result: ptr (element type). *)
PROCEDURE BuildArrayElemAddr(b: Block;  name: TEXT;
                             arr: Value;  idx: Value): Value;

PROCEDURE BuildNew(b: Block;  name: TEXT;  type: T): Value;
                                             (* type must be Object; result: gc_ref type *)

PROCEDURE BuildDispatch(b: Block;  name: TEXT;
                        obj: Value;  methodName: TEXT;
                        READONLY args: ARRAY OF Value): Value;

PROCEDURE BuildNarrow(b: Block;  name: TEXT;
                      obj: Value;  targetType: T): Value;
PROCEDURE BuildIstype(b: Block;  name: TEXT;
                      obj: Value;  targetType: T): Value;

(* TYPECASE is a structured terminator op.  Each clause names a target type
   (or ELSE) and a parameterized block.  ELSE is mandatory at MSIR level.
   uid  = M3 type fingerprint (Type.GlobalUID) used to build the
          RTHooks__ScanTypecase type table; 0 for the ELSE terminator.
   targetType = MSIR type of the narrowed value (NIL if isElse). *)
TYPE TypecaseClause = RECORD
  isElse:     BOOLEAN;
  uid:        LONGINT;  (* M3 type UID; 0 for ELSE *)
  targetType: T;        (* NIL if isElse *)
  block:      Block;
END;

PROCEDURE BuildTypecase(b: Block;  value: Value;
                        READONLY clauses: ARRAY OF TypecaseClause);

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
   isCleanup = FALSE → `catch ptr @_ZTI7_M3Exc` (for TRY/EXCEPT)
   Result type: TLandingPad() = { ptr, i32 }. *)
PROCEDURE BuildLandingPad(b: Block;  name: TEXT;  isCleanup: BOOLEAN): Value;

(* `extractvalue` — extract field idx from a struct aggregate.
   Result type = type of field idx in aggregate's struct type. *)
PROCEDURE BuildExtractValue(b: Block;  name: TEXT;
                             aggregate: Value;  idx: INTEGER): Value;

(* `resume` — re-throw the in-flight exception.  lp must have type
   TLandingPad().  Terminator. *)
PROCEDURE BuildResume(b: Block;  lp: Value);

(* `raise` raises an M3 exception by identity symbol, with optional value.
   Pass NIL value for exceptions that carry no payload. *)
PROCEDURE BuildRaise(b: Block;  exceptionSym: TEXT;  value: Value);

(* `unwind_to` is the only legal way to br out of an envelope to a block
   in an outer scope (running enclosing FINALLYs). *)
PROCEDURE BuildUnwindTo(b: Block;  target: Block;
                        READONLY args: ARRAY OF Value);

(* `ret_through_envelope` is the only legal way to ret from inside an
   envelope (running enclosing FINALLYs). *)
PROCEDURE BuildRetThroughEnvelope(b: Block;  value: Value);

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

(* `subarray oa, lo, count` — 1D only. Result: openarray<1> elt.
   Bounds check must be emitted separately if needed; this op is pure
   arithmetic + struct construction. *)
PROCEDURE BuildSubarray(b: Block;  name: TEXT;
                        oa: Value;  lo, count: Value): Value;

(* `subscript_check idx, bound` — raises if idx not in [0, bound). *)
PROCEDURE BuildSubscriptCheck(b: Block;  idx, bound: Value);

(* `nil_check ref` — raises if ref is nil. *)
PROCEDURE BuildNilCheck(b: Block;  ref: Value);

(* `range_check value, lo, hi` — raises if value not in [lo..hi].
   lo and hi are typically constants but may be any Value. *)
PROCEDURE BuildRangeCheck(b: Block;  value, lo, hi: Value);

(*--------------------------------------------- Conversion / set builders *)

(* `convert value to targetType` — pure representation change (sign extend,
   truncate, sitofp, type-system narrowing, etc.). m3front emits a preceding
   range_check or other guard when narrowing semantics demand it. *)
PROCEDURE BuildConvert(b: Block;  name: TEXT;
                       value: Value;  targetType: T): Value;

PROCEDURE BuildSetUnion     (b: Block;  name: TEXT;  x, y: Value): Value;
PROCEDURE BuildSetIntersect (b: Block;  name: TEXT;  x, y: Value): Value;
PROCEDURE BuildSetDifference(b: Block;  name: TEXT;  x, y: Value): Value;
PROCEDURE BuildSetMember    (b: Block;  name: TEXT;
                             set: Value;  elt: Value): Value;
                             (* result: i1 *)

(*----------------------------------------------- indirect dispatch *)

(* Advance a pointer by idx BYTES: `getelementptr i8, ptr %base, i64 idx`.
   General byte-offset GEP; result type = same as base.
   Used for vtable slot indexing (pass idx = method_slot * sizeof(ptr))
   and for heap object field access (pass idx = total byte offset). *)
PROCEDURE BuildPtrAdd(b: Block;  name: TEXT;  base: Value;  idx: LONGINT): Value;

(* Dynamic byte-offset GEP: getelementptr inbounds i8, ptr %base, i64 %offset.
   Used for ADDRESS arithmetic (INC/DEC on ADDRESS variables). *)
PROCEDURE BuildGepByte(b: Block;  name: TEXT;  base, offset: Value): Value;

(* Call through a function-pointer value.  fn must have ptr type.
   rtype is the MSIR return type (NIL for void).  Like BuildCall but the
   callee is a runtime value rather than a statically known Proc. *)
PROCEDURE BuildCallIndirect(b: Block;  name: TEXT;  fn: Value;  rtype: T;
                             READONLY args: ARRAY OF Value): Value;

(* Invoke through a function-pointer value, with EH normal/unwind targets. *)
PROCEDURE BuildInvokeIndirect(b: Block;  name: TEXT;  fn: Value;  rtype: T;
                               READONLY args: ARRAY OF Value;
                               normalBlock: Block;  unwindBlock: Block): Value;

(*------------------------------------------------- heap-array builders *)

(* `openarray.new` heap-allocates a `REF ARRAY OF elt` with the given
   per-dimension sizes. Result type: `gc_ref HeapArray<rank, elt>`. *)
PROCEDURE BuildOpenArrayNew(b: Block;  name: TEXT;
                            elt: T;
                            READONLY sizes: ARRAY OF Value): Value;

(* `openarray.deref` extracts the fat-pointer view from a heap-array ref.
   Input: gc_ref HeapArray<rank, elt>.  Output: openarray<rank> elt. *)
PROCEDURE BuildOpenArrayDeref(b: Block;  name: TEXT;  ref: Value): Value;

END MSIR.
