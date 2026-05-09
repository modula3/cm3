(* Modula-3 Semantic IR — typed-SSA IR sitting between m3front and LLVM.

   See repo-root MSIR.md (strategic narrative) and MSIR-design.md
   (decisions and open questions). This is the v0 skeleton: a minimal
   in-memory IR sufficient to construct and print small procedures
   programmatically. Verifier, parser, and LLVM emitter come next. *)

INTERFACE MSIR;

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

TYPE ValueKind = {ConstInt, ConstNil, Param, BlockParam, InsnResult, GlobalRef};

PROCEDURE ConstInt(t: T;  v: LONGINT): Value;
PROCEDURE ConstBool(v: BOOLEAN): Value;
PROCEDURE ConstNil(t: T): Value;      (* t must be Ptr / GcRef *)

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
PROCEDURE ProcParam(p: Proc;  i: INTEGER): Value;
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
PROCEDURE ModuleAddImport(m: Module;  name: TEXT);
PROCEDURE ModuleImportCount(m: Module): INTEGER;
PROCEDURE ModuleImport(m: Module;  i: INTEGER): TEXT;
PROCEDURE ModuleAddProc(m: Module;  p: Proc);
PROCEDURE ModuleProcCount(m: Module): INTEGER;
PROCEDURE ModuleProc(m: Module;  i: INTEGER): Proc;

(*-------------------------------------------------------- module globals *)

TYPE Global <: REFANY;

(* A module-level global. If isTraced, the slot must be registered as a
   GC root in the module descriptor (D14) and stores go through
   gc.store; loads go through gc.load. Otherwise plain load/store. *)
PROCEDURE NewGlobal(name: TEXT;  type: T;  isTraced: BOOLEAN): Global;
PROCEDURE GlobalName(g: Global): TEXT;
PROCEDURE GlobalType(g: Global): T;          (* the user's value type *)
PROCEDURE GlobalIsTraced(g: Global): BOOLEAN;
PROCEDURE GlobalValue(g: Global): Value;
                                             (* an addressable Value:
                                                gc_slot type if traced,
                                                ptr type otherwise *)

PROCEDURE ModuleAddGlobal(m: Module;  g: Global);
PROCEDURE ModuleGlobalCount(m: Module): INTEGER;
PROCEDURE ModuleGlobal(m: Module;  i: INTEGER): Global;

(*------------------------------------------------------------------- Insn *)

TYPE Insn <: REFANY;

TYPE Op = {
  (* memory *)
  Alloca, Load, Store, GcLoad, GcStore, FieldAddr,
  (* arithmetic *)
  IAdd, ISub, IMul,
  (* compare *)
  ICmp,
  (* control *)
  Br, CondBr, Ret, Unreachable,
  UnwindTo, RetThroughEnvelope,        (* cross-envelope exits *)
  (* call / alloc *)
  Call, Invoke, New,
  (* M3 object/RTTI intrinsics *)
  Dispatch, Narrow, Istype, Typecase,
  (* exception *)
  Raise,
  (* open arrays *)
  OpenArraySize, OpenArrayElemAddr, Subarray,
  OpenArrayNew, OpenArrayDeref,
  (* fixed arrays *)
  ArrayElemAddr,
  (* runtime checks *)
  SubscriptCheck, NilCheck, RangeCheck,
  (* representation conversion *)
  Convert,
  (* sets *)
  SetUnion, SetIntersect, SetDifference, SetMember
};

TYPE CmpPred = {
  Eq, Ne,
  Slt, Sle, Sgt, Sge,
  Ult, Ule, Ugt, Uge
};

PROCEDURE InsnOp(i: Insn): Op;
PROCEDURE InsnResult(i: Insn): Value;        (* NIL if Void-typed *)
PROCEDURE InsnOperandCount(i: Insn): INTEGER;
PROCEDURE InsnOperand(i: Insn;  k: INTEGER): Value;

(* Some opcodes carry extra data that isn't an SSA operand: *)
PROCEDURE InsnCmpPred(i: Insn): CmpPred;     (* ICmp only *)
PROCEDURE InsnBrTarget(i: Insn;  k: INTEGER): Block;
                                             (* k=0 for Br, k∈{0,1} for CondBr *)
PROCEDURE InsnBrArgCount(i: Insn;  k: INTEGER): INTEGER;
PROCEDURE InsnBrArg(i: Insn;  k, j: INTEGER): Value;
PROCEDURE InsnCallee(i: Insn): Proc;         (* Call only *)
PROCEDURE InsnTargetType(i: Insn): T;        (* Alloca, New, Narrow, Istype *)
PROCEDURE InsnSelector(i: Insn): TEXT;       (* Dispatch: method name; FieldAddr: field name *)

(*---------------------------------------------------------------- Builders *)

PROCEDURE BuildIAdd(b: Block;  name: TEXT;  x, y: Value): Value;
PROCEDURE BuildISub(b: Block;  name: TEXT;  x, y: Value): Value;
PROCEDURE BuildIMul(b: Block;  name: TEXT;  x, y: Value): Value;
PROCEDURE BuildICmp(b: Block;  name: TEXT;  pred: CmpPred;  x, y: Value): Value;

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
                                             (* result: ptr type *)

PROCEDURE BuildGcLoad(b: Block;  name: TEXT;  slot: Value): Value;
                                             (* slot must have type gc_slot T *)
PROCEDURE BuildGcStore(b: Block;  slot: Value;  value: Value);

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

(* TYPECASE is a structured terminator op. Each clause names a target type
   (or ELSE) and a parameterized block. The block's first parameter (if any)
   receives the matched value narrowed to the clause type. ELSE is mandatory
   at MSIR level — m3front synthesizes one if source omits it. *)
TYPE TypecaseClause = RECORD
  isElse:     BOOLEAN;
  targetType: T;        (* NIL if isElse *)
  block:      Block;
END;

PROCEDURE BuildTypecase(b: Block;  value: Value;
                        READONLY clauses: ARRAY OF TypecaseClause);

PROCEDURE InsnTypecaseClauseCount(i: Insn): INTEGER;
PROCEDURE InsnTypecaseClause(i: Insn;  k: INTEGER): TypecaseClause;

(*--------------------------------------------------- EH builders / control *)

(* `invoke` is `call` from inside a `try` envelope; lowering routes its
   unwind path to the enclosing envelope's handler chain. The verifier
   (when added) checks that invoke appears only inside try envelopes. *)
PROCEDURE BuildInvoke(b: Block;  name: TEXT;  callee: Proc;
                      READONLY args: ARRAY OF Value): Value;

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
