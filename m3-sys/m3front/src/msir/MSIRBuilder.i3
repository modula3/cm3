(* MSIRBuilder — per-procedure builder state.

   The builder is a singleton; m3front is single-threaded. BeginProc
   creates an MSIR.Proc and an entry Block; EndProc finalizes the proc
   and adds it to the current MSIREmit module. If at any point the
   builder encounters an M3 construct it cannot translate yet, the
   caller marks the proc unsupported via Abandon; EndProc then drops
   the proc instead of adding it to the module.

   v0 only carries enough state to emit a proc skeleton (signature,
   parameters, empty body). Body translation arrives in the next
   plan step. *)

INTERFACE MSIRBuilder;

IMPORT MSIR, MSIRBitLoc, Type, Value, Scope, Variable, RunTyme, CaptureAnalysis, Target;

(* BeginProc: create a fresh MSIR.Proc with the given name and signature.
   formals is the head of the formal-list returned by ProcType.Formals.
   syms is the procedure's local scope (used to resolve VarExpr
   references back to MSIR Param values).
   Returns FALSE if the signature is outside the supported subset
   (e.g. has an unsupported parameter or result type); the caller should
   then suppress all subsequent MSIR calls for this procedure. *)
PROCEDURE BeginProc(name: TEXT;
                    formals: Value.T;
                    syms: Scope.T;
                    result: Type.T;
                    isExternal: BOOLEAN;
                    captures: CaptureAnalysis.T := NIL): BOOLEAN;
(* When captures is non-NIL (nested proc): lambda-lift by generating one
   ptr param per captured variable instead of a single %__env frame pointer.
   Call sites get the same list from Procedure.Captures to build the
   capture args. *)

(* Resolve a Variable.T to its current SSA value.
   For formals: returns the MSIR Param value directly.
   For locals:  emits a Load from the variable's alloca and returns
                the loaded value.
   Returns NIL if not bound; callers should treat NIL as Abandon. *)
PROCEDURE LookupVar(v: Variable.T): MSIR.Value;

(* Resolve a Variable.T to its alloca address (for store targets).
   Returns NIL for formals (cannot store to by-value formal in v0)
   and for unbound variables. *)
PROCEDURE LookupVarAddr(v: Variable.T): MSIR.Value;

(* Register a local (non-formal) Variable.T in the current proc.
   Emits an Alloca in the current block and records the ptr in the
   var map so that LookupVar / LookupVarAddr work.
   Returns FALSE (and Abandons) if the type is unsupported. *)
PROCEDURE AddLocal(v: Variable.T): BOOLEAN;

(* Bind a Variable.T to an existing address (no alloca emitted).
   LookupVar(v) will emit a load through addr; LookupVarAddr(v) returns addr.
   Used for WITH designator variables, where addr is the target's address. *)
PROCEDURE BindVarAddr(v: Variable.T; addr: MSIR.Value; elemType: MSIR.T;
                      container: MSIR.Value := NIL);
(* container: for a WITH alias over a heap designator, the containing heap
   object (the designator's Expr.HeapLValueMSIR container); VarContainer
   returns it, so stores through the alias carry the write barrier. *)

(* Bind a WITH alias over a designator with no byte address (`WITH s =
   rec.bitf`, `WITH e = packed[i]`) to its bit location, whose base was
   evaluated once at WITH entry.  LookupVar(v) reads through ExtractBitLoc;
   LookupVarAddr(v) returns NIL; BitLocOfVar gives the location for writes.
   Matches the C backend's by-reference alias (writes reach the field). *)
PROCEDURE BindVarBitField(v: Variable.T; READONLY loc: MSIRBitLoc.T);
PROCEDURE IsBitFieldVar(v: Variable.T): BOOLEAN;
PROCEDURE BitLocOfVar(v: Variable.T; VAR loc: MSIRBitLoc.T): BOOLEAN;
(* TRUE, with v's location in loc, if v is bound by BindVarBitField. *)

(* EndProc finalizes the current proc. If unsupported was ever
   asserted, the proc is dropped; otherwise it is appended to the
   current MSIREmit module. *)
PROCEDURE EndProc();

(* Mark the in-progress proc unsupported. EndProc will drop it. *)
PROCEDURE Abandon(reason: TEXT);

(* Lightweight proc context for generated helper procs (e.g. object type init).
   Sets curProc and curBlock so Expr.CompileMSIR works; restores on End.
   The proc and entry block must be pre-built with the raw MSIR API. *)
PROCEDURE BeginHelperProc(p: MSIR.Proc;  entry: MSIR.Block);
PROCEDURE EndHelperProc();

(* Are we currently inside a BeginProc/EndProc bracket and still
   supported? Body translators check this gate before doing anything. *)
PROCEDURE InProc(): BOOLEAN;

(* Has the current procedure been abandoned?  An abandon is final: the
   module is marked incomplete (see Abandon), so there is no clearing it. *)
PROCEDURE IsAbandoned(): BOOLEAN;

(* The heap object holding the designator a WITH alias v is bound to (see
   BindVarAddr), which a store through v must dirty; NIL if none. *)
PROCEDURE VarContainer(v: Variable.T): MSIR.Value;

(* Accessors for body translators. NIL outside a supported in-progress proc. *)
PROCEDURE CurrentProc(): MSIR.Proc;
PROCEDURE CurrentBlock(): MSIR.Block;

(*-------------------------------------------------------------- Control flow *)

(* Create a new block, add it to curProc, return it.
   Does NOT switch curBlock; use SetCurrentBlock for that. *)
PROCEDURE NewBlock(label: TEXT): MSIR.Block;

(* Switch curBlock to b.  b must belong to curProc. *)
PROCEDURE SetCurrentBlock(b: MSIR.Block);

(* TRUE if curBlock has a terminator as its last instruction. *)
PROCEDURE CurrentBlockTerminated(): BOOLEAN;

(* EH scopes (D26).  New blocks go in the current scope; TryStmt,
   TryFinStmt and LockStmt switch it around a TRY's parts and restore it. *)
PROCEDURE CurrentScope(): MSIR.Scope;        (* the proc's root if none *)
PROCEDURE SetScope(s: MSIR.Scope): MSIR.Scope;  (* returns the previous *)
PROCEDURE NewScope(kind: MSIR.ScopeKind;  parent: MSIR.Scope;
                   owner: MSIR.Scope := NIL): MSIR.Scope;

(* Branch from the current block to target, with unwind_to if that leaves
   EH scopes (D8). *)
PROCEDURE EmitGoto(target: MSIR.Block;
                   READONLY args: ARRAY OF MSIR.Value);

(* Return v (NIL for none) from the current procedure: ret, or
   ret_through_envelope inside EH scopes.  Marker.EmitReturnMSIR decides
   where a RETURN goes. *)
PROCEDURE EmitRet(v: MSIR.Value);

(*-------------------------------------------------------------- Proc registry *)

(* Register a procedure value with its MSIR.Proc so call sites can find it.
   Called from Procedure.m3 after a successful BeginProc. *)
PROCEDURE RegisterProc(v: Value.T;  p: MSIR.Proc);
(* Register v -> p in the proc map. *)

(* Look up v in the registry; if not found, build an external stub from
   procType (the m3front ProcType.T).  Returns NIL and calls Abandon if
   any parameter or result type is unsupported. *)
PROCEDURE LookupOrCreateProc(v: Value.T;  procType: Type.T): MSIR.Proc;

(* TRUE only if v is registered AND its MSIR proc already has a compiled body
   (>= 1 block).  A bodyless forward stub returns FALSE so GenBodyMSIR fills it
   in rather than skipping (used with capture pre-registration to compile a
   nested proc forward-referenced by an earlier-compiled sibling). *)
PROCEDURE ProcMapHasBody(v: Value.T): BOOLEAN;

(*------------------------------------------------------ Exception handling *)

(* Emit a call in the current block.  Calls inside EH scopes stay plain
   calls; EH lowering (MSIREH) decides which unwind (D26). *)
PROCEDURE EmitCall(name: TEXT;  callee: MSIR.Proc;
                   READONLY args: ARRAY OF MSIR.Value): MSIR.Value;

(* Like EmitCall but dispatches through a runtime function-pointer value.
   fn is the ptr-typed callee value; rtype is the return type (NIL for void).
   Emits call or invoke depending on current try context. *)
PROCEDURE EmitCallIndirect(name: TEXT;  fn: MSIR.Value;  rtype: MSIR.T;
                            READONLY args: ARRAY OF MSIR.Value): MSIR.Value;

(* Emit a runtime-fault guard.  When faultCond (an i1 value) is TRUE, branches
   to a fault block that calls RTHooks__ReportFault(@<curMod>_M3_info,
   line*32 + errCode) — which never returns — and continues in a fresh block
   otherwise.  errCode is ORD of an M3CG.RuntimeError / RuntimeError.T (< 32).
   Mirrors the C backend's <module>_CRASH(info) wrapper.  No-op outside a proc
   or if faultCond is NIL.  The ReportFault call routes through EmitCall, so it
   correctly invokes to the enclosing TRY's unwind block when inside one. *)
PROCEDURE EmitReportFault(faultCond: MSIR.Value;  errCode: INTEGER);

(* Like EmitReportFault but for <*ASSERT expr*> with a message: when faultCond is
   TRUE, calls RTHooks__AssertFailed(@<curMod>_M3_info, line, msgVal) — which
   raises RuntimeError.AssertFailed carrying msgVal as info0 so the backstop
   prints `<*ASSERT*> failed: <msg>` (ReportFault only yields the generic
   `<*ASSERT*> failed.`).  msgVal is the already-compiled TEXT message value.
   No-op outside a proc or if faultCond/msgVal is NIL. *)
PROCEDURE EmitAssertFail(faultCond: MSIR.Value;  msgVal: MSIR.Value);

(* Returns a ptr to the current module's descriptor — &@<curMod>_M3_info at
   offset 0.  Callers use this to pass the module to runtime hooks (e.g.
   RTHooks__Raise) WITHOUT importing MSIREmit directly: a direct client->MSIREmit
   import edge perturbs m3front's module-initialization order and corrupts MSIR
   emission of the first compiled unit (observed as an EndProc/BlockInsnCount
   crash on RTHooks.i3).  MSIRBuilder already imports MSIREmit, so routing
   through here is safe.  NIL if there is no current module. *)
PROCEDURE CurrentModuleInfoRef(): MSIR.Value;

(* Like EmitCall but prepends the capture arguments for a lambda-lifted nested
   proc.  caps is the callee's capture list (Procedure.Captures).  For each
   capture, passes LookupVarAddr(cap.var) from the current (outer) proc's
   varMap.  Results (large or small) are returned by value — no hidden result
   pointer. *)
PROCEDURE EmitNestedCall(name: TEXT;  callee: MSIR.Proc;
                         caps: REF ARRAY OF CaptureAnalysis.Capture;
                         READONLY args: ARRAY OF MSIR.Value): MSIR.Value;

(* Emit a virtual method dispatch on a CM3 object reference.
   obj:  the receiver (gc_ref void or ptr — first word is the vtable pointer)
   midx: vtable slot index (= Method.Info.offset / Target.Address.size)
   rtype: MSIR return type of the method (NIL for void)
   args:  explicit arguments (NOT including the implicit self/obj first arg)
   Prepends obj as the first argument. *)
PROCEDURE EmitMethodCall(name: TEXT;  obj: MSIR.Value;  midx: INTEGER;
                          rtype: MSIR.T;
                          READONLY args: ARRAY OF MSIR.Value): MSIR.Value;
(* rtype is the (by-value) return type; the result — large or small — is
   returned by value. *)

(* Like EmitCallIndirect but emits a runtime CL_marker check so the call
   works whether fn is a plain function pointer or a fat-pointer closure.
   If the first word of fn equals M3RT.CL_marker_value (-1), the closure
   path loads CL_frame (env) and CL_proc (shim) and calls shim(env, args…).
   Otherwise calls fn(args…) directly.  The result comes back by value.
   Used by indirect-call sites where CouldBeClosure is true. *)
PROCEDURE EmitClosureCall(name: TEXT;  fn: MSIR.Value;  rtype: MSIR.T;
                           READONLY args: ARRAY OF MSIR.Value): MSIR.Value;

(* Construct a stack-allocated fat-pointer closure for a nested procedure
   value.  v is the nested Procedure.T; procType is its M3 PROCEDURE type.
   Returns a ptr-typed MSIR.Value pointing to the closure struct
   {CL_marker=-1, CL_proc=shim, CL_frame=env}.
   caps is v's capture list (Procedure.Captures).
   Abandons and returns NIL if captures are unsupported. *)
PROCEDURE BuildClosureValue(v: Value.T; procType: Type.T;
                            caps: REF ARRAY OF CaptureAnalysis.Capture): MSIR.Value;

(*---------------------------------------------- RunTyme hook lookup for MSIR *)

(* Look up RunTyme hook h and return the corresponding MSIR.Proc extern stub,
   creating it if not already registered.  Returns NIL if the hook cannot be
   found or MSIR is not enabled.
   Usage in CompileMSIR methods (replaces hardcoded "RTHooks__Foo" names):
     proc := MSIRBuilder.HookProc(RunTyme.Hook.Foo);
     IF proc = NIL THEN RETURN NIL END;
     EVAL MSIRBuilder.EmitCall("", proc, args); *)
PROCEDURE HookProc (h: RunTyme.Hook): MSIR.Proc;

(* Get or create an exception descriptor global for exception value v.
   Returns a Value of ptr type pointing to the descriptor.
   The descriptor { uid, null, 0 } is registered with the current module. *)
PROCEDURE ExcDescValue (v: Value.T): MSIR.Value;
(* The module's descriptor for exception v, with its argument's type. *)
PROCEDURE ExcDescFor (v: Value.T): MSIR.ExcDesc;

(* The current procedure's RAISES clause is to be enforced, and excs are the
   exceptions it allows (MSIR.ProcSetRaises). *)
PROCEDURE SetProcRaises (READONLY excs: ARRAY OF Value.T);

(* Analog of Type.LoadInfo: create a TypeLink entry in MI_type_cell_ptrs and
   emit a load of TypeLink.defn in the current block.  After RTLinker resolves
   the TypeLink, the loaded value is the TypeCell pointer.
   A unit's own typecells are RefType.TypecellMSIR / InitTypecellMSIR and
   ObjectType.InitTypecellMSIR. *)
PROCEDURE TypeLinkValueForRef      (t: Type.T): MSIR.Value;
PROCEDURE TypeLinkValueForRefArray (t: Type.T): MSIR.Value;
PROCEDURE TypeLinkValueForObject   (t: Type.T): MSIR.Value;

(* Record a REVEAL entry (lhs=opaque UID, rhs=revealed UID) for this module.
   Called from Revelation.GenList so MSIRToLLVM can emit the full_rev array. *)
PROCEDURE AddRevelation (lhsUID, rhsUID: INTEGER);

(*----------------------------------------------- Bitfield read/write helpers *)

(* Extract bitWidth bits at bit offset bitOff from the byte array at base.
   Uses one i8 load when the field fits in a single byte, two i8 loads (stitched)
   when it spans a byte boundary.  ZExt/SExt to rawFieldType's natural M3 type. *)
PROCEDURE ExtractBitField (base: MSIR.Value;  bitOff, bitWidth: INTEGER;
                            rawFieldType: Type.T): MSIR.Value;

(* Read-modify-write: store rhs into bitWidth bits at bit offset bitOff
   in the byte array at base.  Single-byte or two-byte depending on span. *)
PROCEDURE InsertBitField (base: MSIR.Value;  bitOff, bitWidth: INTEGER;
                           rhs: MSIR.Value);

(* Dynamic (runtime index) variants for packed-element arrays.  When eltPack
   divides 8 the byte holding the element is read or written; otherwise an
   element may cross a byte, and the whole containerBits container (a multiple
   of 8, at most 64) is.  Outside that, Extract returns NIL and Insert abandons.
   idx is the element index (biased to 0); base points to the ByteArrayFallback. *)
PROCEDURE ExtractBitFieldDyn (base: MSIR.Value;  eltPack: INTEGER;
                               containerBits: INTEGER := 0;  (* total bits; 0 = power-of-2 only *)
                               idx: MSIR.Value;  rawEltType: Type.T): MSIR.Value;
PROCEDURE InsertBitFieldDyn  (base: MSIR.Value;  eltPack: INTEGER;
                               containerBits: INTEGER := 0;
                               idx: MSIR.Value;  rhs: MSIR.Value);

(* Read or write the designator at loc (static or packed-array element).
   InsertBitLoc abandons for a read-only location. *)
PROCEDURE ExtractBitLoc (READONLY loc: MSIRBitLoc.T): MSIR.Value;
PROCEDURE InsertBitLoc  (READONLY loc: MSIRBitLoc.T;  rhs: MSIR.Value);

(*------------------------------------------------------------- Module globals *)

(* A private constant global holding elts, each of type eltT, registered with
   the current module.  Returns a ptr-typed Value pointing to it. *)
PROCEDURE ConstArrayGlobal(eltT: MSIR.T;
                           READONLY elts: ARRAY OF MSIR.Value): MSIR.Value;

(* Pointer to element [0,0,...,0] of an open array = the flat data pointer.
   Emits BuildOpenArrayElemAddr with one zero index per dope rank, so it is
   correct for any rank (a bare single-zero index asserts on rank>1). *)
PROCEDURE OpenArrayDataPtr(blk: MSIR.Block;  oa: MSIR.Value): MSIR.Value;

(* Emit a memcpy(dst, src, byteCount) call in the current block.
   Uses the C library memcpy; the result ptr is discarded.
   No-op when not inside a proc or already abandoned. *)
PROCEDURE EmitMemcpy(dst, src: MSIR.Value; byteCount: INTEGER);

(* Dynamic variant: byteCount is a runtime MSIR.Value (i64). *)
PROCEDURE EmitMemcpyDyn(dst, src, byteCount: MSIR.Value);

(* Emit memset(dst, 0, byteCount) in the current block. *)
PROCEDURE EmitZeroBytes(dst: MSIR.Value;  byteCount: INTEGER);

(* A counted loop over idx in [0, count).  BeginCountedLoop branches from the
   current block, leaves the current block in the loop body and returns idx;
   EndCountedLoop closes the body and leaves the current block after the loop.
   Returns NIL, and
   EndCountedLoop does nothing, when there is no current block.  The
   counter is an alloca (MSIRToLLVM hoists allocas to the entry block, and
   does not lower block parameters), so loops nest. *)
TYPE CountedLoop = RECORD
    hdr, exit : MSIR.Block := NIL;
    idxA, idx : MSIR.Value := NIL;
  END;
PROCEDURE BeginCountedLoop(count: MSIR.Value;  VAR l: CountedLoop): MSIR.Value;
PROCEDURE EndCountedLoop(READONLY l: CountedLoop);

PROCEDURE BuildMemcmp(b: MSIR.Block;  a, bb: MSIR.Value;
                       byteCount: INTEGER): MSIR.Value;
(* memcmp(a, bb, byteCount) -> i32 (0 iff equal).  For byte-wise comparison of
   aggregate values where LLVM icmp does not apply. *)

(* If rhsVal is an OpenArray value and lhsPtr is a pointer to a FixedArray,
   emit the appropriate element copy (typed load+store for rank-1 matching
   elements, memcpy otherwise) and return TRUE.
   Returns FALSE when no special handling is needed (types compatible for
   a plain BuildStore).  lhsType is the Modula-3 type of the destination
   (used to compute the byte size for the memcpy branch). *)
PROCEDURE OpenArrayToFixedStore(lhsPtr, rhsVal: MSIR.Value;
                                lhsType: Type.T): BOOLEAN;

(* TRUE if t, a set's MSIR type, is wider than a word.  Such sets use the
   D19 set intrinsics (MSIR.BuildSetUnion etc.); a single-word set uses
   integer operations. *)
PROCEDURE IsWideSet(t: MSIR.T): BOOLEAN;

(* v, an element of a set, as the i64 bit index of a set intrinsic: v
   widened to INTEGER, less the set's first element minOrd. *)
PROCEDURE SetIndex(blk: MSIR.Block;  v: MSIR.Value;  minOrd: INTEGER): MSIR.Value;

(* Sign-aware integer extension/truncation used by arithmetic operators.
   Widens v from its current bit-width to rt's bit-width (SExt for I-kind
   signed types, ZExt for W-kind unsigned types) or truncates if narrower.
   Returns v unchanged if bit-widths are identical or either is unknown. *)
PROCEDURE CoerceToMSIR(blk: MSIR.Block;  v: MSIR.Value;  rt: MSIR.T): MSIR.Value;

(* Widen a procedure-call result from its MType (natural width, per
   MSIRType.TranslateResult) up to the ZType computation width, SExt/ZExt per the
   MType kind (the load-side of the MType<->ZType discipline for proc results).
   No-op for NIL / void / already-ZType-width results.  Call it on EVERY
   small-result call return in UserProc so callers see machine-width values —
   required for <*EXTERNAL*> C functions returning sub-word ints (p228). *)
PROCEDURE WidenCallResult(v: MSIR.Value;  resultType: Type.T): MSIR.Value;

(* OR the low bitWidth bits of `value` into the little-endian byte array
   `byteVals` starting at bit offset `bitOff`, matching InsertBitField's layout
   (byte = bit DIV 8, bit-in-byte = bit MOD 8).  Used to assemble packed
   BITS-n-FOR array/record CONSTANTS at compile time so their const-init bytes
   match what the runtime constructor path (InsertBitField) and the readers
   (ExtractBitField) expect.  Callers pre-zero byteVals and pass non-overlapping
   fields, so bits are only ever set (never cleared). *)
PROCEDURE PackConstBits(byteVals: REF ARRAY OF INTEGER;
                        bitOff, bitWidth, value: INTEGER);

(* Front-end INTEGER helpers — avoid LONGINT in m3front source files. *)
PROCEDURE ConstInt       (t: MSIR.T;  READONLY v: Target.Int): MSIR.Value;
PROCEDURE BuildPtrByteOff(b: MSIR.Block;  name: TEXT;  base: MSIR.Value;  off: INTEGER): MSIR.Value;
PROCEDURE TFixedArrayI   (len: INTEGER;  elt: MSIR.T): MSIR.T;

(* Reset the global map.  Call once at the start of each new module
   (from MSIREmit.BeginUnit). *)
PROCEDURE BeginModule();

(* Raw map-management helpers.  Variable.m3 calls these after doing its own
   type translation and condition checks. *)

(* Return rawName if unused in the current proc's varMap; otherwise return
   rawName & "." & N for the smallest N >= 1 that is free.
   M3 identifiers cannot contain '.', so any .<digits> suffix is unambiguous.
   Must only be called inside a BeginProc/EndProc bracket. *)
PROCEDURE UniqueLocalName(rawName: TEXT): TEXT;

(* Like NewBlock but creates the block with a single block parameter.
   Returns the block; the parameter value is retrievable via
   MSIR.BlockParamValue(blk, 0). *)
PROCEDURE NewBlockWithParam(label: TEXT;  paramName: TEXT;  paramType: MSIR.T): MSIR.Block;

(* Register a loop-variable as a block-parameter SSA value (no alloca).
   LookupVar(v) returns paramVal directly; LookupVarAddr abandons (loop
   variables are read-only and non-addressable). *)
PROCEDURE RegisterLoopVar(v: Variable.T;  paramVal: MSIR.Value);

PROCEDURE GlobalMapAdd(v: Variable.T;  g: MSIR.Global;  m: MSIR.Module);

(* Like GlobalMapAdd but for globals embedded in the @Mod_M3_info struct.
   Sets the global's byteOffset and refValue to a StructFieldRef GEP. *)
PROCEDURE GlobalMapAddStruct(v: Variable.T;  g: MSIR.Global;  m: MSIR.Module;
                              infoName: TEXT;  byteOff: INTEGER;
                              fieldType: MSIR.T;  needsLoad: BOOLEAN := FALSE;
                              dataType: MSIR.T := NIL);

(* Register an imported (non-external) M3 variable for access via the RT0
   import-chain mechanism.  At code-generation time, LookupVar/LookupVarAddr
   loads the II_import pointer from @<curMod>_M3_imp.k (where k is the index
   of ownerBinder in the current module's import binder list) and advances by
   varByteOff bytes to reach the variable's storage in the imported module's
   interface struct.  varMSIRType is the MSIR element type of the stored value
   (use TPtr(TVoid()) for traced GcRef types). *)
PROCEDURE GlobalMapAddImport(v: Variable.T;  m: MSIR.Module;
                              ownerBinder: TEXT;  varByteOff: INTEGER;
                              varMSIRType: MSIR.T;
                              needsLoad: BOOLEAN := FALSE;
                              dataType: MSIR.T := NIL);
PROCEDURE VarMapAdd(v: Variable.T;  val: MSIR.Value;  elt: MSIR.T);
PROCEDURE VarMapContains(v: Variable.T): BOOLEAN;

(* Start the module initialisation procedure.  Sets up curProc/curBlock
   for the init body without walking any scope (globals are already in
   the globalMap).  Returns FALSE when MSIR is not enabled. *)
PROCEDURE BeginModuleInit(name: TEXT): BOOLEAN;

(* Record the current unit's source file (from the Scanner's current position)
   as RT0.ModuleInfo.file, so the unhandled-exception backstop can print
   `file "..." line N`.  Call ONCE per unit, from Module.EmitBody after
   Scanner.offset has been set to the body origin — the MSIR analogue of the
   front-end's single CG.set_source_file call.  No-op when MSIR is disabled. *)
PROCEDURE NoteModuleSrcFile();

(* Update the MSIR current-source-line from Scanner.offset.
   Call once per statement at the top of CompileMSIR statement loops,
   after setting Scanner.offset := t.origin.  No-op outside a proc. *)
PROCEDURE GenLocation();

END MSIRBuilder.
