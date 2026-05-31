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

IMPORT MSIR, Expr, Type, Value, Scope, Variable, RunTyme, CaptureAnalysis, Target;

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
   The caller must also pass captures to RegisterProc so call sites can
   reconstruct the capture args. *)

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
PROCEDURE BindVarAddr(v: Variable.T; addr: MSIR.Value; elemType: MSIR.T);

(* EndProc finalizes the current proc. If unsupported was ever
   asserted, the proc is dropped; otherwise it is appended to the
   current MSIREmit module. *)
PROCEDURE EndProc();

(* Mark the in-progress proc unsupported. EndProc will drop it. *)
PROCEDURE Abandon(reason: TEXT);

(* Are we currently inside a BeginProc/EndProc bracket and still
   supported? Body translators check this gate before doing anything. *)
PROCEDURE InProc(): BOOLEAN;

(* Query/clear the abandoned flag.  Used by SubscriptExpr.LValueMSIR to
   attempt LValueMSIR on a base expression, then fall back to rvalue
   materialization if LValueMSIR abandoned.  ClearAbandoned must only be
   called when the caller knows it can handle the failure itself. *)
PROCEDURE IsAbandoned(): BOOLEAN;
PROCEDURE ClearAbandoned();

(* GC write barrier container side-channel.
   LValueMSIR for heap object fields calls SetPendingContainer with the
   object pointer (the GC container) before returning the slot address.
   AssignStmt.CompileMSIR calls TakePendingContainer to retrieve it —
   returns NIL (and clears) if not set (e.g. for globals or locals). *)
PROCEDURE SetPendingContainer(v: MSIR.Value);
PROCEDURE TakePendingContainer(): MSIR.Value;

(* Accessors for body translators. NIL outside a supported in-progress proc. *)
PROCEDURE CurrentProc(): MSIR.Proc;
PROCEDURE CurrentBlock(): MSIR.Block;

(* Hidden result pointer for large-result procs (records, arrays, large sets).
   Non-NIL while inside a proc whose M3 result type satisfies ProcType.LargeResult.
   ReturnStmt.CompileMSIR stores through this pointer and emits ret void.
   CurrentResultType() is the non-void MSIR type of the result (same as what
   the proc would have returned before the hidden-ptr convention was applied). *)
PROCEDURE CurrentResultPtr(): MSIR.Value;
PROCEDURE CurrentResultType(): MSIR.T;

(*-------------------------------------------------------------- Control flow *)

(* Create a new block, add it to curProc, return it.
   Does NOT switch curBlock; use SetCurrentBlock for that. *)
PROCEDURE NewBlock(label: TEXT): MSIR.Block;

(* Switch curBlock to b.  b must belong to curProc. *)
PROCEDURE SetCurrentBlock(b: MSIR.Block);

(* TRUE if curBlock has a terminator as its last instruction. *)
PROCEDURE CurrentBlockTerminated(): BOOLEAN;

(* Loop-exit block stack for EXIT statement translation. *)
PROCEDURE PushExitBlock(b: MSIR.Block);
PROCEDURE PopExitBlock();
PROCEDURE CurrentExitBlock(): MSIR.Block;  (* NIL if not inside a loop *)

(* TRY/FINALLY selector codes (stored in the per-finally i32 selector alloca):
   why the finally body was entered, so its epilogue can dispatch. *)
CONST Sel_Normal = 0;  Sel_Exc = 1;  Sel_Exit = 2;

(* TRY/FINALLY registers a Finally cleanup frame around its body so a non-local
   EXIT in the body runs the finally first.  finBody is the shared finally-body
   entry; selector is an i32 alloca the EXIT stores Sel_Exit into.  Push around
   the body only (pop before compiling the finally itself). *)
PROCEDURE PushFinallyCleanup(finBody: MSIR.Block;  selector: MSIR.Value);
PROCEDURE PopFinallyCleanup();

(* Emit an EXIT: branch to the innermost loop's exit block, running every
   intervening finally first.  Used by ExitStmt and by a finally epilogue
   continuing an EXIT that passed through it. *)
PROCEDURE EmitExitMSIR();

(* TRUE if an EXIT routed through the innermost (current) finally cleanup frame.
   TryFinStmt calls this — while its Finally frame is still on top — to decide
   whether to emit the Sel_Exit dispatch arm in the finally epilogue.  Avoids an
   EmitExitMSIR with no loop target for finallys that contain no through-EXIT. *)
PROCEDURE CurrentFinallyExitSeen(): BOOLEAN;

(*-------------------------------------------------------------- Proc registry *)

(* Register a procedure value with its MSIR.Proc so call sites can find it.
   Called from Procedure.m3 after a successful BeginProc. *)
PROCEDURE RegisterProc(v: Value.T;  p: MSIR.Proc;
                       caps: REF ARRAY OF CaptureAnalysis.Capture := NIL);
(* Register v → p in the proc map.  caps is the capture list from
   CaptureAnalysis.GetCaptures; NIL for non-nested procs. *)

PROCEDURE GetProcCaptures(v: Value.T): REF ARRAY OF CaptureAnalysis.Capture;
(* Return the capture list stored by RegisterProc for v; NIL if not nested
   or not yet registered.  Used by EmitNestedCall and call sites to build
   the capture argument list. *)

(* Look up v in the registry; if not found, build an external stub from
   procType (the m3front ProcType.T).  Returns NIL and calls Abandon if
   any parameter or result type is unsupported. *)
PROCEDURE LookupOrCreateProc(v: Value.T;  procType: Type.T): MSIR.Proc;

(* TRUE if v is already registered in the proc map.
   Used to skip re-emitting MSIR for nested procs compiled inline via GenBodyMSIR. *)
PROCEDURE ProcMapContains(v: Value.T): BOOLEAN;

(*------------------------------------------------------ Exception handling *)

(* Push/pop a try context.  While a try context is active, EmitCall emits
   `invoke` (routing the unwind path to lpadBlock) rather than `call`. *)
PROCEDURE PushTryContext(lpadBlock: MSIR.Block);
PROCEDURE PopTryContext();
PROCEDURE CurrentUnwindBlock(): MSIR.Block;  (* NIL if not in a try *)

(* Smart call emitter.  If inside a try context, creates a normal-continuation
   block, emits `invoke callee(args) to label %normal unwind label %lpad`,
   then switches curBlock to the continuation block.
   Otherwise, emits a plain `call`. *)
PROCEDURE EmitCall(name: TEXT;  callee: MSIR.Proc;
                   READONLY args: ARRAY OF MSIR.Value): MSIR.Value;

(* Like EmitCall but dispatches through a runtime function-pointer value.
   fn is the ptr-typed callee value; rtype is the return type (NIL for void).
   Emits call or invoke depending on current try context. *)
PROCEDURE EmitCallIndirect(name: TEXT;  fn: MSIR.Value;  rtype: MSIR.T;
                            READONLY args: ARRAY OF MSIR.Value): MSIR.Value;

(* Like EmitCall but prepends the capture arguments for a lambda-lifted nested
   proc.  calleeVal is the Value.T for the nested proc (used to look up the
   capture list registered by RegisterProc).  For each capture, passes
   LookupVarAddr(cap.var) from the current (outer) proc's varMap.
   resultPtr: for large-result procs, the hidden result-slot pointer to place
   at arg index 0, before the capture args; pass NIL for normal-result procs. *)
PROCEDURE EmitNestedCall(name: TEXT;  callee: MSIR.Proc;  calleeVal: Value.T;
                         resultPtr: MSIR.Value;
                         READONLY args: ARRAY OF MSIR.Value): MSIR.Value;

(* Emit a virtual method dispatch on a CM3 object reference.
   obj:  the receiver (gc_ref void or ptr — first word is the vtable pointer)
   midx: vtable slot index (= Method.Info.offset / Target.Address.size)
   rtype: MSIR return type of the method (NIL for void)
   args:  explicit arguments (NOT including the implicit self/obj first arg)
   Prepends obj as the first argument.  Uses invoke inside a TRY context. *)
(* Itanium C++ ABI helpers needed around CATCH landingpads.
   CxaBeginCatch(exc_header) -> exc_obj  — converts exception-header ptr
     (field 0 of the landingpad result) to the actual thrown exception object.
   CxaEndCatch()             — releases the exception; must be called once
     per CxaBeginCatch, before every exit from the handler (including resume). *)
PROCEDURE CxaBeginCatch      (): MSIR.Proc;
PROCEDURE CxaEndCatch        (): MSIR.Proc;
PROCEDURE CxaRethrow         (): MSIR.Proc;
  (* __cxa_rethrow(): re-throw the exception currently being handled (after a
     CxaBeginCatch).  noreturn — unwinds to the enclosing handler.  Used to
     implement TRY/FINALLY: catch-all, run the finally, then rethrow. *)
PROCEDURE CxaGetExceptionPtr (): MSIR.Proc;
  (* __cxa_get_exception_ptr(ptr) -> ptr: peek at the exception object
     WITHOUT acquiring ownership — no matching __cxa_end_catch needed. *)

(* Catch-handler context stack.  Push before compiling a handler body so that
   ReturnStmt.CompileMSIR can emit __cxa_end_catch before any ret inside the
   handler.  Pop after the handler body is compiled. *)
PROCEDURE PushCatchContext (endCatch: MSIR.Proc);
PROCEDURE PopCatchContext  ();
PROCEDURE CurrentCatchEndProc (): MSIR.Proc;  (* NIL when not in a handler *)

PROCEDURE EmitMethodCall(name: TEXT;  obj: MSIR.Value;  midx: INTEGER;
                          rtype: MSIR.T;  resultSlot: MSIR.Value;
                          READONLY args: ARRAY OF MSIR.Value): MSIR.Value;
(* rtype is the return type for small-result calls (resultSlot = NIL).
   For large-result (struct) calls, pass resultSlot = the alloca that receives
   the result; rtype is ignored and the call uses void return with the hidden
   result pointer prepended as arg[0] before obj. *)

(* Like EmitCallIndirect but emits a runtime CL_marker check so the call
   works whether fn is a plain function pointer or a fat-pointer closure.
   If the first word of fn equals M3RT.CL_marker_value (-1), the closure
   path loads CL_frame (env) and CL_proc (shim) and calls shim(env, args…).
   Otherwise calls fn(args…) directly.
   Used by indirect-call sites where CouldBeClosure is true. *)
PROCEDURE EmitClosureCall(name: TEXT;  fn: MSIR.Value;  rtype: MSIR.T;
                           READONLY args: ARRAY OF MSIR.Value): MSIR.Value;

(* Construct a stack-allocated fat-pointer closure for a nested procedure
   value.  v is the nested Procedure.T; procType is its M3 PROCEDURE type.
   Returns a ptr-typed MSIR.Value pointing to the closure struct
   {CL_marker=-1, CL_proc=shim, CL_frame=env}.
   Abandons and returns NIL if captures are unsupported. *)
PROCEDURE BuildClosureValue(v: Value.T; procType: Type.T): MSIR.Value;

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

(* Look up or create a TypeCell descriptor for a REF type.
   Used by GenRefMSIR; handles both locally-declared and imported ref types. *)
PROCEDURE TypeDescValueForRef(t: Type.T;  dataSize: INTEGER;
                               dataAlignment: INTEGER;
                               isTraced: BOOLEAN): MSIR.Value;

(* Look up or create an Array TypeCell descriptor for a REF ARRAY OF T type.
   dopeSize is the dope-vector byte size; nDimensions/elementSize match ATC fields. *)
PROCEDURE TypeDescValueForRefArray(t: Type.T;  dopeSize: INTEGER;
                                    dataAlignment: INTEGER;
                                    nDimensions: INTEGER;
                                    elementSize: INTEGER;
                                    isTraced: BOOLEAN): MSIR.Value;

(* Return a forward reference to the ObjectTypeCell for t.
   The TypeCell global (@tc_obj_<uid>) is defined later by compileMSIR.
   Used by GenObjectMSIR before GenLinkerInfoMSIR has run. *)
PROCEDURE ObjectTypeCellRef(t: Type.T): MSIR.Value;

(* Return a forward reference to the ArrayTypeCell for a REF ARRAY type.
   The @tc_arr_<uid> global is defined by InitTypecellMSIR via Type.GenCells. *)
PROCEDURE ArrayTypeCellRef(t: Type.T): MSIR.Value;

(* Analog of Type.LoadInfo: create a TypeLink entry in MI_type_cell_ptrs and
   emit a load of TypeLink.defn in the current block.  After RTLinker resolves
   the TypeLink, the loaded value is the TypeCell pointer.
   Use these at NEW call sites instead of TypeDescValueForRef* or TypeCellRef. *)
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

(* Dynamic (runtime index) variants for packed-element arrays.
   eltPack must divide 8 (i.e. eltPack ∈ {1,2,4}); otherwise Abandon is called.
   idx is the element index (biased to 0); base points to the ByteArrayFallback. *)
PROCEDURE ExtractBitFieldDyn (base: MSIR.Value;  eltPack: INTEGER;
                               idx: MSIR.Value;  rawEltType: Type.T): MSIR.Value;
PROCEDURE InsertBitFieldDyn  (base: MSIR.Value;  eltPack: INTEGER;
                               idx: MSIR.Value;  rhs: MSIR.Value);

(*------------------------------------------------------------- Module globals *)

(* Materialise a CONST ARRAY OF T expression as a private constant LLVM global.
   constExpr is the underlying ArrayExpr (or a ConsExpr wrapping one); m3Val
   is the Value.T used as a de-dup key so the same CONST produces one global.
   Returns a ptr-typed Value that points to the global — suitable as the LValue
   for a subscript expression.  Abandons and returns NIL on unsupported types. *)
PROCEDURE MaterializeConstArray(m3Val: Value.T; constExpr: Expr.T): MSIR.Value;

(* Emit a memcpy(dst, src, byteCount) call in the current block.
   Uses the C library memcpy; the result ptr is discarded.
   No-op when not inside a proc or already abandoned. *)
PROCEDURE EmitMemcpy(dst, src: MSIR.Value; byteCount: INTEGER);

(* Dynamic variant: byteCount is a runtime MSIR.Value (i64). *)
PROCEDURE EmitMemcpyDyn(dst, src, byteCount: MSIR.Value);

(* If rhsVal is an OpenArray value and lhsPtr is a pointer to a FixedArray,
   emit the appropriate element copy (typed load+store for rank-1 matching
   elements, memcpy otherwise) and return TRUE.
   Returns FALSE when no special handling is needed (types compatible for
   a plain BuildStore).  lhsType is the Modula-3 type of the destination
   (used to compute the byte size for the memcpy branch). *)
PROCEDURE OpenArrayToFixedStore(lhsPtr, rhsVal: MSIR.Value;
                                lhsType: Type.T): BOOLEAN;

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

(* Update the MSIR current-source-line from Scanner.offset.
   Call once per statement at the top of CompileMSIR statement loops,
   after setting Scanner.offset := t.origin.  No-op outside a proc. *)
PROCEDURE GenLocation();

END MSIRBuilder.
