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

IMPORT MSIR, Type, Value, Scope, Variable, RunTyme, CaptureAnalysis;

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

(* Like EmitCall but prepends the capture arguments for a lambda-lifted nested
   proc.  calleeVal is the Value.T for the nested proc (used to look up the
   capture list registered by RegisterProc).  For each capture, passes
   LookupVarAddr(cap.var) from the current (outer) proc's varMap. *)
PROCEDURE EmitNestedCall(name: TEXT;  callee: MSIR.Proc;  calleeVal: Value.T;
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
PROCEDURE CxaGetExceptionPtr (): MSIR.Proc;
  (* __cxa_get_exception_ptr(ptr) -> ptr: peek at the exception object
     WITHOUT acquiring ownership — no matching __cxa_end_catch needed. *)

(* Catch-handler context stack.  Push before compiling a handler body so that
   ReturnStmt.CompileMSIR can emit __cxa_end_catch before any ret inside the
   handler.  Pop after the handler body is compiled. *)
PROCEDURE PushCatchContext (endCatch: MSIR.Proc);
PROCEDURE PopCatchContext  ();
PROCEDURE CurrentCatchEndProc (): MSIR.Proc;  (* NIL when not in a handler *)

PROCEDURE EmitMethodCall(name: TEXT;  obj: MSIR.Value;  midx: LONGINT;
                          rtype: MSIR.T;
                          READONLY args: ARRAY OF MSIR.Value): MSIR.Value;

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

(*------------------------------------------------------------- Module globals *)

(* Reset the global map.  Call once at the start of each new module
   (from MSIREmit.BeginUnit). *)
PROCEDURE BeginModule();

(* Raw map-management helpers.  Variable.m3 calls these after doing its own
   type translation and condition checks. *)

PROCEDURE GlobalMapAdd(v: Variable.T;  g: MSIR.Global;  m: MSIR.Module);

(* Like GlobalMapAdd but for globals embedded in the @Mod_M3_info struct.
   Sets the global's byteOffset and refValue to a StructFieldRef GEP. *)
PROCEDURE GlobalMapAddStruct(v: Variable.T;  g: MSIR.Global;  m: MSIR.Module;
                              infoName: TEXT;  byteOff: INTEGER;
                              fieldType: MSIR.T);
PROCEDURE VarMapAdd(v: Variable.T;  val: MSIR.Value;  elt: MSIR.T);
PROCEDURE VarMapContains(v: Variable.T): BOOLEAN;

(* Start the module initialisation procedure.  Sets up curProc/curBlock
   for the init body without walking any scope (globals are already in
   the globalMap).  Returns FALSE when MSIR is not enabled. *)
PROCEDURE BeginModuleInit(name: TEXT): BOOLEAN;

END MSIRBuilder.
