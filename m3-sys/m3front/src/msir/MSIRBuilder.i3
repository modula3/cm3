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

IMPORT MSIR, M3ID, Type, Value, Scope, Variable;

(* BeginProc: create a fresh MSIR.Proc with the given name and signature.
   formals is the head of the formal-list returned by ProcType.Formals.
   syms is the procedure's local scope (used to resolve VarExpr
   references back to MSIR Param values).
   Returns FALSE if the signature is outside the supported subset
   (e.g. has an unsupported parameter or result type); the caller should
   then suppress all subsequent MSIR calls for this procedure. *)
PROCEDURE BeginProc(name: M3ID.T;
                    formals: Value.T;
                    syms: Scope.T;
                    result: Type.T;
                    isExternal: BOOLEAN): BOOLEAN;

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

(* EndProc finalizes the current proc. If unsupported was ever
   asserted, the proc is dropped; otherwise it is appended to the
   current MSIREmit module. *)
PROCEDURE EndProc();

(* Mark the in-progress proc unsupported. EndProc will drop it. *)
PROCEDURE Abandon(reason: TEXT);

(* Are we currently inside a BeginProc/EndProc bracket and still
   supported? Body translators check this gate before doing anything. *)
PROCEDURE InProc(): BOOLEAN;

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
PROCEDURE RegisterProc(v: Value.T;  p: MSIR.Proc);

(* Look up v in the registry; if not found, build an external stub from
   procType (the m3front ProcType.T).  Returns NIL and calls Abandon if
   any parameter or result type is unsupported. *)
PROCEDURE LookupOrCreateProc(v: Value.T;  procType: Type.T): MSIR.Proc;

END MSIRBuilder.
