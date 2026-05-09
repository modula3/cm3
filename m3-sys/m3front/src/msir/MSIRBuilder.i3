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

(* Resolve a VarExpr's target Variable.T to the MSIR Value that
   represents it in the current proc. Returns NIL if not bound
   (e.g. a local that's not yet supported); callers should treat
   NIL as a signal to call Abandon. *)
PROCEDURE LookupVar(v: Variable.T): MSIR.Value;

(* EndProc finalizes the current proc. If unsupported was ever
   asserted, the proc is dropped; otherwise it is appended to the
   current MSIREmit module. *)
PROCEDURE EndProc();

(* Mark the in-progress proc unsupported. EndProc will drop it. *)
PROCEDURE Abandon(reason: TEXT);

(* Are we currently inside a BeginProc/EndProc bracket and still
   supported? Body translators check this gate before doing anything. *)
PROCEDURE InProc(): BOOLEAN;

(* Accessors for body translators (next plan step). NIL outside a
   supported in-progress proc. *)
PROCEDURE CurrentProc(): MSIR.Proc;
PROCEDURE CurrentBlock(): MSIR.Block;

END MSIRBuilder.
