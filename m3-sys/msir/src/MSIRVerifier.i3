(* MSIR verifier -- checks the op-level invariants the passes and
   lowerings assume (A9 in MSIR-design.md).

   Local checks: operand counts and types, branch-target arity, one
   terminator per block, TYPECASE exhaustiveness, EH scope shape (D26),
   return types, and the forms of the runtime ops (D32-D36).  Not checked:
   SSA dominance and the GC contract beyond barrier operand forms. *)

INTERFACE MSIRVerifier;

IMPORT MSIR;

(* Returns NIL on success, or a REF ARRAY OF TEXT of human-readable error
   messages on failure. Each message is prefixed with a location such as
   "@ProcName: block label: insn N".  'lowered': m has been through EH
   lowering (MSIREH), so invoke, landingpad, extractvalue and resume may
   appear; otherwise they are errors (D26). *)
PROCEDURE VerifyModule(m: MSIR.Module;  lowered := FALSE): REF ARRAY OF TEXT;
(* Read-barrier consistency audit: every traced load from non-local storage
   is gc.load or carries the front end's no-barrier decision (D30); returns
   violation descriptions, NIL if none.  Gated by the caller (MSIREmit runs
   it under @M3msirrbaudit). *)
PROCEDURE AuditReadBarriers(m: MSIR.Module): REF ARRAY OF TEXT;
PROCEDURE VerifyProc(p: MSIR.Proc;  lowered := FALSE): REF ARRAY OF TEXT;

END MSIRVerifier.
