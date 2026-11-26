(* MSIR verifier — checks op-level invariants the optimizer assumes.

   Per A9 in MSIR-design.md. The v0 verifier covers local checks: operand
   counts, operand types, branch target arity, block terminator placement,
   typecase exhaustiveness, try envelope shape, ret value type. SSA
   dominance, cross-envelope control rules, and detailed GC-contract
   checks come later. *)

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
