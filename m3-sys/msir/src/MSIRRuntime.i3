(* MSIRRuntime -- the runtime ops, expanded for the lowerings.

   The ops through which MSIR states its interactions with the Modula-3
   runtime (D31): checks that may fault (D32), type tests (D33),
   allocation (D34), RAISE (D35) and method lookup (D36).  Each lowering
   runs Expand first, before EH lowering, turning them into ordinary
   compares, branches, loads and calls of the runtime procedures in the
   module's RtHook table; new blocks keep their block's EH scope, so a call
   inside a TRY becomes an invoke there.  Hooks lists every runtime
   procedure a module's lowering calls, for the front end to bind. *)

INTERFACE MSIRRuntime;

IMPORT MSIR;

PROCEDURE IsCheck (op: MSIR.Op): BOOLEAN;
(* Is op a check op (one that may fault)?  Expand replaces these and
   istype. *)

PROCEDURE Hooks (m: MSIR.Module): SET OF MSIR.RtHook;
(* Every runtime procedure m's lowering calls: those Expand calls for its
   ops, and those the lowerings call for the GC barriers, TYPECASE, EH and
   TEXT literals.  The front end binds each with MSIR.SetModuleRtHook
   before lowering. *)

PROCEDURE Expand (m: MSIR.Module);
(* Replace each check op in m (nil_check, range_check, subscript_check, shape_check, fault, narrow; istype, D33; new and openarray.new, D34; raise, D35; method, D36) by a compare and a branch to a
   fault block that calls RTHooks.ReportFault (module, line * 32 + code)
   and does not return; the rest of the check's block continues in a new
   block.  New blocks keep the original block's EH scope, so a fault inside
   a TRY is handled as any call there is.  Idempotent: run by each lowering
   before anything else. *)

END MSIRRuntime.
