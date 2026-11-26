(* MSIRChecks -- expansion of the check ops (D32) for the lowerings. *)

INTERFACE MSIRChecks;

IMPORT MSIR;

PROCEDURE IsCheck (op: MSIR.Op): BOOLEAN;
(* Is op a check op that Expand replaces? *)

PROCEDURE Expand (m: MSIR.Module);
(* Replace each check op in m (nil_check, range_check) by a compare and a branch to a
   fault block that calls RTHooks.ReportFault (module, line * 32 + code)
   and does not return; the rest of the check's block continues in a new
   block.  New blocks keep the original block's EH scope, so a fault inside
   a TRY is handled as any call there is.  Idempotent: run by each lowering
   before anything else. *)

END MSIRChecks.
