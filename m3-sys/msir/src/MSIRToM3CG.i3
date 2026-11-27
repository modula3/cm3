(* MSIRToM3CG -- lowering MSIR to M3CG (D25 step 4); see the overview in
   MSIR-design.md until this interface gets its own (planned). *)

INTERFACE MSIRToM3CG;

IMPORT MSIR, M3CG, Wr;

(* TRUE if Lower can take every part of m.  If FALSE, the caller compiles the
   unit through the front end's own M3CG stream instead (per-module
   fallback).  If why # NIL, the reasons are written there. *)
PROCEDURE Lowerable (m: MSIR.Module;  why: Wr.T := NIL): BOOLEAN;

(* Emit the whole unit m into cg, from begin_unit to end_unit.
   Requires Lowerable(m). *)
PROCEDURE Lower (m: MSIR.Module;  cg: M3CG.T);

END MSIRToM3CG.
