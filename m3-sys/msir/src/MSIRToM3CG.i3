(* MSIRToM3CG -- lowering MSIR to M3CG (D25 step 4).

   Spike stage (D25 step 2): only a census.  For each procedure of a module,
   Census reports whether a straightforward lowering of its body to M3CG
   could handle it, and if not, which features block it. *)

INTERFACE MSIRToM3CG;

IMPORT MSIR, M3CG, Wr;

(* Write one line per procedure of m to wr:
     msir-census: <module> <proc> ok
     msir-census: <module> <proc> blk <feature> <feature> ...
   Features are deduplicated per procedure. *)
PROCEDURE Census (wr: Wr.T;  m: MSIR.Module);

(* TRUE if Lower can take every part of m.  If FALSE, the caller compiles the
   unit through the front end's own M3CG stream instead (per-module
   fallback).  If why # NIL, the reasons are written there. *)
PROCEDURE Lowerable (m: MSIR.Module;  why: Wr.T := NIL): BOOLEAN;

(* Emit the whole unit m into cg, from begin_unit to end_unit.
   Requires Lowerable(m). *)
PROCEDURE Lower (m: MSIR.Module;  cg: M3CG.T);

END MSIRToM3CG.
