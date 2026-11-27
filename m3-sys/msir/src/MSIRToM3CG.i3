(* MSIRToM3CG -- lowering a whole MSIR unit to M3CG (D25 step 4).

   Pipeline: m3front -> MSIR -> MSIRRuntime.Expand -> MSIRToM3CG -> an M3CG
   backend.  The M3CG counterpart of MSIRToLLVM.  M3Backend uses it in C
   backend mode with @M3m3front-msir and @M3msircg: the front end's own
   M3CG stream is recorded (M3CG_MultiPass), and at Close the unit is either
   lowered from MSIR or the recording is replayed (the per-module
   fallback).

   Lowerable checks first that the target is 64-bit and that every runtime
   procedure the module needs is bound (MSIRRuntime.Unbound), then runs
   MSIRRuntime.Expand on m, which changes m in place, lays out the unit's
   segments (Setup) and returns FALSE if any part of m is outside what
   Lower handles: an op it does not lower, a type with no M3CG
   representation, an initializer or global it cannot lay out, or a module
   MSIRBuilder marked incomplete.  The unit then falls back to the front
   end's own M3CG stream.

   The output follows CG's data conventions, so lowered and replayed units
   link together.  One data segment (M_/I_<Mod>) holds the ModuleInfo, the
   user globals at the front end's offsets, imports, typecells, type links,
   revelations, exception descriptors and TYPECASE tables; one constant
   segment holds names, ProcInfo, gc and type maps, brands, method tables,
   TEXT literals, constant arrays and ex_frame handler lists.  SSA values
   live in M3CG locals; integers are held sign-extended to 64 bits, and
   aggregates as addresses of temporaries.

   EH (D26 scopes, lowered here rather than by MSIREH):
   - ex_stack (Target.Has_stack_walker): start_try/end_try around a
     protected scope's blocks; calls become invoke_* naming its
     landing_pad.
   - ex_frame: an EF1 frame linked with RTHooks.PushEFrame plus setjmp on an
     alloca'd jmp_buf at scope entry, PopEFrame on normal exit, and a RAISES
     frame per procedure that checks its RAISES clause.
   Both share handler dispatch by uid and the FINALLY selector, whose
   outcomes are planned before emission (PlanExits).

   The caller binds every hook in MSIRRuntime.Hooks (m) with
   MSIR.SetModuleRtHook before calling Lowerable. *)

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
