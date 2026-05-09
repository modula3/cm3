(* MSIREmit — module/unit-level driver for MSIR emission.

   Bracketed by Module.Compile (the unit-level entry in m3front).
   Honors a runtime opt-in: emission is active only when the
   environment variable M3FRONT_MSIR is set (checked once at
   process startup via RTParams). When inactive, all entry points
   are no-ops so this can ship in a production compiler with zero
   user-visible effect. *)

INTERFACE MSIREmit;

IMPORT MSIR, M3ID;

PROCEDURE IsEnabled(): BOOLEAN;

(* Return the current MSIR.Module being assembled.  NIL outside a
   BeginUnit/EndUnit bracket or when not enabled. *)
PROCEDURE CurrentModule(): MSIR.Module;

(* Begin a new compilation unit. Creates a fresh MSIR.Module.
   No-op if not enabled. *)
PROCEDURE BeginUnit(name: M3ID.T);

(* Add a finished MSIR.Proc to the current module. Called by
   MSIRBuilder.EndProc when a proc was successfully translated. *)
PROCEDURE AddProc(p: MSIR.Proc);

(* Record that a proc was skipped (BeginProc returned FALSE or Abandon
   was called). Appended as a comment in the MSIR output. *)
PROCEDURE NoteSkipped(procName: TEXT;  reason: TEXT);

(* Finalize the unit: write the assembled MSIR.Module to
   <unit-name>.msir in the current working directory and clear
   state. No-op if not enabled. *)
PROCEDURE EndUnit();

END MSIREmit.
