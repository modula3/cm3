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
   isInterface=TRUE when compiling a .i3 file.  No-op if not enabled. *)
PROCEDURE BeginUnit(name: M3ID.T;  isInterface: BOOLEAN := FALSE);

(* Add a finished MSIR.Proc to the current module. Called by
   MSIRBuilder.EndProc when a proc was successfully translated. *)
PROCEDURE AddProc(p: MSIR.Proc);

(* Register an imported module binder so the emitter can build the
   RT0.ImportInfo chain in the module descriptor.  binder is the
   mangled binder name, e.g. "Fmt_M3" or "IO_M3". *)
PROCEDURE RegisterImport(binder: TEXT);

(* Record that a proc was skipped (BeginProc returned FALSE or Abandon
   was called). Appended as a comment in the MSIR output. *)
PROCEDURE NoteSkipped(procName: TEXT;  reason: TEXT);

(* Override the output path for the .ll file.  Used by the MSIRObj/MSIRAsm
   backend modes in Builder.m3 to redirect LLVM IR output to the path that
   compile_llvm expects (e.g. Foo.mb rather than Foo.ll).
   Must be called before BeginUnit.  NIL resets to the default (<name>.ll). *)
PROCEDURE SetLLOutPath(path: TEXT);

(* Finalize the unit: write the assembled MSIR.Module to
   <unit-name>.msir and the LLVM IR to the path set by SetLLOutPath
   (or <unit-name>.ll by default), then clear state.
   No-op if not enabled. *)
PROCEDURE EndUnit();

(* Return TRUE if a MODULE (non-interface) with this name has already been
   emitted to a .ll file this run.  Used by DeclareGlobalsMSIR to decide
   whether an exported interface variable should be owned by the
   implementation module or accessed via the import chain. *)
PROCEDURE ModuleEmitted(name: TEXT): BOOLEAN;

END MSIREmit.
