(* MSIREmit -- the per-unit driver of MSIR emission.

   Module.Compile brackets each unit with BeginUnit and EndUnit.  Emission
   is on when the backend mode is MSIRObj or MSIRAsm, or when the compiler
   runs with @M3m3front-msir (alongside the C backend); IsEnabled decides
   once.  When it is off every entry point is a no-op.

   Per-unit state: curModule, from BeginUnit to EndUnit (BeginUnit also
   resets MSIRBuilder's per-unit maps).  Per-run state: lastModule (taken
   once by LastModule), llOutPath (set by the driver before BeginUnit),
   driverLowers, and the names of modules already emitted.

   EndUnit binds the runtime procedures MSIRRuntime.Hooks says the module
   needs (looking a hook up records an import, as the C walk does where it
   emits a call), writes <unit>.msir, verifies the module (fatal only in
   the MSIR modes), and hands it to the driver (driverLowers) or writes its
   LLVM IR.  It skips the .ll of an interface whose module was already
   emitted.  Front-end modules may use CurrentModule () to add globals and
   descriptors, but only MSIRBuilder.EndProc adds procedures. *)

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

(* The driver lowers each unit's module itself (MSIR -> M3CG, D25): EndUnit
   then leaves a verified module unchanged for LastModule and writes no
   LLVM IR for it, since MSIRToLLVM rewrites a module's EH scopes in place. *)
PROCEDURE SetDriverLowers(on: BOOLEAN);

(* The module of the unit EndUnit last finished, if it passed the verifier;
   else NIL.  Clears it, so each unit's module is taken at most once.  Used
   by the cm3 driver to lower the unit through MSIR -> M3CG (D25). *)
PROCEDURE LastModule(): MSIR.Module;


END MSIREmit.
