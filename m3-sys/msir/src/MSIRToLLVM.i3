(* MSIRToLLVM -- lowering of an MSIR module to LLVM IR text (LLVM 15+,
   opaque pointers).

   Pipeline: front end -> MSIR (verified) -> MSIRRuntime.Expand (D31-D36:
   the runtime ops become compares, branches and calls of RtHook
   procedures) -> MSIREH.LowerModule (D26: EH scopes become invoke,
   landingpad and resume, a catch-all on _ZTI6_M3Exc) -> this module.
   Module runs both passes itself; they are idempotent.

   What it emits, in order: the header (triple and data layout from the
   module, O19); the ABI guard m3_abi_msir_v1 (forRuntime only); the C++ EH
   typeinfo for _M3Exc; the barrier declares; globals; extern declares; one
   define per proc, each with the __gxx_personality_v0 personality; TEXT
   literals; constant arrays; RT0 typecells and link procedures; type
   links; exception descriptors; the binder @<Mod>_M3 / _I3 and the RT0
   ModuleInfo with its import chain, gc map, proc_info and full_rev; the
   constant-global constructor; TYPECASE tables; gc.load_block maps; DWARF
   metadata.

   Design decisions it implements: D30 (gc.load, gc.store, gc.dirty and
   gc.load_block expand inline to the CM3 read and write barriers); D24 (RT0
   layouts come from M3RT; CheckRTLayouts checks the LLVM field tables
   against it once per run); D19 (sets as iN); D13/O3 (open arrays as
   { ptr, i64 x rank }); D12 (traced refs are plain ptr).

   The caller must provide:
   - every hook in MSIRRuntime.Hooks (m), bound with MSIR.SetModuleRtHook;
     Module stops, naming the hook, if one is missing (MSIRRuntime.Unbound);
   - forRuntime = TRUE when linking against the real CM3 runtime (type
     links only, RTLinker resolves them, the constant-global constructor
     runs).  forRuntime = FALSE is the standalone test-harness mode: it adds
     an MSIR_InitTypeLinks constructor that assigns typecodes 1..n per
     module, so only one such module can be linked.

   Only for ex_stack targets (O21 lists what 32-bit would take).  Not
   reentrant: it keeps module-level state. *)

INTERFACE MSIRToLLVM;

IMPORT MSIR, Wr;

PROCEDURE Module(wr: Wr.T;  m: MSIR.Module;  forRuntime: BOOLEAN := FALSE);

END MSIRToLLVM.
