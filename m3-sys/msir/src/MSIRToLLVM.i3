(* MSIR-to-LLVM IR text emission.
   Module writes LLVM IR text compatible with LLVM 15+ opaque-pointer mode. *)

INTERFACE MSIRToLLVM;

IMPORT MSIR, Wr;

PROCEDURE Module(wr: Wr.T;  m: MSIR.Module;  forRuntime: BOOLEAN := FALSE);

END MSIRToLLVM.
