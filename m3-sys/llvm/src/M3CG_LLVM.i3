INTERFACE M3CG_LLVM;

IMPORT M3CG,Wr;

TYPE
  U <: Public;

  Public = M3CG.T OBJECT
  METHODS
    dumpLLVMIR(BitcodeFileName, AsmFileName: TEXT);
  END;

(* returns a fresh, initialized code generator that writes LLVM IR
   as bitcode to 'wr_bin' and as LLVM assembly to 'wr_char'. 
   Either may be NIL. *)
PROCEDURE New (wr: Wr.T): M3CG.T;

END M3CG_LLVM.
