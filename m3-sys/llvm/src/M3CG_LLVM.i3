INTERFACE M3CG_LLVM;

IMPORT M3CG,Wr;

TYPE
  U <: Public;

  Public = M3CG.T OBJECT
  METHODS
    dumpLLVMIR();
  END;

(* returns a fresh, initialized code generator that writes its
   calls to LLVM IR generator 'wr'. *)
PROCEDURE New (wr: Wr.T): M3CG.T;

END M3CG_LLVM.
