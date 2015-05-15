INTERFACE M3CG_LLVM;

IMPORT M3CG,Wr;


TYPE U <: Public;
TYPE Public = M3CG.T OBJECT
METHODS
  dumpLLVMIR();
END;


PROCEDURE New (wr: Wr.T): M3CG.T;
(* returns a fresh, initialized code generator that writes its
   calls as calls to LLVM IR generator 'wr'. *)

END M3CG_LLVM.
