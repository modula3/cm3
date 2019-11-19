INTERFACE M3CG_LLVM;

IMPORT M3CG,Wr;

TYPE
  U <: Public;

  Public = M3CG.T OBJECT
  METHODS
    dumpLLVMIR(BitcodeFileName, AsmFileName: TEXT);
  END;

TYPE m3llvmDebugLevTyp = [ 0 .. 5 ]; (* Just leave some space here. *) 

(* returns a fresh, initialized code generator that writes LLVM IR
   as bitcode to 'wr_bin' and as LLVM assembly to 'wr_char'. 
   Either may be NIL. *)
PROCEDURE New 
  (output: Wr.T; targetTriple,dataRep : TEXT; m3llvmDebugLev: m3llvmDebugLevTyp; genDebug: BOOLEAN)
: M3CG.T; 

END M3CG_LLVM.
