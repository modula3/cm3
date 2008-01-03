INTERFACE ObRefShSeqCB;

IMPORT ObLib, SynLocation, ObValue, ObLoader;
IMPORT RefShSeqCB; 

PROCEDURE SetupPackage ();
PROCEDURE SetupModule (loader : ObLoader.T);

PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : RefShSeqCB.T 
    RAISES {ObValue.Error, ObValue.Exception};

END ObRefShSeqCB.
