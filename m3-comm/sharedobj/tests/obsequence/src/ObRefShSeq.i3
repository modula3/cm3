INTERFACE ObRefShSeq;

IMPORT ObLib, SynLocation, ObValue, ObLoader;
IMPORT RefShSeq;

PROCEDURE SetupPackage ();
PROCEDURE SetupModule (loader : ObLoader.T);

PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : RefShSeq.T 
    RAISES {ObValue.Error, ObValue.Exception};

END ObRefShSeq.
