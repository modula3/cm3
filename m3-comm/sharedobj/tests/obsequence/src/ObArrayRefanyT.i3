INTERFACE ObArrayRefanyT;

IMPORT ObLib, ObValue, SynLocation, ObLoader;
IMPORT Refany;

TYPE T <: ObValue.Val;

PROCEDURE SetupPackage ();
PROCEDURE SetupModule (loader: ObLoader.T);

PROCEDURE M3ToObliq (READONLY val : ARRAY OF Refany.T) : T;
PROCEDURE ObliqToM3 (val : T) : ARRAY OF Refany.T RAISES {ObValue.Error};

PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : ARRAY OF Refany.T
  RAISES {ObValue.Error};

END ObArrayRefanyT. 
