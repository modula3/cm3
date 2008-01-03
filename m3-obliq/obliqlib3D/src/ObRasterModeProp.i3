(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Thu Nov 10 13:25:07 PST 1994 by najork                   *)
(*       Created on Sat May 28 11:15:04 PDT 1994 by najork                   *)


INTERFACE ObRasterModeProp;

IMPORT ObLib, ObProtoLoader, ObValue, RasterModeProp, SynLocation;

PROCEDURE SetupPackage ();
PROCEDURE SetupModule (loader : ObProtoLoader.T);

PROCEDURE GetOverloadedVal (args    : ObValue.ArgArray; 
                            idx     : INTEGER; 
                            package : ObLib.T; 
                            opCode  : ObLib.OpCode; 
                            loc     : SynLocation.T) : RasterModeProp.Val 
    RAISES {ObValue.Error, ObValue.Exception};

END ObRasterModeProp.
