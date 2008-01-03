(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Jun 27 10:17:26 PDT 1994 by najork                   *)
(*       Created on Sat May 28 11:15:04 PDT 1994 by najork                   *)


INTERFACE ObMarkerTypeProp;

IMPORT MarkerTypeProp, ObLib, ObProtoLoader, ObValue, SynLocation;

PROCEDURE SetupPackage ();
PROCEDURE SetupModule (loader : ObProtoLoader.T);

PROCEDURE GetOverloadedVal (args    : ObValue.ArgArray; 
                            idx     : INTEGER; 
                            package : ObLib.T; 
                            opCode  : ObLib.OpCode; 
                            loc     : SynLocation.T) : MarkerTypeProp.Val 
    RAISES {ObValue.Error, ObValue.Exception};

END ObMarkerTypeProp.
