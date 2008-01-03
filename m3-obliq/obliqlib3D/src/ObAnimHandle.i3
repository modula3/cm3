(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Sat Jun  4 18:10:22 PDT 1994 by najork                   *)
(*       Created on Sat May 28 20:00:54 PDT 1994 by najork                   *)


INTERFACE ObAnimHandle;

IMPORT AnimHandle, ObLib, ObProtoLoader, ObValue, SynLocation;

PROCEDURE SetupPackage ();
PROCEDURE SetupModule (loader : ObProtoLoader.T);

PROCEDURE GetT (args    : ObValue.ArgArray; 
                idx     : INTEGER; 
                package : ObLib.T; 
                opCode  : ObLib.OpCode; 
                loc     : SynLocation.T) : AnimHandle.T 
    RAISES {ObValue.Error, ObValue.Exception};

END ObAnimHandle.
