(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Thu Jul 21 09:51:51 PDT 1994 by najork                   *)
(*       Created on Sat Mar  5 20:12:26 PST 1994 by najork                   *)


INTERFACE ObCameraGO;

IMPORT CameraGO, ObGO, ObLib, ObProtoLoader, ObValue, SynLocation;

PROCEDURE SetupPackage ();
PROCEDURE SetupModule (loader : ObProtoLoader.T);


TYPE T <: ObGO.T;


PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : CameraGO.T 
    RAISES {ObValue.Error, ObValue.Exception};


END ObCameraGO.
