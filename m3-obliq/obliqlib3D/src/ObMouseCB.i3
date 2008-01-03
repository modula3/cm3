(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Jul 25 11:38:41 PDT 1994 by najork                   *)
(*       Created on Fri Jul 22 18:07:46 PDT 1994 by najork                   *)


INTERFACE ObMouseCB;

IMPORT MouseCB, ObLib, ObProtoLoader, ObValue, SynLocation;

PROCEDURE SetupPackage ();
PROCEDURE SetupModule (loader : ObProtoLoader.T);

PROCEDURE GetT (args    : ObValue.ArgArray; 
                idx     : INTEGER; 
                package : ObLib.T; 
                opCode  : ObLib.OpCode; 
                loc     : SynLocation.T) : MouseCB.T 
    RAISES {ObValue.Error, ObValue.Exception};

PROCEDURE GetRec (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : MouseCB.Rec
    RAISES {ObValue.Error, ObValue.Exception};

END ObMouseCB.
