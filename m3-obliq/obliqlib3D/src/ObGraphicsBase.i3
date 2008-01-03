(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Wed Jul 20 18:11:08 PDT 1994 by najork                   *)
(*       Created on Mon Mar  7 10:13:26 PST 1994 by najork                   *)


INTERFACE ObGraphicsBase;

IMPORT GraphicsBase, ObLib, ObProtoLoader, ObProxiedObj, ObValue, SynLocation;

PROCEDURE SetupPackage ();
PROCEDURE SetupModule (loader : ObProtoLoader.T);


TYPE T <: ObProxiedObj.T;

VAR 
  Failure : ObValue.ValException;


PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : GraphicsBase.T 
    RAISES {ObValue.Error, ObValue.Exception};


END ObGraphicsBase.
