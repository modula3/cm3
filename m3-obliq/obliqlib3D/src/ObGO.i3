(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Jul 20 18:27:12 PDT 1994 by najork                   *)
(*       Created on Tue Jan 19 14:00:00 PST 1994 by najork                   *)


INTERFACE ObGO;

IMPORT GO, ObLib, ObProtoLoader, ObProxiedObj, ObValue, SynLocation;

PROCEDURE SetupPackage ();
PROCEDURE SetupModule (loader : ObProtoLoader.T);

TYPE T <: ObProxiedObj.T;

VAR 
  PropUndefined : ObValue.ValException;
  StackError    : ObValue.ValException;


PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : GO.T 
    RAISES {ObValue.Error, ObValue.Exception};

END ObGO.
