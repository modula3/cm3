(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Jun 27 11:15:49 PDT 1994 by najork                   *)
(*       Created on Sat May 28 11:15:04 PDT 1994 by najork                   *)


INTERFACE ObTransformProp;

IMPORT ObLib, ObProtoLoader, ObValue, SynLocation, TransformProp;

PROCEDURE SetupPackage ();
PROCEDURE SetupModule (loader : ObProtoLoader.T);

PROCEDURE GetOverloadedVal (args    : ObValue.ArgArray; 
                            idx     : INTEGER; 
                            package : ObLib.T; 
                            opCode  : ObLib.OpCode; 
                            loc     : SynLocation.T) : TransformProp.Val 
    RAISES {ObValue.Error, ObValue.Exception};

END ObTransformProp.
