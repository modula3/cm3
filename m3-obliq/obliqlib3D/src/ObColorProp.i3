(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Thu Jun 23 18:57:56 PDT 1994 by najork                   *)
(*       Created on Sat May 28 11:15:04 PDT 1994 by najork                   *)


INTERFACE ObColorProp;

IMPORT ColorProp, ObLib, ObProtoLoader, ObValue, SynLocation;

PROCEDURE SetupPackage ();
PROCEDURE SetupModule (loader : ObProtoLoader.T);

PROCEDURE GetOverloadedVal (args    : ObValue.ArgArray; 
                            idx     : INTEGER; 
                            package : ObLib.T; 
                            opCode  : ObLib.OpCode; 
                            loc     : SynLocation.T) : ColorProp.Val 
    RAISES {ObValue.Error, ObValue.Exception};

END ObColorProp.
