(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Thu Aug  4 12:13:57 PDT 1994 by najork                   *)
(*       Created on Sat May 28 11:15:04 PDT 1994 by najork                   *)


INTERFACE ObPointProp;

IMPORT ObLib, ObProtoLoader, ObValue, Obliq, PointProp, SynLocation;

PROCEDURE SetupPackage ();
PROCEDURE SetupModule (loader : ObProtoLoader.T);

PROCEDURE GetOverloadedVal (args    : ObValue.ArgArray; 
                            idx     : INTEGER; 
                            package : ObLib.T; 
                            opCode  : ObLib.OpCode; 
                            loc     : SynLocation.T) : PointProp.Val 
    RAISES {ObValue.Error, ObValue.Exception};

PROCEDURE ObliqToM3 (val : Obliq.Val) : PointProp.Val 
    RAISES {ObValue.Error, ObValue.Exception};


END ObPointProp.
