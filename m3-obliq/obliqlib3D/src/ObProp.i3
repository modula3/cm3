(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Jul 26 18:54:35 PDT 1994 by najork                   *)
(*       Created on Fri May 27 20:25:07 PDT 1994 by najork                   *)


INTERFACE ObProp;

IMPORT ObLib, ObProtoLoader, ObProxiedObj, ObValue, Prop, SynLocation;

PROCEDURE SetupPackage ();
PROCEDURE SetupModule (loader : ObProtoLoader.T);

TYPE
  Name    <: ObProxiedObj.T;
  Val     <: ObProxiedObj.T;
  Beh     <: ObProxiedObj.T;
  Request <: ObProxiedObj.T;

VAR 
  BadMethod   : ObValue.ValException;
  BadInterval : ObValue.ValException;

PROCEDURE GetT (args    : ObValue.ArgArray; 
                idx     : INTEGER; 
                package : ObLib.T; 
                opCode  : ObLib.OpCode; 
                loc     : SynLocation.T) : Prop.T 
    RAISES {ObValue.Error, ObValue.Exception};

PROCEDURE GetName (args    : ObValue.ArgArray; 
                   idx     : INTEGER; 
                   package : ObLib.T; 
                   opCode  : ObLib.OpCode; 
                   loc     : SynLocation.T) : Prop.Name 
    RAISES {ObValue.Error, ObValue.Exception};

PROCEDURE NameToObliq (pn : Prop.Name) : ObValue.Val;

END ObProp.
