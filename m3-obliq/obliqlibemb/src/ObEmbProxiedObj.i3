(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Sun Aug 13 12:51:44 EDT 1995 by macintyre                *)
(*      modified on Wed Jul 20 18:07:59 PDT 1994 by najork                   *)
(*       Created on Wed Jul 20 09:34:14 PDT 1994 by najork                   *)
(*                                                                           *)
(* Used to be ObProxiedObj in Anim3D, but I don't want everything            *)
(* depending on Anim3D so I made a separate "embedded language" package      *)

INTERFACE ObEmbProxiedObj;

IMPORT ObLoader, ObValue, EmbProxiedObj, ObLib, SynLocation, Obliq;

PROCEDURE SetupPackage ();
PROCEDURE SetupModule (loader : ObLoader.T);

PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : EmbProxiedObj.T 
    RAISES {ObValue.Error, ObValue.Exception};

PROCEDURE Extend(READONLY objects: Obliq.Vals): Obliq.Val 
    RAISES {ObValue.Error, ObValue.Exception};
(* Similar to Obliq.Clone but works by calling "extend" on the Obliq
   side of the proxy. *)

TYPE
  T <: Public;
  Public = ObValue.ValAnything OBJECT
    po : EmbProxiedObj.T;
  END;

END ObEmbProxiedObj.
