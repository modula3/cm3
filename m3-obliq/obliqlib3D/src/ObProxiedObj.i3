(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Wed Jul 20 18:07:59 PDT 1994 by najork                   *)
(*       Created on Wed Jul 20 09:34:14 PDT 1994 by najork                   *)


INTERFACE ObProxiedObj;

IMPORT ObProtoLoader, ObValue, ProxiedObj;

PROCEDURE SetupPackage ();
PROCEDURE SetupModule (loader : ObProtoLoader.T);

TYPE
  T <: Public;
  Public = ObValue.ValAnything OBJECT
    po : ProxiedObj.T;
  END;

END ObProxiedObj.
