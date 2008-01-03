(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Sat Jun  4 20:03:27 PDT 1994 by najork                   *)
(*       Created on Sat Mar  5 18:41:58 PST 1994 by najork                   *)


INTERFACE ObLightGO;

IMPORT ObGO, ObProtoLoader;

PROCEDURE SetupPackage ();
PROCEDURE SetupModule (loader : ObProtoLoader.T);

TYPE T <: ObGO.T;

END ObLightGO.
