(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Sat Jun  4 20:07:48 PDT 1994 by najork                   *)
(*       Created on Sat Mar  5 20:40:51 PST 1994 by najork                   *)


INTERFACE ObRootGO;

IMPORT ObProtoLoader;

PROCEDURE SetupPackage ();
PROCEDURE SetupModule (loader : ObProtoLoader.T);

END ObRootGO.
