(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Sat Jun  4 18:49:57 PDT 1994 by najork                   *)
(*       Created on Mon Mar  7 10:14:20 PST 1994 by najork                   *)


INTERFACE ObX_PEX_Base;

IMPORT ObProtoLoader;


PROCEDURE SetupPackage ();
PROCEDURE SetupModule (loader : ObProtoLoader.T);


END ObX_PEX_Base.
