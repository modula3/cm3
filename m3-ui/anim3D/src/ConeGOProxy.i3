(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon May 30 19:14:40 PDT 1994 by najork                   *)
(*       Created on Mon May 30 19:13:19 PDT 1994 by najork                   *)


INTERFACE ConeGOProxy;

FROM ConeGO IMPORT T;

(* The Proxy Maker (PM) procedure for ConeGO.T is 
   registered by assigning it to MkProxyT. *)

VAR 
  MkProxyT : PROCEDURE (x : T) := NIL;

END ConeGOProxy.
