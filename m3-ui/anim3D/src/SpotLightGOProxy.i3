(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon May 30 19:15:52 PDT 1994 by najork                   *)
(*       Created on Mon May 30 19:12:43 PDT 1994 by najork                   *)


INTERFACE SpotLightGOProxy;

FROM SpotLightGO IMPORT T;

(* The Proxy Maker (PM) procedure for SpotLightGO.T is 
   registered by assigning it to MkProxyT. *)

VAR 
  MkProxyT : PROCEDURE (x : T) := NIL;

END SpotLightGOProxy.
