(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon May 30 19:15:15 PDT 1994 by najork                   *)
(*       Created on Mon May 30 19:13:01 PDT 1994 by najork                   *)


INTERFACE AmbientLightGOProxy;

FROM AmbientLightGO IMPORT T;

(* The Proxy Maker (PM) procedure for AmbientLightGO.T is 
   registered by assigning it to MkProxyT. *)

VAR 
  MkProxyT : PROCEDURE (x : T) := NIL;

END AmbientLightGOProxy.
