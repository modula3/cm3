(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon May 30 19:16:07 PDT 1994 by najork                   *)
(*       Created on Mon May 30 19:12:37 PDT 1994 by najork                   *)


INTERFACE OrthoCameraGOProxy;

FROM OrthoCameraGO IMPORT T;

(* The Proxy Maker (PM) procedure for OrthoCameraGO.T is 
   registered by assigning it to MkProxyT. *)

VAR 
  MkProxyT : PROCEDURE (x : T) := NIL;

END OrthoCameraGOProxy.
