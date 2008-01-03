(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon May 30 18:55:00 PDT 1994 by najork                   *)
(*       Created on Mon May 30 18:54:47 PDT 1994 by najork                   *)


INTERFACE MarkerGOProxy;

FROM MarkerGO IMPORT T;

(* The Proxy Maker (PM) procedure for MarkerGO.T is 
   registered by assigning it to MkProxyT. *)

VAR 
  MkProxyT : PROCEDURE (x : T) := NIL;

END MarkerGOProxy.
