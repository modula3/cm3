(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon May 30 18:56:37 PDT 1994 by najork                   *)
(*       Created on Mon May 30 18:56:21 PDT 1994 by najork                   *)


INTERFACE BoxGOProxy;

FROM BoxGO IMPORT T;

(* The Proxy Maker (PM) procedure for BoxGO.T is 
   registered by assigning it to MkProxyT. *)

VAR 
  MkProxyT : PROCEDURE (x : T) := NIL;

END BoxGOProxy.
