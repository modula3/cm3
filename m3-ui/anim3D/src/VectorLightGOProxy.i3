(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Fri Jul 29 17:45:01 PDT 1994 by najork                   *)
(*       Created on Mon May 30 19:12:55 PDT 1994 by najork                   *)


INTERFACE VectorLightGOProxy;

FROM VectorLightGO IMPORT T;

(* The Proxy Maker (PM) procedure for VectorLightGO.T is 
   registered by assigning it to MkProxyT. *)

VAR 
  MkProxyT : PROCEDURE (x : T) := NIL;

END VectorLightGOProxy.
