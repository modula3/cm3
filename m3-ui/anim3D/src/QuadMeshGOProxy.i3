(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon May 30 19:08:50 PDT 1994 by najork                   *)
(*       Created on Mon May 30 19:08:34 PDT 1994 by najork                   *)


INTERFACE QuadMeshGOProxy;

FROM QuadMeshGO IMPORT T;

(* The Proxy Maker (PM) procedure for QuadMeshGO.T is 
   registered by assigning it to MkProxyT. *)

VAR 
  MkProxyT : PROCEDURE (x : T) := NIL;

END QuadMeshGOProxy.
