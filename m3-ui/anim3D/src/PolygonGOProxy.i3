(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon May 30 19:07:24 PDT 1994 by najork                   *)
(*       Created on Mon May 30 19:07:11 PDT 1994 by najork                   *)


INTERFACE PolygonGOProxy;

FROM PolygonGO IMPORT T;

(* The Proxy Maker (PM) procedure for PolygonGO.T is 
   registered by assigning it to MkProxyT. *)

VAR 
  MkProxyT : PROCEDURE (x : T) := NIL;

END PolygonGOProxy.
