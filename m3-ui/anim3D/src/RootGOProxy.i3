(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon May 30 19:03:55 PDT 1994 by najork                   *)
(*       Created on Mon May 30 19:03:39 PDT 1994 by najork                   *)


INTERFACE RootGOProxy;

FROM RootGO IMPORT T;

(* The Proxy Maker (PM) procedure for RootGO.T is 
   registered by assigning it to MkProxyT. *)

VAR 
  MkProxyT : PROCEDURE (x : T) := NIL;

END RootGOProxy.
