(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon May 30 19:14:53 PDT 1994 by najork                   *)
(*       Created on Mon May 30 19:13:06 PDT 1994 by najork                   *)


INTERFACE TorusGOProxy;

FROM TorusGO IMPORT T;

(* The Proxy Maker (PM) procedure for TorusGO.T is 
   registered by assigning it to MkProxyT. *)

VAR 
  MkProxyT : PROCEDURE (x : T) := NIL;

END TorusGOProxy.
