(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon May 30 19:14:03 PDT 1994 by najork                   *)
(*       Created on Mon May 30 19:13:25 PDT 1994 by najork                   *)


INTERFACE DiskGOProxy;

FROM DiskGO IMPORT T;

(* The Proxy Maker (PM) procedure for DiskGO.T is 
   registered by assigning it to MkProxyT. *)

VAR 
  MkProxyT : PROCEDURE (x : T) := NIL;

END DiskGOProxy.
