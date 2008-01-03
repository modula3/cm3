(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon May 30 19:14:21 PDT 1994 by najork                   *)
(*       Created on Mon May 30 19:13:12 PDT 1994 by najork                   *)


INTERFACE CylinderGOProxy;

FROM CylinderGO IMPORT T;

(* The Proxy Maker (PM) procedure for CylinderGO.T is 
   registered by assigning it to MkProxyT. *)

VAR 
  MkProxyT : PROCEDURE (x : T) := NIL;

END CylinderGOProxy.
