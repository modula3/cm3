(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon May 30 19:16:22 PDT 1994 by najork                   *)
(*       Created on Mon May 30 19:12:31 PDT 1994 by najork                   *)


INTERFACE PerspCameraGOProxy;

FROM PerspCameraGO IMPORT T;

(* The Proxy Maker (PM) procedure for PerspCameraGO.T is 
   registered by assigning it to MkProxyT. *)

VAR 
  MkProxyT : PROCEDURE (x : T) := NIL;

END PerspCameraGOProxy.
