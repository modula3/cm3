(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Sat May 28 20:25:58 PDT 1994 by najork                   *)
(*       Created on Sat May 28 20:03:47 PDT 1994 by najork                   *)


INTERFACE AnimHandleProxy;

FROM AnimHandle IMPORT T;

(* The Proxy Maker (PM) procedure for AnimHandle.T is 
   registered by assigning it to MkProxyT. *)

VAR 
  MkProxyT : PROCEDURE (x : T) := NIL;

END AnimHandleProxy.
