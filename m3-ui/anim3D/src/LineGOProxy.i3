(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon May 30 18:52:50 PDT 1994 by najork                   *)
(*       Created on Mon May 30 18:51:57 PDT 1994 by najork                   *)


INTERFACE LineGOProxy;

FROM LineGO IMPORT T;

(* The Proxy Maker (PM) procedure for LineGO.T is 
   registered by assigning it to MkProxyT. *)

VAR 
  MkProxyT : PROCEDURE (x : T) := NIL;

END LineGOProxy.
