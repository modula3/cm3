(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon May 30 18:49:25 PDT 1994 by najork                   *)
(*       Created on Sat May 28 18:53:30 PDT 1994 by najork                   *)


INTERFACE PropProxy;

FROM Prop IMPORT T;

(* The Proxy Maker (PM) procedure for Prop.T is 
   registered by assigning it to MkProxyT. *)

VAR 
  MkProxyT : PROCEDURE (x : T) := NIL;

END PropProxy.
