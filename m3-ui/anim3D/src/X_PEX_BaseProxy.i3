(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue May 31 15:17:45 PDT 1994 by najork                   *)
(*       Created on Tue May 31 15:17:09 PDT 1994 by najork                   *)


INTERFACE X_PEX_BaseProxy;

FROM X_PEX_Base IMPORT T;

(* The Proxy Maker (PM) procedure for X_PEX_Base.T is 
   registered by assigning it to MkProxyT. *)

VAR 
  MkProxyT : PROCEDURE (x : T) := NIL;

END X_PEX_BaseProxy.
