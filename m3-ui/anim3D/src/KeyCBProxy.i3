(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Fri Jul 22 16:55:37 PDT 1994 by najork                   *)
(*       Created on Fri Jul 22 16:55:23 PDT 1994 by najork                   *)


INTERFACE KeyCBProxy;

IMPORT CB, ProxiedObj;

FROM KeyCB IMPORT T, Rec;

(* The Proxy Maker (PM) procedure for KeyCB.T is 
   registered by assigning it to MkProxyT. *)

VAR 
  MkProxyT : PROCEDURE (x : T) := NIL;

TYPE
  Proxy = ProxiedObj.Proxy OBJECT
  METHODS
    invoke (kr : Rec) RAISES {CB.BadMethod};
  END;

END KeyCBProxy.
