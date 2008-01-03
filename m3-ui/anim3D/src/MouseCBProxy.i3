(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Fri Jul 22 16:24:01 PDT 1994 by najork                   *)
(*       Created on Fri Jul 22 16:10:48 PDT 1994 by najork                   *)


INTERFACE MouseCBProxy;

IMPORT CB, ProxiedObj;

FROM MouseCB IMPORT T, Rec;

(* The Proxy Maker (PM) procedure for MouseCB.T is 
   registered by assigning it to MkProxyT. *)

VAR 
  MkProxyT : PROCEDURE (x : T) := NIL;

TYPE
  Proxy = ProxiedObj.Proxy OBJECT
  METHODS
    invoke (mr : Rec) RAISES {CB.BadMethod};
  END;

END MouseCBProxy.
