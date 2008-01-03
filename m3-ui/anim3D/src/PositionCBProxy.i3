(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Fri Jul 22 16:54:54 PDT 1994 by najork                   *)
(*       Created on Fri Jul 22 16:54:28 PDT 1994 by najork                   *)


INTERFACE PositionCBProxy;

IMPORT CB, ProxiedObj;

FROM PositionCB IMPORT T, Rec;

(* The Proxy Maker (PM) procedure for PositionCB.T is 
   registered by assigning it to MkProxyT. *)

VAR 
  MkProxyT : PROCEDURE (x : T) := NIL;

TYPE
  Proxy = ProxiedObj.Proxy OBJECT
  METHODS
    invoke (pr : Rec) RAISES {CB.BadMethod};
  END;

END PositionCBProxy.
