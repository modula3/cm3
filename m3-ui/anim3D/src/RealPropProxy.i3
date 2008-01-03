(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Thu Jul 21 09:10:06 PDT 1994 by najork                   *)
(*       Created on Sun May 22 00:08:25 PDT 1994 by najork                   *)


INTERFACE RealPropProxy;

IMPORT Prop, ProxiedObj;

FROM RealProp IMPORT Name, Val, ConstBeh, SyncBeh, AsyncBeh, DepBeh, Request;

(* Proxy Maker (PM) procedures for the various proxied object types are 
   registered by assigning them to these variables. These variables could be 
   put in their own interface. *)

VAR 
  NamePM     : PROCEDURE (x : Name)     := NIL;
  ValPM      : PROCEDURE (x : Val)      := NIL;
  ConstBehPM : PROCEDURE (x : ConstBeh) := NIL;
  SyncBehPM  : PROCEDURE (x : SyncBeh)  := NIL;
  AsyncBehPM : PROCEDURE (x : AsyncBeh) := NIL;
  DepBehPM   : PROCEDURE (x : DepBeh)   := NIL;
  RequestPM  : PROCEDURE (x : Request)  := NIL;

(* The "AsyncBeh" and "DepBeh" types are abstract supertypes; concrete 
   asynchronous and dependent behaviors are created by overriding the "value" 
   method. We want to be able to do this not only from Modula-3, but also from 
   the embedded language (e.g. Obliq). This is achieved by providing special
   proxy types. 

   The "proxy" field of "AsyncBeh" and "DepBeh" must contain either NIL or 
   an object of type "AsyncBehProxy" or "DepBehProxy". 
*)

TYPE
  AsyncBehProxy = ProxiedObj.Proxy OBJECT
  METHODS
    compute (time : LONGREAL) : REAL RAISES {Prop.BadMethod};
  END;

  DepBehProxy = ProxiedObj.Proxy OBJECT
  METHODS
    compute (time : LONGREAL) : REAL RAISES {Prop.BadMethod};
  END;

  RequestProxy = ProxiedObj.Proxy OBJECT
  METHODS
    value (startval : REAL; reltime : REAL) : REAL 
        RAISES {Prop.BadMethod};
  END;

END RealPropProxy.
