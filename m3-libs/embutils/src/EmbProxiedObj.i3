(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(*                                                                           *)
(* Last modified on Thu Jul 21 08:54:00 PDT 1994 by najork                   *)
(*       Created on Thu May 19 08:53:24 PDT 1994 by najork                   *)
(* Was "ProxiedObj.T" in Anim3D.  Don't want to depend on Anim3D just        *)
(* for this type!                                                            *)

(* A "EmbProxiedObj.T" is a Modula-3 object for which we want to
   have proxy, an object that mirrors this object in some way. The
   prototypical use is to connect Modula-3 objects to their
   counterparts in some embedded interpreted language (e.g. Obliq). *)
   
INTERFACE EmbProxiedObj;

TYPE
  T = OBJECT
    proxy : Proxy := NIL;
  METHODS
    makeProxy ();
  END;

(* Subtypes of "EmbProxiedObj.T" for which objects will be created
   before the embedded language has registered itself should override
   the "makeProxy()" method. The embedded language is responsible for
   invoking "makeProxy" on all objects with a NIL "proxy" field. *)

EXCEPTION Error(TEXT);

TYPE 
  Proxy <: PublicProxy;
  PublicProxy = OBJECT
    obj : REFANY := NIL;
  METHODS
    init(obj : REFANY): Proxy;
    changeProxy(obj: REFANY) RAISES {Error};
  END;

END EmbProxiedObj.
