(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Thu Jul 21 08:54:00 PDT 1994 by najork                   *)
(*       Created on Thu May 19 08:53:24 PDT 1994 by najork                   *)


(* A "ProxiedObj.T" is a Modula-3 object for which we want to have proxy, 
   an object that mirrors this object in some way. The prototypical use
   is to connect Modula-3 objects to their counterparts in some embedded 
   interpreted language (e.g. Obliq). *)
   
INTERFACE ProxiedObj;

TYPE
  T = OBJECT
    proxy : Proxy := NIL;
  METHODS
    makeProxy ();
  END;

(* Subtypes of "ProxiedObj.T" for which objects will be created before the 
   embedded language has registered itself should override the "makeProxy()" 
   method. (In Anim3D, these are all subtypes of "Prop.Name".) The embedded
   language is responsible for invoking "makeProxy" on all objects with a
   NIL "proxy" field. *)

TYPE 
  Proxy = OBJECT
    obj : REFANY;
  END;

END ProxiedObj.
