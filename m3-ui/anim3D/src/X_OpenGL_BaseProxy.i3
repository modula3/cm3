(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Jul 11 23:20:48 PDT 1995 by najork                   *)
(*       Created on Tue May 31 15:17:09 PDT 1994 by najork                   *)


INTERFACE X_OpenGL_BaseProxy;

FROM X_OpenGL_Base IMPORT T;

(* The Proxy Maker (PM) procedure for X_OpenGL_Base.T is 
   registered by assigning it to MkProxyT. *)

VAR 
  MkProxyT : PROCEDURE (x : T) := NIL;

END X_OpenGL_BaseProxy.
