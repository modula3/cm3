(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Oct  3 16:32:03 PDT 1995 by najork                   *)
(*       Created on Tue May 31 15:17:09 PDT 1994 by najork                   *)


INTERFACE Win_OpenGL_BaseProxy;

FROM Win_OpenGL_Base IMPORT T;

(* The Proxy Maker (PM) procedure for Win_OpenGL_Base.T is 
   registered by assigning it to MkProxyT. *)

VAR 
  MkProxyT : PROCEDURE (x : T) := NIL;

END Win_OpenGL_BaseProxy.
