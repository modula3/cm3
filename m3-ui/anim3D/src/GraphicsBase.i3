(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue May 31 15:11:41 PDT 1994 by najork                   *)
(*       Created on Wed Feb 16 14:53:40 PST 1994 by najork                   *)

(* A "GraphicsBase.T" is an abstraction of a window system (e.g.\ X, Trestle, 
   or NT) and a 3D graphics library (e.g.\ PEX or OpenGL). 

   I have not yet isolated the window- and graphics-system dependent code;
   this is on my to-do list. Consequently, this interface still needs to be 
   fleshed out. *)

INTERFACE GraphicsBase;

IMPORT ProxiedObj;

TYPE T <: ProxiedObj.T;

EXCEPTION Failure;

END GraphicsBase.
