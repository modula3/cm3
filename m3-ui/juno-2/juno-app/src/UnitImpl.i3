(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jun 24 10:49:31 PDT 1994 by heydon                   *)

INTERFACE UnitImpl;

(* An implementation of the built-in Unit module. *)

IMPORT View, JunoScope;

PROCEDURE New(rt: View.Root): JunoScope.Mod;
(* Return a module entity containing bindings for procedures
   implementing the "Unit" module that operate on the drawing
   view "rt.currView". *)

END UnitImpl.
