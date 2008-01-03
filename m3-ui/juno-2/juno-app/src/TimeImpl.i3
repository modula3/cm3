(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jun  3 14:49:06 PDT 1994 by heydon                   *)

INTERFACE TimeImpl;

(* An implementation of the built-in Time module. *)

IMPORT JunoScope;

PROCEDURE New(): JunoScope.Mod;
(* Return a module entity containing bindings for procedures
   implementing the "Time" module. *)

END TimeImpl.

