(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Feb  4 11:20:57 PST 2000 by heydon                   *)

INTERFACE PrintImpl;

(* An implementation of the built-in Print module. *)

IMPORT JunoScope;

PROCEDURE New(): JunoScope.Mod;
(* Return a module entity containing bindings for procedures
   implementing the "Print" module. *)

END PrintImpl.
