(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jun  3 14:15:39 PDT 1994 by heydon                   *)

INTERFACE TextImpl;

(* An implementation of the built-in Text module. *)

IMPORT JunoScope;

PROCEDURE New(): JunoScope.Mod;
(* Return a module entity containing bindings for procedures
   implementing the "Text" module. *)

END TextImpl.
