(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sun Apr 14 14:21:47 PDT 1996 by heydon                   *)
(*      modified on Tue Feb  7 14:20:14 PDT 1995 by gnelson                  *)

INTERFACE JunoUIImpl;

(* An implementation of the built-in "JunoUI" module, which provides
   information to the Juno program about the state of the application user
   interface. *)

IMPORT View, JunoScope, Atom;

VAR (*CONST*) ModSym, DragSym: Atom.T;

PROCEDURE New(rt: View.Root): JunoScope.Mod;
(* Return the module entity; requires that "rt" be the root of the Juno
   application. The resulting entity also binds the name in the global
   variable "DragSym" to an external procedure for dragging. *)

END JunoUIImpl.
