(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sun Apr 14 14:51:37 PDT 1996 by heydon                   *)

INTERFACE RandomImpl;

(* An implementation of the built-in "Random" module, which provides a random
   number generator. *)

IMPORT JunoScope;

PROCEDURE New(): JunoScope.Mod;
(* Return the module entity defining the "Random" module's external
   procedures. Requires that "rt" be the root of the Juno application. *)

END RandomImpl.
