(* Copyright 1992 Digital Equipment Corporation                              *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Oct  5 16:22:52 PDT 1992 by heydon                   *)
(*      modified on Fri Aug  7 21:51:53 PDT 1992 by myers                    *)

(* A "JunoToolbox.T" is a VBT that allows users to select tools that are
   procedures or predicates contained in the local-only part scope it is
   created with.  It presents an array of buttons labeled with names, each
   of which causes the closure "cl" to be invoked with a corresponding
   "Drawing.Tool".

   If the button corresponds to a procedure or predicate with zero
   arguments, the selection "sel" is acquired, and the button is
   highlighted to indicate that it is the current tool. *)

INTERFACE JunoToolbox;

IMPORT VBT, JunoScope AS Scope, Drawing, Atom;

TYPE
  T <: VBT.T;

  Closure = OBJECT METHODS apply (t: Drawing.Tool) END;


PROCEDURE New (module_name: Atom.T;
               md         : Scope.Mod;
               cl         : Closure;
               sel        : VBT.Selection): T;

END JunoToolbox.
