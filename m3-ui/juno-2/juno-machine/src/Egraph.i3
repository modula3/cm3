(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Mar 26 16:05:52 PST 1993 by heydon                   *)

(* An "Egraph.T" is a node of an oriented, directed graph in which every node
   has out-degree 0 or 2 and on which there is an equivalence relation. *)

INTERFACE Egraph;

IMPORT Equiv;

TYPE
  T <: Public;
  Public = Equiv.T OBJECT
    car, cdr: T := NIL;
  METHODS
    init(): T;
  END;

END Egraph.

(* "NEW(Egraph.T, car := a, cdr := b).init()" returns a new node in its own
   equivalence class with children initialized to the values "a" and "b".
   For any existing node "x", "x.init().find() = x". However, the node
   "x' = x.init()" is not necessarily in its own equivalence class; there may
   be other existing elements "y" such that "y.find() = x'".

   The "car" and "cdr" fields are READONLY after initialization. *)
