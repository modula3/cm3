(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Aug 18 11:15:55 PDT 1994 by heydon                   *)

(* An "Equiv.T" is an element of an equivalence relation. *)

INTERFACE Equiv;

EXCEPTION Forbidden;

TYPE
  T <: Public;
  Public = OBJECT
    root: T;
  METHODS
    init(): T;
    union(y: T): T RAISES {Forbidden};
  END;

END Equiv.

(* A "NEW(Equiv.T).init()" is in an equivalence class by itself. For any
   existing element "x", "x.init().root = x". However, the element
   "x' = x.init()" is not necessarily in its own equivalence class; there may
   be other existing elements "y" such that "y.root = x'".

   The field "x.root" is the distinguished representative of "x"'s
   equivalence class.

   The call "x.union(y)" combines the equivalence classes represented by "x"
   and "y" and returns the representative of the new class. It is a checked
   run-time error for "x" and "y" not to be roots of their equivalence
   classes. The method raises "Forbidden" if the union operation is illegal;
   the default "union" method never raises "Forbidden", but it may be useful
   for subtypes to override it.

   After the call "x.union(y)", "x.root = y.root". The distinguished
   representative of the class formed by "x.union(y)" is guaranteed to be
   either the distinguished representative of "x"'s class or the distinguished
   representative of "y"'s class before the union. *)
