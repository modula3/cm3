(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* ListPkl.i3 *)
(* Last modified on Tue Sep  1 15:42:30 PDT 1992 by evers  *)

(* If a program contains an import of this interface, a "Pickle.Special"
   with "sp.sc = TYPECODE (List.T)" will be passed to
   "Pickle.RegisterSpecial" during initialization.  This special pickles
   references "r" with "TYPECODE (r) = TYPECODE (List.T)" iteratively rather
   than recursively, and is thus useful for long lists in small stacks. 
   Both Pickle and Pickle2 get such specials. *)

INTERFACE ListPkl;

END ListPkl.
