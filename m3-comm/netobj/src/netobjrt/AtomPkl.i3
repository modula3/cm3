(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* ListPkl.i3 *)

(* Last modified on Wed Feb 10 17:14:04 PST 1993 by owicki  *)

(* If a program contains an import of this interface, a "Pickle.Special"
   with "sp.sc = TYPECODE (Atom.T)" will be passed to
   "Pickle.RegisterSpecial" during initialization.  This special pickles
   references "r" with "TYPECODE (r) = TYPECODE (Atom.T)" by
   passing the text obtained from Atom.Name(r). 
   Both Pickle and Pickle2 get such specials. *)

INTERFACE AtomPkl;

END AtomPkl.
