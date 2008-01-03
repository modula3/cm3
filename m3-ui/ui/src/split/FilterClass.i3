(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Tue Mar 10 18:59:59 1992 by steveg   *)
(*      modified on Mon Feb 24 13:53:16 PST 1992 by muller   *)
(*      modified on Wed Dec 11 23:14:29 PST 1991 by gnelson  *)
(*      modified on Wed Sep 11 15:14:27 PDT 1991 by msm      *)

<*PRAGMA LL*>

(* The "FilterClass" interface reveals the representation
   of a filter.   If you are implementing a subtype of "Filter.T",
   you can import "FilterClass" to gain access to the 
   child field. *)

INTERFACE FilterClass;

IMPORT Filter, VBT;

REVEAL Filter.T <: Public;

TYPE Public = 
  Filter.Public OBJECT <* LL >= {SELF, VBT.mu} *>
    ch: VBT.T
  END;

(* A filter "f" is a split with the single child "f.ch", or
   with no children if "f.ch=NIL".  

   The "beChild" method initializes "ch" and calls "Split.T.beChild".
   The "succ", "pred", "nth", "index", and "locate" methods use the
   "ch" field in the obvious way.  The "misc", "key", "read", "write",
   "reshape", "shape", and "axisOrder" methods forward to the child.
   *)

END FilterClass.
