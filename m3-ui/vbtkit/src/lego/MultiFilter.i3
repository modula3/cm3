(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jun 10 13:48:22 PDT 1993 by meehan *)
(*      modified on Mon Feb  1 00:36:23 PST 1993 by mhb    *)
(*      modified on Tue Jun 16 13:08:36 PDT 1992 by muller *)
(*      modified on Fri Mar 27 01:47:38 1992 by steveg*)
<* PRAGMA LL *>

(* The "MultiFilter" interface defines the functionality that is common 
   to all clients of multi-filters; namely, retrieving and changing a 
   multi-filter's multi-child. 

   A multi-filter is a multi-split with at most one child. Thus,
   you can use the procedures in the "MultiSplit" interface on a VBT
   that is a multi-filter. The semantics of the "MultiSplit" procedures
   on a multi-filter should be obvious, with the following exceptions: 
   "MultiSplit.Move" on a multi-filter is a no-op, and "MultiSplit.Insert" 
   on a multi-filter replaces the child, if any. *)

INTERFACE MultiFilter;

IMPORT VBT;

TYPE T = VBT.T;
(* A "MultiFilter.T" is a "VBT.T" with a "MultiClass.Filter" in its
   property set. *)

(* The following procedures can accept either a "MultiFilter.T"
   or a "Filter.T" as the first argument.  If the first argument
   is not a "MultiFilter.T", the procedure just calls the corresponding
   procedure in the "Filter" interface. *)

PROCEDURE Child (v: VBT.T): VBT.T;
<* LL.sup = VBT.mu *>
(* Return the child of "v", or "NIL" if there is no child. *)

PROCEDURE Replace (v, ch: VBT.T): VBT.T;
<* LL.sup = VBT.mu *>
(* Replace "v"'s child by "ch" and return "v"'s  old child. *)

(* "MultiFilter.Replace" is similar to "MultiSplit.Replace",
   except that it returns the old multi-child instead of taking
   the old multi-child as an argument, and if "ch" is "NIL" it is
   similar to "MultiSplit.Delete". *)

END MultiFilter.

