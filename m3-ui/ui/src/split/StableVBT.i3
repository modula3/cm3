(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Oct 20 18:15:44 PDT 1992 by msm                      *)
(*      modified on Mon Feb 24 14:01:21 PST 1992 by muller                   *)
<*PRAGMA LL*>

(* A "StableVBT.T" is a filter whose size range is determined
   as follows:

   its max and min size are its child's max and min size.

   its preferred size is determined as follows: if its own size
   satisfies its max and min size, then its preferred size is its own
   size, otherwise its preferred size is its child's preference,
   unless the filter is projecting, in which case the preferred size
   is the projection of the VBTs own size into the child's size range.  Its 
   "own size" is its current size if this is non-empty, or its last non-empty
   size otherwise, unless. SetShape has been called the next calls to the
   shape procedures use the values set.

   Thus when the child changes its preferred shape, the parent generally
   does not.  To allow a new preferred shape of a child to get through
   the filter, the filter can be temporarily disabled.
   
   *)

INTERFACE StableVBT;

IMPORT Filter, VBT;

TYPE
  T <: Public;
  Public = Filter.T OBJECT METHODS
    <* LL.sup <= VBT.mu *>
    init(ch: VBT.T; project := TRUE): T
  END;

(* The call "v.init(ch, b)" initializes "v" as a "StableVBT" with
   child "ch".  v is projecting if b is TRUE. *)

PROCEDURE New(ch: VBT.T; project := TRUE): T;
(* "New(...)" is equivalent to "NEW(T).init(...)". *)

PROCEDURE Disable(v: VBT.T); <* LL.sup < v *>
(* Temporarily disable the lowest (possibly improper) ancestor of "v"
   that is a "StableVBT.T", if any.  *)

(* While disabled, the filter reports its preferred size as the 
   child's preferred size.  The filter will be reenabled as soon
   as it is reshaped to a non-empty domain. *)

PROCEDURE SetShape(v: VBT.T; hPref, vPref: CARDINAL); <* LL.sup < v *>
(* Set the own shape of the lowest "StableVBT.T" ancestor of "v".  A value
   of "0" leaves the current value unchanged. *)

PROCEDURE GetProjecting(v: VBT.T): BOOLEAN; <* LL.sup < v *>
(* Return TRUE if the lowest "StableVBT.T" ancestor of "v" is projecting *)

PROCEDURE SetProjecting(v: VBT.T; project: BOOLEAN); <* LL.sup < v *>
(* Set the lowest "StableVBT.T" ancestor of "v" to be projecting *)

END StableVBT.
  
