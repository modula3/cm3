(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jul  6 20:10:12 PDT 1993 by msm                      *)
(*      modified on Mon Feb 24 14:01:21 PST 1992 by muller                   *)
<*PRAGMA LL*>

(* A "CostableVBT.T" is a filter whose size range is determined so as
   to avoid changing the pref except as necessary, similar to a StableVBT.
   In addition, a "CostableVBT.T" works with other buddies to maintain a
   shared version of their current size.
   *)

INTERFACE CostableVBT;

IMPORT Filter, VBT;

TYPE
  Link <: LinkPublic;
  LinkPublic = MUTEX OBJECT METHODS
    init(): Link
  END;

  T <: Public;
  Public = Filter.T OBJECT METHODS
    <* LL.sup <= VBT.mu *>
    init(ch: VBT.T; project := TRUE; link: Link := NIL): T
  END;

(* The call "v.init(ch, b)" initializes "v" as a "CostableVBT" with
   child "ch".  v is projecting if b is TRUE.  If "link" is non-NIL,
   "v" shares shape computations with other VBTs using "link". *)

PROCEDURE New(ch: VBT.T; project := TRUE; link: Link := NIL): T;
(* "New(...)" is equivalent to "NEW(T).init(...)". *)

PROCEDURE Disable(v: VBT.T); <* LL.sup < v *>
(* Temporarily disable the lowest (possibly improper) ancestor of "v"
   that is a "CostableVBT.T", if any, and all linked VBTs.  *)

(* While disabled, the filter reports its preferred size as the 
   child's preferred size.  The filter will be reenabled as soon
   as it is reshaped to a non-empty domain. *)

PROCEDURE SetShape (v: VBT.T; hPref, vPref: CARDINAL); <* LL.sup < v *>
(* Set the preferred shape of the lowest "CostableVBT.T" ancestor of "v".  A
   value of "0" leaves the current value unchanged. *)

PROCEDURE GetProjecting(v: VBT.T): BOOLEAN; <* LL.sup < v *>
(* Return TRUE if the lowest "CostableVBT.T" ancestor of "v" is projecting *)

PROCEDURE SetProjecting(v: VBT.T; project: BOOLEAN); <* LL.sup < v *>
(* Set the lowest "CostableVBT.T" ancestor of "v" to be projecting *)

END CoStableVBT.
  
