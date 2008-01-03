(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jun 11 22:00:14 PDT 1993 by meehan *)
(*      modified on Thu Jan 28 14:03:14 PST 1993 by mhb    *)
(*      modified on Wed Jan 13 09:10:53 PST 1993 by steveg *)
(*      modified on Tue Jun 16 13:08:22 PDT 1992 by muller *)
<* PRAGMA LL *>
<* PRAGMA SUBTYPE *>

(* A "ScaleFilter" is a multi-filter whose child's screentype is
   the same as the parent's except that the resolution is scaled. *)

INTERFACE ScaleFilter;

IMPORT VBT;

TYPE
  <* SUBTYPE T <: MultiFilter.T *>
  T <: Public;
  Private <: VBT.T;
  Public = Private OBJECT
           METHODS
             <* LL.sup <= VBT.mu *>
             init (ch: VBT.T): T
           END;

(* The call "v.init(ch)" initializes "v" as a "ScaleFilter" with
   multi-child "ch" and with horizontal and vertical scale factors 
   both equal to 1.0. *)

(* There are two ways you can use a "ScaleFilter": Procedure "Scale" allows
   you to explicitly set a horizontal and vertical scale factor.
   Procedure "AutoScale" looks at the preferred size of the child and 
   dynamically sets the scale factors such that the child's preferred 
   size always fills its domain. *)
 
PROCEDURE Scale (v: T; hscale, vscale: REAL);
<* LL.sup = VBT.mu.v *>
(* Set "v"'s horizontal and vertical scale factors to be "hscale"
   and "vscale" respectively, and mark "v" for redisplay. *)

(* Thus, if the "v" has resolution of "px" and "py" horizontally
   and vertically, then the resolution of "v"'s multi-child will
   be "hscale*px" and "vscale*py". 

   Note that the locking level of "Scale" does not
   require the full share of "VBT.mu".  Therefore, it can be
   called from "v"'s "reshape" or "rescreen" method, for
   example, since those methods are called with only "v"'s share
   of "VBT.mu" locked. This fact is useful for the implementation
   of procedure "AutoScale": *)

PROCEDURE AutoScale (v: T; keepAspectRatio := FALSE);
<* LL.sup = VBT.mu *>
(* Set "v"'s scale factor such that the preferred size of "v"'s
   child "ch" is scaled to fit into "VBT.Domain(ch)".  If
   "keepAspectRatio" is "TRUE", then "ch" is scaled by the same
   amount "f" both horizontally and vertically.  The amount "f" is
   chosen so that the preferred size of "ch" just fits in the
   larger direction of "v" and fits fine in the other
   direction. In any event, "v" is marked for redisplay. *)

(* The call to "AutoScale" has the effect of causing "Scale" to be
   called each time that "v" is reshaped. Thus, it is important that
   "Scale" have a locking level of "VBT.mu.v" rather than simply
   "VBT.mu". *)

PROCEDURE Get(v: T; VAR (* OUT *) hscale, vscale: REAL);
<* LL.sup = VBT.mu *>
(* Return "v"'s current horizontal and vertical scale factors. *)

(* If "Scale" was called more recently than "AutoScale", then
   "Get" returns the values passed to "Scale".  On the other hand, if
   "AutoScale" was called more recently, then "Get" will return
   values that reflect scaling for "v"'s current domain. *)

END ScaleFilter.
