(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jan 29 15:15:13 PST 1993 by mhb    *)
(*      modified on Mon Aug 10  0:17:27 PDT 1992 by meehan *)
(*      modified on Tue Jun 16 12:59:09 PDT 1992 by muller *)
<* PRAGMA LL *>
<* PRAGMA SUBTYPE *>

(* An "AnchorSplit" is a multi-split version of "AnchorBtnVBT".
   The first child is the {\em anchor} that is displayed (such
   as a text string or an icon).  The second child is the {\em
   menu} that is displayed when the anchor is activated.  Attempts
   to give an anchor-split more than two children cause the extra
   children to be lost.

   At initialization time, the feedback for the anchor is
   specified.  It must be a childless multi-filter.  Also at
   initialization time, a frame is specified that will surround
   the menu.  The frame is also a childless multi-filter. *)

INTERFACE AnchorSplit;

IMPORT AnchorBtnVBT, FeedbackVBT, MultiFilter, VBT;

TYPE
  <* SUBTYPE T <: MultiSplit.T *>
  T <: Public;
  Public = AnchorBtnVBT.T OBJECT
           METHODS
             <* LL <= VBT.mu *>
             init (f           : FeedbackVBT.T;
                   menuFrame   : MultiFilter.T;
                   n           : CARDINAL        := 0;
                   anchorParent: VBT.T           := NIL;
                   hfudge                        := 0.0;
                   vfudge                        := 0.0  ): T;
           END;

(* The call "v.init(...)" initializes "v" as an "AnchorSplit".
   The feedback "f" and the multi-filter "menuFrame" must have no
   multi-children.  That is, calling "MultiFilter.Child(f)" and
   "MultiFilter.Child(menuFrame)" must both return "NIL".  The
   other parameters, "n", "anchorParent", "hfudge", and "vfudge"
   are the same as in "AnchorBtnVBT". *)


END AnchorSplit.



