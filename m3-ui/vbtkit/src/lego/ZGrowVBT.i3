(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Mar  1 15:23:53 PST 1993 by meehan *)
(*      modified on Mon Feb  1 00:21:36 PST 1993 by mhb    *)
(*      modified on Tue Jun 16 13:07:58 PDT 1992 by muller *)
<* PRAGMA LL *>
<* PRAGMA SUBTYPE *>

(* A "ZGrowVBT" is a switch that has the side effect of reshaping its
   nearest ancestor subwindow.

   If the initial mouse click is unshifted, the subwindow is lifted to
   the top of its sibling; otherwise, the subwindow keeps its current
   top-to-bottom ordering among its siblings.  As the mouse is moved,
   the cursor changes to give appropriate feedback, and an outline of
   the subwindow is displayed to show the shape the subwindow will
   acquire on an uncancelled upclick.  The shape of the subwindow is
   not actually changed until the uncancelled upclick.  The outline is
   removed on an uncancelled upclick or on a chord-cancel. *)

INTERFACE ZGrowVBT;

IMPORT FeedbackVBT, ZMoveVBT;

TYPE
  <* SUBTYPE T <: MulitFilter.T *>
  T <: Public;
  Public = ZMoveVBT.T OBJECT
           METHODS
             <* LL <= VBT.mu *>
             init (f: FeedbackVBT.T): T;
           END;

END ZGrowVBT.









