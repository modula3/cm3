(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri May 17 10:23:12 PDT 1996 by mhb    *)
(*      modified on Fri Oct  8 14:00:43 PDT 1993 by steveg *)
(*      modified on Mon Jun 14 20:42:03 PDT 1993 by meehan *)
(*      modified on Tue Jun 16 13:08:04 PDT 1992 by muller *)
<* PRAGMA LL *>
<* PRAGMA SUBTYPE *>

(* A "ViewportVBT" is a multi-filter that displays multiple
   views of a child "VBT", with optional horizontal and
   vertical scrollbars.  When the child's preferred size is
   larger than the viewport's {\em interior} (that is, the screen
   of the viewport minus the scrollbars), the child is reformatted
   to its preferred size.  Since only part of the child is
   visible, the user can pan the child using the scrollbars.
   When the child's preferred size is smaller than the viewport's
   screen, the child is reformatted to the size of the viewport
   interior, and the scrollbars are ineffective.

   Views may be added or deleted under program control or by
   appropriate gestures in the scrollbar: Option Left click adds
   a new view after the view in which the user clicked.  Option
   Right click removes the view (unless, of course, it would
   leave the viewport with zero views!). *)

INTERFACE ViewportVBT;

IMPORT Axis, HVSplit, Rect, Shadow, VBT;

TYPE
  <* SUBTYPE T <: MultiFilter.T *>
  T <: Public;
  Public = HVSplit.T OBJECT
           METHODS
             <* LL <= VBT.mu *>
             init (ch             : VBT.T;
                   axis           : Axis.T   := Axis.T.Ver;
                   shadow         : Shadow.T := NIL;
                   step           : CARDINAL := 10;
                   scrollStyle := ScrollStyle.AlaViewport;
                   shapeStyle  := ShapeStyle.Unrelated;
                   multiView: BOOLEAN := TRUE): T;
           END;

(* The call to "v.init(..)" initializes "v" as a "ViewportVBT.T".  The
   "axis" parameter says whether the views are arranged vertically or
   horizontally.  "step" is the number of pixels to move while
   auto-scrolling.  "shadow" gives the shadow for displaying scrollbars,
   resets and adjusting bars.  "scrollStyle" and "shapeStyle" are explained
   below.  When "multiView" is true, multiple views may be created and an
   adjusting bar will be inserted between views so users can adjust the
   screen allocated to each view.  However, to achieve this, Join VBTs are
   used and the child coordinates are offset from the parent coordinates.
   This may cause problems when a viewport child sends commands to a
   viewport parent, such as HighlightVBT or ZSplit, to hightlight a region
   or pop up a menu or hint bubble.

   The internal structure of a viewport is a rather complex collection of
   "JoinedVBT"s, "HVSplit"s, "ScrollerVBT"s, and others.  It depends on the
   options with which the viewport was created.  Be sure to use the
   "MultiFilter" interface to get at the child. *)

TYPE 
  View = INTEGER;

(* A "View" is an internal ID for a view.  The value is valid for
   the life of a view (i.e., until it is removed by a user
   gesture or by a call to "RemoveView").  Thereafter, the ID may
   be reused.  The initial view created by the "init" method has
   a value of 0. *)

(* A viewport can be created with a number of different styles of
   scrollbars: *)

TYPE
  ScrollStyle =
    {HorAndVer, 
     HorOnly,
     VerOnly, 
     NoScroll, 
     AlaViewport, 
     Auto};

(* \noindent The styles are as follows:

   \begin{itemize}

   \item "HorAndVer" puts a horizontal and vertical scrollbar on every
   view.  In addition, nestled between the scrollbars in the southwest
   corner, there's a little ``reset'' button that moves the northwest
   corner of the child to the northwest corner of the view.

   \item "HorOnly" places a scrollbar at the bottom.

   \item "VerOnly" places a scrollbar at the left side.

   \item "NoScroll" specifies that views will not have scrollbars.

   \item "AlaViewport" specifies that there is a scrollbar in the same axis
   as the viewport.  Thus, "AlaViewport" for a vertical viewport is
   equivalent to "VerOnly".

   \item "Auto" specifies that scrollbars appear only when the preferred
   size of the child exceeds the size of the viewport (in that dimension).

   \end{itemize}

   The location of the scrollbar is further controlled by the environment
   variable "SCROLLBARMODEL"; see the "VBTKitEnv" interface. 

   *)


(* There are two possible shape-relationships between a viewport
   and its child: *)

TYPE ShapeStyle = {Unrelated, Related};

(* "Unrelated" makes the shape of the child equal to its preferred
   shape---completely unrelated to the viewport's current shape.

   "Related" makes the child's shape equal to the viewport's shape in
   the non-axis direction of the viewport.  In the viewport's axis
   direction, the child's preferred shape is used.  For example, the
   width of the child in a "Vertical" viewport is the width of the
   viewport. *)

(* \subsubsection{Panning the contents} *)


PROCEDURE ScrollTo (         v    : T;
                    READONLY r    : Rect.T;
                             view : View     := 0;
                             force: BOOLEAN  := TRUE);
<* LL = VBT.mu *>
(* Scroll the viewport "v" so that rectangle "r" is visible in
   view "view".  Rectangle "r" will be roughly centered within
   "v", but if "r" is too big to be seen entirely, its northwest
   corner will be made visible.  If "force" is "FALSE" and "r"
   is already entirely visible, this procedure is a no-op.  *)

PROCEDURE Normalize (v: T; w: VBT.T; view: View := 0);
<* LL = VBT.mu *>
(* If the domain of "w" is non-empty and it's entirely visible,
   do nothing.  Otherwise, do a "ScrollTo" to "w"'s domain in
   view "view". *)

(* At first blush, "Normalize" seems to be just a call to
| ScrollTo(v, VBT.Domain(w), FALSE)
   However, if "w" doesn't
   have a domain, as is the case when "w" has been recently
   installed and the "VBT" tree has not been redisplayed, a thread
   is forked to wait until it can acquire "VBT.mu" (recall that
   "Normalize" and "ScrollTo" have "LL = VBT.mu").  After the
   lock is acquired, all pending redisplays are satisfied, and
   then "ScrollTo" of "w"'s domain is invoked.  Since the thread
   executes outside event-time, it explicitly causes all marked
   "VBT"s to be redisplayed after it calls "ScrollTo". *)

(* \subsubsection{Multiple views} *)

PROCEDURE AddView (v: T; pred: View := -1; split := TRUE):
  View;
<* LL = VBT.mu *>
(* Add another view after the view "pred" (-1 means add as the
   first view) of the child.  If "split" is "TRUE", then the new
   view and the view "pred" will occupy the area previously
   occupied by the view "pred".  The area of all other views
   will be unchanged.  The value returned is an internal ID for
   the view.  This value may be reused after the view has been
   removed. *)

PROCEDURE RemoveView (v: T; view: View);
<* LL = VBT.mu *>
(* Remove the view "view" from "v"'s child.  The ID for the initial
   view created by the "init" method is 0. *)

END ViewportVBT.


