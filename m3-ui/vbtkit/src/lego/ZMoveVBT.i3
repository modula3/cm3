(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jun 11 16:01:24 PDT 1993 by meehan                   *)
(*      modified on Mon Feb  1 23:14:08 PST 1993 by mhb                      *)
(*      modified on Tue Jun 16 13:07:57 PDT 1992 by muller                   *)
(*      modified on Fri Mar 27 02:12:27 1992 by steveg                       *)
<* PRAGMA LL *>

(* A "ZMoveVBT" is a switch that has the side-effect of
   repositioning its nearest ancestor subwindow.

   If the initial mouse click is unshifted, the subwindow is
   lifted to the top of its sibling; otherwise, the subwindow
   keeps its current top-to-bottom ordering among its siblings.
   As the mouse is moved, the cursor is changed to give
   appropriate feedback, and an outline of the subwindow is moved
   to show where it will be repositioned on an uncancelled
   upclick.  On an uncancelled upclick or chord-cancel, the
   outline is removed. *)

INTERFACE ZMoveVBT;

IMPORT Rect, SourceVBT;

TYPE T <: SourceVBT.T;

(* The following procedure is useful for subclasses, such as "ZGrowVBT", 
   to control the shape of the outline of "v"'s subwindow as the 
   mouse is being dragged. *)

PROCEDURE MoveAndHighlight (v: T; READONLY rect: Rect.T);
<* LL = VBT.mu *>
(* Show the outline of "v" as "rect".  Should only be called by the
   "during" method of a subclass. *)

(* The default "during" method calls "MoveAndHighlight" with
   "rect" equal to the domain of the subwindow being moved,
   translated by an appropriate amount to reflect the mouse
   movement since the initial mouse click.

   On an uncancelled upclick, the default "post" method moves the
   subwindow to the rectangle last specified to
   "MoveAndHighlight" and calls "ZChildVBT.Moved" and
   "ZChildVBT.Grew".

   The highlighter used for displaying an outline of the
   subwindow contain "v" is the "HighlightVBT" returned by
   "SourceVBT.GetHighlighter(v)".  An appropriate paint op is
   constructed by examing the colors of the background child of
   the subwindow's parent.  Those colors are found using the
   "VBTColors" interface; be sure to use that interface to 
   record the background child's primary foreground and background
   colors. *)

END ZMoveVBT.








