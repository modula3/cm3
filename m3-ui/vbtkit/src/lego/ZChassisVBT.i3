(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Aug 10 14:39:17 PDT 1994 by mhb                      *)
(*      modified on Fri Jun 11 15:57:37 PDT 1993 by meehan                   *)
(*      modified on Tue Jun 16 13:08:01 PDT 1992 by muller                   *)
<* PRAGMA LL *>
<* PRAGMA SUBTYPE *>

(* A "ZChassisVBT" multi-filter provides a {\em chassis} for a
   subwindow. The visual display of the chassis is hard-wired into
   this module; in particular, it won't look like a top-level window
   of most of the common X window managers.  The top of the chassis is
   a banner containing (from left to right) a {\em close button}, {\em
   draggable title}, and a {\em grow button}. (See
   Figure~\ref{fig:zchassis}.)

   Clicking on the close button unmaps the "ZChassisVBT", thereby
   causing it to disappear.  Dragging the title allows the user to
   reposition the "ZChassisVBT" within its parent. Clicking on the
   grow button allows the user to change the size of the
   "ZChassisVBT", subject to its size constraints.  That is, the user
   isn't allowed to make the interior of the chassis smaller or larger
   than its reported bounds along each dimension.  *)

INTERFACE ZChassisVBT;

IMPORT Shadow, VBT, ZChildVBT, ZSplit;

TYPE
  <* SUBTYPE T <: MultiFilter.T *>
  T <: Public;
  Public =
    ZChildVBT.T OBJECT
    METHODS
      <* LL.sup <= VBT.mu *>
      init (ch      : VBT.T;
            title   : VBT.T;
            shadow  : Shadow.T := NIL;
            closable: BOOLEAN  := TRUE;
            open    : BOOLEAN  := TRUE;
            h, v               := 0.5;
            loc                := ZChildVBT.Location.Center;
            type               := ZChildVBT.CoordType.Scaled;
            shaper: ZSplit.ReshapeControl := NIL):
            T;
      initFromEdges (ch        : VBT.T;
                     title     : VBT.T;
                     w, e, n, s: REAL;
                     shadow    : Shadow.T := NIL;
                     closable  : BOOLEAN  := TRUE;
                     open      : BOOLEAN  := TRUE;
                     type := ZChildVBT.CoordType.Absolute;
                     shaper: ZSplit.ReshapeControl := NIL): T;

      <* LL = VBT.mu *>
      callback (READONLY cd: VBT.MouseRec);
    END;

END ZChassisVBT.

(* The call "v.init(...)" initializes "v" as a "ZChassisVBT".  It is
   assumed that "v" will be a subwindow.  The interior of the chassis,
   "ch", is "v"'s child in the multi-child sense.

   An alternative method, "v.initFromEdges", also initializes "v",
   using different information for specifying the initial location of
   the subwindow.  (See the "ZChildVBT" interface on
   page~\pageref{ZChildVBTSection} for details of the "h", "v", "loc",
   and "type" parameters to "init", as well as for details of the "w",
   "e", "n", "s", and "type" parameters to "initFromEdges".)

   A close button is displayed iff "closable" is set.  The grow button
   is implemented with a "ZGrowVBT".  "title" also functions as a drag
   bar.  It is implemented by a "ZMoveVBT".  The looks of these
   buttons is governed by the "shadow" parameter.

   If "open" is "TRUE", then "v" will be visible when it is inserted
   as a child of its parent "ZSplit".

   In the current implementation, a chassis has the following general
   structure (using FormsVBT notation):

|  (Stable 
|    (Border
|      (VBox
|        (HBox (CloseButton "C")
|              (ZMove `{\it title}`)
|              (ZGrow "G"))
|        (Frame `{\it ch}`)))))

   See Figure~\ref{fig:zchassis}.
   
   However, don't try to traverse the VBT tree directly; it is subject
   to change.  To retrieve the contents of a chassis "v", use
   "MultiFilter.Child(v)".

   "v.callback(cd)" is invoked when the close button is activated.
   The default method is a no-op.

   A "ZChassisVBT"'s move, grow, and close buttons are not
   effective unless the "ZChassis" is a non-background child of a
   "ZSplit". *)




