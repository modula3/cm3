(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* PackSplit.i3 by cgn & msm, coded Sat Nov  1 18:19:34 1986 *)
(* Last modified on Sat Oct  3 12:09:10 PDT 1992 by msm     *)
(*      modified on Mon Feb 24 13:53:50 PST 1992 by muller  *)
(*      modified on Sat Dec 21 16:15:10 PST 1991 by gnelson *)
(*      modified on Wed Aug 22 08:18:35 1990 by tomr *)
(*      modified on Mon Apr 23 16:23:37 PDT 1990 by steveg *)
(*      modified on Fri Feb  2 14:03:59 PST 1990 by glassman *)
<*PRAGMA LL*>

(* A "PackSplit.T" is a parent window whose children are packed into
   multiple rows or columns, depending on the {\it axis} of the split.

   If the axis is horizontal, the children are packed into rows from west
   to east, moving south to a new row when the current row fills up.  This
   is the normal style used in placing words in a paragraph.

   If the axis is vertical, the children are packed into columns from north
   to south, moving east to a new column when the current column fills up.
   This is the normal style used in placing paragraphs in a newspaper
   article.

   A "PackSplit" always gives its children their preferred height and
   width, even if this makes them extend outside the parent domain (in
   which case they will be clipped).

   If the axis is horizontal, the children in any given row have their
   north edges aligned, and all children that are first in their row have
   their west edges aligned with the west edge of the parent.  A child will
   be horizontally clipped if its requested horizontal size exceeds the
   parent's horizontal size; in this case the child will be alone in its
   row.

   If the axis is vertical, the children in any given column have their
   west edges aligned, and all children that are first in their column have
   their north edge aligned with the north edge of the parent.  A child
   will be vertically clipped if its requested vertical size exceeds the
   parent's vertical size; in this case the child will be alone in its
   column.

   The {\it size} of a window is the extent of its domain in the axis of
   the "PackSplit"; its {\it cross-size} is its extent in the other axis.

   The minimum desirable size for a "PackSplit" is the maximum of the prefs
   of its children; the preferred size is the current size, unless this is
   0, in which case the preferred size is same as the minimum size.  The
   maximum size is the default for a "VBT".  The shape method uses the size
   to determine the cross-size that is just large enough to pack in all the
   children at their preferred sizes, and returns as its range of desirable
   cross-sizes a singleton interval containing only this cross-size. *)

INTERFACE PackSplit;

IMPORT VBT, PaintOp, Pixmap, Axis;

TYPE
  T <: Public;
  Private <: VBT.Split;
  Public = Private OBJECT METHODS
    <* LL.sup <= VBT.mu *>
    init(hv := Axis.T.Hor;
      hgap, vgap := 1.5;
      txt: Pixmap.T := Pixmap.Solid; 
      op: PaintOp.T := PaintOp.Bg; 
      nwAlign := FALSE;
      saveBits := FALSE): T
  END;

(* The call "v.init(...)" initializes "v" as an empty packsplit with 
   axis "hv". *)

(* For a horizontal "PackSplit", "hgap" is the gap to leave between
   children in each row; "vgap" is the gap to leave between rows.  For
   a vertical "PackSplit", "vgap" is the gap to leave between children
   in each column; "hgap" is the gap to leave between columns.  The gaps
   are specified in millimeters.

   The area not covered by children is painted using the painting
   operation "op" and the texture "txt+delta", where "delta" is the
   origin unless "nwAlign" is set to "TRUE", in which case "delta" will
   be set to the northwest corner of "v".

   If "saveBits" is "TRUE", the implementation will try to save the
   children's old bits when reshaping; if the children don't use the
   old bits, it is more efficient to let "saveBits" default to "FALSE". *)


PROCEDURE New(
  hv := Axis.T.Hor;
  hgap, vgap := 1.5;
  txt: Pixmap.T := Pixmap.Solid; 
  op: PaintOp.T := PaintOp.Bg; 
  nwAlign := FALSE;
  saveBits := FALSE): T; <* LL.sup <= VBT.mu *>
(* "New(...)" is equivalent to "NEW(T).init(...)". *)

PROCEDURE Set(
  v: T;   
  txt: Pixmap.T; 
  op: PaintOp.T := PaintOp.BgFg;
  nwAlign := FALSE); <* LL.sup = VBT.mu *>
(* Change the texture displayed by "v" and mark "v" for redisplay. *)

PROCEDURE Get(
  v: T;   
  VAR txt: Pixmap.T; 
  VAR op: PaintOp.T;
  VAR nwAlign: BOOLEAN
  ); <* LL.sup = VBT.mu *>
(* Fetch the texture displayed by "v". *)

PROCEDURE AxisOf(v: T): Axis.T; <* LL.sup <= VBT.mu *>
(* Return the axis of "v".  *)


PROCEDURE HGap(v: T): REAL; <* LL.sup <= VBT.mu *>
(* Return the "hgap" of "v".  *)


PROCEDURE VGap(v: T): REAL; <* LL.sup <= VBT.mu *>
(* Return the "vgap" of "v".  *)

END PackSplit.
