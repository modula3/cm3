(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Mar 23 12:37:48 PST 1993 by meehan *)
(*      modified on Sat Jan 30 01:50:53 PST 1993 by mhb    *)
(*      modified on Tue Jun 16 13:08:21 PDT 1992 by muller *)
(*      modified on Fri Mar 27 02:13:18 1992 by steveg *)
<* PRAGMA LL *>

(* The "ScrollerVBTClass" interface allows you to customize the
   user interface of a scrollbar. *)

INTERFACE ScrollerVBTClass;

IMPORT Axis, PaintOp, Pixmap, ScrollerVBT, VBT;

REVEAL
  ScrollerVBT.Private <: T;

TYPE
  T <: Public;
  Public =
    VBT.Leaf OBJECT
    METHODS
      <* LL.sup = VBT.mu *>
      init (axis := Axis.T.Ver; colors: PaintOp.ColorQuad := NIL): T;
      <* LL = VBT.mu *>
      scroll (READONLY cd        : VBT.MouseRec;
                       part      : INTEGER;
                       height    : INTEGER;
                       towardsEOF: BOOLEAN       );
      autoScroll (READONLY cd           : VBT.MouseRec;
                           linesToScroll: CARDINAL;
                           towardsEOF   : BOOLEAN       );
      thumb (READONLY cd: VBT.MouseRec; part: INTEGER; height: INTEGER);

    END;

(* The call to "v.init(axis, colors)" initializes "v" as a
   "ScrollerVBT" in the "axis" orientation, and returns "v".
   It is displayed using "colors".  If "colors" is "NIL",
   "PaintOp.bgFg" will be used.

   The default methods for "scroll", "autoScroll", and "thumb"
   are no-ops: the stripe within the scroller doesn't change.

   When the user scrolls, the implementation calls
| v.scroll(cd, part, height, towardsEOF)
   on the up-click.  "cd" is the mouse event; "height" is the number
   of pixels in the domain of "v" in the "v.axis" orientation.  "part"
   is number of pixels away from the top/left edge that the upclick
   happened.  "towardEOF" is "TRUE" when invoked from a left-upclick,
   "FALSE" when invoked from a right-upclick.  (Of these, only "cd"
   is really needed; the others can be computed from "v" and "cd".)

   While the user is in continuous or proportional scrolling,
   the implementation calls "v.autoScroll(...)" repeatedly.  The
   "linesToScroll" is somewhat of a misnomer (but kept for
   historical purposes).  For continuous scrolling, the value
   is always 1. For proportional scrolling, the value is the
   number of pixels the mouse has moved.  Think of
   "linesToScroll" as simply the ``amount that should be
   scrolled.'' The "cd" field in proportional scrolling is fine,
   except it's really a position event, not a mouse event, that
   caused the action (this is a good use for an "AnyEvent", but
   for historical reasons\dots).  For continuous scrolling, "cd"
   is set to be the mouse record for the down-click that initiate
   the scrolling, but with "cd.time = 0".

   Finally, "v.thumb(cd, part, height)" is called when the user
   thumbs or continuous thumbs.  "height" is the number of pixels
   in the domain of "v" in the "v.axis" orientation.  "part" is
   the distance in pixels between the mouse and the top/left
   edge.  The "cd" always has a valid time, cursor position, and
   modifier fields.  (OK, it isn't the real event, since
   continuous thumbing is a position event whereas thumbing is
   a mouse event.  Again, a good potential client of "AnyEvent".)

   By and large, these methods will change the position and size
   of the stripe.  This is done using the following procedure: *)


PROCEDURE Update (v: T; start, end, length: CARDINAL);
<* LL.sup < v *>
(* Set new values of the stripe and (if they've changed) mark "v" for
   redisplay. *)

(* The coordinate system for "start", "end", and "length" is as
   follows (we'll consider just a horizontal scrollbar): The left
   edge of the domain of "v" is at coordinate 0, and the right
   edge consider to be at "length".  The stripe extends from
   "max(start,0)" to "max(min(end,length),start)".  The
   implementation will draw a stripe to represent these
   quantities, scaled to the actual length of the scrollbar.

   The visual appearance of a "ScrollerVBT" is governed by the
   following data structures and procedures: *)

TYPE
  Attributes = RECORD
                 axis               : Axis.T;
                 margin             : REAL;
                 scrollPaintOps     : ARRAY Axis.T OF PaintOp.T;
                 scrollPixmaps      : ARRAY Axis.T OF Pixmap.T;
                 minStripeLen       : REAL;
                 stripeWidth        : REAL;
                 stripePaintOps     : ARRAY Axis.T OF PaintOp.T;
                 stripePixmaps      : ARRAY Axis.T OF Pixmap.T;
                 stripeBorder       : REAL;
                 stripeBorderPaintOp: PaintOp.T;
                 stripeBorderPixmap : Pixmap.T;
               END;

PROCEDURE GetAttributes (v: T): Attributes;
(* Return the attribute currently in effect for "v". *)

PROCEDURE SetAttributes (v: T; READONLY a: Attributes);
<* LL.sup = VBT.mu *>
(* Change the attributes on "v" to be "a".  Mark "v" for
   redisplay and notify "v"'s parent that its shape might have
   changed. *)

PROCEDURE Colorize (v: T; colors: PaintOp.ColorQuad);
<* LL.sup = VBT.mu *>
(* Sets the paint op of all the scroller's textures and borders
   to be "colors.bgFg".  Mark "v" for redisplay. *)

END ScrollerVBTClass.


