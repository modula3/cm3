(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jun 14 18:51:55 PDT 1993 by meehan *)
(*      modified on Sun Jan 31 21:58:53 PST 1993 by mhb    *)
(*      modified on Tue Jun 16 13:08:34 PDT 1992 by muller *)
<* PRAGMA LL *>

(* A "ScrollerVBT" is a scrollbar with an orientation along an {\em
   axis}.  For the sake of brevity in this interface, we'll only talk
   about vertical scrollers.  For horizontal scrollers, replace
   phrases like {\it top and bottom edges} by {\it left and right
   edges}, and so on.

   Like a "NumericVBT", a "ScrollerVBT" provides a {\em
   bounded-value\/} abstraction.  That is, a "ScrollerVBT" has a {\it
   value\/} associated with it, and that value is guaranteed to stay
   within some bounds.  Various user gestures change the value and
   invoke a "callback" method on the "ScrollerVBT".  The callback
   method can inquire the value of the scrollbar, and can change the
   value and bounds.

   Visually, a scrollbar contains a {\em stripe} that spans some
   fraction of the height of the scrollbar and is slightly
   narrower than the scrollbar.  The stripe represents the value
   of the scrollbar.  Various user-gestures cause the stripe to
   move.

   More specifically, the state of a "ScrollerVBT" consists of
   five integer quantities: "min", "max", "thumb", "step", and
   "value".  The "value" is guaranteed to stay in the range "[min
   ..  max-thumb]".  Visually, the "value" is represented by the
   position (top edge) of a stripe in the scroller, and "thumb"
   by the length of the stripe.  The amount that "value" should
   change when continuous scrolling is given by "step", the {\em
   stepping} amount.

   Although each "VBT" class that uses a "ScrollerVBT" is free to
   associate any meaning with the length of the stripe, the
   following convention is suggested for using scrollbars to view
   an object:

   \begin{quote}

   The ratio of the height of the stripe to the height of the
   scrollbar should be the same as the ratio of the amount of the
   object visible vertically to its entire height.  The position
   of top of the stripe reflects the position of top of the view
   of the object within the entire object.

   \end{quote}

   Here is some terminology and the user-interface provided by a
   "ScrollerVBT":

   \begin{itemize} \item To {\em scroll}\index{scrolling} means
   to left-click or right-click in the scrollbar.

   \item You need to release the button relatively quickly, or else
   you'll start {\em continuous scrolling}.  You stop continuous
   scrolling by releasing the button, by chord-cancelling\index{chord}
   or by moving the mouse.

   \item When you move the mouse, you are then using {\em proportional
   scrolling}.  This means that the more that you move the mouse
   vertically, the more the stripe will be moved in the direction of
   the mouse movement.  You stop proportional scrolling by upclicking
   or chord-cancelling.

   \item The left and right buttons are inverses: the left button
   moves the stripe downward and the right button moves the stripe
   upward.

   \item You {\em thumb}\index{thumb} with a middle-click.  The top of
   the stripe moves to the position of the cursor.  Thus, middle-click
   above the top of the stripe moves the stripe up, and middle-click
   below the top moves the stripe down.

   \item Middle-drag causes {\em continuous thumbing}.  As you drag to
   a new position, the top of the stripe moves to match the current
   cursor position.  You stop continuous thumbing by middle-upclicking
   or chord-canceling.

   \end{itemize}

   If you want a different user interface, you need to subclass
   various methods (e.g., a "thumb", "scroll", "autoscroll") of the
   scrollbar.  These methods are defined in the "ScrollerVBTClass"
   interface. *)

INTERFACE ScrollerVBT;

IMPORT Axis, PaintOp, VBT;

TYPE
  T <: Public;
  Private <: VBT.T;
  Public = Private OBJECT
           METHODS
             <* LL.sup = VBT.mu *>
             init (axis  : Axis.T;
                   min   : INTEGER;
                   max   : INTEGER;
                   colors: PaintOp.ColorQuad;
                   step  : CARDINAL            := 1;
                   thumb : CARDINAL            := 0  ): T;
             <* LL = VBT.mu *>
             callback (READONLY cd: VBT.MouseRec);
           END;

(* The call to "v.init(...)" initializes "v" as a
   "ScrollerVBT" in the "axis" orientation.  It is
   displayed using "colors".

   The implementation calls "v.callback(cd)" after "v"'s value
   has been changed by the user; it is not called when the value
   is changed as the result of calls to "Put" or "PutBounds".
   The default "callback" method is a no-op. *)

PROCEDURE Put (v: T; n: INTEGER);
<* LL.sup = VBT.mu *>
(* Change the value of "v", projected to "[min .. max-thumb]", and
   mark "v" for redisplay. *)

PROCEDURE PutBounds (v    : T;
                     min  : INTEGER;
                     max  : INTEGER;
                     thumb: CARDINAL  := 0);
<* LL.sup = VBT.mu *>
(* Set the bounds, project "v"'s value into "[min .. max-thumb]", and
   mark "v" for redisplay. *)

PROCEDURE PutStep (v: T; step: CARDINAL);
<* LL.sup = VBT.mu *>
(* Change the amount that "v"'s value should change while
   continuous scrolling to "step".  If "step = 0", scrolling will
   be disabled. *)

PROCEDURE Get      (v: T): INTEGER;  <* LL.sup = VBT.mu *>
PROCEDURE GetMin   (v: T): INTEGER;  <* LL.sup = VBT.mu *>
PROCEDURE GetMax   (v: T): INTEGER;  <* LL.sup = VBT.mu *>
PROCEDURE GetThumb (v: T): CARDINAL; <* LL.sup = VBT.mu *>
PROCEDURE GetStep  (v: T): CARDINAL; <* LL.sup = VBT.mu *>
(* Return the current "value", "min", "max", "thumb", and
   "step". *)

END ScrollerVBT.

