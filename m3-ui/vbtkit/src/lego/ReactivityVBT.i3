(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Feb  8 22:56:41 PST 1996 by mhb                      *)
(*      modified on Fri Jun 11 21:44:25 PDT 1993 by meehan                   *)
(*      modified on Tue Jun 16 13:08:25 PDT 1992 by muller                   *)
(*      modified on Fri Mar 27 01:59:50 PST 1992 by steveg                   *)
<* PRAGMA LL *>

(* A "ReactivityVBT" is a filter that can make its child active,
   passive, dormant, and invisible.  The {\em active\/} state does
   nothing; mouse and keyboard events are relayed to child. The {\em
   passive\/} state doesn't allow mouse or keyboard events to go to
   the child. 
   The {\em dormant\/} state doesn't send mouse or keyboard events to
   the child; it also grays out the child.  The {\em vanish\/} state
   also doesn't send mouse or keyboard events to go to the child; in
   addition, it draws over the child in the background color, thereby
   making it invisible.

   When the state of a "ReactivityVBT" is set, the caller also
   specifies a cursor to be used. 

   If a VBT-descendant of a "ReactivityVBT" is painted, it will appear
   correctly.  For example, if the "ReactivityVBT" is in the vanished
   state, the descendant will not appear until the state changes; if
   the "ReactivityVBT" is in a dormant state, the descendant will be
   grayed out.

   A "ReactivityVBT" also passes on any miscellaneous events to take
   the keyboard focus to the descendant that last acquired the
   keyboard focus. *)

INTERFACE ReactivityVBT;

IMPORT Cursor, ETAgent, PaintOp, Rect, VBT;

TYPE
  State = {Active, Passive, Dormant, Vanish};
  T <: Public;
  Public =
    ETAgent.T OBJECT
    METHODS
      <* LL.sup <= VBT.mu *>
      init (ch: VBT.T; colors: PaintOp.ColorScheme := NIL): T;
      <* LL = VBT.mu.v *>
      paintDormant (r: Rect.T; colors: PaintOp.ColorScheme);
    END;

(* The call "v.init(..)" initializes "v" as a "ReactivityVBT"
   with child "ch" and with an initial state of "Active".  If
   "colors" is "NIL", then "PaintOp.bgFg" is used instead.  The
   "colors" are used to draw the vanished and dormant states. 

   The implementation calls "v.paintDormant(r, colors)" to paint
   the part of "ch" bounded by rectangle "r" when "v"'s state
   is "Dormant".  The ``current colors'' of "v" are passed as
   "colors".  Initially, the current colors are those that were
   specified when the "ReactivityVBT" was initialized.  They can
   be changed using the "SetColors" procedure.  The default
   method paints a "Pixmap.Gray" texture using
   "colors.transparentBg".  *)

PROCEDURE Set (v: T; state: State; cursor: Cursor.T);
<* LL.sup = VBT.mu *>
(* Change "v"'s state and cursor, and mark "v" for redisplay. *)

PROCEDURE Get (v: T): State;
<* LL.sup = VBT.mu *>
(* Retrieve "v"'s current state. *)

PROCEDURE GetCursor (v: T): Cursor.T;
<* LL.sup = VBT.mu *>
(* Retrieve "v"'s current cursor. *)

PROCEDURE SetColors (v: T; colors: PaintOp.ColorScheme);
<* LL.sup = VBT.mu *>
(* Change the colors that "v" uses for the "Dormant" and "Vanish"
   states.  If "v" is currently in the "Dormant" or "Vanish"
   state, mark "v" for redisplay. *)

END ReactivityVBT.











