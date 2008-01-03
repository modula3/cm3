(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Feb 24 13:52:58 PST 1992 by muller                   *)
<*PRAGMA LL*>

(* A "ButtonVBT.T" is a filter with an associated action procedure that
   is called when the user clicks on the button or makes some other
   appropriate gesture.

   Different subtypes of "ButtonVBTs" invoke the action procedure on
   different user gestures, but all "ButtonVBTs" have the three methods
   "pre", "post", and "cancel".  They all interpret user gestures in
   such a way that the sequence of calls will be in the regular
   expression

| ( (pre cancel) | (pre action post) )*

   The minimum, maximum, and preferred size of a "ButtonVBT" 
   are all equal to the minimum size of its child, in each axis.  *)

INTERFACE ButtonVBT;

IMPORT VBT, Filter, PackSplit, PaintOp;

TYPE
  T <: Public;
  Public = Filter.T OBJECT (*CONST*) 
    action: Proc
  METHODS 
    <* LL.sup = VBT.mu *>
    pre();
    post();
    cancel();
    <* LL.sup <= VBT.mu *>
    init(ch: VBT.T; 
      action: Proc; 
      ref: REFANY := NIL): T; 
  END;
  
  Proc = 
    PROCEDURE(self: T; READONLY cd: VBT.MouseRec);
    <* LL.sup = VBT.mu *>

(* The call "v.init(...)" initializes "v" with child "ch" and action
   proc "action" and adds "ref" to "v"'s property set if it is not
   "NIL".  The action procedure can access "ref" (if it is not "NIL")
   by calling "VBT.GetProp".

   The mouse and position methods of a "ButtonVBT.T" call the
   "pre" method on a down click, and then call the "cancel" method if
   the user chords by clicking another mouse button or if the user
   moves the mouse out of the button.  Otherwise they call the action
   procedure "proc" if the user releases the mouse button.

   The default "pre" method highlights the button, the default "post" and 
   "cancel" methods unhighlight it.  Consequently there should be a 
   "HighlightVBT" somewhere above the button.  Since "Trestle.Install"
   automatically inserts a "HighlightVBT", you usually don't have
   to worry about this.

   The action procedure is a field rather than a method in order to 
   allow buttons with different action procedures to share their 
   method suites. *)

PROCEDURE New(
  ch: VBT.T; 
  action: Proc; 
  ref: REFANY := NIL): T; <* LL.sup = VBT.mu *>
(* "New(...)" is equivalent to "NEW(T).init(...)". *)

PROCEDURE MenuBar(
  ch0, ch1, ch2, ch3, ch4, ch5, 
    ch6, ch7, ch8, ch9: VBT.T := NIL;
  op: PaintOp.T := PaintOp.Bg)
  : PackSplit.T; <* LL.sup = VBT.mu *>
(* Return a "PackSplit" with the given children, left-justified,
   and with its background painted with "op". *)

(* "MenuBar" is convenient for building a horizontal row of buttons.
   If the row fills up, the extra buttons will wrap to the next line. *)

END ButtonVBT.
