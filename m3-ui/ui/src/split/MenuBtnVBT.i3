(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Feb 24 13:53:46 PST 1992 by muller                   *)
<*PRAGMA LL*>

(* A "MenuBtnVBT.T" is a button suitable for the items in pop-up
   and pull-down menus.

   When the cursor rolls into a menu button, the "pre" method is called
   and the button is {\it readied}.  If it receives a mouse transition
   of type "LastUp" while it is readied, the "action" and "post" methods
   are called.  The "cancel" method is called if the cursor leaves the
   button or the user chords with the mouse while the button is readied
   .  *)

INTERFACE MenuBtnVBT;

IMPORT ButtonVBT, VBT;

TYPE T <: ButtonVBT.T;

(* The call "v.init(ch, action, ref)" initializes "v" as a menu button 
   with child "ch" and action procedure "action", and adds "ref" to
   "v"'s property set if it is not "NIL". *)

PROCEDURE New(
  ch: VBT.T; action: ButtonVBT.Proc; 
  ref: REFANY := NIL): T; <* LL.sup = VBT.mu *>
(* "New(...)" is equivalent to "NEW(T).init(...)". *)

PROCEDURE TextItem(
  name: TEXT; action: ButtonVBT.Proc; 
  ref: REFANY := NIL): T; <* LL.sup = VBT.mu *>
(* Return a menu button that displays the text "name". *)

(* "TextItem" is a convenience procedure for making a menu button with
   a "TextVBT" child.  The borders are initialized to make the button
   suitable for stacking into a menu using a vertical "HVSplit".  More
   precisely, "TextItem" is equivalent to:

| New(TextVBT.New(name, 0.0, 0.5, 3.0, 0.5), 
|     action, ref)

*)

END MenuBtnVBT.
