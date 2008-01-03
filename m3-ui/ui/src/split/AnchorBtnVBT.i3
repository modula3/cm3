(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Mar 10 19:00:43 1992 by steveg                   *)
(*      modified on Mon Feb 24 13:52:16 PST 1992 by muller                   *)
<*PRAGMA LL*>

(* An "AnchorBtnVBT.T "is a button that activates a pull-down menu when 
   you click on it or roll into it from another anchor button. 

   Associated with each anchor button "b" is 
   
   \medskip\bulletitem "b.menu", the menu to be activated, 
   
   \medskip\bulletitem "b.hfudge" and "b.vfudge", dimensions in millimeters
   that control where the menu is popped up,
   
   \medskip\bulletitem "b.n", a count of the number of "ZSplit" ancestors
   of "b" to skip when looking for the "ZSplit" to insert the
   menu into.

   \medskip\noindent A down click on an anchor button "b" {\it activates} 
   it by: 
   
   \medskip\bulletitem  calling the method "b.pre()", and then   

   \medskip\bulletitem inserting the window "b.menu" so that its
   northwest corner is "b.hfudge" millimeters to the right and
   "b.vfudge" millimeters below the southwest corner of "b".
   The menu will be inserted into the ("b.n")th "ZSplit" ancestor of
   "b" (counting the first "ZSplit" ancestor as zero), or as an
   undecorated top-level window if "b" has at most "b.n" "ZSplit"
   ancestors.

   \medskip\noindent The anchor button will be deactivated when it gets
   another mouse transition or when the user rolls the mouse over a
   sibling anchor button, in which case the sibling will be activated.
   Two anchor buttons are siblings if they have the same ``anchor parent''.
   The anchor parent is specified when the anchor button is created;
   if it is "NIL", then the normal parent is used as the anchor parent.
   When an anchor button is deactivated, its cancel method is called
   and its menu is deleted from its "ZSplit".
   
   The default "pre" method highlights the anchor button; the default
   "cancel" method unhighlights it.

   In the common case in which the user down-clicks on the anchor, rolls
   over the menu, and up-clicks on one of the items, the upclick will
   be delivered to the item first, which will invoke the appropriate
   action, and then will be delivered to the anchor button (since the
   anchor button has the mouse focus), which will delete the menu.

   A "HighlightVBT" is automatically inserted over the menu when it is 
   inserted, and discarded when the menu is deleted.  This allows the
   menu items to highlight themselves without interfering with the 
   highlighting of the anchor button.

   The "action" procedure and "post" method of an anchor button are never
   called.  The "pre" and "cancel" methods can be overridden; for
   example, the "pre" method could prepare the menu before it is
   inserted.  This is the reason the menu field is revealed in the type
   declaration.

   The same menu can be associated with several anchor buttons, provided
   that only one of them is active at a time.  *)


INTERFACE AnchorBtnVBT;

IMPORT ButtonVBT, VBT;

TYPE 
  T <: Public;
  Public = ButtonVBT.T OBJECT 
    menu: VBT.T 
  METHODS <* LL.sup <= VBT.mu *>
    init(ch: VBT.T; 
      menu: VBT.T;
      n: CARDINAL := 0;
      anchorParent: VBT.T := NIL;
      hfudge, vfudge := 0.0;
      ref: REFANY := NIL): T
  END;
  
(* The call "v.init(...)" initializes the button with the given
   attributes, and adds "ref" to "v"'s property set if it is not "NIL".
   This includes a call to "ButtonVBT.T.init(v, ch)". 
   
   You must not change the menu while the "AnchorBtnVBT" is active.*)


PROCEDURE New(
  ch: VBT.T; 
  menu: VBT.T;
  n: CARDINAL := 0;
  anchorParent: VBT.T := NIL;
  hfudge, vfudge := 0.0;
  ref: REFANY := NIL): T; <* LL.sup <= VBT.mu *>
(* "New(...)" is equivalent to "NEW(T).init(...)". *)

PROCEDURE SetParent(v: T; p: VBT.T); 
<* LL.sup = VBT.mu *>
(* Set the anchor parent of "v" to be "p".  If "v" is active, this is a 
   checked runtime error. *)

PROCEDURE GetParent(v: T): VBT.T; <* LL.sup = VBT.mu *>
(* Return the anchor parent of "v". *)

PROCEDURE Set(v: T;  n: CARDINAL; hfudge, vfudge: REAL); 
<* LL.sup = VBT.mu *>
(* Set the attributes of "v".  If "v" is active, this is a 
   checked runtime error. *)

PROCEDURE Get(v: T; VAR n: CARDINAL; 
  VAR hfudge, vfudge: REAL); <* LL.sup = VBT.mu *>
(* Fetch the attributes of "v". *)
 
PROCEDURE IsActive(v: T): BOOLEAN; <* LL.sup = VBT.mu *>
(* Return "TRUE" if and only if "v" is active. *)

END AnchorBtnVBT.


 
