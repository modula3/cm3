(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Sep 26 14:54:38 1995 by dagenais                   *)
(*      modified on Tue Mar 10 19:00:43 1992 by steveg                   *)
(*      modified on Mon Feb 24 13:52:16 PST 1992 by muller                   *)
<*PRAGMA LL*>

(* An "AnchorHelpVBT.T" is a VBT which activates a pop up help window
   upon entry, after some delay. It is used for providing help bubbles. 

   When the position remains inside an AnchorHelpVBT for more than
   "inDelay" seconds, the help window pops up. Thereafter, help
   windows in any AnchorHelpVBT within the same Trestle top level window
   pop up directly upon entry. This behavior stops whenever the position
   remains outside any AnchorHelpVBT for more than "outDelay" seconds.

   The "position" method may be overridden to obtain a different behavior.

   Associated with each anchor VBT "v" is 
   
   \medskip\bulletitem "v.help", the help window to be activated, 
   
   \medskip\bulletitem "v.hfudge" and "v.vfudge", dimensions in millimeters
   that control where the window is popped up,
   
   \medskip\bulletitem "v.n", a count of the number of "ZSplit" ancestors
   of "v" to skip when looking for the "ZSplit" to insert the
   help window into.

   When the help window is activated, it is inserted
   into the ("v.n")th "ZSplit" ancestor of
   "v" (counting the first "ZSplit" ancestor as zero), or as an
   undecorated top-level window if "v" has at most "v.n" "ZSplit"
   ancestors. It is positioned so that its
   northwest corner is "v.hfudge" millimeters to the right and
   "v.vfudge" millimeters below the southwest corner of "v".

   The same help window can be associated with several anchors, provided
   that only one of them is active at a time.  *)


INTERFACE AnchorHelpVBT;

IMPORT VBT, Filter;

TYPE 
  T <: Public;
  Public = Filter.T OBJECT 
    help: VBT.T 
  METHODS <* LL.sup <= VBT.mu *>
    init(ch: VBT.T; 
      help: VBT.T;
      n: CARDINAL := 0;
      hfudge := 0.0;
      vfudge := 1.0): T
  END;
  
(* The call "v.init(...)" initializes the vbt with the given
   attributes. This includes a call to "Filter.T.init(v, ch)". 
   
   You must not change the help while the "AnchorHelpVBT" is active.*)


PROCEDURE New(
  ch: VBT.T; 
  help: VBT.T;
  n: CARDINAL := 0;
  hfudge, vfudge := 2.0): T; <* LL.sup <= VBT.mu *>
(* "New(...)" is equivalent to "NEW(T).init(...)". *)

PROCEDURE Activate(v: T); 
<* LL.sup = VBT.mu *>
(* Activate the help window. *)

PROCEDURE Deactivate(v: T); 
<* LL.sup = VBT.mu *>
(* Deactivate the help window. *)

PROCEDURE Enter(v: T); 
<* LL.sup = VBT.mu *>
(* Enter the help window. It gets activated after some delay. *)

PROCEDURE Leave(v: T); 
<* LL.sup = VBT.mu *>
(* Leave the help window. It gets deactivated. *)

PROCEDURE Set(v: T;  n: CARDINAL; hfudge, vfudge: REAL); 
<* LL.sup = VBT.mu *>
(* Set the attributes of "v".  If "v" is active, this is a 
   checked runtime error. *)

PROCEDURE Get(v: T; VAR n: CARDINAL; 
  VAR hfudge, vfudge: REAL); <* LL.sup = VBT.mu *>
(* Fetch the attributes of "v". *)
 
PROCEDURE IsActive(v: T): BOOLEAN; <* LL.sup = VBT.mu *>
(* Return "TRUE" if and only if "v" is active. *)

PROCEDURE GetDelay(v: VBT.T; VAR inDelay, outDelay: LONGREAL);
<* LL.sup = VBT.mu *>
(* Fetch the delay values for the VBT tree. *)

PROCEDURE SetDelay(v: VBT.T; inDelay, outDelay: LONGREAL);
<* LL.sup = VBT.mu *>
(* Change the delay values for the VBT tree. *)

END AnchorHelpVBT.


 
