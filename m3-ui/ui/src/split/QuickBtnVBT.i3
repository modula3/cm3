(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Feb 24 13:54:07 PST 1992 by muller                   *)
<*PRAGMA LL*>

(* A "QuickBtnVBT.T" is a button that activates immediately on down-clicks. 
   Quick buttons are useful for boolean toggles and radio buttons. 

   A "QuickBtnVBT" has its "pre", "action", and "post" methods called on 
   every mouse click of type "FirstDown" in its domain.  Its "cancel" 
   method is never called.  Its default "pre" and "post" methods are 
   no-ops. *)


INTERFACE QuickBtnVBT;

IMPORT ButtonVBT, VBT;

TYPE T <: ButtonVBT.T;

(* The call "v.init(ch, action, ref)" initializes "v" as a quick button 
   with child "ch" and action procedure "action", and adds "ref" to
   "v"'s property set if it is not "NIL". *)
   
PROCEDURE New(
  ch: VBT.T; 
  action: ButtonVBT.Proc; 
  ref: REFANY := NIL): T; <* LL.sup = VBT.mu *>
(* "New(...)" is equivalent to "NEW(T).init(...)". *)

END QuickBtnVBT.
