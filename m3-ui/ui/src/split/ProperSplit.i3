(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Jun  3 14:15:14 PDT 1996 by heydon   *)
(*      modified on Wed Mar  8 18:34:37 PST 1995 by msm      *)
(*      modified on Tue Mar 10 19:01:05 1992 by steveg   *)
(*      modified on Mon Feb 24 13:53:54 PST 1992 by muller   *)
(*      modified on Sun Nov 10 17:34:34 PST 1991 by gnelson  *)

<*PRAGMA LL*>

(* A "ProperSplit.T" is a type of "VBT.Split" that contains a
   circularly-linked list of its children.  All of Trestle's built-in
   splits that are not filters are subclasses of "ProperSplit".  *)
   
INTERFACE ProperSplit;

IMPORT VBT, Split;

TYPE 
  T <: Public;
  Public = VBT.Split OBJECT 
    <* LL >= {SELF, VBT.mu} *>
    lastChild: Child := NIL
  END;
  Child = OBJECT 
    <* LL >= {SELF.ch.parent, VBT.mu} *>
    pred, succ: Child := NIL; 
    ch: VBT.T 
  END;

(* If "ch" is a child of a "ProperSplit.T", then "ch.upRef" must be of
   type "ProperSplit.Child", and "ch.upRef.ch" must equal "ch".  The
   "succ" and "pred" links represent a doubly-linked list of the
   children.  The "succ" links are circular; the "pred" links are
   linear.  The parent's "lastChild" field is is "NIL" if there are
   no children; otherwise it points to the last child in "succ" order.
   
   The locking level comments imply that to write any of the links,
   a thread must have both "VBT.mu" and the parent locked.
   
   If "v" is a "T", the call "v.beChild(ch)" sets "ch.upref"
   to "NEW(Child)" if it is "NIL".  In any case it sets
   "ch.upref.ch := ch" and calls "VBT.Split.beChild(v, ch)". 

   A "T" provides replace, insert, and move methods that preserve the linked
   list structure of children, but allocates no screen space or event
   dispatching to the children.  These methods use the procedures below;
   the replace method uses the default insert method to add the new child,
   which may be inappropriate for some class layout policies. *)
   
(* The following procedures are useful for implementing subtypes
   of "ProperSplit.T": *)

PROCEDURE Insert(v: T; pred: Child; newch: VBT.T);
<* LL >= {VBT.mu, v, newch} *>
(* Insert "newch" as a new child after "pred", and mark "v" for
   redisplay. *)

(* The child "newch" must be detached and of the appropriate
   screentype. The argument "pred" can be "NIL" to indicate
   insertion at the head of the list. "Insert" calls the "beChild"
   method of "newCh".  *)

PROCEDURE PreInsert(v: T; pred, ch: VBT.T): Child 
  RAISES {Split.NotAChild}; <* LL.sup = VBT.mu *>
(* Rescreen "ch" to have "v"'s screentype (if necessary), cause a checked
   runtime error if "ch" is attached, raise "Split.NotAChild" if "pred"
   is non-nil and not a child of "v", and finally return "pred.upRef", or
   "NIL" if "pred" is "NIL".  *)
   
PROCEDURE Move(v: T; pred, ch: Child);
<* LL >= {VBT.mu, v} *>
(* Move "ch" in the list of children so that it follows "pred" and
   mark "v" for redisplay.  *)

PROCEDURE Delete(v: T; ch: Child);
<* LL >= {VBT.mu} AND LL.sup < v  *>
(* Remove "ch" from the list of children, detach "ch.ch", and
   mark "v" for redisplay. *)

END ProperSplit.
