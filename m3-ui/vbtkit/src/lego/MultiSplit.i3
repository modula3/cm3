(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jan 11 10:12:57 PST 1994 by mhb    *)
(*      modified on Mon Jun 14 20:56:02 PDT 1993 by meehan *)
(*      modified on Tue Jun 16 13:08:36 PDT 1992 by muller *)
(*      modified on Fri Mar 27 01:47:38 1992 by steveg*)
<* PRAGMA LL *>

(* The "MultiSplit" interface defines operations that are common to
   all multi-splits, such as enumerating and deleting children. *)

INTERFACE MultiSplit;

IMPORT Point, VBT;

EXCEPTION NotAChild;

TYPE T = VBT.T;
(* A "MultiSplit.T" is a "VBT.T" with a "MultiClass.Split" in its
   property set. *)

(* All of the procedures in this interface can accept either a
   "MultiSplit.T" or a "Split.T" as the first argument.  If the first
   argument is not a "MultiSplit.T", the procedure just calls the
   corresponding procedure in the "Split" interface, re-raising any
   "Split.NotAChild" exceptions as "NotAChild" exceptions.

   Unlike the procedures in the "Split" interface, the procedures here
   do not perform any VBT operations.  For example, "Split.Delete(v, ch)"
   deletes the child "ch" of split "v", detaches "ch", and marks
   "v" for redisplay, whereas "MultiSplit.Delete" just deletes the
   multi-child "ch" of multi-split "v", without detaching "ch" or
   marking "v" for redisplay.  The "MultiClass" methods of "v" that
   implement the "Delete" functionality will most likely manipulate
   the VBT tree using "Split.Delete" (or other calls to "Split" and
   "Filter" as appropriate), so that "v" will be marked and "ch" will
   be detached, as one would expect. *)


PROCEDURE Succ (v: VBT.T; ch: VBT.T): VBT.T 
  RAISES {NotAChild};
<* LL >= {VBT.mu} *>
(* Return the child of "v" that follows the child "ch". *)

(* The successor of "NIL" is the first child; the successor of 
   the last child is "NIL"; the successor of "NIL" is "NIL" if there 
   are no children. *)

PROCEDURE Pred (v: VBT.T; ch: VBT.T): VBT.T 
  RAISES {NotAChild};
<* LL >= {VBT.mu} *>
(* Return the child of "v" that precedes the child "ch". *)

(* More precisely, "Pred(v,ch) = x" iff "Succ(v,x) = ch". *)

PROCEDURE NumChildren (v: VBT.T): CARDINAL 
  RAISES {NotAChild};
<* LL >= {VBT.mu} *>
(* Return the number of children of "v". *)

PROCEDURE Nth (v: VBT.T; n: CARDINAL): VBT.T;
<* LL >= {VBT.mu} *>
(* Return the child of "v" with index "n". *)

(* More precisely, "Nth(v, n)" is the child of "v" with "n" predecessors,
   or "NIL" if "v" has at most "n" children. *)

PROCEDURE Index (v: VBT.T; ch: VBT.T): CARDINAL 
  RAISES {NotAChild};
<* LL >= {VBT.mu} *>
(* Return the index of "v"'s child "ch". *)

(* In other words, "Index(v, ch)" is the value "n" such that "Nth(v, n) = ch".
   It is always true that "Index(v, NIL)" equals "NumChildren(v)". *)

PROCEDURE Locate (v: VBT.T; READONLY pt: Point.T): VBT.T;
<* LL.sup = VBT.mu *>
(* Return the child of "v" that would receive a mouse click at
   point "pt", or "NIL" if there is no such child. *)

PROCEDURE Delete(v: T; ch: VBT.T) 
  RAISES {NotAChild};
<* LL.sup = VBT.mu *>
(* Delete the child "ch" of "v". *)

PROCEDURE Replace (v: VBT.T; ch, new: VBT.T) 
  RAISES {NotAChild};
<* LL.sup = VBT.mu *>
(* Replace child "ch" of "v" with "new". *)

PROCEDURE Insert (v: VBT.T; pred, new: VBT.T) 
  RAISES {NotAChild};
<* LL.sup = VBT.mu *>
(* Add "new" as a child of "v" following "pred". *)

(* \sloppy Some multi-splits can accommodate only a bounded number of
   children.  Whenever "Insert(v,pred,new)" is applied to a multi-split
   "v" that cannot accommodate an additional child, then "pred"
   (or the original first child, if "pred=NIL") is deleted from
   the multi-split.  The precise semantics are defined by the
   individual multi-splits. *)


PROCEDURE Move (v: VBT.T; pred, ch: VBT.T) 
  RAISES {NotAChild};
<* LL.sup = VBT.mu *>
(* Move child "ch" of "v" to follow "pred".  "ch" and, if
   non-"NIL", "pred", must be children of "v". *)

PROCEDURE AddChildArray (
    v: VBT.T; 
    READONLY new: ARRAY OF VBT.T);
<* LL.sup = VBT.mu *>
(* Insert the non-"NIL" elements of "new" at the end of "v"'s 
   list of children. *)

(* Procedure "AddChildArray" is equivalent to 
|  pred := Pred(v, NIL);
|  FOR i := FIRST(new) TO LAST(new) DO
|    IF new[i] # NIL THEN
|      Insert(v, pred, new[i]);
|      pred := new[i]
|    END
|  END
   *)

PROCEDURE AddChild (
    v: VBT.T;
    n0, n1, n2, n3, n4, n5, n6, n7, n8, n9: VBT.T := NIL);
<* LL.sup = VBT.mu *>
(* Insert the non-"NIL" parameters as children to "v". *)

(* Procedure "AddChild" is equivalent to 
|  AddChildArray(v,
|    ARRAY OF VBT.T{n0, n1, n2, n3, n4, n5, n6, n7, n8, n9})
   *)

END MultiSplit.






















