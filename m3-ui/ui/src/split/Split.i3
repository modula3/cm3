(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Mar 10 18:59:47 1992 by steveg     *)
(*      modified on Mon Feb 24 13:54:29 PST 1992 by muller     *)
(*      modified on Sat Dec 21 16:13:22 PST 1991 by gnelson    *)

<*PRAGMA LL*>

(* The Split interface provides the functionality that is common 
   to all splits; for example, enumerating and deleting children.  
   
   This interface is for clients of splits; see the "VBTClass" and
   "ProperSplit" interfaces for information about implementing your own 
   split classes.  *)

INTERFACE Split;

IMPORT VBT, Point;

TYPE T = VBT.Split;

EXCEPTION NotAChild;

(* A "Split.T" is a "VBT" that divides its screen up between one or more 
   child "VBTs".  
   The children of a split are ordered; they can be enumerated
   with the "Succ" and "Pred" procedures: *)

PROCEDURE Succ(v: T; ch: VBT.T): VBT.T 
RAISES {NotAChild}; <* LL >= {VBT.mu} *>
(* Return the child of "v" that follows the child "ch". *)

(* The successor of "NIL" is the first child; the successor of the last
   child is "NIL"; the successor of "NIL" is "NIL" if there are no
   children.  The exception "NotAChild" is raised if "ch" is not a child
   of "v".  *)


PROCEDURE Pred(v: T; ch: VBT.T): VBT.T 
RAISES {NotAChild}; <* LL >= {VBT.mu} *>
(* Return the child of "v" that precedes the child "ch". *)

(* More precisely, "Pred(v,ch) = x" iff "Succ(v,x) = ch".  All of
  Trestle's standard splits implement "Succ" and "Pred" in constant
  time.  *)

PROCEDURE NumChildren(v: T): CARDINAL;
<* LL >= {VBT.mu} *>
(* Return the number of children of "v". *)

PROCEDURE Nth(v: T; n: CARDINAL): VBT.T;
<* LL >= {VBT.mu} *>
(* Return the child of "v" with index "n". *)

(* More precisely, "Nth(v, n)" is the child of "v" with "n"
   predecessors, or "NIL" if "v" has at most "n" children.  Warning:
   for Trestle's standard splits, "Nth" requires time proportional to
   "n", so it would be wasteful to enumerate the children by calling
   it repeatedly; use "Succ" instead.  *)


PROCEDURE Index(v: T; ch: VBT.T): CARDINAL
RAISES {NotAChild}; <* LL >= {VBT.mu} *>
(* Return the index of "v"'s child "ch". *)

(* "Index(v, ch)" is the value "n" such that "Nth(v, n) = ch".  
   "Index(v, NIL)" equals "NumChildren(v)".  *)


PROCEDURE Locate(v: T; READONLY pt: Point.T): VBT.T;
<* LL.sup = VBT.mu *>
(* Return the child of "v" that controls the point "pt", 
   or "NIL" if there is no such child. *)
   
PROCEDURE Delete(v: T; ch: VBT.T) RAISES {NotAChild};
<* LL.sup = VBT.mu *>
(* Delete the child "ch" of the split "v", detach "ch", and mark "v"
   for redisplay.  *)

PROCEDURE Replace(v: T; ch, new: VBT.T)
RAISES {NotAChild}; <* LL.sup = VBT.mu *>
(* Replace child "ch" of "v" with "new", detach "ch" (which must not be "NIL"),
   and mark "v" for redisplay.  *)

PROCEDURE Insert(v: T; pred, new: VBT.T)
RAISES {NotAChild}; <* LL.sup = VBT.mu *>
(* Add "new" as a child of "v" following "pred". *)

(* Some split classes can accomodate only a bounded number of children
  (for example, filters).  If "Insert(v, pred, new)" is applied to
  a split "v" that cannot accomodate an additional child, then "pred"
  (or the original first child, if "pred = NIL") is deleted from the
  split and discarded.  The precise semantics are defined by the
  individual splits.  "Insert" raises "NotAChild" if "pred" isn't a
  child of "v", and is a checked run-time error if "new" isn't detached.
  *)

PROCEDURE Move(v: T; pred, ch: VBT.T)
RAISES {NotAChild}; <* LL.sup = VBT.mu *>
(* Move child "ch" of "v" to follow "pred".  Both "ch" and (if non-"NIL") 
   "pred" must be children of "v".  *)

PROCEDURE AddChildArray(v: T; 
  READONLY new: ARRAY OF VBT.T);
<* LL.sup = VBT.mu *>
(* Insert the non-"NIL" elements of "new" at the end of the "v"'s 
   list of children.  *)

(* "AddChildArray" is equivalent to
   
|  pred := Pred(v, NIL);
|  FOR i := 0 TO LAST(new) DO
|    IF new[i] # NIL THEN
|      InsertAfter(v, pred, new[i]);
|      pred := new[i]
|    END
|  END
 *)

PROCEDURE AddChild(v: T;
  v0, v1, v2, v3, v4, v5, v6, v7, v8, v9: VBT.T := NIL);
<* LL.sup = VBT.mu *>
(* Add the given children to "v". *)

(* "AddChild" is equivalent to 
|  AddChildArray(v, 
|    ARRAY OF VBT.T{v0, v1, ..., v9})
*)

END Split.
