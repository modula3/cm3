(* Copyright (C) 1992, Digital Equipment Corporation                        *)
(* All rights reserved.                                                     *)
(* See the file COPYRIGHT for a full description.                           *)
(*                                                                          *)
(* Last modified on Thu Jun 10 13:57:31 PDT 1993 by meehan                  *)
(*      modified on Mon Feb  1 00:52:05 PST 1993 by mhb                     *)
(*      modified on Tue Jun 16 13:08:35 PDT 1992 by muller                  *)
<* PRAGMA LL *>

(* An arbitrary VBT is made into a multi by providing a set of
   methods for maintaining the logical structure. The methods are used
   for replacing, inserting, traversing, and performing other common
   operations on the children.

   In a language with multiple inheritance, multis would simply
   inherit different methods from different parent-types.  In
   Modula-3, however, we achieve this effect by creating an instance
   "mc" of type "MultiClass.T", and attaching "mc" to a VBT "v" by way
   of "v"'s property set.  The object "mc" points back to "v" via the
   field "mc.vbt".

   Clients defining their own multis can make a VBT "v" ``into'' a
   multi by calling "Be(v,mc)" during the initialization of the VBT.
   They must call "BeChild" on each new child when it is inserted, and
   "UnChild" when a child of a multi is deleted.
   "MultiFilter.Replace", "MultiSplit.Replace", and
   "MultiSplit.Insert" all do this automatically, and
   "MultiSplit.Insert" calls "BeChild". *)

INTERFACE MultiClass;

IMPORT RefList, VBT;

TYPE
  T = ROOT OBJECT
        vbt: VBT.T;              (* READONLY *)
      METHODS
        <* LL = VBT.mu *>
        replace (ch, new: VBT.T);
        insert  (pred, new: VBT.T);
        move    (pred, ch: VBT.T);
        succ    (ch: VBT.T): VBT.T;
        pred    (ch: VBT.T): VBT.T;
        nth     (n: CARDINAL): VBT.T;
        index   (ch: VBT.T): CARDINAL;
      END;

(* \subsubsection{The MultiSplit methods} *)

(* The methods implement the behavior in the "MultiSplit"
   interface.

   The method call "mc.replace(ch,new)" implements the operation
| MultiSplit.Replace(mc.vbt, ch, new)
   and the call "mc.replace(ch,NIL)" implements
| MultiSplit.Delete(mc.vbt, ch)
   Before calling the method, the generic code in the "MultiSplit"
   interface checks that "ch" is a multi-child of "mc.vbt", and, if
   "new" is not "NIL", calls "BeChild(mc.vbt, new)".  After calling
   the method, the generic code calls "UnChild(mc.vbt, ch)", if "ch"
   was not "NIL".

   Similarly, the method call "mc.insert(pred,new)" implements the
   operation 
| MultiSplit.Insert(mc.vbt, pred, new) 
   Before calling the method, the generic code in "MultiSplit" checks
   that "pred" is a multi-child of "mc.vbt" and calls "BeChild(mc.vbt,
   new)".  If "new" is "NIL", "MultiSplit.Insert" raises a runtime
   exception.

   The default methods for "replace" and "insert" are both equal
   to "NIL", so every multi-split needs to override these
   methods.

   The method call "mc.move(pred, ch)" implements
| MultiSplit.Move(mc.vbt, pred, ch)
   Before calling the method, the generic code in "MultiSplit"
   verifies that "ch" and "pred" are both multi-children of "mc.vbt"
   (or "NIL", in the case of "pred").  The call to "mc.move" is
   avoided if "pred=ch" or "mc.succ(pred)=ch".

   The default "move" method for a "MultiClass.T" object "mc" is
   simply a call to "mc.replace(ch, NIL)" followed by a call to
   "mc.insert(pred, ch)".

   This default method is naive on two fronts. One, it is not
   particularly efficient since the tree of VBTs is typically being
   manipulated twice. Two, and more importantly, some multi-splits
   will take action as part of the "replace" method (e.g.,
   reallocating the screen layout of its children) that is not
   ``undone'' by the subsequent call to the "insert" method.

   The method calls 

| mc.succ(ch)
| mc.pred(ch)
| mc.nth(n)
| mc.index(ch)

   all implement the corresponding operations in the "MultiSplit"
   interface.  The default "pred", "nth" and "index" methods are
   implemented by repeatedly calling the "succ" method.  The default
   "succ" method finds the successor of "ch" for the "MultiClass.T"
   object "mc" by a depth-first walk of "mc.vbt"'s descendants,
   starting after "ch", and stopping at the first VBT "w" for which
   "IsChild(mc.vbt, w)" returns "TRUE", or when all of "mc.vbt"'s
   descendants have been visited, in which case, "ch" has no successor
   so "NIL" is returned.  In practice, the default "succ" method seems
   to work nearly all of the time; however, there is often a more
   efficient way to implement a "succ" method for any particular
   multi-split. *)

(* \subsubsection{The MultiFilter methods} *)

TYPE
  Split <: T;
  Filter <: Split;

(* The default methods for a "Filter" are the same as for a
   "Split", except that the "insert" method has a default.  Thus,
   you only need to override the "replace" method of a
   multi-filter.

   The default method call "mc.insert(pred, new)" is

| mc.replace (mc.succ(pred), new)

   Also, the "move" method is never run; the generic code in
   "Split.Move" ensures this. *)

(* \subsubsection{Procedures for creating multis} *)

PROCEDURE Be (v: VBT.T; mc: T);
<* LL.sup <= VBT.mu *>
(* Make "v" into a multi by storing "mc" on "v"'s property set
   and setting "mc.vbt" to "v". *)

PROCEDURE Resolve (v: VBT.T): T;
<* LL.sup < v *>
(* Return the multiclass of "v", that is, the "mc" for which
   "Be(v,mc)" was previously called.  Return "NIL" if there is no
   such "mc". *)

PROCEDURE BeChild (v: VBT.T; ch: VBT.T);
<* LL.sup < ch *>
(* Make "ch" into one of "v"'s children that is exposed to the
   client via the "MultiSplit" or "MultiFilter" interfaces.  
   It is possible for "ch" to
   be a child of more than one multi, and it is possible that
   "ch" is not related to "v" in the VBT hierarchy. *)

PROCEDURE UnChild (v: VBT.T; ch: VBT.T);
<* LL.sup < ch *>
(* Unmark "ch" as one of "v"'s children that is exposed to the
   client via the "MultiSplit" or "MultiFilter" interfaces. *)

PROCEDURE IsChild (v: VBT.T; ch: VBT.T): BOOLEAN;
<* LL.sup < ch *>
(* Return "TRUE" iff "BeChild(v,ch)" was previously invoked and
   "UnChild(v,ch)" has not been subsequently called. *)

PROCEDURE Parents (ch: VBT.T): RefList.T (* of VBT.T *);
<* LL.sup < ch *>
(* Return a list of VBTs for which "IsChild(v,ch)" is "TRUE".
   The list may be "NIL". *)

END MultiClass.




