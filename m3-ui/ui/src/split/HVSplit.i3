(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* File HVSplit.i3 coded by MSM and CGN, Mon Nov 18 21:52:15 1985 *)
(* Last modified on Tue Mar 10 19:00:23 1992 by steveg  *)
(*      modified on Mon Feb 24 13:53:24 PST 1992 by muller  *)
(*      modified on Sat Dec 21 16:14:45 PST 1991 by gnelson *)
(*      modified on Thu Sep  6 21:11:15 PDT 1990 by msm *)
(*      modified on Fri Feb  2 14:01:44 PST 1990 by glassman *)
<*PRAGMA LL*>

(* An "HVSplit.T" is a parent window that splits its screen into a
   row or column of child windows, depending on the {\it axis} of
   the split.

   If the axis is horizontal, "Split.Succ" enumerates the children 
   from west to east; if the axis is vertical, it enumerates them 
   from north to south.

   An "HVSplit" can be {\it adjustable} or {\it unadjustable}, a 
   property that affects the way its space is divided between its children.    

   The {\it size} of a child is the extent of its domain in the 
   axis of split, the {\it cross-size} is its extent in the other axis.  
   For example, for a vertical split, a child's size is its height 
   and its cross-size is its width.

   The children of an "HVSplit" all have the same cross-size as the
   parent.  To determine the sizes of the children, the "HVSplit" begins
   by computing the range of desirable sizes and the preferred size
   for each child by calling its shape method, passing the method the
   cross-size, so that, for example, the height of a child of a vertical
   split can depend on its width.  At this point there are several cases.

   If the sum of the minimum sizes of the children is greater than the
   size of the parent, then the split is said to be {\it overfull}.
   In this case the children are considered in order and given their
   minimum sizes, as long as there is room.  The first child that
   doesn't fit is given all the space that's left, and the remaining
   children are given size zero.

   If the split is not overfull, then the children are stretched
   according to the TeX model of boxes and glue.  The details depend
   on whether the split is adjustable or unadjustable.  For an
   adjustable split, each child's {\it stretchability} is its maximum
   desirable size minus its current size, and its {\it shrinkability}
   is its current size minus its minimum desirable size.  If the size
   of the parent is increased by some amount "X", then the sizes of
   the children are increased by amounts that total to "X" and are
   proportional to the children's stretchabilities.  Similarly, if the
   size of the parent is decreased by some amount "X", then the sizes
   of the children are decreased by amounts that total to "X" and are
   proportional to the children's shrinkabilities.

   For a non-adjustable split, all the children's sizes are first set
   to their preferred sizes, and then they are stretched or shrunk the
   same as an adjustable split.  Thus for a non-adjustable split each
   redistribution of space depends only on the children's shape methods,
   not on their current sizes.

   A non-adjustable split is best if the layout can be controlled purely
   by stretchability and shrinkability.  If the layout is also changed
   under user or program control, an adjustable split is required.  For
   example, in a column of editable text windows, you should make the
   vertical split adjustable, since if the user makes one window big,
   and then the parent changes size slightly, you do not want the big
   window child to snap back to being small.  On the other hand if you
   are using a horizontal split to center a "ButtonVBT" between two
   stretchy "TextureVBTs", you should make it unadjustable, since in
   this case you always want to compute the division of space from the
   children's original shapes.

   If the sum of the maximum sizes of the children is less than the
   size of the parent, the split is said to be {\it underfull}.  There
   are no special rules for the underfull case: the TeX stretching
   algorithm is used without change.  This produces a state in which
   the children are stretched larger than their maximum sizes.

   A split is {\it infeasible} if it is overfull or underfull, and {\it
   feasible} otherwise.
    
   The shape of an "HVSplit" is computed as follows: its maximum,
   minimum, and preferred sizes are obtained by adding up the
   corresponding values of its children.  The cross-size range is the
   intersection of the cross-size ranges of its children (if this
   intersection is empty, the children's maximum cross-sizes are
   increased until the intersection is non-empty).  The preferred
   cross-size of "v" is the maximum of the preferred cross-sizes
   of its children, projected into "v"'s cross-size range.  *)

   
INTERFACE HVSplit;

IMPORT VBT, Split, Axis, Interval;

TYPE
  T <: Public;
  Private <: Split.T;
  Public = Private OBJECT METHODS
    <* LL.sup <= VBT.mu *>
    init(hv: Axis.T;
      saveBits := FALSE;
      parlim := -1;
      adjustable := TRUE): T
  END;

(* The call "v.init(...)" initializes "v" as an "HVSplit" with axis 
   "hv" and no children.

   If "saveBits" is "TRUE", the implementation will try to save the
   children's old bits when reshaping; if the children don't use them
   anyway, it is faster to let "saveBits" default to "FALSE".  The value of
   "parlim" is the minimum area of a child for which a separate thread
   will be forked to "reshape" or "repaint"; if it is "-1", it is set
   to an appropriate default (see the "VBTTuning" interface).  *)


PROCEDURE New(
    hv: Axis.T;
    saveBits := FALSE;
    parlim := -1;
    adjustable := TRUE): T;
<* LL.sup <= VBT.mu *>
(* "New(...)" is equivalent to "NEW(T).init(...)". *)


PROCEDURE AxisOf(v: T): Axis.T; 
<* LL.sup = VBT.mu *>
(* Return the axis of "v". *)

(* \subsubsection{Inserting children} *)

(* See the "Split" interface to insert and reorder children. *) 

PROCEDURE Cons(
    hv: Axis.T;
    ch0, ch1, ch2, ch3, ch4, 
      ch5, ch6, ch7, ch8, ch9: VBT.T := NIL;
    saveBits := FALSE;
    parlim := -1;
    adjustable := TRUE): T; <* LL.sup = VBT.mu *>
 (* Create an "HVSplit" with axis "hv" and children "ch0", "ch1", .... *) 
 
(* "Cons" is equivalent to the following:

| result := New(hv, saveBits, parlim, adjustable);
| Split.AddChild(result, ch0, ch1, ..., ch9);
| RETURN result

*)


PROCEDURE ConsArray(
    hv: Axis.T;
    READONLY ch: ARRAY OF VBT.T;
    saveBits := FALSE;
    parlim := -1;
    adjustable := TRUE): T; <* LL.sup = VBT.mu *>
(* Create an "HVSplit" with axis "hv" and children "ch[0]", "ch[1]", .... *)

(* "ConsArray" ignores any "NILs" in the array "ch".  It is equivalent to:

| VAR result := New(hv, saveBits, parlim, adjustable);
| BEGIN
|   Split.AddChildArray(result, ch);
|   RETURN result
| END

  *)

(* \subsubsection{Adjusting the division of space} *)

(* The {\it division point after a child} is the sum of the sizes of
   all children up to and including the child. *)
   

PROCEDURE Adjust(v: T; ch: VBT.T; totsz: INTEGER) 
RAISES {Split.NotAChild}; <* LL.sup = VBT.mu *>
(* Change the sizes of the children of "v" so that the division point
   after "ch" is as close to "totsz" as possible, and mark "v" for
   redisplay. *)

(* "Adjust" respects the size constraints on the children, and resizes
   children near the division point in preference to children far from
   the division point.  For example, a sufficiently small adjustment
   will be made by resizing only "ch" and its successor.  An adjustment
   large enough to make one of these children reach its max or min size
   will also resize the neighbor of that child, and so forth.

   "Adjust" is a no-op if the split is infeasible or non-adjustable. *)


PROCEDURE FeasibleRange(v: T; ch: VBT.T): Interval.T 
RAISES {Split.NotAChild}; <* LL.sup = VBT.mu *>
(* Return the interval of feasible positions for the division point
   after "ch". *)
   

PROCEDURE AvailSize(v: T): CARDINAL;
<* LL.sup = VBT.mu *>
(* Return the largest size of a child that can be inserted into "v"
   without making "v" infeasible. *)

(* If the split is infeasible, "AvailSize" returns 0 and "FeasibleRange" 
   returns the empty interval.  Both procedures assume the 
   total size available is the total of all child sizes. *)


END HVSplit.
