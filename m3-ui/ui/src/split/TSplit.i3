(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* PackSplit.i3 by cgn & msm, coded Sat Nov  1 18:19:34 1986 *)
(* Last modified on Mon Feb 24 13:54:47 PST 1992 by muller  *)
(*      modified on Thu Dec 12  1:07:03 PST 1991 by gnelson *)
<*PRAGMA LL*>

(* A "TSplit.T" is a parent window that giving its entire screen to
   one child at a time.  The child being displayed is called the {\it
   current child}.  The current child can be "NIL", in which case the
   "TSplit" ignores all events.  *)

INTERFACE TSplit;

IMPORT VBT, Split;

TYPE
  T <: Public;
  Private <: Split.T;
  Public = Private OBJECT METHODS
    <* LL.sup <= VBT.mu *>
    init(fickle := TRUE): T
  END;

(* The call "v.init(fickle)" initialize "v" as an empty "TSplit".

   If "fickle" is "TRUE", then the shape of "v" will be the shape of
   its current child, or a "VBT"'s default shape if the current child
   is "NIL".  If "fickle" is "FALSE", then in each axis the size range
   of "v" will be the intersection of the size ranges of its children (if
   this intersection is empty, the children's maxsizes are increased
   until the intersection is non-empty). The preferred size of "v" is
   the the maximum of the preferred sizes of its children, projected
   into "v"'s size range.  If "v" has no children, its shape is a 
   "VBT"'s default shape.  *)


PROCEDURE SetCurrent(v: T; ch: VBT.T) 
RAISES {Split.NotAChild}; <* LL.sup = VBT.mu *>
(* Set the current child of "v" to be "ch" and mark "v" for redisplay. *)

PROCEDURE GetCurrent(v: T): VBT.T; <* LL.sup = VBT.mu *>
(* Return the current child of "v". *)

PROCEDURE Cons(ch0, ch1, ch2, ch3, ch4: VBT.T := NIL; 
  fickle := TRUE): T; <* LL.sup = VBT.mu *>
(* Create a "TSplit" with children "ch0", "ch1", .... *)

(* "Cons" is equivalent to

| v := NEW(T).init(fickle);
| Split.AddChild(v, ch0, ch1, ch2, ch3, ch4);  
| IF ch0 # NIL THEN SetCurrent(v, ch0) END;
| RETURN v

*)

END TSplit.
