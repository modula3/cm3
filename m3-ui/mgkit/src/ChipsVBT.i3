(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Sun Jul 19 19:26:53 1992 by mhb *)
<* PRAGMA LL *>

(* A ChipsVBT continuously displays an [1..C]x[1..R] array of
   colored rectangles representing values [1..N].  The rectangle
   at the lower left is (1,1), and one at the upper right is
   (C,R) the This VBT class gets its name from the collection of
   ``paint chips'' it resembles. *)

INTERFACE ChipsVBT;

IMPORT RectsVBT;

TYPE
  T <: Public;
  Public = RectsVBT.T OBJECT METHODS init ():T END;

PROCEDURE Reset (v: T; C, R, K: INTEGER);
<* LL.sup < v *>
(* Clear the current display and get ready to show [1..R] rows of
   [1..C] chips per row, each displaying a value in [1..K]. *)

PROCEDURE Set (v: T; c, r, k: INTEGER);
<* LL.sup < v *>
(* Set the value of chip at column "c" in row "r" to be "k" and
   display it appropriately.  If any of the values are out of
   bounds, this call is a noop. *)

END ChipsVBT.

