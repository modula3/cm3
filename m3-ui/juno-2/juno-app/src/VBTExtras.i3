(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Dec 21 18:31:47 PST 1994 by heydon                   *)

INTERFACE VBTExtras;

(* Extensions to the "VBT" interface. *)

IMPORT VBT, Font, Rect;

PROCEDURE TightBoundingBox(v: VBT.Leaf; txt: TEXT; fnt: Font.T): Rect.T;
(* Return the smallest rectangle enclosing the ink resulting from painting
   "txt" at the origin on "v"'s screentype in the font "fnt". If "v"'s
   screentype is "NIL", the result is a rectangle whose southwest corner is
   the origin, and whose height is 1 and whose width is the length of "txt".
   Requires "v # NIL" and "txt # NIL". *)

END VBTExtras.
