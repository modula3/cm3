(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* File: HighlightVBT.i3, by cgn, Tue Apr 21 22:00:25 1987 *)
(* Last modified on Mon Feb 24 13:53:30 PST 1992 by muller  *)
(*      modified on Wed Dec 11 18:32:16 PST 1991 by gnelson *)
(*      modified on Wed Sep 11 15:26:35 PDT 1991 by msm *)
(*      modified on Fri Feb  2 14:02:09 PST 1990 by glassman *)
<*PRAGMA LL*>

(* A "HighlightVBT.T" is a filter that highlights a rectangular outline
   over its child.

   The parent screen is obtained from the child screen by
   texturing an outline inset in a rectangle, using an inverting
   painting operation.
    
   The parent keeps its screen correct as the child paints.  Since the
   parent screen is always correct, it never needs to mark itself
   for redisplay.  *)

INTERFACE HighlightVBT;

IMPORT VBT, Rect, Point, Filter, Pixmap, PaintOp;

TYPE
  T <: Public;
  Public = Filter.T OBJECT METHODS
    <* LL.sup <= VBT.mu *>
    init(ch: VBT.T; 
      op: PaintOp.T := PaintOp.TransparentSwap;
      txt: Pixmap.T := Pixmap.Gray;
      READONLY delta := Point.T{h := 0, v := 1}): T
  END;

(* The call "v.init(ch, ...)" initializes "v" as a "HighlightVBT" with 
   child "ch" and the given parameters, and returns "v".

   The highlight rectangle is initially empty.  The filter brings up 
   the highlight by calling 
   
| VBT.PaintTexture(v, `highlight region`, op, txt, delta)

   and brings down the highlight the same way; therefore the painting 
   operation must be its own inverse for the filter to work correctly.
   
   The default values for the texture and delta are such that the
   highlight will be visible over white, black, or the standard gray
   texture.  (If delta were "(0,0)" instead of "(0,1)", the highlight
   would look fine over white or black but would be barely noticeable
   over standard gray.)  *)

PROCEDURE New(
  ch: VBT.T;
  op := PaintOp.TransparentSwap;
  txt: Pixmap.T := Pixmap.Gray; 
  READONLY delta := Point.T{h := 0, v := 1}) : T ;
<* LL.sup <= VBT.mu *>
(* "New(...)" is equivalent to "NEW(T).init(...)". *)

PROCEDURE Find(v: VBT.T): T; <* LL.sup = VBT.mu *>
(* Return the lowest (possibly improper) ancestor of "v" that is a
   "HighlightVBT.T" or "NIL" if there isn't one. *)

PROCEDURE SetRect(
  v: VBT.T; 
  READONLY rect: Rect.T; 
  inset: CARDINAL := 2);
 <* LL.sup = VBT.mu *>
(* Set the rectangle and inset of "Find(v)" to the given values. *)

(* The inset is given in pixels, not in millimeters. *)

PROCEDURE SetTexture(
  v: VBT.T; 
  txt: Pixmap.T; 
  READONLY delta := Point.Origin;
  op := PaintOp.TransparentSwap);
 <* LL.sup = VBT.mu *>
(* Set the "txt", "delta", and "op" of "Find(v)" to the given 
   values.  *)

PROCEDURE Get(
  v: VBT.T;
  VAR rect: Rect.T;
  VAR inset: CARDINAL;
  VAR txt: Pixmap.T;
  VAR delta: Point.T;
  VAR op: PaintOp.T): BOOLEAN; <* LL.sup = VBT.mu *>
(* Fetch the parameters for the "HighlightVBT" above "v", and return "TRUE".
   If "v" has no such ancestor, return "FALSE". *)

PROCEDURE Invert(v: VBT.T; 
  READONLY r: Rect.T; 
  inset: CARDINAL); <* LL.sup = VBT.mu *>
(*  Highlight the outline inset into the rectangle "r" with width
    "inset", using a solid texture. *)
    
(* "Invert" operates on "Find(v)".  It is equivalent to:

  | SetTexture(v, Pixmap.Solid);
  | SetRect(v, r, inset)
*)

(* "SetRect", "SetTexture", and "Invert" are no-ops if "Find(v)" is "NIL". *)

END HighlightVBT.
