(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* File: TextVBT.i3, coded by cgn Sun Jun 28 16:13:38 1987 *)
(* Last modified on Mon Feb 24 13:54:50 PST 1992 by muller  *)
(*      modified on Wed Dec 11 18:35:49 PST 1991 by gnelson *)
(*      modified on Fri Mar  3 20:59:04 PST 1989 by msm *)
<*PRAGMA LL*>

(* A "TextVBT.T" is a "VBT" that displays a text string.

   The minimum size of a "TextVBT" is just large enough to display its
   text (surrounded by any margins that were supplied when the "TextVBT"
   was created), except that if its text is empty its minimum size is
   just large enough to display the text ``"X"''.  Its preferred size
   is the same as its minimum size, and its maximum size is very large.
   *)

INTERFACE TextVBT;

IMPORT VBT, Font, PaintOp, Rect;

TYPE
  T <: Public;
  Public = VBT.Leaf OBJECT METHODS
    <* LL.sup <= VBT.mu *>
    init(txt: TEXT;
      halign, valign: REAL := 0.5;
      hmargin: REAL := 0.5;
      vmargin: REAL := 0.0;
      fnt: Font.T := Font.BuiltIn;
      bgFg: PaintOp.ColorQuad := NIL): T
  END;

(* The call "v.init(...)" initializes "v" as a "TextVBT" that displays the 
   text "txt" in the font "fnt", and returns "v".

   The text will be painted with "bgFg"'s foreground; the background
   will be painted with "bgFg"'s background.  If "bgFg" is "NIL" these
   default to "PaintOp.Fg" and "PaintOp.Bg".  The text should not
   contain any newline characters: it will be treated as a single line.
   If "halign = 0.0", the west boundary of the text will be indented
   by the given "hmargin" (in millimeters) from the west boundary of
   the "VBT"; if "halign = 1.0", the east boundary of the text will
   be inside the east boundary of the "VBT" by the given "hmargin";
   for other values of "halign", the horizontal position of the text
   is computed by linear interpolation.  In particular, "halign = 0.5"
   centers the text horizontally.  The vertical position is determined
   by "vmargin" and "valign" in a similar way.  
   
   Control-left-click in the text sets the source selection to be a
   readonly version of the text.  Thus you can copy the text out of
   any "TextVBT".  *)

PROCEDURE New(
    txt: TEXT;
    halign, valign: REAL := 0.5;
    hmargin: REAL := 0.5;
    vmargin: REAL := 0.0;
    fnt: Font.T := Font.BuiltIn;
    bgFg: PaintOp.ColorQuad := NIL) : T;
    <* LL.sup <= VBT.mu *>
(* "New(...)" is equivalent to "NEW(T).init(...)". *)

PROCEDURE Put(v: T; txt: TEXT); <* LL.sup < v *>
(* Change the text displayed by "v" to be "txt" and mark "v" for redisplay. *)

PROCEDURE Get(v: T): TEXT; <* LL.sup < v *>
(* Return the text displayed by v. *)

PROCEDURE SetFont(
  v: T; 
  fnt: Font.T; 
  bgFg : PaintOp.ColorQuad := NIL);
<* LL.sup = VBT.mu *>
(* Set "v"'s "font" and "bgFg" to the given values and mark "v" 
   for redisplay. If "bgFg" is defaulted, "PaintOp.bgFg" is used. *)

PROCEDURE GetFont(v: T): Font.T; <* LL.sup = VBT.mu *>
(* Return "v"'s font. *)

PROCEDURE GetQuad(v: T): PaintOp.ColorQuad; 
<* LL.sup = VBT.mu *>
(* Return "v"'s color quad. *)

PROCEDURE GetTextRect(v: T): Rect.T;
<* LL.sup = VBT.mu *>
(* Return the current bounding rectangle of "v"'s text. *)

END TextVBT.
