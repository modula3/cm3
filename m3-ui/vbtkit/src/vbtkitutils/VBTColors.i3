(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Mon Jan  4 12:22:59 PST 1993 by mhb *)
(* modified on Tue Aug 4 11:40:50 PDT 1992 by meehan *)
(* modified on Tue Jun 16 12:57:47 PDT 1992 by muller *)
<* PRAGMA LL *>

(* The "VBTColors" interface provides a way to associate a
   "VBT"'s background and foreground colors with the "VBT".  This
   information can be retrieved by some other "VBT" to compute a
   related color. *)

INTERFACE VBTColors;

IMPORT PaintOp, VBT;

PROCEDURE Put (v: VBT.T; colors: PaintOp.ColorScheme);
<* LL.sup < v *>
(* Store "colors" with "v". *)

PROCEDURE Get (v: VBT.T): PaintOp.ColorScheme;
<* LL.sup < v *>
(* Return the colors stored by the most recent call to "Put".  If
   "Put" has never been called on "v", return "PaintOp.bgFg". *)

END VBTColors.

