(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jun 14 21:47:38 PDT 1993 by meehan *)
(*      modified on Tue Feb  2 00:14:43 PST 1993 by mhb    *)
(*      modified on Tue Jun 16 13:08:17 PDT 1992 by muller *)
<* PRAGMA LL *>

(* A "ShadowedBarVBT.T" is a leaf-VBT that displays a horizontal or
   vertical 3-D line.

   The following chart summarizes the visual effects:

             \begin{center}
             \begin{tabular}{l|l|l}

                               & {\em top (vertical)}    & {\em bottom (vertical)}\\
                               & {\em left (horizontal)} & {\em right (horizontal)} \\
             {\em Style}       &                         & \\
             \hline
                 Flat      &   Background  &  Background\\
                 Raised    &   Light       &  Dark\\
                 Lowered   &   Dark        &  Light\\
                 Ridged    &   Light       &  Dark\\
                 Chiseled  &   Dark        &  Light

             \end{tabular}
             \end{center}
*)

INTERFACE ShadowedBarVBT;

IMPORT Axis, Shadow, VBT;

TYPE
  T <: Public;
  Public = VBT.Leaf OBJECT
           METHODS
             <* LL.sup <= VBT.mu *>
             init (axis  : Axis.T;
                   shadow: Shadow.T := NIL;
                   style            := Shadow.Style.Flat): T;
           END;

(* The call "v.init(...)" initializes "v" as a "ShadowedBarVBT" with
   the "axis" orientation and with the given "shadow" and "style". The
   default "shadow" is "Shadow.None".  If the "shadow.size" along the
   "axis" dimension results in an odd number of pixels, the extra
   pixel goes to the top half.

   When "Shadow.Supported(shadow, v)" is "TRUE", the shape of "v"
   is "ABS(shadow.size)" in the primary axis, and unconstrained in the
   other dimension. Otherwise, the shape of "v" in the primary axis
   is "ABS(shadow.size/2)". *)

PROCEDURE Set (v: T; shadow: Shadow.T);
<* LL.sup = VBT.mu *>
(* Change the size and colors of "v"'s shadow and mark "v" for redisplay. *)

PROCEDURE SetStyle (v: T; style: Shadow.Style);
<* LL.sup = VBT.mu *>
(* Change the style of "v"'s shadow and mark "v" for redisplay. *)

END ShadowedBarVBT.

