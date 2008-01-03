(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Sep 28 20:34:12 PDT 1995 by mhb        *)
(*      modified on Fri Jun 11 16:07:37 PDT 1993 by meehan     *)
(*      modified on Tue Jun 16 13:08:31 PDT 1992 by muller     *)
<*PRAGMA LL*>

(* A "PixmapVBT.T" is a VBT that displays a pixmap. 

   The minimum size of a "PixmapVBT" is just large enough to display
   its pixmap (surrounded by any margins that were supplied when the
   "PixmapVBT" was created). Its preferred size is the same as its
   minimum size, and its maximum size is very large. *)

INTERFACE PixmapVBT;

IMPORT VBT, PaintOp, Pixmap;

TYPE
  T <: Public;
  Public =
    VBT.Leaf OBJECT
    METHODS
      <* LL.sup <= VBT.mu *>
      init (pm: Pixmap.T;
            halign, valign: REAL := 0.5;
            hmargin, vmargin: REAL := 0.0;
            op: PaintOp.T := PaintOp.BgFg;
            bg: PaintOp.T := PaintOp.Bg): T
    END;

(* The call "v.init(...)" initializes "v" as a "PixmapVBT" displaying
   pixmap "pm" using the paint op "op", and returns "v".  

   If "halign = 0.0", the west boundary of the pixmap will be indented
   by the given "hmargin" (in millimeters) from the west boundary of
   the "VBT"; if "halign = 1.0", the east boundary of the pixmap will
   be inside the east boundary of the "VBT" by the given "hmargin";
   for other values of "halign", the horizontal position of the text
   is computed by linear interpolation.  In particular, "halign = 0.5"
   centers the pixmap horizontally.  The vertical position is determined
   by "vmargin" and "valign" in a similar way.  

   If the domain of "v" is larger than the pixmap, the background is
   painted using the tint "bg".

   When the pixmap has depth 1, "op" should be a pair of tints.
   Otherwise, a good choice for "op" is "PaintOp.Copy". *)

PROCEDURE Put (v: T; pm: Pixmap.T);
<* LL.sup = VBT.mu *>
(* Change the pixmap displayed by "v" to "pm", and mark "v" for redisplay. *)

PROCEDURE SetColors (v : T;
                     op: PaintOp.T;
                     bg: PaintOp.T   := PaintOp.Bg);
<* LL.sup = VBT.mu *>
(* Change the "op" and "bg" of "v", and mark "v" for redisplay. *)

END PixmapVBT.




