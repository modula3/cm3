(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Sep 25 15:37:17 PDT 1992 by msm  *)
<*PRAGMA LL*>

INTERFACE TwoTone;
IMPORT PaintOp, Point, Pixmap, Rect, VBT;

TYPE
  T = RECORD
        op : PaintOp.T;
        txt: Pixmap.T;
      END;

PROCEDURE New (op: PaintOp.T; bwtxt: Pixmap.T): T;
  (* Result:
       ".op" is "PaintOp.BgFg" on a black and white display, "op" otherwise.
       ".txt" is "bwtxt" on a black and white display, "Pixmap.Solid" otherwise
   *)

PROCEDURE Paint (         v    : VBT.Leaf;
                 READONLY clip : Rect.T;
                 READONLY tone : T;
                 READONLY delta             := Point.Origin); <* LL.sup < v *>
(* Paint the rectangle "clip" with the texture "tone.txt+delta" using
   the operation "tone.op". *) 

END TwoTone.
