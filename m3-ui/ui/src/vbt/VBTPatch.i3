(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Feb 24 13:58:56 PST 1992 by muller                   *)

(* \subsection{Texturing irregular patches} *)

TYPE
  DeltaPair = RECORD dl, dr: BITS 8 FOR [-128 .. 127] END;


PROCEDURE PaintPatch(
    v: Leaf;
    READONLY clip: Rect.T;
    hl, hr, vlo, vhi, start: INTEGER;
    READONLY deltaArray: ARRAY OF DeltaPair;
    op: PaintOp.T := PaintOp.BgFg;
    src: Pixmap.T := Pixmap.Solid;
    READONLY delta: Point.T := Point.Origin); <* LL.sup < v *>
(* This is like "PaintTexture", except that the region to be textured is a
   ``patch'' instead of a rectangle. A patch is region that intersects
   each horizontal line in a single interval. "vlo" is the index of the
   first (i.e., northernmost) scan line that intersects the patch, and
   "[hl .. hr)" is the intersection of the patch with this scan line. The
   intersections of the patch with the following scan lines are
   represented in "deltaArray". Since the interval for any scan line is
   likely to be close to the interval for the previous scan line, the
   entries in the array are not the interval endpoints themselves, but
   the deltas from the previous endpoints. The information can begin
   anywhere in "deltaArray"; the parameter "start" is the index of the first
   relevant delta pair. Finally, "vhi" is the index of the first scan line
   that passes below the patch. *)
   
(*  More precisely,

|   `for each pair of points "p", "q" such that`
|       p `is in the patch (see below),` p `is in` clip`, and`
|       p = q + delta,
|   `assign`
|       v[p] := op(v[p], src[Rect.Mod(q, domain(src))]).

   The patch contains "p" if:
   
|       p.v IN [vlo .. vhi) AND p.h IN [hl + DL .. hr + DR)
|   `where`
|       DL = `sum of` deltaArray[start + i - vlo].dl `and`
|       DR = `sum of` deltaArray[start + i - vlo].dr
|   `for` i `in` [vlo .. p.v).

   The value of "start" must be non-negative.

   The patch is clipped vertically to exclude any rows for which "DL" and
   "DR" are undefined due to exhaustion of "deltaArray". *)

