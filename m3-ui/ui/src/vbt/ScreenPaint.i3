(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Feb 24 13:57:53 PST 1992 by muller   *)
(*      modified on Wed Sep 11 15:20:21 PDT 1991 by msm      *)
<*PRAGMA LL*>

UNSAFE INTERFACE ScreenPaint;

(* The following painting procedures are like the corresponding 
   procedures in VBT, except that (1) they use screen-dependent 
   resources rather than screen-independent resources, and (2) it is 
   an unchecked runtime error to call them without locking the VBT. 

   If the pixmap or paintop supplied for an operation is not suitable 
   for the VBT's screentype, the effect is undefined, but limited to 
   the clipping region.  The NIL source represents a bitmap of solid 
   1's. 
   
   In all cases, LL = v. *)

IMPORT Rect, ScrnPaintOp, ScrnPixmap, Point, Region, ScrnFont, 
VBT, Path, Trapezoid;

PROCEDURE PaintTexture(
    v: VBT.T;
    READONLY clip: Rect.T;
    op: ScrnPaintOp.T;
    src: ScrnPixmap.T := NIL;
    READONLY delta: Point.T := Point.Origin);

PROCEDURE PaintTint(
    v: VBT.T;
    READONLY clip: Rect.T;
    op: ScrnPaintOp.T);
(* Like PaintTexture with a source of depth 1 that is all 1's. *)

PROCEDURE PolyTint(
    v: VBT.T;
    READONLY clp: ARRAY OF Rect.T;
    op: ScrnPaintOp.T);

PROCEDURE PolyTexture(
    v: VBT.T;
    READONLY clp: ARRAY OF Rect.T;
    op: ScrnPaintOp.T;
    src: ScrnPixmap.T := NIL;
    READONLY delta: Point.T := Point.Origin);

PROCEDURE PaintRegion(
    v: VBT.T;
    READONLY rgn: Region.T;
    op: ScrnPaintOp.T;
    src: ScrnPixmap.T := NIL;
    READONLY delta: Point.T := Point.Origin);

PROCEDURE PaintPixmap(
    v: VBT.T;
    READONLY clip: Rect.T;
    op: ScrnPaintOp.T;
    src: ScrnPixmap.T;
    READONLY delta: Point.T);

PROCEDURE PaintText(
    v: VBT.T;
    READONLY clip: Rect.T;
    READONLY pt: Point.T;
    fnt: ScrnFont.T;
    t: TEXT;
    op: ScrnPaintOp.T;
    dl: VBT.DisplacementList := NIL);

PROCEDURE PaintSub(
    v: VBT.T;
    READONLY clip: Rect.T;
    READONLY pt: Point.T;
    fnt: ScrnFont.T;
    READONLY chars: ARRAY OF CHAR;
    op: ScrnPaintOp.T;
    dl: VBT.DisplacementList := NIL);

PROCEDURE PaintPatch(
    v: VBT.T;
    READONLY clip: Rect.T;
    hl, hr, vlo, vhi, start: INTEGER;
    READONLY deltaArray: ARRAY OF VBT.DeltaPair;
    op: ScrnPaintOp.T;
    src: ScrnPixmap.T := NIL;
    READONLY delta: Point.T := Point.Origin);

PROCEDURE Scroll(
    v: VBT.T;
    READONLY clip: Rect.T;
    READONLY delta: Point.T;
    op: ScrnPaintOp.T);

PROCEDURE Fill(
    v: VBT.T;
    READONLY clip: Rect.T;
    path: Path.T;
    wind: VBT.WindingCondition;
    op: ScrnPaintOp.T;
    src: ScrnPixmap.T := NIL;
    READONLY delta: Point.T := Point.Origin);

PROCEDURE Stroke(
    v: VBT.T;
    READONLY clip: Rect.T;
    path: Path.T;
    width: INTEGER := 1;
    end := VBT.EndStyle.RoundEnd;
    join := VBT.JoinStyle.RoundJoin;
    op: ScrnPaintOp.T;
    src: ScrnPixmap.T := NIL;
    READONLY delta: Point.T := Point.Origin);

PROCEDURE PaintTrapezoid(
    v: VBT.T;
    READONLY clip: Rect.T;
    READONLY trap: Trapezoid.T;
    op: ScrnPaintOp.T;
    src: ScrnPixmap.T := NIL;
    READONLY delta: Point.T := Point.Origin);

END ScreenPaint.
