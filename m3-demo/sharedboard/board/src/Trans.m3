(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

MODULE Trans;

IMPORT Point, Rect, Color, PaintOp, 
       PointR, RectR, Focus;


<*INLINE*> PROCEDURE PointB2W (p: PointR.T; focus: Focus.T): Point.T =
  BEGIN 
    RETURN Point.T {h := ROUND ((p.h - focus.offset.h) * focus.scale),
                    v := ROUND ((p.v - focus.offset.v) * focus.scale)};
  END PointB2W;

<*INLINE*> PROCEDURE PointW2B (p: Point.T; focus: Focus.T): PointR.T =
  BEGIN 
    RETURN PointR.T {h := focus.offset.h + FLOAT(p.h)/focus.scale,
                     v := focus.offset.v + FLOAT(p.v)/focus.scale};
  END PointW2B; 

<*INLINE*> PROCEDURE RectB2W (r: RectR.T; focus: Focus.T): Rect.T =
  VAR width := ROUND ((r.east-r.west)*focus.scale);
      height := ROUND ((r.south-r.north)*focus.scale);
      west := ROUND ((r.west - focus.offset.h) * focus.scale);
      north := ROUND ((r.north - focus.offset.v) * focus.scale);
  BEGIN 
    RETURN Rect.T {west := west,
                   east := west+width,
                   north := north,
                   south := north+height}
  END RectB2W;

(* old 
<*INLINE*> PROCEDURE RectB2W (r: RectR.T; focus: Focus.T): Rect.T =
  BEGIN 
    RETURN Rect.T {west := ROUND ((r.west - focus.offset.h) * focus.scale),
                   east := ROUND ((r.east - focus.offset.h) * focus.scale),
                   north := ROUND ((r.north - focus.offset.v) * focus.scale),
                   south := ROUND ((r.south - focus.offset.v) * focus.scale)}
  END RectB2W;
*)

<*INLINE*> PROCEDURE RectW2B (r: Rect.T; focus: Focus.T): RectR.T =
  BEGIN 
    RETURN RectR.T {west := focus.offset.h + FLOAT(r.west)/focus.scale,
                    east := focus.offset.h + FLOAT(r.east)/focus.scale,
                    north := focus.offset.v + FLOAT(r.north)/focus.scale,
                    south := focus.offset.v + FLOAT(r.south)/focus.scale};
  END RectW2B; 

<*INLINE*> PROCEDURE Color2Op (c: Color.T): PaintOp.T =
  BEGIN
    RETURN PaintOp.FromRGB (c.r, c.g, c.b);
  END Color2Op;


BEGIN
END Trans.
