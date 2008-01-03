(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

(* The "Trans" interface provides routines to transform attributes of
   items between the forms in which they are recorded and the forms suitable
   for painting the items on a window.
*)

INTERFACE Trans;

IMPORT Point, Rect, Color, PaintOp,
       PointR, RectR, Focus;

(* The following routines convert geometrics between the coordinate
   systems of the board and the window. The information regarding the
   positioning of the window's focus is supplied.
*)

<*INLINE*> PROCEDURE PointB2W (p: PointR.T; focus: Focus.T): Point.T;

<*INLINE*> PROCEDURE PointW2B (p: Point.T; focus: Focus.T): PointR.T;

<*INLINE*> PROCEDURE RectB2W (r: RectR.T; focus: Focus.T): Rect.T;

<*INLINE*> PROCEDURE RectW2B (r: Rect.T; focus: Focus.T): RectR.T;

(* In the above routines, "focus" provides the offset and scale of the
   window's focus.  
   "PointB2W" transforms a point in the board coordinates to one in
   the window. Other routines can be interpretted similarly.
*)

<*INLINE*> PROCEDURE Color2Op (c: Color.T): PaintOp.T;
(* Converts a "Color.T" into a "PaintOp.T". *)

END Trans.
