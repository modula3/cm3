(* Copyright 1989 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Fri Jul 24 17:44:26 PDT 1992 by mjordan *)
(*      modified on Sun Jul 19 20:13:06 1992 by mhb *)

<* PRAGMA LL *>

(* A RectsVBT continuously displays a collection of rectangles.
   Each rectangle is defined by a location in world coordinates,
   a color, and flag indicating whether it exists.  Only those
   rectangles that exist are displayed.

   The client specifies N, how many rectangle there will be, and
   then accesses the items using a number [0..N].  The 0th item
   is typically used for temporary drawing, or for highlighting.
   When the RectsVBT is refreshed, the items are painted from 1
   to N, and then 0.  Obviously, only items for which information
   has been defined are ever painted.

   The rectangles are always displayed with the client's world
   coordinate system mapped to the entire VBT domain, optionally
   with a margin of some number of points.

   A "RectsVBT" in internally syncrhonized; it can safely be called 
   from mulitple threads. Each procedure has "LL.sup < rects".
   In addition, "VBT.mu < rects" for any "rects: RectsVBT.T". *)

INTERFACE RectsVBT;

IMPORT PaintOp, Point, Rect, VBT;

TYPE
  T <: Public;
  Public = VBT.Leaf OBJECT METHODS init (): T; END;


(*
**
** Global Parameters
**
*)

PROCEDURE SetBg (v: T; op: PaintOp.T);
(* Use "op" for the background, and mark "v" for redisplay.  The
   default value is PaintOp.Bg. *)

PROCEDURE SetMargin (v: T; west, south, east, north: REAL);
(* Rather than using all of "v"'s domain to map world
   coordinates, inset the domain by this many points on each
   side.  The default margin is 2.0 on all sides.  Mark "v" for
   redisplay. *)

PROCEDURE SetWC (v: T; west, south, east, north: REAL);
(* Set the window in world coordinates for drawing and mark "v"
   for redisplay.  The default is (0,0,1,1). *)

PROCEDURE SetMins (v: T; wd, ht: REAL);
(* Set the smallest size, in points, that each visible rectangle
   is allowed to be, and mark "v" for redisplay.  The default
   value is 4.0 in both dimensions. *)


(*
**
** Painting
**
*)

PROCEDURE Draw (v: T; i: CARDINAL);
(* Draw the ith rectangle. *)

PROCEDURE Erase (v: T; i: CARDINAL);
(* Erase the ith rectangle by redrawing the rectangle using the
   background color.  There is no concept of refreshing what was
   underneath the rectangle.  It's the responsibility of the
   caller to redraw whatever is necessary. *)


(*
**
** Creating and deleting
**
*)

PROCEDURE SetN (v: T; N: CARDINAL; redisplayFg: BOOLEAN := FALSE);
(* Create a new set of [0..N] items, after deleting the current
   set.  If "redisplayFg" is TRUE, then erase each rectangle
   before deleting it. *)

PROCEDURE Exists (v: T; i: CARDINAL): BOOLEAN;
(* Does the ith rectangle exits?  A rectangle is considered to
   ``existing'' as soon as one of its attributes (i.e., color or
   position) is defined; it no longer exists when Delete *)

PROCEDURE Delete (v          : T;
                  i          : CARDINAL;
                  redisplayFg: BOOLEAN    := FALSE);
(* Record the ith rectangle as no longer existing.  If
   redisplayFg is TRUE, then erase the item before deleting
   it. *)


(*
**
** Attributes
**
*)

PROCEDURE Position (v                       : T;
                    i                       : CARDINAL;
                    west, south, east, north: REAL;
                    redisplayFg             : BOOLEAN    := FALSE);
(* Set the position in world coordinates of the ith rectangle.
   If redisplayFg is TRUE, then erase the item (at its old
   position) and draw it in its new location. *)

PROCEDURE Color (v          : T;
                 i          : CARDINAL;
                 op         : PaintOp.T;
                 redisplayFg: BOOLEAN     := FALSE);
(* Set the color of the "i"th rectangle.  If redisplayFg is TRUE,
   then draw the rectangle in its new color. *)

PROCEDURE GetColor (v          : T;
                    i          : CARDINAL): PaintOp.T;
(* Get the color of the "i"th rectangle. A checked run-time
   error if the "i"th rectangle hasn't been defined yet. *)

PROCEDURE Locate (v: T; i: CARDINAL): Rect.T;
(* Return the screen coordinates where the "i"th rectangle is
   being displayed.  Return Rect.Empty if the "i"th rectangle
   hasn't been defined yet. *)


(*
**
** Coordinate transformations
**
*)

TYPE
  RealPoint = RECORD h, v: REAL END;
  RealRect = RECORD north, south, west, east: REAL END;

PROCEDURE VBT2WC (v: T; pt: Point.T): RealPoint;
(* Translate "pt" from screen coordinates to world coordinate. *)

PROCEDURE WC2VBT (v: T; pt: RealPoint): Point.T;
(* Translate "pt" from world coordinates to screen coordinate. *)


END RectsVBT.


