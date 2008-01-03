(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman and Stephen Harrison                                    *)
(* Last modified on Fri Aug 19 16:15:39 PDT 1994 by steveg   *)
(*      modified on Tue Jul 21 17:17:36 PDT 1992 by harrison *)
(*      modified on Fri Jul 10 18:55:54 1992 by mhb      *)

INTERFACE MG;

<* PRAGMA LL *>

(* All R2.T coordinates are in Cartesian (XY) space with x increasing
   to the right and y increasing upward *)

IMPORT Font, Matrix2D, MGV, PaintOp, Point, R2, Rect, Region, VBT, R2Path,
R2Box;

TYPE
  V = MGV.V;

TYPE
  T = MGV.MGT;

REVEAL
  T <: TPublic;

TYPE
  TPublic =
    OBJECT
      (* all fields READONLY except at initialization or through
         methods *)
      id := NoID;
      (* The ID provided by the algorithm that is associated with
         this element.  Output events from the algorithm are in
         terms of an id which is mapped by the v.lookup method to
         the element.  Input events to the algorithm are in terms
         of the id associated with the element *)

      m := Matrix2D.Identity;
      (* The position of self is m * R2.Origin which is the tx ty
         of a matrix translation. *)

      highlight := 0.0;         (* 0.0 -> normal, 1.0 -> highlit *)
      visible   := 1.0;         (* 0.0 -> invisible, 1.0 -> visible *)
      color     : PaintOp.ColorScheme;  (* !!!  := PaintOp.bgFg*)
      weight                            := 1.0;
      label                             := "";
      font                              := DefaultFont;
      alignment                         := Alignment.Center;
      appearance: Appearance;

      dirty := FALSE;           (* internal bookkeeping field *)
    METHODS
      <* LL < v.mu *>
      init (v: V := NIL; parent: Group := NIL): T;
      (* If self.appearance is NIL then appearance gets
         AppearanceDefault (paints the object "as is")

         init also adds self to the group "parent" or, if
         "parent" is NIL and "v" is not NIL, to "v"'s display
         list.

         init also registers self with the lookup table for "v"
         if v is not NIL and self.id is not NoID. *)

      <* LL = v.mu *>
      setColor (v: V; color: PaintOp.ColorScheme (* := PaintOp.bgFg *));
      setFont  (v: V; font := DefaultFont);
      setHighlight  (v: V; highlight := 0.0);
      setVisible    (v: V; visible := 1.0);
      setWeight     (v: V; weight := 1.0);
      setAlignment  (v: V; alignment := Alignment.Center);
      setLabel      (v: V; label := "");
      setAppearance (v: V; appearance := NIL);
      (* appearance = NIL => AppearanceDefault;
         appearance.prevAppearance gets set unless appearance =
         NIL *)

      transform (v: V; READONLY m: Matrix2D.T);
      (* apply the 2D transformation to self.  The default
         transform method transforms self.m, subtypes with other
         values must transform the other values themselves or
         either when the transform method is called or when the
         values are read. *)

      render (v: V; VAR (* IN/OUT *) clip: Rect.T);
      (* renders self to the screen.

         repaintRect should be updated to include any dirty
         elements painted (normally only changed when painting a
         group or an appearance.

         Do not call directly, call self.appearance.paint (which
         uses render) *)

      bounds (v: V): R2Box.T;
      (* returns the bounding rectangle of the element (possibly ignoring
         the label).  Do not call directly, call
         self.appearance.boundingBox (which uses boundingBox) *)

      rgn (v: V): Region.T;
      (* returns the (conservative) region the element occupies
         in v in screen pixels (mostly for use by appearance).
         Do not call directly, call self.appearance.region (which
         calls region) *)
    END;

TYPE
  ID = INTEGER;

CONST
  NoID: INTEGER = -1;
  DefaultFont = Font.T{fnt := 923199216};

TYPE
  Alignment = {N, NW, W, SW, S, SE, E, NE, Center};
  (* Label is located outside the bounding box in the direction
     specified by the alignment *)

TYPE
  AppearanceDefault <: Appearance;

VAR
  appearanceDefault: AppearanceDefault;
  (* paints the object "as is" (i.e. no effects) *)

TYPE
  (* An appearance handles highlight effects (drop shadows, underlays,
     marquees, blinking, partial color changes) and other special effects.

     The intention is for an appearance object to be independent of the
     object it affects, so that an apperance can be applied to a group and
     will affect all of the elements in the group.
  *)
  Appearance <: AppearancePublic;
  AppearancePublic =
    OBJECT
      <* LL = v.mu *>
      prevAppearance: Appearance := NIL;
      (* the previous appearance of the object for composing appearances *)
    METHODS
      <* LL = v.mu *>
      paint (t: T; v: V; VAR clip: Rect.T);

      boundingBox (t: T; v: V): R2Box.T;
      (* returns the bounding rectagle of the element (possibly ignoring
         the label) *)

      region (t: T; v: V): Region.T;
      (* returns the (conservative) region the element occupies in v in
         screen pixels (mostly for internal use) *)
    END;

(* ------------------------------------------------------------------
    See the Appearances interface for some predefined appearances 
------------------------------------------------------------------ *)

TYPE
  GroupIterator = OBJECT        <* LL = v.mu *>
                    v          : V;
                    recursively: BOOLEAN;
                    fromBack   : BOOLEAN;
                  METHODS       <* LL = v.mu *>
                    proc (t: T): (* more *) BOOLEAN
                  END;

TYPE
  Group = MGV.MGGroup;

REVEAL
  Group <: GroupPublic;

TYPE
  GroupPublic =
    T OBJECT
      <* LL = v.mu *>
      elems: REF ARRAY OF T;
      (* elements are stored in visual back to front order.  elems # NIL *)
      cntElems: INTEGER := 0;   (* cnt of non-nil elements *)
    METHODS
      init (elemsSize               := 4;
            v          : V          := NIL;
            parentGroup: Group      := NIL   ): Group;
      (* initializes elems to an array of "elemsSize" elements (all
         NIL), *)

      <* LL = v.mu *>
      iterate (gi: GroupIterator; recursively := FALSE; fromBack := TRUE):
               BOOLEAN;
      (* iterate over all the elements of the group calling gi.proc until
         it returns FALSE.  Result is false if any proc returned FALSE else
         TRUE *)

      addAfter (v: V; t: T; prev: T := NIL);
      (* prev = NIL -> add as first element
         "t" gets painted on top of (after) "prev"
         first element in the group is on the bottom
       *)
      addBefore (v: V; t: T; next: T := NIL);
      (* next = NIL -> add as last element
         "t" gets painted underneath (before) "next"
         last element of the group is on top
      *)

      top    (v: V; t: T);
      (* make "t" the last element in the group *)
      bottom (v: V; t: T);
      (* make "t" the first element in the group *)

      remove (v: V; t: T);
      (* remove "t" from the group *)
    END;

TYPE
  Label <: LabelPublic;
  LabelPublic = T OBJECT (* must call init method *) END;

TYPE
  Line <: LinePublic;
  LinePublic =
    T OBJECT
      style         := VBT.EndStyle.Round;
      to   : R2.T;
      (* Pos(self) is "from" point of the line self.m * self.to is "to"
         point.  Call "MGPublic.LineTo" or "MG.LineToLocked" to get the
         "to" position of the line. "weight" is interpreted as the
         thickness of the line.  If "weight" <= 0 the line is invisible. *)
    METHODS
      init (READONLY from, to: R2.T; v: V := NIL; group: Group := NIL):
            Line;

      (* LL = v.mu *)
      setStyle (v: V; style := VBT.EndStyle.Round);
      reshape  (v: V; READONLY from, to: R2.T);
    END;

TYPE
  LineEnd <: LineEndPublic;
  LineEndPublic = T OBJECT
    line: Line := NIL;
    controlsFrom := TRUE;
  METHODS
    (* must call init method *)
  END;
  (* Identical behaviour to line except that the transform method
     only affects the from or to endpoint (depending on controlsFrom).
     Only the "from" end of a line repaints it.

     This is useful for creating a group affecting only one endpoint of
     the line. 

     (Uses standard init method)
   *)

TYPE
  Rectangle <: RectanglePublic;
  RectanglePublic =
    T OBJECT
      ne: R2.T;                 (* Pos(self) is southwest corner of the
                                   rectangle.  self.m * self.ne is
                                   northeast corner. *)
    METHODS
      init (READONLY corner1, corner2: R2.T;
                     v               : V          := NIL;
                     group           : Group      := NIL   ): Rectangle;

      (* LL = v.mu *)
      reshape (v: V; READONLY corner1, corner2: R2.T);
    END;
    (* The rectangle is painted in the background color of the paint scheme.
       The weight is interpreted as the thickness of an inset border
       around the rectangle.  The color of the border is the foreground
       color of the color scheme *)

TYPE
  Ellipse <: EllipsePublic;
  EllipsePublic =
    T OBJECT
      ne: R2.T;                 (* Pos(self) is southwest corner of the
                                   ellipse.  self.m * self.ne is
                                   northeast corner. *)
    METHODS
      init (READONLY corner1, corner2: R2.T;
                     v          : V          := NIL;
                     group      : Group      := NIL   ): Ellipse;

      (* LL = v.mu *)
      reshape (v: V; READONLY corner1, corner2: R2.T);
    END;
    (* The ellipse is painted in the background color of the color scheme.
       The weight is interpreted as the thickness of an 
       inset border around the ellipse.  The color of the border is the 
       foreground color of the color scheme. *)


TYPE
  Shape <: ShapePublic;
  ShapePublic =
    T OBJECT
    METHODS
      init (READONLY origin: R2.T;
            READONLY path: R2Path.T;
            READONLY fill := TRUE;
                     v          : V          := NIL;
                     group      : Group      := NIL   ): Shape;

      (* LL = v.mu *)
      reshape (v: V; READONLY origin: R2.T; READONLY path: R2Path.T; READONLY fill := TRUE);
    END;
  (* A filled shape uses odd winding to determine the inside/outside of the shape.
     "weight" is interpreted as the thickness of the stroke following the path.
     If "weight" is <= 0, then the stroke is invisible.  *)

<* LL = v.mu for following procedures *>
<* INLINE *> PROCEDURE PosLocked (t: T; v: V): R2.T;
<* INLINE *> PROCEDURE SetPosLocked (t: T; READONLY pos: R2.T; v: V);

<* INLINE *> PROCEDURE LineFromLocked (line: Line; v: V): R2.T;
<* INLINE *> PROCEDURE LineToLocked (line: Line; v: V): R2.T;

<* INLINE *> PROCEDURE RectangleSWLocked (rectangle: Rectangle; v: V): R2.T;
<* INLINE *> PROCEDURE RectangleNELocked (rectangle: Rectangle; v: V): R2.T;

<* INLINE *> PROCEDURE EllipseSWLocked (ellipse: Ellipse; v: V): R2.T;
<* INLINE *> PROCEDURE EllipseNELocked (ellipse: Ellipse; v: V): R2.T;

<* INLINE *> PROCEDURE ShapeOriginLocked (shape: Shape; v: V): R2.T;
<* INLINE *> PROCEDURE ShapePathLocked (shape: Shape; v: V): R2Path.T;

<* INLINE *>
  PROCEDURE BoundingBoxLocked (t: T; v: V): R2Box.T;

PROCEDURE RTranslateLocked (t: T; v: V; READONLY by: R2.T);
PROCEDURE TranslateToLocked (         t     : T;
                                      v     : V;
                             READONLY dest  : R2.T;
                                      center         := FALSE);
PROCEDURE ScaleLocked (         t     : T;
                                v     : V;
                       READONLY factor      := R2.Ones;
                       READONLY wrt         := R2.Origin);
PROCEDURE RotateLocked (         t     : T;
                                 v     : V;
                                 angle : REAL;
                        READONLY origin         := R2.Origin);
(* Rotate by "angle" degrees counter clockwise around "origin" *)

PROCEDURE TransformLocked (t: T; v: V; READONLY m: Matrix2D.T);

(* Conversion procedures *)
<* INLINE *> PROCEDURE ScreenPointToMGC (v: V; READONLY pt: Point.T): R2.T;
<* INLINE *> PROCEDURE MGCToScreenPoint (v: V; READONLY p: R2.T): Point.T;
<* INLINE *>
PROCEDURE MGCToScreenRect (v: V; READONLY corner1, corner2: R2.T):
  Rect.T;
(* MGCToScreenRect will produce Rect.Empty if the screen points of corner1 and corner2
   coincide.  Use:
     Rect.Join(Rect.FromPoint(MGCToScreenPoint(corner1)), 
               Rect.FromPoint(MGCScreenPoint(corner2)))
   if you need a non-empty rectangle *)

PROCEDURE ResetLookupsLocked(v: V);
(* Clear the lookup table for "v". *)

END MG.
