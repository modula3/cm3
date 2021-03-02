UNSAFE INTERFACE XShape;

(*==============================================================*)
(* The X11 R4 Interface for Modula 3 *)
(* *)
(* contains: /usr/include/X11/extensions/shape.h *)
(*==============================================================*)

FROM Ctypes IMPORT int, int_star, unsigned_int, unsigned_long,
                   unsigned_int_star;

FROM X IMPORT DisplayStar, Pixmap, Region, XRectangleStar, Window, Time,
              BoolStar, Status;


CONST
  SHAPENAME = "SHAPE";

  ShapeSet       = 0;
  ShapeUnion     = 1;
  ShapeIntersect = 2;
  ShapeSubtract  = 3;
  ShapeInvert    = 4;

  ShapeBounding = 0;
  ShapeClip     = 1;
  ShapeInput    = 2;

  ShapeNotifyMask = 1L;
  ShapeNotify     = 0;

  ShapeNumberEvents = ShapeNotify + 1;

TYPE
  ShapeEvent =
    RECORD
      type: int;                 (* of event *)
      serial: unsigned_long;     (* # of last request processed by
                                    server *)
      send_event: BOOLEAN;       (* true if this came frome a SendEvent
                                    request *)
      display: DisplayStar;      (* Display the event was read from *)
      window : Window;           (* window of event *)
      kind   : int;              (* ShapeBounding or ShapeClip *)
      x, y   : int;              (* extents of new region *)
      width, height: unsigned_int;
      time: Time;                (* server timestamp when region changed *)
      shaped: BOOLEAN;           (* true if the region exists *)
    END;

<* EXTERNAL XShapeQueryExtension *>
PROCEDURE QueryExtension
  (display: DisplayStar; eventBase, errorBase: int_star): BOOLEAN;


<* EXTERNAL XShapeQueryVersion *>
PROCEDURE QueryVersion
  (display: DisplayStar; majorVersion, minorVersion: int_star): Status;

<* EXTERNAL XShapeCombineRegion *>
PROCEDURE CombineRegion (display   : DisplayStar;
                         dest      : Window;
                         destKind  : int;
                         xOff, yOff: int;
                         region    : Region;
                         op        : int          );

<* EXTERNAL XShapeCombineRectangles *>
PROCEDURE CombineRectangles (display   : DisplayStar;
                             dest      : Window;
                             destKind  : int;
                             xOff, yOff: int;
                             rectangles: XRectangleStar;
                             nRects    : int;
                             op        : int;
                             ordering  : int             );

<* EXTERNAL XShapeCombineMask *>
PROCEDURE XShapeCombineMask (display   : DisplayStar;
                             dest      : Window;
                             destKind  : int;
                             xOff, yOff: int;
                             src       : Pixmap;
                             op        : int          );

<* EXTERNAL XShapeCombineShape *>
PROCEDURE CombineShape (display   : DisplayStar;
                        dest      : Window;
                        destKind  : int;
                        xOff, yOff: int;
                        src       : Window;
                        srcKind   : int;
                        op        : int          );

<* EXTERNAL XShapeOffsetShape *>
PROCEDURE OffsetShape
  (display: DisplayStar; dest: Window; destKind: int; xOff, yOff: int);

<* EXTERNAL XShapeQueryExtents *>
PROCEDURE XShapeQueryExtents (display             : DisplayStar;
                              window              : Window;
                              boundingShaped      : BoolStar;
                              xBounding, yBounding: int_star;
                              wBounding, hBounding: unsigned_int_star;
                              clipShaped          : BoolStar;
                              xClip, yClip        : int_star;
                              wClip, hClip        : unsigned_int_star  ):
  Status;

<* EXTERNAL XShapeSelectInput *>
PROCEDURE SelectInput
  (display: DisplayStar; window: Window; mask: unsigned_long);

<* EXTERNAL XShapeInputSelected *>
PROCEDURE InputSelected (display: DisplayStar; window: Window; ):
  unsigned_long;

<* EXTERNAL XShapeGetRectangles *>
PROCEDURE GetRectangles (display        : DisplayStar;
                         window         : Window;
                         kind           : int;
                         count, ordering: int_star     ): XRectangleStar;

END XShape.
