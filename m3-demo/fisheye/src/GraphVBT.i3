(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Oct 29 10:04:15 PST 1992 by mhb                      *)
(*      modified on Tue Jun 16 16:46:30 PDT 1992 by muller                   *)

INTERFACE GraphVBT;

(* This is a vbt class for displaying graphs (in the graph theory sense,
   not the Cartesian sense).  A graph is specified as a collection of
   nodes, connected by a collection of links.  Geometry of the display is
   entirely client-specified, in terms of real-number world coordinates.
   The client establishes a "relevant rectangle" of world coordinate space;
   the VBT automatically maintains the transformation so that this
   rectangle is always shown in the VBT, regardless of its size and shape.

   These graphs allow more than one distinct link between the same pair of
   nodes, so links are always specified in terms of (node number, link
   number) rather than node pair.  Thus there are two names for every link,
   depending on which end it is viewed from. When creating links it is
   allowable but not necessary to create a link from both ends.
*)

IMPORT GraphData, VBT;
IMPORT Point, RealPoint, Rect, Text;
IMPORT PaintOp, Pixmap, Font;

CONST
  MaxLink = GraphData.MaxEdge;

TYPE
  T <: VBT.Leaf;
  
  NodeIndex = CARDINAL;

  LinkIndex = [0..MaxLink];

  PointArray = GraphData.PointArray;

  Coord = REAL;

  ErrorKind = {NoSuchNode, NoSuchLink, NoSuchPoint, InvalidColor,
               TooManySizes, ShapeNotProvided};

  LinkHalf = {Top, Bottom};

  FocusDevice = {Mouse, Keyboard};

EXCEPTION
  Error(ErrorKind);


(**********************************************************************)
PROCEDURE New(): T;

(****************************** Corodinate system ********************
     Client coordinates are given as real numbers.  The rectangle
     specified in SetWC is the active rectangle; as the window is
     resized and reshaped, world coordinates will be scaled to screen
     coordinates in such a way that the active rectangle is always
     visible.  Default active rectangle is [-100..100] in both
     dimensions.  Aspect ratio of world coordinates depends on the
     setting of the "square" parameter.  If it is true, the aspect
     ratio is unchangeably set to 1:1 - a square as specified in world
     coordinates will always appear as a square on the screen.  If
     the aspect ratio of the window does not match that of the active
     rectangle, there will be empty white space in the dimension in
     which the window is excessive.  If "square" is false, the aspect
     ratio will vary with the shape of the window, and world coordinate
     space will fill the window in both dimensions.

     It is ok to make n > s or s > n, to get a right-handed or
     left-handed coordinate system as desired.
*)

TYPE
  FisheyeMapType = {Cartesian, Polar};
  FisheyeType = {Graphical, Semantic};

PROCEDURE SetFisheyeFocus(v: T; focus: RealPoint.T);

PROCEDURE GetFisheyeFocus(v: T): RealPoint.T;

PROCEDURE SetFisheyeDistortion(v: T; distortion: REAL);
PROCEDURE GetFisheyeDistortion(v: T): REAL;

PROCEDURE SetFisheyeFocusDevice(v: T; device: FocusDevice);
PROCEDURE GetFisheyeFocusDevice(v: T): FocusDevice;

PROCEDURE GetFisheyeFocusSize(v: T): INTEGER;

PROCEDURE SetFisheyeSizeFactor(v: T; sizeFactor: REAL);
PROCEDURE GetFisheyeSizeFactor(v: T): REAL;

PROCEDURE SetFisheye(v: T; READONLY val: BOOLEAN);
PROCEDURE GetFisheye(v: T): BOOLEAN;

PROCEDURE SetCageSize(v: T; READONLY val: INTEGER);
PROCEDURE GetCageSize(v: T): INTEGER;

PROCEDURE SetFisheyeText(v:T; READONLY val: BOOLEAN);
PROCEDURE GetFisheyeText(v: T): BOOLEAN;

PROCEDURE SetFisheyeVWThreshold(v: T; READONLY val: REAL);
PROCEDURE GetFisheyeVWThreshold(v: T): REAL;

PROCEDURE SetFisheyeSizeAPICoeff(v: T; READONLY val: REAL);
PROCEDURE GetFisheyeSizeAPICoeff(v: T): REAL;

PROCEDURE SetFisheyeSizeAPIPower(v:T; READONLY val: REAL);
PROCEDURE GetFisheyeSizeAPIPower(v: T): REAL;

PROCEDURE SetShape (v: T; shape: Shape);
PROCEDURE SetStyle (v: T; style: Style);

PROCEDURE SetFisheyeType(v: T; fisheye_type: FisheyeType);

PROCEDURE SetFisheyeMapType(v: T; fisheye_map_type: FisheyeMapType);

PROCEDURE SetSemanticColor(v: T; useColor: BOOLEAN);
PROCEDURE SetNodeColor(v: T; color: Color);
PROCEDURE SetFocusColor(v: T; color: Color);
PROCEDURE SetLinkColor(v: T; color: Color);
PROCEDURE SetLabelColor(v: T; color: Color);
PROCEDURE SetLinkThickness(v: T; thickness: INTEGER);
PROCEDURE SetNodeInteriorColor(v: T; color: Color);

(* Establish a margin around the visible part of coordinates.
   The margin will be m times the default node size.  Should be called
   before SetWC; modifies the effect of all SetWC calls thereafter. *)
PROCEDURE SetMarginSize(v : T; m: REAL);


(* Set the world coordinate rectangle.  If the SetMarginSize feature
   is used, this should take place after the margin size and the
   default node size are established. *)
PROCEDURE SetWC(v : T; n, w, s, e : REAL; square: BOOLEAN := TRUE);


PROCEDURE GetWC(v: T; VAR (*out*) n, w, s, e: REAL);


(* Convert a point from world coordinates to screen coordinates.
   These are virtual screen coordinates; there is no checking to
   see that the point is in fact visible on the screen. *)
PROCEDURE WtoS(v: T; rp: RealPoint.T): Point.T;


(* Convert a point from screen coordinates to world coordinates. *)
PROCEDURE StoW(v: T; p: Point.T): RealPoint.T;


(*****************  Other global parameters  *******************)

PROCEDURE SetMaxNodes(v: T; max: CARDINAL);
  (* Establish a maximum number of nodes supported in this view. Calling this
     procedure wipes out the entire graph previously defined, if any.  It
     also establishes the acceptable range of NodeIndex parameters as
     [0..max-1].  If this procedure has never been called on v, then the
     maximum is 100. *)

PROCEDURE SetBg(v: T; tint: PaintOp.T);
  (* Background tint of the vbt.  Default PaintOp.Bg. *)

PROCEDURE SetFont(v: T; font: Font.T);
  (* Font used in node labels.  Default VT6x13. *)


(**********************************************************************)
TYPE
  Color = [-3..253];

CONST
  Black = -2;
  White = -1;
  DefaultColor = -3;

CONST
  Red    = 0;
  Orange = 1;
  Yellow = 2;
  Green  = 3;
  Blue   = 4;
  Purple = 5;
  Maroon = 6;
  Sky = 7;

TYPE
  ColorCount = [0..253];

PROCEDURE SetNColors(v: T; n: ColorCount);
  (* Set the size of the color space (not including Black and White). This
     wipes out any previous color setup, so should be done only during
     initialization. *)


PROCEDURE SetColorFromTint(v: T; color: Color; tint: PaintOp.T);
  (* Define the mapping from GraphVBT color space to an arbitrary tint. *)


PROCEDURE GetColorFromName(name: Text.T): Color;

(******************  Miscellaneous node properties  *******************)

TYPE
  Style = {Filled, Border};

  LabelStyle = {NoLabel, Left, Center, Right};


(*************************** Shapes *****************************
     A shape is defined by a collection of bitmaps for various sizes
     of the two styles Filled and Border.  NameWithin is a variant of
     Border, so it takes to part in shape definition.  Several shapes
     are predefined; clients may add others.
*)

CONST
  MaxShape = 10;

TYPE
  Shape = [0..MaxShape];

CONST  (* predefined shapes *)
  Circle = 0;
  Rectangle = 1;
  Diamond = 2;
  InvTriangle = 3;
  Triangle = 4;
  Pentagon = 5;
  Hexagon = 6;
  Octagon = 8;
  Ellipse = 9;

(* To define a shape, one begins by specifying the maximum
   number of sizes that will be supported for it. *)
PROCEDURE InitShape(shape: Shape; nSizes: CARDINAL);

(* The rest of the job is to provide bitmaps for the various
   sizes and styles. *)
PROCEDURE ProvideShapeCircle(
    shape: Shape;   (* must be already Inited *)
    size: CARDINAL; (* "radius" in pixels *)
    style: Style;
    box: Rect.T;
    pixmap: Pixmap.T) RAISES {Error};

PROCEDURE ProvideShapeRectangle(
    shape: Shape;   (* must be already Inited *)
    size: CARDINAL; (* "radius" in pixels *)
    style: Style;
    box: Rect.T) RAISES {Error};


(*********************** Graph creation  **********************)
PROCEDURE NewNode(
    v: T;
    n: NodeIndex;
    x, y: Coord;
    api: REAL;
    label: Text.T := NIL) RAISES {Error};

(* NewLink: create a link from node a, slot aIndex, to node b, slot
   bIndex.  Order of a and b is unimportant, and it is ok to make
   duplicate calls, one from each end, provided they agree.

   Segmented links, i.e. those that are drawn in more than one piece,
   are permitted, and are specified by providing a non-NIL segPoints,
   which is an array of intermediate vertices, in order from a to b.
   These vertices should be roughly evenly spaced if arrows are to
   be used. *)
PROCEDURE NewLink(
    v: T;
    a, b: NodeIndex;
    aIndex, bIndex: LinkIndex;
    segPoints: GraphData.PointArray := NIL) RAISES {Error};

(* A GraphData.T is a linked data structure that can describe an entire
   graph.  Setup loads the contents of a GraphVBT from such a structure,
   creating nodes and links with default properties.  Node labels
   come from the names of nodes as given in g.  Setup calls SetMaxNodes
   if necessary, and will reserve extraNodes node slots beyond (numbered
   higher than) those filled by g.

   If ComputeWC is true, Setup will also establish world coordinates
   based upon the bounding box of the coordinates of nodes in g.
   The world coordinate box will be that bounding box, expanded by
   a margin equal to <margin> fraction of its greater dimension.
   Thus in the default case, a margin of 1/10 of the greater dimension
   of the bounding box will be used. *)

PROCEDURE Setup(
    v: T;
    g: GraphData.T;
    allowSegmented: BOOLEAN := TRUE;
    extraNodes: CARDINAL := 0;
    computeWC: BOOLEAN := TRUE;
    margin: REAL := 0.1);

PROCEDURE Redraw(v: T; force: BOOLEAN := TRUE);

END GraphVBT.



