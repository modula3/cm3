(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jan 31 11:37:31 PST 1995 by kalsow                   *)
(*      modified on Sat Oct  9 15:16:09 PDT 1993 by mhb                      *)
(*      modified on Tue Jun 16 16:46:29 PDT 1992 by muller                   *)

MODULE GraphVBT;

IMPORT Trestle, TrestleComm;
IMPORT Stdio, Wr, Text, Thread, VBT;
IMPORT ColorTools, PaintOp, Font, Pixmap, ScrnPixmap;
IMPORT Point, RealPoint, Path, Rect, RealRect, Region;
IMPORT Math;

IMPORT GraphData;
IMPORT SortedHashTable, SortedIndexTable;
IMPORT CreatePixmap;

<* FATAL Wr.Failure, Thread.Alerted, Error *>
<* FATAL TrestleComm.Failure *>

CONST MaxInt = LAST(INTEGER);

TYPE
  Node = REF NodeRec;
  Link = REF LinkRec;
  ArrowRef = REF Arrows;
  BiLink = REF BiLinkRec;



(*************************************************************************)  
  NodeRec = RECORD
    pos: RealPoint.T;
    fpos: RealPoint.T; (* for fisheye views *)
    radius: REAL;
    api: REAL;
    fVW: REAL;
    dist: REAL; (* needed for semantic fisheye views *)
    label: Text.T;
    link: ARRAY [0..MaxLink] OF Link;

    color: Color;
    shape: Shape;
    style: Style;
    size: REAL;

    labelStyle: LabelStyle;
    labelColor: Color;
    labelFont: Font.T;
    flabelFont: Font.T;     (* for fisheye views *)
    labelShift: INTEGER;    (* a horizontal positional correction, in pixels *)

    sizeIndex: INTEGER;     (* a cache of size->bitmap translation *)
                            (* -1 means cache is invalid *)
    fsizeIndex: INTEGER;    (* for fisheye views *)
  END;



(*************************************************************************)
  LinkRec = RECORD
    from: Node;
    alterEgo: Link;         (* the link in the opposite direction *)
    primary: BOOLEAN;       (* exactly one link of each pair is primary *)

    color: Color;
    thickness: CARDINAL;    (* pixels *)
    fthickness: CARDINAL;   (* for fisheye views *)

    segPoints: PointArray;
    fsegPoints: PointArray; (* for fisheye views *)

    arrows: ArrowRef;       (* REF ARRAY OF ArrowRec *)
    arrowCount: CARDINAL;   (* number of arrows currently in use *)
    angle: REAL;            (* in radians, a cache used by arrow-drawing *)
                            (* angle is computed once, when arrows is 
                               initialized *)
    divided: BOOLEAN;
    bilinks: ARRAY LinkHalf OF BiLink;   (* color, thickness, label *) 
  END;

  ArrowRec = RECORD
    where: REAL;            (* 0.0 .. 1.0 *)
    color: Color;
    p, q, r: Point.T;       (* in screen coordinates, a cache *)
  END;

  Arrows = ARRAY OF ArrowRec;

  BiLinkRec = RECORD
    color: Color;
    thickness: CARDINAL;     (* pixels *)
    fthickness: CARDINAL;    (* for fisheye views *)
    label: Text.T;
    labelFont: Font.T;
    flabelFont: Font.T;      (* for fisheye views *)
  END;
    
  ColorRec = RECORD
    tint: PaintOp.T;
  END;



(*************************************************************************)
  REVEAL T = VBT.Leaf BRANDED OBJECT
    offscreen: VBT.Leaf;
    
    dist: REF ARRAY OF ARRAY OF REAL;

    maxNodes: CARDINAL;
    table: REF ARRAY OF Node;

    ncolors: INTEGER;             (* number of client-settable colors *)
    colors : REF ARRAY OF ColorRec;

    north, south, east, west: REAL; (* boundaries of world coordinate space *)

    square: BOOLEAN; (* aspect ration *)
    marginSize: REAL;
    background: PaintOp.T;
    foreground: PaintOp.T;
    font: Font.T;
    borderMin: CARDINAL;  (* nodes smaller than this always show as Filled *)

    nodeColor: Color;
    nodeInteriorColor: Color;
    nodeShape: Shape;
    nodeStyle: Style;
    nodeSize:  REAL;

    nodeLabelStyle: LabelStyle;
    nodeLabelColor: Color;
    nodeLabelFont: Font.T;  (* normally Font.Null, meaning use v.font *)
    nodeLabelShift: INTEGER;

    linkColor: Color;
    linkThickness: CARDINAL;
    linkLabelFont: Font.T;

    (* derived properties *)
    viewVisible: BOOLEAN;
      (* whether there is anything to draw and anywhere to draw it *)
    needRedraw: BOOLEAN;
      (* whether anything has been changed in the data structure and
         not yet on the screen. *)
    favoriteSize: REAL;
      (* the size we will currently use if the user has specified
         the default with size=0. *)
    arrowSize: REAL;
      (* the size (world coords) we will use for arrowheads on links *)
    trustCaches: BOOLEAN;
      (* true unless there has just been a size-changing reformat,
         which invalidates all cached pixel-size information. *)

    (* coordinate transformation *)
    scalex, scaley: REAL;
    transx, transy: INTEGER;  (* translation applied after scaling *)

    (* for debugging, not sure if useful *)
    nodeCount : CARDINAL;   (* number of nodes *)
    edgeCount : CARDINAL;   (* number of edges *)

    (* for fisheye fiews *)
    fisheye: BOOLEAN;
    fisheyeType: FisheyeType;
    fisheyeMapType: FisheyeMapType;
    semanticColor: BOOLEAN;
    nodePositionFixed: BOOLEAN;   (* should the nodes move? *)
    cageSize: INTEGER;            (* sensitivity to cursor movement *)
    currentFocusNode: INTEGER;    (* node focused on *)
    fisheyeScreenFocus: Point.T;  (* in screen coordinates *)
    fDistortion: REAL;
    fSizeFactor: REAL;
    fFocusDevice: FocusDevice;
    fisheyeOldFocus: RealPoint.T;
    fisheyeWorldFocus: RealPoint.T;
    focusColor: Color;
    fisheyeText: BOOLEAN;
    highlight: BOOLEAN;

    fisheyeVWThreshold: REAL;
    fisheyeSizeAPICoeff: REAL;
    fisheyeSizeAPIPower: REAL;
    hTable: SortedHashTable.T;
    sTable: SortedIndexTable.T;

  OVERRIDES
    repaint := Repaint;
    reshape := Reshape;
    position := Position;
    mouse := Mouse;
  END;



(*************************************************************************)
TYPE
  ShapeSizeRec = RECORD
    size: CARDINAL;
    boxes: ARRAY Style OF Rect.T;
    pixmaps: ARRAY Style OF Pixmap.T;
  END;

  ShapeRef = REF ARRAY OF ShapeSizeRec;

VAR
  shapes: ARRAY [0..MaxShape] OF ShapeRef;


(*************************************************************************)
PROCEDURE InitShape(shape: Shape; nSizes: CARDINAL) =
  BEGIN
    shapes[shape] := NEW(ShapeRef, nSizes);
    FOR i := 0 TO nSizes-1 DO
      shapes[shape]^[i].size := MaxInt;
    END;
  END InitShape;


(*************************************************************************)
PROCEDURE ProvideShapeRectangle(
    shape: Shape;   (* must be already Inited *)
    size: CARDINAL; (* "diameter" in pixels *)
    style: Style;
    box: Rect.T) RAISES {Error} =
  VAR
    s: ShapeRef;
    n, i: INTEGER;
  BEGIN
    s := shapes[shape];
    n := NUMBER(s^);
    i := 0;
    WHILE (i < n) AND (size > s^[i].size) DO INC(i) END;
    IF (i >= n) THEN 
      RAISE Error(ErrorKind.TooManySizes);
    ELSIF s^[i].size # size THEN
      IF (s^[n-1].size # MaxInt) THEN RAISE Error(ErrorKind.TooManySizes) END;
      FOR j := n-2 TO i BY -1 DO
        s^[j+1] := s^[j];
      END;
      s^[i].size := size;
    END;
    (* now we have a space ready to store the current size *)
    s^[i].boxes[style] := box; 
END ProvideShapeRectangle;


(************************************************************************)
PROCEDURE ProvideShapeCircle(
    shape: Shape;   (* must be already Inited *)
    size: CARDINAL; (* "diameter" in pixels *)
    style: Style;
    box: Rect.T;
    pixmap: Pixmap.T) RAISES {Error} =
  VAR
    s: ShapeRef;
    n, i: INTEGER;
  BEGIN
    s := shapes[shape];
    n := NUMBER(s^);
    i := 0;
    WHILE (i < n) AND (size > s^[i].size) DO INC(i) END;
    IF (i >= n) THEN 
      RAISE Error(ErrorKind.TooManySizes);
    ELSIF s^[i].size # size THEN
      IF (s^[n-1].size # MaxInt) THEN RAISE Error(ErrorKind.TooManySizes) END;
      FOR j := n-2 TO i BY -1 DO
        s^[j+1] := s^[j];
      END;
      s^[i].size := size;
    END;
    (* now we have a space ready to store the current size *)
    s^[i].boxes[style] := box;
    s^[i].pixmaps[style] := pixmap;   
END ProvideShapeCircle;


(***********************************************************************)
PROCEDURE SetUpShapes() =  (* to be called during initialization *)
  BEGIN
    InitShape(Circle, 64);
    InitShape(Rectangle, 64);

    FOR i := 0 TO 63 DO
        ProvideShapeCircle(Circle, i, Style.Filled,
          CreatePixmap.CircleBox(FLOAT(i)),
          CreatePixmap.Circle(FLOAT(i)));
        ProvideShapeRectangle(Rectangle, i, Style.Filled,
          CreatePixmap.RectangleBox(FLOAT(2*i), FLOAT(2*i)));
    END;

    FOR i := 0 TO 63 DO
      IF i > 0 THEN
          ProvideShapeCircle(Circle, i, Style.Border,
            CreatePixmap.CircleBox(FLOAT(i)),
            CreatePixmap.Circle(FLOAT(i-1)));
          ProvideShapeRectangle(Rectangle, i, Style.Border,
            CreatePixmap.RectangleBox(FLOAT(2*i-2), FLOAT(2*i-2)));
      ELSE
          ProvideShapeCircle(Circle, i, Style.Border,
            CreatePixmap.CircleBox(FLOAT(i)),
            CreatePixmap.Circle(FLOAT(i)));
          ProvideShapeRectangle(Rectangle, i, Style.Border,
            CreatePixmap.RectangleBox(FLOAT(2*i), FLOAT(2*i)));
      END;
    END;
END SetUpShapes;


(****************************  Color space  *****************************)
PROCEDURE SetColors (v: T) =
  BEGIN
    SetNColors(v, 32);
    v.colors^[Red + 2].tint :=
      PaintOp.FromRGB(r := 1.0, g := 0.0, b := 0.0);
    v.colors^[Orange + 2].tint :=
      PaintOp.FromRGB(r := 0.7, g := 0.2, b := 0.0);
    v.colors^[Yellow + 2].tint :=
      PaintOp.FromRGB(r := 0.1, g := 0.3, b := 0.4);
    v.colors^[Green + 2].tint :=
      PaintOp.FromRGB(r := 0.0, g := 1.0, b := 0.0);
    v.colors^[Blue + 2].tint :=
      PaintOp.FromRGB(r := 0.0, g := 0.0, b := 1.0);
    v.colors^[Purple + 2].tint :=
      PaintOp.FromRGB(r := 0.8, g := 0.0, b := 0.5);
    v.colors^[Maroon + 2].tint :=
      PaintOp.FromRGB(r := 0.4, g := 0.1, b := 0.1);
    v.colors^[Sky + 2].tint :=
      PaintOp.FromRGB(r := 0.8, g := 0.8, b := 1.0);
    WITH start = Sky + 3 DO
      FOR i := start TO start + 10 DO
        v.colors^[i].tint :=
          ColorTools.FromHSV(
            h := FLOAT(i - start + 1) / 15.0, s := 1.0, v := 1.0,
            mode := PaintOp.Mode.Accurate);
      END
    END
  END SetColors;


(************************************************************************)
PROCEDURE SetNColors(v: T; n: ColorCount) =
  BEGIN
      v.ncolors := n;
      v.colors := NEW(REF ARRAY OF ColorRec, n + 2);
      v.colors^[0].tint := PaintOp.Fg;
      v.colors^[1].tint := PaintOp.Bg;
  END SetNColors;

PROCEDURE SetColorFromTint(v: T; color: Color; tint: PaintOp.T) =
  BEGIN
    v.colors^[color + 2].tint := tint;
  END SetColorFromTint;


PROCEDURE GetColorFromName(name: Text.T): Color =
  BEGIN
    IF Text.Equal(name, "White") THEN
      RETURN White;
    ELSIF Text.Equal(name, "Black") THEN
      RETURN Black;
    ELSIF Text.Equal(name, "Red") THEN
      RETURN Red;
    ELSIF Text.Equal(name, "Orange") THEN
      RETURN Orange;
    ELSIF Text.Equal(name, "Yellow") THEN
      RETURN Yellow;
    ELSIF Text.Equal(name, "Green") THEN
      RETURN Green;
    ELSIF Text.Equal(name, "Blue") THEN
      RETURN Blue;
    ELSIF Text.Equal(name, "Purple") THEN
      RETURN Purple;
    ELSIF Text.Equal(name, "Maroon") THEN
      RETURN Maroon;
    ELSIF Text.Equal(name, "Sky") THEN
      RETURN Sky;
    ELSE
      Wr.PutText(Stdio.stderr, "Unrecogniged color\n");
      RETURN DefaultColor;
    END;
  END GetColorFromName;


(*************************************************************************)
PROCEDURE SetFisheye(v: T; READONLY val: BOOLEAN) =
  BEGIN
    IF v.fisheye # val THEN
      v.fisheye := val;
      IF NOT v.fisheye THEN
        RedrawMe(v, Rect.Full);
      ELSE
        UpdateView(v);
      END;
    END;
  END SetFisheye;


PROCEDURE GetFisheye(v: T): BOOLEAN =
  BEGIN
    RETURN v.fisheye;
  END GetFisheye;

PROCEDURE SetShape (v: T; shape: Shape) =
  BEGIN
    IF v.nodeShape # shape THEN
      v.nodeShape := shape;
      RedrawFisheye (v, Rect.Full);
    END;    
  END SetShape;

PROCEDURE SetStyle (v: T; style: Style) =
  BEGIN
    IF v.nodeStyle # style THEN
      v.nodeStyle := style;
      RedrawFisheye (v, Rect.Full);
    END;    
  END SetStyle;

PROCEDURE SetSemanticColor (v: T; useColor: BOOLEAN) =
  BEGIN
    IF v.semanticColor # useColor THEN
      v.semanticColor := useColor;
      IF v.fisheye THEN UpdateView(v) END
    END;
  END SetSemanticColor;

PROCEDURE SetFisheyeText(v: T; READONLY val: BOOLEAN) =
  BEGIN
    IF v.fisheyeText # val THEN
      v.fisheyeText := val;
      Redraw(v);
    END;
  END SetFisheyeText;


PROCEDURE GetFisheyeText(v: T): BOOLEAN =
  BEGIN
    RETURN v.fisheyeText;
  END GetFisheyeText;


PROCEDURE SetCageSize(v: T; READONLY val: INTEGER) =
  BEGIN
    v.cageSize := val;
  END SetCageSize;


PROCEDURE GetCageSize(v: T): INTEGER =
  BEGIN
    RETURN v.cageSize;
  END GetCageSize;


(**********************************************************************)
PROCEDURE SetFisheyeFocus(v: T; focus: RealPoint.T) =
  BEGIN
    IF NOT RealPoint.Equal(v.fisheyeWorldFocus, focus) THEN
      v.fisheyeWorldFocus := focus;
      IF v.fisheye THEN
        UpdateView(v);
      END;
    END;
  END SetFisheyeFocus;


PROCEDURE GetFisheyeFocus(v: T): RealPoint.T =
  BEGIN
    RETURN v.fisheyeWorldFocus;
  END GetFisheyeFocus;


PROCEDURE SetFisheyeDistortion(v: T; distortion: REAL) =
  BEGIN
    IF v.fDistortion # distortion THEN
      v.fDistortion := distortion;
      IF v.fisheye THEN
        UpdateView(v);
      END;
    END;
  END SetFisheyeDistortion;


PROCEDURE GetFisheyeDistortion(v: T;): REAL =
  BEGIN
    RETURN v.fDistortion;
  END GetFisheyeDistortion;


PROCEDURE SetFisheyeSizeFactor(v: T; sizeFactor: REAL) =
  BEGIN
    IF v.fSizeFactor # sizeFactor THEN
      v.fSizeFactor := sizeFactor;
      IF v.fisheye THEN
        UpdateView(v);
      END;
    END;
  END SetFisheyeSizeFactor;


PROCEDURE GetFisheyeSizeFactor(v: T): REAL =
  BEGIN
    RETURN v.fSizeFactor;
  END GetFisheyeSizeFactor;


PROCEDURE GetFisheyeFocusSize(v: T): INTEGER = 
  BEGIN
    IF v.currentFocusNode < 0 THEN RETURN -1 END;
    WITH node = v.table^[v.currentFocusNode] DO
      RETURN node.fsizeIndex;
    END
  END GetFisheyeFocusSize;


PROCEDURE SetFisheyeVWThreshold(v:T; READONLY val: REAL)=
  BEGIN
    IF v.fisheyeVWThreshold # val THEN
      v.fisheyeVWThreshold := val;
      IF v.fisheye THEN
        UpdateView(v);
      END;
    END;
  END SetFisheyeVWThreshold;


PROCEDURE GetFisheyeVWThreshold(v: T): REAL =
  BEGIN
    RETURN v.fisheyeVWThreshold;
  END GetFisheyeVWThreshold;


PROCEDURE SetFisheyeSizeAPICoeff(v:T; READONLY val: REAL) =
  BEGIN
    IF v.fisheyeSizeAPICoeff # val THEN
      v.fisheyeSizeAPICoeff := val;
      IF v.fisheye THEN
        UpdateView(v);
      END;
    END;
  END SetFisheyeSizeAPICoeff;


PROCEDURE GetFisheyeSizeAPICoeff(v: T): REAL =
  BEGIN
    RETURN v.fisheyeSizeAPICoeff;
  END GetFisheyeSizeAPICoeff;


PROCEDURE SetFisheyeSizeAPIPower(v:T; READONLY val: REAL) =
  BEGIN
    IF v.fisheyeSizeAPIPower # val THEN
      v.fisheyeSizeAPIPower := val;
      IF v.fisheye THEN
        UpdateView(v);
      END;
    END;
  END SetFisheyeSizeAPIPower;


PROCEDURE GetFisheyeSizeAPIPower(v: T): REAL =
  BEGIN
    RETURN v.fisheyeSizeAPIPower;
  END GetFisheyeSizeAPIPower;


PROCEDURE SetFisheyeFocusDevice(v: T; device: FocusDevice) =
  BEGIN
    v.fFocusDevice := device;
  END SetFisheyeFocusDevice;


PROCEDURE GetFisheyeFocusDevice(v: T): FocusDevice =
  BEGIN
    RETURN v.fFocusDevice;
  END GetFisheyeFocusDevice;



PROCEDURE SetNodeColor(v: T; color: Color) =
  BEGIN
    v.nodeColor := color;
  END SetNodeColor;


PROCEDURE SetFocusColor(v: T; color: Color) =
  BEGIN
    v.focusColor := color;
  END SetFocusColor;


PROCEDURE SetLinkColor(v: T; color: Color) =
  BEGIN
    v.linkColor := color;
  END SetLinkColor;


PROCEDURE SetLabelColor(v: T; color: Color) =
  BEGIN
    v.nodeLabelColor := color;
  END SetLabelColor;

PROCEDURE SetLinkThickness(v: T; thickness: INTEGER) =
  BEGIN
    IF v.linkThickness # thickness THEN
      v.linkThickness := thickness;
    END;
  END SetLinkThickness;


PROCEDURE SetNodeInteriorColor(v: T; color: Color) =
  BEGIN
    v.nodeInteriorColor := color;
  END SetNodeInteriorColor;



(*********************************************************************)
(* graph related parameters                                          *)
(*********************************************************************)
PROCEDURE SetMaxNodes(v: T; max: CARDINAL) =
  BEGIN
    v.maxNodes := max;
    v.table := NEW(REF ARRAY OF Node, max);
  END SetMaxNodes;


PROCEDURE SetMarginSize(v : T; m: REAL) =
  BEGIN
    v.marginSize := m;
  END SetMarginSize;

PROCEDURE SetBg(v: T; tint: PaintOp.T) =
  BEGIN
    v.background := tint;
    IF v.viewVisible THEN RedrawMe(v, Rect.Full) END;
  END SetBg;

PROCEDURE SetFont(v: T; font: Font.T) =
  BEGIN
    v.font := font;
    IF v.viewVisible THEN RedrawMe(v, Rect.Full) END;
  END SetFont;


(**********************************************************************)
PROCEDURE SetFisheyeType(v: T; fisheye_type: FisheyeType) =
  VAR
    change: BOOLEAN;
  BEGIN
    change := FALSE;
    IF NOT v.fisheye THEN
      v.fisheye := TRUE;
      change := TRUE;
    END;
    IF v.fisheyeType # fisheye_type THEN
      v.fisheyeType := fisheye_type;
      change := TRUE;
    END;
    IF change THEN
      IF v.fisheyeType # FisheyeType.Graphical THEN
        ComputeDistance(v)
      END;
      UpdateView(v);
    END;
  END SetFisheyeType;


(**********************************************************************)
PROCEDURE SetFisheyeMapType(v: T; fisheye_map_type: FisheyeMapType) =
  BEGIN
    IF v.fisheyeMapType # fisheye_map_type THEN
      v.fisheyeMapType := fisheye_map_type;
      IF NOT v.fisheye THEN
        RedrawMe(v, Rect.Full);
      ELSE
        UpdateView(v);
      END;
    END;
  END SetFisheyeMapType;


PROCEDURE SetWC(v : T; n, w, s, e : REAL; square: BOOLEAN := TRUE) =
  VAR
    margin: REAL;
  BEGIN
    (* ASSERT((w # e) AND (n # s),"GraphVBT: degenerate world coordinates"); *)
    v.square := square;
    margin := v.marginSize * v.nodeSize;
    v.west := w - margin;
    v.east := e + margin;
    IF n < s THEN
      v.north := n - margin;
      v.south := s + margin;
    ELSE
      v.north := n + margin;
      v.south := s - margin;
    END;

    IF v.viewVisible THEN (* do as Reshape does *)
      v.trustCaches := FALSE;
      ComputeTransformation(v, VBT.Domain(v));
      RedrawMe(v, Rect.Full);
    END;

  END SetWC;

PROCEDURE GetWC(v : T; VAR (*out*) n, w, s, e : REAL) =
  BEGIN
    n := v.north;
    w := v.west;
    s := v.south;
    e := v.east;
  END GetWC;



(*************************  Graph creation  ***************************)
PROCEDURE NewNode(
    v: T;
    n: NodeIndex;
    x, y: Coord;
    api: REAL;
    label: Text.T := NIL) RAISES {Error} = 
  VAR
    node: Node;
  BEGIN
    IF n >= v.maxNodes THEN RAISE Error(ErrorKind.NoSuchNode) END;
    node := NEW(Node);
    v.table^[n] := node;
    node.pos := RealPoint.T{x,y}; 
    node.fpos := RealPoint.T{x,y};
    node.radius := 0.0;
    node.api := api;
    node.fVW := 1.0;
    node.label := label;
    node.color := v.nodeColor;
    node.shape := v.nodeShape;
    node.style := v.nodeStyle;
    node.size := v.nodeSize;
    node.labelStyle := v.nodeLabelStyle;
    node.labelColor := v.nodeLabelColor;
    node.labelFont := v.nodeLabelFont;
    node.labelShift := v.nodeLabelShift;
  END NewNode;


(***********************************************************************)
PROCEDURE NewLink(
    v: T;
    a, b: NodeIndex;
    aIndex, bIndex: LinkIndex;
    segmentPoints: PointArray := NIL) RAISES {Error} =
  VAR
    nodea, nodeb: Node;
    linka, linkb: Link;
  BEGIN
    nodea := v.table^[a];
    nodeb := v.table^[b];
 
    IF (nodea = NIL) OR (nodeb = NIL) THEN 
      RAISE Error(ErrorKind.NoSuchNode);
    END;

    linka := NEW(Link);
    linkb := NEW(Link);

    linka.from := nodea;
    linka.alterEgo := linkb;
    linka.primary := TRUE;
    linka.color := v.linkColor;
    linka.thickness := v.linkThickness;
    linka.segPoints := segmentPoints;
    IF linka.segPoints # NIL THEN
      linka.fsegPoints := NEW(PointArray, NUMBER(linka.segPoints^));    
      linka.fsegPoints^ := linka.segPoints^;
    END;
    linka.divided := FALSE;

    linkb.from := nodeb;
    linkb.alterEgo := linka;
    linkb.primary := FALSE;
    linkb.color := v.linkColor;
    linkb.thickness := v.linkThickness;
    IF segmentPoints # NIL THEN
      linkb.segPoints := GraphData.RevPoints(segmentPoints);
      linkb.fsegPoints := NEW(PointArray, NUMBER(linkb.segPoints^));
      linkb.fsegPoints^ := linkb.segPoints^;
    END;
    linkb.divided := FALSE;

    nodea.link[aIndex] := linka;
    nodeb.link[bIndex] := linkb;
  END NewLink;


CONST
  Huge = 1.0E8;

(*************************************************************************)
PROCEDURE Setup(
    v: T;
    g: GraphData.T;
    allowSegmented: BOOLEAN := TRUE;
    extraNodes: CARDINAL := 0;
    computeWC: BOOLEAN := TRUE;
    margin: REAL := 0.1) =
  VAR
    n: CARDINAL;
    marg: REAL;
    bbox: RealRect.T;
  BEGIN
    n := NUMBER(g^);
    IF n + extraNodes > v.maxNodes THEN
      SetMaxNodes(v, n + extraNodes)
    ELSE
      v.maxNodes := n+extraNodes;
    END;

    v.hTable := SortedHashTable.New(v.maxNodes);
    v.sTable := SortedIndexTable.New(v.maxNodes);

    (* pass 1: make the nodes *)
    FOR i := 0 TO n-1 DO
      IF g^[i].present THEN
        NewNode(v, i, g^[i].x, g^[i].y, g^[i].api, g^[i].name) END;
    END; (* FOR i *)

    (* pass 2: put in the links *)
    FOR i := 0 TO n-1 DO
      IF g^[i].present THEN
        FOR j := 0 TO GraphData.MaxEdge DO
          WITH e = g^[i].edge[j] DO
            IF e.present AND (i < e.dest) (* avoid duplicate effort *) THEN
              IF allowSegmented THEN
                NewLink(v, i, e.dest, j, e.destEdge, e.segPoints);
              ELSE
                NewLink(v, i, e.dest, j, e.destEdge, NIL);
              END;
            END;
          END; (* WITH *)
        END; (* FOR j *)
      END; (* IF node is present *)
    END; (* FOR i *)

    IF computeWC THEN
      bbox := GraphData.BoundingBox(g);
      IF (bbox.east > bbox.west) OR (bbox.south > bbox.north) THEN
        marg := MAX(bbox.east - bbox.west, bbox.south - bbox.north) 
                  * margin;
      END;
      (* world coordinates:  y increasing upward *)
      SetWC(v, bbox.south+marg, bbox.west-marg, 
          bbox.north-marg, bbox.east+marg);
      (* does an implicit redraw *)
    END;

    IF v.fisheyeType # FisheyeType.Graphical THEN
      ComputeDistance(v);
    END;

  END Setup;



(***************************  Methods  ******************************)
PROCEDURE Mouse(v: T; READONLY cd: VBT.MouseRec) =
  VAR
     delta, fdelta, mouse_point: RealPoint.T;
     Xmax, Ymax: REAL;
     
  BEGIN
    IF (v.fFocusDevice = FocusDevice.Mouse) AND (v.fisheye) AND
      (NOT cd.cp.gone) THEN
      IF (cd.clickType = VBT.ClickType.FirstDown) THEN
        mouse_point := StoW(v, cd.cp.pt);
        IF v.fisheyeMapType = FisheyeMapType.Cartesian THEN     
          fdelta := RealPoint.Sub(mouse_point, v.fisheyeOldFocus);
          IF fdelta.h < 0.0 THEN 
     	    Xmax := v.west - v.fisheyeOldFocus.h;
          ELSE
            Xmax := v.east - v.fisheyeOldFocus.h;
          END;
     	  IF fdelta.v < 0.0 THEN
            Ymax := v.south - v.fisheyeOldFocus.v;
          ELSE
     	    Ymax := v.north - v.fisheyeOldFocus.v;
          END;
          delta := UnmapPoint(v, fdelta, Xmax, Ymax);
          v.fisheyeWorldFocus := RealPoint.Add(delta, v.fisheyeOldFocus);
        ELSE
          v.fisheyeWorldFocus := mouse_point;
        END;
        NewScreenFocus(v);
      ELSIF (cd.clickType = VBT.ClickType.LastUp) AND (v.fisheyeText) THEN
	RedrawFisheye(v, Rect.Full);
      END;
    END;
  END Mouse;


(*************************************************************************) 
PROCEDURE Position(v: T; READONLY cd: VBT.PositionRec) =
  BEGIN
    IF (v.fisheye) AND
      (v.fFocusDevice = FocusDevice.Mouse) AND
      (NOT cd.cp.gone) AND
      (VBT.Modifier.MouseL IN cd.modifiers) THEN
        v.fisheyeWorldFocus := StoW(v, cd.cp.pt);
        NewScreenFocus(v);
    END;
    IF v.cageSize # 0 THEN
      VBT.SetCage(v, VBT.CageFromRect(
        Rect.T{cd.cp.pt.h - v.cageSize,
               cd.cp.pt.h + v.cageSize,
               cd.cp.pt.v - v.cageSize,
               cd.cp.pt.v + v.cageSize},
        cd.cp));
    ELSE
      VBT.SetCage(v, VBT.CageFromPosition(cd.cp));
    END;
  END Position;


(*************************************************************************)
PROCEDURE Reshape(v: T; READONLY cd: VBT.ReshapeRec) =
  BEGIN
    IF Rect.IsEmpty(cd.new) THEN
      v.viewVisible := FALSE;
    ELSE
      SetImage(v);
      v.viewVisible := TRUE;
      v.trustCaches := FALSE;
      ComputeTransformation(v, cd.new);
      IF v.fisheye THEN
        RedrawFisheye(v, Rect.Full);
      ELSE
        RedrawMe(v, Rect.Full);
      END;
    END;
  END Reshape;


(***********************************************************************)
PROCEDURE Repaint(v: T; READONLY r: Region.T) =
  BEGIN
    (* later we should really clip to r *)
      IF v.fisheye THEN
        RedrawFisheye(v, r.r);
      ELSE
        RedrawMe(v, r.r);
      END;
  END Repaint;


(**********************************************************************)
PROCEDURE Redraw(v: T; <* UNUSED *> force: BOOLEAN := TRUE) =
  BEGIN
    IF v.fisheye THEN
      RedrawFisheye(v, Rect.Full);
    ELSE
      RedrawMe(v, Rect.Full);
    END;
  END Redraw;



(********************** Painting procedures ******************************)
PROCEDURE RedrawMe(
    v: T;
    READONLY clip: Rect.T) =
  VAR
    node: Node;
    link: Link;
    p: Point.T;
    labelBox, drawBox: Rect.T;
  BEGIN


     VBT.PaintTint(v, clip, v.background);
     (* links first, then nodes, for better appearance *)
      FOR i := 0 TO v.maxNodes-1 DO
        node := v.table^[i];
        IF node # NIL THEN
          FOR j := 0 TO MaxLink DO
            link := node.link[j];
            IF (link # NIL) AND (link.primary) THEN
              DrawLink(v, link, FALSE, v.trustCaches);
            END;
          END;
        END;
      END;

      FOR i := 0 TO v.maxNodes-1 DO
        node := v.table^[i];
        IF node # NIL THEN
          DrawNode(v, node, v.trustCaches);
          p := ConvertPoint(v, node.pos);
          labelBox := VBT.BoundingBox(v, node.label, v.font);
          drawBox := Rect.Inset(
            shapes[node.shape]^[node.sizeIndex].boxes[node.style], 1);
          IF (node.labelStyle # LabelStyle.NoLabel) AND
            (node.label # NIL) AND (
            ((Rect.HorSize(labelBox) <= Rect.HorSize(drawBox)) AND
             (Rect.VerSize(labelBox) <= Rect.VerSize(drawBox)))) THEN
            DrawLabel(v, node, p);
          END;
        END;
      END;
    v.trustCaches := TRUE;
  END RedrawMe;


PROCEDURE DrawNode(
    v: T; 
    node: Node; 
    trustSize: BOOLEAN := TRUE) =
  VAR
    p: Point.T;
  BEGIN
    p := ConvertPoint(v, node.pos);
    IF (NOT trustSize) OR (node.sizeIndex < 0) THEN 
      node.sizeIndex := ChooseSize(v, node, node.style);
    END;
    PaintBox(v, p, shapes[node.shape]^[node.sizeIndex], node.style, -1);
  END DrawNode;


PROCEDURE DrawLink(
    v: T; 
    link: Link; 
    <* UNUSED *> andEnds: BOOLEAN := TRUE;
    <* UNUSED *> trustCaches: BOOLEAN := TRUE) =
  VAR
    p1, p2: Point.T;
  BEGIN
    IF link.segPoints # NIL THEN 
        DrawLinkSegmented(v, link);
     ELSE
       p1 := ConvertPoint(v, link.from.pos);
       p2 := ConvertPoint(v, link.alterEgo.from.pos);
       PaintLink(v, link, p1, p2);
      END;
  END DrawLink;


PROCEDURE DrawLinkSegmented(
    v: T;
    link: Link;
    <* UNUSED *> erase: BOOLEAN := FALSE) =
  VAR
    p1, p2, pFinal: Point.T;
    final: CARDINAL;
    path: Path.T;
    p, q, r, s: Point.T;
    count: INTEGER;
  BEGIN
      count := 0;
      p := Point.Origin; q := Point.Origin;
      r := Point.Origin; s := Point.Origin;
      path := NEW(Path.T);

      p1 := ConvertPoint(v, link.from.pos);

      p := p1;
      count := count + 1;
      Path.MoveTo(path, p);

      pFinal := ConvertPoint(v, link.alterEgo.from.pos);
      final := NUMBER(link.segPoints^);

      FOR i := 0 TO final DO
        IF i < final THEN 
          p2 := ConvertPoint(v, link.segPoints^[i]);

          count := count + 1;
          IF count = 4 THEN
            s := p2;
            Path.CurveTo(path, q, r, s);
            p := s;
            count := 1;
          ELSE
            IF count = 2 THEN
              q := p2;
            ELSIF count = 3 THEN
              r := p2;
            END;
          END;
        ELSE 
          p2 := pFinal;

          count := count + 1;
          IF count = 2 THEN
            s := p2;
            Path.LineTo(path, s);
          ELSIF count = 3 THEN
            Path.CurveTo(path, q, q, s);
          ELSIF count = 4 THEN
            Path.CurveTo(path, q, r, s);
          END;
          count := 1;
          p := p2;
        END;

        PaintLink(v, link, p1, p2);
        p1 := p2;
      END;
     
      (* path := Path.Flatten(path);
      VBT.Stroke(v, Rect.Full, path); *)

  END DrawLinkSegmented;


PROCEDURE DrawLabel(v: T; node: Node; p: Point.T) =
  VAR
    mid, refpt: Point.T;
    r: Rect.T;
    tint: PaintOp.T;
    font: Font.T;
  BEGIN
    r := VBT.BoundingBox(v, node.label, v.font);
    mid := Rect.Middle(r);
    refpt.v := p.v - mid.v + 1; (* +1 adjusts for absent descenders *)
   
    CASE node.labelStyle OF
      LabelStyle.Left =>  refpt.h := p.h - r.east - 
                            shapes[node.shape]^[node.sizeIndex].size - 3;
    | LabelStyle.Center => refpt.h := p.h - mid.h;
    | LabelStyle.Right => refpt.h := p.h - r.west +
                            shapes[node.shape]^[node.sizeIndex].size + 3;
    ELSE <* ASSERT FALSE *>
    END;

    INC(refpt.h, node.labelShift);
    IF v.nodeLabelColor # DefaultColor THEN
      tint := v.colors^[v.nodeLabelColor+2].tint;
    ELSIF (node.style = Style.Filled) AND 
      (node.labelStyle = LabelStyle.Center) THEN
      tint := v.background;
    ELSE
      tint := PaintOp.Fg;
    END;
    IF node.labelFont # Font.BuiltIn THEN font := node.labelFont;
    ELSE font := v.font;
    END;
    PaintText(v, refpt, font, tint, node.label);
  END DrawLabel;




PROCEDURE RedrawFisheye (v: T; clip: Rect.T; withText := TRUE) =
  VAR
    node  : Node;
    p     : Point.T;
    br    : Region.T;
    pixmap: ScrnPixmap.T;
  BEGIN

    VBT.PaintTint(v.offscreen, clip, v.background);
    FOR i := 0 TO v.maxNodes - 1 DO
      node := v.table^[i];
      (* node := v.table^[v.sTable.items^[i].data]; *)
      IF (node # NIL) AND (node.fVW >= v.fisheyeVWThreshold) THEN
        FOR j := 0 TO MaxLink DO
          VAR link := node.link[j];
          BEGIN
            IF (link # NIL) AND (link.primary)
                 AND (link.alterEgo.from.fVW
                        >= v.fisheyeVWThreshold) THEN
              FisheyeDrawLink(v, link);
            END;
          END;
        END;
      END;
    END;

    FOR i := 0 TO v.maxNodes - 1 DO
      node := v.table^[i];
      (* node := v.table^[v.sTable.items^[i].data]; *)
      IF (node # NIL) AND (node.fVW >= v.fisheyeVWThreshold)
           AND (node.fsizeIndex > 0) THEN
        FisheyeDrawNode(v, node, i);
        IF withText AND (v.fisheyeText) AND (node.label # NIL) THEN
          FisheyeDrawLabel(v, node);
        END;
      END;
    END;

    (* to make sure that the focus is on top of everything
       else *)
    IF v.currentFocusNode # -1 THEN
      node := v.table^[v.currentFocusNode];
      FisheyeDrawFocusNode(v, node);
      IF withText AND (v.fisheyeText) AND (node.label # NIL) THEN
        FisheyeDrawLabel(v, node);
      END;
    END;

    (* draw a plus sign for mouse *)
    p := ConvertPoint(v, v.fisheyeWorldFocus);
    VBT.Line(v.offscreen, Rect.Full, Point.T{p.h - 6, p.v},
             Point.T{p.h + 6, p.v}, op := PaintOp.Fg);
    VBT.Line(v.offscreen, Rect.Full, Point.T{p.h, p.v - 6},
             Point.T{p.h, p.v + 6}, op := PaintOp.Fg);

    pixmap := VBT.Capture(v.offscreen, VBT.Domain(v), br);
    VBT.PaintScrnPixmap(v, src := pixmap, delta := Point.Origin);
    VBT.Sync(v);
    pixmap.free();
    v.trustCaches := TRUE;
  END RedrawFisheye;


(*********************************************************************)

PROCEDURE FisheyeDrawNode (v: T; node: Node; id: INTEGER) =
  VAR p := ConvertPoint(v, node.fpos);
  BEGIN
    IF v.fisheyeType = FisheyeType.Graphical THEN
      PaintBox(v, p, shapes[node.shape]^[node.fsizeIndex],
               node.style, -1)
    ELSE
      IF v.semanticColor THEN
        PaintBox(
          v, p, shapes[node.shape]^[node.fsizeIndex], node.style,
          TRUNC(v.dist[v.currentFocusNode, id]));
      ELSE
        (* adjust the size *)
        WITH dist = MIN(
                      10, TRUNC(v.dist[v.currentFocusNode, id])),
             sz = MAX(2, node.fsizeIndex + 5 - 2*dist) DO
          PaintBox(v, p, shapes[node.shape]^[sz], node.style, -1)
        END
      END
    END
  END FisheyeDrawNode;

PROCEDURE FisheyeDrawFocusNode(v: T; node: Node) =
  VAR
    p := ConvertPoint(v, node.fpos);
  BEGIN
    PaintFocusBox(v, p, shapes[node.shape]^[node.fsizeIndex], node.style);
END FisheyeDrawFocusNode;


(********************************************************************)
PROCEDURE FisheyeDrawLink(v: T; link: Link) =
  VAR
    p1, p2: Point.T;
  BEGIN
    IF link.fsegPoints # NIL THEN 
      FisheyeDrawLinkSegmented(v, link);
    ELSE
      p1 := ConvertPoint(v, link.from.fpos);
      p2 := ConvertPoint(v, link.alterEgo.from.fpos);
      PaintLink(v, link, p1, p2);
    END;
  END FisheyeDrawLink;

(********************************************************************)
PROCEDURE FisheyeDrawLinkSegmented(
    v: T;
    link: Link) =
  VAR
    p1, p2, pFinal: Point.T;
    final: CARDINAL;
    path: Path.T;
    p, q, r, s: Point.T;
    count: INTEGER;
  BEGIN
      count := 0;
      path := NEW(Path.T);
      p1 := ConvertPoint(v, link.from.fpos);
      p := p1;
      count := count + 1;
      Path.MoveTo(path, p);
      pFinal := ConvertPoint(v, link.alterEgo.from.fpos);
      final := NUMBER(link.fsegPoints^);
     
      FOR i := 0 TO final DO
        IF i < final THEN 
          p2 := ConvertPoint(v, link.fsegPoints^[i]);

          count := count + 1;
          IF count = 4 THEN
            s := p2;
            Path.CurveTo(path, q, r, s);
            p := s;
            count := 1;
          ELSE
            IF count = 2 THEN
              q := p2;
            ELSIF count = 3 THEN
              r := p2;
            END;
          END;
        ELSE 
          p2 := pFinal;

          count := count + 1;
          IF count = 2 THEN
            s := p2;
            Path.LineTo(path, s);
          ELSIF count = 3 THEN
            Path.CurveTo(path, q, q, s);
          ELSIF count = 4 THEN
            Path.CurveTo(path, q, r, s);
          END;
          count := 1;
          p := p2;
        END;        
        PaintLink(v, link, p1, p2);
        p1 := p2;
      END;

      (* path := Path.Flatten(path);
      VBT.Stroke(v.offscreen, Rect.Full, path); *)

  END FisheyeDrawLinkSegmented;


(***********************************************************************)
PROCEDURE FisheyeDrawLabel(v: T; node: Node) =
  VAR
    mid, refpt: Point.T;
    r: Rect.T;
    tint: PaintOp.T;
    font: Font.T;
    drawBox: Rect.T;
    i, labelLen: INTEGER;
    label: Text.T;
    done: BOOLEAN;
    p: Point.T;
  BEGIN
    p := ConvertPoint(v, node.fpos);
    drawBox := Rect.Inset(
      shapes[node.shape]^[node.fsizeIndex].boxes[node.style], 2);
    labelLen := Text.Length(node.label);
    done := FALSE;
    i := labelLen + 1;
    WHILE (NOT done) AND (i > 0) DO
      i := i - 1;
      label := Text.Sub(node.label, 0, i);
      r := VBT.BoundingBox(v.offscreen, label, v.font);
      IF (Rect.HorSize(r) < Rect.HorSize(drawBox)) AND
        (Rect.VerSize(r) < Rect.VerSize(drawBox)) THEN
          done := TRUE;
      END;
    END;
    IF i > 0 THEN
      mid := Rect.Middle(r);
      refpt.v := p.v - mid.v + 1; (* +1 adjusts for absent descenders *)

      CASE node.labelStyle OF
        LabelStyle.Left =>  refpt.h := p.h - r.east - 
                            shapes[node.shape]^[node.fsizeIndex].size - 3;
      | LabelStyle.Center => refpt.h := p.h - mid.h;
      | LabelStyle.Right => refpt.h := p.h - r.west +
                            shapes[node.shape]^[node.fsizeIndex].size + 3;
      ELSE <* ASSERT FALSE *>
      END;

      INC(refpt.h, node.labelShift);
      IF v.nodeLabelColor # DefaultColor THEN
        tint := v.colors^[v.nodeLabelColor+2].tint;
      ELSIF (node.style = Style.Filled) AND 
        (node.labelStyle = LabelStyle.Center) THEN
        tint := v.background;
      ELSE
        tint := PaintOp.Fg;
      END;
      IF node.labelFont # Font.BuiltIn THEN font := node.labelFont;
      ELSE font := v.font;
      END;
      PaintText(v, refpt, font, tint, label);
    END;
  END FisheyeDrawLabel;


(***************************** painting ***************************)
PROCEDURE PaintBox(
    v: T;
    READONLY p: Point.T;
    READONLY sh: ShapeSizeRec; 
    <* UNUSED *> style: Style;
    distance: INTEGER) =
  VAR
    tint_border := v.colors^[v.nodeColor+2].tint;
    tint_interior := v.colors^[v.nodeInteriorColor+2].tint;
  BEGIN
    IF distance # -1 THEN
      tint_interior := v.colors^[MIN(distance,10)+Sky+3].tint;
    END;
    IF v.nodeShape = Rectangle THEN
      VBT.PaintTint(v.offscreen, 
        Rect.Add(sh.boxes[Style.Filled], p), tint_border);
      IF v.nodeStyle = Style.Border THEN
        VBT.PaintTint(v.offscreen, Rect.Add(sh.boxes[Style.Border], p),
          tint_interior); 
      END;
    ELSE
      VBT.PaintPixmap(v.offscreen, Rect.Full, PaintOp.TransparentFg,
        sh.pixmaps[Style.Filled], p);
      IF v.nodeStyle = Style.Border THEN
        VBT.PaintPixmap(v.offscreen, Rect.Full, PaintOp.TransparentBg,
          sh.pixmaps[Style.Border], p);
      END;
    END;
END PaintBox;


PROCEDURE PaintFocusBox(
    v: T;
    READONLY p: Point.T;
    READONLY sh: ShapeSizeRec;
    <* UNUSED *> style: Style) =
  VAR
    tint_border := v.colors^[v.nodeColor+2].tint;
    tint_interior := v.colors^[v.focusColor+2].tint;
  BEGIN
    IF v.nodeShape = Rectangle THEN
      VBT.PaintTint(v.offscreen, Rect.Add(sh.boxes[Style.Filled], p), 
        tint_border);
      IF v.nodeStyle = Style.Border OR v.highlight = TRUE THEN
        VBT.PaintTint(v.offscreen, Rect.Add(sh.boxes[Style.Border], p),
          tint_interior);
      END;
    ELSE
      VBT.PaintPixmap(v.offscreen, Rect.Full, PaintOp.TransparentFg,
           sh.pixmaps[Style.Filled], p);
      IF v.nodeStyle = Style.Border OR v.highlight = TRUE THEN
         VBT.PaintPixmap(v.offscreen, Rect.Full, PaintOp.TransparentBg,
           sh.pixmaps[Style.Border], p);
      END;
    END;
  END PaintFocusBox;


PROCEDURE PaintText(
    v: T;
    READONLY refpt: Point.T;
    font: Font.T;
    tint: PaintOp.T;
    text: Text.T) =
  BEGIN
    VBT.PaintText(v.offscreen, Rect.Full, refpt, font, text, 
      PaintOp.Pair(PaintOp.Transparent, tint));
  END PaintText;


PROCEDURE PaintLink(
    v: T;
    <* UNUSED *> link: Link;
    READONLY p1, p2: Point.T) =
  VAR
    tint: PaintOp.T;
  BEGIN
    tint := v.colors^[v.linkColor + 2].tint; 
    VBT.Line(v.offscreen, Rect.Full, p1, p2, width := v.linkThickness-1, 
      op := tint);
  END PaintLink;


(***** Transformations *****************************************************)

(* Compute transformation:  we compute scalex, scaley and transx, transy to
   convert world coordinates into screen coordinates:
      screenPoint = worldPoint * scaleFactor + trans
   Depending on the value of v.square, we either do this on a per-
   coordinate basis, or we do it in such a way that the world coordinate
   rectangle has its vient-fixed aspect ratio and fits the vbt exactly
   in the tighter dimension.  The center of world coordinate space
   always maps to the center of the vbt. *)

PROCEDURE ComputeTransformation(v: T; READONLY domain: Rect.T) =
VAR
  screenh, screenv: INTEGER;  (* screen rect width and height *)
  screenCenter: Point.T;      (* screen rect center *)
  worldh, worldv: REAL;       (* world rect width and height *)
  rh, rv: REAL;               (* width and height ratios *)
  realcx, realcy: REAL;       (* world rect center *)
  scaleFactor: REAL;          (* world -> screen scaling factor *)
BEGIN
  (* compute screen rectangle with and height *)
  screenh := domain.east - domain.west;
  screenv := domain.south - domain.north;
  screenCenter := Rect.Middle(domain);
    
  (* compute world rectangle width and height *)
  worldh := v.east - v.west;
  worldv := v.south - v.north;

  (* also compute the center of the world rectangle *)
  realcx := (v.east + v.west) / 2.0;
  realcy := (v.north + v.south) / 2.0;
    
  rh := FLOAT(screenh) / worldh;
  rv := FLOAT(screenv) / worldv;

  IF v.square THEN  (* fixed aspect ratio *)
    scaleFactor := MIN(ABS(rh), ABS(rv));
    v.scalex := scaleFactor;  (* might change someday *)
    IF rv > 0.0 THEN 
      v.scaley := scaleFactor;
    ELSE 
      v.scaley := -scaleFactor;
    END;
  ELSE  (* flexible aspect ratio *)
    v.scalex := rh;
    v.scaley := rv;
  END;
 
  IF v.maxNodes = 0 THEN v.favoriteSize := 10.0
  ELSE 
    v.favoriteSize := ABS(MIN(worldh, worldv) /
    (6.0 * FLOAT(Math.sqrt(FLOAT(v.maxNodes, LONGREAL)))));
    END;

  v.arrowSize := v.favoriteSize * 0.65;
  (* - arrow size is experimental, may need tuning *)

  v.transx := screenCenter.h - TRUNC(realcx * v.scalex);
  v.transy := screenCenter.v - TRUNC(realcy * v.scaley);
END ComputeTransformation;


PROCEDURE WtoS(v: T; rp: RealPoint.T): Point.T =
  BEGIN
    RETURN ConvertPoint(v, rp);
  END WtoS;

PROCEDURE StoW(v: T; p: Point.T): RealPoint.T =
  BEGIN
    RETURN ConvertBack(v, p);
  END StoW;

PROCEDURE ConvertPoint(vbt: T; rp: RealPoint.T): Point.T =
  VAR
    p: Point.T;
  BEGIN
    p.h := TRUNC(rp.h * vbt.scalex) + vbt.transx;
    p.v := TRUNC(rp.v * vbt.scaley) + vbt.transy;
    RETURN p
  END ConvertPoint;

PROCEDURE ConvertBack(vbt: T; p: Point.T): RealPoint.T =
  VAR
    rp: RealPoint.T;
  BEGIN
    rp.h := FLOAT(p.h - vbt.transx) / vbt.scalex;
    rp.v := FLOAT(p.v - vbt.transy) / vbt.scaley;
    RETURN rp;
  END ConvertBack;



(*********************************************************************)

(* ChooseSize:  given node's "true" size and the scaling currently in
   effect for this vbt, choose the best size available in node's shape
   and style.  Return its index, an index into the size/bitmap array
   for this shape. *)

PROCEDURE ChooseSize(v: T; node: Node; <* UNUSED *> style: Style): CARDINAL =
  VAR
    nodeSize := node.size;
    pixSize: INTEGER;
  BEGIN
    IF nodeSize = 0.0 THEN nodeSize := v.favoriteSize END;
    pixSize := TRUNC(0.5 + nodeSize * ABS(v.scalex));
    RETURN MAX (0, MIN (pixSize, 63));
  END ChooseSize;


PROCEDURE GetSizeIndex(
    <* UNUSED *> v: T;
    pixSize: INTEGER; 
    <* UNUSED *> style: Style := Style.Border): INTEGER =
  BEGIN
    IF pixSize <= 0 THEN 
      RETURN 0;
    ELSIF pixSize >= 63 THEN
      RETURN 63;
    ELSE
      RETURN pixSize;
    END;
  END GetSizeIndex;



(*********************************** Compute Fisheye Views   **************)

CONST
  InfiniteDist: REAL = 9999.0;

TYPE
  SetOfNodes = ARRAY [0..400] OF BOOLEAN;

PROCEDURE InitializeSingleSource(VAR v: T; s: INTEGER) =
BEGIN
  FOR i := 0 TO v.maxNodes-1 DO
    v.table^[i].dist := InfiniteDist;
  END;
  v.table^[s].dist := 0.0;
END InitializeSingleSource;


PROCEDURE Relax(VAR v: T; u1: INTEGER; u2: INTEGER) =
BEGIN
  IF (v.table^[u2].dist > (v.table^[u1].dist + 1.0)) THEN
    v.table^[u2].dist := v.table^[u1].dist + 1.0;
  END;
END Relax;

PROCEDURE ExtractMin(v: T; VAR Q: SetOfNodes): INTEGER =
VAR
  index: INTEGER;
  min: REAL;
BEGIN
  min := InfiniteDist+1.0;
  index := -1;
  FOR i := 0 TO v.maxNodes-1 DO
    IF ( Q[i] = TRUE) AND (v.table^[i].dist < min) THEN
      min := v.table^[i].dist;
      index := i;
    END;
  END;
  IF (index # -1) THEN
    Q[index] := FALSE;
    RETURN index;
  ELSE
    Wr.PutText(Stdio.stdout, "No node left to extract: error\n");
    Wr.Flush(Stdio.stdout);
    RETURN index;
  END;
END ExtractMin;


PROCEDURE Adjacent (v: T; u1: INTEGER; u2: INTEGER): BOOLEAN =
  VAR linku1, linku2: Link;
  BEGIN
    IF (u1 = u2) THEN RETURN FALSE; END;
    FOR i := 0 TO MaxLink - 1 DO
      linku1 := v.table^[u1].link[i];
      linku2 := v.table^[u2].link[i];
      IF (linku1 # NIL) THEN
        IF RealPoint.Equal(
             linku1.alterEgo.from.pos, v.table^[u2].pos) THEN
          RETURN TRUE;
        END;
      END;
      IF (linku2 # NIL) THEN
        IF RealPoint.Equal(
             linku2.alterEgo.from.pos, v.table^[u1].pos) THEN
          RETURN TRUE;
        END;
      END;
    END;
    RETURN FALSE;
  END Adjacent;


PROCEDURE SetEmpty(v: T; READONLY Q: SetOfNodes): BOOLEAN =
VAR
  res: BOOLEAN;
BEGIN
  res := TRUE;
  FOR i := 0 TO v.maxNodes-1 DO
    IF Q[i] THEN res := FALSE; END;
  END;
  RETURN res;
END SetEmpty;


PROCEDURE ComputeDistanceFromNode(v: T; id: INTEGER) =
VAR
  S, Q: SetOfNodes;
  u1: INTEGER;
BEGIN
  InitializeSingleSource(v, id);
  FOR i := 0 TO v.maxNodes-1 DO
    S[i] := FALSE;
    Q[i] := TRUE;
  END;
  WHILE (SetEmpty(v,Q) = FALSE) DO
    u1 := ExtractMin(v, Q);
    S[u1] := TRUE;
    FOR u2 := 0 TO v.maxNodes-1 DO
      IF (Adjacent(v, u1, u2) = TRUE) THEN
        Relax(v, u1, u2);
      END;
    END;
  END;
END ComputeDistanceFromNode;

PROCEDURE ComputeDistance (v: T) =
  BEGIN
    v.dist :=
      NEW(REF ARRAY OF ARRAY OF REAL, v.maxNodes, v.maxNodes);
    FOR i := 0 TO v.maxNodes - 1 DO
      ComputeDistanceFromNode(v, i);
      FOR j := 0 TO v.maxNodes - 1 DO
        v.dist[i, j] := v.table^[j].dist
      END
    END
  END ComputeDistance;


PROCEDURE DetermineFocus(v: T): INTEGER =
VAR
  closest: INTEGER;
  closestDistSqr: REAL;
  distSqr: REAL;
BEGIN
  closest := 0;
  closestDistSqr := RealPoint.DistSquare(v.fisheyeWorldFocus, 
    v.table^[0].pos);
  FOR i := 1 TO v.maxNodes-1 DO
    distSqr := RealPoint.DistSquare(v.table^[i].pos, v.fisheyeWorldFocus);
    IF  distSqr < closestDistSqr THEN
      closest := i;
      closestDistSqr := distSqr;
    END;
  END;
  RETURN closest;
END DetermineFocus;

PROCEDURE SemanticViewChangeRequired(v: T): BOOLEAN =
VAR
  focus: INTEGER;
BEGIN
  focus := DetermineFocus(v);
  IF focus # v.currentFocusNode THEN
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END SemanticViewChangeRequired;

PROCEDURE ComputeSemanticFisheye (v: T) =
  CONST epsilon = 0.0001;
  VAR
    node        : Node;
    MaxVW, MinVW: REAL;
    radius      : REAL;
  BEGIN
    v.currentFocusNode := DetermineFocus(v);
    MaxVW := -99999.0;
    MinVW := +99999.0;
    FOR i := 0 TO v.maxNodes - 1 DO
      node := v.table^[i];
      IF node # NIL THEN
        node.fpos := node.pos;
        IF (NOT v.trustCaches) OR (node.sizeIndex < 0) THEN
          node.sizeIndex := ChooseSize(v, node, node.style);
        END;
        radius := v.fSizeFactor * FLOAT(
                    shapes[node.shape]^[node.sizeIndex].size)
                    / v.scalex;
        node.fsizeIndex :=
          GetSizeIndex(v, TRUNC(0.5 + radius * v.scalex));
        node.fVW := FLOAT(node.api - v.dist[v.currentFocusNode,i]);
        IF node.fVW > MaxVW THEN MaxVW := node.fVW; END;
        IF node.fVW < MinVW THEN MinVW := node.fVW; END;
      END;
    END;
    FOR i := 0 TO v.maxNodes - 1 DO
      node := v.table^[i];
      IF node # NIL THEN
        node.fVW :=
          ABS((node.fVW - MinVW) / (MaxVW - MinVW + epsilon));
      END;
    END;
  END ComputeSemanticFisheye;



(******************* Graphical Fisheye Views *****************************)

(* R, X and resulting value must all have the same sign *)


PROCEDURE MapCoordinate(v: T; x: REAL; Xmax: REAL): REAL =
  BEGIN
    RETURN
      Xmax * (((v.fDistortion + 1.0) * (x / Xmax)) /
        ((v.fDistortion * (x / Xmax) + 1.0)));
  END MapCoordinate;


PROCEDURE MapPoint(
    v: T;
    point: RealPoint.T;
    Xmax: REAL;
    Ymax: REAL): RealPoint.T =
  BEGIN
    RETURN RealPoint.T{
      Xmax * (((v.fDistortion + 1.0) * (point.h / Xmax)) /
        ((v.fDistortion * (point.h / Xmax) + 1.0))),
      Ymax * (((v.fDistortion + 1.0) * (point.v / Ymax)) /
        ((v.fDistortion * (point.v / Ymax) + 1.0)))};
  END MapPoint;


PROCEDURE UnmapPoint(
     v: T;
     point: RealPoint.T;
     Xmax: REAL;
     Ymax: REAL): RealPoint.T =
  BEGIN
     RETURN RealPoint.T{
       point.h / ( v.fDistortion * (1.0 - point.h / Xmax) + 1.0),
       point.v / ( v.fDistortion * (1.0 - point.v / Ymax) + 1.0)};
  END UnmapPoint;


PROCEDURE MapPolarPoint(
  v: T;
  point: RealPoint.T;
  Xmax: REAL;
  Ymax: REAL): RealPoint.T =
VAR
  rnorm, rfeye, rmax: REAL;
  pfeye: RealPoint.T;
  theta: LONGREAL;
  epsilon: REAL := 0.0000000001;
  hsign, vsign: REAL;
  tan_theta: REAL;
  p1, p2: RealPoint.T;
BEGIN
  tan_theta := (point.v + epsilon) / (point.h + epsilon);
  IF (point.h < 0.0 ) THEN hsign := -1.0; ELSE hsign := 1.0; END;
  IF (point.v < 0.0 ) THEN vsign := -1.0; ELSE vsign := 1.0; END;
  rnorm := FLOAT(Math.sqrt(FLOAT
    (RealPoint.DistSquare(point, RealPoint.Origin), LONGREAL)));
  theta := Math.atan2(FLOAT(point.v + epsilon, LONGREAL), 
    FLOAT(point.h + epsilon, LONGREAL));
  p1 := RealPoint.T{Xmax, tan_theta * Xmax};
  p2 := RealPoint.T{Ymax / tan_theta, Ymax};
  IF (RealPoint.DistSquare(p1,RealPoint.Origin) <
      RealPoint.DistSquare(p2,RealPoint.Origin)) THEN
    rmax := ABS(Xmax / FLOAT(Math.cos(theta)));
  ELSE
    rmax := ABS(Ymax / FLOAT(Math.sin(theta)));
  END;
  rfeye := MapCoordinate(v, rnorm, rmax);
  pfeye.h := rfeye * FLOAT(Math.cos(theta));
  pfeye.v := rfeye * FLOAT(Math.sin(theta));
  RETURN pfeye;    
END MapPolarPoint;



PROCEDURE MapSize(v: T; node: Node; delta: RealPoint.T; fdelta: RealPoint.T; 
  radius: REAL; Xmax: REAL; Ymax: REAL): REAL = 
VAR
  rx, ry: REAL;
  fradius: REAL;
BEGIN
  rx := MapCoordinate(v, ABS(delta.h) + radius, ABS(Xmax)) - ABS(fdelta.h);
  ry := MapCoordinate(v, ABS(delta.v) + radius, ABS(Ymax)) - ABS(fdelta.v);
  fradius := MIN(rx,ry) *
      FLOAT(Math.pow(FLOAT(node.api * v.fisheyeSizeAPICoeff, LONGREAL),
        FLOAT(v.fisheyeSizeAPIPower, LONGREAL)));
  RETURN fradius;
END MapSize;


(**************************************************************************)

PROCEDURE ViewChangeRequired(v: T): BOOLEAN =
VAR
  new_view: BOOLEAN;
  delta, f_delta, focus: RealPoint.T;
  radius, f_radius: REAL;
  xmax, ymax: REAL;
  f_size_index: INTEGER;
  node: Node;
BEGIN
  new_view := FALSE;
  IF v.currentFocusNode = -1 THEN
    new_view := TRUE;
  ELSE (* v.currentFocusNode # -1 i.e. user is focused on some node *)
    node := v.table^[v.currentFocusNode];
    focus := v.fisheyeWorldFocus;
    delta := RealPoint.Sub(node.pos, focus);

    IF delta.h < 0.0 THEN
      xmax := v.west - focus.h; (* negative *)
    ELSE
      xmax := v.east - focus.h; (* positive *)
    END;

    IF delta.v < 0.0 THEN 
      ymax := v.south - focus.v; (* negative *)
    ELSE 
      ymax := v.north - focus.v; (* positive *)
    END;

    IF (v.fisheyeMapType = FisheyeMapType.Polar) THEN
      f_delta := MapPolarPoint(v, delta, xmax, ymax);
    ELSE
      f_delta := MapPoint(v, delta, xmax, ymax);
    END;

    (* size *)
    IF (NOT v.trustCaches) OR (node.sizeIndex < 0) THEN
      node.sizeIndex := ChooseSize(v, node, node.style);
    END;
    radius := v.fSizeFactor * 
     	  FLOAT(shapes[node.shape]^[node.sizeIndex].size) / v.scalex;
    f_radius := MapSize(v, node, delta, f_delta, radius, xmax, ymax);
    f_size_index := GetSizeIndex(v, TRUNC(0.5 + f_radius * v.scalex));

    IF ((ABS(f_delta.h) > ABS(f_radius)) OR 
      (ABS(f_delta.v) > ABS(f_radius))) OR
      (f_size_index < node.fsizeIndex) THEN 
        new_view := TRUE;
        v.currentFocusNode := -1;
    ELSE
        new_view := FALSE;
    END;
  END;
  RETURN new_view;
END ViewChangeRequired;


(************************************************************************)
PROCEDURE ComputeGraphicalFisheye(v: T) =
VAR
    wfocus, delta, fdelta: RealPoint.T;
    node: Node;
    link: Link;
    rWest, rEast, rSouth, rNorth: REAL;
    RX, RY, radius, fRadius: REAL;
    MaxRadius, MinRadius: REAL;
BEGIN
    wfocus := v.fisheyeWorldFocus;
    v.fisheyeOldFocus := wfocus;
    rWest := v.west - wfocus.h; (* negative *)
    rEast := v.east - wfocus.h; (* positive *)
    rSouth := v.south - wfocus.v; (* negative *)
    rNorth := v.north - wfocus.v; (* positive *)

    MaxRadius := 0.0;
    MinRadius := Huge;

    FOR i := 0 TO v.maxNodes-1 DO
      node := v.table^[i];
      IF node # NIL THEN
        delta := RealPoint.Sub(node.pos, wfocus);
        IF delta.h < 0.0 THEN RX := rWest; ELSE RX := rEast; END;
        IF delta.v < 0.0 THEN RY := rSouth; ELSE RY := rNorth; END;
  
        (* position *)
        IF (v.fisheyeMapType = FisheyeMapType.Polar) THEN
          fdelta := MapPolarPoint(v, delta, RX, RY);
        ELSE
          fdelta := MapPoint(v, delta, RX, RY);
        END;
        node.fpos := RealPoint.Add(wfocus, fdelta);

        (* size *)
        IF (NOT v.trustCaches) OR (node.sizeIndex < 0) THEN
          node.sizeIndex := ChooseSize(v, node, node.style);
        END;

        radius := v.fSizeFactor *  
     	  FLOAT(shapes[node.shape]^[node.sizeIndex].size)/v.scalex;
        fRadius := MapSize(v, node, delta, fdelta, radius, RX, RY);
        node.fsizeIndex := GetSizeIndex(v, TRUNC(0.5 + fRadius * v.scalex));

(*
          PrintUtil.PrintReal(" d=", v.fDistortion);
     	  PrintUtil.PrintInt(" vtx ", i);
     	  PrintUtil.PrintRealPair(" Pn=", delta.h, delta.v);
     	  PrintUtil.PrintRealPair(" Pf=", fdelta.h, fdelta.v);
          PrintUtil.PrintReal(" Rn=", radius);
          PrintUtil.PrintReal(" Rf=", fRadius);
          Wr.PutText(Stdio.stdout,"\n");
     	  Wr.Flush(Stdio.stdout);
*)
        (* focus and focus radius *)
        IF (ABS(fdelta.h) <= fRadius) AND (ABS(fdelta.v) <= fRadius) THEN
          v.currentFocusNode := i;
          fRadius := MapSize(v, node, RealPoint.Origin, RealPoint.Origin,
            radius, RX, RY);
          node.fsizeIndex := 
               GetSizeIndex(v, TRUNC(0.5 + fRadius * v.scalex));
        END;

        (* calculating min and max size for visualworth *)
        node.radius := fRadius;
        IF fRadius > MaxRadius THEN MaxRadius := fRadius; END;
        IF fRadius < MinRadius THEN MinRadius := fRadius; END;

        (* bend points *)
        FOR j := 0 TO MaxLink DO
          link := node.link[j];
          IF (link # NIL) AND (link.primary) AND (link.segPoints # NIL) THEN
            FOR k := 0 TO NUMBER(link.segPoints^)-1 DO
              delta := RealPoint.Sub(link.segPoints^[k], wfocus);
              IF delta.h < 0.0 THEN RX := rWest; ELSE RX := rEast; END;
              IF delta.v < 0.0 THEN RY := rSouth; ELSE RY := rNorth; END;
              IF (v.fisheyeMapType = FisheyeMapType.Polar) THEN
                fdelta := MapPolarPoint(v, delta, RX, RY);
              ELSE
                fdelta := MapPoint(v, delta, RX, RY);
              END;
              link.fsegPoints^[k] := RealPoint.Add(wfocus, fdelta);
            END;
          END;
        END;
      END;
    END;
  
    FOR i := 0 TO v.maxNodes-1 DO
      node := v.table^[i];
      node.fVW := ABS((node.radius - MinRadius)/(MaxRadius - MinRadius));
    END;
END ComputeGraphicalFisheye;


(*************************************************************************)

<* UNUSED *>
PROCEDURE SortNodes(v: T) =
  VAR
    node: Node;
  BEGIN
    SortedHashTable.Clear(v.hTable);
    SortedIndexTable.Clear(v.sTable);
    FOR i := 0 TO v.maxNodes-1 DO
      node := v.table^[i];
      IF NOT SortedHashTable.Insert(v.hTable, node.fVW, i) THEN
         Wr.PutText(Stdio.stdout, "Hashtable insertion error");
         Wr.Flush(Stdio.stdout);
      END;
    END;

    IF NOT SortedIndexTable.CopySortedHashTable(v.hTable, v.sTable,
      v.maxNodes) THEN
      Wr.PutText(Stdio.stdout, "Hashtable copying error");
      Wr.Flush(Stdio.stdout);
    END;
  END SortNodes;


(************************************************************************)
PROCEDURE NewScreenFocus(v: T) =
  BEGIN
    IF v.fisheyeType = FisheyeType.Graphical THEN
      IF ViewChangeRequired(v) THEN
       ComputeGraphicalFisheye(v);
       (* SortNodes(v); *)
       RedrawFisheye(v, Rect.Full, FALSE);
       (* Video.VIDEOabekasRecordAndAdvance(); *)
      END;
    ELSE
      IF SemanticViewChangeRequired(v) THEN
        UpdateView(v);
      END;
    END;
  END NewScreenFocus;


(**********************************************************************)
PROCEDURE UpdateView(v: T) =
  BEGIN
    IF v.fisheyeType = FisheyeType.Graphical THEN
      ComputeGraphicalFisheye(v);
      (* SortNodes(v); *)
    ELSE
      ComputeSemanticFisheye(v);
    END;
    RedrawFisheye(v, Rect.Full);
  END UpdateView;


(********************************************************************)
PROCEDURE SetImage(v: T) =
  VAR 
    domain := VBT.Domain(v);
    trestle := Trestle.ScreenOf(v, Point.Origin).trsl;
    screen_type := VBT.ScreenTypeOf(v);
  BEGIN
    IF v.offscreen # NIL THEN
      Trestle.Delete(v.offscreen);
    ELSE
      v.offscreen := NEW(VBT.Leaf);
    END;
    IF trestle # NIL AND screen_type # NIL THEN
      Trestle.Attach(v.offscreen, trestle);
      Trestle.InstallOffscreen(v.offscreen, domain.east - domain.west, 
        domain.south - domain.north, screen_type);
    END;
  END SetImage;


(******************************************** New VBT *****************)

PROCEDURE New(): T =
  VAR 
     v := NEW(T);
  BEGIN

    v.offscreen := NIL;
    SetImage(v);
    v.maxNodes := 0;
    v.table := NIL;
    v.nodeCount := 0;
    v.edgeCount := 0;

    (* no color setup here, leave colors = NIL *)

    (* set up world coordinates, Cartesian, 0 in center *)
    v.north := 100.0;    v.south := -100.0;
    v.west := -100.0;    v.east := 100.0;
    v.marginSize := 0.0;
    v.background := PaintOp.Bg;
    v.foreground := PaintOp.Fg;
    v.font := Font.BuiltIn;
    v.borderMin := 10;

    v.nodeColor := Blue;
    v.nodeInteriorColor := Sky;
    
    v.nodeShape := Rectangle;
    v.nodeStyle := Style.Border;
    v.nodeSize := 0.0;    (* meaning, pick based on world coordinates *)

    v.nodeLabelStyle := LabelStyle.Center;
    v.nodeLabelColor := Black;
    v.nodeLabelFont := Font.BuiltIn;
    v.nodeLabelShift := 0;
            
    v.linkColor := Maroon;
    v.linkThickness := 1;

    (* derivative properties *)
    v.viewVisible := TRUE;
    v.trustCaches := FALSE;

    SetColors(v);

    v.fisheyeMapType := FisheyeMapType.Cartesian;
    v.fisheyeType := FisheyeType.Graphical;
    v.semanticColor := FALSE;
    v.highlight := TRUE;
    v.fisheye := FALSE;
    v.cageSize := 0;
    v.fisheyeScreenFocus := Point.Origin;
    v.fisheyeOldFocus := RealPoint.T{10.0, 10.0};
    v.fisheyeWorldFocus := RealPoint.T{10.0, 10.0};
    v.focusColor := Red;
    v.currentFocusNode := -1; (* means not focused on any node *)
    v.fDistortion := 2.0;
    v.fSizeFactor := 2.0;
    v.fFocusDevice := FocusDevice.Mouse;
    v.fisheyeText := TRUE;

    v.fisheyeVWThreshold := 0.0;
    v.fisheyeSizeAPICoeff := 0.001;
    v.fisheyeSizeAPIPower := 0.001;

    v.hTable := NIL; (* SortingTable.NewHashTable(100); *)
    (* make sure that the keys fall within [0.0,1.0) *)
    v.sTable := NIL; (* SortingTable.NewSortedTable(100); *)

    RETURN v;
  END New;


BEGIN
  SetUpShapes();
END GraphVBT.

