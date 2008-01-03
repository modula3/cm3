(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)

(* Last modified on Fri Jul  9 13:33:25 PDT 1993 by steveg *)
(*      modified on Mon Aug  3 17:09:00 PDT 1992 by jdd *)

INTERFACE GraphVBT;

(* A GraphVBT.T is a VBT class for constructing displays of graphs of
   vertices, edges, and polygons.  Alternatively, you can use it to display
   certain other data structures; for example, you can just use GraphVBT as
   a way to draw and move vertices on a VBT.

   There are operations to build a graph, and operations to update it
   (e.g., by moving vertices to new positions, automatically bringing their
   edges with them).  Many such updates can be animated, with objects
   slowly sliding toward their new positions, etc. *)

(* OBJECT CLASSES

   T.  A VBT class for displaying graph elements.

   Vertex.  A colored rectangle or ellipse with a textual label, with its
   center at specified coordinates.

   Edge.  A straight or curved colored line between a pair of vertices,
   with optional arrowheads.

   VertexHighlight.  A colored border on a vertex.

   Polygon.  A filled colored region specified by a path of vertices. *)

(* COORDINATES

   Each GraphVBT.T has a specified world coordinate system: [w..e]*[n..s],
   where (w, n) is the window's NW corner.  It is required that e # w, and
   that s # n.  It is allowed for e to be less than w, or s less than n, in
   which case the coordinate system changes handedness.

   All positions and sizes are given in world coordinates.

   The preferred size is the size of the graph vbt when it is first created.  
   Any changes by the user to the size of the window take precedence.
   The world coordinate system is automatically scaled to fit the current
   size of the window and all objects and fonts are scaled appropriately.

   The display rectangle is centered in the window, bordered by at least
   the fixed margin.  Optionally, its size in pixels can be constrained to
   be a multiple of specified values horizontally and vertically; this can
   improve esthetics when the client deals with the corresponding fractions
   of world coordinates.  Also optionally, the display rectangle can be
   constrained to (approximately) a specified aspect ratio.

   If an element's coordinates lie outside the specified coordinate system,
   they are still mapped linearly onto the VBT, but they may be partially
   or completely invisible. *)

(* APPEARANCE

   The graph itself has a white (Bg) background.

   Each vertex is painted as a colored solid shape--a rectangle or an
   ellipse--with a colored textual label in a specified font, with its
   center at a specified coordinate position.  The client specifies the
   size and colors of the vertex.  The vertex has an optional inset border
   of a specified size and color.

   The actual point size of the font used to display the label is the 
   closest available font size to the scaled world size.

   Edges are painted as colored lines connecting the centers of their
   vertices.  There is an optional arrowhead at each end; the size of the
   arrowhead is scaled with the width of the line.  Edges may be either
   straight lines or cubic Bezier curves.  If they are Beziers, the client
   specifies two additional vertices that act as control points.  If
   desired, these vertices can be zero-size and therefore invisible.

   Vertex highlights are painted as colored shapes that appear centered
   behind specified vertices, giving the appearance of a colored borders.
   The vertex highlight has the same shape as its vertex, and the same size
   plus a margin.  When a vertex highlight moves during an animation, the
   whole highlight appears when it is temporarily not behind any vertex.

   Polygons are painted as colored polygonal filled regions whose sides are
   straight and whose corners are the centers of specified vertices.  The
   handedness of the outline is ignored; it's always the interior of the
   polygon that is filled.

   When vertices are moved, their edges, polygons, and highlights move with
   them.

   On the display, vertexes appear in front of edges, edges in front of
   vertex highlights, and vertex highlights in front of polygons.  There is
   no other specified order of objects.  However, a vertex may be moved to
   the front or to the back of the other vertices, leaving the relative
   order of the others the same, and similarly for the other elements.  No
   other operations change the relative orders of objects on the display
   (except init and remove, of course). *)

(* ANIMATION

   Operations on objects in the graph may be either IMMEDIATE or ANIMATED,
   depending on the "animated" parameter.  (Some operations are always
   immediate due to implementation restrictions; others because animation
   would not make sense.)

   IMMEDIATE operations take place when invoked.  However, they may or may
   not appear on the screen immediately.  They can be forced to appear by
   calling the graph's "redisplay" method.

   The results of ANIMATED operations remain pending until the graph's
   "animate" method is invoked.  For example, if a vertex is moved in
   animated mode, the new position does not take effect immediately, but
   remains pending until the graph is animated.  At that time, all pending
   animated operations are performed over a specified time interval; a
   vertex will move in a smooth path from its current position to its final
   position, as will its highlights, polygons, and edges (including any
   Bezier curves for which it's a control point).

   Multiple moves without intervening animations overwrite each other.

   Animated operations normally maintain a pending STATE.  If a vertex is
   moved to one position and then another in the same animation, it slides
   directly to the final position during the animation.

   During an animation, elements normally takes straight paths from their
   initial positions to their new positions.  Optionally, an arbitrary path
   can be specified for a vertex; an optional AnimationPath object contains
   a method that maps a time during the animation (scaled to be between 0.0
   and 1.0) to the desired vertex position in world coordinates.  When a
   vertex follows such a path, so do any edge ends, highlights, corners of
   polygons, etc., that remain attached during the animation.  Otherwise,
   edge ends, vertex highlights, and corners of polygons follow straight
   paths to their final positions.

   There may be multiple animations per Zeus event.  Each animation starts
   at Zeus time t0 and ends at Zeus time t1.  (Zeus times are given in
   seconds but are scaled by Zeus.) For an event's first animation, t0
   should be 0; for subsequent animations for the same event, t0 should be
   the previous animation's t1.

   If t0 = t1, the animation appears to happen immediately. 

   Animations may be aborted by a Thread.Alert on the thread that called
   graph.animate.  The animation checks for an alert at the end of each frame in
   the animation.
*)

(* LOCKING LEVELS

   Almost all methods in this interface have <* LL.sup = graph.mu *>.

   The "init" methods have <* LL.sup < graph.mu *>.

   The "animate" method on graphs also has <* LL.sup < graph.mu *>.

   The "redisplay" method on graphs has <* LL.sup <= VBT.mu *>. *)

(* DESIGN DEFICIENCIES.

   No direct support for graphs.  For example, there is no operations to go
   from an adjacency matrix to a representation on the screen.

   No labels on edges.

   It is unclear how easily these method classes can be overridden by
   clients. *)

(* IMPLEMENTATION RESTRICTIONS.

   The interface should not advertise that GraphVBT.T is a subtype of MG.V.
   This is a temporary workaround.

   Not all interesting operations can be animated.

   When edges move, their arrowheads may seem to float off and back on.

   Vertices' inset borders are drawn in fontColor, nor borderColor.

   The verticesAt/edgesAt/vertexHighlightsAt/polygonsAt operations have an 
   internal model distinct from the actual renderer's, and sometimes return 
   the wrong answers near boundaries.

   GraphVBT must be used under Zeus. *)

(* POSSIBLE ADDITIONS.

   Set coordinate system and margins.

   Edge highlights.

   Highlights "on top".

   Group highlight.

   Callbacks for input events. *)

IMPORT RefList, PaintOp, R2, Rect, Thread, VBT;

<* PRAGMA LL *>

TYPE
  T <: TPublic;
  TPrivate <: VBT.T;
  TPublic =
    TPrivate OBJECT
      mu: MUTEX;

      (* READONLY after initialization: MAY be initialized by client *)
      <* LL >= {SELF.mu} *>
      world  := WorldRectangle{w := 0.0, e := 1.0, n := 0.0, s := 1.0};
      margin := 0.0;             (* margin size *)
      pixelSizeDivisor := ARRAY [0 .. 1] OF CARDINAL{1, 1};
      (* the display rectangle will be a multiple of pixelSizeDivisor[0]
         pixels wide, and a multiple of pixelSizeDivisor[1] pixels tall *)
      aspect := 0.0;
      (* if non-zero, display rectangle keeps (approximately) this ratio of
         height / width, subject to pixelSizeDivisor. *)

      preferredSize := R2.T{100.0, 100.0};
      (* The initial size (in mm) of the window when first installed.
         Scaling is adjusted so that the nominal size of the display
         remains the preferredSize at all times. *)

      (* READONLY *)
      vertices: RefList.T (* OF Vertex *) := NIL; (* unordered *)
      edges   : RefList.T (* OF Edge *)   := NIL; (* unordered *)
      vertexHighlights: RefList.T (* OF VertexHighlight *) := NIL; (* unordered *)
      polygons: RefList.T (* OF Polygon *) := NIL; (* unordered *)
    METHODS

      (* MUST be called by client. *)
      <* LL.sup < SELF.mu *>
      init (): T;                (* no-op if already initialized *)

      (* "setWorld" updates self's world rectangle. *)
      <* LL.sup = SELF.mu *>
      setWorld (world: WorldRectangle (* the new world rectangle *));

      (* "setMargin" updates self's margin. *)
      <* LL.sup = SELF.mu *>
      setMargin (margin: REAL (* the new margin *));

      (* "setPixelSizeDivisor" updates the self's pixelSizeDivisors. *)
      <* LL.sup = SELF.mu *>
      setPixelSizeDivisor (pixelSizeDivisor: ARRAY [0 .. 1] OF CARDINAL (* the divisors *));

      (* "setAspect" updates the self's aspect. *)
      <* LL.sup = SELF.mu *>
      setAspect (aspect: REAL (* the new aspect *));

      (* "setPreferredSize" updates the self's preferredSize. *)
      <* LL.sup = SELF.mu *>
      setPreferredSize (preferredSize: R2.T (* the new prferred size *));

      (* "redisplay" should be called after performing immediate operations
         to make the display consistent. *)
      <* LL.sup <= VBT.mu *>
      redisplay ();

      (* "animate" animates all pending animated changes to a graph, over a
         specified time interval.  If t0 = t1, the changes appear
         instantaneously. *)
      <* LL.sup < SELF.mu *>
      animate (t0, t1: REAL) RAISES {Thread.Alerted};

      (* "clear" removes all graphic objects from the graph (calling their
         "remove" methods). *)
      <* LL.sup = SELF.mu *>
      clear ();

      (* "verticesAt" returns a list, in front-to-back order, of all the
         vertices that lie partly or totally inside a specified screen
         rectangle. *)
      <* LL.sup = SELF.mu *>
      verticesAt (pixelRect: Rect.T): RefList.T (* OF Vertex *);

      (* "edgesAt" returns a list, in front-to-back order, of all the edges
         that lie partly or totally inside a specified screen rectangle. *)
      <* LL.sup = SELF.mu *>
      edgesAt (pixelRect: Rect.T): RefList.T (* OF Edge *);

      (* "vertexHighlightsAt" returns a list, in front-to-back order, of
         all the vertex highlights that lie partly or totally inside a
         specified screen rectangle. *)
      <* LL.sup = SELF.mu *>
      vertexHighlightsAt (pixelRect: Rect.T):
                          RefList.T (* OF VertexHighlight *);

      (* "polygonsAt" returns a list, in front-to-back order, of all the
         polygons that lie partly or totally inside a specified screen
         rectangle. *)
      <* LL.sup = SELF.mu *>
      polygonsAt (pixelRect: Rect.T): RefList.T (* OF Polygon *);

      <* LL.sup arbitrary *>
      font (family : TEXT  := "Helvetica";
            size   : REAL  := 0.0353;
            slant  : Slant := Slant.Roman;
            weight : TEXT  := "Normal";
            foundry: TEXT  := "*"          ): WorldFont;
      (* "font" returns a font with the given characteristics where "size"
         is in world coordinates *)
    END;

TYPE
  Slant = {Roman, Italic, Oblique, ReverseItalic, 
    ReverseOblique, Other, Any};

TYPE
  Vertex <: VertexPublic;
  VertexPublic =
    OBJECT

      (* READONLY after initialization: MUST be initialized by client *)
      (* CONST *)
      graph: T;                 (* the GraphVBT.T containing this vertex *)

      (* READONLY after initialization: MUST be initialized by client *)
      <* LL >= {SELF.graph.mu} *>
      pos: R2.T;                (* the vertex's position *)
      shape: VertexShape;       (* the vertex's shape.  use "setShape" to
                                   update. *)

      (* READONLY after initialization: one of two MUST be initialized by
         client *)
      <* LL >= {SELF.graph.mu} *>
      size := R2.T{0.0, 0.0};  (* the vertex's h and v siz.  
                                  use "setSize" to update. *)

      (* READONLY after initialization: MAY be initialized by client *)
      <* LL >= {SELF.graph.mu} *>
      color: PaintOp.T := PaintOp.Fg; (* the tint for the vertex (the
                                         default is black).  use "setColor"
                                         to update. *)
      label: TEXT := "";        (* the vertex's label.  use "setLabel" to
                                   update. *)
      font: WorldFont := NIL; (* the font for the label.  use
                                 "setFont" to update. IF "font" = NIL then
                                 DefaultFont will be used. *)
      fontColor: PaintOp.T := PaintOp.Bg; (* the tint for the vertex's
                                             label (the default is white).
                                             use "setFontColor" to
                                             update. *)
      border := 0.0;          (* the vertex's inset border size.  
                                 use "setBorder" to  update. *)
      borderColor: PaintOp.T := PaintOp.Fg; (* the tint for the vertex's
                                               inset border (the default is
                                               black).  use
                                               "setBorderColor" to
                                               update. *)
      (* IMPLEMENTATION RESTRICTION: The value of borderColor is ignored
         and the inset margin is drawn in "fontColor". *)

      (* READONLY *)
      edges: RefList.T (* OF Edge *) := NIL;
      (* the edges that mention this vertex.  unordered.  edges that
         mention this vertex more than once (e.g., edges from this vertex
         to itself) may or may not appear more than once. *)
      vertexHighlights: RefList.T (* OF VertexHighlight *) := NIL;
      (* the highlights on this vertex.  unordered. *)
      polygons: RefList.T (* OF Polygon *) := NIL;
      (* the polygons with corners at this vertex.  unordered.  polygons
         with multiple corners at this vertex may or may not appear more
         than once. *)

    METHODS

      (* MUST be called by client. *)
      <* LL.sup < SELF.graph.mu *>
      init (): Vertex;          (* no-op if already initialized *)

      (* "move" moves the vertex to a new position given by h and v; when a
         vertex moves, so do its edges, highlights, and polygons. *)
      <* LL.sup = SELF.graph.mu *>
      move (pos: R2.T (* the new position *);
            animated: BOOLEAN := FALSE; (* if TRUE, the move is animated *)
            start := 0.0; stop := 1.0; (* if animated, the timing of the animation *)
            path: AnimationPath := NIL); (* if non-NIL, specfies the path
                                            for the animation *)

      (* "setSize" updates the vertex's size. *)
      <* LL.sup = SELF.graph.mu *>
      setSize (size: R2.T (* the new size*));

      (* "setShape" updates the vertex's shape. *)
      <* LL.sup = SELF.graph.mu *>
      setShape (shape: VertexShape);

      (* "setColor" updates the vertex's color. *)
      <* LL.sup = SELF.graph.mu *>
      setColor (color: PaintOp.T);

      (* "setLabel" updates the vertex's label. *)
      <* LL.sup = SELF.graph.mu *>
      setLabel (label: TEXT);

      (* "setFont" updates the label's Font.  If "font" = NIL then DefaultFont
         will be used. *)
      <* LL.sup = SELF.graph.mu *>
      setFont (font: WorldFont);

      (* "setFontColor" updates the label's color. *)
      <* LL.sup = SELF.graph.mu *>
      setFontColor (fontColor: PaintOp.T);

      (* "setBorder" updates the vertex's inset border size. *)
      <* LL.sup = SELF.graph.mu *>
      setBorder (border: REAL (* the new inset border size *));

      (* "setBorderColor" updates the inset border's color. *)
      <* LL.sup = SELF.graph.mu *>
      setBorderColor (borderColor: PaintOp.T);

      (* "toFront" moves the vertex's appearance to the front of the other
         vertices, the foreground or the background objects in its graph. *)
      <* LL.sup = SELF.graph.mu *>
      toFront (zorder := ZOrder.Normal);

      (* "toBack" moves the vertex to the back of the other vertices,
          the foreground or the background objects. *)
      <* LL.sup = SELF.graph.mu *>
      toBack (zorder := ZOrder.Normal);

      (* "remove" removes a vertex from its graph, along with all its
         edges, highlights, and polygons.  they may be reinstalled with
         init. *)
      <* LL.sup = SELF.graph.mu *>
      remove ();

    END;

TYPE
  ZOrder = {Foreground, Normal, Background};
  (* All object are overlaid in the following order.  Foreground objects are
     on top of all other objects.  They are followed by Normal vertices,
     then Normal vertex highlights, Normal edges and Normal polygons.
     Finally all Background objects are under all other objects.

     The toFront and toBack methods are used to change the overlay order
     of an object.
  *)

TYPE
  WorldFont <: REFANY;

VAR (* CONST *)
  DefaultFont: WorldFont;
  (* "DefaultFont" will appear as a 10pt helvetica font in the default 
      world configuration:
      self.world = {0, 1, 0, 1} and preferred size = {100, 100}.

      It is equivalent to a font whose size = 0.0353 in world coordinates.
  *)

TYPE
  Edge <: EdgePublic;
  EdgePublic =
    OBJECT

      (* READONLY after initialization: MUST be initialized by client *)
      <* LL >= {SELF.vertex0.graph.mu} *>
      vertex0, vertex1: Vertex;  (* the vertices that the edge connects.
                                    must belong to same graph.  use "move"
                                    to update. *)

      (* READONLY after initialization: MAY be initialized by client *)
      <* LL >= {SELF.vertex0.graph.mu} *>
      width := 0.007;         (* the edge's width. use "setWidth" to update. *)
      color: PaintOp.T := PaintOp.Fg; (* the edge's color (the default is
                                         black).  use "setColor" to
                                         update. *)
      arrow := ARRAY [0 .. 1] OF BOOLEAN{FALSE, FALSE};
      (* whether there is an arrowhead at vertex0 or at vertex1.  use
         "setArrow" to update. *)
      control0, control1: Vertex := NIL;
      (* if both NIL, the edge is straight.  otherwise, the edge is a
         Bezier, and these vertices (in the same graph) are the control
         points.  use "move" to update. *)

    METHODS

      (* MUST be called by client. *)
      <* LL.sup < SELF.vertex0.graph.mu *>
      init (): Edge;            (* no-op if already initialized *)

      (* "move" connects the edge to new vertices in the same graph. *)
      <* LL.sup = SELF.vertex0.graph.mu *>
      move (vertex0, vertex1  : Vertex;
            control0, control1: Vertex   := NIL;
            animated          : BOOLEAN  := FALSE;
            start := 0.0; stop := 1.0); (* if animated, the timing of the animation *)

      (* "setWidth" updates the edge's width. *)
      <* LL.sup = SELF.vertex0.graph.mu *>
      setWidth (width: REAL);

      (* "setColor" updates the edge's color. *)
      <* LL.sup = SELF.vertex0.graph.mu *>
      setColor (color: PaintOp.T);

      (* "setArrow" updates whether there are arrowheads at the ends of the
         edge. *)
      <* LL.sup = SELF.vertex0.graph.mu *>
      setArrow (arrow: ARRAY [0 .. 1] OF BOOLEAN);

      (* "toFront" moves the edge's appearance to the front of the other
         edges, the foreground or the background objects in its graph. *)
      <* LL.sup = SELF.graph.mu *>
      toFront (zOrder := ZOrder.Normal);

      (* "toBack" moves the edge to the back of the other edges, the 
         foreground or the background objects. *)
      <* LL.sup = SELF.graph.mu *>
      toBack (zOrder := ZOrder.Normal);

      (* "remove" removes a edge from its graph.  it may be reinstalled
         with init. *)
      <* LL.sup = SELF.vertex0.graph.mu *>
      remove ();

    END;

TYPE
  VertexHighlight <: VertexHighlightPublic;
  VertexHighlightPublic =
    OBJECT

      (* READONLY after initialization: MUST be initialized by client *)
      <* LL >= {SELF.vertex.graph.mu} *>
      vertex: Vertex;           (* the vertex to vertexHighlight *)
      color: PaintOp.T;         (* the tint for the vertexHighlight; note
                                   that a white (Bg) vertexHighlight is
                                   invisible against the background.  use
                                   "setColor" to update. *)

      (* READONLY after initialization: one of two MUST be initialized by
         client *)
      <* LL >= {SELF.vertex.graph.mu} *>
      border := R2.T{0.0, 0.0}; (* border width horizontally and vertically.
                                   use "setBorder" to update. *)

    METHODS

      (* MUST be called by client. *)
      <* LL.sup < SELF.vertex.graph.mu *>
      init (): VertexHighlight; (* no-op if already initialized *)

      (* "move" moves the vertexHighlight to a new vertex in the same
         graph. *)
      <* LL.sup = SELF.vertex.graph.mu *>
      move (vertex: Vertex; animated: BOOLEAN := FALSE;
            start := 0.0; stop := 1.0); (* if animated, the timing of the animation *)

      (* "setBorder" updates the vertexHighlight border width. *)
      <* LL.sup = SELF.vertex.graph.mu *>
      setBorder (border: R2.T);

      (* "setColor" updates the vertexHighlight's PaintOp.T. *)
      <* LL.sup = SELF.vertex.graph.mu *>
      setColor (color: PaintOp.T);

      (* "toFront" moves the vertex highlight's appearance to the front of
         the other vertex highlights, the foreground or the background objects
         in its graph. *)
      <* LL.sup = SELF.graph.mu *>
      toFront (zOrder := ZOrder.Normal);

      (* "toBack" moves the vertex highlight to the back of the other
         vertex highlights, the foreground or the background objects. *)
      <* LL.sup = SELF.graph.mu *>
      toBack (zOrder := ZOrder.Normal);

      (* "remove" removes a vertexHighlight from its graph.  it may be
         reinstalled with init. *)
      <* LL.sup = SELF.vertex.graph.mu *>
      remove ();

    END;

TYPE
  Polygon <: PolygonPublic;
  PolygonPublic =
    OBJECT
      (* READONLY after initialization: MUST be initialized by client *)
      <* LL >= {SELF.vertices.head.graph.mu} *>
      vertices: RefList.T (* OF Vertex or List of 3 Vertices *);  
        (* specifies the edges defining the polygon.  A vertex 
           indicates a straight edge from the end point of the previous edge.
           A list of 3 vertices indicates a bezier edge.  The first 2
           vertices are the control vertices of the edge and the third vertex
           is the end point of the vertex.  All vertices must belong to the same
           graph.  use "move" to  update. *)

      (* READONLY after initialization: MAY be initialized by client *)
      <* LL >= {RefList.First(SELF.vertices).graph.mu} *>
      color: PaintOp.T := PaintOp.Fg; (* the polygon's color (the default
                                         is black).  use "setColor" to
                                         update. *)

    METHODS

      (* MUST be called by client. *)
      <* LL.sup < RefList.First(SELF.vertices).graph.mu *>
      init (): Polygon;         (* no-op if already initialized *)

      (* "move" connects the polygon to new vertices in the same graph. *)
      <* LL.sup = RefList.First(SELF.vertices).graph.mu *>
      move (vertices: RefList.T (* OF Vertex or List of 3 vertices *); 
            animated: BOOLEAN := FALSE;
            start := 0.0; stop := 1.0); (* if animated, the timing of the animation *)

      (* "setColor" updates the polygon's color. *)
      <* LL.sup = RefList.First(SELF.vertices).graph.mu *>
      setColor (color: PaintOp.T);

      (* "toFront" moves the polygon's appearance to the front of the other
         polygons, the foreground or the background objects in its graph. *)
      <* LL.sup = SELF.graph.mu *>
      toFront (zOrder := ZOrder.Normal);

      (* "toBack" moves the polygon to the back of the other polygons, the 
         foreground or the background objects. *)
      <* LL.sup = SELF.graph.mu *>
      toBack (zOrder := ZOrder.Normal);

      (* "remove" removes a polygon from its graph.  it may be reinstalled
         with init. *)
      <* LL.sup = RefList.First(SELF.vertices).graph.mu *>
      remove ();

    END;

TYPE
  (* A WorldRectangle is contained in a GraphVBT.T.  It defines the bounds
     of the world coordinate system for the GraphVBT.T. *)
  WorldRectangle = RECORD
                     w: REAL := 0.0;
                     e: REAL := 1.0;
                     n: REAL := 0.0;
                     s: REAL := 1.0;
                   END;

TYPE
  (* A VertexShape designates the shape of a vertex.  There are only a few
     possible shapes. *)
  VertexShape = {Rectangle, Ellipse};

TYPE
  (* An AnimationPath can specify the path a vertex takes when it is
     animated.  Its "pos" method maps a time between 0.0 and 1.0 onto a
     position in world coordinates. *)
  AnimationPath = OBJECT 
  METHODS
    <* LL.sup = {vertex.graph.mu, vertex.graph.mgv.mu *>
    pos (t: REAL): R2.T;
   END;

END GraphVBT.
