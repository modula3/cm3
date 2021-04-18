(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Aug 21 15:58:11 PDT 1995 by najork                   *)
(*       Created on Tue Jul 11 22:37:39 PDT 1995 by najork                   *)

(* Limitations:

     drawPolygon, drawQuadMesh, drawColoredQuadMesh methods:
         surface edges have slight artifacts

     drawMarker method:
         marker types are ignored; markers are drawn as dots

     setDistinguishFacetsFlag method:
         not implemented

     pushMatrix, popMatrix methods:
         I use the native OpenGL matrix stack, which allows only for a fixed
         number of matrices to be pushed. This number is guaranteed to be at
         least 32.

     setDepthCueing method:
         The arguments "frontScale" and "backScale" are ignored, since OpenGL
         does not have the concept of a fog scaling factor.

   Bugs:

     SRC 129 program 18 (page 35) does not work properly!

*)


UNSAFE MODULE X_OpenGL_Base EXPORTS X_OpenGL_Base, X_OpenGL_BaseProxy;

IMPORT AuxG, AnimServer, Color, ColorPropPrivate, Ctypes, FileRd, FloatMode,
       GL, GLu, GLX, GO, GOPrivate, GraphicsBase, GraphicsBasePrivate,
       IntIntTbl, KeyCB, KeyboardKey, Lex, LineTypeProp, M3toC, MarkerGO,
       MarkerTypeProp, MarkerTypePropPrivate, Math, Matrix4, MouseCB, Mth,
       OSError, ParseParams, Point, Point3, PositionCB, Process, PropPrivate,
       RasterModeProp, Rd, RealPropPrivate, RootGOPrivate, ShadingProp, Stdio,
       SurfaceGO, Text, Thread, VBT, Word, X, Xatom;

IMPORT IO;

<* FATAL X.Error *>

REVEAL
  T = Public BRANDED OBJECT
    window              : X.Window;
    context             : GLX.GLXContext;
    drawBuffer          : GL.GLenum;
    winWidth            : INTEGER;
    winHeight           : INTEGER;

    near                : REAL;         (* Used by glOrtho, gluPerspective, *)
    far                 : REAL;         (* and for fog calculation.         *)

    phase               : INTEGER;           (* current drawing phase       *)
    transflag           : BOOLEAN;           (* transparent parts in scene? *)
    modifiers           : VBT.Modifiers;     (* what modifiers are pressed  *)
    buttonDownCount     : INTEGER;           (* how many buttons are down   *)
    awaitDeleteMu       : Thread.Mutex;      (* Mutex used by Thread.Wait   *)
    awaitDeleteCV       : Thread.Condition;  (* CV for awaitDelete method   *)
    stateSize           : INTEGER;
    dlTable             : IntIntTbl.T;

    from                : Point3.T;
    to                  : Point3.T;
    up                  : Point3.T;
    projType            : ProjType;
    aspect              : REAL;
    fovy                : REAL;
    height              : REAL;
  (*** light management ***)
    lighting         : BOOLEAN := TRUE;
    lightCount       : INTEGER;
    lightList        : GL.GLuint;
    ambientLight     : GLrgba;
  (*** markers ***)
    markerColor      : Color.T;                    (* Initialized by "Init" *)
    markerScale      : REAL;                       (* Initialized by "Init" *)
    markerType       : MarkerTypeProp.Kind;        (* Initialized by "Init" *)
  (*** lines ***)
    lineType         : GL.GLint := Solid;
    lineWidth        : REAL     := 1.0;
    lineColor        : Color.T  := Color.White;
  (*** surfaces ***)
    frontColor       : Color.T := Color.White;
    backColor        : Color.T := Color.White;
    transmission     : REAL    := 1.0;

    ambientReflCoeff : REAL    := 0.5;
    diffuseReflCoeff : REAL    := 1.0;
    specularReflCoeff: REAL    := 0.0;
    specularReflColor: Color.T := Color.White;
    specularReflConc : REAL;                       (* Initialized by "Init" *)

    rasterMode                 := RasterModeProp.Kind.Solid;
  (*** surface edges ***)
    edgeFlag         : BOOLEAN  := FALSE;
    edgeType         : GL.GLint := Solid;
    edgeWidth        : REAL     := 1.0;
    edgeColor        : Color.T  := Color.White;
  (*** caching of OpenGL display lists for prototypical objects ***)
    sphereStructures   : StructureList := NIL;
    coneStructures     : StructureList := NIL;
    cylinderStructures : StructureList := NIL;
    diskStructures     : StructureList := NIL;
  OVERRIDES
  (*** Methods that may be called by any thread ***)
    init               := Init;
    changeTitle        := ChangeTitle; (* should be called only by server *)
    awaitDelete        := AwaitDelete;
    destroy            := Destroy;
  (*** Methods that may be called only by animation server thread ***)
    processEvents      := ProcessEvents;
    repair             := Repair;
    unmap              := Unmap;

    push             := Push;
    pop              := Pop;

    addAmbientLight  := AddAmbientLight;
    addVectorLight   := AddVectorLight;
    addPointLight    := AddPointLight;
    addSpotLight     := AddSpotLight;

    openDisplayList  := OpenDisplayList;
    closeDisplayList := CloseDisplayList;
    callDisplayList  := CallDisplayList;
    freeDisplayList  := FreeDisplayList;

    pushMatrix       := PushMatrix;
    popMatrix        := PopMatrix;

    setLookAt                := SetLookAt;
    setOrthoProj             := SetOrthoProj;
    setPerspProj             := SetPerspProj;
    setupCamera              := SetupCamera;
    screenToWorld            := ScreenToWorld;

    setBackgroundColor       := SetBackgroundColor;
    setDepthcueing           := SetDepthcueing;
    setMarkerColor           := SetMarkerColor;
    setMarkerScale           := SetMarkerScale;
    setMarkerType            := SetMarkerType;
    setLineColor             := SetLineColor;
    setLineWidth             := SetLineWidth;
    setLineType              := SetLineType;
    setSurfaceColor          := SetSurfaceColor;
    setSurfaceBackColor      := SetSurfaceBackColor;
    setRasterMode            := SetRasterMode;
    setDistinguishFacetsFlag := SetDistinguishFacetsFlag;
    setLighting              := SetLighting;
    setShading               := SetShading;
    setSurfaceEdgeFlag       := SetSurfaceEdgeFlag;
    setSurfaceEdgeColor      := SetSurfaceEdgeColor;
    setSurfaceEdgeType       := SetSurfaceEdgeType;
    setSurfaceEdgeWidth      := SetSurfaceEdgeWidth;
    setAmbientReflCoeff      := SetAmbientReflCoeff;
    setDiffuseReflCoeff      := SetDiffuseReflCoeff;
    setSpecularReflCoeff     := SetSpecularReflCoeff;
    setSpecularReflConc      := SetSpecularReflConc;
    setSpecularReflColor     := SetSpecularReflColor;
    setTransmissionCoeff     := SetTransmissionCoeff;
    drawMarker               := DrawMarker;
    drawLine                 := DrawLine;
    drawPolygon              := DrawPolygon;
    drawQuadMesh             := DrawQuadMesh;
    drawColoredQuadMesh      := DrawColoredQuadMesh;
    drawProtoSphere          := DrawProtoSphere;
    drawProtoCone            := DrawProtoCone;
    drawProtoCylinder        := DrawProtoCylinder;
    drawProtoDisk            := DrawProtoDisk;
    drawProtoTorus           := DrawProtoTorus;
  END;

TYPE
  ProjType = {Persp, Ortho};

  GLrgba = RECORD
    r, g, b, a: REAL;
  END;

  GLpoint3d = ARRAY [1 .. 3] OF GL.GLdouble;
(*
  GLpoint3d = RECORD
    x, y, z: LONGREAL;
  END;
*)

  GLpoint4f = RECORD
    x, y, z, w: REAL;
  END;

  GLmatrixf = ARRAY [0 .. 15] OF GL.GLfloat;


CONST
  Solid   = 2_1111111111111111;
  Dashed  = 2_1111000011110000;
  Dotted  = 2_1010101010101010;
  DashDot = 2_1110010011100100;


PROCEDURE Init (self: T; title: TEXT; x, y, w, h: INTEGER): T
    RAISES {GraphicsBase.Failure} =
  CONST
    bw = 1;                                          (* window border width *)
  VAR
    visual   : X.XVisualInfoStar;
    cmap     : X.Colormap;
    wattrs   : X.XSetWindowAttributes;               (* window attributes   *)
    wmask    : Ctypes.unsigned_long;                 (* window attr. mask   *)
    attrList := ARRAY [1 .. 5] OF Ctypes.int {
                             GLX.GLX_RGBA,           (* RGB vs index color  *)
                             GLX.GLX_DEPTH_SIZE, 16, (* depth buf > 16 bits *)
                             GLX.GLX_DOUBLEBUFFER,   (* double buffer       *)
                             X.None};                (* ... that's it!      *)
    cstr     : Ctypes.char_star;
  BEGIN
    IF conn = NIL THEN
      conn := NEW (Connection).init ();
    END;
    IF NOT conn.avail THEN
      RAISE GraphicsBase.Failure;
    END;

    (*** Initialize awaitDeleteCV ***)
    self.awaitDeleteMu := NEW (Thread.Mutex);
    self.awaitDeleteCV := NEW (Thread.Condition);

    (*** Initialize the display list table ***)
    self.dlTable := NEW (IntIntTbl.Default).init ();

    TRY

      WITH dpy = conn.dpy, window = self.window DO

        (*** Ensure single-threaded access to the display connection ***)
        LOCK conn DO

          (* Find the best visual.  Unlike PEX, OpenGL has a routine
             that does it for us. *)
          visual := GLX.glXChooseVisual (dpy,
                                         X.XDefaultScreen (dpy),
                                         ADR (attrList));
          IF visual = NIL THEN
            RAISE GraphicsBase.Failure;
          END;

          (* MK tests if the visual is a TrueColor visual, and bails out if
             it's not.  That seems to be a bit harsh ... *)

          (* Create a GLX rendering context (the equivalent to a PEX renderer).
             "NIL" indicates that we don't want to share display lists with
             other contexts (this might be unnecessarily conservative);
             "True" indicates that we want to access the graphics system
             directly (as opposed to through the X server) if possible. *)
          self.context := GLX.glXCreateContext (dpy, visual, NIL, X.True);
          IF self.context = NIL THEN
            RAISE GraphicsBase.Failure;
          END;

          (* Create a color map *)
          cmap := X.XCreateColormap (dpy, X.XRootWindow (dpy, visual.screen),
                                     visual.visual, X.AllocNone);

          (* Create a window. The next 20 or thirty lines are identical to the
             code in X_PEX_Base.  It should be factored out into an auxiliary
             module that is shared by both bases. *)
          wmask := 0;

          wattrs.colormap := cmap;
          wmask := Word.Or (wmask, X.CWColormap);

          wattrs.background_pixel := X.XBlackPixel(dpy, X.XDefaultScreen(dpy));
          wmask := Word.Or (wmask, X.CWBackPixel);

          wattrs.border_pixel := X.XWhitePixel (dpy, X.XDefaultScreen (dpy));
          wmask := Word.Or (wmask, X.CWBorderPixel);

          window := X.XCreateWindow(
                             dpy, X.XRootWindow (dpy, visual.screen),
                             x, y, w, h,
                             bw,
                             visual.depth,
                             X.InputOutput,
                             visual.visual,
                             wmask, ADR(wattrs));
          self.winWidth  := w;
          self.winHeight := h;

          X.XSelectInput(dpy, window,
                         Word.Or(X.ExposureMask,
                          Word.Or(X.StructureNotifyMask,
                           Word.Or(X.KeyPressMask,
                            Word.Or(X.KeyReleaseMask,
                             Word.Or(X.ButtonPressMask,
                              Word.Or(X.ButtonReleaseMask,
                                      X.PointerMotionMask)))))));

          (*** set the window's title ***)
          cstr := M3toC.SharedTtoS (title);
          X.XChangeProperty (dpy, window, Xatom.XA_WM_NAME, Xatom.XA_STRING, 8,
                             X.PropModeReplace,
                             LOOPHOLE (cstr,
                                       Ctypes.unsigned_char_star),
                             Text.Length (title));
          M3toC.FreeSharedS(title, cstr);
          (* ask the WM to send ClientMessage events when f.kill is chosen *)
          EVAL X.XSetWMProtocols (dpy, window, ADR (conn.wm_delete_window), 1);

          (*** map the window ***)
          X.XMapWindow (dpy, window);

          (**********************************************)
          (* End of code that's identical to X_PEX_Base *)
          (**********************************************)

          (* Bind "self.context" to "self.window" *)
          WITH status = GLX.glXMakeCurrent (dpy, window, self.context) DO
            IF status = X.False THEN
              RAISE GraphicsBase.Failure;
            END;
          END;

          (*** Determine the default frame buffer ***)
          GL.glGetIntegerv (GL.GL_DRAW_BUFFER, ADR (self.drawBuffer));

          (*** Enable depth buffering and set the depth buffer clear value ***)
          GL.glEnable (GL.GL_DEPTH_TEST);
          GL.glDepthFunc (GL.GL_GREATER);
          GL.glClearDepth (0.0d0);

          (*** Create the display list for light sources ***)
          GL.glEnable (GL.GL_LIGHTING);
          GL.glLightModeli(GL.GL_LIGHT_MODEL_TWO_SIDE, GL.GL_TRUE);
          self.lightList := GL.glGenLists (1);
          IF self.lightList = 0 THEN
            RAISE GraphicsBase.Failure;
          END;

          (* Select flat shading and auto-normalization of normal vectors *)
          GL.glShadeModel (GL.GL_FLAT);
          GL.glEnable (GL.GL_NORMALIZE);

          GL.glEnable (GL.GL_LINE_STIPPLE);
          GL.glLineStipple (1, Solid);

        END;
      END;

      self.stacks := PropPrivate.NewStacks ();
      self.stateSize := NUMBER (self.stacks^);

      (* The rest of this function is copied straight from X_PEX_Base. *)

      self.modifiers := VBT.Modifiers {};
      self.buttonDownCount := 0;

      self.status := GraphicsBasePrivate.Status.Mapped;

      (* Initialize the sate variables *)
      self.setSpecularReflConc (
          SurfaceGO.SpecularReflectionConc.getState (self));

      self.setMarkerColor (MarkerGO.Colour.getState (self));
      self.setMarkerScale (MarkerGO.Scale.getState (self));
      self.setMarkerType  (MarkerGO.Type.getState (self));

      WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO
        IF pp.keywordPresent("-largeCursor") THEN
          LargeCursor(self);
        END;
      END;

      IF MkProxyT # NIL THEN
        MkProxyT (self);
      END;

      RETURN self;
    EXCEPT
      X.Error => RAISE GraphicsBase.Failure;
    END;
  END Init;


(*****************************************************************************)
(* The following procedures are copied pretty much directly from X_PEX_Base  *)
(*****************************************************************************)


PROCEDURE LargeCursor (self : T) =
  VAR
    pm := X.XCreatePixmap (conn.dpy, self.window, 64, 64, 1);
    fg, bg : X.XColor;
    hot : X.XPoint;
    pts : REF ARRAY OF X.XPoint;
    bg_gcv, fg_gcv : X.XGCValues;
    bg_gc, fg_gc : X.GC;
  BEGIN
    bg_gcv.function := X.GXclear;
    bg_gc := X.XCreateGC(conn.dpy, pm, X.GCFunction, ADR(bg_gcv));
    fg_gcv.function := X.GXset;
    fg_gc := X.XCreateGC(conn.dpy, pm, X.GCFunction, ADR(fg_gcv));

    TRY
      WITH rd = FileRd.Open("cursordata"),
           n  = Lex.Int(rd) DO
        pts := NEW (REF ARRAY OF X.XPoint, n);
        FOR i := FIRST(pts^) TO LAST(pts^) DO
          pts[i].x := Lex.Int(rd); pts[i].y := Lex.Int(rd);
        END;
        hot.x := Lex.Int(rd); hot.y := Lex.Int(rd);
        fg.red := Lex.Int(rd); fg.green := Lex.Int(rd); fg.blue := Lex.Int(rd);
        bg.red := Lex.Int(rd); bg.green := Lex.Int(rd); bg.blue := Lex.Int(rd);
      END;
    EXCEPT
    | OSError.E, FloatMode.Trap, Lex.Error, Rd.Failure, Thread.Alerted =>
      pts := NEW (REF ARRAY OF X.XPoint, 7);
      pts^ := ARRAY OF X.XPoint{X.XPoint{0,0},
                                X.XPoint{45,15},
                                X.XPoint{35,25},
                                X.XPoint{63,53},
                                X.XPoint{53,63},
                                X.XPoint{25,35},
                                X.XPoint{15,45}};
      hot.x := 0; hot.y := 0;
      fg.red := 65535; fg.green := 0; fg.blue := 0; (* red *)
      bg.red := 0;     bg.green := 0; bg.blue := 0; (* black *)
    END;

    X.XFillRectangle (conn.dpy, pm, bg_gc, 0, 0, 64, 64);
    X.XFillPolygon (conn.dpy, pm, fg_gc,
                    ADR(pts[0]), NUMBER(pts^),
                    X.Nonconvex,  X.CoordModeOrigin);
    WITH cursor = X.XCreatePixmapCursor(conn.dpy, pm, pm,
                                        ADR(fg), ADR(bg),
                                        hot.x, hot.y) DO
      X.XDefineCursor (conn.dpy, self.window, cursor);
    END;
  END LargeCursor;


PROCEDURE ChangeTitle (self: T; title : TEXT) =
  VAR cstr : Ctypes.char_star;
  BEGIN
    cstr := M3toC.SharedTtoS (title);
    LOCK conn DO
      X.XChangeProperty (conn.dpy,
                         self.window,
                         Xatom.XA_WM_NAME,
                         Xatom.XA_STRING,
                         8,
                         X.PropModeReplace,
                         LOOPHOLE (cstr,
                                   Ctypes.unsigned_char_star),
                         Text.Length (title));
    END;
    M3toC.FreeSharedS(title, cstr);
  END ChangeTitle;


PROCEDURE AwaitDelete (self : T) =
  BEGIN
    LOCK self.awaitDeleteMu DO
      Thread.Wait (self.awaitDeleteMu, self.awaitDeleteCV);
    END;
  END AwaitDelete;


PROCEDURE Destroy (self : T) =
  BEGIN
    LOCK AnimServer.internalLock DO
      self.status := GraphicsBasePrivate.Status.Destroyed;
    END;
  END Destroy;


PROCEDURE Unmap (self : T) =
  BEGIN
    (*** Destroy the window ***)
    X.XDestroyWindow (conn.dpy, self.window);
    X.XSync (conn.dpy, X.False);
    self.window := X.None;
    self.status := GraphicsBasePrivate.Status.Unmapped;

    (*** signal all threads that are blocked ***)
    Thread.Broadcast (self.awaitDeleteCV);
  END Unmap;


PROCEDURE Available () : BOOLEAN =
  BEGIN
    IF conn = NIL THEN
      conn := NEW (Connection).init ();
    END;
    RETURN conn.avail;
  END Available;


(*****************************************************************************)
(* End of replicated code                                                    *)
(*****************************************************************************)


PROCEDURE Push (self : T; caller : GO.T) =
  VAR
    props := caller.props;
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    WHILE props # NIL DO
      WITH prop = props.head DO
        prop.n.push (self, prop.v);
      END;
      props := props.tail;
    END;
  END Push;


PROCEDURE Pop (self : T; caller : GO.T) =
  VAR
    props := caller.props;
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    WHILE props # NIL DO
      props.head.n.pop (self);
      props := props.tail;
    END;
  END Pop;


(*****************************************************************************)
(* Phase 1 methods: Camera and light source management                       *)
(*****************************************************************************)


PROCEDURE AddAmbientLight (self: T; color: Color.T) =
  BEGIN
    IF self.phase = 1 THEN
      self.ambientLight := GLrgba {self.ambientLight.r + color.r,
                                   self.ambientLight.g + color.g,
                                   self.ambientLight.b + color.b,
                                   self.ambientLight.a};
    END;
  END AddAmbientLight;


PROCEDURE AddVectorLight (self: T; color: Color.T; d: Point3.T) =
  VAR
    pos := GLpoint4f {-d.x, -d.y, -d.z, 0.0};
    black := GLrgba {0.0, 0.0, 0.0, 1.0};
    col := GLrgba {color.r, color.g, color.b, 1.0};
  BEGIN
    IF self.phase = 1 THEN
      WITH l = GL.GL_LIGHT0 + self.lightCount DO
        <* ASSERT l < GL.GL_LIGHT0 + GL.GL_MAX_LIGHTS *>
        GL.glLightfv (l, GL.GL_AMBIENT, ADR (black));
        GL.glLightfv (l, GL.GL_DIFFUSE, ADR (col));
        GL.glLightfv (l, GL.GL_SPECULAR, ADR (col));
        GL.glLightfv (l, GL.GL_POSITION, ADR (pos));

        (* Since this is a directional light source, attenuation is disabled,
           so we don't need to specify "GL_CONSTANT_ATTENUATION",
           "GL_LINEAR_ATTENUATION", and "GL_QUADRATIC_ATTENUATION".  On the
           other hand, we have to specify "GL_SPOT_CUTOFF" and
           "GL_SPOT_EXPONENT", since OpenGL allows for directional spotlights
           (with their effect being undefined). We initialize them for uniform
           light distribution. Since "GL_SPOT_CUTOFF" is 180 degrees, we don't
           need to specify "GL_SPOT_DIRECTION". *)
        GL.glLightf (l, GL.GL_SPOT_EXPONENT, 0.0);
        GL.glLightf (l, GL.GL_SPOT_CUTOFF, 180.0);

        GL.glEnable (l);
        INC (self.lightCount);
      END;
    END;
  END AddVectorLight;


PROCEDURE AddPointLight (self      : T;
                         color     : Color.T;
                         p         : Point3.T;
                         att0, att1: REAL) =
  VAR
    pos := GLpoint4f {p.x, p.y, p.z, 1.0};
    black := GLrgba {0.0, 0.0, 0.0, 1.0};
    col := GLrgba {color.r, color.g, color.b, 1.0};
  BEGIN
    IF self.phase = 1 THEN
      WITH l = GL.GL_LIGHT0 + self.lightCount DO
        <* ASSERT l < GL.GL_LIGHT0 + GL.GL_MAX_LIGHTS *>

        GL.glLightfv (l, GL.GL_AMBIENT, ADR (black));
        GL.glLightfv (l, GL.GL_DIFFUSE, ADR (col));
        GL.glLightfv (l, GL.GL_SPECULAR, ADR (col));
        GL.glLightfv (l, GL.GL_POSITION, ADR (pos));

        GL.glLightf (l, GL.GL_SPOT_EXPONENT, 0.0);
        GL.glLightf (l, GL.GL_SPOT_CUTOFF, 180.0);

        GL.glLightf (l, GL.GL_CONSTANT_ATTENUATION,  att0);
        GL.glLightf (l, GL.GL_LINEAR_ATTENUATION,    att1);
        GL.glLightf (l, GL.GL_QUADRATIC_ATTENUATION, 0.0);

        GL.glEnable (l);
        INC (self.lightCount);
      END;
    END;
  END AddPointLight;


PROCEDURE AddSpotLight (self: T; color: Color.T; p, d: Point3.T;
                        conc, spread, att0, att1: REAL) =
  VAR
    pos := GLpoint4f {p.x, p.y, p.z, 1.0};
    black := GLrgba {0.0, 0.0, 0.0, 1.0};
    col := GLrgba {color.r, color.g, color.b, 1.0};
  BEGIN
    IF self.phase = 1 THEN
      WITH l = GL.GL_LIGHT0 + self.lightCount DO
        <* ASSERT l < GL.GL_LIGHT0 + GL.GL_MAX_LIGHTS *>

        GL.glLightfv (l, GL.GL_AMBIENT, ADR (black));
        GL.glLightfv (l, GL.GL_DIFFUSE, ADR (col));
        GL.glLightfv (l, GL.GL_SPECULAR, ADR (col));
        GL.glLightfv (l, GL.GL_POSITION, ADR (pos));

        GL.glLightfv (l, GL.GL_SPOT_DIRECTION, ADR (d));
        GL.glLightf (l, GL.GL_SPOT_EXPONENT, conc);
        GL.glLightf (l, GL.GL_SPOT_CUTOFF, spread / FLOAT (Math.Degree, REAL));

        GL.glLightf (l, GL.GL_CONSTANT_ATTENUATION,  att0);
        GL.glLightf (l, GL.GL_LINEAR_ATTENUATION,    att1);
        GL.glLightf (l, GL.GL_QUADRATIC_ATTENUATION, 0.0);

        GL.glEnable (l);
        INC (self.lightCount);
      END;
    END;
  END AddSpotLight;


PROCEDURE SetLookAt (self: T; from, to, up: Point3.T) =
  BEGIN
    IF self.phase = 1 THEN
      self.from := from;
      self.to   := to;
      self.up   := up;
    END;
  END SetLookAt;


PROCEDURE SetPerspProj (self: T; fovy, aspect: REAL) =
  BEGIN
    IF self.phase = 1 THEN
      self.projType := ProjType.Persp;
      self.fovy     := fovy;
      self.aspect   := aspect;
    END;
  END SetPerspProj;


PROCEDURE SetOrthoProj (self: T; height, aspect: REAL) =
  BEGIN
    IF self.phase = 1 THEN
      self.projType := ProjType.Ortho;
      self.height   := height;
      self.aspect   := aspect;
    END;
  END SetOrthoProj;


(*****************************************************************************)
(* Display-List management                                                   *)
(*****************************************************************************)


PROCEDURE OpenDisplayList (self : T; go : GO.T) =
  VAR
    dl : INTEGER;
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    IF self.phase = 2 THEN
      (*** Extract the display list associated with the GO. ***)
      IF go.dl = 0 THEN
        go.dl := AnimServer.NewDisplayList (go);
      END;
      IF NOT self.dlTable.get (go.dl, dl) THEN
        dl := GL.glGenLists (1);
        <* ASSERT dl # 0 *>
        EVAL self.dlTable.put (go.dl, dl);
      END;

      (*** Open the OpenGL display list ***)
      GL.glNewList (dl, GL.GL_COMPILE);
    END;
  END OpenDisplayList;


PROCEDURE CloseDisplayList (self : T) =
  BEGIN
    IF self.phase = 2 THEN
      GL.glEndList ();
    END;
  END CloseDisplayList;


PROCEDURE CallDisplayList (self : T; go : GO.T) =
  VAR
    dl: INTEGER;
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    IF self.phase = 2 THEN
      (*** Extract the display list associated with the GO. ***)
      IF NOT self.dlTable.get (go.dl, dl) THEN
        <* ASSERT FALSE *>
      END;

      GL.glCallList (dl);
    END;
  END CallDisplayList;


PROCEDURE FreeDisplayList (self: T; go: GO.T) =
  VAR
    dl : INTEGER;
  BEGIN
    IF self.dlTable.delete (go.dl, dl) THEN
      GL.glDeleteLists (dl, 1);
    END;
  END FreeDisplayList;



(*****************************************************************************)
(* Matrix Stack management                                                   *)
(*****************************************************************************)


PROCEDURE PushMatrix (<*UNUSED*> self : T; READONLY matrix : Matrix4.T) =
  VAR
    V := FromMatrix4 (matrix);
  BEGIN
    GL.glPushMatrix ();
    GL.glMultMatrixf (ADR (V[0]));
  END PushMatrix;


PROCEDURE PopMatrix (<*UNUSED*> self : T) =
  BEGIN
    GL.glPopMatrix ();
  END PopMatrix;


(*****************************************************************************)
(* Changing the state of the abstract graphics machine                       *)
(*****************************************************************************)


PROCEDURE FromMatrix4 (READONLY M: Matrix4.T): GLmatrixf =
  BEGIN
    RETURN GLmatrixf {M[0][0], M[1][0], M[2][0], M[3][0],
                      M[0][1], M[1][1], M[2][1], M[3][1],
                      M[0][2], M[1][2], M[2][2], M[3][2],
                      M[0][3], M[1][3], M[2][3], M[3][3]};
  END FromMatrix4;


PROCEDURE ToMatrix4 (READONLY M: GLmatrixf): Matrix4.T =
  BEGIN
    RETURN Matrix4.T {Matrix4.Row {M[0], M[4], M[ 8], M[12]},
                      Matrix4.Row {M[1], M[5], M[ 9], M[13]},
                      Matrix4.Row {M[2], M[6], M[10], M[14]},
                      Matrix4.Row {M[3], M[7], M[11], M[15]}};
  END ToMatrix4;


PROCEDURE SetupCamera (self: T) =
  CONST
    epsilon = 0.1;
    min_far = 0.01;
  VAR
    V   : GLmatrixf;
  BEGIN
    GL.glMatrixMode (GL.GL_MODELVIEW);
    GL.glLoadIdentity ();
    GLu.gluLookAt (FLOAT (self.from.x, LONGREAL),
                   FLOAT (self.from.y, LONGREAL),
                   FLOAT (self.from.z, LONGREAL),
                   FLOAT (self.to.x,   LONGREAL),
                   FLOAT (self.to.y,   LONGREAL),
                   FLOAT (self.to.z,   LONGREAL),
                   FLOAT (self.up.x,   LONGREAL),
                   FLOAT (self.up.y,   LONGREAL),
                   FLOAT (self.up.z,   LONGREAL));

    GL.glGetFloatv (GL.GL_MODELVIEW_MATRIX, ADR (V[0]));

    WITH bs = self.getBoundingVolume(),
         M = ToMatrix4 (V),
         center = Point3.T {
                      M[0][0] * bs.center.x + M[0][1] * bs.center.y +
                      M[0][2] * bs.center.z + M[0][3],
                      M[1][0] * bs.center.x + M[1][1] * bs.center.y +
                      M[1][2] * bs.center.z + M[1][3],
                      M[2][0] * bs.center.x + M[2][1] * bs.center.y +
                      M[2][2] * bs.center.z + M[2][3]},
         radius = bs.radius * Mth.sqrt (M[0][0] * M[0][0] +
                                        M[1][0] * M[1][0] +
                                        M[2][0] * M[2][0]) DO
      self.far  := MAX (ABS (center.z) - radius - epsilon, min_far);
      self.near := MAX (ABS (center.z) + radius + epsilon, min_far);
    END;

    GL.glMatrixMode (GL.GL_PROJECTION);
    GL.glLoadIdentity ();

    WITH aspect = self.aspect * FLOAT(self.winWidth) / FLOAT(self.winHeight) DO
      CASE self.projType OF
      | ProjType.Persp =>
        GLu.gluPerspective (FLOAT (self.fovy, LONGREAL) / Math.Degree,
                            FLOAT (aspect, LONGREAL),
                            FLOAT (self.near, LONGREAL),
                            FLOAT (self.far, LONGREAL));
      | ProjType.Ortho =>
        GL.glOrtho (FLOAT (-self.height * aspect * 0.5, LONGREAL),
                    FLOAT ( self.height * aspect * 0.5, LONGREAL),
                    FLOAT (-self.height          * 0.5, LONGREAL),
                    FLOAT ( self.height          * 0.5, LONGREAL),
                    FLOAT (self.near, LONGREAL),
                    FLOAT (self.far, LONGREAL));
      END;
    END;

    (*** Switch back to model/view matrix ***)
    GL.glMatrixMode (GL.GL_MODELVIEW);
  END SetupCamera;


PROCEDURE ScreenToWorld (self: T; pos: Point.T; zpos: REAL): Point3.T =
  VAR
    modelMatrix: ARRAY [0 .. 15] OF GL.GLdouble;
    projMatrix : ARRAY [0 .. 15] OF GL.GLdouble;
    viewPort   : ARRAY [0 .. 3]  OF GL.GLint;
    rx, ry, rz : GL.GLdouble;
    status     : GL.GLint;
  BEGIN
    (*** Retrieve the modelview and the projection matrix ***)
    GL.glGetDoublev (GL.GL_MODELVIEW_MATRIX, ADR (modelMatrix[0]));
    GL.glGetDoublev (GL.GL_PROJECTION_MATRIX, ADR (projMatrix[0]));
    GL.glGetIntegerv(GL.GL_VIEWPORT, ADR (viewPort[0]));

    (*** Call "UnProject" ***)
    WITH x = FLOAT (pos.h, LONGREAL),
         y = FLOAT (self.winHeight - 1 - pos.v, LONGREAL),
         z = FLOAT (zpos, LONGREAL) DO
      status := GLu.gluUnProject (x, y, z,
                                  ADR (modelMatrix[0]),
                                  ADR (projMatrix[0]),
                                  ADR (viewPort[0]),
                                  ADR (rx), ADR (ry), ADR (rz));
    END;

    <* ASSERT status = GL.GL_TRUE *>

    (*** Return the result ***)
    RETURN Point3.T {FLOAT (rx), FLOAT (ry), FLOAT (rz)};
  END ScreenToWorld;


PROCEDURE SetBackgroundColor (<* UNUSED *> self : T; color : Color.T) =
  BEGIN
    GL.glClearColor (color.r, color.g, color.b, 1.0);
  END SetBackgroundColor;


PROCEDURE SetDepthcueing (           self       : T;
                                     switch     : BOOLEAN;
                                     frontPlane : REAL;
                                     backPlane  : REAL;
                          <*UNUSED*> frontScale : REAL;
                          <*UNUSED*> backScale  : REAL;
                                     color      : Color.T) =
  VAR
    rgba := GLrgba {color.r, color.g, color.b, 1.0};
  BEGIN
    IF self.phase = 2 THEN
      IF switch THEN
        WITH start = self.far - frontPlane * (self.far - self.near),
             end   = self.far - backPlane  * (self.far - self.near) DO
          GL.glEnable (GL.GL_FOG);
          GL.glFogi (GL.GL_FOG_MODE, GL.GL_LINEAR);
          GL.glFogf (GL.GL_FOG_START, start);
          GL.glFogf (GL.GL_FOG_END, end);
          GL.glFogfv (GL.GL_FOG_COLOR, ADR (rgba));
          (* OpenGL does not have the concept of fog scaling factors. Hence,
             we have to ignore "frontScale" and "backScale". Conversely,
             we don't need to specify values for "GL.GL_FOG_INDEX", as we are
             in RGBA mode, and for "GL.GL_FOG_DENSITY", since we use the
             linear fog equation. *)
        END;
      ELSE
        GL.glDisable (GL.GL_FOG);
      END;
    END;
  END SetDepthcueing;


PROCEDURE SetMarkerColor (self: T; col: Color.T) =
  BEGIN
    self.markerColor := col;
  END SetMarkerColor;


PROCEDURE SetMarkerScale (self : T; scale : REAL) =
  BEGIN
    self.markerScale := scale;
  END SetMarkerScale;


PROCEDURE SetMarkerType (self : T; type : MarkerTypeProp.Kind) =
  BEGIN
    self.markerType := type;
  END SetMarkerType;


PROCEDURE SetLineColor (self: T; col: Color.T) =
  BEGIN
    self.lineColor := col;
  END SetLineColor;


PROCEDURE SetLineWidth (self: T; width: REAL) =
  BEGIN
    self.lineWidth := width;
  END SetLineWidth;


PROCEDURE SetLineType (self : T; type : LineTypeProp.Kind) =
  BEGIN
    CASE type OF
    | LineTypeProp.Kind.Solid   => self.lineType := Solid;
    | LineTypeProp.Kind.Dashed  => self.lineType := Dashed;
    | LineTypeProp.Kind.Dotted  => self.lineType := Dotted;
    | LineTypeProp.Kind.DashDot => self.lineType := DashDot;
    END;
  END SetLineType;


PROCEDURE SetSurfaceColor (self : T; col : Color.T) =
  BEGIN
    self.frontColor := col;
  END SetSurfaceColor;


PROCEDURE SetSurfaceBackColor (self : T; col : Color.T) =
  BEGIN
    self.backColor := col;
  END SetSurfaceBackColor;


PROCEDURE SetRasterMode (self : T; val : RasterModeProp.Kind) =
  BEGIN
    self.rasterMode := val;
  END SetRasterMode;


PROCEDURE SetDistinguishFacetsFlag (<*UNUSED*> self : T;
                                    <*UNUSED*> val : BOOLEAN) =
  BEGIN
    IO.Put ("### SetDistinguishFacetsFlag not implemented \n");
  END SetDistinguishFacetsFlag;


PROCEDURE SetLighting (self : T; val : BOOLEAN) =
  BEGIN
    self.lighting := val;
    IF val THEN
      GL.glEnable (GL.GL_LIGHTING);
    ELSE
      GL.glDisable (GL.GL_LIGHTING);
    END;
  END SetLighting;


PROCEDURE SetShading (<*UNUSED*> self : T; val : ShadingProp.Kind) =
  BEGIN
    CASE val OF
    | ShadingProp.Kind.Flat    => GL.glShadeModel (GL.GL_FLAT);
    | ShadingProp.Kind.Gouraud => GL.glShadeModel (GL.GL_SMOOTH);
    END;
  END SetShading;


PROCEDURE SetSurfaceEdgeFlag (self : T; val : BOOLEAN) =
  BEGIN
    self.edgeFlag := val;
  END SetSurfaceEdgeFlag;


PROCEDURE SetSurfaceEdgeColor (self: T; col: Color.T) =
  BEGIN
    self.edgeColor := col;
  END SetSurfaceEdgeColor;


PROCEDURE SetSurfaceEdgeType (self : T; val : LineTypeProp.Kind) =
  BEGIN
    CASE val OF
    | LineTypeProp.Kind.Solid   => self.edgeType := Solid;
    | LineTypeProp.Kind.Dashed  => self.edgeType := Dashed;
    | LineTypeProp.Kind.Dotted  => self.edgeType := Dotted;
    | LineTypeProp.Kind.DashDot => self.edgeType := DashDot;
    END;
  END SetSurfaceEdgeType;


PROCEDURE SetSurfaceEdgeWidth (self: T; width: REAL) =
  BEGIN
    self.edgeWidth := width;
  END SetSurfaceEdgeWidth;


PROCEDURE SetAmbientReflCoeff (self : T; val : REAL) =
  BEGIN
    self.ambientReflCoeff := val;
  END SetAmbientReflCoeff;


PROCEDURE SetDiffuseReflCoeff (self : T; val : REAL) =
  BEGIN
    self.diffuseReflCoeff := val;
  END SetDiffuseReflCoeff;


PROCEDURE SetSpecularReflCoeff (self : T; val : REAL) =
  BEGIN
    self.specularReflCoeff := val;
  END SetSpecularReflCoeff;


PROCEDURE SetSpecularReflConc (self : T; val : REAL) =
  BEGIN
    (* I try to make the "GL_SHININESS" value to look as much as possible like
       the "specularConc" component for "PEXSetReflectionAttributes".
       This formula is taken essentially out of thin air, but seems to produce
       reasonably similar images. *)
    self.specularReflConc := MIN (MAX (val * 2.0 + 4.0, 0.0), 128.0);
  END SetSpecularReflConc;


PROCEDURE SetSpecularReflColor (self : T; val : Color.T) =
  BEGIN
    self.specularReflColor := val;
  END SetSpecularReflColor;


PROCEDURE SetTransmissionCoeff (self: T; val: REAL) =
  BEGIN
    self.transmission := 1.0 - val;
  END SetTransmissionCoeff;


PROCEDURE DrawMarker (self : T; p : Point3.T) =
  BEGIN
    IF self.phase = 2 THEN
      (*** SRC 129 says that markers are not affected by lighting ... ***)
      GL.glDisable (GL.GL_LIGHTING);

      GL.glColor3fv (ADR (self.markerColor));
      GL.glPointSize (self.markerScale);

      GL.glBegin (GL.GL_POINTS);
      GL.glVertex3fv (ADR (p));
      GL.glEnd ();

      (*** Reset GL lighting to its previous state *)
      SetLighting (self, self.lighting);
    END;
  END DrawMarker;


PROCEDURE DrawLine (self: T; p1, p2: Point3.T) =
  BEGIN
    IF self.phase = 2 THEN
      (*** SRC 129 says that lines are not affected by lighting ... ***)
      GL.glDisable (GL.GL_LIGHTING);

      GL.glColor3fv (ADR (self.lineColor));
            (* ... strictly speaking wrong: Color.T # ARRAY [1..3] OF REAL *)

      GL.glLineWidth (self.lineWidth);
      GL.glLineStipple (ROUND (self.lineWidth), self.lineType);

      GL.glBegin (GL.GL_LINES);
      GL.glVertex3fv (ADR (p1));
      GL.glVertex3fv (ADR (p2));
      GL.glEnd ();

      (*** Reset GL lighting to its previous state *)
      SetLighting (self, self.lighting);
    END;
  END DrawLine;


PROCEDURE DrawPolygon (self         : T;
                       READONLY pts : ARRAY OF Point3.T;
                       shape        : GO.Shape) =

  PROCEDURE DrawHollowPolygon () =
    BEGIN
      (*** Draw a line-loop around the contour of the polygon ***)
      GL.glBegin (GL.GL_LINE_LOOP);
      FOR i := 0 TO LAST (pts) DO
        GL.glVertex3fv (ADR (pts[i]));
      END;
      GL.glEnd ();
    END DrawHollowPolygon;

  PROCEDURE DrawSolidConvexPolygon () =
    VAR
      n: Point3.T;
    BEGIN
      GL.glBegin (GL.GL_POLYGON);

      (* If the polygon is non-degenerate, take the first 3 vertices,
         compute the normal vector, and set it.  We don't scale the normal
         vector to unit length (presumably, OpenGL can do it more efficiently),
         and we cannot determine which side of the polygon is the "front". *)

      IF NUMBER (pts) >= 3 THEN
        n := Point3.CrossProduct (Point3.Minus (pts[1], pts[0]),
                                  Point3.Minus (pts[2], pts[0]));
        GL.glNormal3fv (ADR (n));
      END;

      FOR i := 0 TO LAST (pts) DO
        GL.glVertex3fv (ADR (pts[i]));
      END;
      GL.glEnd ();
    END DrawSolidConvexPolygon;

  PROCEDURE DrawSolidNonConvexPolygon () =
    BEGIN
      (** Note: We can get around with a single global tesselation object **)
      WITH tess = GLu.gluNewTess () DO
        <* ASSERT tess # NIL *>

        GLu.gluTessCallback (tess, GLu.GLU_BEGIN,
                             LOOPHOLE (GL.glBegin, GLu.GLUtessAnyProc));
        GLu.gluTessCallback (tess, GLu.GLU_VERTEX,
                             LOOPHOLE (GL.glVertex3dv, GLu.GLUtessAnyProc));
        GLu.gluTessCallback (tess, GLu.GLU_END,
                             LOOPHOLE (GL.glEnd, GLu.GLUtessAnyProc));

        GLu.gluBeginPolygon (tess);

        WITH verts = NEW (REF ARRAY OF GLpoint3d, NUMBER (pts)) DO
          FOR i := 0 TO LAST (pts) DO
            WITH v = verts[i], p = pts[i] DO
              v := GLpoint3d {FLOAT (p.x, LONGREAL),
                              FLOAT (p.y, LONGREAL),
                              FLOAT (p.z, LONGREAL)};
              GLu.gluTessVertex (tess, ADR (v), ADR (v));
            END;
          END;
        END;

        GLu.gluEndPolygon (tess);

        GLu.gluDeleteTess (tess);
      END;
    END DrawSolidNonConvexPolygon;

  PROCEDURE DrawSolidComplexPolygon () =
    (* This procedure uses a trick described in the "Red Book"
       (the OpenGL Programming Guide by the OpenGL Architecture Review Board)
       on page 398f. *)
    VAR
      n: Point3.T;
    BEGIN
      (*** Clear the stencil buffer ***)
      GL.glClearStencil (0);
      GL.glClear (GL.GL_STENCIL_BUFFER_BIT);

      (* If the polygon is non-degenerate, take the first 3 vertices, and
         compute the normal vector.  We don't scale the normal vector to unit
         length (presumably, OpenGL can do it more efficiently), and we cannot
         determine which side of the polygon is the "front". *)

      IF NUMBER (pts) >= 3 THEN
        n := Point3.CrossProduct (Point3.Minus (pts[1], pts[0]),
                                  Point3.Minus (pts[2], pts[0]));
      END;
      (* (p2 - p0) x (p1 - p0)  ->  Lower side is dark *)
      (* (p1 - p0) x (p2 - p0)  ->  Upper side is dark *)

      (*** Enable the stencil test. For each fragment of the triangles to
           come, invert the corresponding stencil buffer entry, but leave
           the frame buffer entry unchanged. ***)
      GL.glStencilFunc (GL.GL_NEVER, 0, 0);
      GL.glStencilOp (GL.GL_INVERT, GL.GL_KEEP, GL.GL_KEEP);
      GL.glEnable (GL.GL_STENCIL_TEST);

      (*** Draw series of triangles (affecting only stencil buffer) ***)
      GL.glBegin (GL.GL_TRIANGLE_FAN);
      FOR i := 0 TO LAST (pts) DO
        GL.glVertex3fv (ADR (pts[i]));
      END;
      GL.glEnd ();

      (*** For each fragment of the triangles to come, modify the corresponding
           frame buffer entry iff the stencil buffer entry is non-zero. Leave
           the stencil buffer entry unchanged. ***)
      GL.glStencilFunc (GL.GL_EQUAL, 1, 1);
      GL.glStencilOp (GL.GL_KEEP, GL.GL_KEEP, GL.GL_KEEP);

      (* Draw series of triangles (affecting frame buffer). Note that we have
         to specify a normal vector, and that OpenGL will invert the normal of
         polygons that are specified through clockwise vertices *)
      FOR i := 1 TO LAST (pts) - 1 DO
        GL.glBegin (GL.GL_TRIANGLES);
        n := Point3.CrossProduct (Point3.Minus (pts[i],   pts[0]),
                                  Point3.Minus (pts[i+1], pts[0]));
        GL.glNormal3fv (ADR (n));
        GL.glVertex3fv (ADR (pts[0]));
        GL.glVertex3fv (ADR (pts[i]));
        GL.glVertex3fv (ADR (pts[i+1]));
        GL.glEnd ();
      END;


      (*** Disable stencil test ***)
      GL.glDisable (GL.GL_STENCIL_TEST);
    END DrawSolidComplexPolygon;


  PROCEDURE DrawSolidPolygon () =
    BEGIN
      CASE shape OF
      | GO.Shape.Convex    => DrawSolidConvexPolygon();
      | GO.Shape.NonConvex => DrawSolidNonConvexPolygon();
      | GO.Shape.Complex   => DrawSolidComplexPolygon();
      | GO.Shape.Unknown   => DrawSolidComplexPolygon();
      END;
    END DrawSolidPolygon;

  BEGIN
    IF self.phase = 2 THEN
      SetSurfaceMaterial (self);
      CASE self.rasterMode OF
      | RasterModeProp.Kind.Solid  => DrawSolidPolygon ();
      | RasterModeProp.Kind.Hollow => DrawHollowPolygon ();
      | RasterModeProp.Kind.Empty  => (*** a no-op ***)
      END;
      UnsetSurfaceMaterial (self);
    END;

    IF self.edgeFlag THEN
      (*** SRC 129 says that lines are not affected by lighting ... ***)
      GL.glDisable (GL.GL_LIGHTING);

      GL.glColor3fv (ADR (self.edgeColor));
      GL.glLineWidth (self.edgeWidth);
      GL.glLineStipple (ROUND (self.edgeWidth), self.edgeType);

      DrawHollowPolygon ();

      (*** Reset GL lighting to its previous state *)
      SetLighting (self, self.lighting);
    END;
  END DrawPolygon;


PROCEDURE DrawQuadMesh (self         : T;
                        READONLY pts : ARRAY OF ARRAY OF Point3.T;
                        shape        : GO.Shape) =

  PROCEDURE DrawHollowQuadMesh () =
    BEGIN
      FOR i := 0 TO LAST (pts) DO
        GL.glBegin (GL.GL_LINE_STRIP);
        FOR j := 0 TO LAST(pts[i]) DO
          GL.glVertex3fv (ADR (pts[i][j]));
        END;
        GL.glEnd ();
      END;

      FOR j := 0 TO LAST(pts[0]) DO
        GL.glBegin (GL.GL_LINE_STRIP);
        FOR i := 0 TO LAST (pts) DO
          GL.glVertex3fv (ADR (pts[i][j]));
        END;
        GL.glEnd ();
      END;
    END DrawHollowQuadMesh;

  PROCEDURE DrawSolidQuadMesh () =
    BEGIN
      IF shape = GO.Shape.Convex THEN
        DrawSolidConvexQuadMesh ();
      ELSE
        DrawSolidGeneralQuadMesh ();
      END;
    END DrawSolidQuadMesh;

  PROCEDURE DrawSolidConvexQuadMesh () =
    BEGIN
      FOR i := 0 TO LAST (pts) - 1 DO
        WITH line1 = pts[i], line2 = pts[i+1] DO
          GL.glBegin (GL.GL_QUAD_STRIP);
          FOR j := 0 TO LAST(line1) DO
            (* We don't specify any normal vectors here. Probably we should! *)
            GL.glVertex3fv (ADR (line1[j]));
            GL.glVertex3fv (ADR (line2[j]));
          END;
          GL.glEnd ();
        END;
      END;

      IF self.edgeFlag THEN
        (*** SRC 129 says that lines are not affected by lighting ... ***)
        GL.glDisable (GL.GL_LIGHTING);

        GL.glColor3fv (ADR (self.edgeColor));
        GL.glLineWidth (self.edgeWidth);
        GL.glLineStipple (ROUND (self.edgeWidth), self.edgeType);

        DrawHollowQuadMesh ();

        (*** Reset GL lighting to its previous state *)
        SetLighting (self, self.lighting);
      END;

    END DrawSolidConvexQuadMesh;

  PROCEDURE DrawSolidGeneralQuadMesh () =
    BEGIN
      FOR i := 0 TO LAST (pts) - 1 DO
        WITH line1 = pts[i], line2 = pts[i+1] DO
          FOR j := 0 TO LAST(line1) - 1 DO
            WITH quad = ARRAY OF Point3.T {line1[j],
                                           line2[j],
                                           line2[j+1],
                                           line1[j+1]} DO
              DrawPolygon (self, quad, shape);
            END;
          END;
        END;
      END;
    END DrawSolidGeneralQuadMesh;


  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    IF self.phase = 2 THEN
      SetSurfaceMaterial (self);
      CASE self.rasterMode OF
      | RasterModeProp.Kind.Solid  => DrawSolidQuadMesh ();
      | RasterModeProp.Kind.Hollow => DrawHollowQuadMesh ();
      | RasterModeProp.Kind.Empty  => (*** a no-op ***)
      END;
      UnsetSurfaceMaterial (self);
    END;
  END DrawQuadMesh;


PROCEDURE DrawColoredQuadMesh (         self  : T;
                               READONLY points: ARRAY OF ARRAY OF Point3.T;
                               READONLY colors: ARRAY OF ARRAY OF Color.T;
                                        shape : GO.Shape) =

  PROCEDURE DrawHollowQuadMesh (lit: BOOLEAN) =

    PROCEDURE EmitColoredVertex (i, j: INTEGER) =
      VAR
        rgba : GLrgba;
        n    : Point3.T;
      BEGIN
        WITH x = MIN (i, LAST(colors)),
             y = MIN (j, LAST(colors[x])),
             c = colors [x][y] DO

          (*** Compute a normal vector ***)
          WITH a = points[x][y],
               b = points[x+1][y],
               c = points[x][y+1] DO
            n := Point3.CrossProduct (Point3.Minus(b, a), Point3.Minus(c, a));
            GL.glNormal3fv (ADR (n));
          END;

          (*** Set the color-related material properties ***)
          rgba := GLrgba {self.ambientReflCoeff * c.r,
                          self.ambientReflCoeff * c.g,
                          self.ambientReflCoeff * c.b,
                          self.transmission};
          GL.glMaterialfv (GL.GL_FRONT_AND_BACK, GL.GL_AMBIENT, ADR (rgba));

          rgba := GLrgba {self.diffuseReflCoeff * c.r,
                          self.diffuseReflCoeff * c.g,
                          self.diffuseReflCoeff * c.b,
                          self.transmission};
          GL.glMaterialfv (GL.GL_FRONT_AND_BACK, GL.GL_DIFFUSE, ADR (rgba));

          rgba := GLrgba {self.specularReflCoeff * self.specularReflColor.r,
                          self.specularReflCoeff * self.specularReflColor.g,
                          self.specularReflCoeff * self.specularReflColor.b,
                          self.transmission};
          GL.glMaterialfv (GL.GL_FRONT_AND_BACK, GL.GL_SPECULAR, ADR (rgba));

          (*** Set the color -- no idea why I have to do it ... ***)
          GL.glColor3fv (ADR (c));

          (*** Emit the vertex ***)
          GL.glVertex3fv (ADR (points[i][j]));
        END;
      END EmitColoredVertex;

    BEGIN
      FOR i := 0 TO LAST (points) DO
        GL.glBegin (GL.GL_LINE_STRIP);
        FOR j := 0 TO LAST(points[i]) DO
          IF lit THEN
            EmitColoredVertex (i, j);
          ELSE
            GL.glVertex3fv (ADR (points[i][j]));
          END;
        END;
        GL.glEnd ();
      END;

      FOR j := 0 TO LAST(points[0]) DO
        GL.glBegin (GL.GL_LINE_STRIP);
        FOR i := 0 TO LAST (points) DO
          IF lit THEN
            EmitColoredVertex (i, j);
          ELSE
            GL.glVertex3fv (ADR (points[i][j]));
          END;
        END;
        GL.glEnd ();
      END;
    END DrawHollowQuadMesh;

  PROCEDURE DrawSolidQuadMesh () =
    BEGIN
      IF shape = GO.Shape.Convex THEN
        DrawSolidConvexQuadMesh ();
      ELSE
        DrawSolidGeneralQuadMesh ();
      END;
    END DrawSolidQuadMesh;

  PROCEDURE DrawSolidConvexQuadMesh () =
    BEGIN
      FOR i := 0 TO LAST (points) - 1 DO
        WITH line1 = points[i], line2 = points[i+1] DO
          GL.glBegin (GL.GL_QUAD_STRIP);
          FOR j := 0 TO LAST(line1) DO
            (* We don't specify any normal vectors here. Probably we should! *)
            IF j > 0 THEN
              GL.glColor3fv (ADR (colors[i][j-1]));
            END;
            GL.glVertex3fv (ADR (line1[j]));
            GL.glVertex3fv (ADR (line2[j]));
          END;
          GL.glEnd ();
        END;
      END;

      IF self.edgeFlag THEN
        (*** SRC 129 says that lines are not affected by lighting ... ***)
        GL.glDisable (GL.GL_LIGHTING);

        GL.glColor3fv (ADR (self.edgeColor));
        GL.glLineWidth (self.edgeWidth);
        GL.glLineStipple (ROUND (self.edgeWidth), self.edgeType);

        DrawHollowQuadMesh (FALSE);

        (*** Reset GL lighting to its previous state *)
        SetLighting (self, self.lighting);
      END;

    END DrawSolidConvexQuadMesh;

  PROCEDURE DrawSolidGeneralQuadMesh () =
    VAR
      fc, bc : Color.T;
    BEGIN
      FOR i := 0 TO LAST (points) - 1 DO
        WITH line1 = points[i], line2 = points[i+1] DO
          FOR j := 0 TO LAST(line1) - 1 DO
            WITH quad = ARRAY OF Point3.T {line1[j],
                                           line2[j],
                                           line2[j+1],
                                           line1[j+1]} DO
              fc := self.frontColor;
              bc := self.backColor;
              self.frontColor := colors[i][j];
              self.backColor := colors[i][j];
              DrawPolygon (self, quad, shape);
              self.frontColor := fc;
              self.backColor  := bc;
            END;
          END;
        END;
      END;
    END DrawSolidGeneralQuadMesh;

  BEGIN
    IF self.phase = 2 THEN
      SetSurfaceMaterial (self);
      CASE self.rasterMode OF
      | RasterModeProp.Kind.Solid  => DrawSolidQuadMesh ();
      | RasterModeProp.Kind.Hollow => DrawHollowQuadMesh (TRUE);
      | RasterModeProp.Kind.Empty  => (*** a no-op ***)
      END;
      UnsetSurfaceMaterial (self);
    END;
  END DrawColoredQuadMesh;


PROCEDURE SetSurfaceMaterial (self: T) =
  VAR
    rgba : GLrgba;
  BEGIN
    IF self.transmission < 1.0 THEN
      (* If the sphere is transparent, disable depth buffer writing (so
         transparent fragments won't mask out opaque ones behind them),
         enable blending, and set up the blending function *)
      GL.glDepthMask (GL.GL_FALSE);
      GL.glEnable (GL.GL_BLEND);
      GL.glBlendFunc (GL.GL_SRC_ALPHA, GL.GL_ONE_MINUS_SRC_ALPHA);
    END;

    (* We could keep track of the color value set by the last call to
       "glColor", and call it only if there is a change.  For now, I use
       the conservative (aka brute force) approach -- always call it! *)

    (* If "GL_LIGHTING" is disabled, the color of a polygon is set through
       "glColor"; otherwise, it is set through "glMaterial". It seems that
       "glColor" does not distinguish between front faces and back faces. *)
    GL.glColor3fv (ADR (self.frontColor));

    rgba := GLrgba {self.ambientReflCoeff * self.frontColor.r,
                    self.ambientReflCoeff * self.frontColor.g,
                    self.ambientReflCoeff * self.frontColor.b,
                    self.transmission};
    GL.glMaterialfv (GL.GL_FRONT_AND_BACK, GL.GL_AMBIENT, ADR (rgba));

    rgba := GLrgba {self.diffuseReflCoeff * self.frontColor.r,
                    self.diffuseReflCoeff * self.frontColor.g,
                    self.diffuseReflCoeff * self.frontColor.b,
                    self.transmission};
    GL.glMaterialfv (GL.GL_FRONT_AND_BACK, GL.GL_DIFFUSE, ADR (rgba));

    rgba := GLrgba {self.specularReflCoeff * self.specularReflColor.r,
                    self.specularReflCoeff * self.specularReflColor.g,
                    self.specularReflCoeff * self.specularReflColor.b,
                    self.transmission};
    GL.glMaterialfv (GL.GL_FRONT_AND_BACK, GL.GL_SPECULAR, ADR (rgba));

    GL.glMaterialf (GL.GL_FRONT_AND_BACK, GL.GL_SHININESS,
                    self.specularReflConc);
  END SetSurfaceMaterial;


PROCEDURE UnsetSurfaceMaterial (self: T) =
  BEGIN
    IF self.transmission < 1.0 THEN
      GL.glDepthMask (GL.GL_TRUE);
      GL.glDisable (GL.GL_BLEND);
    END;
  END UnsetSurfaceMaterial;


CONST
  NoList = 0;
TYPE
  StructureList = REF RECORD
    prec   : INTEGER;
    fillId : GL.GLuint := NoList;
    lineId : GL.GLuint := NoList;
    next   : StructureList;
  END;


PROCEDURE DrawProtoSphere (self: T; prec: INTEGER) =

  TYPE Kind = {Line, Fill};

  PROCEDURE Draw (kind: Kind) =
    VAR
      list : StructureList := self.sphereStructures;
      prev : StructureList := NIL;
    BEGIN
      (* Iterate over "list" until we find a cell with the right precision,
         or fall off the back of the list. *)
      WHILE list # NIL AND list.prec # prec DO
        prev := list;
        list := list.next;
      END;
      (* At this point, "list" is either NIL, or points to a cell with the
         right precision. *)

      (* Move the cell to the front of "self.sphereStructures". *)
      IF list = NIL THEN
        (* Not found in "self.sphereStructures" (which might be NIL).
           Create a new cell, and insert it at the head of the list. *)
        list := NEW (StructureList, prec := prec);
        list.next := self.sphereStructures;
        self.sphereStructures := list;
      ELSIF prev # NIL THEN
        (* Found in "self.sphereStructures" (not at head).
           Move cell to head. *)
        prev.next := list.next;
        list.next := self.sphereStructures;
        self.sphereStructures := list;
      END;
      (* At this point, "list" is non-NIL, and point to a cell "c" such that
         "c.prec = prec". "c.fillId" and "c.lineId" contain either "NoList"
         or a valid display list. *)

      (* If we have the right display lists cached, call them and return. *)
      CASE kind OF
      | Kind.Fill =>
        IF list.fillId # NoList THEN
          GL.glCallList (list.fillId);
          RETURN;
        END;
      | Kind.Line =>
        IF list.lineId # NoList THEN
          GL.glCallList (list.lineId);
          RETURN;
        END;
      END;

      (* Did not find a matching sphere in the cache -- need to create one *)
      WITH dlid = GL.glGenLists (1) DO

        IF dlid # NoList THEN
          GL.glNewList (dlid, GL.GL_COMPILE_AND_EXECUTE);
        END;

        WITH quad = GLu.gluNewQuadric () DO
          <* ASSERT quad # NIL *>

          CASE kind OF
          | Kind.Fill =>
            GLu.gluQuadricDrawStyle (quad, GLu.GLU_FILL);
            GLu.gluSphere (quad, 1.0d0, prec, prec);
            list.fillId := dlid;
          | Kind.Line =>
            GLu.gluQuadricDrawStyle (quad, GLu.GLU_LINE);
            GLu.gluSphere (quad, 1.005d0, prec, prec);     (* 0.5 % larger *)
            list.lineId := dlid;
          END;

        END;

        IF dlid # NoList THEN
          GL.glEndList ();
        END;

      END;

    END Draw;

  BEGIN
    IF self.phase = 2 THEN

      SetSurfaceMaterial (self);
      CASE self.rasterMode OF
      | RasterModeProp.Kind.Solid  => Draw (Kind.Fill);
      | RasterModeProp.Kind.Hollow => Draw (Kind.Line);
      | RasterModeProp.Kind.Empty  => (*** no-op ***)
      END;
      UnsetSurfaceMaterial (self);

      IF self.edgeFlag THEN
        (*** SRC 129 says that lines are not affected by lighting ... ***)
        GL.glDisable (GL.GL_LIGHTING);

        GL.glColor3fv (ADR (self.edgeColor));
        GL.glLineWidth (self.edgeWidth);
        GL.glLineStipple (ROUND (self.edgeWidth), self.edgeType);

        Draw (Kind.Line);

        (*** Reset GL lighting to its previous state *)
        SetLighting (self, self.lighting);
      END;

    END;
  END DrawProtoSphere;


PROCEDURE DrawProtoCone (self: T; prec: INTEGER) =

  TYPE Kind = {Line, Fill};

  PROCEDURE Draw (kind: Kind) =
    VAR
      list : StructureList := self.coneStructures;
      prev : StructureList := NIL;
    BEGIN
      (* Iterate over "list" until we find a cell with the right precision,
         or fall off the back of the list. *)
      WHILE list # NIL AND list.prec # prec DO
        prev := list;
        list := list.next;
      END;
      (* At this point, "list" is either NIL, or points to a cell with the
         right precision. *)

      (* Move the cell to the front of "self.coneStructures". *)
      IF list = NIL THEN
        (* Not found in "self.coneStructures" (which might be NIL).
           Create a new cell, and insert it at the head of the list. *)
        list := NEW (StructureList, prec := prec);
        list.next := self.coneStructures;
        self.coneStructures := list;
      ELSIF prev # NIL THEN
        (* Found in "self.coneStructures" (not at head).
           Move cell to head. *)
        prev.next := list.next;
        list.next := self.coneStructures;
        self.coneStructures := list;
      END;
      (* At this point, "list" is non-NIL, and point to a cell "c" such that
         "c.prec = prec". "c.fillId" and "c.lineId" contain either "NoList"
         or a valid display list. *)

      (* If we have the right display lists cached, call them and return. *)
      CASE kind OF
      | Kind.Fill =>
        IF list.fillId # NoList THEN
          GL.glCallList (list.fillId);
          RETURN;
        END;
      | Kind.Line =>
        IF list.lineId # NoList THEN
          GL.glCallList (list.lineId);
          RETURN;
        END;
      END;

      (* Did not find a matching cone in the cache -- need to create one *)
      WITH dlid = GL.glGenLists (1) DO

        IF dlid # NoList THEN
          GL.glNewList (dlid, GL.GL_COMPILE_AND_EXECUTE);
        END;

        WITH quad = GLu.gluNewQuadric () DO
          <* ASSERT quad # NIL *>

          CASE kind OF
          | Kind.Fill =>
            GLu.gluQuadricDrawStyle (quad, GLu.GLU_FILL);
            GLu.gluCylinder (quad, 1.0d0, 0.0d0, 1.0d0, prec, prec);
            list.fillId := dlid;
          | Kind.Line =>
            GLu.gluQuadricDrawStyle (quad, GLu.GLU_LINE);
            GLu.gluCylinder (quad, 1.005d0, 0.0d0, 1.005d0, prec, prec);
            list.lineId := dlid;
          END;

        END;

        IF dlid # NoList THEN
          GL.glEndList ();
        END;

      END;

    END Draw;

  BEGIN
    IF self.phase = 2 THEN

      SetSurfaceMaterial (self);
      CASE self.rasterMode OF
      | RasterModeProp.Kind.Solid  => Draw (Kind.Fill);
      | RasterModeProp.Kind.Hollow => Draw (Kind.Line);
      | RasterModeProp.Kind.Empty  => (*** no-op ***)
      END;
      UnsetSurfaceMaterial (self);

      IF self.edgeFlag THEN
        (*** SRC 129 says that lines are not affected by lighting ... ***)
        GL.glDisable (GL.GL_LIGHTING);

        GL.glColor3fv (ADR (self.edgeColor));
        GL.glLineWidth (self.edgeWidth);
        GL.glLineStipple (ROUND (self.edgeWidth), self.edgeType);
        Draw (Kind.Line);

        (*** Reset GL lighting to its previous state *)
        SetLighting (self, self.lighting);
      END;

    END;
  END DrawProtoCone;


PROCEDURE DrawProtoCylinder (self: T; prec: INTEGER) =

  TYPE Kind = {Line, Fill};

  PROCEDURE Draw (kind: Kind) =
    VAR
      list : StructureList := self.cylinderStructures;
      prev : StructureList := NIL;
    BEGIN
      (* Iterate over "list" until we find a cell with the right precision,
         or fall off the back of the list. *)
      WHILE list # NIL AND list.prec # prec DO
        prev := list;
        list := list.next;
      END;
      (* At this point, "list" is either NIL, or points to a cell with the
         right precision. *)

      (* Move the cell to the front of "self.cylinderStructures". *)
      IF list = NIL THEN
        (* Not found in "self.cylinderStructures" (which might be NIL).
           Create a new cell, and insert it at the head of the list. *)
        list := NEW (StructureList, prec := prec);
        list.next := self.cylinderStructures;
        self.cylinderStructures := list;
      ELSIF prev # NIL THEN
        (* Found in "self.cylinderStructures" (not at head).
           Move cell to head. *)
        prev.next := list.next;
        list.next := self.cylinderStructures;
        self.cylinderStructures := list;
      END;
      (* At this point, "list" is non-NIL, and point to a cell "c" such that
         "c.prec = prec". "c.fillId" and "c.lineId" contain either "NoList"
         or a valid display list. *)

      (* If we have the right display lists cached, call them and return. *)
      CASE kind OF
      | Kind.Fill =>
        IF list.fillId # NoList THEN
          GL.glCallList (list.fillId);
          RETURN;
        END;
      | Kind.Line =>
        IF list.lineId # NoList THEN
          GL.glCallList (list.lineId);
          RETURN;
        END;
      END;

      (* Did not find a matching cylinder in the cache -- need to create one *)
      WITH dlid = GL.glGenLists (1) DO

        IF dlid # NoList THEN
          GL.glNewList (dlid, GL.GL_COMPILE_AND_EXECUTE);
        END;

        WITH quad = GLu.gluNewQuadric () DO
          <* ASSERT quad # NIL *>

          CASE kind OF
          | Kind.Fill =>
            GLu.gluQuadricDrawStyle (quad, GLu.GLU_FILL);
            GLu.gluCylinder (quad, 1.0d0, 1.0d0, 1.0d0, prec, prec);
            list.fillId := dlid;
          | Kind.Line =>
            GLu.gluQuadricDrawStyle (quad, GLu.GLU_LINE);
            GLu.gluCylinder (quad, 1.005d0, 1.005d0, 1.0d0, prec, prec);
            list.lineId := dlid;
          END;

        END;

        IF dlid # NoList THEN
          GL.glEndList ();
        END;

      END;

    END Draw;

  BEGIN
    IF self.phase = 2 THEN

      SetSurfaceMaterial (self);
      CASE self.rasterMode OF
      | RasterModeProp.Kind.Solid  => Draw (Kind.Fill);
      | RasterModeProp.Kind.Hollow => Draw (Kind.Line);
      | RasterModeProp.Kind.Empty  => (*** no-op ***)
      END;
      UnsetSurfaceMaterial (self);

      IF self.edgeFlag THEN
        (*** SRC 129 says that lines are not affected by lighting ... ***)
        GL.glDisable (GL.GL_LIGHTING);

        GL.glColor3fv (ADR (self.edgeColor));
        GL.glLineWidth (self.edgeWidth);
        GL.glLineStipple (ROUND (self.edgeWidth), self.edgeType);
        Draw (Kind.Line);

        (*** Reset GL lighting to its previous state *)
        SetLighting (self, self.lighting);
      END;

    END;
  END DrawProtoCylinder;


PROCEDURE DrawProtoDisk (self: T; prec: INTEGER) =

  TYPE Kind = {Line, Fill};

  PROCEDURE Draw (kind: Kind) =
    VAR
      list : StructureList := self.diskStructures;
      prev : StructureList := NIL;
    BEGIN
      (* Iterate over "list" until we find a cell with the right precision,
         or fall off the back of the list. *)
      WHILE list # NIL AND list.prec # prec DO
        prev := list;
        list := list.next;
      END;
      (* At this point, "list" is either NIL, or points to a cell with the
         right precision. *)

      (* Move the cell to the front of "self.diskStructures". *)
      IF list = NIL THEN
        (* Not found in "self.diskStructures" (which might be NIL).
           Create a new cell, and insert it at the head of the list. *)
        list := NEW (StructureList, prec := prec);
        list.next := self.diskStructures;
        self.diskStructures := list;
      ELSIF prev # NIL THEN
        (* Found in "self.diskStructures" (not at head).
           Move cell to head. *)
        prev.next := list.next;
        list.next := self.diskStructures;
        self.diskStructures := list;
      END;
      (* At this point, "list" is non-NIL, and point to a cell "c" such that
         "c.prec = prec". "c.fillId" and "c.lineId" contain either "NoList"
         or a valid display list. *)

      (* If we have the right display lists cached, call them and return. *)
      CASE kind OF
      | Kind.Fill =>
        IF list.fillId # NoList THEN
          GL.glCallList (list.fillId);
          RETURN;
        END;
      | Kind.Line =>
        IF list.lineId # NoList THEN
          GL.glCallList (list.lineId);
          RETURN;
        END;
      END;

      (* Did not find a matching disk in the cache -- need to create one *)
      WITH dlid = GL.glGenLists (1) DO

        IF dlid # NoList THEN
          GL.glNewList (dlid, GL.GL_COMPILE_AND_EXECUTE);
        END;

        WITH quad = GLu.gluNewQuadric () DO
          <* ASSERT quad # NIL *>

          CASE kind OF
          | Kind.Fill =>
            GLu.gluQuadricDrawStyle (quad, GLu.GLU_FILL);
            GLu.gluDisk (quad, 0.0d0, 1.0d0, prec, prec);
            list.fillId := dlid;
          | Kind.Line =>
            GLu.gluQuadricDrawStyle (quad, GLu.GLU_LINE);
            GLu.gluDisk (quad, 0.0d0, 1.0d0, prec, prec);
              (* lies in same plane ==>  surface edges have slight artifacts *)
            list.lineId := dlid;
          END;

        END;

        IF dlid # NoList THEN
          GL.glEndList ();
        END;

      END;

    END Draw;

  BEGIN
    IF self.phase = 2 THEN

      IF self.edgeFlag THEN
        (*** SRC 129 says that lines are not affected by lighting ... ***)
        GL.glDisable (GL.GL_LIGHTING);

        (* Set up edge color, width, and type ("stipple" in OpenGL) *)
        GL.glColor3fv (ADR (self.edgeColor));
        GL.glLineWidth (self.edgeWidth);
        GL.glLineStipple (ROUND (self.edgeWidth), self.edgeType);

        (*** Clear the stencil buffer ***)
        GL.glClearStencil (0);
        GL.glClear (GL.GL_STENCIL_BUFFER_BIT);

        (* First, set all entries in the stencil buffer to 0.  Then, set up the
           stencil test: for each fragment of the surface edges that passes the
           Z-buffer test, set the stencil buffer entry to 1. *)
        GL.glStencilFunc (GL.GL_ALWAYS, 1, 1);
        GL.glStencilOp (GL.GL_KEEP, GL.GL_KEEP, GL.GL_REPLACE);
        GL.glEnable (GL.GL_STENCIL_TEST);

        Draw (Kind.Line);

        (*** Reset GL lighting to its previous state ***)
        SetLighting (self, self.lighting);

        (* Set up the stencil test: Draw any future fragment only if the
           corresponding stencil buffer entry is 0.  In other words, mask out
           the surface edges. *)
        GL.glStencilFunc (GL.GL_EQUAL, 0, 1);
      END;

      SetSurfaceMaterial (self);
      CASE self.rasterMode OF
      | RasterModeProp.Kind.Solid  => Draw (Kind.Fill);
      | RasterModeProp.Kind.Hollow => Draw (Kind.Line);
      | RasterModeProp.Kind.Empty  => (*** no-op ***)
      END;
      UnsetSurfaceMaterial (self);

      IF self.edgeFlag THEN
        GL.glDisable (GL.GL_STENCIL_TEST);
      END;
    END;
  END DrawProtoDisk;




TYPE
  VertexData  = RECORD
    pt   : Point3.T;
    norm : Point3.T;
  END;
  TorusVertices = REF ARRAY OF ARRAY OF VertexData;


PROCEDURE DrawProtoTorus (self : T; prec : INTEGER; radiusRatio : REAL ) =

  PROCEDURE DrawHollowTorus () =
    BEGIN
      WITH verts = ComputeUnitTorus (prec, radiusRatio * 1.005) DO
        FOR i := 0 TO LAST (verts^) DO
          GL.glBegin (GL.GL_LINE_STRIP);
          FOR j := 0 TO LAST(verts[i]) DO
            GL.glNormal3fv (ADR (verts[i][j].norm));
            GL.glVertex3fv (ADR (verts[i][j].pt));
          END;
          GL.glEnd ();
        END;
        FOR j := 0 TO LAST(verts[0]) DO
          GL.glBegin (GL.GL_LINE_STRIP);
          FOR i := 0 TO LAST (verts^) DO
            GL.glNormal3fv (ADR (verts[i][j].norm));
            GL.glVertex3fv (ADR (verts[i][j].pt));
          END;
          GL.glEnd ();
        END;
      END;
    END DrawHollowTorus;

  PROCEDURE DrawSolidTorus () =
    BEGIN
      WITH verts = ComputeUnitTorus (prec, radiusRatio) DO
        FOR i := 0 TO LAST (verts^) - 1 DO
          WITH line1 = verts[i], line2 = verts[i+1] DO
            GL.glBegin (GL.GL_QUAD_STRIP);
            FOR j := 0 TO LAST(line1) DO
              WITH point1 = line1[j], point2 = line2[j] DO
                GL.glNormal3fv (ADR (point1.norm));
                GL.glVertex3fv (ADR (point1.pt));
                GL.glNormal3fv (ADR (point2.norm));
                GL.glVertex3fv (ADR (point2.pt));
              END;
            END;
            GL.glEnd ();
          END;
        END;
      END;
    END DrawSolidTorus;

  BEGIN
    IF self.phase = 2 THEN

      SetSurfaceMaterial (self);
      CASE self.rasterMode OF
      | RasterModeProp.Kind.Solid  => DrawSolidTorus ();
      | RasterModeProp.Kind.Hollow => DrawHollowTorus ();
      | RasterModeProp.Kind.Empty  => (*** no-op ***)
      END;
      UnsetSurfaceMaterial (self);

      IF self.edgeFlag THEN
        (*** SRC 129 says that lines are not affected by lighting ... ***)
        GL.glDisable (GL.GL_LIGHTING);
        GL.glColor3fv (ADR (self.edgeColor));
        GL.glLineWidth (self.edgeWidth);
        GL.glLineStipple (ROUND (self.edgeWidth), self.edgeType);

        DrawHollowTorus ();

        (*** Reset GL lighting to its previous state *)
        SetLighting (self, self.lighting);
      END;

    END;
  END DrawProtoTorus;


PROCEDURE ComputeUnitTorus (prec : INTEGER; radius2 : REAL) : TorusVertices =
  VAR
    verts : TorusVertices := NEW (TorusVertices, prec+1, prec+1);
  BEGIN
    WITH u = AuxG.GetUnitCirclePoints (prec),
             (* normal of unit circle is z-axis *)
         normal = Point3.T {0.0, 0.0, 1.0} DO
      FOR i := 0 TO prec DO
        WITH aux  = u[i],
             a2   = Point3.Plus (aux, Point3.ScaleToLen (normal, radius2)),
             b2   = Point3.Plus (aux, Point3.ScaleToLen (aux, radius2)),
             c2   = Point3.Plus (aux, Point3.CrossProduct(aux, normal)),
             N    = Matrix4.TransformUnitCube (aux, a2, b2, c2) DO
          FOR j := 0 TO prec DO
            WITH p = Matrix4.TransformPoint3 (N, u[j]),
                 n = Point3.Minus (aux, p) DO
              verts[i][j] := VertexData {p, n};
            END;
          END;
        END;
      END;
    END;
    RETURN verts;
  END ComputeUnitTorus;


(*****************************************************************************)
(* Event handling                                                            *)
(*****************************************************************************)


PROCEDURE ProcessEvents (self : T) =

  PROCEDURE CheckTypedWindowEvent (self : T;
                                   type : Ctypes.int;
                                   VAR event : X.XEvent) : X.Bool =
    BEGIN
      LOCK conn DO
        RETURN X.XCheckTypedWindowEvent (conn.dpy, self.window, type,
                                         ADR (event));
      END;
    END CheckTypedWindowEvent;

  PROCEDURE CheckWindowEvent (self : T;
                              mask : Ctypes.long;
                              VAR event : X.XEvent) : X.Bool =
    BEGIN
      LOCK conn DO
        RETURN X.XCheckWindowEvent (conn.dpy, self.window, mask, ADR (event));
      END;
    END CheckWindowEvent;

  VAR
    ev        : X.XEvent;
    button    : VBT.Button;
    clickType : VBT.ClickType;
    mask      : Ctypes.long;
  BEGIN
    (*** Set up the mask for events we are interested in. ***)
    mask := 0;
    mask := Word.Or (mask, X.ExposureMask);         (* for X.Expose          *)
    mask := Word.Or (mask, X.PointerMotionMask);    (* for X.MotionNotify    *)
    mask := Word.Or (mask, X.ButtonPressMask);      (* for X.ButtonPress     *)
    mask := Word.Or (mask, X.ButtonReleaseMask);    (* for X.ButtonRelease   *)
    mask := Word.Or (mask, X.KeyPressMask);         (* for X.KeyPress        *)
    mask := Word.Or (mask, X.KeyReleaseMask);       (* for X.KeyRelease      *)
    mask := Word.Or (mask, X.StructureNotifyMask);  (* for X.ConfigureNotify *)

    (*
     * For some reason, ClientMessage events are not picked up by
     * X.XCheckWindowEvent, so I take care of them here.
     *)

    IF CheckTypedWindowEvent (self, X.ClientMessage, ev) = X.True THEN
      WITH e = ClientMessageEvent(ev) DO
        IF e.message_type = conn.wm_protocols AND e.format = 32 THEN
          IF e.data[0] = conn.wm_delete_window THEN
            self.destroy ();
            RETURN;
          END;
        END;
      END;
    END;

    (*
     * If there is no ClientMessage event indicating a "delete window"
     * request by the window manager, I look for other events pending:
     *)

    WHILE CheckWindowEvent (self, mask, ev) = X.True DO
      CASE ButtonEvent(ev).type OF
      | X.Expose =>
        (*** damage the root object to force a redraw ***)
        IF self.root # NIL THEN
          self.root.damaged := TRUE;
        END;

      | X.ConfigureNotify =>
        WITH w = LOOPHOLE (ADR (ev), X.XConfigureEventStar).width,
             h = LOOPHOLE (ADR (ev), X.XConfigureEventStar).height DO
          self.winWidth  := w;
          self.winHeight := h;
          GL.glViewport (0, 0, w, h);             (* adjust the viewport *)
        END;

      | X.MotionNotify =>
        (*** If several motions in queue, jump to last ***)
        WHILE CheckWindowEvent(self, X.PointerMotionMask, ev) # X.False DO END;

        WITH mev    = MotionEvent (ev),
             posrec = PositionCB.Rec {pos2D := Point.T {mev.x, mev.y},
                                      modifiers := self.modifiers} DO
          self.root.invokePositionCB (posrec);
        END;
      | X.ButtonPress =>
        WITH bev = ButtonEvent(ev) DO
          CASE bev.button OF
          | X.Button1 => button := VBT.Modifier.MouseL;
          | X.Button2 => button := VBT.Modifier.MouseM;
          | X.Button3 => button := VBT.Modifier.MouseR;
          ELSE
            Process.Crash ("G.WaitForEvent: Unknown button event");
          END;
          IF self.buttonDownCount = 0 THEN
            clickType := VBT.ClickType.FirstDown;
          ELSE
            clickType := VBT.ClickType.OtherDown;
          END;
          INC (self.buttonDownCount);
          WITH mouserec = MouseCB.Rec {pos2D       := Point.T {bev.x, bev.y},
                                       whatChanged := button,
                                       modifiers   := self.modifiers,
                                       clickType   := clickType} DO
            self.root.invokeMouseCB (mouserec);
            self.modifiers := self.modifiers + VBT.Modifiers {button};
          END;
        END;
      | X.ButtonRelease =>
        WITH bev = ButtonEvent(ev) DO
          CASE bev.button OF
          | X.Button1 => button := VBT.Modifier.MouseL;
          | X.Button2 => button := VBT.Modifier.MouseM;
          | X.Button3 => button := VBT.Modifier.MouseR;
          ELSE
            Process.Crash ("G.WaitForEvent: Unknown button event");
          END;
          DEC (self.buttonDownCount);
          IF self.buttonDownCount = 0 THEN
            clickType := VBT.ClickType.LastUp;
          ELSE
            clickType := VBT.ClickType.OtherUp;
          END;
          WITH mouserec = MouseCB.Rec {pos2D       := Point.T {bev.x, bev.y},
                                       whatChanged := button,
                                       modifiers   := self.modifiers,
                                       clickType   := clickType} DO
            self.root.invokeMouseCB (mouserec);
            self.modifiers := self.modifiers - VBT.Modifiers {button};
          END;
        END;
      | X.KeyPress =>
        WITH keysym = GetKeySym (ev),
             keyrec = KeyCB.Rec {
                            whatChanged := keysym,
                            wentDown    := TRUE,
                            modifiers   := self.modifiers} DO
          self.root.invokeKeyCB (keyrec);
          self.modifiers := self.modifiers + KeySymToModifierSet (keysym);
        END;
      | X.KeyRelease =>
        WITH keysym = GetKeySym (ev),
             keyrec = KeyCB.Rec {
                            whatChanged := keysym,
                            wentDown    := FALSE,
                            modifiers   := self.modifiers} DO
          self.root.invokeKeyCB (keyrec);
          self.modifiers := self.modifiers - KeySymToModifierSet (keysym);
        END;
      ELSE
        (* some other X event *)
      END;

    END;

  END ProcessEvents;


PROCEDURE KeySymToModifierSet (keysym : VBT.KeySym) : VBT.Modifiers =
  BEGIN
    CASE keysym OF
    | KeyboardKey.Shift_L, KeyboardKey.Shift_R =>
      RETURN VBT.Modifiers {VBT.Modifier.Shift};
    | KeyboardKey.Shift_Lock =>
      RETURN VBT.Modifiers {VBT.Modifier.Lock};
    | KeyboardKey.Control_L, KeyboardKey.Control_R =>
      RETURN VBT.Modifiers {VBT.Modifier.Control};
    | KeyboardKey.Meta_L, KeyboardKey.Meta_R =>
      RETURN VBT.Modifiers {VBT.Modifier.Option};
    ELSE
      RETURN VBT.Modifiers {};
    END;
  END KeySymToModifierSet;


<* INLINE *>
PROCEDURE GetKeySym (VAR ev : X.XEvent) : VBT.KeySym =
  BEGIN
    RETURN X.XLookupKeysym (LOOPHOLE (ADR (ev), X.XKeyEventStar), 0);
  END GetKeySym;


<* INLINE *>
PROCEDURE MotionEvent(VAR ev : X.XEvent) : X.XMotionEventStar =
  BEGIN
    RETURN LOOPHOLE (ADR (ev), X.XMotionEventStar);
  END MotionEvent;


<* INLINE *>
PROCEDURE ButtonEvent(VAR ev : X.XEvent) : X.XButtonEventStar =
  BEGIN
    RETURN LOOPHOLE (ADR (ev), X.XButtonEventStar);
  END ButtonEvent;


<* INLINE *>
PROCEDURE ClientMessageEvent(VAR ev : X.XEvent) : X.XClientMessageEvent_l_star =
  BEGIN
    RETURN LOOPHOLE (ADR (ev), X.XClientMessageEvent_l_star);
  END ClientMessageEvent;


(*****************************************************************************)
(* Animation Server                                                          *)
(*****************************************************************************)


PROCEDURE Setup (self: T) =
  BEGIN
    <* ASSERT AnimServer.IsServer() *>
    WITH status = GLX.glXMakeCurrent (conn.dpy, self.window, self.context) DO
      <* ASSERT status = X.True *>
    END;

    (*** Clear the color and the depth buffer ***)
    GL.glClear (Word.Or (GL.GL_COLOR_BUFFER_BIT, GL.GL_DEPTH_BUFFER_BIT));
  END Setup;


PROCEDURE Repair (self : T; VAR damaged : BOOLEAN) =
  BEGIN
    (*** Redraw the scene only if there is one and it was damaged ***)
    IF self.root # NIL AND self.root.damaged THEN
      damaged := TRUE;

      LOCK conn DO
        (*** determine the object's current transparency ***)
        self.transflag := self.root.needsTransparency(0.0);
                                   (* 0.0 is the default transmission coeff *)

        (*** set up the rendering pipeline for a new round ***)
        Setup (self);

        (*** reset the bounding volume and the light state ***)
        self.resetBoundingVolume();

        (*** switch off all GL lights ***)
        FOR i := 0 TO GL.GL_MAX_LIGHTS - 1 DO
          GL.glDisable (GL.GL_LIGHT0 + i);
        END;

        (*** reset "self.lightCount" and "self.ambientLight" ***)
        self.lightCount   := 0;
        self.ambientLight := GLrgba {0.0, 0.0, 0.0, 1.0};

        (*** Put all light sources into a display list. As a side effect,
             determine the relevant parameters of the current camera. ***)
        GL.glNewList (self.lightList, GL.GL_COMPILE);
        self.phase := 1;
        self.root.draw (self);
        GL.glEndList ();

        (*** Now set up the camera ***)
        SetupCamera (self);

        (*** Switch on the light sources by executing the display list ***)
        GL.glLightModelfv (GL.GL_LIGHT_MODEL_AMBIENT, ADR (self.ambientLight));
        GL.glCallList (self.lightList);

        (*** Then draw everything else ***)
        self.phase := 2;
        self.root.draw (self);

        (*** Finally, swap the buffers to update the display ***)
        GLX.glXSwapBuffers (conn.dpy, self.window);
        X.XSync (conn.dpy, X.False);
      END;
    END;
  END Repair;


(*****************************************************************************)
(* Connection Management                                                     *)
(*****************************************************************************)


TYPE
  Connection = MUTEX OBJECT             (* mutex synchronizes access to dpy *)
    dpy              : X.DisplayStar;
    avail            : BOOLEAN;
    wm_protocols     : X.Atom;
    wm_delete_window : X.Atom;
  METHODS
    init (): Connection := InitConnection;
  END;


VAR conn: Connection := NIL;


PROCEDURE InitConnection (self : Connection) : Connection =
  VAR
    errBase : Ctypes.int;
    evtBase : Ctypes.int;
    cstr1   : Ctypes.char_star;
    cstr2   : Ctypes.char_star;
  BEGIN
    (*** open the display ***)

    self.dpy := X.XOpenDisplay (NIL);
    IF self.dpy = NIL THEN
      Process.Crash ("Could not open display");
    END;

    (*** "internalize" some X atoms ***)

    cstr1 := M3toC.CopyTtoS ("WM_PROTOCOLS");
    cstr2 := M3toC.CopyTtoS ("WM_DELETE_WINDOW");
    self.wm_protocols :=
       X.XInternAtom(self.dpy, cstr1, X.False);
    self.wm_delete_window :=
       X.XInternAtom(self.dpy, cstr2, X.False);
    M3toC.FreeCopiedS(cstr1);
    M3toC.FreeCopiedS(cstr2);
    (* Check whether the GL extension is available on the server *)
    WITH res = GLX.glXQueryExtension (self.dpy, ADR(errBase), ADR(evtBase)) DO
      self.avail := res = X.True;
    END;

    RETURN self;
  END InitConnection;


BEGIN
END X_OpenGL_Base.
