(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Aug 27 16:15:36 PDT 1996 by najork                   *)
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


UNSAFE MODULE Win_OpenGL_Base EXPORTS Win_OpenGL_Base, Win_OpenGL_BaseProxy;

IMPORT AuxG, AnimServer, Color, ColorPropPrivate, Ctypes, GL, GLu, GO, 
       GOPrivate, GraphicsBase, GraphicsBasePrivate, IntIntTbl, IntRefTbl, 
       KeyCB, KeyboardKey, Latin1Key, LineTypeProp, M3toC, MarkerGO, 
       MarkerTypeProp, MarkerTypePropPrivate, Math, Matrix4, MouseCB, Mth, 
       ParseParams, Point, Point3, PositionCB, PropPrivate, RTLinker, 
       RasterModeProp, RealPropPrivate, RootGOPrivate, ShadingProp, Stdio, 
       SurfaceGO, Thread, VBT, WinDef, WinGDI, WinUser, Word;

IMPORT IO, Fmt;

REVEAL
  T = Public BRANDED OBJECT
    hwnd                : WinDef.HWND;
    hdc                 : WinDef.HDC;   (* a private device context! *)
    hglrc               : WinDef.HGLRC;
    title               : TEXT;
    origin              : Point.T;      (* NW corner of the window *)
    dimen               : Point.T;      (* width and height of drawing area *)
    eventQueue          : EventQueue;
    windowThreadCV      : Thread.Condition;

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
  False = 0;  <*NOWARN*>
  True  = 1;

CONST 
  Solid   = 2_1111111111111111;
  Dashed  = 2_1111000011110000;
  Dotted  = 2_1010101010101010;
  DashDot = 2_1110010011100100;


PROCEDURE Init (self: T; title: TEXT; x, y, w, h: INTEGER): T 
    RAISES {GraphicsBase.Failure} =
  VAR
  BEGIN
    (*** Initialize windowThreadCV ***)
    self.windowThreadCV := NEW (Thread.Condition);

    (*** Initialize awaitDeleteCV ***)
    self.awaitDeleteMu := NEW (Thread.Mutex);
    self.awaitDeleteCV := NEW (Thread.Condition);

    (*** Initialize the display list table ***)
    self.dlTable := NEW (IntIntTbl.Default).init ();

    self.stacks := PropPrivate.NewStacks ();
    self.stateSize := NUMBER (self.stacks^);

    (* The rest of this function is copied straight from X_PEX_Base. *)

    self.modifiers := VBT.Modifiers {};
    self.buttonDownCount := 0;

    self.status := GraphicsBasePrivate.Status.Mapped;

    self.winWidth  := w;
    self.winHeight := h;

    (* Initialize the state variables *)
    self.setSpecularReflConc (
        SurfaceGO.SpecularReflectionConc.getState (self));
         
    self.setMarkerColor (MarkerGO.Colour.getState (self));
    self.setMarkerScale (MarkerGO.Scale.getState (self));
    self.setMarkerType  (MarkerGO.Type.getState (self));

    (* save title, position, and dimensions of the window to be created *)
    self.title := title;
    self.origin := Point.T {x, y};
    self.dimen  := Point.T {w, h};

    (* Set hwnd, hdc, hglrc to NIL *)
    self.hwnd := NIL;
    self.hdc := NIL;
    self.hglrc := NIL;

    (* Create an event queue for buffering Windows messages *)
    self.eventQueue := NEW (EventQueue).init ();

    IF MkProxyT # NIL THEN
      MkProxyT (self);
    END;

    RETURN self;
  END Init;



(*****************************************************************************)
(* The following procedures are copied pretty much directly from X_PEX_Base  *)
(*****************************************************************************)


PROCEDURE ChangeTitle (self: T; title : TEXT) =
  VAR
    status : WinDef.BOOL;
  BEGIN
    LOCK conn DO
      status := WinUser.SetWindowText (self.hwnd, M3toC.TtoS (title));
      <* ASSERT status = True *>
    END;
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
  VAR
    status : WinDef.BOOL;
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    (* Delete the OpenGL rendering context.  Since the device context is
       "private", there is no need to release or delete it. *)
    status := WinGDI.wglDeleteContext (self.hglrc);
    <* ASSERT status = True *>

    (* Windows can be destroyed only by the thread that created them. 
       So, ask the "window thread" to destroy "self.hwnd". *)
    EVAL WinUser.SendMessage(self.hwnd, WM_INITIATE_DESTROY, 0, 0);

    self.status := GraphicsBasePrivate.Status.Unmapped;

    (*** signal all threads that are blocked ***)
    Thread.Broadcast (self.awaitDeleteCV);
  END Unmap;


PROCEDURE Available () : BOOLEAN =
  BEGIN
    (* This procedure is supposed to determine whether OpenGL is available. 
       This is straightforward under X (use "glXQueryExtension"), but it's not
       clear how it should be done under Windows.  The Microsoft documentation
       suggests to use "GetVersion", but does not say which versions of Windows
       support OpenGL.  I assume that OpenGL is supported if OPENGL32.DLL is 
       around. If OPENGL32.DLL is not around, the application will fail upon 
       startup. So, I simply cross my fingers and return TRUE. *)

    RETURN TRUE;
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
        GL.glLightf (l, GL.GL_SPOT_CUTOFF, 180.0 * spread / Math.Pi);

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
        GLu.gluPerspective (FLOAT (self.fovy / Math.Pi * 180.0, LONGREAL), 
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
  BEGIN
    self.eventQueue.drain (self);
  END ProcessEvents;


(*****************************************************************************)
(* The "window thread".  There is one such thread per window / graphics base.*)
(* Responsible for creating a window, and for moving messages from the       *)
(* Windows message queue to a user-level event queue. The actual processing  *)
(* of the buffered events is then done by the animation server thread.       *)
(*****************************************************************************)


TYPE
  Closure = Thread.Closure OBJECT
    base: T;
  OVERRIDES
    apply := Apply;
  END;


PROCEDURE Apply (self: Closure): REFANY =
  VAR
    base  := self.base;
    status: WinDef.BOOL;
    cs    : WinUser.CREATESTRUCT;
    pf    : Ctypes.int;
    pfd   : WinGDI.PIXELFORMATDESCRIPTOR;
    msg   : WinUser.MSG;
  BEGIN
    LOCK conn DO
      (* Create a window *)
      conn.currBase := base;
      base.hwnd := WinUser.CreateWindow (
                       conn.windowclassName,
                       M3toC.CopyTtoS (base.title),
                       WinUser.WS_OVERLAPPEDWINDOW 
                         + WinUser.WS_CLIPSIBLINGS  
                         + WinUser.WS_CLIPCHILDREN,
                       base.origin.h, 
                       base.origin.v,
                       base.dimen.h + conn.nonclient.h, 
                       base.dimen.v + conn.nonclient.v,
                       NIL, NIL, conn.hInst,
                       ADR (cs));
      <* ASSERT base.hwnd # NIL *>

      (* add the window handle to the map from handles to bases, and reset
         the "currBase" field. *)
      EVAL conn.hwndMap.put (LOOPHOLE (base.hwnd, INTEGER), base);
      conn.currBase := NIL;

      (* map the window *)
      EVAL WinUser.ShowWindow (base.hwnd, WinUser.SW_SHOWDEFAULT);
            
      (* update the window (repaint its client area) *)
      status := WinUser.UpdateWindow (base.hwnd);
      <* ASSERT status = True *>

      (* Cache the device context in "base.hdc". Note that we can do this only 
         because we declared the device context to be private ("CS_OWNDC"). *)
      base.hdc := WinUser.GetDC (base.hwnd);
      <* ASSERT base.hdc # NIL *>

(*
      (* As a test, dump out the supported pixel formats *)
      DumpPixelFormats (base.hdc);
*)

      (* Choose the best pixel format.  This is the Windows equivalent of
         choosing the best visual in X. *)
      pfd.nSize        := BYTESIZE (WinGDI.PIXELFORMATDESCRIPTOR);
      pfd.nVersion     := 1;                     (* must be 1 *)
      pfd.dwFlags      := WinGDI.PFD_DRAW_TO_WINDOW +
                          WinGDI.PFD_SUPPORT_OPENGL +
                          WinGDI.PFD_DOUBLEBUFFER +
                          WinGDI.PFD_STEREO_DONTCARE;
      pfd.iPixelType   := WinGDI.PFD_TYPE_RGBA;  (* RGB vs index color *)
      pfd.cColorBits   := 24;
      pfd.cAlphaBits   := 8;
      pfd.cAccumBits   := 0;                     (* don't need Accum buffer  *)
      pfd.cDepthBits   := 32;
      pfd.cStencilBits := 1;                     (* need 1-bit stencil buffer*)
      pfd.cAuxBuffers  := 0;                     (* don't need aux. buffers  *)
      pfd.iLayerType   := WinGDI.PFD_MAIN_PLANE; (* only supported value ... *)
  
      pf := WinGDI.ChoosePixelFormat (base.hdc, ADR (pfd));
      <* ASSERT pf > 0 *>
(*
      IO.Put ("ChoosePixelformat suggests format " & Fmt.Int (pf) & "\n");
*)

(***
   1: white sphere, red and blue shadows, regular repaints (no DB)
   2: white sphere, red and blue shadows, regular repaints (no DB)
   3: white sphere, red and blue shadows
   4: white sphere, red and blue shadows
   5: red sphere, regular repaints (no DB)
   6: red sphere, regular repaints (no DB)
   7: red sphere
   8: red sphere
   9: breaks
  10: breaks
  11: breaks
  12: breaks
  13: breaks
  14:
  15:
  16:
  17:
  18:
  19:
  20:
  21:
  22:
  23:
  24: breaks
***)
      status := WinGDI.SetPixelFormat (base.hdc, pf, ADR (pfd));
      <* ASSERT status = True *>

      (* Create an OpenGL rendering context, and make it current. *)
      base.hglrc := WinGDI.wglCreateContext (base.hdc);
      <* ASSERT base.hglrc # NIL *>

      status := WinGDI.wglMakeCurrent (base.hdc, base.hglrc);
      <* ASSERT status = True *>

(*****
      (* Create a color map *)
      cmap := X.XCreateColormap (dpy, X.XRootWindow (dpy, visual.screen),
                                 visual.visual, X.AllocNone);
****)

      (*** Determine the default frame buffer ***)
      GL.glGetIntegerv (GL.GL_DRAW_BUFFER, ADR (base.drawBuffer));

      (*** Enable depth buffering and set the depth buffer clear value ***)
      GL.glEnable (GL.GL_DEPTH_TEST);
      GL.glDepthFunc (GL.GL_GREATER);
      GL.glClearDepth (0.0d0);

      (*** Create the display list for light sources ***)
      GL.glEnable (GL.GL_LIGHTING);
      GL.glLightModeli(GL.GL_LIGHT_MODEL_TWO_SIDE, GL.GL_TRUE);
      base.lightList := GL.glGenLists (1);
      IF base.lightList = 0 THEN
        RAISE GraphicsBase.Failure;
      END;
            
      (* Select flat shading and auto-normalization of normal vectors *)
      GL.glShadeModel (GL.GL_FLAT);
      GL.glEnable (GL.GL_NORMALIZE);

      GL.glEnable (GL.GL_LINE_STIPPLE);
      GL.glLineStipple (1, Solid);

      (* In Windows, an OpenGL rendering context can be current to at most
         one thread at a time.  So, this thread (the window thread) must 
         release "base.hglrc" for the animation server thread to make it 
         current. *)
      status := WinGDI.wglMakeCurrent (base.hdc, NIL);
      <* ASSERT status = True *>

      WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO
        IF pp.keywordPresent("-largeCursor") THEN
          (*** LargeCursor(base); ***) (*** For now, We don't deal with it ***)
        END;
      END;

      (* Signal the animation server thread that window creation is complete *)
      Thread.Signal (base.windowThreadCV);
    END; (* release conn *)

    (* start the message loop for this window / graphics base *)
    WHILE WinUser.GetMessage (ADR (msg), NIL, 0, 0) = True DO
      EVAL WinUser.TranslateMessage (ADR (msg));
      EVAL WinUser.DispatchMessage (ADR (msg));
    END;

    (* terminate the thread *)
    RETURN NIL;
  END Apply;


<*UNUSED*>
PROCEDURE DumpPixelFormats (hdc: WinDef.HDC) =
  VAR
    pfd   : WinGDI.PIXELFORMATDESCRIPTOR;
    n     : Ctypes.int;
    status: Ctypes.int;
  BEGIN
    n := WinGDI.DescribePixelFormat (hdc, 1, BYTESIZE (pfd), ADR (pfd));
    <* ASSERT n > 0 *>

    FOR i := 1 TO n DO
      status := WinGDI.DescribePixelFormat (hdc, i, BYTESIZE (pfd), ADR (pfd));
      <* ASSERT status # 0 *>
        IO.Put ("PF " & Fmt.Int (i) & "\n"); 
        IO.Put ("  flags : " & Fmt.Int (pfd.dwFlags) & " (");
        WITH f = pfd.dwFlags DO
          IF Word.And (f, WinGDI.PFD_DOUBLEBUFFER) # 0 THEN
            IO.Put (" DOUBLEBUFFER ");
          END;
          IF Word.And (f, WinGDI.PFD_STEREO) # 0 THEN
            IO.Put (" STEREO ");
          END;
          IF Word.And (f, WinGDI.PFD_DRAW_TO_WINDOW) # 0 THEN
            IO.Put (" DRAW_TO_WINDOW ");
          END;
          IF Word.And (f, WinGDI.PFD_DRAW_TO_BITMAP) # 0 THEN
            IO.Put (" DRAW_TO_BITMAP ");
          END;
          IF Word.And (f, WinGDI.PFD_SUPPORT_GDI) # 0 THEN
            IO.Put (" SUPPORT_GDI ");
          END;
          IF Word.And (f, WinGDI.PFD_SUPPORT_OPENGL) # 0 THEN
            IO.Put (" SUPPORT_OPENGL ");
          END;
          IF Word.And (f, WinGDI.PFD_GENERIC_FORMAT) # 0 THEN
            IO.Put (" GENERIC_FORMAT ");
          END;
          IF Word.And (f, WinGDI.PFD_NEED_PALETTE) # 0 THEN
            IO.Put (" NEED_PALETTE ");
          END;
          IF Word.And (f, WinGDI.PFD_NEED_SYSTEM_PALETTE) # 0 THEN
            IO.Put (" NEED_SYSTEM_PALETTE ");
          END;
        END;
        IO.Put (")\n");
        IO.Put ("  type  : ");
        IF pfd.iPixelType = WinGDI.PFD_TYPE_RGBA THEN
          IO.Put ("RGBA");
        ELSIF pfd.iPixelType = WinGDI.PFD_TYPE_COLORINDEX THEN
          IO.Put ("color index");
        ELSE
          IO.Put ("unknown");
        END;
        IO.Put ("\n");
        IO.Put ("  color bits   : " & Fmt.Int (pfd.cColorBits) & "\n"); 
        IO.Put ("  alpha bits   : " & Fmt.Int (pfd.cAlphaBits) & "\n"); 
        IO.Put ("  depth bits   : " & Fmt.Int (pfd.cDepthBits) & "\n"); 
        IO.Put ("  stencil bits : " & Fmt.Int (pfd.cStencilBits) & "\n"); 
    END;
  END DumpPixelFormats;


(*****************************************************************************)
(* Transfer from Windows message queue to user-level event queue             *)
(*****************************************************************************)

PROCEDURE GetBase (hwnd: WinDef.HWND): T =
    (* Find the graphics base that correspend to the window handle. Normally,
       this is done by looking up the handle in the table "conn.hwndMap".
       However, the handle can be  entered into the table only after it is
       returned by "CreateWindow".  So, if the window is currently being 
       created, the handle will not be found.  In this case, we use the 
       base that is cached in "conn.currBase". *)
  VAR
    ref: REFANY;
  BEGIN
    IF conn.hwndMap.get (LOOPHOLE (hwnd, INTEGER), 
                         LOOPHOLE (ref, REFANY)) THEN
      RETURN ref;
    ELSIF conn.currBase # NIL THEN
      RETURN conn.currBase;
    ELSE
      <* ASSERT FALSE *>
    END;
  END GetBase;


CONST
  WM_INITIATE_DESTROY = WinUser.WM_USER;


<*CALLBACK*> PROCEDURE WindowProc (hwnd   : WinDef.HWND;
                                   message: WinDef.UINT; 
                                   wParam : WinDef.WPARAM; 
                                   lParam : WinDef.LPARAM  ): WinDef.LRESULT =
  BEGIN
    CASE message OF
    | WM_INITIATE_DESTROY =>
      VAR
        status: WinDef.BOOL;
      BEGIN
        WITH base = GetBase (hwnd) DO
          status := WinUser.DestroyWindow (base.hwnd);
          <* ASSERT status = True *>
        END;
      END;
      RETURN 0;
    | WinUser.WM_PAINT =>
      WITH base = GetBase (hwnd) DO
        base.eventQueue.put (NEW (ExposeEvent)); 
      END;
      RETURN 0;
    | WinUser.WM_CLOSE =>
      WITH base = GetBase (hwnd) DO
        base.eventQueue.put (NEW (DestroyEvent)); 
      END;
      RETURN 0;
    | WinUser.WM_SIZE =>
      WITH base = GetBase (hwnd),
           w = WinDef.LOWORD (lParam),
           h = WinDef.HIWORD (lParam),
           e = NEW (ReshapeEvent, width := w, height := h) DO
        base.eventQueue.put (e); 
      END;
      RETURN 0;
    | WinUser.WM_KEYDOWN =>
      WITH base = GetBase (hwnd),
           key  = VirtualKeyToKeySym (wParam), 
           e = NEW (KeyEvent, key := key, down := TRUE) DO
        base.eventQueue.put (e); 
      END;
      RETURN 0;
    | WinUser.WM_KEYUP =>
      WITH base = GetBase (hwnd),
           key  = VirtualKeyToKeySym (wParam), 
           e = NEW (KeyEvent, key := key, down := FALSE) DO
        base.eventQueue.put (e); 
      END;
      RETURN 0;
    | WinUser.WM_LBUTTONDOWN, WinUser.WM_MBUTTONDOWN, WinUser.WM_RBUTTONDOWN =>
      VAR 
        button: VBT.Button;
      BEGIN 
        CASE message OF
        | WinUser.WM_LBUTTONDOWN => button := VBT.Modifier.MouseL;
        | WinUser.WM_MBUTTONDOWN => button := VBT.Modifier.MouseM;
        | WinUser.WM_RBUTTONDOWN => button := VBT.Modifier.MouseR;
        ELSE
        END;
        WITH base = GetBase (hwnd),
             pos  = Point.T {WinDef.LOWORD (lParam), WinDef.HIWORD (lParam)},
             e    = NEW (ButtonDownEvent, button := button, pos := pos) DO
           base.eventQueue.put (e); 
        END; 
      END;
      RETURN 0;
    | WinUser.WM_LBUTTONUP, WinUser.WM_MBUTTONUP, WinUser.WM_RBUTTONUP =>
      VAR 
        button: VBT.Button;
      BEGIN 
        CASE message OF
        | WinUser.WM_LBUTTONUP => button := VBT.Modifier.MouseL;
        | WinUser.WM_MBUTTONUP => button := VBT.Modifier.MouseM;
        | WinUser.WM_RBUTTONUP => button := VBT.Modifier.MouseR;
        ELSE
        END;
        WITH base = GetBase (hwnd),
             pos  = Point.T {WinDef.LOWORD (lParam), WinDef.HIWORD (lParam)},
             e    = NEW (ButtonUpEvent, button := button, pos := pos) DO
           base.eventQueue.put (e); 
        END; 
      END;
      RETURN 0;
    | WinUser.WM_MOUSEMOVE =>
      WITH base = GetBase (hwnd),
           pos  = Point.T {WinDef.LOWORD (lParam), WinDef.HIWORD (lParam)},
           e = NEW (MotionEvent, pos := pos) DO
        base.eventQueue.put (e); 
      END;
      RETURN 0;
    ELSE
      RETURN WinUser.DefWindowProc (hwnd, message, wParam, lParam);
    END;
  END WindowProc;


PROCEDURE VirtualKeyToKeySym (vk: [0 .. 255]): VBT.KeySym =
  VAR
    shifted := Word.And (WinUser.GetKeyState (WinUser.VK_SHIFT), 16_8000) # 0;
  BEGIN
    IF NOT shifted THEN
      CASE vk OF
      | (* 01 *) WinUser.VK_LBUTTON  => RETURN KeyboardKey.VoidSymbol;
      | (* 02 *) WinUser.VK_RBUTTON  => RETURN KeyboardKey.VoidSymbol;
      | (* 03 *) WinUser.VK_CANCEL   => RETURN KeyboardKey.Cancel;
      | (* 04 *) WinUser.VK_MBUTTON  => RETURN KeyboardKey.VoidSymbol;
      | (* 08 *) WinUser.VK_BACK     => RETURN KeyboardKey.BackSpace;
      | (* 09 *) WinUser.VK_TAB      => RETURN KeyboardKey.Tab;
      | (* 0C *) WinUser.VK_CLEAR    => RETURN KeyboardKey.Clear;
      | (* 0D *) WinUser.VK_RETURN   => RETURN KeyboardKey.Return;

      | (* 10 *) WinUser.VK_SHIFT    => RETURN KeyboardKey.Shift_L;   (* simplification *)
      | (* 11 *) WinUser.VK_CONTROL  => RETURN KeyboardKey.Control_L; (* simplification *)
      | (* 12 *) WinUser.VK_MENU     => RETURN KeyboardKey.Menu;
      | (* 13 *) WinUser.VK_PAUSE    => RETURN KeyboardKey.Pause;
      | (* 14 *) WinUser.VK_CAPITAL  => RETURN KeyboardKey.Caps_Lock;
      | (* 1B *) WinUser.VK_ESCAPE   => RETURN KeyboardKey.Escape;

      | (* 20 *) WinUser.VK_SPACE    => RETURN Latin1Key.space;
      | (* 21 *) WinUser.VK_PRIOR    => RETURN KeyboardKey.Prior;
      | (* 22 *) WinUser.VK_NEXT     => RETURN KeyboardKey.Next;
      | (* 23 *) WinUser.VK_END      => RETURN KeyboardKey.End;
      | (* 24 *) WinUser.VK_HOME     => RETURN KeyboardKey.Home;
      | (* 25 *) WinUser.VK_LEFT     => RETURN KeyboardKey.Left;
      | (* 26 *) WinUser.VK_UP       => RETURN KeyboardKey.Up;
      | (* 27 *) WinUser.VK_RIGHT    => RETURN KeyboardKey.Right;
      | (* 28 *) WinUser.VK_DOWN     => RETURN KeyboardKey.Down;
      | (* 29 *) WinUser.VK_SELECT   => RETURN KeyboardKey.Select;
      | (* 2A *) WinUser.VK_PRINT    => RETURN KeyboardKey.Print;
      | (* 2B *) WinUser.VK_EXECUTE  => RETURN KeyboardKey.Execute;
      | (* 2C *) WinUser.VK_SNAPSHOT => RETURN KeyboardKey.VoidSymbol;
      | (* 2D *) WinUser.VK_INSERT   => RETURN KeyboardKey.Insert;
      | (* 2E *) WinUser.VK_DELETE   => RETURN KeyboardKey.Delete;
      | (* 2F *) WinUser.VK_HELP     => RETURN KeyboardKey.Help;

      | 16_30 => RETURN Latin1Key.zero;
      | 16_31 => RETURN Latin1Key.one;
      | 16_32 => RETURN Latin1Key.two;
      | 16_33 => RETURN Latin1Key.three;
      | 16_34 => RETURN Latin1Key.four;
      | 16_35 => RETURN Latin1Key.five;
      | 16_36 => RETURN Latin1Key.six;
      | 16_37 => RETURN Latin1Key.seven;
      | 16_38 => RETURN Latin1Key.eight;
      | 16_39 => RETURN Latin1Key.nine;

      | 16_41 => RETURN Latin1Key.a;
      | 16_42 => RETURN Latin1Key.b;
      | 16_43 => RETURN Latin1Key.c;
      | 16_44 => RETURN Latin1Key.d;
      | 16_45 => RETURN Latin1Key.e;
      | 16_46 => RETURN Latin1Key.f;
      | 16_47 => RETURN Latin1Key.g;
      | 16_48 => RETURN Latin1Key.h;
      | 16_49 => RETURN Latin1Key.i;
      | 16_4A => RETURN Latin1Key.j;
      | 16_4B => RETURN Latin1Key.k;
      | 16_4C => RETURN Latin1Key.l;
      | 16_4D => RETURN Latin1Key.m;
      | 16_4E => RETURN Latin1Key.n;
      | 16_4F => RETURN Latin1Key.o;
      | 16_50 => RETURN Latin1Key.p;
      | 16_51 => RETURN Latin1Key.q;
      | 16_52 => RETURN Latin1Key.r;
      | 16_53 => RETURN Latin1Key.s;
      | 16_54 => RETURN Latin1Key.t;
      | 16_55 => RETURN Latin1Key.u;
      | 16_56 => RETURN Latin1Key.v;
      | 16_57 => RETURN Latin1Key.w;
      | 16_58 => RETURN Latin1Key.x;
      | 16_59 => RETURN Latin1Key.y;
      | 16_5A => RETURN Latin1Key.z;

      | (* 60 *) WinUser.VK_NUMPAD0   => RETURN KeyboardKey.KP_0;
      | (* 61 *) WinUser.VK_NUMPAD1   => RETURN KeyboardKey.KP_1;
      | (* 62 *) WinUser.VK_NUMPAD2   => RETURN KeyboardKey.KP_2;
      | (* 63 *) WinUser.VK_NUMPAD3   => RETURN KeyboardKey.KP_3;
      | (* 64 *) WinUser.VK_NUMPAD4   => RETURN KeyboardKey.KP_4;
      | (* 65 *) WinUser.VK_NUMPAD5   => RETURN KeyboardKey.KP_5;
      | (* 66 *) WinUser.VK_NUMPAD6   => RETURN KeyboardKey.KP_6;
      | (* 67 *) WinUser.VK_NUMPAD7   => RETURN KeyboardKey.KP_7;
      | (* 68 *) WinUser.VK_NUMPAD8   => RETURN KeyboardKey.KP_8;
      | (* 69 *) WinUser.VK_NUMPAD9   => RETURN KeyboardKey.KP_9;
      | (* 6A *) WinUser.VK_MULTIPLY  => RETURN KeyboardKey.KP_Multiply;
      | (* 6B *) WinUser.VK_ADD       => RETURN KeyboardKey.KP_Add;
      | (* 6C *) WinUser.VK_SEPARATOR => RETURN KeyboardKey.KP_Separator;
      | (* 6D *) WinUser.VK_SUBTRACT  => RETURN KeyboardKey.KP_Subtract;
      | (* 6E *) WinUser.VK_DECIMAL   => RETURN KeyboardKey.KP_Decimal;
      | (* 6F *) WinUser.VK_DIVIDE    => RETURN KeyboardKey.KP_Divide;
        
      | (* 70 *) WinUser.VK_F1        => RETURN KeyboardKey.F1;
      | (* 71 *) WinUser.VK_F2        => RETURN KeyboardKey.F2;
      | (* 72 *) WinUser.VK_F3        => RETURN KeyboardKey.F3;
      | (* 73 *) WinUser.VK_F4        => RETURN KeyboardKey.F4;
      | (* 74 *) WinUser.VK_F5        => RETURN KeyboardKey.F5;
      | (* 75 *) WinUser.VK_F6        => RETURN KeyboardKey.F6;
      | (* 76 *) WinUser.VK_F7        => RETURN KeyboardKey.F7;
      | (* 77 *) WinUser.VK_F8        => RETURN KeyboardKey.F8;
      | (* 78 *) WinUser.VK_F9        => RETURN KeyboardKey.F9;
      | (* 79 *) WinUser.VK_F10       => RETURN KeyboardKey.F10;
      | (* 7A *) WinUser.VK_F11       => RETURN KeyboardKey.F11;
      | (* 7B *) WinUser.VK_F12       => RETURN KeyboardKey.F12;
      | (* 7C *) WinUser.VK_F13       => RETURN KeyboardKey.F13;
      | (* 7D *) WinUser.VK_F14       => RETURN KeyboardKey.F14;
      | (* 7E *) WinUser.VK_F15       => RETURN KeyboardKey.F15;
      | (* 7F *) WinUser.VK_F16       => RETURN KeyboardKey.F16;
      | (* 80 *) WinUser.VK_F17       => RETURN KeyboardKey.F17;
      | (* 81 *) WinUser.VK_F18       => RETURN KeyboardKey.F18;
      | (* 82 *) WinUser.VK_F19       => RETURN KeyboardKey.F19;
      | (* 83 *) WinUser.VK_F20       => RETURN KeyboardKey.F20;
      | (* 84 *) WinUser.VK_F21       => RETURN KeyboardKey.F21;
      | (* 85 *) WinUser.VK_F22       => RETURN KeyboardKey.F22;
      | (* 86 *) WinUser.VK_F23       => RETURN KeyboardKey.F23;
      | (* 87 *) WinUser.VK_F24       => RETURN KeyboardKey.F24;

      | (* 90 *) WinUser.VK_NUMLOCK   => RETURN KeyboardKey.Num_Lock;
      | (* 91 *) WinUser.VK_SCROLL    => RETURN KeyboardKey.Scroll_Lock;

      | (* A0 *) WinUser.VK_LSHIFT    => RETURN KeyboardKey.Shift_L;
      | (* A1 *) WinUser.VK_RSHIFT    => RETURN KeyboardKey.Shift_R;
      | (* A2 *) WinUser.VK_LCONTROL  => RETURN KeyboardKey.Control_L;
      | (* A3 *) WinUser.VK_RCONTROL  => RETURN KeyboardKey.Control_R;
      | (* A4 *) WinUser.VK_LMENU     => RETURN KeyboardKey.Alt_L;
      | (* A5 *) WinUser.VK_RMENU     => RETURN KeyboardKey.Alt_R;

      (* The next few codes are device-specific ... *)

      | 16_BA => RETURN Latin1Key.semicolon;
      | 16_BB => RETURN Latin1Key.equal;
      | 16_BC => RETURN Latin1Key.comma;
      | 16_BD => RETURN Latin1Key.minus;
      | 16_BE => RETURN Latin1Key.period;
      | 16_BF => RETURN Latin1Key.slash;
      | 16_C0 => RETURN Latin1Key.grave;
      | 16_DB => RETURN Latin1Key.bracketleft;
      | 16_DC => RETURN Latin1Key.backslash;
      | 16_DD => RETURN Latin1Key.bracketright;
      | 16_DE => RETURN Latin1Key.apostrophe;

      | (* F6 *) WinUser.VK_ATTN      => RETURN KeyboardKey.VoidSymbol;
      | (* F7 *) WinUser.VK_CRSEL     => RETURN KeyboardKey.VoidSymbol;
      | (* F8 *) WinUser.VK_EXSEL     => RETURN KeyboardKey.VoidSymbol;
      | (* F9 *) WinUser.VK_EREOF     => RETURN KeyboardKey.VoidSymbol;
      | (* FA *) WinUser.VK_PLAY      => RETURN KeyboardKey.VoidSymbol;
      | (* FB *) WinUser.VK_ZOOM      => RETURN KeyboardKey.VoidSymbol;
      | (* FC *) WinUser.VK_NONAME    => RETURN KeyboardKey.VoidSymbol;
      | (* FD *) WinUser.VK_PA1       => RETURN KeyboardKey.VoidSymbol;
      | (* FE *) WinUser.VK_OEM_CLEAR => RETURN KeyboardKey.VoidSymbol;

      ELSE
        RETURN KeyboardKey.VoidSymbol;
      END;

    ELSE

      CASE vk OF
      | (* 01 *) WinUser.VK_LBUTTON  => RETURN KeyboardKey.VoidSymbol;
      | (* 02 *) WinUser.VK_RBUTTON  => RETURN KeyboardKey.VoidSymbol;
      | (* 03 *) WinUser.VK_CANCEL   => RETURN KeyboardKey.Cancel;
      | (* 04 *) WinUser.VK_MBUTTON  => RETURN KeyboardKey.VoidSymbol;
      | (* 08 *) WinUser.VK_BACK     => RETURN KeyboardKey.BackSpace;
      | (* 09 *) WinUser.VK_TAB      => RETURN KeyboardKey.Tab;
      | (* 0C *) WinUser.VK_CLEAR    => RETURN KeyboardKey.Clear;
      | (* 0D *) WinUser.VK_RETURN   => RETURN KeyboardKey.Return;

      | (* 10 *) WinUser.VK_SHIFT    => RETURN KeyboardKey.Shift_L;   (* simplification *)
      | (* 11 *) WinUser.VK_CONTROL  => RETURN KeyboardKey.Control_L; (* simplification *)
      | (* 12 *) WinUser.VK_MENU     => RETURN KeyboardKey.Menu;
      | (* 13 *) WinUser.VK_PAUSE    => RETURN KeyboardKey.Pause;
      | (* 14 *) WinUser.VK_CAPITAL  => RETURN KeyboardKey.Caps_Lock;
      | (* 1B *) WinUser.VK_ESCAPE   => RETURN KeyboardKey.Escape;

      | (* 20 *) WinUser.VK_SPACE    => RETURN Latin1Key.space;
      | (* 21 *) WinUser.VK_PRIOR    => RETURN KeyboardKey.Prior;
      | (* 22 *) WinUser.VK_NEXT     => RETURN KeyboardKey.Next;
      | (* 23 *) WinUser.VK_END      => RETURN KeyboardKey.End;
      | (* 24 *) WinUser.VK_HOME     => RETURN KeyboardKey.Home;
      | (* 25 *) WinUser.VK_LEFT     => RETURN KeyboardKey.Left;
      | (* 26 *) WinUser.VK_UP       => RETURN KeyboardKey.Up;
      | (* 27 *) WinUser.VK_RIGHT    => RETURN KeyboardKey.Right;
      | (* 28 *) WinUser.VK_DOWN     => RETURN KeyboardKey.Down;
      | (* 29 *) WinUser.VK_SELECT   => RETURN KeyboardKey.Select;
      | (* 2A *) WinUser.VK_PRINT    => RETURN KeyboardKey.Print;
      | (* 2B *) WinUser.VK_EXECUTE  => RETURN KeyboardKey.Execute;
      | (* 2C *) WinUser.VK_SNAPSHOT => RETURN KeyboardKey.VoidSymbol;
      | (* 2D *) WinUser.VK_INSERT   => RETURN KeyboardKey.Insert;
      | (* 2E *) WinUser.VK_DELETE   => RETURN KeyboardKey.Delete;
      | (* 2F *) WinUser.VK_HELP     => RETURN KeyboardKey.Help;

      | 16_30 => RETURN Latin1Key.parenright;
      | 16_31 => RETURN Latin1Key.exclam;
      | 16_32 => RETURN Latin1Key.at;
      | 16_33 => RETURN Latin1Key.numbersign;
      | 16_34 => RETURN Latin1Key.dollar;
      | 16_35 => RETURN Latin1Key.percent;
      | 16_36 => RETURN Latin1Key.asciicircum;
      | 16_37 => RETURN Latin1Key.ampersand;
      | 16_38 => RETURN Latin1Key.asterisk;
      | 16_39 => RETURN Latin1Key.parenleft;

      | 16_41 => RETURN Latin1Key.A;
      | 16_42 => RETURN Latin1Key.B;
      | 16_43 => RETURN Latin1Key.C;
      | 16_44 => RETURN Latin1Key.D;
      | 16_45 => RETURN Latin1Key.E;
      | 16_46 => RETURN Latin1Key.F;
      | 16_47 => RETURN Latin1Key.G;
      | 16_48 => RETURN Latin1Key.H;
      | 16_49 => RETURN Latin1Key.I;
      | 16_4A => RETURN Latin1Key.J;
      | 16_4B => RETURN Latin1Key.K;
      | 16_4C => RETURN Latin1Key.L;
      | 16_4D => RETURN Latin1Key.M;
      | 16_4E => RETURN Latin1Key.N;
      | 16_4F => RETURN Latin1Key.O;
      | 16_50 => RETURN Latin1Key.P;
      | 16_51 => RETURN Latin1Key.Q;
      | 16_52 => RETURN Latin1Key.R;
      | 16_53 => RETURN Latin1Key.S;
      | 16_54 => RETURN Latin1Key.T;
      | 16_55 => RETURN Latin1Key.U;
      | 16_56 => RETURN Latin1Key.V;
      | 16_57 => RETURN Latin1Key.W;
      | 16_58 => RETURN Latin1Key.X;
      | 16_59 => RETURN Latin1Key.Y;
      | 16_5A => RETURN Latin1Key.Z;

      | (* 60 *) WinUser.VK_NUMPAD0   => RETURN KeyboardKey.KP_0;
      | (* 61 *) WinUser.VK_NUMPAD1   => RETURN KeyboardKey.KP_1;
      | (* 62 *) WinUser.VK_NUMPAD2   => RETURN KeyboardKey.KP_2;
      | (* 63 *) WinUser.VK_NUMPAD3   => RETURN KeyboardKey.KP_3;
      | (* 64 *) WinUser.VK_NUMPAD4   => RETURN KeyboardKey.KP_4;
      | (* 65 *) WinUser.VK_NUMPAD5   => RETURN KeyboardKey.KP_5;
      | (* 66 *) WinUser.VK_NUMPAD6   => RETURN KeyboardKey.KP_6;
      | (* 67 *) WinUser.VK_NUMPAD7   => RETURN KeyboardKey.KP_7;
      | (* 68 *) WinUser.VK_NUMPAD8   => RETURN KeyboardKey.KP_8;
      | (* 69 *) WinUser.VK_NUMPAD9   => RETURN KeyboardKey.KP_9;
      | (* 6A *) WinUser.VK_MULTIPLY  => RETURN KeyboardKey.KP_Multiply;
      | (* 6B *) WinUser.VK_ADD       => RETURN KeyboardKey.KP_Add;
      | (* 6C *) WinUser.VK_SEPARATOR => RETURN KeyboardKey.KP_Separator;
      | (* 6D *) WinUser.VK_SUBTRACT  => RETURN KeyboardKey.KP_Subtract;
      | (* 6E *) WinUser.VK_DECIMAL   => RETURN KeyboardKey.KP_Decimal;
      | (* 6F *) WinUser.VK_DIVIDE    => RETURN KeyboardKey.KP_Divide;
        
      | (* 70 *) WinUser.VK_F1        => RETURN KeyboardKey.F1;
      | (* 71 *) WinUser.VK_F2        => RETURN KeyboardKey.F2;
      | (* 72 *) WinUser.VK_F3        => RETURN KeyboardKey.F3;
      | (* 73 *) WinUser.VK_F4        => RETURN KeyboardKey.F4;
      | (* 74 *) WinUser.VK_F5        => RETURN KeyboardKey.F5;
      | (* 75 *) WinUser.VK_F6        => RETURN KeyboardKey.F6;
      | (* 76 *) WinUser.VK_F7        => RETURN KeyboardKey.F7;
      | (* 77 *) WinUser.VK_F8        => RETURN KeyboardKey.F8;
      | (* 78 *) WinUser.VK_F9        => RETURN KeyboardKey.F9;
      | (* 79 *) WinUser.VK_F10       => RETURN KeyboardKey.F10;
      | (* 7A *) WinUser.VK_F11       => RETURN KeyboardKey.F11;
      | (* 7B *) WinUser.VK_F12       => RETURN KeyboardKey.F12;
      | (* 7C *) WinUser.VK_F13       => RETURN KeyboardKey.F13;
      | (* 7D *) WinUser.VK_F14       => RETURN KeyboardKey.F14;
      | (* 7E *) WinUser.VK_F15       => RETURN KeyboardKey.F15;
      | (* 7F *) WinUser.VK_F16       => RETURN KeyboardKey.F16;
      | (* 80 *) WinUser.VK_F17       => RETURN KeyboardKey.F17;
      | (* 81 *) WinUser.VK_F18       => RETURN KeyboardKey.F18;
      | (* 82 *) WinUser.VK_F19       => RETURN KeyboardKey.F19;
      | (* 83 *) WinUser.VK_F20       => RETURN KeyboardKey.F20;
      | (* 84 *) WinUser.VK_F21       => RETURN KeyboardKey.F21;
      | (* 85 *) WinUser.VK_F22       => RETURN KeyboardKey.F22;
      | (* 86 *) WinUser.VK_F23       => RETURN KeyboardKey.F23;
      | (* 87 *) WinUser.VK_F24       => RETURN KeyboardKey.F24;

      | (* 90 *) WinUser.VK_NUMLOCK   => RETURN KeyboardKey.Num_Lock;
      | (* 91 *) WinUser.VK_SCROLL    => RETURN KeyboardKey.Scroll_Lock;

      | (* A0 *) WinUser.VK_LSHIFT    => RETURN KeyboardKey.Shift_L;
      | (* A1 *) WinUser.VK_RSHIFT    => RETURN KeyboardKey.Shift_R;
      | (* A2 *) WinUser.VK_LCONTROL  => RETURN KeyboardKey.Control_L;
      | (* A3 *) WinUser.VK_RCONTROL  => RETURN KeyboardKey.Control_R;
      | (* A4 *) WinUser.VK_LMENU     => RETURN KeyboardKey.Alt_L;
      | (* A5 *) WinUser.VK_RMENU     => RETURN KeyboardKey.Alt_R;

      (* The next few codes are device-specific ... *)

      | 16_BA => RETURN Latin1Key.colon;
      | 16_BB => RETURN Latin1Key.plus;
      | 16_BC => RETURN Latin1Key.less;
      | 16_BD => RETURN Latin1Key.underscore;
      | 16_BE => RETURN Latin1Key.greater;
      | 16_BF => RETURN Latin1Key.question;
      | 16_C0 => RETURN Latin1Key.asciitilde;
      | 16_DB => RETURN Latin1Key.braceleft;
      | 16_DC => RETURN Latin1Key.bar;
      | 16_DD => RETURN Latin1Key.braceright;
      | 16_DE => RETURN Latin1Key.quotedbl;

      | (* F6 *) WinUser.VK_ATTN      => RETURN KeyboardKey.VoidSymbol;
      | (* F7 *) WinUser.VK_CRSEL     => RETURN KeyboardKey.VoidSymbol;
      | (* F8 *) WinUser.VK_EXSEL     => RETURN KeyboardKey.VoidSymbol;
      | (* F9 *) WinUser.VK_EREOF     => RETURN KeyboardKey.VoidSymbol;
      | (* FA *) WinUser.VK_PLAY      => RETURN KeyboardKey.VoidSymbol;
      | (* FB *) WinUser.VK_ZOOM      => RETURN KeyboardKey.VoidSymbol;
      | (* FC *) WinUser.VK_NONAME    => RETURN KeyboardKey.VoidSymbol;
      | (* FD *) WinUser.VK_PA1       => RETURN KeyboardKey.VoidSymbol;
      | (* FE *) WinUser.VK_OEM_CLEAR => RETURN KeyboardKey.VoidSymbol;
      ELSE
        RETURN KeyboardKey.VoidSymbol;
      END;
    END;
  END VirtualKeyToKeySym;


<*UNUSED*>
PROCEDURE PrintMessageType (message: WinDef.UINT) =
  BEGIN
    IO.Put("message " & Fmt.Int(message) & " = ");
    CASE message OF
    | WinUser.WM_NULL => IO.Put("WM_NULL");
    | WinUser.WM_CREATE => IO.Put("WM_CREATE");
    | WinUser.WM_DESTROY => IO.Put("WM_DESTROY");
    | WinUser.WM_MOVE => IO.Put("WM_MOVE");
    | WinUser.WM_SIZE => IO.Put("WM_SIZE");
    | WinUser.WM_ACTIVATE => IO.Put("WM_ACTIVATE");
    | WinUser.WM_SETFOCUS => IO.Put("WM_SETFOCUS");
    | WinUser.WM_KILLFOCUS => IO.Put("WM_KILLFOCUS");
    | WinUser.WM_ENABLE => IO.Put("WM_ENABLE");
    | WinUser.WM_SETREDRAW => IO.Put("WM_SETREDRAW");
    | WinUser.WM_SETTEXT => IO.Put("WM_SETTEXT");
    | WinUser.WM_GETTEXT => IO.Put("WM_GETTEXT");
    | WinUser.WM_GETTEXTLENGTH => IO.Put("WM_GETTEXTLENGTH");
    | WinUser.WM_PAINT => IO.Put("WM_PAINT");
    | WinUser.WM_CLOSE => IO.Put("WM_CLOSE");
    | WinUser.WM_QUERYENDSESSION => IO.Put("WM_QUERYENDSESSION");
    | WinUser.WM_QUIT => IO.Put("WM_QUIT");
    | WinUser.WM_QUERYOPEN => IO.Put("WM_QUERYOPEN");
    | WinUser.WM_ERASEBKGND => IO.Put("WM_ERASEBKGND");
    | WinUser.WM_SYSCOLORCHANGE => IO.Put("WM_SYSCOLORCHANGE");
    | WinUser.WM_ENDSESSION => IO.Put("WM_ENDSESSION");
    | WinUser.WM_SHOWWINDOW => IO.Put("WM_SHOWWINDOW");
    | WinUser.WM_WININICHANGE => IO.Put("WM_WININICHANGE");
    | WinUser.WM_DEVMODECHANGE => IO.Put("WM_DEVMODECHANGE");
    | WinUser.WM_ACTIVATEAPP => IO.Put("WM_ACTIVATEAPP");
    | WinUser.WM_FONTCHANGE => IO.Put("WM_FONTCHANGE");
    | WinUser.WM_TIMECHANGE => IO.Put("WM_TIMECHANGE");
    | WinUser.WM_CANCELMODE => IO.Put("WM_CANCELMODE");
    | WinUser.WM_SETCURSOR => IO.Put("WM_SETCURSOR");
    | WinUser.WM_MOUSEACTIVATE => IO.Put("WM_MOUSEACTIVATE");
    | WinUser.WM_CHILDACTIVATE => IO.Put("WM_CHILDACTIVATE");
    | WinUser.WM_QUEUESYNC => IO.Put("WM_QUEUESYNC");
    | WinUser.WM_GETMINMAXINFO => IO.Put("WM_GETMINMAXINFO");
    | WinUser.WM_PAINTICON => IO.Put("WM_PAINTICON");
    | WinUser.WM_ICONERASEBKGND => IO.Put("WM_ICONERASEBKGND");
    | WinUser.WM_NEXTDLGCTL => IO.Put("WM_NEXTDLGCTL");
    | WinUser.WM_SPOOLERSTATUS => IO.Put("WM_SPOOLERSTATUS");
    | WinUser.WM_DRAWITEM => IO.Put("WM_DRAWITEM");
    | WinUser.WM_MEASUREITEM => IO.Put("WM_MEASUREITEM");
    | WinUser.WM_DELETEITEM => IO.Put("WM_DELETEITEM");
    | WinUser.WM_VKEYTOITEM => IO.Put("WM_VKEYTOITEM");
    | WinUser.WM_CHARTOITEM => IO.Put("WM_CHARTOITEM");
    | WinUser.WM_SETFONT => IO.Put("WM_SETFONT");
    | WinUser.WM_GETFONT => IO.Put("WM_GETFONT");
    | WinUser.WM_SETHOTKEY => IO.Put("WM_SETHOTKEY");
    | WinUser.WM_GETHOTKEY => IO.Put("WM_GETHOTKEY");
    | WinUser.WM_QUERYDRAGICON => IO.Put("WM_QUERYDRAGICON");
    | WinUser.WM_COMPAREITEM => IO.Put("WM_COMPAREITEM");
    | WinUser.WM_FULLSCREEN => IO.Put("WM_FULLSCREEN");
    | WinUser.WM_COMPACTING => IO.Put("WM_COMPACTING");
    | WinUser.WM_OTHERWINDOWCREATED => IO.Put("WM_OTHERWINDOWCREATED");
    | WinUser.WM_OTHERWINDOWDESTROYED => IO.Put("WM_OTHERWINDOWDESTROYED");
    | WinUser.WM_COMMNOTIFY => IO.Put("WM_COMMNOTIFY");
    | WinUser.WM_HOTKEYEVENT => IO.Put("WM_HOTKEYEVENT");
    | WinUser.WM_WINDOWPOSCHANGING => IO.Put("WM_WINDOWPOSCHANGING");
    | WinUser.WM_WINDOWPOSCHANGED => IO.Put("WM_WINDOWPOSCHANGED");
    | WinUser.WM_POWER => IO.Put("WM_POWER");
    | WinUser.WM_COPYDATA => IO.Put("WM_COPYDATA");
    | WinUser.WM_NCCREATE => IO.Put("WM_NCCREATE");
    | WinUser.WM_NCDESTROY => IO.Put("WM_NCDESTROY");
    | WinUser.WM_NCCALCSIZE => IO.Put("WM_NCCALCSIZE");
    | WinUser.WM_NCHITTEST => IO.Put("WM_NCHITTEST");
    | WinUser.WM_NCPAINT => IO.Put("WM_NCPAINT");
    | WinUser.WM_NCACTIVATE => IO.Put("WM_NCACTIVATE");
    | WinUser.WM_GETDLGCODE => IO.Put("WM_GETDLGCODE");
    | WinUser.WM_NCMOUSEMOVE => IO.Put("WM_NCMOUSEMOVE");
    | WinUser.WM_NCLBUTTONDOWN => IO.Put("WM_NCLBUTTONDOWN");
    | WinUser.WM_NCLBUTTONUP => IO.Put("WM_NCLBUTTONUP");
    | WinUser.WM_NCLBUTTONDBLCLK => IO.Put("WM_NCLBUTTONDBLCLK");
    | WinUser.WM_NCRBUTTONDOWN => IO.Put("WM_NCRBUTTONDOWN");
    | WinUser.WM_NCRBUTTONUP => IO.Put("WM_NCRBUTTONUP");
    | WinUser.WM_NCRBUTTONDBLCLK => IO.Put("WM_NCRBUTTONDBLCLK");
    | WinUser.WM_NCMBUTTONDOWN => IO.Put("WM_NCMBUTTONDOWN");
    | WinUser.WM_NCMBUTTONUP => IO.Put("WM_NCMBUTTONUP");
    | WinUser.WM_NCMBUTTONDBLCLK => IO.Put("WM_NCMBUTTONDBLCLK");
    | WinUser.WM_KEYDOWN => IO.Put("WM_KEYDOWN (aka WM_KEYFIRST)");
    | WinUser.WM_KEYUP => IO.Put("WM_KEYUP");
    | WinUser.WM_CHAR => IO.Put("WM_CHAR");
    | WinUser.WM_DEADCHAR => IO.Put("WM_DEADCHAR");
    | WinUser.WM_SYSKEYDOWN => IO.Put("WM_SYSKEYDOWN");
    | WinUser.WM_SYSKEYUP => IO.Put("WM_SYSKEYUP");
    | WinUser.WM_SYSCHAR => IO.Put("WM_SYSCHAR");
    | WinUser.WM_SYSDEADCHAR => IO.Put("WM_SYSDEADCHAR");
    | WinUser.WM_KEYLAST => IO.Put("WM_KEYLAST");
    | WinUser.WM_INITDIALOG => IO.Put("WM_INITDIALOG");
    | WinUser.WM_COMMAND => IO.Put("WM_COMMAND");
    | WinUser.WM_SYSCOMMAND => IO.Put("WM_SYSCOMMAND");
    | WinUser.WM_TIMER => IO.Put("WM_TIMER");
    | WinUser.WM_HSCROLL => IO.Put("WM_HSCROLL");
    | WinUser.WM_VSCROLL => IO.Put("WM_VSCROLL");
    | WinUser.WM_INITMENU => IO.Put("WM_INITMENU");
    | WinUser.WM_INITMENUPOPUP => IO.Put("WM_INITMENUPOPUP");
    | WinUser.WM_MENUSELECT => IO.Put("WM_MENUSELECT");
    | WinUser.WM_MENUCHAR => IO.Put("WM_MENUCHAR");
    | WinUser.WM_ENTERIDLE => IO.Put("WM_ENTERIDLE");
    | WinUser.WM_CTLCOLORMSGBOX => IO.Put("WM_CTLCOLORMSGBOX");
    | WinUser.WM_CTLCOLOREDIT => IO.Put("WM_CTLCOLOREDIT");
    | WinUser.WM_CTLCOLORLISTBOX => IO.Put("WM_CTLCOLORLISTBOX");
    | WinUser.WM_CTLCOLORBTN => IO.Put("WM_CTLCOLORBTN");
    | WinUser.WM_CTLCOLORDLG => IO.Put("WM_CTLCOLORDLG");
    | WinUser.WM_CTLCOLORSCROLLBAR => IO.Put("WM_CTLCOLORSCROLLBAR");
    | WinUser.WM_CTLCOLORSTATIC => IO.Put("WM_CTLCOLORSTATIC");
    | WinUser.WM_MOUSEMOVE => IO.Put("WM_MOUSEMOVE (aka WM_MOUSEFIRST)");
    | WinUser.WM_LBUTTONDOWN => IO.Put("WM_LBUTTONDOWN");
    | WinUser.WM_LBUTTONUP => IO.Put("WM_LBUTTONUP");
    | WinUser.WM_LBUTTONDBLCLK => IO.Put("WM_LBUTTONDBLCLK");
    | WinUser.WM_RBUTTONDOWN => IO.Put("WM_RBUTTONDOWN");
    | WinUser.WM_RBUTTONUP => IO.Put("WM_RBUTTONUP");
    | WinUser.WM_RBUTTONDBLCLK => IO.Put("WM_RBUTTONDBLCLK");
    | WinUser.WM_MBUTTONDOWN => IO.Put("WM_MBUTTONDOWN");
    | WinUser.WM_MBUTTONUP => IO.Put("WM_MBUTTONUP");
    | WinUser.WM_MBUTTONDBLCLK => IO.Put("WM_MBUTTONDBLCLK (aka MOUSELAST)");
    | WinUser.WM_PARENTNOTIFY => IO.Put("WM_PARENTNOTIFY");
    | WinUser.WM_ENTERMENULOOP => IO.Put("WM_ENTERMENULOOP");
    | WinUser.WM_EXITMENULOOP => IO.Put("WM_EXITMENULOOP");
    | WinUser.WM_MDICREATE => IO.Put("WM_MDICREATE");
    | WinUser.WM_MDIDESTROY => IO.Put("WM_MDIDESTROY");
    | WinUser.WM_MDIACTIVATE => IO.Put("WM_MDIACTIVATE");
    | WinUser.WM_MDIRESTORE => IO.Put("WM_MDIRESTORE");
    | WinUser.WM_MDINEXT => IO.Put("WM_MDINEXT");
    | WinUser.WM_MDIMAXIMIZE => IO.Put("WM_MDIMAXIMIZE");
    | WinUser.WM_MDITILE => IO.Put("WM_MDITILE");
    | WinUser.WM_MDICASCADE => IO.Put("WM_MDICASCADE");
    | WinUser.WM_MDIICONARRANGE => IO.Put("WM_MDIICONARRANGE");
    | WinUser.WM_MDIGETACTIVE => IO.Put("WM_MDIGETACTIVE");
    | WinUser.WM_MDISETMENU => IO.Put("WM_MDISETMENU");
    | WinUser.WM_ENTERSIZEMOVE_UNDOCUMENTED => IO.Put("WM_ENTERSIZEMOVE_UNDOCUMENTED");
    | WinUser.WM_EXITSIZEMOVE_UNDOCUMENTED => IO.Put("WM_EXITSIZEMOVE_UNDOCUMENTED");
    | WinUser.WM_DROPFILES => IO.Put("WM_DROPFILES");
    | WinUser.WM_MDIREFRESHMENU => IO.Put("WM_MDIREFRESHMENU");
    | WinUser.WM_CUT => IO.Put("WM_CUT");
    | WinUser.WM_COPY => IO.Put("WM_COPY");
    | WinUser.WM_PASTE => IO.Put("WM_PASTE");
    | WinUser.WM_CLEAR => IO.Put("WM_CLEAR");
    | WinUser.WM_UNDO => IO.Put("WM_UNDO");
    | WinUser.WM_RENDERFORMAT => IO.Put("WM_RENDERFORMAT");
    | WinUser.WM_RENDERALLFORMATS => IO.Put("WM_RENDERALLFORMATS");
    | WinUser.WM_DESTROYCLIPBOARD => IO.Put("WM_DESTROYCLIPBOARD");
    | WinUser.WM_DRAWCLIPBOARD => IO.Put("WM_DRAWCLIPBOARD");
    | WinUser.WM_PAINTCLIPBOARD => IO.Put("WM_PAINTCLIPBOARD");
    | WinUser.WM_VSCROLLCLIPBOARD => IO.Put("WM_VSCROLLCLIPBOARD");
    | WinUser.WM_SIZECLIPBOARD => IO.Put("WM_SIZECLIPBOARD");
    | WinUser.WM_ASKCBFORMATNAME => IO.Put("WM_ASKCBFORMATNAME");
    | WinUser.WM_CHANGECBCHAIN => IO.Put("WM_CHANGECBCHAIN");
    | WinUser.WM_HSCROLLCLIPBOARD => IO.Put("WM_HSCROLLCLIPBOARD");
    | WinUser.WM_QUERYNEWPALETTE => IO.Put("WM_QUERYNEWPALETTE");
    | WinUser.WM_PALETTEISCHANGING => IO.Put("WM_PALETTEISCHANGING");
    | WinUser.WM_PALETTECHANGED => IO.Put("WM_PALETTECHANGED");
    | WinUser.WM_HOTKEY => IO.Put("WM_HOTKEY");
    | WinUser.WM_PENWINFIRST => IO.Put("WM_PENWINFIRST");
    | WinUser.WM_PENWINLAST => IO.Put("WM_PENWINLAST");
    | WinUser.WM_MM_RESERVED_FIRST => IO.Put("WM_MM_RESERVED_FIRST");
    | WinUser.WM_MM_RESERVED_LAST => IO.Put("WM_MM_RESERVED_LAST");
    | WinUser.WM_USER => IO.Put("WM_USER");
    ELSE
      IO.Put("<not in my incomplete table>");
    END;
    IO.Put("\n");
  END PrintMessageType;


(*****************************************************************************)
(* The event queue data structure. I chose a sentinel-based implementation.  *)
(*****************************************************************************)

(* The following invariant is maintained:             
 *  <* ASSERT self.front # NIL AND self.end # NIL 
 *        AND self.end.head = NIL AND self.end.tail = NIL *>
 *)

TYPE 
  EventList = REF RECORD
    head: Event;
    tail: EventList;
  END;

  EventQueue = MUTEX OBJECT
    front: EventList := NIL;  (* dequeue at front *)
    end  : EventList := NIL;  (* enqueue at end *)
  METHODS
    init (): EventQueue  := InitEQ;
    put (e: Event)       := PutEQ;
    drain (base: T)      := DrainEQ;
  END;

PROCEDURE InitEQ (self: EventQueue): EventQueue =
  BEGIN
    (* Enter sentinel element *)
    self.front := NEW (EventList, head := NIL, tail := NIL);
    self.end := self.front;
    <* ASSERT self.front # NIL AND self.end # NIL 
          AND self.end.head = NIL AND self.end.tail = NIL *>
    RETURN self;
  END InitEQ;


PROCEDURE PutEQ (self: EventQueue; e: Event) =
  BEGIN
    LOCK self DO
      <* ASSERT self.front # NIL AND self.end # NIL 
            AND self.end.head = NIL AND self.end.tail = NIL *>
      self.end.head := e;
      self.end.tail := NEW (EventList, head := NIL, tail := NIL);
      self.end := self.end.tail;
      <* ASSERT self.front # NIL AND self.end # NIL 
            AND self.end.head = NIL AND self.end.tail = NIL *>
    END;
  END PutEQ;


PROCEDURE DrainEQ (self: EventQueue; base: T) =
  BEGIN
    LOCK self DO
      <* ASSERT self.front # NIL AND self.end # NIL 
            AND self.end.head = NIL AND self.end.tail = NIL *>
      WHILE self.front # self.end DO
        self.front.head.process (base);
        self.front := self.front.tail;
      END;
      <* ASSERT self.front # NIL AND self.end # NIL 
            AND self.end.head = NIL AND self.end.tail = NIL *>
    END;
  END DrainEQ;


(*****************************************************************************)
(* Event types                                                               *)
(*****************************************************************************)

TYPE 
  Event = OBJECT
  METHODS
    process (base: T);
  END;


TYPE
  MotionEvent = Event BRANDED OBJECT
    pos: Point.T;
  OVERRIDES
    process := ProcessMotion;
  END;

PROCEDURE ProcessMotion (self: MotionEvent; base: T) =
  BEGIN
    WITH posrec = PositionCB.Rec {pos2D := self.pos, 
                                  modifiers := base.modifiers} DO
      base.root.invokePositionCB (posrec);
    END;
  END ProcessMotion;


TYPE
  ButtonUpEvent = Event BRANDED OBJECT
    pos   : Point.T;
    button: VBT.Button;
  OVERRIDES
    process := ProcessButtonUp;
  END;

PROCEDURE ProcessButtonUp (self: ButtonUpEvent; base: T) =
  VAR
    clickType : VBT.ClickType;
  BEGIN
    DEC (base.buttonDownCount);
    IF base.buttonDownCount = 0 THEN
      clickType := VBT.ClickType.LastUp;
    ELSE
      clickType := VBT.ClickType.OtherUp;
    END;
    WITH mouserec = MouseCB.Rec {pos2D       := self.pos,
                                 whatChanged := self.button,
                                 modifiers   := base.modifiers,
                                 clickType   := clickType} DO
      base.root.invokeMouseCB (mouserec);
      base.modifiers := base.modifiers - VBT.Modifiers {self.button};
    END;
  END ProcessButtonUp;


TYPE
  ButtonDownEvent = Event BRANDED OBJECT
    pos   : Point.T;
    button: VBT.Button;
  OVERRIDES
    process := ProcessButtonDown;
  END;

PROCEDURE ProcessButtonDown (self: ButtonDownEvent; base: T) =
  VAR
    clickType : VBT.ClickType;
  BEGIN
    IF base.buttonDownCount = 0 THEN
      clickType := VBT.ClickType.FirstDown;
    ELSE
      clickType := VBT.ClickType.OtherDown;
    END;
    INC (base.buttonDownCount);
    WITH mouserec = MouseCB.Rec {pos2D       := self.pos,
                                 whatChanged := self.button,
                                 modifiers   := base.modifiers,
                                 clickType   := clickType} DO
      base.root.invokeMouseCB (mouserec);
      base.modifiers := base.modifiers + VBT.Modifiers {self.button};
    END;
  END ProcessButtonDown;


TYPE
  KeyEvent = Event BRANDED OBJECT
    key : VBT.KeySym;
    down: BOOLEAN;
  OVERRIDES
    process := ProcessKey;
  END;

PROCEDURE ProcessKey (self: KeyEvent; base: T) =

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

  BEGIN
    WITH keyrec = KeyCB.Rec {whatChanged := self.key,
                             wentDown    := self.down,
                             modifiers   := base.modifiers} DO
      base.root.invokeKeyCB (keyrec);
      IF self.down THEN
        base.modifiers := base.modifiers + KeySymToModifierSet (self.key);
      ELSE
        base.modifiers := base.modifiers - KeySymToModifierSet (self.key);
      END;
    END;
  END ProcessKey;


TYPE
  ExposeEvent = Event BRANDED OBJECT
  OVERRIDES
    process := ProcessExpose;
  END;

PROCEDURE ProcessExpose (<*UNUSED*> self: ExposeEvent; base: T) =
  BEGIN
    (*** damage the root object to force a redraw ***)
    IF base.root # NIL THEN
      base.root.damaged := TRUE;
    END;
  END ProcessExpose;


TYPE 
  ReshapeEvent = Event BRANDED OBJECT
    width, height: INTEGER;
  OVERRIDES
    process := ProcessReshape;
  END;

PROCEDURE ProcessReshape (self: ReshapeEvent; base: T) =
  BEGIN
    base.winWidth  := self.width;
    base.winHeight := self.height;
    GL.glViewport (0, 0, self.width, self.height);  (* adjust the viewport *)

    (*** damage the root object to force a redraw ***)
    IF base.root # NIL THEN
      base.root.damaged := TRUE;
    END;
  END ProcessReshape;

TYPE 
  DestroyEvent = Event BRANDED OBJECT
  OVERRIDES
    process := ProcessDestroy;
  END;

PROCEDURE ProcessDestroy (<*UNUSED*> self: DestroyEvent; base: T) =
  BEGIN
    base.destroy ();
  END ProcessDestroy;


(*****************************************************************************)
(* Animation Server                                                          *)
(*****************************************************************************)


PROCEDURE Setup (self: T) =
  BEGIN
    <* ASSERT AnimServer.IsServer() *>
    WITH status = WinGDI.wglMakeCurrent (self.hdc, self.hglrc) DO
      <* ASSERT status = True *>
    END;

    (*** Clear the color and the depth buffer ***)
    GL.glClear (Word.Or (GL.GL_COLOR_BUFFER_BIT, GL.GL_DEPTH_BUFFER_BIT));
  END Setup;


PROCEDURE Repair (self : T; VAR damaged : BOOLEAN) =
  VAR 
    status : WinDef.BOOL;
  BEGIN
    (*** Redraw the scene only if there is one and it was damaged ***)
    IF self.root # NIL AND self.root.damaged THEN
      damaged := TRUE;

      LOCK conn DO
        (*** first, make sure that all resources have been created ***)
        IF self.hwnd = NIL THEN
          EVAL Thread.Fork (NEW (Closure, base := self));
          Thread.Wait (conn, self.windowThreadCV);
        END;
        
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
        status := WinGDI.SwapBuffers (self.hdc);
        <* ASSERT status = True *>
      END;
    END;
  END Repair;


(*****************************************************************************)
(* Connection Management                                                     *)
(*****************************************************************************)


VAR conn := NEW (Connection).init ();


TYPE 
  Connection = MUTEX OBJECT       (* mutex protects fields *)
    currBase        : T;
    hwndMap         : IntRefTbl.T;
    hInst           : WinDef.HINSTANCE;
    windowclassName : Ctypes.char_star;
    nonclient       : Point.T;
  METHODS
    init (): Connection := InitConnection;
  END;


PROCEDURE InitConnection (self : Connection) : Connection =
  VAR
    wc    : WinUser.WNDCLASS;
    status: WinDef.BOOL;
  BEGIN
    (* Initialize the various fields of "self" *)
    self.currBase := NIL;
    self.hwndMap := NEW (IntRefTbl.Default).init ();
    self.hInst := RTLinker.info.instance;
    self.windowclassName := M3toC.CopyTtoS("Anim3D Window");
    self.nonclient.h := 2 * WinUser.GetSystemMetrics (WinUser.SM_CXFRAME);
    self.nonclient.v := 2 * WinUser.GetSystemMetrics (WinUser.SM_CYFRAME) +
                        WinUser.GetSystemMetrics (WinUser.SM_CYSCREEN) - 
                        WinUser.GetSystemMetrics (WinUser.SM_CYFULLSCREEN) - 1;

    (* Register the window class *)
    wc.style := WinUser.CS_HREDRAW + WinUser.CS_VREDRAW + WinUser.CS_OWNDC;
    wc.lpfnWndProc := WindowProc;
    wc.cbClsExtra := 0;
    wc.cbWndExtra := 0;
    wc.hInstance := self.hInst;
    wc.hIcon := WinUser.LoadIcon (NIL, WinUser.IDI_APPLICATION);
    wc.hCursor := WinUser.LoadCursor (NIL, WinUser.IDC_ARROW); 
    wc.hbrBackground := NIL;
    wc.lpszMenuName := NIL;
    wc.lpszClassName := self.windowclassName;

    status := WinUser.RegisterClass (ADR (wc));
    <* ASSERT status # 0 *>

    RETURN self;
  END InitConnection;


BEGIN
END Win_OpenGL_Base.
