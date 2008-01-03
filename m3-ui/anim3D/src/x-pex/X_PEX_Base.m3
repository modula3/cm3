(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Aug 21 15:57:57 PDT 1995 by najork                   *)
(*       Created on Wed Feb 16 17:15:31 PST 1994 by najork                   *)


UNSAFE MODULE X_PEX_Base EXPORTS X_PEX_Base, X_PEX_BaseProxy;

IMPORT AnimServer, AuxG, Color, ColorPropPrivate, Ctypes, GO, GOPrivate, 
       GraphicsBase, GraphicsBasePrivate, IntRefTbl, KeyCB, KeyboardKey, 
       LineGO, LineTypeProp, MarkerGO, MarkerTypeProp, Matrix4, M3toC, Math, 
       MouseCB, Mth, PEX, Point, Point3, PositionCB, Process, PropPrivate, 
       RasterModeProp, RealPropPrivate, RootGOPrivate, ShadingProp, SurfaceGO,
       Text, Thread, TransformPropPrivate, VBT, Word, X, Xatom, Xmbuf;

IMPORT FileRd, FloatMode, Lex, OSError, ParseParams, Rd, Stdio;

<* FATAL X.Error *>

REVEAL
  T = Public BRANDED OBJECT
    window              : X.Window;
    capx_info           : PEX.pxlColourApproxEntry;
    xmbBuffers          : ARRAY BOOLEAN OF X.XID;
    curBuf              := FALSE;
    rd                  : PEX.pexRenderer;
    viewLut             : PEX.pxlLookupTable; 
    depthCueLut         : PEX.pxlLookupTable; 
    lightLut            : PEX.pxlLookupTable;
    oc                  : PEX.pxlOCBufStar; 
    camOcBuf            : PEX.pxlOCBufStar;   
    lightOcBuf          : PEX.pxlOCBufStar;
    matrixOcBuf         : PEX.pxlOCBufStar;
    transflag           : BOOLEAN;           (* transparent parts in scene? *)
    modifiers           : VBT.Modifiers;     (* what modifiers are pressed  *)
    buttonDownCount     : INTEGER;           (* how many buttons are down   *)
    awaitDeleteMu       : Thread.Mutex;      (* Mutex used by Thread.Wait   *)
    awaitDeleteCV       : Thread.Condition;  (* CV for awaitDelete method   *)
    dlTable             : IntRefTbl.T;

    stateSize           : INTEGER;

    from                : Point3.T;
    to                  : Point3.T;
    up                  : Point3.T;
    projType            : ProjType;
    aspect              : REAL;
    fovy                : REAL;
    height              : REAL;
    camTrans            : Matrix4.T := Matrix4.Id;
  (*** Things associated with light sources ***)
    lia           : REF ARRAY OF PEX.pxlTableIndex; 
                             (* The "light index array" *)
    lastLightSlot : INTEGER; (* The last slot used during a particular draw *)
    maxLights     : INTEGER; (* The highest used index into lightLut *)

  (*** Things associated with display lists ***)
    ocbufStack       : OcbufStack;
    ocbufStackPtr    : INTEGER;
  (*** Things associated with the matrix stack (PEX-specific) ***)
    matrixStack      : MatrixStack;
    matrixStackTop   : INTEGER;
  (*** need to accumulate some PEX state ***)
    surfRefl         : PEX.pxlReflectionAttr;    (* USED FOR A DIRTY HACK *)
  (*** caching of PEX structures for prototypical objects ***)
    sphereStructures   : StructureList := NIL;
    coneStructures     : StructureList := NIL;
    cylinderStructures : StructureList := NIL;
    diskStructures     : StructureList := NIL;
    torusStructures    : TorusStructureCache;
  METHODS
    setup()            := Setup;
    establishLights()  := EstablishLights;
  OVERRIDES
    init               := Init;         (* should be called only by server *)
    changeTitle        := ChangeTitle;  (* should be called only by server *)
    awaitDelete        := AwaitDelete;
    destroy            := Destroy;
  (*** called only by the animation server thread ***)
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
    setPerspProj             := SetPerspProj;
    setOrthoProj             := SetOrthoProj;
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

TYPE ProjType = {Persp, Ortho};


PROCEDURE Init (self  : T; 
                title : TEXT; 
                win_x, win_y, win_w, win_h : INTEGER) : T 
    RAISES {GraphicsBase.Failure} =
  CONST
    ocsize   = 8192;
    bw       = 1;                               (* window border width *)
  VAR
    attribs   : PEX.pxlRendererAttributes;	(* renderer attributes *)
    rmask     : Ctypes.unsigned_int;            (* renderer attribute mask *)
    mpexRA    : PEX.mpxlRendererAttributes;
    visual    : X.XVisualInfo;
    cmap_info : X.XStandardColormap;
    capx_info : PEX.pxlColourApproxEntry;
    wattrs    : X.XSetWindowAttributes;         (* window attributes *)
    wmask     : Ctypes.unsigned_long;           (* window attribute mask *)
  BEGIN
    (*** First, ensure that there is a display connection ***)
    IF man = NIL THEN
      man := NEW (Manager).init ();
    END;

    (*** Initialize awaitDeleteCV ***)
    self.awaitDeleteMu := NEW (Thread.Mutex);
    self.awaitDeleteCV := NEW (Thread.Condition);

    (*** Initialize the display list table ***)
    self.dlTable := NEW (IntRefTbl.Default).init ();

    WITH disp = man.disp, window = self.window, xmbBuffers = self.xmbBuffers, 
         curBuf = self.curBuf, rd = self.rd DO
      LOCK man DO

        (*** Create and initialize a window ***)
        visual := FindBestVisual (disp);
        TRY
          CreateColorMap (disp, visual, cmap_info, capx_info);
        EXCEPT
          CmapAllocError => RAISE GraphicsBase.Failure;
        END;
        self.capx_info := capx_info;

        (*** create and initialize a window ***)
    
        (* Create the output window. *)
        wmask := 0;

        wattrs.background_pixel := X.XBlackPixel(disp, X.XDefaultScreen(disp));
        wmask := Word.Or (wmask, X.CWBackPixel);

        wattrs.border_pixel := X.XWhitePixel (disp, X.XDefaultScreen(disp));
        wmask := Word.Or (wmask, X.CWBorderPixel);

        wattrs.colormap := cmap_info.colormap;
        wmask := Word.Or (wmask, X.CWColormap);

        window := X.XCreateWindow (disp, X.XRootWindow (disp, visual.screen),
                                   win_x, win_y, win_w, win_h, bw,
                                   visual.depth, X.InputOutput, 
                                   visual.visual, wmask, ADR (wattrs));

        IF NOT TestEnumAvailable (self, 
                                  PEX.PEXETColourType, 
                                  PEX.PEXRgbFloatColour) THEN
          RAISE GraphicsBase.Failure;
        END;

        X.XSelectInput(
                    disp, window, 
                    Word.Or(X.ExposureMask,
                     Word.Or(X.StructureNotifyMask,
                      Word.Or(X.KeyPressMask,
                       Word.Or(X.KeyReleaseMask,
                        Word.Or(X.ButtonPressMask,
                         Word.Or(X.ButtonReleaseMask,
                                 X.PointerMotionMask)))))));
  
        (*** set the window's title ***)
  
        X.XChangeProperty(disp, window, Xatom.XA_WM_NAME, Xatom.XA_STRING, 8,
                          X.PropModeReplace,
                          LOOPHOLE (M3toC.TtoS (title), 
                                    Ctypes.unsigned_char_star),
                          Text.Length (title));
  
        (*** ask the WM to send ClientMessage events when f.kill is chosen ***)
  
        EVAL X.XSetWMProtocols (disp, window, ADR (man.wm_delete_window), 1);
  
        (*** map the window ***)
  
        X.XMapWindow(disp, window);
      
        (*** create a pixmap for double buffering ***)
  
        EVAL Xmbuf.XmbufCreateBuffers(disp, window, 2,
                                      Xmbuf.MultibufferActionCopied,
                                      Xmbuf.MultibufferHintFrequent,
                                      ADR (xmbBuffers));
        curBuf := FALSE;
  
        (*** create the lookup tables ***)
  
        self.viewLut := 
                    PEX.PEXCreateLookupTable(disp, window, PEX.PEXViewLUT);
        self.lightLut := 
                    PEX.PEXCreateLookupTable(disp, window, PEX.PEXLightLUT);
        self.depthCueLut := 
                    PEX.PEXCreateLookupTable(disp, window, PEX.PEXDepthCueLUT);
  
        (*** create the renderer ***)
        
        rmask := PEX.PEXRDClipList;
    
        attribs.hlhsrMode := PEX.PEXHlhsrZBuffer; 
        rmask := Word.Or (rmask, PEX.PEXRDHlhsrMode);
    
        attribs.viewTable := self.viewLut;
        rmask := Word.Or (rmask, PEX.PEXRDViewTable);
    
        attribs.lightTable := self.lightLut;
        rmask := Word.Or (rmask, PEX.PEXRDLightTable);
    
        attribs.depthCueTable := self.depthCueLut;
        rmask := Word.Or (rmask, PEX.PEXRDDepthCueTable);
        
        (* Create a color approximation table and set the default entry, 
           entry 0, to the colormap approximation specified. *)

        rmask := Word.Or (rmask, PEX.PEXRDColourApproxTable);
        attribs.colourApproxTable := 
            PEX.PEXCreateLookupTable (disp, window, PEX.PEXColourApproxLUT);
        PEX.PEXSetTableEntries (disp, attribs.colourApproxTable, 
                                PEX.PEXColourApproxLUT, 0, 1, ADR (capx_info));

        rd := PEX.PEXCreateRenderer (disp, window, rmask, ADR (attribs));
  
        (********** crucial for MIT double buffering **********)
  
        mpexRA.backgroundPixel := X.XBlackPixel (disp, X.XDefaultScreen(disp));
        mpexRA.clearI := PEX.PEXOn;
        mpexRA.clearZ := PEX.PEXOn;
        PEX.MPEXChangeNewRenderer(disp, rd,
                                   Word.Or(PEX.MPEXNRAClearI,
                                    Word.Or(PEX.MPEXNRAClearZ,
                                            PEX.MPEXNRABackgroundPixel)),
                                   ADR(mpexRA));
  
        (*** create the output command buffers ***)
  
        self.oc         := PEX.PEXAllocateRetainedOCBuffer(
                               disp,PEX.pxlRenderImmediate,
                               rd, PEX.PEXDefaultOCError, ocsize);
        self.lightOcBuf := PEX.PEXAllocateRetainedOCBuffer(
                               disp,PEX.pxlRenderImmediate,
                               rd, PEX.PEXDefaultOCError, ocsize);
        self.camOcBuf  := PEX.PEXAllocateRetainedOCBuffer(
                              disp,PEX.pxlRenderImmediate,
                              rd, PEX.PEXDefaultOCError, ocsize);
        
        self.stacks := PropPrivate.NewStacks ();
        self.stateSize := NUMBER (self.stacks^);

        self.surfRefl.ambient := 
            SurfaceGO.AmbientReflectionCoeff.getState (self);
        self.surfRefl.diffuse := 
            SurfaceGO.DiffuseReflectionCoeff.getState (self);
        self.surfRefl.specular := 
            SurfaceGO.SpecularReflectionCoeff.getState (self);
        self.surfRefl.specularConc := 
            SurfaceGO.SpecularReflectionConc.getState (self);
        self.surfRefl.transmission := 
            SurfaceGO.TransmissionCoeff.getState (self);
        WITH val = SurfaceGO.SpecularReflectionColour.getState (self) DO
          self.surfRefl.specularColour := PexColourSpecifier (val);
        END;

        (*** Initialize light-related stuff ***)
        self.maxLights := 0;
        self.lia := NEW (REF ARRAY OF PEX.pxlTableIndex, 10);
        FOR i := FIRST (self.lia^) TO LAST (self.lia^) DO
          self.lia[i] := i  + 1;
        END;

        InitDisplayListManagement (self);
        InitMatrixStack (self);
  
        self.matrixOcBuf := PEX.PEXAllocateRetainedOCBuffer(
                                   disp,PEX.pxlRenderImmediate,
                                   rd, PEX.PEXDefaultOCError, ocsize);
  
        (*** initialize depth cueing with default values ***)
        self.setDepthcueing (FALSE, 1.0, 0.0, 1.0, 0.0, Color.Black);
  
      END; (* release the display lock *)
    END;

    self.modifiers := VBT.Modifiers {};
    self.buttonDownCount := 0;

    self.status := GraphicsBasePrivate.Status.Mapped;

    WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO
      IF pp.keywordPresent("-largeCursor") THEN
        LargeCursor(self);
      END;
    END;

    IF MkProxyT # NIL THEN
      MkProxyT (self);
    END;

    RETURN self;
  END Init;


PROCEDURE LargeCursor (self : T) =
  VAR
    pm := X.XCreatePixmap (man.disp, self.window, 64, 64, 1);
    fg, bg : X.XColor; 
    hot : X.XPoint;
    pts : REF ARRAY OF X.XPoint;
    bg_gcv, fg_gcv : X.XGCValues;
    bg_gc, fg_gc : X.GC;
  BEGIN
    bg_gcv.function := X.GXclear;
    bg_gc := X.XCreateGC(man.disp, pm, X.GCFunction, ADR(bg_gcv));
    fg_gcv.function := X.GXset;
    fg_gc := X.XCreateGC(man.disp, pm, X.GCFunction, ADR(fg_gcv));
    
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
    
    X.XFillRectangle (man.disp, pm, bg_gc, 0, 0, 64, 64);
    X.XFillPolygon (man.disp, pm, fg_gc, 
                    ADR(pts[0]), NUMBER(pts^), 
                    X.Nonconvex,  X.CoordModeOrigin);
    WITH cursor = X.XCreatePixmapCursor(man.disp, pm, pm, 
                                        ADR(fg), ADR(bg), 
                                        hot.x, hot.y) DO
      X.XDefineCursor (man.disp, self.window, cursor);
    END;
  END LargeCursor;


PROCEDURE ChangeTitle (self: T; title : TEXT) =
  BEGIN
    LOCK man DO
      X.XChangeProperty (man.disp, 
                         self.window, 
                         Xatom.XA_WM_NAME, 
                         Xatom.XA_STRING, 
                         8,
                         X.PropModeReplace,
                         LOOPHOLE (M3toC.TtoS (title), 
                                   Ctypes.unsigned_char_star),
                         Text.Length (title));
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
  BEGIN
    (*** Destroy the window ***)
    X.XDestroyWindow (man.disp, self.window);
    X.XSync (man.disp, X.False);
    self.window := X.None;
    self.status := GraphicsBasePrivate.Status.Unmapped;

    (*** signal all threads that are blocked ***)
    Thread.Broadcast (self.awaitDeleteCV);
  END Unmap;

      
PROCEDURE Available () : BOOLEAN =
  BEGIN
    IF man = NIL THEN
      man := NEW (Manager).init ();
    END;
    RETURN man.avail;
  END Available;


(* Caller must hold man *)

PROCEDURE ShowWindow (self : T) =
  VAR
    first : INTEGER;
  BEGIN
    WITH disp = man.disp, pixmap = self.xmbBuffers[self.curBuf], rd = self.rd DO
      IF self.transflag THEN
        first := X.True;
        REPEAT
          PEX.MPEXBeginTransparencyRendering(disp, pixmap, rd, first);
          PEX.PEXSendOCBuffer (self.matrixOcBuf);
          PEX.PEXSendOCBuffer (self.camOcBuf);
          PEX.PEXSendOCBuffer (self.lightOcBuf);
          PEX.PEXSendOCBuffer (self.oc);
          first := X.False;
        UNTIL PEX.MPEXEndTransparencyRendering(disp, rd, X.True) = 0;
        PEX.PEXFlushOCBuffer(self.matrixOcBuf);
      ELSE
        PEX.PEXBeginRendering(disp, pixmap, rd);
        PEX.PEXFlushOCBuffer(self.matrixOcBuf);
        PEX.PEXSendOCBuffer (self.camOcBuf);
        PEX.PEXSendOCBuffer (self.lightOcBuf);
        PEX.PEXSendOCBuffer (self.oc);
        PEX.PEXEndRendering (disp, rd, X.True); 
      END;
      Xmbuf.XmbufDisplayBuffers(disp, 1, ADR(pixmap), 0, 0);
      self.curBuf := NOT self.curBuf;
      
      X.XSync (disp, X.False);

    END;
  END ShowWindow;


PROCEDURE TestEnumAvailable (self : T; 
                             enumType : Ctypes.int;
                             enumVal  : Ctypes.short) : BOOLEAN =
  TYPE
    T = UNTRACED REF ARRAY [0 .. 1000000] OF PEX.pxlEnumTypeDescList;
  VAR
    status : Ctypes.int;
    values : PEX.pxlEnumTypeDescListStar;
    count  : Ctypes.int;
  BEGIN
    status := PEX.PEXGetEnumTypeInfo (man.disp, self.window, 
                                      enumType, PEX.PEXETIndex,
                                      ADR (values), ADR (count));
    <* ASSERT status = 0 *>
    WITH v = LOOPHOLE (values, T) DO
      FOR i := 0 TO count - 1 DO 
        IF v[i].enumVal = enumVal THEN
          RETURN TRUE;
        END;
      END;
    END;
    RETURN FALSE;
  END TestEnumAvailable;


PROCEDURE Setup (self : T) =
  VAR 
    pexrep : PEX.pexRgbFloatColour;
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    (*** flush and initialize OC buffers ***)
    PEX.PEXFlushOCBuffer   (self.camOcBuf);
    PEX.PEXFlushOCBuffer   (self.lightOcBuf);
    PEX.PEXFlushOCBuffer   (self.oc);

    WITH oc = self.oc DO
      WITH val = MarkerGO.Colour.getState (self) DO
        pexrep := PEX.pexRgbFloatColour {val.r, val.g, val.b};
        PEX.PEXSetMarkerColour (oc, ADR (pexrep));
      END;
      WITH val = LineGO.Colour.getState (self) DO
        pexrep := PEX.pexRgbFloatColour {val.r, val.g, val.b};
        PEX.PEXSetLineColour (oc, ADR (pexrep));
      END;
      WITH val = SurfaceGO.Colour.getState (self) DO
        pexrep := PEX.pexRgbFloatColour {val.r, val.g, val.b};
        PEX.PEXSetSurfaceColour (oc, ADR (pexrep));
      END;
      WITH val = SurfaceGO.EdgeColour.getState (self) DO
        pexrep := PEX.pexRgbFloatColour {val.r, val.g, val.b};
        PEX.PEXSetSurfaceEdgeColour (oc, ADR (pexrep));
      END;
      PEX.PEXSetReflectionModel       (oc, PEX.PEXReflectionSpecular);
      PEX.PEXSetBFReflectionModel     (oc, PEX.PEXReflectionSpecular); (*Lyle*)
      PEX.PEXSetInteriorStyle         (oc, PEX.PEXInteriorStyleSolid);
      PEX.PEXSetBFInteriorStyle       (oc, PEX.PEXInteriorStyleSolid); (*Lyle*)
      PEX.PEXSetReflectionAttributes  (oc, ADR(self.surfRefl));
      PEX.PEXSetBFReflectionAttributes(oc, ADR(self.surfRefl));
      PEX.PEXSetDepthCueIndex(oc, 1);
    END;

    (* Reset the light counter *)
    self.lastLightSlot := 0;
  END Setup;


PROCEDURE Push (self : T; caller : GO.T) =
  VAR
    oldSurfRefl : PEX.pxlReflectionAttr;
    props := caller.props;
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    oldSurfRefl := self.surfRefl;

    WHILE props # NIL DO
      WITH prop = props.head DO
        prop.n.push (self, prop.v);
      END;
      props := props.tail;
    END;

    IF oldSurfRefl # self.surfRefl THEN
      PEX.PEXSetReflectionAttributes (self.oc, ADR(self.surfRefl));
      PEX.PEXSetBFReflectionAttributes (self.oc, ADR(self.surfRefl));
    END;
  END Push;


PROCEDURE Pop (self : T; caller : GO.T) =
  VAR
    oldSurfRefl : PEX.pxlReflectionAttr;
    props := caller.props;
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    oldSurfRefl := self.surfRefl;

    WHILE props # NIL DO
      props.head.n.pop (self);
      props := props.tail;
    END;

    IF oldSurfRefl # self.surfRefl THEN
      PEX.PEXSetReflectionAttributes (self.oc, ADR(self.surfRefl));
      PEX.PEXSetBFReflectionAttributes (self.oc, ADR(self.surfRefl));
    END;
  END Pop;


(*****************************************************************************)
(* Light source management                                                   *)
(*****************************************************************************)

PROCEDURE AddLight (self : T; READONLY light : PEX.pxlLightEntry) =
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    INC (self.lastLightSlot);
    self.maxLights := MAX (self.maxLights, self.lastLightSlot);
    PEX.PEXSetTableEntries (man.disp, 
                            self.lightLut, 
                            PEX.PEXLightLUT, 
                            self.lastLightSlot,
                            1, 
                            ADR (light));

    (*** Check if we need to grow the light index array ***)
    IF self.maxLights > NUMBER (self.lia^) THEN
      self.lia := NEW (REF ARRAY OF PEX.pxlTableIndex, 2 * NUMBER (self.lia^));
      FOR i := FIRST (self.lia^) TO LAST (self.lia^) DO
        self.lia[i] := i + 1;
      END;
    END;
  END AddLight;


PROCEDURE AddAmbientLight (self: T; color: Color.T) =
  VAR
    light : PEX.pxlLightEntry;
  BEGIN
    (* Build a PEX light structure *)
    light.lightType   := PEX.PEXLightAmbient;
    light.lightColour := PexColourSpecifier (color);

    (* Add the light to the graphics state's light set *)
    AddLight (self, light);
  END AddAmbientLight;


PROCEDURE AddVectorLight (self: T; color: Color.T; d: Point3.T) =
  VAR
    light : PEX.pxlLightEntry;
    A     : Matrix4.T;
    dir   : Point3.T;
  BEGIN
    (* Get the current transformation matrix. *)
    A := GO.Transform.getState (self);

    <* ASSERT A[3][0] = 0.0 AND A[3][1] = 0.0 AND 
              A[3][2] = 0.0 AND A[3][3] = 1.0 *>

    (* Transform dir into world coordinate space. Mask out any translations.
       We don't care about scalings. We do care about rotations. *)

    dir := Point3.T {A[0][0] * d.x + A[0][1] * d.y + A[0][2] * d.z,
                     A[1][0] * d.x + A[1][1] * d.y + A[1][2] * d.z,
                     A[2][0] * d.x + A[2][1] * d.y + A[2][2] * d.z};

    (* Build a PEX light structure *)
    light.lightType   := PEX.PEXLightWcsVector;
    light.lightColour := PexColourSpecifier (color);
    light.direction   := dir;

    (* Add the light to the light set *)
    AddLight (self, light);
  END AddVectorLight;


PROCEDURE AddPointLight (self      : T; 
                         color     : Color.T; 
                         p         : Point3.T; 
                         att0, att1: REAL) =
  VAR
    light : PEX.pxlLightEntry;
    A     : Matrix4.T;
    point : Point3.T;
  BEGIN
    (* Get the current value of the transformation property. *) 
    A := GO.Transform.getState (self);

    <* ASSERT A[3][0] = 0.0 AND A[3][1] = 0.0 AND 
              A[3][2] = 0.0 AND A[3][3] = 1.0 *>

    (* Transform the origin into world coordinate space. *)
    point := Point3.T {
                   A[0][0] * p.x + A[0][1] * p.y + A[0][2] * p.z + A[0][3],
                   A[1][0] * p.x + A[1][1] * p.y + A[1][2] * p.z + A[1][3],
                   A[2][0] * p.x + A[2][1] * p.y + A[2][2] * p.z + A[2][3]};

    (* Build a PEX light structure *)
    light.lightType    := PEX.PEXLightWcsPoint;
    light.lightColour  := PexColourSpecifier (color);
    light.point        := point;
    light.attenuation1 := att0;
    light.attenuation2 := att1;

    (* Add the light to the light set *)
    AddLight (self, light);
  END AddPointLight;


PROCEDURE AddSpotLight (self: T; color: Color.T; p, d: Point3.T; 
                        conc, spread, att0, att1: REAL) =
  VAR
    light : PEX.pxlLightEntry;
    A     : Matrix4.T;
    point : Point3.T;
    dir   : Point3.T;
  BEGIN
    (* Get the current value of the transformation property. *) 
    A := GO.Transform.getState (self);

    <* ASSERT A[3][0] = 0.0 AND A[3][1] = 0.0 AND 
              A[3][2] = 0.0 AND A[3][3] = 1.0 *>

    (* Transform origin and direction into world coordinate space. *)
    point := Point3.T {
                 A[0][0] * p.x + A[0][1] * p.y + A[0][2] * p.z + A[0][3],
                 A[1][0] * p.x + A[1][1] * p.y + A[1][2] * p.z + A[1][3],
                 A[2][0] * p.x + A[2][1] * p.y + A[2][2] * p.z + A[2][3]};
    dir   := Point3.T {
                 A[0][0] * d.x + A[0][1] * d.y + A[0][2] * d.z,
                 A[1][0] * d.x + A[1][1] * d.y + A[1][2] * d.z,
                 A[2][0] * d.x + A[2][1] * d.y + A[2][2] * d.z};

    (* Build a PEX light structure *)
    light.lightType     := PEX.PEXLightWcsSpot;
    light.lightColour   := PexColourSpecifier (color);
    light.point         := point;
    light.direction     := dir;
    light.concentration := conc;
    light.spreadAngle   := spread;
    light.attenuation1  := att0;
    light.attenuation2  := att1;

    (* Add the light to the light set *)
    AddLight (self, light);
  END AddSpotLight;


PROCEDURE EstablishLights (self : T) =
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    PEX.PEXSetLightSourceState (self.lightOcBuf, 
                                ADR (self.lia [0]), 
                                self.lastLightSlot, 
                                ADR (self.lia [self.lastLightSlot]), 
                                self.maxLights - self.lastLightSlot);
  END EstablishLights;


(*****************************************************************************)
(* Display-List management (PEX-specific)                                    *)
(*****************************************************************************)

TYPE
  StructureList = REF RECORD
    prec : INTEGER;
    dl   : DisplayList;
    next : StructureList;
  END;

  DisplayList = REF RECORD
    structure : PEX.pxlStructure;
    ocbuf     : PEX.pxlOCBufStar;
  END;

  DisplayListPool = REF RECORD
    dl   : DisplayList;
    next : DisplayListPool;
  END;

  OcbufStack = REF ARRAY OF PEX.pxlOCBufStar;

VAR
  dl_pool : DisplayListPool;
  (* This is a simplification. There should really be one pool per X server. *)


PROCEDURE InitDisplayListManagement (self : T) =
  BEGIN  
    (*** Create an initial output command buffer stack ***)
    self.ocbufStackPtr := 0;
    self.ocbufStack := NEW (OcbufStack, 10);
  END InitDisplayListManagement;


PROCEDURE OpenDisplayList (self : T; go : GO.T) =
  VAR
    ref      : REFANY;
    dl       : DisplayList;
    tmpStack : OcbufStack;
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    (*** Extract the display list associated with the GO. ***)
    IF go.dl = 0 THEN
      go.dl := AnimServer.NewDisplayList (go);
    END;
    IF self.dlTable.get (go.dl, ref) THEN
      dl := NARROW (ref, DisplayList);
    ELSE
      dl := NewDisplayList (self);
      EVAL self.dlTable.put (go.dl, dl);
    END;
    
    (*** Push the oc-buffer stack ***)
    WITH s = self.ocbufStack, p = self.ocbufStackPtr, n = NUMBER (s^) DO
      IF p >= n THEN
        tmpStack := NEW (OcbufStack, 2 * n);
        SUBARRAY (tmpStack^, 0, n) := s^;
        s := tmpStack;
      END;
      s[p] := self.oc;
      INC (p);
    END;

    (*** I assume that deleting elements is cheaper than creating a new 
      structure. ***)
    PEX.PEXDeleteElements (man.disp, dl.structure, 
                           PEX.PEXBeginning, 0, PEX.PEXEnd, 0);

    (*** Activate the oc-buffer that leads into the structure ***)
    self.oc := dl.ocbuf;

    (*** Push an identity matrix onto the matrix stack, reflecting the 
         semantics of PEXExecuteStructure, which maintains ints own
         matrix stack. ***)
    WITH s = self.matrixStack, n = NUMBER (s^), top = self.matrixStackTop DO
      INC (top);
      IF top >= n THEN
        WITH tmp = NEW (MatrixStack, 2 * n) DO
          SUBARRAY (tmp^, 0, n) := s^;
          s := tmp;
        END;
      END;
      s[top] := Matrix4.Id;
    END;
  END OpenDisplayList;


PROCEDURE CloseDisplayList (self : T) =
  BEGIN
    (*** pop the oc-buffer stack ***)    
    WITH s = self.ocbufStack, p = self.ocbufStackPtr DO
      DEC (p);
      self.oc := s[p];
    END;

    (*** pop the identity matrix from the matrix stack ***)
    DEC (self.matrixStackTop);
  END CloseDisplayList;
        

PROCEDURE CallDisplayList (self : T; go : GO.T) =
  VAR
    ref: REFANY;
    dl : DisplayList;
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    (*** Extract the display list associated with the GO. ***)
    IF self.dlTable.get (go.dl, ref) THEN
      dl := NARROW (ref, DisplayList);
    ELSE
      <* ASSERT FALSE *>
    END;

    PEX.PEXExecuteStructure (self.oc, dl.structure);
  END CallDisplayList;


PROCEDURE FreeDisplayList (self: T; go: GO.T) =
  VAR
    ref: REFANY;
  BEGIN
    IF self.dlTable.delete (go.dl, ref) THEN
      dl_pool := NEW (DisplayListPool, 
                      dl := NARROW (ref, DisplayList), 
                      next := dl_pool);
    END;
  END FreeDisplayList;


PROCEDURE NewDisplayList (<*UNUSED*> self : T) : DisplayList =
  VAR
    dl : DisplayList;
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    IF dl_pool # NIL THEN
      (*** Take an unused display list from the pool ***)
      dl := dl_pool.dl;
      dl_pool := dl_pool.next;
    ELSE
      (*** The pool is empty, so create a new display list ***)
      dl := NEW (DisplayList);
      dl.structure := PEX.PEXCreateStructure (man.disp);
      dl.ocbuf := PEX.PEXAllocateTransientOCBuffer (
                              man.disp, 
                              PEX.pxlAddToStructure, 
                              dl.structure, 
                              PEX.PEXDefaultOCError, 0);
      (*** I tried 0 and 8192, seems to not make much difference ***)
    END;
    
    RETURN dl;
  END NewDisplayList;


(*****************************************************************************)
(* The Matrix Stack (PEX-specific)                                           *)
(*****************************************************************************)

TYPE
  MatrixStack = REF ARRAY OF Matrix4.T;

PROCEDURE InitMatrixStack (self : T) =
  BEGIN
    self.matrixStack := NEW (MatrixStack, 32);
    PEX.PEXIdentityMatrix (ADR (self.matrixStack[0]));
    self.matrixStackTop := 0;
  END InitMatrixStack;


PROCEDURE PushMatrix (self : T; READONLY matrix : Matrix4.T) =
  VAR
    tmp : MatrixStack;
    transformdata : PEX.pxlLocalTransform3DData;
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    WITH s = self.matrixStack, n = NUMBER (s^), top = self.matrixStackTop DO
      INC (top);
      IF top >= n THEN
        tmp := NEW (MatrixStack, 2 * n);
        SUBARRAY (tmp^, 0, n) := s^;
        s := tmp;
      END;
      PEX.PEXMultiplyMatrices (ADR (s[top - 1]), ADR (matrix), ADR (s[top]));
      transformdata.composition := PEX.PEXReplace;
      transformdata.matrix := s[top];
      PEX.PEXSetLocalTransform (self.oc, ADR (transformdata));
    END;
  END PushMatrix;


PROCEDURE PopMatrix (self : T) =
  VAR
    transformdata : PEX.pxlLocalTransform3DData;
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    DEC (self.matrixStackTop);
    transformdata.composition := PEX.PEXReplace;
    transformdata.matrix := self.matrixStack[self.matrixStackTop];
    PEX.PEXSetLocalTransform (self.oc, ADR (transformdata));
  END PopMatrix;


(*****************************************************************************)
(* Hooks into PEX functions                                                  *)
(*****************************************************************************)


PROCEDURE SetLookAt (self: T; from, to, up: Point3.T) =
  BEGIN
    self.from := from;
    self.to   := to;
    self.up   := up;
  END SetLookAt;


PROCEDURE SetPerspProj (self: T; fovy, aspect: REAL) =
  BEGIN
    self.projType := ProjType.Persp;
    self.fovy     := fovy;
    self.aspect   := aspect;
  END SetPerspProj;


PROCEDURE SetOrthoProj (self: T; height, aspect: REAL) =
  BEGIN
    self.projType := ProjType.Ortho;
    self.height   := height;
    self.aspect   := aspect;
  END SetOrthoProj;


PROCEDURE SetupCamera (self: T) =
  VAR
    viewEntry : PEX.pxlViewEntry;
    near      : REAL;
    far       : REAL;
    distance  : REAL;
    viewTrans : Matrix4.T;
    projTrans : Matrix4.T;
    s         : X.Status;
  BEGIN
    s := PEX.PEXLookatViewMatrix (ADR(self.from), ADR(self.to), ADR(self.up), 
                                  ADR (viewTrans));
    <* ASSERT s = 0 *>

    WITH bs = self.getBoundingVolume(),
         M = viewTrans,
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
      far  := center.z - radius;
      near := center.z + radius;
    END;

    CASE self.projType OF
    | ProjType.Persp =>
      (* Handling bad cases (cases where the bounding sphere intersects
         the viewing plane) *)
      distance := Point3.Distance (self.from, self.to);
      IF distance <= near THEN
        near := distance - 0.1;
      END;
      IF near <= far THEN
        far := near - 0.1;
      END;
      s := PEX.PEXPerspProjMatrix (FLOAT (self.fovy, LONGREAL), 
                                   FLOAT (distance, LONGREAL), 
                                   FLOAT (self.aspect, LONGREAL), 
                                   FLOAT (near, LONGREAL), 
                                   FLOAT (far, LONGREAL),
                                   ADR (projTrans));
      <* ASSERT s = 0 *>
    | ProjType.Ortho =>
      (* Handle singularity "near = far" *)
      IF near = far THEN
        near := near + 0.01;
        far  := far  - 0.01;
      END;
      s := PEX.PEXOrthoProjMatrix (FLOAT (self.height, LONGREAL), 
                                   FLOAT (self.aspect, LONGREAL), 
                                   FLOAT (near, LONGREAL), 
                                   FLOAT (far, LONGREAL), 
                                   ADR (projTrans));
      <* ASSERT s = 0 *>
    END;

    viewEntry.clipFlags   := 0; 
    viewEntry.orientation := viewTrans;
    viewEntry.mapping     := projTrans;
    PEX.PEXSetTableEntries (man.disp, self.viewLut, PEX.PEXViewLUT, 1, 1, 
                            ADR (viewEntry));
    PEX.PEXSetViewIndex (self.camOcBuf, 1);

    self.camTrans := Matrix4.Multiply (projTrans, viewTrans);
  END SetupCamera;


PROCEDURE ScreenToWorld (self: T; pos: Point.T; z: REAL): Point3.T =
  VAR
    winAttrs: X.XWindowAttributes;
    status  : X.Status;
  BEGIN
    LOCK man DO
      status := X.XGetWindowAttributes (man.disp, self.window, ADR(winAttrs));
      <* ASSERT status = X.True *>
    END;
    WITH len = FLOAT (MIN (winAttrs.width, winAttrs.height)),
         top = MAX (0, winAttrs.height - winAttrs.width),
         x   = FLOAT (pos.h) / len,
         y   = 1.0 - FLOAT (pos.v - top) / len,
         p   = Point3.T {x, y, z},
         M   = Matrix4.Invert (self.camTrans) DO
      RETURN Matrix4.TransformPoint3 (M, p);
    END;
  END ScreenToWorld;


(* "SetBackgroundColor" is called by "RootGO.Draw". The locking level is
   {AnimServer.internalLock, AnimServer.externalLock} *)

PROCEDURE SetBackgroundColor (self : T; color : Color.T) =
  VAR
    mpexRA : PEX.mpxlRendererAttributes;
  BEGIN
    WITH ca = self.capx_info, 
         r = ca.mult1 * ROUND (FLOAT (ca.max1) * color.r),
         g = ca.mult2 * ROUND (FLOAT (ca.max2) * color.g),
         b = ca.mult3 * ROUND (FLOAT (ca.max3) * color.b) DO
      mpexRA.backgroundPixel := ca.basePixel + r + g + b;
      PEX.MPEXChangeNewRenderer (man.disp, self.rd, PEX.MPEXNRABackgroundPixel,
                                 ADR (mpexRA));
    END;
  END SetBackgroundColor;


PROCEDURE SetDepthcueing (self       : T;
                          switch     : BOOLEAN;
                          frontPlane : REAL;
                          backPlane  : REAL;
                          frontScale : REAL; 
                          backScale  : REAL;
                          color      : Color.T) =
  VAR
    dc : PEX.pxlDepthCueEntry;
  BEGIN
    IF switch THEN
      dc.mode := PEX.PEXOn;
    ELSE
      dc.mode := PEX.PEXOff;
    END;

    dc.frontPlane := frontPlane;
    dc.backPlane := backPlane;
    dc.frontScaling := frontScale;
    dc.backScaling := backScale;
    dc.depthCueColour := PexColourSpecifier (color);

    PEX.PEXSetTableEntries (man.disp, self.depthCueLut, PEX.PEXDepthCueLUT, 
                            1, 1, ADR (dc));
  END SetDepthcueing;


PROCEDURE SetMarkerColor (self : T; col : Color.T) =
  VAR
    pexrep := PEX.pexRgbFloatColour {col.r, col.g, col.b};
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    PEX.PEXSetMarkerColour (self.oc, ADR (pexrep));
  END SetMarkerColor;


PROCEDURE SetMarkerScale (self : T; scale : REAL) =
  BEGIN
   <* ASSERT AnimServer.IsServer() *>

   PEX.PEXSetMarkerScale (self.oc, FLOAT (scale, LONGREAL));
  END SetMarkerScale;


PROCEDURE SetMarkerType (self : T; type : MarkerTypeProp.Kind) =
  VAR
    pexrep : Ctypes.int;
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    CASE type OF
    | MarkerTypeProp.Kind.Dot      => pexrep := PEX.PEXMarkerDot;
    | MarkerTypeProp.Kind.Cross    => pexrep := PEX.PEXMarkerCross;
    | MarkerTypeProp.Kind.Asterisk => pexrep := PEX.PEXMarkerAsterisk;
    | MarkerTypeProp.Kind.Circle   => pexrep := PEX.PEXMarkerCircle;
    | MarkerTypeProp.Kind.X        => pexrep := PEX.PEXMarkerX;
    END;
    PEX.PEXSetMarkerType (self.oc, pexrep);
  END SetMarkerType;


PROCEDURE SetLineColor (self : T; col : Color.T) =
  VAR
    pexrep := PEX.pexRgbFloatColour {col.r, col.g, col.b};
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    PEX.PEXSetLineColour (self.oc, ADR (pexrep));
  END SetLineColor;


PROCEDURE SetLineWidth (self : T; scale : REAL) =
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    PEX.PEXSetLineWidth (self.oc, FLOAT (scale, LONGREAL));
  END SetLineWidth;


PROCEDURE SetLineType (self : T; type : LineTypeProp.Kind) =
  VAR
    pexrep : Ctypes.int;
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    CASE type OF
    | LineTypeProp.Kind.Solid   => pexrep := PEX.PEXLineTypeSolid;
    | LineTypeProp.Kind.Dashed  => pexrep := PEX.PEXLineTypeDashed;
    | LineTypeProp.Kind.Dotted  => pexrep := PEX.PEXLineTypeDotted;
    | LineTypeProp.Kind.DashDot => pexrep := PEX.PEXLineTypeDashDot;
    END;
    PEX.PEXSetLineType (self.oc, pexrep);
  END SetLineType;


PROCEDURE SetSurfaceColor (self : T; col : Color.T) =
  VAR
    pexrep := PEX.pexRgbFloatColour {col.r, col.g, col.b};
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    PEX.PEXSetSurfaceColour (self.oc, ADR (pexrep));
  END SetSurfaceColor;


PROCEDURE SetSurfaceBackColor (self : T; col : Color.T) =
  VAR
    pexrep := PEX.pexRgbFloatColour {col.r, col.g, col.b};
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    PEX.PEXSetBFSurfaceColour (self.oc, ADR (pexrep));
  END SetSurfaceBackColor;


PROCEDURE SetRasterMode (self : T; val : RasterModeProp.Kind) =
  VAR
    pexrep : Ctypes.int;
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    CASE val OF
    | RasterModeProp.Kind.Hollow => pexrep := PEX.PEXInteriorStyleHollow;
    | RasterModeProp.Kind.Solid  => pexrep := PEX.PEXInteriorStyleSolid;
    | RasterModeProp.Kind.Empty  => pexrep := PEX.PEXInteriorStyleEmpty;
    END;
    PEX.PEXSetInteriorStyle (self.oc, pexrep);
    PEX.PEXSetBFInteriorStyle (self.oc, pexrep); (* Lyle *)
  END SetRasterMode;


PROCEDURE SetDistinguishFacetsFlag (self : T; val : BOOLEAN) =
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    IF val THEN
      PEX.PEXSetFacetDistinguishFlag (self.oc, X.True);
    ELSE
      PEX.PEXSetFacetDistinguishFlag (self.oc, X.False);
    END;
  END SetDistinguishFacetsFlag;


PROCEDURE SetLighting (self : T; val : BOOLEAN) =
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    IF val THEN
      PEX.PEXSetReflectionModel (self.oc, PEX.PEXReflectionSpecular);
      PEX.PEXSetBFReflectionModel (self.oc, PEX.PEXReflectionSpecular); (* Lyle *)
    ELSE
      PEX.PEXSetReflectionModel (self.oc, PEX.PEXReflectionNoShading);
      PEX.PEXSetBFReflectionModel (self.oc, PEX.PEXReflectionNoShading); (* Lyle *)
    END;
  END SetLighting;


PROCEDURE SetShading (self : T; val : ShadingProp.Kind) =
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    CASE val OF
    | ShadingProp.Kind.Flat => 
      PEX.PEXSetSurfaceInterpMethod (self.oc, PEX.PEXSurfaceInterpNone);
    | ShadingProp.Kind.Gouraud => 
      PEX.PEXSetSurfaceInterpMethod (self.oc, PEX.PEXSurfaceInterpColour);
    END;
  END SetShading;


PROCEDURE SetSurfaceEdgeFlag (self : T; val : BOOLEAN) =
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    IF val THEN
      PEX.PEXSetSurfaceEdgeFlag (self.oc, PEX.PEXOn);
    ELSE
      PEX.PEXSetSurfaceEdgeFlag (self.oc, PEX.PEXOff);
    END;
  END SetSurfaceEdgeFlag;


PROCEDURE SetSurfaceEdgeColor (self : T; val : Color.T) =
  VAR
    pexrep := PEX.pexRgbFloatColour {val.r, val.g, val.b};
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    PEX.PEXSetSurfaceEdgeColour (self.oc, ADR (pexrep));
  END SetSurfaceEdgeColor;


PROCEDURE SetSurfaceEdgeType (self : T; val : LineTypeProp.Kind) =
  VAR
    pexrep : Ctypes.int;
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    CASE val OF
    | LineTypeProp.Kind.Solid   => pexrep := PEX.PEXSurfaceEdgeSolid;
    | LineTypeProp.Kind.Dashed  => pexrep := PEX.PEXSurfaceEdgeDashed;
    | LineTypeProp.Kind.Dotted  => pexrep := PEX.PEXSurfaceEdgeDotted;
    | LineTypeProp.Kind.DashDot => pexrep := PEX.PEXSurfaceEdgeDashDot;
    END;
    PEX.PEXSetSurfaceEdgeType (self.oc, pexrep);
  END SetSurfaceEdgeType;


PROCEDURE SetSurfaceEdgeWidth (self : T; val : REAL) =
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    PEX.PEXSetSurfaceEdgeWidth (self.oc, FLOAT (val, LONGREAL));
  END SetSurfaceEdgeWidth;


PROCEDURE SetAmbientReflCoeff (self : T; val : REAL) =
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    self.surfRefl.ambient := val;
  END SetAmbientReflCoeff;


PROCEDURE SetDiffuseReflCoeff (self : T; val : REAL) =
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    self.surfRefl.diffuse := val;
  END SetDiffuseReflCoeff;


PROCEDURE SetSpecularReflCoeff (self : T; val : REAL) =
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    self.surfRefl.specular := val;
  END SetSpecularReflCoeff;


PROCEDURE SetSpecularReflConc (self : T; val : REAL) =
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    self.surfRefl.specularConc := val;
  END SetSpecularReflConc;


PROCEDURE SetSpecularReflColor (self : T; val : Color.T) =
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    self.surfRefl.specularColour := PexColourSpecifier (val);
  END SetSpecularReflColor;


PROCEDURE SetTransmissionCoeff (self : T; val : REAL) =
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    self.surfRefl.transmission := val;
  END SetTransmissionCoeff;


PROCEDURE DrawMarker (self : T; p : Point3.T) =
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    PEX.PEXMarkers (self.oc, ADR (p), 1);
  END DrawMarker;


PROCEDURE DrawLine (self : T; p1, p2 : Point3.T) =
  VAR
    line := ARRAY [1 .. 2] OF PEX.pxlCoord3D {p1, p2};
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    PEX.PEXPolyline (self.oc, ADR (line), 2);
  END DrawLine;


PROCEDURE DrawPolygon (self         : T; 
                       READONLY pts : ARRAY OF Point3.T; 
                       shape        : GO.Shape) = 
  VAR
    pexrep : Ctypes.int;
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    CASE shape OF
    | GO.Shape.Complex   => pexrep := PEX.PEXComplex;
    | GO.Shape.NonConvex => pexrep := PEX.PEXNonconvex;
    | GO.Shape.Convex    => pexrep := PEX.PEXConvex;
    | GO.Shape.Unknown   => pexrep := PEX.PEXUnknownShape;
    END;
    PEX.PEXFillArea (self.oc, pexrep, X.False, ADR (pts[0]), NUMBER (pts));
  END DrawPolygon;


PROCEDURE DrawQuadMesh (self         : T; 
                        READONLY pts : ARRAY OF ARRAY OF Point3.T; 
                        shape        : GO.Shape) =
  VAR
    pexrep : Ctypes.int;
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    CASE shape OF
    | GO.Shape.Complex   => pexrep := PEX.PEXComplex;
    | GO.Shape.NonConvex => pexrep := PEX.PEXNonconvex;
    | GO.Shape.Convex    => pexrep := PEX.PEXConvex;
    | GO.Shape.Unknown   => pexrep := PEX.PEXUnknownShape;
    END;
    PEX.PEXQuadMesh (self.oc, pexrep, 0, 0, NIL, ADR(pts[0][0]), 
                     NUMBER (pts), NUMBER (pts[0]));
  END DrawQuadMesh;


PROCEDURE DrawColoredQuadMesh (         self  : T; 
                               READONLY points: ARRAY OF ARRAY OF Point3.T;
                               READONLY colors: ARRAY OF ARRAY OF Color.T;
                                        shape : GO.Shape) =
  VAR
    pexrep : Ctypes.int;
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    CASE shape OF
    | GO.Shape.Complex   => pexrep := PEX.PEXComplex;
    | GO.Shape.NonConvex => pexrep := PEX.PEXNonconvex;
    | GO.Shape.Convex    => pexrep := PEX.PEXConvex;
    | GO.Shape.Unknown   => pexrep := PEX.PEXUnknownShape;
    END;
    PEX.PEXQuadMesh (self.oc, pexrep, PEX.PEXGAColour, 0, 
                     ADR(colors[0][0]), ADR(points[0][0]),
                     NUMBER(points), NUMBER(points[0]));
  END DrawColoredQuadMesh;


(*****************************************************************************)
(* The sphere caching mechanism                                              *)
(*****************************************************************************)

TYPE
  VertexData  = RECORD
    pt   : PEX.pexCoord3D;
    norm : PEX.pexCoord3D;
  END;

PROCEDURE DrawProtoSphere (self : T; prec : INTEGER) =
  VAR
    dl   : DisplayList;
    list : StructureList;
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    list := self.sphereStructures;
    WHILE list # NIL DO
      IF list.prec = prec THEN
        PEX.PEXExecuteStructure (self.oc, list.dl.structure);
        RETURN;
      END;
      list := list.next;
    END;
    dl := NewDisplayList (self);
    WITH verts = ComputeUnitSphere (prec) DO
      FOR i := FIRST (verts^) TO LAST (verts^) DO
        PEX.PEXTriangleStrip (dl.ocbuf, 0, PEX.PEXGANormal, NIL, 
                              ADR (verts[i][0]), NUMBER(verts[i]));
      END;
    END;
    self.sphereStructures := NEW (StructureList, 
                                  prec := prec, 
                                  dl   := dl, 
                                  next := self.sphereStructures);
    PEX.PEXExecuteStructure (self.oc, dl.structure);
  END DrawProtoSphere;


PROCEDURE ComputeUnitSphere (prec : INTEGER) : REF ARRAY OF ARRAY OF VertexData =
  CONST
    YMAX           =  1.0;
    YMIN           = -1.0;
  VAR
    Y, DY                : REAL;
    vertexTop, vertexBot : REF ARRAY OF Point3.T;
    verts                : REF ARRAY OF ARRAY OF VertexData;
  BEGIN
    vertexTop := NEW(REF ARRAY OF Point3.T, prec);
    vertexBot := NEW(REF ARRAY OF Point3.T, prec);
    verts     := NEW(REF ARRAY OF ARRAY OF VertexData, prec, 2 * prec + 2);

    (* compute the number of triangle strip *)
    DY := (YMAX - YMIN) / FLOAT(prec);

    CalSphereVertex (vertexTop^, YMAX);
    Y := YMAX - DY;

    FOR i := 0 TO prec - 1 DO
      CalSphereVertex (vertexBot^, Y);

      (* build triangle strip data *)
      FOR j := 0 TO prec - 1 DO
        verts[i][2*j  ] := VertexData {vertexBot[j], vertexBot[j]};
        verts[i][2*j+1] := VertexData {vertexTop[j], vertexTop[j]};
      END;
      verts[i][2*prec  ] := VertexData {vertexBot[0], vertexBot[0]};
      verts[i][2*prec+1] := VertexData {vertexTop[0], vertexBot[0]};

      (* set up for next triangle strip *)
      vertexTop^ := vertexBot^;
      Y := Y - DY;
    END;
    RETURN verts;
  END ComputeUnitSphere;


PROCEDURE CalSphereVertex(VAR vertex : ARRAY OF Point3.T; y : REAL) =
  VAR
    dPhi := 2.0 * Math.Pi / FLOAT (NUMBER (vertex));
    phi  := 0.0;
    r    := 1.0 - y * y;
  BEGIN
    (*
     * This function samples the surface y = -1/r where
     * r is the radius of the circle, i.e. r**2 = x**2 + y**2.
     *)
    IF r < 0.00001 THEN
      r := 0.0;
    END;
    r := FLOAT (Math.sqrt (FLOAT (r, LONGREAL)));
    FOR i := FIRST (vertex) TO LAST (vertex) DO
      vertex[i].x := r * FLOAT (Math.cos (FLOAT (phi, LONGREAL)));
      vertex[i].z := r * FLOAT (Math.sin (FLOAT (phi, LONGREAL)));
      vertex[i].y := y;
      phi := phi + dPhi;
    END;
  END CalSphereVertex;


(*****************************************************************************)
(* The cone caching mechanism                                                *)
(*****************************************************************************)


TYPE
  ConeVertices = REF ARRAY OF ARRAY [1 .. 2] OF VertexData;

PROCEDURE DrawProtoCone (self : T; prec : INTEGER) =
  VAR
    dl   : DisplayList;
    list : StructureList;
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    list := self.coneStructures;
    WHILE list # NIL DO
      IF list.prec = prec THEN
        PEX.PEXExecuteStructure (self.oc, list.dl.structure);
        RETURN;
      END;
      list := list.next;
    END;
    dl := NewDisplayList (self);
    WITH verts = ComputeUnitCone (prec) DO
      PEX.PEXQuadMesh (dl.ocbuf, PEX.PEXConvex, 0, PEX.PEXGANormal, NIL,
                       ADR (verts[0][1]), NUMBER (verts^), 2);
    END;
    self.coneStructures := NEW (StructureList, 
                                prec := prec, 
                                dl   := dl, 
                                next := self.coneStructures);
    PEX.PEXExecuteStructure (self.oc, dl.structure);
  END DrawProtoCone;


(* ComputeUnitCone is called once for each (state,precision) pair. *)

PROCEDURE ComputeUnitCone (prec : INTEGER) : ConeVertices =
  VAR
    v  := NEW (ConeVertices, prec + 1);
  BEGIN
    WITH v1 = AuxG.GetUnitCirclePoints (prec) DO
      FOR i := FIRST(v1^) TO LAST(v1^) DO
        WITH p = v1[i], n = Point3.T {-p.x, -p.y, -1.0} DO
          v[i][1] := VertexData {p, n};
          v[i][2] := VertexData {Point3.T {0.0, 0.0, 1.0}, n};
        END;
      END;
    END;
    RETURN v;
  END ComputeUnitCone;


(*****************************************************************************)
(* The Cylinder caching mechanism                                            *)
(*****************************************************************************)

TYPE
  CylVertices = REF ARRAY OF ARRAY [1 .. 2] OF VertexData;


PROCEDURE DrawProtoCylinder (self : T; prec : INTEGER) =
  VAR
    dl   : DisplayList;
    list : StructureList;
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    list := self.cylinderStructures;
    WHILE list # NIL DO
      IF list.prec = prec THEN
        PEX.PEXExecuteStructure (self.oc, list.dl.structure);
        RETURN;
      END;
      list := list.next;
    END;
    dl := NewDisplayList (self);
    WITH verts = ComputeUnitCylinder (prec) DO
      PEX.PEXQuadMesh (dl.ocbuf, PEX.PEXConvex, 0, PEX.PEXGANormal, NIL,
                       ADR (verts[0][1]), NUMBER (verts^), 2);
    END;
    self.cylinderStructures := NEW (StructureList, 
                                    prec := prec, 
                                    dl   := dl, 
                                    next := self.cylinderStructures);
    PEX.PEXExecuteStructure (self.oc, dl.structure);
  END DrawProtoCylinder;


PROCEDURE ComputeUnitCylinder (prec : INTEGER) : CylVertices =
  VAR
    v  := NEW (CylVertices, prec + 1);
  BEGIN
    WITH v1 = AuxG.GetUnitCirclePoints (prec) DO
      FOR i := FIRST(v1^) TO LAST(v1^) DO
        WITH p = v1[i], n = Point3.T{-p.x, -p.y, -p.z} DO
          v[i][1] := VertexData {p, n};
          v[i][2] := VertexData {Point3.T{p.x, p.y, p.z + 1.0}, n};
        END;
      END;
    END;
    RETURN v;
  END ComputeUnitCylinder;


(*****************************************************************************)
(* The disk caching mechanism                                                *)
(*****************************************************************************)


PROCEDURE DrawProtoDisk (self : T; prec : INTEGER) =
  VAR
    dl   : DisplayList;
    list : StructureList;
  BEGIN
    <* ASSERT AnimServer.IsServer() *>

    list := self.diskStructures;
    WHILE list # NIL DO
      IF list.prec = prec THEN
        PEX.PEXExecuteStructure (self.oc, list.dl.structure);
        RETURN;
      END;
      list := list.next;
    END;
    dl := NewDisplayList (self);
    WITH pts = AuxG.GetUnitCirclePoints (prec) DO
      PEX.PEXFillArea (dl.ocbuf, PEX.PEXConvex, X.False, ADR (pts[0]), prec);
    END;
    self.diskStructures := NEW (StructureList, 
                                prec := prec, 
                                dl   := dl, 
                                next := self.diskStructures);
    PEX.PEXExecuteStructure (self.oc, dl.structure);
  END DrawProtoDisk;


(*****************************************************************************)
(* The torus caching mechanism                                               *)
(*****************************************************************************)


TYPE
  TorusStructure = RECORD
    prec        : INTEGER;
    radiusRatio : REAL;
    dl          : DisplayList := NIL;
  END;
  TorusStructureCache = RECORD
    last: INTEGER := 0;
    elem: ARRAY [1 .. 10] OF TorusStructure;
  END;

  TorusVertices = REF ARRAY OF ARRAY OF VertexData;

PROCEDURE DrawProtoTorus (self : T; prec : INTEGER; radiusRatio : REAL ) =
  BEGIN
    <* ASSERT AnimServer.IsServer() *>
(***
    WITH t = self.torusStructures DO
      FOR i := 1 TO t.last DO
        IF t.elem[i].prec = prec AND t.elem[i].radiusRatio = radiusRatio THEN
          PEX.PEXExecuteStructure (self.oc, t.elem[i].dl.structure);
          VAR 
            tmp := t.elem[i];
          BEGIN
            SUBARRAY (t.elem, 1, i-1) := SUBARRAY (t.elem, 0, i-1);
            t.elem[1] := tmp;
          END;
          RETURN;
        END;
      END;

      (* The cache does not contain a matching element. We shift all elements 
         one position back, and put a new element in the most-recently-used 
         position.  If the cache is full, we discard the least-recently used 
         element, and recycle its display list. *)

      IF t.last := LAST(t.elem) THEN
        (* use the display list of the least-recently-used element *)
        dl := t.elem[t.last];
        PEX.PEXDeleteElements (man.disp, dl.structure, 
                               PEX.PEXBeginning, 0, PEX.PEXEnd, 0);
      ELSE
        dl := NewDisplayList (self);
      END;

      WITH verts = ComputeUnitTorus (prec, radiusRatio),
           last  = MIN (t.last, LAST(t.elem) - 1) DO
        PEX.PEXQuadMesh (dl.ocbuf, PEX.PEXConvex, 0, PEX.PEXGANormal, 
                         NIL, ADR (verts[0][0]), 
                         NUMBER (verts^), NUMBER(verts[0]));
        PEX.PEXExecuteStructure (self.oc, dl.structure);
        
        SUBARRAY (t.elem, 1, last) := SUBARRAY (t.elem, 0, last);
        t.elem[1] := TorusStructure{prec, radiusRatio, dl};
        t.last := last + 1;
      END;
    END;
***)
    WITH verts = ComputeUnitTorus (prec, radiusRatio) DO
      PEX.PEXQuadMesh (self.oc, PEX.PEXConvex, 0, PEX.PEXGANormal, 
                       NIL, ADR (verts[0][0]), 
                       NUMBER (verts^), NUMBER(verts[0]));
    END;
  END DrawProtoTorus;


(* ComputeUnitTorus is called once for each (state,precision,radiusRatio) 
   triple. The constant parameters are:
     center  = (0,0,0)
     normal  = (1,0,0)
     radius1 = 1.0
*)

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
(* Low-level conversion functions to PEX types                               *)
(*****************************************************************************)


PROCEDURE PexColourSpecifier (c : Color.T) : PEX.pxlColourSpecifier =
  VAR
    cs : PEX.pxlColourSpecifier;
    c0 := PEX.pexRgbFloatColour {c.r, c.g, c.b};
  BEGIN
    cs.colourType := PEX.PEXRgbFloatColour;
    cs.colour     := LOOPHOLE (ADR (c0), PEX.pexColourStar)^;
    RETURN cs;
  END PexColourSpecifier;


(*****************************************************************************)
(* Event handling                                                            *)
(*****************************************************************************)

PROCEDURE ProcessEvents (self : T) =

  PROCEDURE CheckTypedWindowEvent (self : T;
                                   type : Ctypes.int; 
                                   VAR event : X.XEvent) : X.Bool =
    BEGIN
      LOCK man DO
        RETURN X.XCheckTypedWindowEvent (man.disp, self.window, type, ADR (event));
      END;
    END CheckTypedWindowEvent;

  PROCEDURE CheckWindowEvent (self : T;
                              mask : Ctypes.long; 
                              VAR event : X.XEvent) : X.Bool =
    BEGIN
      LOCK man DO
        RETURN X.XCheckWindowEvent (man.disp, self.window, mask, ADR (event));
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
    mask := Word.Or (mask, X.ExposureMask);
    mask := Word.Or (mask, X.PointerMotionMask);   (* every motion! *)
    mask := Word.Or (mask, X.ButtonPressMask);
    mask := Word.Or (mask, X.ButtonReleaseMask);
    mask := Word.Or (mask, X.KeyPressMask);
    mask := Word.Or (mask, X.KeyReleaseMask);

    (*
     * For some reason, ClientMessage events are not picked up by
     * X.XCheckWindowEvent, so I take care of them here.
     *)

    IF CheckTypedWindowEvent (self, X.ClientMessage, ev) = X.True THEN
      WITH e = ClientMessageEvent(ev) DO
        IF e.message_type = man.wm_protocols AND e.format = 32 THEN
          WITH data = LOOPHOLE (e.data, ARRAY [0 .. 4] OF Ctypes.long) DO
            IF data[0] = man.wm_delete_window THEN
              self.destroy ();
              RETURN;
            END;
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
        WITH exev = LOOPHOLE (ADR (ev), X.XExposeEventStar)^ DO
          (*** Optimization: redisplay only if this is last expose event ***)
          IF exev.count = 0 THEN
            LOCK man DO
              ShowWindow (self);
            END;
          END;
        END;
      | X.MotionNotify =>
        (*** If several motions in queue, jump to last ***)
        WHILE CheckWindowEvent (self, X.PointerMotionMask, ev) # X.False DO END;

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
PROCEDURE MotionEvent(VAR ev : X.XEvent) : X.XMotionEvent =
  BEGIN
    RETURN LOOPHOLE (ADR (ev), X.XMotionEventStar)^;
  END MotionEvent;


<* INLINE *>
PROCEDURE ButtonEvent(VAR ev : X.XEvent) : X.XButtonEvent =
  BEGIN
    RETURN LOOPHOLE (ADR (ev), X.XButtonEventStar)^;
  END ButtonEvent;


<* INLINE *>
PROCEDURE ClientMessageEvent(VAR ev : X.XEvent) : X.XClientMessageEvent =
  BEGIN
    RETURN LOOPHOLE (ADR (ev), X.XClientMessageEventStar)^;
  END ClientMessageEvent;


(*****************************************************************************)
(* Animation Server                                                          *)
(*****************************************************************************)

PROCEDURE Repair (self : T; VAR damaged : BOOLEAN) =

  PROCEDURE FillOcBuffer () =
    BEGIN
      LOCK man DO
          (*** determine the object's current transparency ***)
          self.transflag := self.root.needsTransparency(0.0);  
                                   (* 0.0 is the default transmission coeff *)

          (*** set up the rendering pipeline for a new round ***)
          self.setup ();

          (*** reset the bounding volume ***)
          self.resetBoundingVolume ();

          (*** draw the object into the oc buffer ***)
          self.root.draw (self);

          (*** now set up the camera ***)
          self.setupCamera ();

          (*** establish the light sources ***)
          self.establishLights ();

          (*** show the result of the drawing ***)
          ShowWindow (self);
      END;
    END FillOcBuffer;

  BEGIN
    IF self.root # NIL THEN
      (*** Redraw the scene only if it was damaged ***)
      IF self.root.damaged THEN
        damaged := TRUE;
        (* Fill the output command buffer. We could inline this call. *)
        FillOcBuffer ();
      END;
    END;
  END Repair;


(*****************************************************************************)
(* Manager                                                                   *)
(*****************************************************************************)


TYPE 
  Manager = MUTEX OBJECT              (* mutex synchronizes access to disp *)
    disp             : X.DisplayStar;
    avail            : BOOLEAN;
    wm_protocols     : X.Atom;
    wm_delete_window : X.Atom;
  METHODS
    init () : Manager := InitManager;
  END;


PROCEDURE InitManager (self : Manager) : Manager =
  VAR
    pexinfo : PEX.pxlInfoStar;       (* PEX initialization info *)
  BEGIN
    (*** open the display ***)

    self.disp := X.XOpenDisplay (NIL);
    IF self.disp = NIL THEN
      Process.Crash ("Could not open display");
    END;

    (*** "internalize" some X atoms ***)

    self.wm_protocols := 
       X.XInternAtom(self.disp,M3toC.TtoS ("WM_PROTOCOLS"), X.False);
    self.wm_delete_window := 
       X.XInternAtom(self.disp,M3toC.TtoS ("WM_DELETE_WINDOW"), X.False);

    (* Check whether the "X3D-PEX" extension is available, and perform some
       PEX initializations that don't depend on a connection. *)
    
    self.avail := PEX.PEXInitialize (self.disp, ADR (pexinfo)) = 0;
    PEX.PEXSetColourType (PEX.PEXRgbFloatColour); 

    RETURN self;
  END InitManager;


(******************************************************************************
 The following procedures are based on the C functions accompanying the
 "PEXlib Programming Manual" by O'Reilly & Associates. Here is their copyright 
 notice:

    Copyright 1992, 1993 O'Reilly and Associates, Inc.  Permission to
    use, copy, and modify this program is hereby granted, as long as
    this copyright notice appears in each copy of the program source
    code.

******************************************************************************)


(*
 * Find the best visual. The best visual is the visual that supports the most
 * colors. If two visuals support the same number of colors, we prefer 
 * TrueColor over DirectColor over PseudoColor over StaticColor over GrayScale
 * over StaticGray.
 *)
PROCEDURE FindBestVisual (dpy : X.DisplayStar) : X.XVisualInfo =

  PROCEDURE Ranking8 (class : Ctypes.int) : INTEGER =
    BEGIN
      CASE class OF
      | -1            => RETURN -1;
      | X.StaticGray  => RETURN 1;
      | X.GrayScale   => RETURN 2;
      | X.StaticColor => RETURN 3;
      | X.DirectColor => RETURN 4;
      | X.TrueColor   => RETURN 5;
      | X.PseudoColor => RETURN 6;
      ELSE
        <* ASSERT FALSE *>
      END;
    END Ranking8;

  PROCEDURE Ranking (class : Ctypes.int) : INTEGER =
    BEGIN
      CASE class OF
      | -1            => RETURN -1;
      | X.StaticGray  => RETURN 1;
      | X.GrayScale   => RETURN 2;
      | X.StaticColor => RETURN 3;
      | X.PseudoColor => RETURN 4;
      | X.DirectColor => RETURN 5;
      | X.TrueColor   => RETURN 6;
      ELSE
        <* ASSERT FALSE *>
      END;
    END Ranking;

  VAR
    vis_templ  : X.XVisualInfo; (* input-template for XGetVisualInfo *)
    numVisuals : Ctypes.int;    (* number of visuals supported by the screen *)
    size       : INTEGER;       (* no. of colors supported by current visual *)
    bestVisual : X.XVisualInfo; (* the "best" visual *)
    bestSize   : INTEGER;       (* number of colors supported by bestVisual *)
    maxDepth   := 0;
  BEGIN
    (* Get all the visuals for the screen. *)
    vis_templ.screen := X.XDefaultScreen (dpy);
    WITH 
      visListPtr = X.XGetVisualInfo (dpy, X.VisualScreenMask, 
                                     (* IN *)  ADR (vis_templ),
                                     (* OUT *) ADR (numVisuals)),
      visListRef = LOOPHOLE (visListPtr, 
                             UNTRACED REF ARRAY [1 .. 1000] OF X.XVisualInfo),
      visuals = SUBARRAY (visListRef^, 0, numVisuals) DO

      (* Determine the max. depth of all the visuals. *)
      FOR i := FIRST (visuals) TO LAST (visuals) DO 
        maxDepth := MAX (maxDepth, visuals[i].depth);
      END;

      (* Determine the best visual available.  The best one is the *)
      (* one with the most colors and highest capabilities. *)
      bestSize := 0;
      bestVisual.class := -1;
      FOR i := FIRST (visuals) TO LAST (visuals) DO 
        (* Determine the number of colors supported by visuals[i] *)
        CASE visuals[i].class OF
        | X.TrueColor, X.DirectColor =>
          size := Word.Or (visuals[i].red_mask, 
                           Word.Or (visuals[i].green_mask, 
                                    visuals[i].blue_mask)) + 1;
        ELSE
          size := visuals[i].colormap_size;
        END;

        (* Choose this one if it is better. *)
        IF size > bestSize THEN
          bestVisual := visuals[i];
          bestSize := size;
        ELSIF size = bestSize THEN
          IF maxDepth = 8 THEN
            IF Ranking8 (visuals[i].class) >= Ranking8 (bestVisual.class) THEN
              bestVisual := visuals[i];
              bestSize := size;
            END;
          ELSE
            IF Ranking (visuals[i].class) >= Ranking (bestVisual.class) THEN
              bestVisual := visuals[i];
              bestSize := size;
            END;
          END;
        END;
      END;
      RETURN bestVisual;
    END;
  END FindBestVisual;


EXCEPTION CmapAllocError;


PROCEDURE CreateColorMap (dpy                     : X.DisplayStar;
                          READONLY visual         : X.XVisualInfo;
                          (* OUT *) VAR cmap_info : X.XStandardColormap;
                          (* OUT *) VAR capx_info : PEX.pexColourApproxEntry) 
    RAISES {CmapAllocError} =
  BEGIN 
    CASE visual.class OF
    | X.DirectColor =>
      (* Create the largest possible equal-length ramps. *)
      CreateDirectMap (dpy, visual, cmap_info, capx_info);
    | X.PseudoColor =>
      (* Create the largest NxNxN color sampling. *)
      WITH n = TRUNC (Math.pow (FLOAT (visual.colormap_size - 1, LONGREAL),
                                1.0d0 / 3.0d0)) DO 
        CreatePseudoMap (dpy, visual, n, n, n, cmap_info, capx_info);
      END;
    | X.GrayScale =>
      (* Create a GrayScale colormap with max number of grays. *)
      (* (but leave one empty spot for the background color.) *)
      CreateGrayMap (dpy, visual, visual.colormap_size - 1, 
                     cmap_info, capx_info);
    | X.TrueColor, X.StaticColor, X.StaticGray =>
      CreateReadOnlyMap (dpy, visual, cmap_info, capx_info );
    ELSE
      <* ASSERT FALSE *>
    END;
  END CreateColorMap;


PROCEDURE CreateDirectMap (dpy                     : X.DisplayStar;
                           READONLY visual         : X.XVisualInfo;
                           (* OUT *) VAR cmap_info : X.XStandardColormap;
                           (* OUT *) VAR capx_info : PEX.pexColourApproxEntry) 
    RAISES {CmapAllocError} =
  VAR
    red_planes, green_planes, blue_planes       : Ctypes.int;
    i                                           : Ctypes.unsigned_long;
    num_reds, num_greens, num_blues, num_colors : Ctypes.unsigned_long;
    rshift, gshift, bshift                      : Ctypes.unsigned_long;
    rmask, gmask, bmask                         : Ctypes.unsigned_long;
  BEGIN
    (* Create the colormap. *)
    cmap_info.visualid := visual.visualid;
    cmap_info.colormap := X.XCreateColormap (
                              dpy, 
                              X.XRootWindow (dpy, X.XDefaultScreen (dpy)), 
                              visual.visual, 
                              X.AllocNone );

    (* Determine the number of red, green, and blue planes and the *)
    (* maximum possible number of color values for each. *)
    red_planes := 0; 
    num_reds := 1;
    i := visual.red_mask;
    WHILE i > 0 DO
      IF Word.And (i, 1) # 0 THEN
        INC (red_planes);
        num_reds := num_reds * 2;
      END;
      i := Word.RightShift (i, 1);
    END;
    cmap_info.red_max := num_reds - 1;
	
    green_planes := 0; 
    num_greens := 1;
    i := visual.green_mask;
    WHILE i > 0 DO
      IF Word.And (i, 1) # 0 THEN
        INC (green_planes);
        num_greens := num_greens * 2;
      END;
      i := Word.RightShift (i, 1);
    END;
    cmap_info.green_max := num_greens - 1;
	
    blue_planes := 0; 
    num_blues := 1;
    i := visual.blue_mask;
    WHILE i > 0 DO
      IF Word.And (i, 1) # 0 THEN
        INC (blue_planes);
        num_blues := num_blues * 2;
      END;
      i := Word.RightShift (i, 1);
    END;
    cmap_info.blue_max := num_blues - 1;
	
    (* Allocate the planes. *)
    IF X.XAllocColorPlanes (dpy, cmap_info.colormap, X.True,
                            ADR (cmap_info.base_pixel), 1,
                            red_planes, green_planes, blue_planes,
                            ADR (rmask), ADR (gmask), ADR (bmask)) = 0 THEN
      RAISE CmapAllocError;
    END;

    (* Determine the red, green, and blue multipliers by finding the first 
       bit set in each mask. *)
    
    rshift := 0;
    WHILE Word.And (rmask, Word.LeftShift (1, rshift)) = 0 DO
      INC (rshift);
    END;
    cmap_info.red_mult := Word.LeftShift (1, rshift);

    gshift := 0;
    WHILE Word.And (rmask, Word.LeftShift (1, gshift)) = 0 DO
      INC (gshift);
    END;
    cmap_info.green_mult := Word.LeftShift (1, gshift);
    
    bshift := 0;
    WHILE Word.And (rmask, Word.LeftShift (1, bshift)) = 0 DO
      INC (bshift);
    END;
    cmap_info.blue_mult := Word.LeftShift (1, bshift);

    (* Store the colors in the colormap. *)
    num_colors := visual.colormap_size;
    WITH colors = NEW (REF ARRAY OF X.XColor, num_colors)^ DO
      FOR i := FIRST (colors) TO LAST (colors) DO
        WITH color = colors[i] DO
          color.flags := 0;
          color.pixel := cmap_info.base_pixel;
          IF i < cmap_info.red_max THEN
            color.flags := Word.Or (color.flags, X.DoRed);
            color.pixel := Word.Or (color.pixel, Word.LeftShift (i, rshift));
            color.red := (i * 65535) DIV cmap_info.red_max;
          END;
          IF i < cmap_info.green_max THEN
            color.flags := Word.Or (color.flags, X.DoGreen);
            color.pixel := Word.Or (color.pixel, Word.LeftShift (i, gshift));
            color.green := (i * 65535) DIV cmap_info.green_max;
          END;
          IF i < cmap_info.blue_max THEN
            color.flags := Word.Or (color.flags, X.DoBlue);
            color.pixel := Word.Or (color.pixel, Word.LeftShift (i, bshift));
            color.blue := (i * 65535) DIV cmap_info.blue_max;
          END;
        END;
      END;
      X.XStoreColors (dpy, cmap_info.colormap, ADR (colors[0]), num_colors );
    END;
    
    (* Fill in the color approximation information. *)
    capx_info.approxType  := PEX.PEXColourSpace;
    capx_info.approxModel := PEX.PEXColourApproxRGB;
    capx_info.dither      := PEX.PEXOn;
    capx_info.basePixel   := cmap_info.base_pixel;
    capx_info.max1        := cmap_info.red_max;
    capx_info.max2        := cmap_info.green_max;
    capx_info.max3        := cmap_info.blue_max;
    capx_info.weight1     := 0.0; (* not used by PEXColorSpace *)
    capx_info.weight2     := 0.0; (* not used by PEXColorSpace *)
    capx_info.weight3     := 0.0; (* not used by PEXColorSpace *)
    capx_info.mult1       := cmap_info.red_mult;
    capx_info.mult2       := cmap_info.green_mult;
    capx_info.mult3       := cmap_info.blue_mult;
  END CreateDirectMap;


PROCEDURE CreatePseudoMap (dpy                     : X.DisplayStar;
                           READONLY visual         : X.XVisualInfo;
                           nr, ng, nb              : INTEGER;
                           (* OUT *) VAR cmap_info : X.XStandardColormap;
                           (* OUT *) VAR capx_info : PEX.pexColourApproxEntry) 
    RAISES {CmapAllocError} =
  VAR
    num_colors, idx, p : INTEGER;
  BEGIN
    (* Create the colormap and fill in the standard cmap info. *)
    cmap_info.colormap := X.XCreateColormap (
                               dpy,
                               X.XRootWindow (dpy, X.XDefaultScreen (dpy)),
                               visual.visual, 
                               X.AllocNone);
    cmap_info.visualid := visual.visualid;
    cmap_info.blue_max := nb - 1;	
    cmap_info.blue_mult := 1;
    cmap_info.green_max := ng - 1;	
    cmap_info.green_mult := nb;
    cmap_info.red_max := nr - 1;	
    cmap_info.red_mult := nb * ng;

    num_colors := nr * ng * nb;
    WITH pixels = NEW (REF ARRAY OF Ctypes.unsigned_long, num_colors)^ DO
      IF X.XAllocColorCells (dpy, cmap_info.colormap, X.True, NIL,
                             0, ADR (pixels[0]), num_colors) = 0 THEN
        RAISE CmapAllocError;
      END;
      cmap_info.base_pixel := pixels[0];
    END;

    WITH colors = NEW (REF ARRAY OF X.XColor, num_colors)^ DO
      p := cmap_info.base_pixel;
      idx := 0;
      FOR i := 0 TO nr - 1 DO
        FOR j := 0 TO ng - 1 DO
          FOR k := 0 TO nb - 1 DO
            WITH color = colors[idx] DO
              color.flags := Word.Or (X.DoRed, Word.Or (X.DoGreen, X.DoBlue));
              color.pixel := p;
              INC (p);
              color.red   := (i * 65535) DIV cmap_info.red_max;
              color.green := (j * 65535) DIV cmap_info.green_max;
              color.blue  := (k * 65535) DIV cmap_info.blue_max;
              INC (idx);
            END;
          END;
        END;
      END;
      X.XStoreColors (dpy, cmap_info.colormap, ADR (colors[0]), num_colors);
    END;
    
    (* Fill in the color approximation information. *)
    capx_info.approxType  := PEX.PEXColourSpace;
    capx_info.approxModel := PEX.PEXColourApproxRGB;
    capx_info.dither      := PEX.PEXOn;
    capx_info.basePixel   := cmap_info.base_pixel;
    capx_info.max1        := cmap_info.red_max;
    capx_info.max2        := cmap_info.green_max;
    capx_info.max3        := cmap_info.blue_max;
    capx_info.weight1     := 0.0; (* not used by PEXColorSpace *)
    capx_info.weight2     := 0.0; (* not used by PEXColorSpace *)
    capx_info.weight3     := 0.0; (* not used by PEXColorSpace *)
    capx_info.mult1       := cmap_info.red_mult;
    capx_info.mult2       := cmap_info.green_mult;
    capx_info.mult3       := cmap_info.blue_mult;
  END CreatePseudoMap;


PROCEDURE CreateGrayMap (dpy                     : X.DisplayStar;
                         READONLY visual         : X.XVisualInfo;
                         num_grays               : INTEGER;
                         (* OUT *) VAR cmap_info : X.XStandardColormap;
                         (* OUT *) VAR capx_info : PEX.pexColourApproxEntry) 
    RAISES {CmapAllocError} =
  VAR
    p : Ctypes.unsigned_long;
  BEGIN
    cmap_info.visualid := visual.visualid;
    cmap_info.colormap := X.XCreateColormap (
                              dpy,
                              X.XRootWindow (dpy, X.XDefaultScreen (dpy)),
                              visual.visual, 
                              X.AllocNone);
    cmap_info.red_max  := num_grays - 1;
    cmap_info.red_mult := 1;

    WITH pixels = NEW (REF ARRAY OF Ctypes.unsigned_long, num_grays)^ DO
      IF X.XAllocColorCells (dpy, cmap_info.colormap, X.True,
                             NIL, 0, ADR (pixels[0]), num_grays) = 0 THEN
        RAISE CmapAllocError;
      END;
      cmap_info.base_pixel := pixels[0];                    
    END;
	
    (* Fill in the RGB color values. *)
    WITH colors = NEW (REF ARRAY OF X.XColor, num_grays)^ DO
      p := cmap_info.base_pixel;
      FOR i := FIRST (colors) TO LAST (colors) DO
        WITH color = colors[i] DO
          color.flags := Word.Or (X.DoRed, Word.Or (X.DoGreen, X.DoBlue));
          color.pixel := p;
          INC (p);
          (* R, G, and B are the same intensity within a cell. *)
          color.red := (i * 65535) DIV (num_grays - 1);
          color.green := color.red;
          color.blue  := color.red;
	END;
      END;    
      X.XStoreColors (dpy, cmap_info.colormap, ADR (colors[0]), num_grays);
    END;
    
    (* Fill in the color approximation information. *)
    capx_info.approxType  := PEX.PEXColourRange;
    capx_info.approxModel := PEX.PEXColourApproxRGB;
    capx_info.dither      := PEX.PEXOn;
    capx_info.basePixel   := cmap_info.base_pixel;
    capx_info.max1        := num_grays - 1;
    capx_info.max2        := 0; (* not used by PEXColorRange *)
    capx_info.max3        := 0; (* not used by PEXColorRange *)
    (* Give the weights the NTSC intensity coefficients. *)
    capx_info.weight1     := 0.299;
    capx_info.weight2     := 0.587;
    capx_info.weight3     := 0.114;
    capx_info.mult1       := 1;
    capx_info.mult2       := 0;
    capx_info.mult3       := 0;
  END CreateGrayMap;


PROCEDURE CreateReadOnlyMap (dpy                     : X.DisplayStar;
                             READONLY visual         : X.XVisualInfo;
                             (* OUT *) VAR cmap_info : X.XStandardColormap;
                             (* OUT *) VAR capx_info : PEX.pexColourApproxEntry) =
  BEGIN
    (* Create the colormap. *)
    cmap_info.colormap := X.XCreateColormap (
                              dpy,
                              X.XRootWindow (dpy, X.XDefaultScreen (dpy)),
                              visual.visual, X.AllocNone);
    
    (* Set up the colormap and color approximation info. *)
    cmap_info.base_pixel := 0;
    cmap_info.visualid := visual.visualid;
    

    (* The rest depends on the visual class. *)
    CASE visual.class OF
    | X.TrueColor, X.StaticColor =>
      cmap_info.red_max := visual.red_mask;
      cmap_info.red_mult := 1;
      WHILE Word.And (cmap_info.red_max, 1) = 0 DO
        cmap_info.red_max := Word.RightShift (cmap_info.red_max, 1);
        cmap_info.red_mult := Word.LeftShift (cmap_info.red_mult, 1);
      END;
      cmap_info.green_max := visual.green_mask;
      cmap_info.green_mult := 1;
      WHILE Word.And (cmap_info.green_max, 1) = 0 DO
        cmap_info.green_max := Word.RightShift (cmap_info.green_max, 1);
        cmap_info.green_mult := Word.LeftShift (cmap_info.green_mult, 1);
      END;
      cmap_info.blue_max := visual.blue_mask;
      cmap_info.blue_mult := 1;
      WHILE Word.And (cmap_info.blue_max, 1) = 0 DO
	cmap_info.blue_max := Word.RightShift (cmap_info.blue_max, 1);
        cmap_info.blue_mult := Word.LeftShift (cmap_info.blue_mult, 1);
      END;

      capx_info.approxType  := PEX.PEXColourSpace;
      capx_info.approxModel := PEX.PEXColourApproxRGB;
      capx_info.dither      := PEX.PEXOn;
      capx_info.basePixel   := cmap_info.base_pixel;
      capx_info.max1        := cmap_info.red_max;
      capx_info.max2        := cmap_info.green_max;
      capx_info.max3        := cmap_info.blue_max;
      capx_info.weight1     := 0.0; (* not used by PEXColorSpace *)
      capx_info.weight2     := 0.0; (* not used by PEXColorSpace *)
      capx_info.weight3     := 0.0; (* not used by PEXColorSpace *)
      capx_info.mult1       := cmap_info.red_mult;
      capx_info.mult2       := cmap_info.green_mult;
      capx_info.mult3       := cmap_info.blue_mult;

    | X.StaticGray =>
      cmap_info.red_max  := visual.colormap_size - 1;
      cmap_info.red_mult := 1;
	
      capx_info.approxType  := PEX.PEXColourRange;
      capx_info.approxModel := PEX.PEXColourApproxRGB;
      capx_info.dither      := PEX.PEXOn;
      capx_info.basePixel   := cmap_info.base_pixel;
      capx_info.max1        := cmap_info.red_max;
      capx_info.max2        := 0; (* not used by PEXColorRange *)
      capx_info.max3        := 0; (* not used by PEXColorRange *)
      (* Give the weights the NTSC intensity coefficients. *)
      capx_info.weight1     := 0.299; 
      capx_info.weight2     := 0.587;
      capx_info.weight3     := 0.114;
      capx_info.mult1       := cmap_info.red_mult;
      capx_info.mult2       := 0;
      capx_info.mult3       := 0;
    ELSE
      <* ASSERT FALSE *>
    END;
  END CreateReadOnlyMap;


VAR
  man : Manager := NIL;

BEGIN
END X_PEX_Base.
