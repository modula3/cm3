(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Oct  9 13:34:22 PDT 1995 by najork                   *)
(*       Created on Wed Feb 16 14:15:37 PST 1994 by najork                   *)


MODULE RootGO EXPORTS RootGO, RootGOPrivate, RootGOProxy;

IMPORT AmbientLightGO, AnimServer, BooleanProp, BooleanPropPrivate, CameraGO, 
       CameraGOPrivate, Color, ColorProp, ColorPropPrivate, Env, GO, GOPrivate,
       GraphicsBase, GraphicsBasePrivate, GroupGO, GroupGOPrivate, MouseCB, 
       ParseParams, PerspCameraGO, Point, Point3, PositionCB, Prop, RealProp, 
       RealPropPrivate, Stdio, Text, TransformProp, VBT, VectorLightGO, 
       Win_OpenGL_Base, X_OpenGL_Base, X_PEX_Base;


REVEAL
  T = Private BRANDED OBJECT
    backgroundColor : Color.T;  (* cached background color *)
  OVERRIDES
    init                := Init;
    changeCamera        := ChangeCamera;
    screenToWorld       := ScreenToWorld;
    findName            := FindName;
    adjust              := Adjust;
    draw                := Draw;
    damageIfDependent   := DamageIfDependent;
    undamage            := Undamage;
  END;


PROCEDURE Init (self : T; cam : CameraGO.T; base : GraphicsBase.T) : T =
  BEGIN
    EVAL GroupGO.T.init (self);
    self.cam   := cam;
    self.base  := base;

    base.root := self;

    (* set the background color to a sentinel value, thereby triggering
       an initial damage repair, i.e. proper background initialization ***)
    self.backgroundColor := Color.T {-1.0, -1.0, -1.0};

    (* Register to root with the animation server *)
    AnimServer.RegisterRootGO (self);

    IF MkProxyT # NIL THEN
      MkProxyT (self);
    END;

    RETURN self;
  END Init;


PROCEDURE ChangeCamera (self : T; cam : CameraGO.T) =
  BEGIN
    (*** Must be protected from interference with the animation server ***)
    LOCK AnimServer.internalLock DO
      self.cam := cam;

      (*** damage the root, forcing a redraw ***)
      self.damaged := TRUE;
    END;
  END ChangeCamera;


PROCEDURE ScreenToWorld (self: T; pos: Point.T; z: REAL): Point3.T =
  BEGIN
    RETURN self.base.screenToWorld (pos, z);
  END ScreenToWorld;


PROCEDURE FindName (self : T; name : TEXT) : GO.T =
  BEGIN
    IF self.cam.findName (name) # NIL THEN
      RETURN self.cam;
    ELSE
      RETURN GroupGO.T.findName (self, name);
    END;
  END FindName;


PROCEDURE Adjust (self : T; time : LONGREAL) = 
  BEGIN
    (*** Adjust self like any other root ... ***)
    GroupGO.T.adjust (self, time);

    (*** ... but also adjust the active camera ... ***)
    self.cam.adjust (time);

    (*** ... and propagate its damage up. ***)
    IF self.cam.damaged THEN
      self.damaged := TRUE;
    END;
  END Adjust;


PROCEDURE Draw (self : T; state : GraphicsBase.T) =
  BEGIN
    state.push (self);

    (*** Take care of the background color ***)
    WITH col = Background.getState (state) DO
      IF self.backgroundColor # col THEN
        self.backgroundColor := col;
        state.setBackgroundColor (col);
      END;
    END;

    (*** Take care of depth cueing ***)
    WITH switch = DepthcueSwitch.getState (state),
         fplane = DepthcueFrontPlane.getState (state),
         bplane = DepthcueBackPlane.getState (state),
         fscale = DepthcueFrontScale.getState (state),
         bscale = DepthcueBackScale.getState (state),
         color  = DepthcueColour.getState (state) DO
      state.setDepthcueing(switch, fplane, bplane, fscale, bscale, color);
    END;

    (*** Now do whatever has to be done for normal groups as well ***)
    FOR i := 0 TO self.last DO
      (* Calling draw may set self.damaged *)
      self.children[i].draw (state);
    END;

    state.pop (self);

    (* Test if the camera has been drawn (i.e. is part of the overall scene);
       if this is not the case, draw it now. *)
    IF NOT self.cam.flag THEN
      (* Note that the order of the arguments to OR matters here!! *)
      self.cam.draw (state);
    END;
    self.cam.flag := FALSE;

    (*** As "caller" is NIL, we don't have to propagate self.damaged ***)
  END Draw;


PROCEDURE DamageIfDependent (self : T; pn : Prop.Name) =
  BEGIN
    IF pn = Background OR pn = DepthcueSwitch OR pn = DepthcueColour OR
       pn = DepthcueFrontPlane OR pn = DepthcueBackPlane OR
       pn = DepthcueFrontScale OR pn = DepthcueBackScale THEN
      self.damaged := TRUE;
    END;
  END DamageIfDependent;


PROCEDURE Undamage (self: T) =
  BEGIN
    GroupGO.T.undamage (self);
    self.cam.undamage ();
  END Undamage;


(*****************************************************************************)
(* Construction procedures                                                   *)
(*****************************************************************************)


PROCEDURE New (cam : CameraGO.T; base : GraphicsBase.T) : T =
  BEGIN
    RETURN NEW(T).init(cam, base);
  END New;


PROCEDURE NewStd (base : GraphicsBase.T) : T RAISES {GraphicsBase.Failure} =

  PROCEDURE NewBase (title: TEXT) : GraphicsBase.T 
      RAISES {GraphicsBase.Failure} =
    TYPE 
      Pref = {None, X_PEX, X_OpenGL};
    VAR
      pref := Pref.None;
    BEGIN
      (*** Check for environment variable $prefPref ***)
      WITH var = Env.Get ("prefBase") DO
        IF var # NIL THEN
          IF Text.Equal (var, "X_PEX") THEN
            pref := Pref.X_PEX;
          ELSIF Text.Equal (var, "X_OpenGL") THEN
            pref := Pref.X_OpenGL;
          END;
        END;
      END;

      (*** Command-line arguments "-prefBase" overrides $prefBase ***)
      WITH pp = NEW (ParseParams.T).init (Stdio.stderr) DO
        IF pp.keywordPresent("-prefBase") THEN
          TRY
            WITH val = pp.getNext () DO
              IF Text.Equal (val, "X_PEX") THEN
                pref := Pref.X_PEX;
              ELSIF Text.Equal (val, "X_OpenGL") THEN
                pref := Pref.X_OpenGL;
              END;
            END;
          EXCEPT
            ParseParams.Error =>  (* ignore ... *)
          END;
        END;
      END;

      (* Try to create the preferred base. 
         If this does not succeed, create any base. *)
      TRY 
        CASE pref OF
        | Pref.None     => 
        | Pref.X_PEX    => RETURN NEW (X_PEX_Base.T).init (title);
        | Pref.X_OpenGL => RETURN NEW (X_OpenGL_Base.T).init (title);
        END;
      EXCEPT
        GraphicsBase.Failure =>
      END;

      TRY 
        RETURN NEW (X_PEX_Base.T).init (title);
      EXCEPT
        GraphicsBase.Failure =>
      END;
      TRY 
        RETURN NEW (X_OpenGL_Base.T).init (title);
      EXCEPT
        GraphicsBase.Failure =>
      END;
      TRY 
        RETURN NEW (Win_OpenGL_Base.T).init (title);
      EXCEPT
        GraphicsBase.Failure =>
      END;
      
      RAISE GraphicsBase.Failure;
    END NewBase;
      
  VAR
    root : T;
    cam := PerspCameraGO.New (from := Point3.T{0.0, 0.0, 100.0},
                              to   := Point3.T{0.0, 0.0, 0.0},
                              up   := Point3.T{0.0, 1.0, 0.0},
                              fovy := 0.05);
  BEGIN
    IF base = NIL THEN
      base := NewBase ("Anim3D Viewer");
    END;
    cam.setName ("default-camera");
    root := NEW (T).init (cam, base);

    (* Attach two lights *)
    WITH light = AmbientLightGO.New (Color.White) DO
      light.setName ("default-ambient-light");
      root.add (light);
    END;
    WITH light = VectorLightGO.New (Color.White, Point3.T{-1.0,-1.0,-1.0}) DO
      light.setName ("default-vector-light");
      root.add (light);
    END;

    (* Attach mouse and position callbacks to the root *)
    root.setProp (GO.Transform.bind (TransformProp.NewConst ()));
    root.pushMouseCB (NEW (MyMouseCB, go := root, invoke := MouseInvoke).init());

    RETURN root;
  END NewStd;


TYPE 
  MyPositionCB = PositionCB.T OBJECT
    go  : T;
    pos : Point.T;
    but : VBT.Button;
  OVERRIDES
    invoke := PositionInvoke;
  END;

  MyMouseCB = MouseCB.T OBJECT
    go : T;
  OVERRIDES
    invoke := MouseInvoke;
  END;


PROCEDURE PositionInvoke (self : MyPositionCB; pr : PositionCB.Rec) =
  <* FATAL GO.PropUndefined *>
  BEGIN
    WITH d   = Point.Sub (pr.pos2D, self.pos), 
         dx  = FLOAT (d.h), dy = FLOAT (d.v), 
         beh = NARROW (GO.GetTransform(self.go).beh, TransformProp.ConstBeh) DO
      IF VBT.Modifier.Shift IN pr.modifiers THEN
        CASE  self.but OF
        | VBT.Modifier.MouseL => beh.translate (dx * 0.01, -dy * 0.01, 0.0);
        | VBT.Modifier.MouseM => beh.scale (1.0 + dx * 0.01,
                                            1.0 + dx * 0.01,
                                            1.0 + dx * 0.01);
        | VBT.Modifier.MouseR => beh.translate (0.0, 0.0, dx * 0.01);
        ELSE
          (* Mice have only three buttons those days ... *)
        END;
      ELSE
        CASE  self.but OF
        | VBT.Modifier.MouseL => beh.rotateX (dx * 0.01);
        | VBT.Modifier.MouseM => beh.rotateY (dx * 0.01);
        | VBT.Modifier.MouseR => beh.rotateZ (dx * 0.01);
        ELSE
          (* Mice have only three buttons those days ... *)
        END;
      END;
    END;
    self.pos := pr.pos2D;
  END PositionInvoke;


PROCEDURE MouseInvoke (self : MyMouseCB; mr : MouseCB.Rec) =
  <* FATAL GO.StackError *>
  BEGIN
    IF mr.clickType = VBT.ClickType.FirstDown THEN
      self.go.pushPositionCB (NEW (MyPositionCB, 
                                   go  := self.go, 
                                   pos := mr.pos2D,
                                   but := mr.whatChanged).init());
    ELSIF mr.clickType = VBT.ClickType.LastUp THEN
      self.go.popPositionCB ();
    END;
  END MouseInvoke;


(*****************************************************************************)
(* Module body                                                               *)
(*****************************************************************************)


BEGIN
  Background         := NEW (ColorProp.Name).init (Color.Black);
  DepthcueColour     := NEW (ColorProp.Name).init (Color.Black);
  DepthcueFrontPlane := NEW (RealProp.Name).init (1.0);
  DepthcueBackPlane  := NEW (RealProp.Name).init (0.0);
  DepthcueFrontScale := NEW (RealProp.Name).init (1.0);
  DepthcueBackScale  := NEW (RealProp.Name).init (0.0);
  DepthcueSwitch     := NEW (BooleanProp.Name).init (FALSE);
END RootGO.
