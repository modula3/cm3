(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Sat Jul 15 00:31:19 PDT 1995 by najork                   *)
(*       Created on Mon Feb 14 16:03:12 PST 1994 by najork                   *)


MODULE PerspCameraGO EXPORTS PerspCameraGO, PerspCameraGOProxy;

IMPORT CameraGO, CameraGOPrivate, GO, GOPrivate, GraphicsBase, 
       GraphicsBasePrivate, Matrix4, Point3, PointProp, PointPropPrivate, Prop,
       RealProp, RealPropPrivate, TransformPropPrivate;


PROCEDURE New (from, to, up : Point3.T; fovy : REAL) : T =
  VAR
    cam := NEW (T).init ();
  BEGIN
    cam.setProp (CameraGO.From.bind (PointProp.NewConst (from)));
    cam.setProp (CameraGO.To.bind (PointProp.NewConst (to)));
    cam.setProp (CameraGO.Up.bind (PointProp.NewConst (up)));
    cam.setProp (Fovy.bind (RealProp.NewConst (fovy)));
    RETURN cam;
  END New;
    

REVEAL
  T = Public BRANDED OBJECT
  OVERRIDES
    init := Init;
    draw := Draw;
    damageIfDependent := DamageIfDependent;
  END;


PROCEDURE Init (self : T) : T =
  BEGIN
    EVAL CameraGO.T.init (self);

    IF MkProxyT # NIL AND self.proxy = NIL THEN
      MkProxyT (self);
    END;

    RETURN self;
  END Init;


PROCEDURE DamageIfDependent (self : T; pn : Prop.Name) =
  BEGIN
    IF pn = CameraGO.From OR pn = CameraGO.To OR pn = CameraGO.Up OR 
       pn = CameraGO.Aspect OR pn = Fovy OR pn = GO.Transform THEN
      self.damaged := TRUE;
    END;
  END DamageIfDependent;


PROCEDURE Draw (self : T; state : GraphicsBase.T) =
  BEGIN
    IF self.damaged THEN
      state.push (self);

      WITH M      = GO.Transform.getState (state),
           from   = Matrix4.TransformPoint3 (M, CameraGO.From.getState(state)),
           to     = Matrix4.TransformPoint3 (M, CameraGO.To.getState(state)),
           up     = Matrix4.TransformPoint3 (M, CameraGO.Up.getState(state)),
           fovy   = Fovy.getState (state),
           aspect = CameraGO.Aspect.getState (state) DO
        state.setLookAt    (from, to, up);
        state.setPerspProj (fovy, aspect);

        self.flag := TRUE;
      END;
      (* If the transformation state contains a non-uniform matrix,
         it is not possible to determine a correct value for fovy ... *)

      state.pop (self);
    END;
  END Draw;


(*****************************************************************************)
(* Module body                                                               *)
(*****************************************************************************)


BEGIN 
  Fovy := NEW (RealProp.Name).init (0.1);
END PerspCameraGO.
