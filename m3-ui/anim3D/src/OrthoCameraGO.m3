(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Sat Jul 15 00:31:04 PDT 1995 by najork                   *)
(*       Created on Mon Feb 14 16:01:56 PST 1994 by najork                   *)


MODULE OrthoCameraGO EXPORTS OrthoCameraGO, OrthoCameraGOProxy;

IMPORT CameraGO, CameraGOPrivate, GO, GOPrivate, GraphicsBase, 
       GraphicsBasePrivate, Matrix4, Point3, PointProp, PointPropPrivate, Prop,
       RealProp, RealPropPrivate, TransformPropPrivate;


PROCEDURE New (from, to, up : Point3.T; height : REAL) : T =
  VAR
    cam := NEW (T).init ();
  BEGIN
    cam.setProp (CameraGO.From.bind (PointProp.NewConst (from)));
    cam.setProp (CameraGO.To.bind (PointProp.NewConst (to)));
    cam.setProp (CameraGO.Up.bind (PointProp.NewConst (up)));
    cam.setProp (Height.bind (RealProp.NewConst (height)));
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
       pn = CameraGO.Aspect OR pn = Height OR pn = GO.Transform THEN
      self.damaged := TRUE;
    END;
  END DamageIfDependent;


PROCEDURE Draw (self : T; state : GraphicsBase.T) =
  BEGIN
    IF self.damaged THEN 
      state.push (self);

      WITH tm   = GO.Transform.getState (state),
           from = Matrix4.TransformPoint3 (tm, CameraGO.From.getState (state)),
           to   = Matrix4.TransformPoint3 (tm, CameraGO.To.getState (state)),
           up   = Matrix4.TransformPoint3 (tm, CameraGO.Up.getState (state)),
           height = Height.getState (state),
           aspect = CameraGO.Aspect.getState (state) DO
        state.setLookAt (from, to, up);
        state.setOrthoProj (height, aspect);

        self.flag := TRUE;
      END;
      (* If the transformation state contains a non-uniform matrix,
         the viewing parallelepiped might be distorted. In this case,
         it is not possible to determine a correct value for height ... *)

      state.pop (self);
    END;
  END Draw;


(*****************************************************************************)
(* Module body                                                               *)
(*****************************************************************************)


BEGIN
  Height := NEW (RealProp.Name).init (10.0);
END OrthoCameraGO.
