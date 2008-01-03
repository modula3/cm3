(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Created by Marc Najork                                                    *)
(* Last modified on Mon Jan 30 22:33:01 PST 1995 by najork                   *)


MODULE SphereGO EXPORTS SphereGO, SphereGOProxy;

IMPORT GO, GOPrivate, GraphicsBase, GraphicsBasePrivate, Matrix4, Point3, 
       PointProp, PointPropPrivate, Prop, RealProp, RealPropPrivate;

REVEAL
  T = Public BRANDED OBJECT
    prec   : INTEGER;    (* desired precision *)
    matrix : Matrix4.T;  (* transforms unit sphere into desired sphere *)
    center : Point3.T;   (* center achieved through matrix *)
    radius : REAL;       (* radius achieved through matrix *)
  OVERRIDES
    init              := Init;
    draw              := Draw;
    damageIfDependent := DamageIfDependent;
  END;


PROCEDURE Init(self : T; prec : INTEGER) : T =
  BEGIN
    EVAL GO.T.init (self);
    self.prec   := prec;
    self.matrix := Matrix4.Id;
    self.center := Point3.Origin;
    self.radius := 1.0;
 
   IF MkProxyT # NIL AND self.proxy = NIL THEN
      MkProxyT (self);
    END;

    RETURN self;
  END Init;


PROCEDURE DamageIfDependent (self : T; pn : Prop.Name) =
  BEGIN
    IF pn = Center OR pn = Radius THEN
      self.damaged := TRUE;
    END;
  END DamageIfDependent;


PROCEDURE Draw (self : T; state : GraphicsBase.T) =
  BEGIN
    state.push (self);
    WITH center = Center.getState (state),
         radius = Radius.getState (state) DO

      IF center # self.center OR radius # self.radius THEN
        self.center := center;
        self.radius := radius;
        self.matrix := Matrix4.Scale (Matrix4.Id, radius, radius, radius);
        self.matrix := Matrix4.Translate (self.matrix, 
                                          center.x, center.y, center.z);
      END;

      state.pushMatrix (self.matrix);
      state.drawProtoSphere (self.prec);
      state.popMatrix ();
      state.growBoundingVolume (center, radius);
    END;
    state.pop (self);
  END Draw;


(*****************************************************************************)
(* Convenience Procedures                                                    *)
(*****************************************************************************)


PROCEDURE New (center : Point3.T; radius : REAL; prec : INTEGER) : T =
  VAR
    sphere := NEW (T).init (prec);
  BEGIN
    SetCenter (sphere, center);
    SetRadius (sphere, radius);
    RETURN sphere;
  END New;


PROCEDURE SetCenter (o : GO.T; v : Point3.T) =
  BEGIN
    o.setProp (Center.bind (PointProp.NewConst (v)));
  END SetCenter;


PROCEDURE SetRadius (o : GO.T; v : REAL) =
  BEGIN
    o.setProp (Radius.bind (RealProp.NewConst (v)));
  END SetRadius;


BEGIN
  Center := NEW (PointProp.Name).init (Point3.T {0.0, 0.0, 0.0});
  Radius := NEW (RealProp.Name).init (1.0);
END SphereGO.
