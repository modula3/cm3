(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Created by Marc Najork                                                    *)
(* Last modified on Mon Jan 30 22:32:14 PST 1995 by najork                   *)


MODULE DiskGO EXPORTS DiskGO, DiskGOProxy;

IMPORT GO, GOPrivate, GraphicsBase, GraphicsBasePrivate, Matrix4, Point3, 
       PointProp, PointPropPrivate, Prop, RealProp, RealPropPrivate;


REVEAL
  T = Public BRANDED OBJECT
    prec   : INTEGER;
    matrix : Matrix4.T;      (* transforms unit sphere into desired sphere *)
    center : Point3.T;       (* center achieved through matrix *)
    normal : Point3.T;       (* normal achieved through matrix *)
    radius : REAL;           (* radius achieved through matrix *)
  OVERRIDES
    init              := Init;
    draw              := Draw;
    damageIfDependent := DamageIfDependent;
  END;


PROCEDURE Init (self : T; prec : INTEGER) : T =
  BEGIN
    EVAL GO.T.init (self);
    self.prec   := prec;
    self.matrix := Matrix4.Identity ();
    self.center := Point3.T {0.0, 0.0, 0.0};
    self.normal := Point3.T {0.0, 0.0, 1.0};
    self.radius := 1.0;

    IF MkProxyT # NIL AND self.proxy = NIL THEN
      MkProxyT (self);
    END;

    RETURN self;
  END Init;


PROCEDURE DamageIfDependent (self : T; pn : Prop.Name) =
  BEGIN
    IF pn = Center OR pn = Normal OR pn = Radius THEN
      self.damaged := TRUE;
    END;
  END DamageIfDependent;


PROCEDURE Draw (self : T; state : GraphicsBase.T) =
  BEGIN
    state.push (self);
    WITH center = Center.getState (state),
         normal = Normal.getState (state), 
         radius = Radius.getState (state) DO

      IF center # self.center OR normal # self.normal OR radius # self.radius THEN
        self.center := center;
        self.normal := normal;
        self.radius := radius;
        WITH s = Point3.OrthoVector (normal),
             t = Point3.CrossProduct (normal, s),
             a = Point3.Plus (center, Point3.ScaleToLen (s, radius)),
             b = Point3.Plus (center, Point3.ScaleToLen (t, radius)),
             c = Point3.Plus (center, normal) DO
          self.matrix := Matrix4.TransformUnitCube (center, a, b, c);
        END;
      END;

      state.pushMatrix (self.matrix);
      state.drawProtoDisk (self.prec);
      state.popMatrix ();
      state.growBoundingVolume (center, radius);
    END;

    state.pop (self);
  END Draw;


(*****************************************************************************)
(* Convenience Procedures                                                    *)
(*****************************************************************************)


PROCEDURE New (p : Point3.T; n : Point3.T; r : REAL; prec := 10) : T =
  VAR
    disk := NEW (T).init (prec);
  BEGIN
    SetCenter (disk, p);
    SetNormal (disk, n);
    SetRadius (disk, r);
    RETURN disk;
  END New;


PROCEDURE SetCenter (o : GO.T; v : Point3.T) =
  BEGIN
    o.setProp (Center.bind (PointProp.NewConst (v)));
  END SetCenter;


PROCEDURE SetNormal (o : GO.T; v : Point3.T) =
  BEGIN
    o.setProp (Normal.bind (PointProp.NewConst (v)));
  END SetNormal;


PROCEDURE SetRadius (o : GO.T; v : REAL) =
  BEGIN
    o.setProp (Radius.bind (RealProp.NewConst (v)));
  END SetRadius;


BEGIN
  Center := NEW (PointProp.Name).init (Point3.T {0.0, 0.0, 0.0});
  Normal := NEW (PointProp.Name).init (Point3.T {0.0, 0.0, 1.0});
  Radius := NEW (RealProp.Name).init (1.0);
END DiskGO.
