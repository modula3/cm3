(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Created by Marc Najork                                                    *)
(* Last modified on Mon Jan 30 22:23:14 PST 1995 by najork                   *)


MODULE TorusGO EXPORTS TorusGO, TorusGOProxy;

IMPORT GO, GOPrivate, GraphicsBase, GraphicsBasePrivate, Matrix4, Point3, 
       PointProp, PointPropPrivate, Prop, RealProp, RealPropPrivate;


REVEAL
  T = Public BRANDED OBJECT
    prec    : INTEGER;
    center  : Point3.T;
    normal  : Point3.T;
    radius1 : REAL;
    matrix  : Matrix4.T;
  OVERRIDES
    init              := Init;
    draw              := Draw;
    damageIfDependent := DamageIfDependent;
  END;


PROCEDURE Init (self : T; prec : INTEGER) : T =
  BEGIN
    EVAL GO.T.init (self);
    self.prec    := prec;
    self.center  := Point3.Origin;
    self.normal  := Point3.T {0.0, 0.0, 1.0};
    self.radius1 := 1.0;
    self.matrix  := Matrix4.Id;

    IF MkProxyT # NIL AND self.proxy = NIL THEN
      MkProxyT (self);
    END;

    RETURN self;
  END Init;


PROCEDURE DamageIfDependent (self : T; pn : Prop.Name) =
  BEGIN
    IF pn = Center OR pn = Normal OR pn = Radius1 OR pn = Radius2 THEN
      self.damaged := TRUE;
    END;
  END DamageIfDependent;


PROCEDURE Draw (self : T; state : GraphicsBase.T) =
  BEGIN
    state.push (self);

    WITH center  = Center.getState (state),
         normal  = Normal.getState (state), 
         radius1 = Radius1.getState (state),
         radius2 = Radius2.getState (state) DO

    
      IF center # self.center OR normal # self.normal OR radius1 # self.radius1
      THEN
        self.center  := center;
        self.normal  := normal;
        self.radius1 := radius1;

        WITH s = Point3.OrthoVector (normal),
             t = Point3.CrossProduct (normal, s),
             a = Point3.Plus (center, Point3.ScaleToLen (s, radius1)),
             b = Point3.Plus (center, Point3.ScaleToLen (t, radius1)),
             c = Point3.Plus (center, Point3.ScaleToLen (normal, radius1)) DO
          self.matrix := Matrix4.TransformUnitCube (center, a, b, c);
        END;
      END;

      state.pushMatrix (self.matrix);
      state.drawProtoTorus (self.prec, radius2 / self.radius1);
      state.popMatrix ();
      state.growBoundingVolume (center, radius1 + radius2);
    END;

    state.pop (self);
  END Draw;


(*****************************************************************************)
(* Convenience Procedures                                                    *)
(*****************************************************************************)


PROCEDURE New (center, normal   : Point3.T; 
               radius1, radius2 : REAL; 
               prec : INTEGER)  : T =
  VAR
    torus := NEW (T).init (prec);
  BEGIN
    SetCenter (torus, center);
    SetNormal (torus, normal);
    SetRadius1 (torus, radius1);
    SetRadius2 (torus, radius2);
    RETURN torus;
  END New;


PROCEDURE SetCenter (o : GO.T; v : Point3.T) =
  BEGIN
    o.setProp (Center.bind (PointProp.NewConst (v)));
  END SetCenter;


PROCEDURE SetNormal (o : GO.T; v : Point3.T) =
  BEGIN
    o.setProp (Normal.bind (PointProp.NewConst (v)));
  END SetNormal;


PROCEDURE SetRadius1 (o : GO.T; v : REAL) =
  BEGIN
    o.setProp (Radius1.bind (RealProp.NewConst (v)));
  END SetRadius1;


PROCEDURE SetRadius2 (o : GO.T; v : REAL) =
  BEGIN
    o.setProp (Radius2.bind (RealProp.NewConst (v)));
  END SetRadius2;


BEGIN
  Center  := NEW (PointProp.Name).init (Point3.T {0.0, 0.0, 0.0});
  Normal  := NEW (PointProp.Name).init (Point3.T {0.0, 0.0, 1.0});
  Radius1 := NEW (RealProp.Name).init (1.0);
  Radius2 := NEW (RealProp.Name).init (0.1);
END TorusGO.
