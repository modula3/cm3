(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Created by Marc Najork                                                    *)
(* Last modified on Mon Jan 30 23:05:34 PST 1995 by najork                   *)


MODULE CylinderGO EXPORTS CylinderGO, CylinderGOProxy;

IMPORT GO, GOPrivate, GraphicsBase, GraphicsBasePrivate, Matrix4, Mth, Point3,
       PointProp, PointPropPrivate, Prop, RealProp, RealPropPrivate;


REVEAL
  T = Public BRANDED OBJECT
    prec   : INTEGER; 
    p1, p2 : Point3.T;       (* endpoints achieved through matrix *)
    radius : REAL;           (* radius achieved through matrix *)
    matrix : Matrix4.T;      (* transforms unit cyl. into desired cyl. *)
  OVERRIDES
    init              := Init;
    draw              := Draw;
    damageIfDependent := DamageIfDependent;
  END;

PROCEDURE Init(self : T; prec : INTEGER) : T =
  BEGIN
    EVAL GO.T.init (self);
    self.prec   := prec;
    self.matrix := Matrix4.Identity ();
    self.p1     := Point3.T {0.0, 0.0, 0.0};
    self.p2     := Point3.T {0.0, 0.0, 1.0};
    self.radius := 1.0;

    IF MkProxyT # NIL AND self.proxy = NIL THEN
      MkProxyT (self);
    END;

    RETURN self;
  END Init;


PROCEDURE DamageIfDependent (self : T; pn : Prop.Name) =
  BEGIN
    IF pn = Point1 OR pn = Point2 OR pn = Radius THEN
      self.damaged := TRUE;
    END;
  END DamageIfDependent;


PROCEDURE Draw (self : T; state : GraphicsBase.T) =
  BEGIN
    state.push (self);
    WITH point1 = Point1.getState (state),
         point2 = Point2.getState (state), 
         radius = Radius.getState (state) DO

      IF point1 # self.p1 OR point2 # self.p2 OR radius # self.radius THEN
        self.p1     := point1;
        self.p2     := point2;
        self.radius := radius;

        WITH n = Point3.Minus (point2, point1),
             s = Point3.OrthoVector (n),
             t = Point3.CrossProduct (n, s),
             a = Point3.Plus (point1, Point3.ScaleToLen (s, radius)),
             b = Point3.Plus (point1, Point3.ScaleToLen (t, radius)) DO
          self.matrix := Matrix4.TransformUnitCube (point1, a, b, point2);
        END;
      END;

      state.pushMatrix (self.matrix);
      state.drawProtoCylinder (self.prec);
      state.popMatrix ();
      WITH center = Point3.MidPoint (point1, point2),
           dist   = Point3.Distance (point1, point2) / 2.0,
           rad    = Mth.sqrt (dist * dist + radius * radius) DO
        state.growBoundingVolume (center, rad);
      END;

    END;
      
    state.pop (self);
  END Draw;


(*****************************************************************************)
(* Convenience Procedures                                                    *)
(*****************************************************************************)


PROCEDURE New (p1, p2 : Point3.T; r : REAL; prec : INTEGER) : T =
  VAR 
    cyl := NEW (T).init (prec);
  BEGIN
    SetPoint1 (cyl, p1);
    SetPoint2 (cyl, p2);
    SetRadius (cyl, r);
    RETURN cyl;
  END New;


PROCEDURE SetPoint1 (o : GO.T; v : Point3.T) =
  BEGIN
    o.setProp (Point1.bind (PointProp.NewConst (v)));
  END SetPoint1;


PROCEDURE SetPoint2 (o : GO.T; v : Point3.T) =
  BEGIN
    o.setProp (Point2.bind (PointProp.NewConst (v)));
  END SetPoint2;


PROCEDURE SetRadius (o : GO.T; v : REAL) =
  BEGIN
    o.setProp (Radius.bind (RealProp.NewConst (v)));
  END SetRadius;


BEGIN
  Point1 := NEW (PointProp.Name).init (Point3.T {0.0, 0.0, 0.0});
  Point2 := NEW (PointProp.Name).init (Point3.T {1.0, 0.0, 0.0});
  Radius := NEW (RealProp.Name).init (1.0);
END CylinderGO.
