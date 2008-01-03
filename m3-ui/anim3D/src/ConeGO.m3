(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Created by Marc Najork                                                    *)
(* Last modified on Mon Jan 30 22:23:52 PST 1995 by najork                   *)


MODULE ConeGO EXPORTS ConeGO, ConeGOProxy;

IMPORT GO, GOPrivate, GraphicsBase, GraphicsBasePrivate, Matrix4, Mth, Point3,
       PointProp, PointPropPrivate, Prop, RealProp, RealPropPrivate;

REVEAL
  T = Public BRANDED OBJECT
    prec   : INTEGER;
    base   : Point3.T;
    tip    : Point3.T;
    radius : REAL;
    matrix : Matrix4.T;      (* transforms unit cone into desired cone *)
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
    self.base   := Point3.T {0.0, 0.0, 0.0};
    self.tip    := Point3.T {0.0, 0.0, 1.0};
    self.radius := 1.0;

    IF MkProxyT # NIL AND self.proxy = NIL THEN
      MkProxyT (self);
    END;

    RETURN self;
  END Init;


PROCEDURE DamageIfDependent (self : T; pn : Prop.Name) =
  BEGIN
    IF pn = Base OR pn = Tip OR pn = Radius THEN
      self.damaged := TRUE;
    END;
  END DamageIfDependent;


PROCEDURE Draw (self : T; state : GraphicsBase.T) =
  BEGIN
    state.push (self);
    WITH base   = Base.getState (state),
         tip    = Tip.getState (state), 
         radius = Radius.getState (state) DO

      IF base # self.base OR tip # self.tip OR radius # self.radius THEN
        self.base   := base;
        self.tip    := tip;
        self.radius := radius;

        (* Compute a transformation matrix that transforms the prototypical 
           cone into the desired one. *)
        WITH n = Point3.Minus (tip, base),
             s = Point3.OrthoVector (n),
             t = Point3.CrossProduct (n, s),
             a = Point3.Plus (base, Point3.ScaleToLen (s, radius)),
             b = Point3.Plus (base, Point3.ScaleToLen (t, radius)) DO
          self.matrix := Matrix4.TransformUnitCube (base, a, b, tip);
        END;

      END;
      
      state.pushMatrix (self.matrix);
      state.drawProtoCone (self.prec);
      state.popMatrix ();
      WITH center = Point3.MidPoint (base, tip),
           dist   = Point3.Distance (base, tip) / 2.0,
           rad    = Mth.sqrt (dist * dist + radius * radius) DO
        state.growBoundingVolume (center, rad);
      END;
    END;

    state.pop (self);
  END Draw;


(*****************************************************************************)
(* Convenience Procedures                                                    *)
(*****************************************************************************)


PROCEDURE New (base, tip : Point3.T; r : REAL; prec := 30) : T =
  VAR
    cone := NEW (T).init (prec);
  BEGIN
    SetBase (cone, base);
    SetTip (cone, tip);
    SetRadius (cone, r);
    RETURN cone;
  END New;
    

PROCEDURE SetBase (o : GO.T; v : Point3.T) =
  BEGIN
    o.setProp (Base.bind (PointProp.NewConst (v)));
  END SetBase;


PROCEDURE SetTip (o : GO.T; v : Point3.T) =
  BEGIN
    o.setProp (Tip.bind (PointProp.NewConst (v)));
  END SetTip;


PROCEDURE SetRadius (o : GO.T; v : REAL) =
  BEGIN
    o.setProp (Radius.bind (RealProp.NewConst (v)));
  END SetRadius;


BEGIN
  Base   := NEW (PointProp.Name).init (Point3.T {0.0, 0.0, 0.0});
  Tip    := NEW (PointProp.Name).init (Point3.T {1.0, 0.0, 0.0});
  Radius := NEW (RealProp.Name).init (1.0);
END ConeGO.
