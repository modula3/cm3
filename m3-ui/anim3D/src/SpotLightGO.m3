(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Thu Feb  2 00:52:41 PST 1995 by najork                   *)
(*       Created on Thu Feb 10 09:33:46 PST 1994 by najork                   *)


MODULE SpotLightGO EXPORTS SpotLightGO, SpotLightGOProxy;

IMPORT BooleanPropPrivate, Color, ColorPropPrivate, GO, GOPrivate, 
       GraphicsBase, GraphicsBasePrivate, LightGO, Point3, PointProp, 
       PointPropPrivate, Prop, RealProp, RealPropPrivate;


PROCEDURE New (c : Color.T; orig, dir : Point3.T; 
               conc, spread, att0, att1 : REAL) : T =
  VAR
    light := NEW (T).init ();
  BEGIN
    LightGO.SetColour (light, c);
    LightGO.SetSwitch (light, TRUE);
    SetOrigin (light, orig);
    SetDirection (light, dir);
    SetConcentration (light, conc);
    SetSpreadAngle (light, spread);
    SetAttenuation0 (light, att0);
    SetAttenuation1 (light, att1);
    RETURN light;
  END New;


REVEAL
  T = Public BRANDED OBJECT
  OVERRIDES
    init              := Init;
    draw              := Draw;
    damageIfDependent := DamageIfDependent;
  END;


PROCEDURE Init (self : T) : T =
  BEGIN
    EVAL GO.T.init (self);

    IF MkProxyT # NIL AND self.proxy = NIL THEN
      MkProxyT (self);
    END;

    RETURN self;
  END Init;


PROCEDURE DamageIfDependent (self : T; pn : Prop.Name) =
  BEGIN
    IF pn = LightGO.Switch OR pn = LightGO.Colour OR pn = GO.Transform OR
       pn = Origin OR pn = Direction OR pn = Concentration OR 
       pn = SpreadAngle OR pn = Attenuation0 OR pn = Attenuation1 THEN
      self.damaged := TRUE;
    END;
  END DamageIfDependent;


PROCEDURE Draw (self : T; state : GraphicsBase.T) =
  BEGIN
    state.push (self);

    IF LightGO.Switch.getState (state) THEN
      WITH color  = LightGO.Colour.getState (state),
           orig   = Origin.getState (state),
           dir    = Direction.getState (state),
           conc   = Concentration.getState (state),
           spread = SpreadAngle.getState (state),
           att0   = Attenuation0.getState (state),
           att1   = Attenuation1.getState (state) DO 
        state.addSpotLight (color, orig, dir, conc, spread, att0, att1);
      END;
    END;

    state.pop (self);
  END Draw;


(*****************************************************************************)
(* Convenience Procedures                                                    *)
(*****************************************************************************)


PROCEDURE SetOrigin (o : GO.T; v : Point3.T) =
  BEGIN
    o.setProp (Origin.bind (PointProp.NewConst (v)));
  END SetOrigin;


PROCEDURE SetDirection (o : GO.T; v : Point3.T) =
  BEGIN
    o.setProp (Direction.bind (PointProp.NewConst (v)));
  END SetDirection;


PROCEDURE SetConcentration (o : GO.T; v : REAL) =
  BEGIN
    o.setProp (Concentration.bind (RealProp.NewConst (v)));
  END SetConcentration;


PROCEDURE SetSpreadAngle (o : GO.T; v : REAL) =
  BEGIN
    o.setProp (SpreadAngle.bind (RealProp.NewConst (v)));
  END SetSpreadAngle;


PROCEDURE SetAttenuation0 (o : GO.T; v : REAL) =
  BEGIN
    o.setProp (Attenuation0.bind (RealProp.NewConst (v)));
  END SetAttenuation0;


PROCEDURE SetAttenuation1 (o : GO.T; v : REAL) =
  BEGIN
    o.setProp (Attenuation1.bind (RealProp.NewConst (v)));
  END SetAttenuation1;


BEGIN
  Origin        := NEW (PointProp.Name).init (Point3.T {0.0, 0.0, 0.0});
  Direction     := NEW (PointProp.Name).init (Point3.T {1.0, 1.0, 1.0});
  Concentration := NEW (RealProp.Name).init (1.0);
  SpreadAngle   := NEW (RealProp.Name).init (1.0);
  Attenuation0  := NEW (RealProp.Name).init (1.0);
  Attenuation1  := NEW (RealProp.Name).init (1.0);
END SpotLightGO.
