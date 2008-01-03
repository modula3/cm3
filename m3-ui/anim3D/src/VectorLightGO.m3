(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Thu Feb  2 00:52:59 PST 1995 by najork                   *)
(*       Created on Wed Feb  9 17:14:29 PST 1994 by najork                   *)


MODULE VectorLightGO EXPORTS VectorLightGO, VectorLightGOProxy;

IMPORT BooleanPropPrivate, Color, ColorPropPrivate, GO, GOPrivate, 
       GraphicsBase, GraphicsBasePrivate, LightGO, Point3, PointProp, 
       PointPropPrivate, Prop;


PROCEDURE New (c : Color.T; dir : Point3.T) : T =
  VAR
    light := NEW (T).init ();
  BEGIN
    LightGO.SetColour (light, c);
    LightGO.SetSwitch (light, TRUE);
    SetDirection (light, dir);
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
    IF pn = LightGO.Switch OR pn = LightGO.Colour OR 
       pn = Direction OR pn = GO.Transform THEN
      self.damaged := TRUE;
    END;
  END DamageIfDependent;


PROCEDURE Draw (self : T; state : GraphicsBase.T) =
  BEGIN
    state.push (self);

    IF LightGO.Switch.getState (state) THEN
      state.addVectorLight (LightGO.Colour.getState (state), 
                            Direction.getState (state));
    END;

    state.pop (self);
  END Draw;


(*****************************************************************************)
(* Convenience Procedures                                                    *)
(*****************************************************************************)


PROCEDURE SetDirection (o : GO.T; v : Point3.T) =
  BEGIN
    o.setProp (Direction.bind (PointProp.NewConst (v)));
  END SetDirection;


BEGIN
  Direction := NEW (PointProp.Name).init (Point3.T {1.0, 1.0, 1.0});
END VectorLightGO.
