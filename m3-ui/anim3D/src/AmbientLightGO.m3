(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Wed Feb  1 23:58:57 PST 1995 by najork                   *)
(*       Created on Wed Feb  9 14:33:47 PST 1994 by najork                   *)


MODULE AmbientLightGO EXPORTS AmbientLightGO, AmbientLightGOProxy;

IMPORT BooleanPropPrivate, Color, ColorPropPrivate, GO, GOPrivate, 
       GraphicsBase, GraphicsBasePrivate, LightGO, Prop;


PROCEDURE New (c : Color.T) : T =
  VAR
    light := NEW (T).init ();
  BEGIN
    LightGO.SetColour (light, c);
    LightGO.SetSwitch (light, TRUE);
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
    IF pn = LightGO.Switch OR pn = LightGO.Colour THEN
      self.damaged := TRUE;
    END;
  END DamageIfDependent;


PROCEDURE Draw (self : T; state : GraphicsBase.T) =
  BEGIN
    state.push (self);

    IF LightGO.Switch.getState (state) THEN
      state.addAmbientLight (LightGO.Colour.getState (state));
    END;

    state.pop (self);
  END Draw;


BEGIN
END AmbientLightGO.
