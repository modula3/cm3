(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Created by Marc Najork                                                    *)
(* Last modified on Mon Jan 30 23:51:17 PST 1995 by najork                   *)


MODULE MarkerGO EXPORTS MarkerGO, MarkerGOProxy;

IMPORT Color, ColorProp, ColorPropPrivate, GO, GOPrivate, GraphicsBase, 
       GraphicsBasePrivate, MarkerTypeProp, MarkerTypePropPrivate, Prop, 
       PropPrivate, Point3, PointProp, PointPropPrivate, RealProp, 
       RealPropPrivate;

REVEAL
  T = Public BRANDED OBJECT
  OVERRIDES
    init              := Init;
    draw              := Draw;
    damageIfDependent := DamageIfDependent;
    needsTransparency := NeedsTransparency;
  END;


PROCEDURE Init(self : T) : T =
  BEGIN
    EVAL GO.T.init (self);

    IF MkProxyT # NIL AND self.proxy = NIL THEN
      MkProxyT (self);
    END;

    RETURN self;
  END Init;


PROCEDURE Draw (self : T; state : GraphicsBase.T) =
  BEGIN
    state.push (self);
    WITH center = Center.getState (state) DO
      state.drawMarker (center);
      (*** The bounding sphere of every GO must have size > 0 ***)
      state.growBoundingVolume (center, 1.0e-10);
    END;
    state.pop (self);
  END Draw;


PROCEDURE DamageIfDependent (self : T; pn : Prop.Name) =
  BEGIN
    IF pn = Center THEN
      self.damaged := TRUE;
    END;
  END DamageIfDependent;


PROCEDURE NeedsTransparency(<* UNUSED *> self : T; 
                            <* UNUSED *> t    : REAL) : BOOLEAN =
  BEGIN
    RETURN FALSE;
  END NeedsTransparency;


PROCEDURE New (p : Point3.T) : T =
  VAR
    marker := NEW (T).init ();
  BEGIN
    SetCenter (marker, p);
    RETURN marker;
  END New;


(*****************************************************************************)
(* Colour_PN                                                                 *)
(*****************************************************************************)

TYPE
  Colour_PN = ColorProp.Name OBJECT
  OVERRIDES
    damage  := DamageColour;
    push    := PushColour;
    pop     := PopColour;
  END;


PROCEDURE DamageColour (<* UNUSED *> self : Colour_PN; caller : GO.T) =
  BEGIN
    caller.damaged := TRUE;
  END DamageColour;


PROCEDURE PushColour (self  : Colour_PN; 
                      state : GraphicsBase.T; 
                      pv    : Prop.Val) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], ColorPropPrivate.Stack),
         val   = NARROW (pv, ColorProp.Val).val DO
      IF stack.top # val THEN
        state.setMarkerColor (val);
      END;
      stack.push (val);
    END;
  END PushColour;


PROCEDURE PopColour (self : Colour_PN; state : GraphicsBase.T) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], ColorPropPrivate.Stack) DO
      state.setMarkerColor (stack.pop ());
    END;
  END PopColour;


(*****************************************************************************)
(* Scale_PN                                                                  *)
(*****************************************************************************)

TYPE
  Scale_PN = RealProp.Name OBJECT
  OVERRIDES
    damage  := DamageScale;
    push    := PushScale;
    pop     := PopScale;
  END;


PROCEDURE DamageScale (<* UNUSED *> self : Scale_PN; caller : GO.T) =
  BEGIN
    caller.damaged := TRUE;
  END DamageScale;


PROCEDURE PushScale (self  : Scale_PN; 
                     state : GraphicsBase.T; 
                     pv    : Prop.Val) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], RealPropPrivate.Stack),
         val   = NARROW (pv, RealProp.Val).val DO
      IF stack.top # val THEN
        state.setMarkerScale (val);
      END;
      stack.push (val);
    END;
  END PushScale;


PROCEDURE PopScale (self : Scale_PN; state : GraphicsBase.T) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], RealPropPrivate.Stack) DO
      state.setMarkerScale (stack.pop ());
    END;
  END PopScale;


(*****************************************************************************)
(* Type_PN                                                                   *)
(*****************************************************************************)

TYPE
  Type_PN = MarkerTypeProp.Name OBJECT
  OVERRIDES
    damage  := DamageType;
    push    := PushType;
    pop     := PopType;
  END;


PROCEDURE DamageType (<* UNUSED *> self : Type_PN; caller : GO.T) =
  BEGIN
    caller.damaged := TRUE;
  END DamageType;


PROCEDURE PushType (self  : Type_PN; 
                    state : GraphicsBase.T; 
                    pv    : Prop.Val) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], MarkerTypePropPrivate.Stack),
         val   = NARROW (pv, MarkerTypeProp.Val).val DO
      IF stack.top # val THEN
        state.setMarkerType (val);
      END;
      stack.push (val);
    END;
  END PushType;


PROCEDURE PopType (self : Type_PN; state : GraphicsBase.T) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], MarkerTypePropPrivate.Stack) DO
      state.setMarkerType (stack.pop ());
    END;
  END PopType;


(*****************************************************************************)
(* Convenience Procedures                                                    *)
(*****************************************************************************)


PROCEDURE SetCenter (o : GO.T; v : Point3.T) =
  BEGIN
    o.setProp (Center.bind (PointProp.NewConst (v)));
  END SetCenter;


PROCEDURE SetColour (o : GO.T; v : Color.T) =
  BEGIN
    o.setProp (Colour.bind (ColorProp.NewConst (v)));
  END SetColour;


PROCEDURE SetScale (o : GO.T; v : REAL) =
  BEGIN
    o.setProp (Scale.bind (RealProp.NewConst (v)));
  END SetScale;


PROCEDURE SetType (o : GO.T; v : MarkerTypeProp.Kind) =
  BEGIN
    o.setProp (Type.bind (MarkerTypeProp.NewConst (v)));
  END SetType;


(*****************************************************************************)
(* Module Body                                                               *)
(*****************************************************************************)

BEGIN
  Center := NEW (PointProp.Name).init (Point3.T {0.0, 0.0, 0.0});
  Colour := NEW (Colour_PN).init (Color.White);
  Scale  := NEW (Scale_PN).init (1.0);
  Type   := NEW (Type_PN).init (MarkerTypeProp.Kind.Asterisk);
END MarkerGO.
