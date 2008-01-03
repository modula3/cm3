(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Created by Marc Najork                                                    *)
(* Last modified on Mon Jan 30 23:00:03 PST 1995 by najork                   *)


MODULE LineGO EXPORTS LineGO, LineGOProxy;

IMPORT Color, ColorProp, ColorPropPrivate, GO, GOPrivate, GraphicsBase, 
       GraphicsBasePrivate, LineTypeProp, LineTypePropPrivate, 
       Point3, PointProp, PointPropPrivate, Prop, PropPrivate, RealProp, 
       RealPropPrivate;

REVEAL 
  T = Public BRANDED OBJECT
  OVERRIDES
    init              := Init;
    draw              := Draw;
    damageIfDependent := DamageIfDependent;
    needsTransparency := NeedsTransparency;
  END;


PROCEDURE Init (self : T) : T = 
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
    WITH p1 = Point1.getState (state),
         p2 = Point2.getState (state) DO
      state.drawLine (p1, p2);
      state.growBoundingVolume (Point3.MidPoint (p1, p2), 
                                Point3.Distance (p1, p2) / 2.0);
    END;
    state.pop (self);
  END Draw;


PROCEDURE DamageIfDependent (self : T; pn : Prop.Name) =
  BEGIN
    IF pn = Point1 OR pn = Point2 THEN
      self.damaged := TRUE;
    END;
  END DamageIfDependent;


PROCEDURE NeedsTransparency (<* UNUSED *> self : T; 
                             <* UNUSED *> t    : REAL) : BOOLEAN =
  BEGIN
    RETURN FALSE;
  END NeedsTransparency;


PROCEDURE New (p1, p2 : Point3.T) : T =
  VAR
    line := NEW (T).init ();
  BEGIN
    SetPoint1 (line, p1);
    SetPoint2 (line, p2);
    RETURN line;
  END New;


(*****************************************************************************)
(* Colour_PN                                                                 *)
(*****************************************************************************)

TYPE
  Colour_PN = ColorProp.Name OBJECT
  OVERRIDES
    damage := DamageColour;
    push   := PushColour;
    pop    := PopColour;
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
        state.setLineColor (val);
      END;
      stack.push (val);
    END;
  END PushColour;


PROCEDURE PopColour (self : Colour_PN; state : GraphicsBase.T) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], ColorPropPrivate.Stack) DO
      state.setLineColor (stack.pop ());
    END;
  END PopColour;
    

(*****************************************************************************)
(* Width_PN                                                                  *)
(*****************************************************************************)

TYPE
  Width_PN = RealProp.Name OBJECT
  OVERRIDES
    damage := DamageWidth;
    push   := PushWidth;
    pop    := PopWidth;
  END;


PROCEDURE DamageWidth (<* UNUSED *> self : Width_PN; caller : GO.T) =
  BEGIN
    caller.damaged := TRUE;
  END DamageWidth;


PROCEDURE PushWidth (self  : Width_PN; 
                     state : GraphicsBase.T; 
                     pv    : Prop.Val) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], RealPropPrivate.Stack),
         val   = NARROW (pv, RealProp.Val).val DO
      IF stack.top # val THEN
        state.setLineWidth (val);
      END;
      stack.push (val);
    END;
  END PushWidth;


PROCEDURE PopWidth (self : Width_PN; state : GraphicsBase.T) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], RealPropPrivate.Stack) DO
      state.setLineWidth (stack.pop ());
    END;
  END PopWidth;


(*****************************************************************************)
(* Type_PN                                                                   *)
(*****************************************************************************)

TYPE
  Type_PN = LineTypeProp.Name OBJECT
  OVERRIDES
    damage := DamageType;
    push   := PushType;
    pop    := PopType;
  END;


PROCEDURE DamageType (<* UNUSED *> self : Type_PN; caller : GO.T) =
  BEGIN
    caller.damaged := TRUE;
  END DamageType;


PROCEDURE PushType (self  : Type_PN; 
                    state : GraphicsBase.T; 
                    pv    : Prop.Val) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], LineTypePropPrivate.Stack),
         val   = NARROW (pv, LineTypeProp.Val).val DO
      IF stack.top # val THEN
        state.setLineType (val);
      END;
      stack.push (val);
    END;
  END PushType;


PROCEDURE PopType (self : Type_PN; state : GraphicsBase.T) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], LineTypePropPrivate.Stack) DO
      state.setLineType (stack.pop ());
    END;
  END PopType;


(*****************************************************************************)
(* Convenience Procedures                                                    *)
(*****************************************************************************)


PROCEDURE SetColour (o : GO.T; v : Color.T) =
  BEGIN
    o.setProp (Colour.bind (ColorProp.NewConst (v)));
  END SetColour;


PROCEDURE SetWidth (o : GO.T; v : REAL) =
  BEGIN
    o.setProp (Width.bind (RealProp.NewConst (v)));
  END SetWidth;


PROCEDURE SetType (o : GO.T; v : LineTypeProp.Kind) =
  BEGIN
    o.setProp (Type.bind (LineTypeProp.NewConst (v)));
  END SetType;


PROCEDURE SetPoint1 (o : GO.T; v : Point3.T) =
  BEGIN
    o.setProp (Point1.bind (PointProp.NewConst (v)));
  END SetPoint1;


PROCEDURE SetPoint2 (o : GO.T; v : Point3.T) =
  BEGIN
    o.setProp (Point2.bind (PointProp.NewConst (v)));
  END SetPoint2;


(*****************************************************************************)
(* Module body                                                               *)
(*****************************************************************************)

BEGIN
  Colour := NEW (Colour_PN).init (Color.White);
  Width  := NEW (Width_PN).init (1.0);
  Type   := NEW (Type_PN).init (LineTypeProp.Kind.Solid);
  Point1 := NEW (PointProp.Name).init (Point3.T {0.0, 0.0, 0.0});
  Point2 := NEW (PointProp.Name).init (Point3.T {1.0, 0.0, 0.0});
END LineGO.
