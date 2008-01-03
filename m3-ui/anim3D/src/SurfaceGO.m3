(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Created by Marc Najork                                                    *)
(* Last modified on Fri Feb  3 14:02:37 PST 1995 by najork                   *)


MODULE SurfaceGO;

IMPORT BooleanProp, BooleanPropPrivate, Color, ColorProp, ColorPropPrivate,
       GO, GOPrivate, GraphicsBase, GraphicsBasePrivate, LineTypeProp, 
       LineTypePropPrivate, Prop, PropPrivate, RasterModeProp, 
       RasterModePropPrivate, RealProp, RealPropPrivate, ShadingProp, 
       ShadingPropPrivate;

REVEAL
  T = GO.T BRANDED OBJECT
  OVERRIDES
    needsTransparency := NeedsTransparency;
  END;


PROCEDURE NeedsTransparency (self : T; t : REAL) : BOOLEAN =
  BEGIN
    IF self.trans # FIRST(REAL) THEN
      t := self.trans;
    END;
    RETURN t > 0.0;
  END NeedsTransparency;


(*****************************************************************************)
(* DistinguishFacets_PN                                                      *)
(*****************************************************************************)

TYPE
  DistinguishFacets_PN = BooleanProp.Name OBJECT
  OVERRIDES
    damage := DamageDistinguishFacets;
    push   := PushDistinguishFacets;
    pop    := PopDistinguishFacets;
  END;


PROCEDURE DamageDistinguishFacets (<* UNUSED *> self   : DistinguishFacets_PN; 
                                                caller : GO.T) =
  BEGIN
    caller.damaged := TRUE;
  END DamageDistinguishFacets;


PROCEDURE PushDistinguishFacets (self  : DistinguishFacets_PN; 
                                 state : GraphicsBase.T;
                                 pv    : Prop.Val) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], BooleanPropPrivate.Stack),
         val   = NARROW (pv, BooleanProp.Val).val DO
      IF stack.top # val THEN
        state.setDistinguishFacetsFlag (val);
      END;
      stack.push (val);
    END;
  END PushDistinguishFacets;


PROCEDURE PopDistinguishFacets (self  : DistinguishFacets_PN; 
                                state : GraphicsBase.T) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], BooleanPropPrivate.Stack) DO
      state.setDistinguishFacetsFlag (stack.pop ());
    END;
  END PopDistinguishFacets;


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
        state.setSurfaceColor (val);
      END;
      stack.push (val);
    END;
  END PushColour;


PROCEDURE PopColour (self  : Colour_PN; 
                     state : GraphicsBase.T) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], ColorPropPrivate.Stack) DO
      state.setSurfaceColor (stack.pop ());
    END;
  END PopColour;


(*****************************************************************************)
(* BackColour_PN                                                             *)
(*****************************************************************************)

TYPE
  BackColour_PN = ColorProp.Name OBJECT
  OVERRIDES
    damage := DamageBackColour;
    push   := PushBackColour;
    pop    := PopBackColour;
  END;


PROCEDURE DamageBackColour (<* UNUSED *> self : BackColour_PN; caller : GO.T) =
  BEGIN
    caller.damaged := TRUE;
  END DamageBackColour;


PROCEDURE PushBackColour (self  : BackColour_PN; 
                          state : GraphicsBase.T;
                          pv    : Prop.Val) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], ColorPropPrivate.Stack),
         val   = NARROW (pv, ColorProp.Val).val DO
      IF stack.top # val THEN
        state.setSurfaceBackColor (val);
      END;
      stack.push (val);
    END;
  END PushBackColour;


PROCEDURE PopBackColour (self  : BackColour_PN; 
                         state : GraphicsBase.T) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], ColorPropPrivate.Stack) DO
      state.setSurfaceBackColor (stack.pop ());
    END;
  END PopBackColour;


(*****************************************************************************)
(* RasterMode_PN                                                             *)
(*****************************************************************************)

TYPE
  RasterMode_PN = RasterModeProp.Name OBJECT
  OVERRIDES
    damage := DamageRasterMode;
    push   := PushRasterMode;
    pop    := PopRasterMode;
  END;


PROCEDURE DamageRasterMode (<* UNUSED *> self   : RasterMode_PN; 
                                         caller : GO.T) =
  BEGIN
    caller.damaged := TRUE;
  END DamageRasterMode;


PROCEDURE PushRasterMode (self  : RasterMode_PN; 
                          state : GraphicsBase.T;
                          pv    : Prop.Val) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], RasterModePropPrivate.Stack),
         val   = NARROW (pv, RasterModeProp.Val).val DO
      IF stack.top # val THEN
        state.setRasterMode (val);
      END;
      stack.push (val);
    END;
  END PushRasterMode;


PROCEDURE PopRasterMode (self  : RasterMode_PN; 
                         state : GraphicsBase.T) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], RasterModePropPrivate.Stack) DO
      state.setRasterMode (stack.pop ());
    END;
  END PopRasterMode;


(*****************************************************************************)
(* AmbientReflectionCoeff_PN                                                 *)
(*****************************************************************************)

TYPE
  AmbientReflectionCoeff_PN = RealProp.Name OBJECT
  OVERRIDES
    damage := DamageAmbientReflectionCoeff;
    push   := PushAmbientReflectionCoeff;
    pop    := PopAmbientReflectionCoeff;
  END;


PROCEDURE DamageAmbientReflectionCoeff (
    <* UNUSED *> self   : AmbientReflectionCoeff_PN; 
                 caller : GO.T) =
  BEGIN
    caller.damaged := TRUE;
  END DamageAmbientReflectionCoeff;


PROCEDURE PushAmbientReflectionCoeff (self  : AmbientReflectionCoeff_PN; 
                                      state : GraphicsBase.T;
                                      pv    : Prop.Val) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], RealPropPrivate.Stack),
         val   = NARROW (pv, RealProp.Val).val DO
      IF stack.top # val THEN
        state.setAmbientReflCoeff (val);
      END;
      stack.push (val);
    END;
  END PushAmbientReflectionCoeff;


PROCEDURE PopAmbientReflectionCoeff (self  : AmbientReflectionCoeff_PN; 
                                     state : GraphicsBase.T) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], RealPropPrivate.Stack) DO
      state.setAmbientReflCoeff (stack.pop ());
    END;
  END PopAmbientReflectionCoeff;


(*****************************************************************************)
(* DiffuseReflectionCoeff_PN                                                 *)
(*****************************************************************************)

TYPE
  DiffuseReflectionCoeff_PN = RealProp.Name OBJECT
  OVERRIDES
    damage := DamageDiffuseReflectionCoeff;
    push   := PushDiffuseReflectionCoeff;
    pop    := PopDiffuseReflectionCoeff;
  END;


PROCEDURE DamageDiffuseReflectionCoeff (
    <* UNUSED *> self   : DiffuseReflectionCoeff_PN; 
                 caller : GO.T) =
  BEGIN
    caller.damaged := TRUE;
  END DamageDiffuseReflectionCoeff;


PROCEDURE PushDiffuseReflectionCoeff (self  : DiffuseReflectionCoeff_PN; 
                                      state : GraphicsBase.T;
                                      pv    : Prop.Val) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], RealPropPrivate.Stack),
         val   = NARROW (pv, RealProp.Val).val DO
      IF stack.top # val THEN
        state.setDiffuseReflCoeff (val);
      END;
      stack.push (val);
    END;
  END PushDiffuseReflectionCoeff;


PROCEDURE PopDiffuseReflectionCoeff (self  : DiffuseReflectionCoeff_PN; 
                                     state : GraphicsBase.T) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], RealPropPrivate.Stack) DO
      state.setDiffuseReflCoeff (stack.pop ());
    END;
  END PopDiffuseReflectionCoeff;


(*****************************************************************************)
(* SpecularReflectionCoeff_PN                                                *)
(*****************************************************************************)

TYPE
  SpecularReflectionCoeff_PN = RealProp.Name OBJECT
  OVERRIDES
    damage := DamageSpecularReflectionCoeff;
    push   := PushSpecularReflectionCoeff;
    pop    := PopSpecularReflectionCoeff;
  END;


PROCEDURE DamageSpecularReflectionCoeff (
    <* UNUSED *> self   : SpecularReflectionCoeff_PN; 
                 caller : GO.T) =
  BEGIN
    caller.damaged := TRUE;
  END DamageSpecularReflectionCoeff;


PROCEDURE PushSpecularReflectionCoeff (self  : SpecularReflectionCoeff_PN; 
                                       state : GraphicsBase.T;
                                       pv    : Prop.Val) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], RealPropPrivate.Stack),
         val   = NARROW (pv, RealProp.Val).val DO
      IF stack.top # val THEN
        state.setSpecularReflCoeff (val);
      END;
      stack.push (val);
    END;
  END PushSpecularReflectionCoeff;


PROCEDURE PopSpecularReflectionCoeff (self  : SpecularReflectionCoeff_PN; 
                                      state : GraphicsBase.T) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], RealPropPrivate.Stack) DO
      state.setSpecularReflCoeff (stack.pop ());
    END;
  END PopSpecularReflectionCoeff;


(*****************************************************************************)
(* SpecularReflectionConcPN                                                  *)
(*****************************************************************************)

TYPE
  SpecularReflectionConcPN = RealProp.Name OBJECT
  OVERRIDES
    damage := DamageSpecularReflectionConc;
    push   := PushSpecularReflectionConc;
    pop    := PopSpecularReflectionConc;
  END;


PROCEDURE DamageSpecularReflectionConc (
    <* UNUSED *> self   : SpecularReflectionConcPN; 
                 caller : GO.T) =
  BEGIN
    caller.damaged := TRUE;
  END DamageSpecularReflectionConc;


PROCEDURE PushSpecularReflectionConc (self  : SpecularReflectionConcPN; 
                                      state : GraphicsBase.T;
                                      pv    : Prop.Val) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], RealPropPrivate.Stack),
         val   = NARROW (pv, RealProp.Val).val DO
      IF stack.top # val THEN
        state.setSpecularReflConc (val);
      END;
      stack.push (val);
    END;
  END PushSpecularReflectionConc;


PROCEDURE PopSpecularReflectionConc (self  : SpecularReflectionConcPN; 
                                     state : GraphicsBase.T) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], RealPropPrivate.Stack) DO
      state.setSpecularReflConc (stack.pop ());
    END;
  END PopSpecularReflectionConc;


(*****************************************************************************)
(* TransmissionCoeff_PN                                                      *)
(*****************************************************************************)

TYPE
  TransmissionCoeff_PN = RealProp.Name OBJECT
  OVERRIDES
    damage := DamageTransmissionCoeff;
    push   := PushTransmissionCoeff;
    pop    := PopTransmissionCoeff;
  END;


PROCEDURE DamageTransmissionCoeff (
    <* UNUSED *> self   : TransmissionCoeff_PN; 
                 caller : GO.T) =
  BEGIN
    caller.damaged := TRUE;
  END DamageTransmissionCoeff;


PROCEDURE PushTransmissionCoeff (self  : TransmissionCoeff_PN; 
                                 state : GraphicsBase.T;
                                 pv    : Prop.Val) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], RealPropPrivate.Stack),
         val   = NARROW (pv, RealProp.Val).val DO
      IF stack.top # val THEN
        state.setTransmissionCoeff (val);
      END;
      stack.push (val);
    END;
  END PushTransmissionCoeff;


PROCEDURE PopTransmissionCoeff (self  : TransmissionCoeff_PN; 
                                state : GraphicsBase.T) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], RealPropPrivate.Stack) DO
      state.setTransmissionCoeff (stack.pop ());
    END;
  END PopTransmissionCoeff;


(*****************************************************************************)
(* SpecularReflectionColour_PN                                               *)
(*****************************************************************************)

TYPE
  SpecularReflectionColour_PN = ColorProp.Name OBJECT
  OVERRIDES
    damage := DamageSpecularReflectionColour;
    push   := PushSpecularReflectionColour;
    pop    := PopSpecularReflectionColour;
  END;


PROCEDURE DamageSpecularReflectionColour (
    <* UNUSED *> self   : SpecularReflectionColour_PN; 
                 caller : GO.T) =
  BEGIN
    caller.damaged := TRUE;
  END DamageSpecularReflectionColour;


PROCEDURE PushSpecularReflectionColour (self  : SpecularReflectionColour_PN; 
                                        state : GraphicsBase.T;
                                        pv    : Prop.Val) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], ColorPropPrivate.Stack),
         val   = NARROW (pv, ColorProp.Val).val DO
      IF stack.top # val THEN
        state.setSpecularReflColor (val);
      END;
      stack.push (val);
    END;
  END PushSpecularReflectionColour;


PROCEDURE PopSpecularReflectionColour (self  : SpecularReflectionColour_PN; 
                                       state : GraphicsBase.T) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], ColorPropPrivate.Stack) DO
      state.setSpecularReflColor (stack.pop ());
    END;
  END PopSpecularReflectionColour;


(*****************************************************************************)
(* Lighting_PN                                                               *)
(*****************************************************************************)

TYPE
  Lighting_PN = BooleanProp.Name OBJECT
  OVERRIDES
    damage := DamageLighting;
    push   := PushLighting;
    pop    := PopLighting;
  END;


PROCEDURE DamageLighting (<* UNUSED *> self   : Lighting_PN; 
                                              caller : GO.T) =
  BEGIN
    caller.damaged := TRUE;
  END DamageLighting;


PROCEDURE PushLighting (self  : Lighting_PN; 
                               state : GraphicsBase.T;
                               pv    : Prop.Val) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], BooleanPropPrivate.Stack),
         val   = NARROW (pv, BooleanProp.Val).val DO
      IF stack.top # val THEN
        state.setLighting (val);
      END;
      stack.push (val);
    END;
  END PushLighting;


PROCEDURE PopLighting (self  : Lighting_PN; 
                              state : GraphicsBase.T) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], BooleanPropPrivate.Stack) DO
      state.setLighting (stack.pop ());
    END;
  END PopLighting;


(*****************************************************************************)
(* Shading_PN                                                                *)
(*****************************************************************************)

TYPE
  Shading_PN = ShadingProp.Name OBJECT
  OVERRIDES
    damage := DamageShading;
    push   := PushShading;
    pop    := PopShading;
  END;


PROCEDURE DamageShading (<* UNUSED *> self : Shading_PN; caller : GO.T) =
  BEGIN
    caller.damaged := TRUE;
  END DamageShading;


PROCEDURE PushShading (self  : Shading_PN; 
                       state : GraphicsBase.T;
                       pv    : Prop.Val) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], ShadingPropPrivate.Stack),
         val   = NARROW (pv, ShadingProp.Val).val DO
      IF stack.top # val THEN
        state.setShading (val);
      END;
      stack.push (val);
    END;
  END PushShading;


PROCEDURE PopShading (self : Shading_PN; state : GraphicsBase.T) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], ShadingPropPrivate.Stack) DO
      state.setShading (stack.pop ());
    END;
  END PopShading;


(*****************************************************************************)
(* EdgeVisibility_PN                                                         *)
(*****************************************************************************)

TYPE
  EdgeVisibility_PN = BooleanProp.Name OBJECT
  OVERRIDES
    damage := DamageEdgeVisibility;
    push   := PushEdgeVisibility;
    pop    := PopEdgeVisibility;
  END;


PROCEDURE DamageEdgeVisibility (<* UNUSED *> self   : EdgeVisibility_PN; 
                                             caller : GO.T) =
  BEGIN
    caller.damaged := TRUE;
  END DamageEdgeVisibility;


PROCEDURE PushEdgeVisibility (self  : EdgeVisibility_PN; 
                              state : GraphicsBase.T;
                              pv    : Prop.Val) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], BooleanPropPrivate.Stack),
         val   = NARROW (pv, BooleanProp.Val).val DO
      IF stack.top # val THEN
        state.setSurfaceEdgeFlag (val);
      END;
      stack.push (val);
    END;
  END PushEdgeVisibility;


PROCEDURE PopEdgeVisibility (self  : EdgeVisibility_PN; 
                             state : GraphicsBase.T) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], BooleanPropPrivate.Stack) DO
      state.setSurfaceEdgeFlag (stack.pop ());
    END;
  END PopEdgeVisibility;


(*****************************************************************************)
(* EdgeColour_PN                                                             *)
(*****************************************************************************)


TYPE
  EdgeColour_PN = ColorProp.Name OBJECT
  OVERRIDES
    damage := DamageEdgeColour;
    push   := PushEdgeColour;
    pop    := PopEdgeColour;
  END;


PROCEDURE DamageEdgeColour (<* UNUSED *> self   : EdgeColour_PN; 
                                         caller : GO.T) =
  BEGIN
    caller.damaged := TRUE;
  END DamageEdgeColour;


PROCEDURE PushEdgeColour (self  : EdgeColour_PN; 
                          state : GraphicsBase.T; 
                          pv    : Prop.Val) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], ColorPropPrivate.Stack),
         val   = NARROW (pv, ColorProp.Val).val DO
      IF stack.top # val THEN
        state.setSurfaceEdgeColor (val);
      END;
      stack.push (val);
    END;
  END PushEdgeColour;


PROCEDURE PopEdgeColour (self : EdgeColour_PN; state : GraphicsBase.T) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], ColorPropPrivate.Stack) DO
      state.setSurfaceEdgeColor (stack.pop ());
    END;
  END PopEdgeColour;


(*****************************************************************************)
(* EdgeType_PN                                                               *)
(*****************************************************************************)

TYPE
  EdgeType_PN = LineTypeProp.Name OBJECT
  OVERRIDES
    damage := DamageEdgeType;
    push   := PushEdgeType;
    pop    := PopEdgeType;
  END;


PROCEDURE DamageEdgeType (<* UNUSED *> self   : EdgeType_PN; 
                                       caller : GO.T) =
  BEGIN
    caller.damaged := TRUE;
  END DamageEdgeType;


PROCEDURE PushEdgeType (self  : EdgeType_PN; 
                        state : GraphicsBase.T; 
                        pv    : Prop.Val) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], LineTypePropPrivate.Stack),
         val   = NARROW (pv, LineTypeProp.Val).val DO
      IF stack.top # val THEN
        state.setSurfaceEdgeType (val);
      END;
      stack.push (val);
    END;
  END PushEdgeType;


PROCEDURE PopEdgeType (self : EdgeType_PN; state : GraphicsBase.T) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], LineTypePropPrivate.Stack) DO
      state.setSurfaceEdgeType (stack.pop ());
    END;
  END PopEdgeType;


(*****************************************************************************)
(* EdgeWidth_PN                                                              *)
(*****************************************************************************)

TYPE
  EdgeWidth_PN = RealProp.Name OBJECT
  OVERRIDES
    damage := DamageEdgeWidth;
    push   := PushEdgeWidth;
    pop    := PopEdgeWidth;
  END;


PROCEDURE DamageEdgeWidth (<* UNUSED *> self   : EdgeWidth_PN; 
                                        caller : GO.T) =
  BEGIN
    caller.damaged := TRUE;
  END DamageEdgeWidth;


PROCEDURE PushEdgeWidth (self  : EdgeWidth_PN; 
                         state : GraphicsBase.T; 
                         pv    : Prop.Val) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], RealPropPrivate.Stack),
         val   = NARROW (pv, RealProp.Val).val DO
      IF stack.top # val THEN
        state.setSurfaceEdgeWidth (val);
      END;
      stack.push (val);
    END;
  END PushEdgeWidth;


PROCEDURE PopEdgeWidth (self : EdgeWidth_PN; state : GraphicsBase.T) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], RealPropPrivate.Stack) DO
      state.setSurfaceEdgeWidth (stack.pop ());
    END;
  END PopEdgeWidth;


(*****************************************************************************)
(* Convenience Procedures                                                    *)
(*****************************************************************************)

PROCEDURE SetDistinguishFacets (o : GO.T; v : BOOLEAN) =
  BEGIN
    o.setProp (DistinguishFacets.bind (BooleanProp.NewConst (v)));
  END SetDistinguishFacets;


PROCEDURE SetColour (o : GO.T; v : Color.T) =
  BEGIN
    o.setProp (Colour.bind (ColorProp.NewConst (v)));
  END SetColour;


PROCEDURE SetBackColour (o : GO.T; v : Color.T) =
  BEGIN
    o.setProp (BackColour.bind (ColorProp.NewConst (v)));
  END SetBackColour;


PROCEDURE SetRasterMode (o : GO.T; v : RasterModeProp.Kind) =
  BEGIN
    o.setProp (RasterMode.bind (RasterModeProp.NewConst (v)));
  END SetRasterMode;


PROCEDURE SetAmbientReflectionCoeff  (o : GO.T; v : REAL) =
  BEGIN
    o.setProp (AmbientReflectionCoeff.bind (RealProp.NewConst (v)));
  END SetAmbientReflectionCoeff;


PROCEDURE SetDiffuseReflectionCoeff  (o : GO.T; v : REAL) =
  BEGIN
    o.setProp (DiffuseReflectionCoeff.bind (RealProp.NewConst (v)));
  END SetDiffuseReflectionCoeff;


PROCEDURE SetSpecularReflectionCoeff (o : GO.T; v : REAL) =
  BEGIN
    o.setProp (SpecularReflectionCoeff.bind (RealProp.NewConst (v)));
  END SetSpecularReflectionCoeff;


PROCEDURE SetSpecularReflectionConc (o : GO.T; v : REAL) =
  BEGIN
    o.setProp (SpecularReflectionConc.bind (RealProp.NewConst (v)));
  END SetSpecularReflectionConc;


PROCEDURE SetTransmissionCoeff (o : GO.T; v : REAL) =
  BEGIN
    o.setProp (TransmissionCoeff.bind (RealProp.NewConst (v)));
  END SetTransmissionCoeff;


PROCEDURE SetSpecularReflectionColour (o : GO.T; v : Color.T) =
  BEGIN
    o.setProp (SpecularReflectionColour.bind (ColorProp.NewConst (v)));
  END SetSpecularReflectionColour;


PROCEDURE SetLighting (o : GO.T; v : BOOLEAN) =
  BEGIN
    o.setProp (Lighting.bind (BooleanProp.NewConst (v)));
  END SetLighting;


PROCEDURE SetShading (o : GO.T; v : ShadingProp.Kind) =
  BEGIN
    o.setProp (Shading.bind (ShadingProp.NewConst (v)));
  END SetShading;


PROCEDURE SetEdgeVisibility (o : GO.T; v : BOOLEAN) =
  BEGIN
    o.setProp (EdgeVisibility.bind (BooleanProp.NewConst (v)));
  END SetEdgeVisibility;


PROCEDURE SetEdgeColour (o : GO.T; v : Color.T) =
  BEGIN
    o.setProp (EdgeColour.bind (ColorProp.NewConst (v)));
  END SetEdgeColour;


PROCEDURE SetEdgeType (o : GO.T; v : LineTypeProp.Kind) =
  BEGIN
    o.setProp (EdgeType.bind (LineTypeProp.NewConst (v)));
  END SetEdgeType;


PROCEDURE SetEdgeWidth (o : GO.T; v : REAL) =
  BEGIN
    o.setProp (EdgeWidth.bind (RealProp.NewConst (v)));
  END SetEdgeWidth;


(*****************************************************************************)
(* Main body                                                                 *)
(*****************************************************************************)

BEGIN
  DistinguishFacets := 
      NEW (DistinguishFacets_PN).init (FALSE);
  Colour := 
      NEW (Colour_PN).init (Color.White);
  BackColour := 
      NEW (BackColour_PN).init (GraphicsBasePrivate.VoidColor);
  RasterMode := 
      NEW (RasterMode_PN).init (RasterModeProp.Kind.Solid);
  AmbientReflectionCoeff := 
      NEW (AmbientReflectionCoeff_PN).init (0.5);
  DiffuseReflectionCoeff := 
      NEW (DiffuseReflectionCoeff_PN).init (1.0);
  SpecularReflectionCoeff := 
      NEW (SpecularReflectionCoeff_PN).init (0.0);
  SpecularReflectionConc := 
      NEW (SpecularReflectionConcPN).init (0.0);
  TransmissionCoeff := 
      NEW (TransmissionCoeff_PN).init (0.0);        (* DIFFERS FROM MANUAL *)
  SpecularReflectionColour := 
      NEW (SpecularReflectionColour_PN).init (Color.White);
  Lighting := 
      NEW (Lighting_PN).init (TRUE);
  Shading := 
      NEW (Shading_PN).init (ShadingProp.Kind.Flat);
  EdgeVisibility := 
      NEW (EdgeVisibility_PN).init (FALSE);
  EdgeColour := 
      NEW (EdgeColour_PN).init (Color.White);
  EdgeType := 
      NEW (EdgeType_PN).init (LineTypeProp.Kind.Solid);
  EdgeWidth := 
      NEW (EdgeWidth_PN).init (1.0);
END SurfaceGO.
