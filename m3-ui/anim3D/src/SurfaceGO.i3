(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Created by Marc Najork                                                    *)
(* Last modified on Fri Feb  3 13:57:57 PST 1995 by najork                   *)


(* A "SurfaceGO.T" is a geometric objects which is composed of surfaces
   (as opposed to lines). Surfaces differ from non-surfaces in a number of 
   ways:
   \begin{itemize}
   \item Surfaces interact with light sources, non-surfaces don't
   \item Surfaces can be transparent, non-surfaces are always opaque
   \end{itemize}
*)

INTERFACE SurfaceGO;

IMPORT BooleanProp, Color, ColorProp, GO, LineTypeProp, RasterModeProp, 
       RealProp, ShadingProp;

TYPE T <: GO.T;
(* A "SurfaceGO.T" is a subtype of \type{GO}{T}. No additional fields or 
   methods are revealed to the user. *)

VAR
  DistinguishFacets        : BooleanProp.Name;
  Colour                   : ColorProp.Name;
  BackColour               : ColorProp.Name;
  RasterMode               : RasterModeProp.Name;
  AmbientReflectionCoeff   : RealProp.Name;
  DiffuseReflectionCoeff   : RealProp.Name;
  SpecularReflectionCoeff  : RealProp.Name;
  SpecularReflectionConc   : RealProp.Name;
  TransmissionCoeff        : RealProp.Name;
  SpecularReflectionColour : ColorProp.Name;
  Lighting                 : BooleanProp.Name;
  Shading                  : ShadingProp.Name;
  EdgeVisibility           : BooleanProp.Name;
  EdgeColour               : ColorProp.Name;
  EdgeType                 : LineTypeProp.Name;
  EdgeWidth                : RealProp.Name;

(* In addition to the properties observed by all \type{GO}{T}'s, there are 
   various additional properties that are observed by "SurfaceGO.T"'s:

   "DistinguishFacets" is the name of a boolean property that decides whether 
   front- and back-faces of surfaces should be distinguished. If this property
   is not specified, front- and back-faces are not distinguished. 

   {\em NOTE: In order for this property to make sense, I should introduce
   other surface-related properties: All properties from "Colour" to
   "Shading" could have backface analogs.}

   "Color" is the name of a color property that specifies the color of the 
   surface. If this property is not specified, the surface is drawn in white.

   "RasterMode" is the name of an raster mode property that specifies what 
   rasterization technique is used to draw the surface (solid, hollow, or 
   empty). If this property is not specified, the surface is drawn solid.

   "AmbientReflectionCoeff" is the name of a real property that specifies the
   ambient reflection coefficient of the surface. If this property is not 
   specified, the ambient reflection coefficient is taken to be 0.5.

   "DiffuseReflectionCoeff" is the name of a real property that specifies the
   diffuse reflection coefficient of the surface. If this property is not 
   specified, the diffuse reflection coefficient is taken to be 1.

   "SpecularReflectionCoeff" is the name of a real property that specifies the
   specular reflection coefficient of the surface. If this property is not 
   specified, the specular reflection coefficient is taken to be 0.

   "SpecularReflectionConc" is the name of a real property that specifies the 
   specular reflection concentration of the surface. If this property is not
   specified, the specular reflection concentration is taken to be 0.

   "TransmissionCoeff" is the name of a real property that specifies the 
   transparency of the surface (with 0 being completely opaque and 1 being 
   completely transparent). If this property is not specified, the 
   transmission coefficient is taken to be 0.

   "SpecularReflectionColour" is the name of a color property that specifies 
   the color of specular reflected light. If this property is not specified, 
   the specular reflection color is taken to be white.

   "Lighting" is the name of a boolean property that decides whether or not 
   lighting and shading computations shall be performed. If this property is 
   not specified, lighting and shading computations are performed.

   "Shading" is the name of a shading property that specifies what shading 
   method shall be used. If this property is not specified, flat shading is 
   used.

   "EdgeVisibility" is the name of a boolean property that specifies whether
   or not the edges of the individual polygons of the surface shall be 
   outlined. If this property is not specified, no surface edges will be drawn.

   "EdgeColour" is the name of a color property that determines the color
   of the surface edges (if they are visible). If this property is not 
   specified and edges are visible, they are drawn in white.

   "EdgeType" is the name of a line type property that determines the line
   type of the surface edges (provided they are visible). If this property 
   is not specified and edges are visible, they are drawn solid.

   "EdgeWidth" is the name of a real property that determines the width of the 
   surface edges (provided they are visible). If this property is not specified
   and edges are visible, they are drawn with a width of 1. 

   The following procedures provide sugaring to attach surface-related 
   properties to geometric objects: *)

PROCEDURE SetDistinguishFacets (o : GO.T; v : BOOLEAN);
(* The expression "SetDistinguishFacets(o,v)" is equivalent to
   "o.setProp(DistinguishFacets.bind(BooleanProp.NewConst(v)))". *)

PROCEDURE SetColour (o : GO.T; v : Color.T);
(* The expression "SetColour(o,v)" is equivalent to
   "o.setProp(Colour.bind(ColorProp.NewConst(v)))". *)

PROCEDURE SetBackColour (o : GO.T; v : Color.T);
(* The expression "SetBackColour(o,v)" is equivalent to
   "o.setProp(BackColour.bind(ColorProp.NewConst(v)))". *)

PROCEDURE SetRasterMode (o : GO.T; v : RasterModeProp.Kind);
(* The expression "SetRasterMode(o,v)" is equivalent to
   "o.setProp(RasterMode.bind(RasterModeProp.NewConst(v)))". *)

PROCEDURE SetAmbientReflectionCoeff (o : GO.T; v : REAL);
(* The expression "SetAmbientReflectionCoeff(o,v)" is equivalent to
   "o.setProp(AmbientReflectionCoeff.bind(RealProp.NewConst(v)))". *)

PROCEDURE SetDiffuseReflectionCoeff (o : GO.T; v : REAL);
(* The expression "SetDiffuseReflectionCoeff(o,v)" is equivalent to
   "o.setProp(DiffuseReflectionCoeff.bind(RealProp.NewConst(v)))". *)

PROCEDURE SetSpecularReflectionCoeff (o : GO.T; v : REAL);
(* The expression "SetSpecularReflectionCoeff(o,v)" is equivalent to
   "o.setProp(SpecularReflectionCoeff.bind(RealProp.NewConst(v)))". *)

PROCEDURE SetSpecularReflectionConc (o : GO.T; v : REAL);
(* The expression "SetSpecularReflectionConc(o,v)" is equivalent to
   "o.setProp(SpecularReflectionConc.bind(RealProp.NewConst(v)))". *)

PROCEDURE SetTransmissionCoeff (o : GO.T; v : REAL);
(* The expression "SetTransmissionCoeff(o,v)" is equivalent to
   "o.setProp(TransmissionCoeff.bind(RealProp.NewConst(v)))". *)

PROCEDURE SetSpecularReflectionColour (o : GO.T; v : Color.T);
(* The expression "SetSpecularReflectionColour(o,v)" is equivalent to
   "o.setProp(SpecularReflectionColour.bind(ColorProp.NewConst(v)))". *)

PROCEDURE SetLighting (o : GO.T; v : BOOLEAN);
(* The expression "SetLighting(o,v)" is equivalent to
   "o.setProp(Lighting.bind(BooleanProp.NewConst(v)))". *)

PROCEDURE SetShading (o : GO.T; v : ShadingProp.Kind);
(* The expression "SetShading(o,v)" is equivalent to
   "o.setProp(Shading.bind(ShadingProp.NewConst(v)))". *)

PROCEDURE SetEdgeVisibility (o : GO.T; v : BOOLEAN);
(* The expression "SetEdgeVisibility(o,v)" is equivalent to
   "o.setProp(EdgeVisibility.bind(BooleanProp.NewConst(v)))". *)

PROCEDURE SetEdgeColour (o : GO.T; v : Color.T);
(* The expression "SetEdgeColour(o,v)" is equivalent to
   "o.setProp(EdgeColour.bind(ColorProp.NewConst(v)))". *)

PROCEDURE SetEdgeType (o : GO.T; v : LineTypeProp.Kind);
(* The expression "SetEdgeType(o,v)" is equivalent to
   "o.setProp(EdgeType.bind(LineTypeProp.NewConst(v)))". *)

PROCEDURE SetEdgeWidth (o : GO.T; v : REAL);
(* The expression "SetEdgeWidth(o,v)" is equivalent to
   "o.setProp(EdgeWidth.bind(RealProp.NewConst(v)))". *)

END SurfaceGO.
