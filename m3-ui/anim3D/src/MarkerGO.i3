(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Created by Marc Najork                                                    *)
(* Last modified on Mon Jun 13 10:38:22 PDT 1994 by najork                   *)


(* A "MarkerGO.T" is a geometric object describing a marker. *)

INTERFACE MarkerGO;

IMPORT Color, ColorProp, GO, MarkerTypeProp, Point3, PointProp, RealProp;

TYPE
  T <: Public;
  Public = GO.T OBJECT
  METHODS
    init () : T;
  END;
(* "m.init()" initializes "m", and returns it. The location of the marker 
   is determined by the property "Center". *)

VAR 
  Center : PointProp.Name;
  Colour : ColorProp.Name;
  Scale  : RealProp.Name;
  Type   : MarkerTypeProp.Name;

(* In addition to the properties observed by all \type{GO}{T}'s, there are 
   some additional properties that are observed by "MarkerGO.T"'s:

   "Center" is the name of a point property that describes the point indicated
   by the marker. If no "Center" property is specified, the marker is drawn at
   the origin. 

   "Colour" is the name of a color property that describes the color of the 
   marker. If no "Colour" property is specified, the marker is drawn in white.

   "Scale" is the name of a real property that describes the scale of the 
   marker. If no "Scale" property is specified, the marker is drawn with 
   scale 1.

   "Type" is the name of a marker type property that describes the type of 
   the marker (dot, cross, circle, asterisk, or X). If no "Type" property
   is specified, the marker is drawn as an asterisk. *)

PROCEDURE New (p : Point3.T) : T;
(* "New(p)" creates a new marker and returns it. It also attaches the 
   property "(Center,PointProp.NewConst(p))" to the new marker. *)

(* The following four procedures provide sugaring to attach the
   "Center", "Colour", "Scale", and "Type" properties to geometric objects: *)

PROCEDURE SetCenter (o : GO.T; v : Point3.T);
(* The expression "SetCenter(o,p)"is equivalent to
   "o.setProp(Center.bind(PointProp.NewConst(p)))". *)

PROCEDURE SetColour (o : GO.T; v : Color.T);
(* The expression "SetColour(o,c)" is equivalent to
   "o.setProp(Colour.bind(ColorProp.NewConst(c)))". *)

PROCEDURE SetScale (o : GO.T; v : REAL);
(* The expression "SetScale(o,r)" is equivalent to
   "o.setProp(Scale.bind(RealProp.NewConst(r)))". *)

PROCEDURE SetType (o : GO.T; v : MarkerTypeProp.Kind);
(* The expression "SetType(o,t)" is equivalent to
   "o.setProp(Type.bind(MarkerTypeProp.NewConst(t)))". *)

END MarkerGO.
