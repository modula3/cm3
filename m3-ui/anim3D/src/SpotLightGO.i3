(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Thu Sep 29 17:26:57 PDT 1994 by najork                   *)
(*       Created on Fri Feb  4 15:33:05 PST 1994 by najork                   *)

(* A "SpotLightGO.T" is a geometric object that describes a light source which
   is emitting a cone of colored light.

   The amount of light that falls onto another geometric object depends on the 
   distance between this object and the light source. This phenomenon is 
   referred to as {\em attenuation}. Refer to the description of the 
   \interface{PointLightGO} interface for details.

   The geometry of the light cone is described by three parameters: 
   The location or {\em origin} of the light source (i.e.\ the tip of the
   cone), the direction vector (going out of the center of the cone),
   and $\alpha$, the angle between the center vector of the cone and 
   its walls. 

   The concentration $\gamma$ describes how the light intensity is distributed
   within the cone. By definition, the intensity in the center of the cone is
   1. Let $\beta$ be the angle between a particular ray of light within the 
   cone and the center of the cone. Then the intensity of this ray is 
   ${\rm cos}^\gamma(\beta)$. So, if $\gamma$ is 0, the intensity is uniform 
   all over the cone (modulo attenuation). *)

INTERFACE SpotLightGO;

IMPORT Color, GO, LightGO, PointProp, Point3, RealProp;

TYPE
  T <: Public;
  Public = LightGO.T OBJECT
  METHODS
    init () : T;
  END;
(* "l.init()" initializes a new spot light source "l" and returns it. *)


PROCEDURE New (c : Color.T;  orig, dir : Point3.T; 
               conc, spread, att0, att1 : REAL) : T;
(* "l.init(c,orig,dir,conc,spread,att0,att1)" initializes a new spot light 
   source "l" and returns it. It also attaches the following properties to "l":
   \begin{verbatim}
      (LightGO.Colour,ColourProp.NewConst(c)) 
      (LightGO.Switch,BooleanProp.NewConst(TRUE))
      (Origin,PointProp.NewConst(dir))
      (Direction,PointProp.NewConst(dir))
      (Concentration,RealProp.NewConst(conc))
      (SpreadAngle,RealProp.NewConst(spread))
      (Attenuation0,RealProp.NewConst(att0))
      (Attenuation1,RealProp.NewConst(att1))
   \end{verbatim}
*)


VAR
  Origin        : PointProp.Name;
  Direction     : PointProp.Name;
  Concentration : RealProp.Name;
  SpreadAngle   : RealProp.Name;
  Attenuation0  : RealProp.Name;
  Attenuation1  : RealProp.Name;
(* In addition to the properties observed by all \type{GO}{T}'s and 
   \type{LightGO}{T}'s, there are six additional properties that are 
   observed by "SpotLightGO.T"'s. 

   "Origin" is the name of a property that determines the origin 
   of the light emitted by the light sources; it associates with a property 
   value of type \type{PointProp}{Val}. 

   "Direction" is the name of a property that determines the direction 
   of the light emitted by the light sources; it associates with a property 
   value of type \type{PointProp}{Val}. 
   
   "Concentration" is a property that determines the intensity distribution 
   within the light cone emitted by the spot light; it associates with a 
   property value of type \type{RealProp}{Val}. 

   "SpreadAngle" is a property that determines the angle (in radians) between 
   the center of the light cone and its walls; it associates with a property 
   value of type \type{RealProp}{Val}. 

   "Attenuation0" is the name of a property that determines the constant 
   attenuation coefficient $C_0$; it associates with a property value of type 
   \type{RealProp}{Val}. 

   "Attenuation1" is the name of a property that determines the linear
   attenuation coefficient $C_1$; it associates with a property value of type 
   \type{RealProp}{Val}. 
*)

PROCEDURE SetOrigin (o : GO.T; origin : Point3.T);
(* The expression "SetOrigin(o,origin)" is equivalent to 
   "o.setProp(Origin.bind(PointProp.NewConst(origin)))". *)

PROCEDURE SetDirection (o : GO.T; dir : Point3.T);
(* "SetDirection" is a convenience procedure. The expression 
   "SetDirection(o,dir)" is equivalent to 
   "o.setProp(Direction.bind(PointProp.NewConst(dir)))". *)

PROCEDURE SetConcentration (o : GO.T; conc : REAL);
(* The expression "SetConcentration(o,conc)" is equivalent to 
   "o.setProp(Concentration.bind(RealProp.NewConst(conc)))". *)

PROCEDURE SetSpreadAngle (o : GO.T; spread : REAL);
(* The expression "SetSpreadAngle(o,spread)" is equivalent to 
   "o.setProp(SpreadAngle.bind(RealProp.NewConst(spread)))". *)

PROCEDURE SetAttenuation0 (o : GO.T; att : REAL);
(* The expression "SetAttenuation1(o,att)" is equivalent to 
   "o.setProp(Attenuation0.bind(RealProp.NewConst(att)))". *)

PROCEDURE SetAttenuation1 (o : GO.T; att : REAL);
(* The expression "SetAttenuation0(o,att)" is equivalent to 
   "o.setProp(Attenuation1.bind(RealProp.NewConst(att)))". *)


END SpotLightGO.
