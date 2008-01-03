(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Created by Marc Najork                                                    *)
(* Last modified on Mon Jun 13 10:12:34 PDT 1994 by najork                   *)


(* A "TorusGO.T" is a geometric object describing a torus. 
   A torus can be described by one major and an infinite set of minor circles.
   
   The major circle of the torus is centerest at point "center", its radius
   is "rad", and its normal vector is "normal". The minor circles (which 
   together make up the surface of the torus) are centered around the perimeter
   of the mayor circle, their radius is "rad2", and their normal vector is 
   parallel to the circumference of the major circle in their center point. 

   The following picture tries to illustrate the roles of the various 
   parameters:
   \begin{center}
   \begin{tabular}{c}
   \psfig{figure=images/TorusGO.ps,width=3in,silent=}
   \end{tabular}
   \end{center} *)

INTERFACE TorusGO;

IMPORT GO, Point3, PointProp, RealProp, SurfaceGO;

TYPE
  T <: Public;
  Public = SurfaceGO.T OBJECT
  METHODS
    init (prec := 30) : T;
  END;
(* "tor.init(prec)" initializes a new torus "tor", composed of "prec" strips 
   of "prec" trapezoids, and returns it. 
   The location, orientation, and size of the torus is determined by the
   "Center", "Normal", "Radius1", and "Radius2" property values. *)

VAR
  Center  : PointProp.Name;
  Normal  : PointProp.Name;
  Radius1 : RealProp.Name;
  Radius2 : RealProp.Name;
(* In addition to the properties observed by all \type{GO}{T}'s and
   \type{SurfaceGO}{T}'s, there are four additional properties that 
   are observed by "TorusGO.T"'s:

   "Center" is the name of a property that describes the center 
   of the torus. It associates with a \type{PointProp}{Val}. If no "Center" 
   property is specified, the center of the torus lies at the origin.

   "Normal" is the name of a property that describes the normal vector of 
   the torus. It associates with a \type{PointProp}{Val}. If no "Normal"
   property is specified, the normal vector is taken to be (0,0,1).
   
   "Radius1" is the name of a property that describes the radius of the 
   major circle of the torus. It associates with a \type{RealProp}{Val}. 
   If no "Radius1" property is specified, the torus has a major radius of 1. 

   "Radius2" is the name of a property that describes the radius of the 
   minor circle of the torus. It associates with a \type{RealProp}{Val}. 
   If no "Radius2" property is specified, the torus has a minor radius 
   of 0.1. *)

PROCEDURE New (center, normal   : Point3.T; 
               radius1, radius2 : REAL; 
               prec := 30) : T;
(* "New(center,normal,radius1, radius2,prec)" creates a new torus,
   whose surface is composed of "prec" strips of "prec" trapezoids,
   and returns it. It also attaches the following properties
   to the new torus:
   \begin{verbatim}
     (Center,PointProp.NewConst(center))
     (Normal,PointProp.NewConst(normal))
     (Radius1,RealProp.NewConst(rad1))
     (Radius2,RealProp.NewConst(rad2))
   \end{verbatim} *)

(* The following three procedures provide sugaring to attach "Center", 
   "Normal", "Radius1", and "Radius2" properties (where the property 
   values have constant behaviors) to geometric objects: *)

PROCEDURE SetCenter (o : GO.T; p : Point3.T);
(* The expression "SetCenter(o,p)" is equivalent to
   "o.setProp(Center.bind(PointProp.NewConst(p)))". *)

PROCEDURE SetNormal (o : GO.T; p : Point3.T);
(* The expression "SetNormal(o,p)" is equivalent to
   "o.setProp(Normal.bind(PointProp.NewConst(p)))". *)

PROCEDURE SetRadius1 (o : GO.T; r : REAL);
(* The expression "SetRadius1(o,r)"is equivalent to
   "o.setProp(Radius1.bind(RealProp.NewConst(r)))". *)

PROCEDURE SetRadius2 (o : GO.T; r : REAL);
(* The expression "SetRadius2(o,r)"is equivalent to
   "o.setProp(Radius2.bind(RealProp.NewConst(r)))". *)

END TorusGO.
