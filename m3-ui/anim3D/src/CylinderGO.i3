(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Created by Marc Najork                                                    *)
(* Last modified on Thu Jun  9 17:06:11 PDT 1994 by najork                   *)


(* A "CylinderGO.T" is a geometric object describing a cylinder. *)

INTERFACE CylinderGO;

IMPORT GO, Point3, PointProp, RealProp, SurfaceGO;

TYPE
  T <: Public;
  Public = SurfaceGO.T OBJECT
  METHODS
    init (prec := 30) : T;
  END;
(* "cyl.init(prec)"  initializes a new cylinder "cyl", whose
   surface is composed of "prec" rectangles, and returns it.
   The location and radius of the cylinder is determined by the 
   three properties "Point1", "Point2", and "Radius". *)


VAR
  Point1 : PointProp.Name;
  Point2 : PointProp.Name;
  Radius : RealProp.Name;
(* In addition to the properties observed by all \type{GO}{T}'s and
   \type{SurfaceGO}{T}'s, there are three additional properties that 
   are observed by "CylinderGO.T"'s:

   "Point1" is the name of a property that describes the first endpoint 
   of the cylinder. It associates with a \type{PointProp}{Val}. If no "Point1" 
   property is specified, the cylinder starts at the origin.

   "Point2" is the name of a property that describes the second endpoint 
   of the cylinder. It associates with a \type{PointProp}{Val}. If no "Point2" 
   property is specified, the cylinder ends at point (1,0,0).

   "Radius" is the name of a property that describes the radius of the 
   cylinder. It associates with a \type{RealProp}{Val}. If no "Radius" property
   is specified, the cylinder has a radius of 1. 

   Assume that in a given context and at a given point in time, the property 
   mapping is such that "Point1" maps to a point property value which 
   evaluates to "p1", "Point2" maps to a point property value which evaluates 
   to "p2", and "Radius" maps to a real property value which evaluates to "r". 
   "p1", "p2", and "r" define the position and shape of the cylinder 
   in this context and at this time as shown below:
   \begin{center}
   \begin{tabular}{c}
   \psfig{figure=images/CylinderGO.ps,width=3in,silent=}
   \end{tabular}
   \end{center}
*)

PROCEDURE New (p1, p2 : Point3.T; r : REAL; prec := 30) : T;
(* "New(p1,p2,r,prec)"  creates a new cylinder, whose surface is composed 
   of "prec" rectangles, and returns it. It also attaches the following 
   properties to the new cylinder:
   \begin{verbatim}
     (Point1,PointProp.NewConst(p1))
     (Point2,PointProp.NewConst(p2))
     (Radius,RealProp.NewConst(r))
   \end{verbatim}
*)
  
(* The following three procedures provide sugaring to attach 
   "Point1", "Point2", and "Radius" properties with non-animated 
   property values to geometric objects: *)

PROCEDURE SetPoint1 (o : GO.T; p : Point3.T);
(* The expression "SetPoint1(o,p)" is equivalent to
   "o.setProp(Point1.bind(PointProp.NewConst(p)))". *)

PROCEDURE SetPoint2 (o : GO.T; p : Point3.T);
(* The expression "SetPoint2(o,p)" is equivalent to
   "o.setProp(Point2.bind(PointProp.NewConst(p))))". *)

PROCEDURE SetRadius (o : GO.T; r : REAL);
(* The expression "SetRadius(o,r)"is equivalent to
   "o.setProp(Radius.bind(RealProp.NewConst(r))". *)

END CylinderGO.
