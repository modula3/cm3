(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Created by Marc Najork                                                    *)
(* Last modified on Sun Jun 12 15:47:31 PDT 1994 by najork                   *)


(* A "ConeGO.T" is a geometric object describing a cone. *)

INTERFACE ConeGO;

IMPORT GO, Point3, PointProp, RealProp, SurfaceGO;

TYPE
  T <: Public;
  Public = SurfaceGO.T OBJECT
  METHODS
    init (prec := 30) : T;
  END;
(* "cone.init(prec)"  initializes a new cone "cone", whose surface
   is composed of "prec" triangles, and returns it. The location and radius of
   the cone is determined by the three properties "Base", "Tip", and "Radius". 
*)


VAR
  Base   : PointProp.Name;
  Tip    : PointProp.Name;
  Radius : RealProp.Name;
(* In addition to the properties observed by all \type{GO}{T}'s and
   \type{SurfaceGO}{T}'s, there are three additional properties that 
   are observed by "ConeGO.T"'s:

   "Base" is the name of a property that describes the center of the base  
   of the cone. It associates with a \type{PointProp}{Val}. If no "Base" 
   property is specified, the base of the cone lies at the origin.

   "Tip" is the name of a property that describes the tip 
   of the cone. It associates with a \type{PointProp}{Val}. If no "Tip" 
   property is specified, the tip of the cone is at point (1,0,0).

   "Radius" is the name of a property that describes the radius of the 
   cone. It associates with a \type{RealProp}{Val}. If no "Radius" property
   is specified, the cone has a radius of 1.  

   Assume that in a given context and at a given point in time, the property 
   mapping is such that "Base" maps to a point property value which 
   evaluates to "base", "Tip" maps to a point property value which evaluates 
   to "tip", and "Radius" maps to a real property value which evaluates to "r".
   "base", "tip", and "r" define the position and shape of the cone in this c
   ontext and at this time as shown below:
   \begin{center}
   \begin{tabular}{c}
   \psfig{figure=images/ConeGO.ps,silent=}
   \end{tabular}
   \end{center}
*)


PROCEDURE New (base, tip : Point3.T; r : REAL; prec := 30) : T;
(* "New(base,tip,r,prec)" creates a new cone, whose surface is composed of 
   "prec" triangles, and returns it. It also attaches the following properties
   to the new cone:
   \begin{verbatim}
     (Base,PointProp.NewConst(base))
     (Tip,PointProp.NewConst(tip))
     (Radius,RealProp.NewConst(r))
   \end{verbatim}
*)

(* The following three procedures provide sugaring to attach 
   "Base", "Tip", and "Radius" properties with non-animated 
   property values to geometric objects: *)

PROCEDURE SetBase (o : GO.T; p : Point3.T);
(* The expression "SetBase(o,p)" is equivalent to
   "o.setProp(Base.bind(PointProp.NewConst(p)))". *)

PROCEDURE SetTip (o : GO.T; p : Point3.T);
(* The expression "SetTip(o,p)" is equivalent to
   "o.setProp(Tip.bind(PointProp.NewConst(p)))". *)

PROCEDURE SetRadius (o : GO.T; r : REAL);
(* The expression "SetRadius(o,r)"is equivalent to
   "o.setProp(Radius.bind(RealProp.NewConst(r)))". *)

END ConeGO.
