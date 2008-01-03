(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Created by Marc Najork                                                    *)
(* Last modified on Sun Jun 12 17:21:04 PDT 1994 by najork                   *)


(* A "SphereGO.T" is a geometric object describing a sphere. *)

INTERFACE SphereGO;

IMPORT GO, Point3, PointProp, RealProp, SurfaceGO;

TYPE
  T <: Public;
  Public = SurfaceGO.T OBJECT
  METHODS
    init (prec := 30) : T;
  END;
(* "sph.init(prec)" initializes a new sphere "sph", whose surface is composed
   of "prec" strips of "prec" triangles,  and returns it. The location and 
   radius of the sphere are determined by the two properties "Center" and 
   "Radius". *)

VAR
  Center : PointProp.Name;
  Radius : RealProp.Name;

(* In addition to the properties observed by all \type{GO}{T}'s and
   \type{SurfaceGO}{T}'s, there are two additional properties that 
   are observed by "SphereGO.T"'s:

   "Center" is the name of a property that describes the center of the sphere.
   It associates with a \type{PointProp}{Val}. If no "Center" property
   is specified, the sphere is centered at the origin.

   "Radius" is the name of a property that describes the radius of the sphere.
   It associates with a \type{RealProp}{Val}. If no "Radius" property
   is specified, the sphere has a radius of 1. *)


PROCEDURE New (center : Point3.T; radius : REAL; prec := 30) : T;
(* "New(center,radius,prec)" creates a new sphere, whose surface is composed of
   "prec" strips of "prec" triangles, and returns it. It also attaches the 
   following properties to the new sphere:
   \begin{verbatim}
     (Center,PointProp.NewConst(center))
     (Radius,RealProp.NewConst(radius))
   \end{verbatim}
*)

(* The following two procedures provide sugaring to attach 
   "Center" and "Radius" properties with non-animated 
   property values to geometric objects: *)

PROCEDURE SetCenter (o : GO.T; center : Point3.T);
(* The expression "SetCenter(o,p)" is equivalent to
   "o.setProp(Center.bind(PointProp.NewConst(p))". *)

PROCEDURE SetRadius (o : GO.T; radius : REAL);
(* The expression "SetRadius(o,r)"is equivalent to
   "o.setProp(Radius.bind(RealProp.NewConst(r)))". *)

END SphereGO.
