(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Created by Marc Najork                                                    *)
(* Last modified on Sun Jun 12 15:48:17 PDT 1994 by najork                   *)


(* A "DiskGO.T" is a geometric object describing a disk. *)

INTERFACE DiskGO;

IMPORT GO, Point3, PointProp, RealProp, SurfaceGO;

TYPE
  T <: Public;
  Public = SurfaceGO.T OBJECT
  METHODS
    init (prec := 10) : T;
  END;
(* "disk.init(prec)"  initializes a new disk "disk", whose surface is 
   approximated by a polygon with "prec" edges, and returns it. The location,
   orientation, and radius of the disk is determined by the three properties 
   "Center", "Normal", and "Radius". *)


VAR
  Center : PointProp.Name;
  Normal : PointProp.Name;
  Radius : RealProp.Name;
(* In addition to the properties observed by all \type{GO}{T}'s and
   \type{SurfaceGO}{T}'s, there are three additional properties that 
   are observed by "DiskGO.T"'s:

   "Center" is the name of a property that describes the center  
   of the disk. It associates with a \type{PointProp}{Val}. If no "Center" 
   property is specified, the disk is centered around the origin.

   "Normal" is the name of a property that describes the normal vector 
   of the disk. It associates with a \type{PointProp}{Val}. If no "Normal" 
   property is specified, the normal vector is taken to be (0,0,1).

   "Radius" is the name of a property that describes the radius of the 
   disk. It associates with a \type{RealProp}{Val}. If no "Radius" property
   is specified, the disk has a radius of 1. 

   Assume that in a given context and at a given point in time, the property 
   mapping is such that "Center" maps to a point property value which 
   evaluates to "p", "Normal" maps to a point property value which evaluates 
   to "n", and "Radius" maps to a real property value which evaluates to "r".
   "p", "n", and "r" define the position and shape of the disk 
   in this context and at this time as shown below:
   \begin{center}
   \begin{tabular}{c}
   \psfig{figure=images/DiskGO.ps,silent=}
   \end{tabular}
   \end{center}
*)


PROCEDURE New (p : Point3.T; n : Point3.T; r : REAL; prec := 10) : T;
(* "New(p,n,r,prec)" creates a new disk, whose surface is approximated by a 
   polygon with "prec" edges, and returns it. It also attaches the following 
   properties to the new cone:
    \begin{verbatim}
     (Center,PointProp.NewConst(p))
     (Normal,PointProp.NewConst(n))
     (Radius,RealProp.NewConst(r))
   \end{verbatim}
*)
   

(* The following three procedures provide sugaring to attach 
   "Center", "Normal", and "Radius" properties with non-animated 
   property values to geometric objects: *)

PROCEDURE SetCenter (o : GO.T; p : Point3.T);
(* The expression "SetCenter(o,p)" is equivalent to
   "o.setProp(Center.bind(PointProp.NewConst(p)))". *)

PROCEDURE SetNormal (o : GO.T; p : Point3.T);
(* The expression "SetNormal(o,p)" is equivalent to
   "o.setProp(Normal.bind(PointProp.NewConst(p))))". *)

PROCEDURE SetRadius (o : GO.T; r : REAL);
(* The expression "SetRadius(o,r)"is equivalent to
   "o.setProp(Radius.bind(RealProp.NewConst(r))". *)

END DiskGO.
