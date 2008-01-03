(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Created by Marc Najork                                                    *)
(* Last modified on Thu Aug  4 10:39:24 PDT 1994 by najork                   *)


(* A "PolygonGO.T" is a geometric object that describes a polygon. *)

INTERFACE PolygonGO;

IMPORT GO, Point3, PointProp, SurfaceGO;

TYPE T <: SurfaceGO.T;

PROCEDURE NewStatic (READONLY pts : ARRAY OF Point3.T; 
                     s := GO.Shape.Unknown) : T;
(* Creates a new polygon "pg" and returns it. "pg" is defined by the array of 
   points "pts". "s" is a ``shape hint'', i.e.\ a hint whether the polygon is 
   convex, non-convex, or complex. Refer to the \interface{GO} interface 
   for details on shape hints. *)

PROCEDURE New (READONLY pts : ARRAY OF PointProp.Val; 
               s := GO.Shape.Unknown) : T;
(* Creates a new polygon "pg" and returns it. The point property values in 
   "pts" are used as the corner points of the polygon. The optional argument 
   "s" indicates the shape of the polygon. *)

(* {\sl 
     Note: Things would be even more general if we could dynamically add 
     points to a polygon or remove them. One idea would be to introduce 
     a new property value "PointSequenceProp.Val", which contains an array 
     of "PointProp.Val"'s. "PointSequence" should be non-animatable, but 
     provide methods to access its elements. For "QuadMeshGO.T"'s, there 
     should be similar types "PointField" and "ColorField".
   } *)

END PolygonGO.
