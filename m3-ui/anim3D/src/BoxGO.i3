(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Created by Marc Najork                                                    *)
(* Last modified on Mon Jun 13 10:05:10 PDT 1994 by najork                   *)


(* A "BoxGO.T" is a geometric object describing a parallelopiped. *)


INTERFACE BoxGO;

IMPORT GO, Point3, PointProp, SurfaceGO;

TYPE
  T <: Public;
  Public = SurfaceGO.T OBJECT
  METHODS
    init () : T;
  END;

(* "box.init()"  initializes a new parallelopiped "box" and returns it.
   The sides of the parallelopiped are axis-aligned, its corners are 
   determined by the "Corner1" and "Corner2" properties. *)

VAR
  Corner1 : PointProp.Name;
  Corner2 : PointProp.Name;

(* In addition to the properties observed by all \type{GO}{T}'s and
   \type{SurfaceGO}{T}'s, there are two additional properties that 
   are observed by "BoxGO.T"'s:

   "Corner1" and "Corner2" are the names of two point properties that 
   describe the two cornerpoints of the box. They associate with 
   \type{PointProp}{Val}s. If they are not specified, (0,0,0) and (1,1,1) 
   are used as cornerpoints. *)

PROCEDURE New (a, b : Point3.T) : T;
(* "New(a,b)" creates a new box and returns it. It also attaches the following
   properties to the new box:
   \begin{verbatim}
     (Corner1,PointProp.NewConst(a))
     (Corner2,PointProp.NewConst(b))
   \end{verbatim}
*)


(* The following two procedures provide sugaring to attach "Corner1" and 
   "Corner2" properties with non-animated property values to geometric 
   objects: *)

PROCEDURE SetCorner1 (o : GO.T; p : Point3.T);
(* The expression "SetCorner1(o,p)" is equivalent to
   "o.setProp(Corner1.bind(PointProp.NewConst(p)))". *)

PROCEDURE SetCorner2 (o : GO.T; p : Point3.T);
(* The expression "SetCorner2(o,p)" is equivalent to
   "o.setProp(Corner2.bind(PointProp.NewConst(p)))". *)

END BoxGO.
