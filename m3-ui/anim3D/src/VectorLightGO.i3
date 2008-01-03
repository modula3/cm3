(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Thu Jun 23 14:39:34 PDT 1994 by najork                   *)
(*       Created on Fri Feb  4 15:23:40 PST 1994 by najork                   *)


(* A "VectorLightGO.T" is a geometric object that is emitting directed light 
   and is infinitely far away from all other scene objects, so that all light 
   rays falling onto the scene are parallel to each other. 

   The common real-world example of a vector light source is the sun 
   (the Earth-Sun distance being so large that all sun rays falling onto 
   earth are close to parallel). *)

INTERFACE VectorLightGO;

IMPORT Color, GO, LightGO, PointProp, Point3;

TYPE
  T <: Public;
  Public = LightGO.T OBJECT
  METHODS
    init () : T;
  END;
(* "l.init()" initializes a new vector light source "l" and returns it. *)


PROCEDURE New (c : Color.T; dir : Point3.T) : T;
(* "New(c,dir)" creates a new vector light source "l" and returns it.
   It also attaches attaches the following properties to "l":
   \begin{verbatim}
     (LightGO.Colour,ColourProp.NewConst(c)) 
     (LightGO.Switch,BooleanProp.NewConst(TRUE))
     (Direction,PointProp.NewConst(dir))
   \end{verbatim}
*)

VAR
  Direction : PointProp.Name;
(* In addition to the properties observed by all \type{GO}{T}'s and 
   \type{LightGO}{T}'s, there is one additional property that is observed 
   by "VectorLightGO.T"'s. "Direction" is the name of a property that 
   determines the direction of the light emitted by the light sources; 
   it associates with a property value of type \type{PointProp}{Val}. *)

PROCEDURE SetDirection (o : GO.T; dir : Point3.T);
(* "SetDirection" is a convenience procedure. The expression 
   "SetDirection(o,dir)" is equivalent to 
   "o.setProp(Direction.bind(PointProp.NewConst(dir)))". *)

END VectorLightGO.
