(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Wed May 25 17:24:35 PDT 1994 by najork                   *)
(*       Created on Fri Feb  4 15:19:54 PST 1994 by najork                   *)

(* Lights are treated as a special kind of geometric objects. A light,
   like any other geometric object, can be a part of a group (and thus,
   by transitivity, of an entire scene), properties can be attached to
   it, and it observes both the properties attached to itself and to its
   ancestors in the scene. In particular, lights are affected by
   transformation properties: When the transformation property of a light
   or of one of its ancestors in the scene changes, the light source
   moves.

   Light sources affect all geometric objects of type \type{SurfaceGO}{T} 
   in the scene, by illuminating their surfaces. However, they are invisible 
   themselves. 

   "LightGO.T" is the abstract class of all light sources. *)


INTERFACE LightGO;

IMPORT BooleanProp, ColorProp, Color, GO;

TYPE
  T <: GO.T;
(* "LightGO.T" is a subtype of \type{GO}{T}. No additional fiels or methods 
   are defined. *)

VAR
  Colour : ColorProp.Name;
  Switch : BooleanProp.Name;
(* In addition to the properties observed by all \type{GO}{T}'s, there are two
   additional properties that are observed by all "LightGO.T"'s. 
   "Colour" is the name of a property that determines the colour of the light 
   emitted by the "LightGO.T"; it associates with a property value of type 
   \type{ColourProp}{Val}.
   "Switch" is the name of a property that determines if the light source 
   is on or off; it associates with a property value of type 
   \type{BooleanProp}{Val}. *)

(* The following two procedures make it more convenient to modify the
   "Colour" and "Switch" properties of a light: *)

PROCEDURE SetColour (o : GO.T; c : Color.T);
(* The expression "SetColour(o,c)" is equivalent to 
   "o.setProp(Colour.bind(ColorProp.NewConst(c)))". *)

PROCEDURE SetSwitch (o : GO.T; switch : BOOLEAN);
(* The expression "SetSwitch(o,b)" is equivalent to 
   "o.setProp(Switch.bind(BooleanProp.NewConst(b)))". *)

END LightGO.
