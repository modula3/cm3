(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Created by Marc Najork                                                    *)
(* Last modified on Sat Jul 15 00:05:33 PDT 1995 by najork                   *)
<* PRAGMA LL *>

(* Geometric objects are one of two main concepts in the 3D animation 
   libraries (properties being the other one). A geometric object is an
   object in the scene we are viewing: a visible object such a line, 
   a sphere, or a torus, an invisible object such as a light source 
   (lights affect other objects, but are themselves invisible) or a 
   camera, or a group of other geometric objects.

   "GO.T" is the abstract class of geometric objects. *)

INTERFACE GO;

IMPORT KeyCB, MouseCB, Prop, ProxiedObj, PositionCB, TransformProp;

EXCEPTION PropUndefined;
EXCEPTION StackError;

TYPE
  Shape = {Complex, NonConvex, Convex, Unknown};
(* "Shape" is used to provide shape hints to some type geometric objects 
   that are composed of polygons (right now, \type{PolygonGO}{T} and 
   \type{QuadMeshGO}{T}).
   "Convex" indicates that the object is composed of convex polygons,
   "NonConvex" indicates that the object is composed on non-convex polygons 
   that have non-intersecting border lines, "Convex" indicates that the 
   object is composed on non-convex polygons that may have intersecting 
   border lines, "Unknown" indicates that the shape of the object is unknown.
*)

TYPE
  T <: Public;
  Public = ProxiedObj.T OBJECT
  METHODS
    init () : T;

    setProp (p : Prop.T);
    unsetProp (pn : Prop.Name) RAISES {PropUndefined};
    getProp (pn : Prop.Name) : Prop.Val RAISES {PropUndefined};

    setName (name : TEXT);
    getName () : TEXT;
    findName (name : TEXT) : T;        <* LL = "all roots of self" *>

    pushMouseCB (cb : MouseCB.T);
    popMouseCB () RAISES {StackError};
    removeMouseCB (cb : MouseCB.T) RAISES {StackError};
    invokeMouseCB (mr : MouseCB.Rec);

    pushPositionCB (cb : PositionCB.T);
    popPositionCB () RAISES {StackError};
    removePositionCB (cb : PositionCB.T) RAISES {StackError};
    invokePositionCB (pr : PositionCB.Rec);

    pushKeyCB (cb : KeyCB.T);
    popKeyCB () RAISES {StackError};
    removeKeyCB (cb : KeyCB.T) RAISES {StackError};
    invokeKeyCB (kr : KeyCB.Rec);
  END;

(* Associated with each geometric object "o" is a property mapping $M_o$.
   A property mapping is a partial function that maps property names to 
   property values.
   We say ``"pn" is associated with "pv"'' if $M_o$("pn") = "pv".
   We also say that ("pn","pv") is attached to the geometric object.

   Name/Value associations in the property mapping are guaranteed to be
   compatible, i.e. a properity name of type \type{PointProp}{Name} will be 
   associated with a value of type \type{PointProp}{Val}. 

   "o.setProp(p)" attaches the property "p" to "o". 

   If $M_o$("pn") = "pv", then "o.unsetProp(pn)" detaches 
   ("pn","pv") from "o", that is, $M_o$("pn") will be undefined afterwards.
   If $M_o$("pn") is undefined, then "o.unsetProp(pn)" raises the
   exception "PropUndefined".

   If $M_o$("pn") = "pv", then "o.getProp(pn)" returns "pv".
   If $M_o$("pn") is undefined, then "o.getProp(pn)" raises "PropUndefined". 

   Geometric objects can have names, i.e.\ strings that identify them. Names 
   provide a convenient way to find a geometric object within a scene DAG. 
   No two GOs with a common ancestor should have the same name; however,
   this is not enforced. 

   When a geometric object is created, no name is associated with it. 
   "o.setName(name)" associates the name "name" with the object "o".
   "o.getName()" returns "o"'s name ("NIL" if no name is associated with "o").
   "o.findName(name)" returns a descendent of "o" with name "name" if there
   is one, "NIL" otherwise.

   Geometric objects are reactive. A {\em callback object} is an object 
   that responds to a particular kind of events. Currently, there are four 
   types of callback objects: \type{MouseCB}{T} (objects that handle mouse 
   button presses and releases), \type{PositionCB}{T} (objects that handle 
   mouse movements), and \type{KeyCB}{T} (objects that handle key presses and 
   releases). 

   Associated with each geometric object are four {\em callback object stacks},
   one for each type of callback object. When a mouse/position/key/click
   event is delivered to "o", the top callback object on the corresponding 
   stack is invoked to handle it. If there is no such object, the event is 
   dropped.

   Having stacks of callback objects rather than single callback objects
   makes it easier to temporally change the behavior of a geometric object 
   and later on to reestablish its old behavior (simply by popping the stack).

   "o.pushMouseCB(cb)" pushes a mouse callback object "cb" onto "o's"
   mouse callback stack. 

   "o.popMouseCB()" removes the top callback object from "o"'s mouse
   callback stack. If the stack is empty, "StackError" is raised.

   "o.removeMouseCB(cb)" removes the callback object "cb" from "o"'s mouse
   callback stack. If "cb" is not in the stack, "StackError" is raised.

   "o.invokeMouseCB(mr)" invokes the top callback object on "o"'s mouse
   callback stack with argument "mr".

   The remaining methods perform analogous tasks for position, and key 
   callbacks.

   {\em The event handling model is largely untested. It is probably
   the least stable part of the interface. Expect changes to "...CB.T",
   "...CB.Rec", and the "invoke...CB" methods.} *)

VAR
  Transform : TransformProp.Name;
(* "Transform" is the name of the transformation property, a property that
   applies to all geometric objects. *)

PROCEDURE GetTransform (o : T) : TransformProp.Val RAISES {PropUndefined};
(* "GetTransform" is a convenience procedure for looking up the transformation 
   property value of a geometric object. The expression "GetTransform (o)" is 
   equivalent to "NARROW (o.getProp (Transform), TransformProp.Val)". *)

END GO.
