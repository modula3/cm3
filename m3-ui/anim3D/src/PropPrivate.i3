(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Jan 30 22:14:23 PST 1995 by najork                   *)
(*       Created on Thu May 19 14:05:27 PDT 1994 by najork                   *)


INTERFACE PropPrivate;

IMPORT GO, GraphicsBase, ProxiedObj;

FROM Prop IMPORT T, Name, Val, BadMethod;

REVEAL T <: PrivateT;

TYPE 
  PrivateT = ProxiedObj.T OBJECT
    n : Name;
    v : Val;
  METHODS
    init (n : Name; v : Val) : T;
    adjust (time : LONGREAL; caller : GO.T);
  END;

(* "p.adjust(time)" reevaluates the value of the property based on the given
   time. If the property value got damaged since the last adjustment, the
   property name is called upon to perform the appropriate damages to the 
   caller and/or its descendants. *)

REVEAL Name <: PrivateName;

TYPE 
  PrivateName = ProxiedObj.T OBJECT
    id : INTEGER;
  METHODS
    init () : Name;
    damage (caller : GO.T);
    push (base : GraphicsBase.T; val : Val);
    pop (base : GraphicsBase.T);
    newStack () : Stack;
  END;

(* "pn.damage(caller)" indicates that a property value bound to "pn" has
   changed since the last adjustment, which might cause damages in the scene.
   The default definition of "damage" asks all descendents of "caller"
   to damage themselves if their appearance depends on "pn", and propagates
   damages back up to "caller".
   Subclasses of "Name" can override the "damage" method to perform the
   appropriate damages to "caller" and/or its descendants. *)

REVEAL 
  Val <: PrivateVal;

TYPE
  PrivateVal = ProxiedObj.T OBJECT
    time    : LONGREAL;    (* Subclasses cache value at time "time".         *)
    damaged : BOOLEAN;     (* Did the value change since the last rendering? *)
  METHODS
    adjust (time : LONGREAL) : BOOLEAN 
        RAISES {BadMethod};
  END;

(* "pv.adjust(now)" does two things: it updates the cached information 
   (time, value, and damage since last rendering cycle), and it returns
   if the property value is damaged, i.e. if it has changed since the last 
   rendering. Note that this technique relied on the fact that there is 
   a single, system-wide animation server, and that all roots get redrawn 
   at the same time. *)


TYPE 
  Stack <: ROOT;


PROCEDURE NewStacks () : REF ARRAY OF Stack;
(* "NewStacks()" returns a new array of property value stacks, one stack
   for each existing property name. *)


END PropPrivate.
