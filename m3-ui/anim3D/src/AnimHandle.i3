(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Sat May 28 20:03:12 PDT 1994 by najork                   *)
(*       Created on Mon Feb 21 15:29:33 PST 1994 by najork                   *)


(* The 3D animation library supports two kinds of animations: synchronous
   and asynchronous ones. 

   An asynchronous animation is performed by attaching an {\em unsynchronized
   time-variant property value} "pv" to a geometric object "go". Attaching 
   "pv" to "go" immediately starts to animate "go" in some fashion, the 
   animation ends once "pv" is again detached. 
   
   A synchronous animation, on the other hand, is performed by attaching a
   {\em synchronized time-variant property value} "pv" to a geometric object
   "go", and then issuing an animation request to "pv". Associated with each 
   synchronous property value is an "AnimHandle.T". However, "pv" will not 
   immediately start to change. 

   The message "ah.animate()" will start to animate all time-varying property 
   values tied to "ah". The call to "ah.animate()" will return only after 
   these animations are completed. 

   Animation handles are monitored: only one thread can call "animate" at any
   given time, and no thread can insert requests into the request queue of a 
   synchronous property value while "animate" is in progress.
*)

INTERFACE AnimHandle;

IMPORT ProxiedObj;

TYPE 
  T <: Public;
  Public = ProxiedObj.T OBJECT
  METHODS
    init () : T;
    animate ();
  END;
(* "ah.init()" initializes a new animation handle "ah" and returns it. 
   "ah.animate()" triggers the animation of all synchronous property values
   that are tied to "ah". It returns when the animation is completed. *)

PROCEDURE New () : T;
(* "New" is a convenience procedure. The expression "New()" is equivalent 
   to "NEW(T).init()" *)

END AnimHandle.
