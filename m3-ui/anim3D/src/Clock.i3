(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Created by Marc Najork                                                    *)
(* Last modified on Thu Feb 24 09:43:12 PST 1994 by najork                   *)

(* Animations in the 3D animation library are specified in terms of their
   duration (as opposed to in terms of number of frames etc.). A "Clock.T"
   is an object that represents the clock used by the animation server to
   drive the animation. A "Clock.T" object encapsulates a real-time clock; 
   subtypes of "Clock.T" may encapsulate other kinds of clocks. For instance, 
   a "ZeusClock.T" is a clock whose speed is determined by the animation 
   speed slider in the Zeus control panel. *)

INTERFACE Clock;

TYPE 
  T <: Public;
  Public = OBJECT
  METHODS
    init () : T;
    time () : LONGREAL;
  END;
(* The "init" method initializes a new "Clock.T" object. The "time" method
   returns the current time of this clock. *)

END Clock.
