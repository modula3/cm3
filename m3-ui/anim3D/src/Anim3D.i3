(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Fri Jul 29 09:21:48 PDT 1994 by najork                   *)
(*       Created on Sat May 21 17:41:56 PDT 1994 by najork                   *)


(* This interface provides access to the ``animation clock'', the clock that
   is used to drive animations. Clients can inquire the current time, and can 
   change the clock that is used. The default animation clock is a real-time
   clock; one particular alternative is "ZeusClock.T", a clock that is 
   controlled by the Zeus algorithm animation system. *)

INTERFACE Anim3D;

IMPORT Clock, Wr;

PROCEDURE Now () : LONGREAL;
(* Return the current value of the animation clock. *)

PROCEDURE ChangeClock (clock : Clock.T);
(* Let "clock" be the new animation clock. This procedure should not be 
   called while an animation is in progress; terrible things might happen. *)

PROCEDURE SetErrorWr (wr : Wr.T);
(* Set the writer to which animation server error messages will be written to
   be "wr". By default, error messages are written to "Stdio.stderr". *)

VAR lock : MUTEX;
(* A client that wants to prevent the animation server from rendering an 
   inconsistent scene should protect the critical section during which 
   the scene is inconsistent by acquiring "lock". Not acquiring "lock" 
   will never crash a program. "lock" may not be held when "ah.animate()"
   is called for some animation handle "ah". *)


END Anim3D.
