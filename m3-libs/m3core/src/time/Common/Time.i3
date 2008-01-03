(* Copyright (C) 1989, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Thu Dec  9 09:55:13 PST 1993 by mcjones *)
(*      Modified on Thu Oct 11 22:04:09 1990 by muller *)
(* Based on the Modula-2+ Time.def, designed by Roy Levin. *)

(* A "Time.T" represents a moment in time, reckoned as a number of
   seconds since some epoch or starting point.
   \index{time!of day}
   \index{time!Time interface@{\tt Time} interface}
*)

INTERFACE Time;

TYPE T = LONGREAL;

PROCEDURE Now(): T;
(* Return the current moment in time. *)

VAR (*CONST*) Grain: LONGREAL;
(* If a thread performs "t0 := Time.Now(); t1 := Time.Now()", then
   either "t1 = t0" or "t1 >= t0 + Time.Grain".  "Grain" always lies
   in the half-open interval "(0..1]" and is usually no larger than
   one sixtieth of a second.  *)

END Time.

(* There are a variety of timekeeping needs, and "Time.Now" may not
   satisfy all of them.  It is intended to be useful for recording times
   and measuring intervals arising during the execution of computer
   programs with a resolution comparable to human reaction times.

   The epoch for a "Time.T" varies from one operating system to
   another.  To determine the epoch, call "Date.FromTime(0.0D0,
   Date.UTC)".  Note that communicating a "Time.T" between systems,
   say via remote procedure call or pickles, is likely to be a bad
   idea.
   \index{epoch}

   In many computers, "Time.Now" is implemented with the technology 
   of an inexpensive wristwatch, and is therefore likely to suffer 
   from similar errors: the rate may vary, and the value may be changed 
   by a human operator. 

   The "Thread" interface contains procedures that delay the execution
   of the calling thread for a specified duration.  The "Tick"
   interface provides access to a clock with subsecond resolution.

*)
