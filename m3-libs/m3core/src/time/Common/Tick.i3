(* Copyright (C) 1989, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Thu Dec  9 09:55:26 PST 1993 by mcjones *)

(* A "Tick.T" represents a value of a clock with subsecond resolution.
   The exact resolution differs from implementation to implementation
   and is typically one sixtieth of a second or smaller.
   \index{time!elapsed}
   \index{elapsed time}
   \index{time!Tick interface@{\tt Tick} interface}
*)

INTERFACE Tick;

IMPORT Word;

TYPE T = Word.T;

PROCEDURE Now(): T;
(* Return the current reading of the tick clock. *)

PROCEDURE ToSeconds(t: Word.T): LONGREAL;
(* Return the number of seconds in "t" ticks. *)

EXCEPTION Overflow;

PROCEDURE FromSeconds(s: LONGREAL): Word.T RAISES {Overflow};
(* Return the number of ticks equivalent to "s" seconds, rounded to
   the nearest whole number, or raise "Overflow" if "s" is negative or
   the result would not be less than "2^Word.Size".  *)

END Tick.

(* If "t0" is a reading of the tick clock and "t1" is another reading
   taken less than $\hbox"2"^{\hbox"Word.Size"}$ ticks after "t0",
   then the number of ticks between "t0" and "t1" is "Word.Minus(t1,
   t0)".

   The values returned by "Tick.Now()" and "Time.Now()" typically
   won't stay synchronized for long periods of time.  The purpose of
   "Tick.Now()" is to provide accurate measurements of short
   intervals.  The purpose of "Time.Now()" is to provide ``wall
   clock'' time, preferably synchronized with UTC (coordinated
   universal time).

*)
