(* $Id$ *)

INTERFACE SXTime;
IMPORT Time, SXLongReal, SXInt;

CONST CurrentOffset = FIRST(LONGREAL);

PROCEDURE New(interval : Time.T; offset := 0.0d0) : SXLongReal.T;
(* start a thread that, every interval seconds, updates the output
   with the current time;

   pass in CurrentOffset to use the offset of the first call.
*)

PROCEDURE NewCounter(interval : Time.T; offset := 0.0d0) : SXInt.T;
(* as above, but counts in integers *)

(**********************************************************************)

(*
   The following two are utility functions that 
   don't really belong in this interface! 
 *)

PROCEDURE Next(interval : Time.T; offset : LONGREAL := 0.0d0) : Time.T;
  (* what is the next time (from now) at a given interval and offset
     (since the epoch) *)

PROCEDURE NextFrom(from, interval : Time.T;  offset : LONGREAL := 0.0d0) : Time.T;
  (* what is the next time (from a given time) at a given interval and offset
     (since the epoch) *)

END SXTime.
