(* $Id$ *)

INTERFACE UCTime;
IMPORT Time;

(* wrappers for Utime C stuff *)

PROCEDURE ctime(clock : Time.T;
                keepNL := TRUE;
                showTZ := FALSE) : TEXT;
  (* like the normal Unix ctime, but with some options... *)

CONST Brand = "UTime";

END UCTime.
