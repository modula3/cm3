(* $Id$ *)

INTERFACE RdWrReset;
IMPORT Rd AS R, Wr AS W;

(* on some systems, Modula-3 readers and writers are limited to 2^31 bytes
   before breaking.  We can solve this---as long as we don't want to be able
   to seek later---by resetting certain counters hidden in them. *)

PROCEDURE Rd(rd : R.T);

PROCEDURE Wr(wr : W.T);

END RdWrReset.
