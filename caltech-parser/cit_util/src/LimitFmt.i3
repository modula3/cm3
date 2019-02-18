(* $Id$ *)
INTERFACE LimitFmt;

PROCEDURE LongReal(lr, maxError : LONGREAL) : TEXT; 
(* format lr with the fewest digits s.t. its max (relative) error is maxError *)

END LimitFmt.


