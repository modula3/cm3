(* $Id: Crash.m3,v 1.1 2011/02/19 20:49:28 mika Exp $ *)

MODULE Crash;

PROCEDURE Me() =
  VAR ptr : REF INTEGER := NIL;
  BEGIN
    ptr^ := 0
  END Me;

BEGIN END Crash.
  
