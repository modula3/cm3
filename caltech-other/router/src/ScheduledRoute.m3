(* $Id$ *)

MODULE ScheduledRoute;
IMPORT Integer;

PROCEDURE Compare(READONLY a, b : T) : [-1..1] =
  BEGIN
    RETURN -Integer.Compare(a.nominalCost, b.nominalCost) 
  END Compare;

<*NOWARN*>PROCEDURE Equal(READONLY a, b : T) : BOOLEAN = 
  BEGIN <* ASSERT FALSE *> END Equal;

BEGIN END ScheduledRoute.
