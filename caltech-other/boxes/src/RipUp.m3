(* $Id$ *)

MODULE RipUp;
IMPORT Debug;
IMPORT RouteState;
FROM Cost IMPORT MinRipUpCost, RipUpIncrement, Infty;
IMPORT Route;
IMPORT RouteID;

PROCEDURE Cost(killerRoute : Route.T; 
               deadRoute : RouteID.T; 
               state : RouteState.T) : INTEGER =
  VAR
    ripUpCost : CARDINAL; 
  BEGIN
    (* if we don't know what we're ripping up, we can't do it: 
       cost to rip up is infinite. *)
    IF deadRoute = RouteID.Nil THEN RETURN Infty END;

    VAR
      ripUpCostReal : REAL;
    BEGIN
      ripUpCostReal := FLOAT(MinRipUpCost) + 
                           FLOAT(killerRoute.ripUpCost(deadRoute)) * 
                           FLOAT(RipUpIncrement);

      IF ripUpCostReal * 2.0 > FLOAT(Infty) THEN 
        ripUpCost := Infty
      ELSE
        ripUpCost := ROUND(ripUpCostReal)
      END
    END;
    IF state # NIL AND state.rippedUpAnywhere(deadRoute) THEN
      Debug.Out("Checking cost to rip up already ripped-up path.");

      RETURN 0 (* is this right? *)
    ELSE
      RETURN ripUpCost (* Hmmm.. *)
    END
  END Cost;

BEGIN END RipUp.
