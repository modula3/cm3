(* $Id$ *)

INTERFACE BDDSystemState;

TYPE SystemState <: REFANY;

PROCEDURE GetSystemState() : SystemState;

PROCEDURE SetSystemState(state : SystemState);

PROCEDURE NewDefaultSystemState() : SystemState;

END BDDSystemState.
