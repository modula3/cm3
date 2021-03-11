(* $Id$ *)

INTERFACE CostRec;
IMPORT GridPoint;

(*
TYPE
  T = RECORD 
    loc : Loc.T;              (* keep track of "session" and current cost *)
    previous : GridPoint.T;   (* back pointer so we can backtrack         *)
                              (* XXX no cycles allowed *)
    searched := FALSE;
  END;
*)

TYPE
  T = RECORD
    previous : GridPoint.T;
    minCost : CARDINAL;
  END;

CONST
  Brand = "CostRec";

END CostRec.
