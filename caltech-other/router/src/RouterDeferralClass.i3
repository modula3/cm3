(* $Id$ *)

INTERFACE RouterDeferralClass;
IMPORT RouterDeferral, GridPointSet;

REVEAL RouterDeferral.T <: Private;

TYPE
  Private = RouterDeferral.Public OBJECT
    gridPoints : GridPointSet.T := NIL;
    (* gridpoints for dummy targets (for internal use) *)
  END;

END RouterDeferralClass.
