(* $Id$ *)

INTERFACE BDDCleaner;
IMPORT BDD, BDDSystemState;

TYPE
  Cleaner <: PublicCleaner;

  PublicCleaner = OBJECT METHODS
    init() : Cleaner;

    state() : BDDSystemState.SystemState;

    clean(b : BDD.T) : BDD.T;
    (* takes BDD b in another system state and rebuilds it in the cleaner's
       state *)
  END;

END BDDCleaner.
