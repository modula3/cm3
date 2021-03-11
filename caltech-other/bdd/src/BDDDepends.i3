INTERFACE BDDDepends;
IMPORT BDD, BDDSet;

PROCEDURE Depends(b : BDD.T) : BDDSet.T;
  (* returns literals that expression b depends on *)

END BDDDepends.
