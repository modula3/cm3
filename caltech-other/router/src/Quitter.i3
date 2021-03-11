(* $Id$ *)

INTERFACE Quitter;
IMPORT Rd, GridPointSet;

EXCEPTION Error(TEXT);

TYPE
  T <: Public;

  Public = OBJECT METHODS
    read(rd : Rd.T) : T RAISES { Error };

    ripupPoints() : GridPointSet.T;
    endVelocity() : LONGREAL;
    endFailures() : CARDINAL;
  END;

CONST Brand = "Quitter";

END Quitter.
    
