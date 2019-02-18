(* $Id$ *)

INTERFACE CITRandom;
IMPORT Random;

TYPE
  T <: Public;

  Public = Random.T OBJECT METHODS

    (* for a fixed generator, call with init(TRUE, SEED) *)
    (* call with init(FALSE) for a random-random generator *)
    (* the SEED should have been returned by an earlier call
       to seed() *)
    (* it is furthermore a checked runtime error to call
       init(TRUE, 0) *)
    init(fixed : BOOLEAN; seed : INTEGER := 0) : T;
    seed() : INTEGER;
  END;

CONST Brand = "CITRandom";

END CITRandom.
