(* $Id$ *)

INTERFACE Semaphore;

TYPE T <: REFANY;

PROCEDURE New() : T;

PROCEDURE P(t : T);

PROCEDURE V(t : T);

PROCEDURE Value(t : T) : INTEGER;

CONST Brand = "Semaphore";

END Semaphore.
