GENERIC INTERFACE VectorSupport(R);
(*Arithmetic for Modula-3, see doc for details

   Abstract: core routines for generic vector math

   The routines are utilized for many similar data types.

   *)

(*==========================*)

TYPE T = ARRAY OF R.T;

PROCEDURE Clear (VAR (*OUT*) z: T);
PROCEDURE Add (VAR (*OUT*) z: T; READONLY x, y: T);
PROCEDURE Sub (VAR (*OUT*) z: T; READONLY x, y: T);
PROCEDURE Neg (VAR (*OUT*) z: T; READONLY x: T);
PROCEDURE Scale (VAR (*OUT*) z: T; READONLY x: T; y: R.T);
PROCEDURE Inner (READONLY (*OUT*) x, y: T): R.T;

PROCEDURE Sum (READONLY x: T): R.T;

(*==========================*)
END VectorSupport.
