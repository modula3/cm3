GENERIC INTERFACE VectorBasic(R, V);
(*Arithmetic for Modula-3, see doc for details *)

(*==========================*)

TYPE T = V.TBody;

PROCEDURE IsZero (READONLY x: T): BOOLEAN;
PROCEDURE Equal (READONLY x, y: T): BOOLEAN ;

PROCEDURE Add (READONLY x, y: T): V.T ;
PROCEDURE Sub (READONLY x, y: T): V.T ;
PROCEDURE Neg (READONLY x: T): V.T;

PROCEDURE Scale (READONLY x: T; y: R.T): V.T;
PROCEDURE Inner (READONLY x, y: T): R.T ; (*adjoint x*)
PROCEDURE Dot(READONLY x, y: T): R.T ;

PROCEDURE Sum (READONLY x: T): R.T;

PROCEDURE ArithSeq (num: CARDINAL; from: R.T; by: R.T): V.T;
PROCEDURE GeomSeq (num: CARDINAL; from: R.T; by: R.T): V.T;

(*==========================*)
END VectorBasic.
