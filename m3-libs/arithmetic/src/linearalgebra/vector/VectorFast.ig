GENERIC INTERFACE VectorFast(R, V);
(*Arithmetic for Modula-3, see doc for details *)
FROM NADefinitions IMPORT Error;
(*==========================*)

TYPE T = V.TBody;

PROCEDURE IsZero (READONLY x: T): BOOLEAN;
PROCEDURE Equal (READONLY x, y: T): BOOLEAN RAISES {Error};

PROCEDURE Add (READONLY x, y: T): V.T RAISES {Error};
PROCEDURE Sub (READONLY x, y: T): V.T RAISES {Error};
PROCEDURE Neg (READONLY x: T): V.T;

PROCEDURE Scale (READONLY x: T; y: R.T): V.T;
PROCEDURE Inner (READONLY x, y: T): R.T RAISES {Error};
CONST Dot = Inner;

PROCEDURE Sum (READONLY x: T): R.T;
PROCEDURE Max (READONLY x: T): R.T;
PROCEDURE Min (READONLY x: T): R.T;

PROCEDURE ArithSeq (num: CARDINAL; from := R.Zero; by := R.One): V.T;
PROCEDURE GeomSeq (num: CARDINAL; from := R.One; by := R.Two): V.T;

(*==========================*)
END VectorFast.
