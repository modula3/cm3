GENERIC INTERFACE VectorBasic(R, V);
(*Copyright (c) 1996, m3na project *)
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

PROCEDURE Sum (READONLY x: T): R.T;

PROCEDURE ArithSeq (num: CARDINAL; from: R.T; by: R.T): V.T;
PROCEDURE GeomSeq (num: CARDINAL; from: R.T; by: R.T): V.T;

(*==========================*)
END VectorBasic.
