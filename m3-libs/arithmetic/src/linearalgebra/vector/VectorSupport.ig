GENERIC INTERFACE VectorSupport(R);
(*Copyright (c) 1996, m3na project

   Abstract: core routines for generic vector math

   The routines are utilized for many similar data types.

   *)
FROM NADefinitions IMPORT Error;
(*==========================*)

TYPE T = ARRAY OF R.T;

PROCEDURE Clear (VAR (*OUT*) z: T);
PROCEDURE Add (VAR (*OUT*) z: T; READONLY x, y: T) RAISES {Error};
PROCEDURE Sub (VAR (*OUT*) z: T; READONLY x, y: T) RAISES {Error};
PROCEDURE Neg (VAR (*OUT*) z: T; READONLY x: T) RAISES {Error};
PROCEDURE Scale (VAR (*OUT*) z: T; READONLY x: T; y: R.T) RAISES {Error};
PROCEDURE Inner (READONLY (*OUT*) x, y: T): R.T RAISES {Error};

PROCEDURE Apply(READONLY x:T;f:ApplyFtn);
PROCEDURE Map(VAR z:T;READONLY x:T;f:MapFtn) RAISES {Error};
PROCEDURE Reduce(READONLY x:T;f:ReduceFtn;init:R.T):R.T;

PROCEDURE Sum(READONLY x:T):R.T;

(*==========================*)
END VectorSupport.
