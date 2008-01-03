INTERFACE PhysicalUnit;
(* Arithmetic for Modula-3, see doc for details

   Abstract: Computations with vectors of exponents of physical units.

   It will most commonly be used in connection with some type of numerical
   values as provided by PhysicalValue *)


IMPORT IntIntTbl AS Tbl, RealBasic AS R;

FROM Arithmetic IMPORT Error;

TYPE
  T = Tbl.Default;
  ExpType = INTEGER;

PROCEDURE New (): T;
PROCEDURE FromArray (READONLY x: ARRAY OF ExpType; ): T;
PROCEDURE Copy (x: T; ): T;

PROCEDURE Equal (x, y: T; ): BOOLEAN;
PROCEDURE IsZero (x: T; ): BOOLEAN;

PROCEDURE Add (x, y: T; ): T;
PROCEDURE Sub (x, y: T; ): T;
PROCEDURE Neg (x: T; ): T;

PROCEDURE Scale (x: T; y: ExpType; ): T;
PROCEDURE ScaleDiv (x: T; y: ExpType; ): T RAISES {Error};
PROCEDURE ScaleReal (x: T; y: R.T; ): T RAISES {Error};

PROCEDURE Norm1 (x: T; ): ExpType;
PROCEDURE NormInf (x: T; ): ExpType;

END PhysicalUnit.
