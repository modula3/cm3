GENERIC INTERFACE FunctionalDeriv2(R,V,M);
(* Arithmetic for Modula-3, see doc for details

   Abstract:

   <describe> *)

IMPORT Arithmetic AS Arith;

(*==========================*)

TYPE
  T = RECORD
        zeroth: R.T;
        first : V.T;
        second: M.T;
      END;

PROCEDURE Add (READONLY x, y: T): T RAISES {Arith.Error};
PROCEDURE Scale (READONLY x: T; y: R.T): T;

(*==========================*)
END FunctionalDeriv2.
