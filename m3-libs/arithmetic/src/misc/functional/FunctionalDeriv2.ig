GENERIC INTERFACE FunctionalDeriv2(R,V,M);
(* Copyright (c) 1996, m3na project

   Abstract:

   <describe> *)

IMPORT NADefinitions AS NA;

(*==========================*)

TYPE
  T = RECORD
        zeroth: R.T;
        first : V.T;
        second: M.T;
      END;

PROCEDURE Add (READONLY x, y: T): T RAISES {NA.Error};
PROCEDURE Scale (READONLY x: T; y: R.T): T;

(*==========================*)
END FunctionalDeriv2.
