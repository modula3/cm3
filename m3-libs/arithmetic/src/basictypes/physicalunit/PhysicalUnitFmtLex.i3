INTERFACE PhysicalUnitFmtLex;
(*Copyright (c) 1996, m3na project

Abstract: Formatter for unit vectors.
          Mainly for debugging purposes.

*)

(*==========================*)

IMPORT PhysicalUnit       AS U;

TYPE
  T = U.T;

PROCEDURE Fmt(unit:T):TEXT;

(*==========================*)
END PhysicalUnitFmtLex.
