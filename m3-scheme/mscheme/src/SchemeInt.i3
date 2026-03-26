(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

INTERFACE SchemeInt;
IMPORT SchemeObject, Mpz;

TYPE T = REF INTEGER;

PROCEDURE FromI(x: INTEGER): SchemeObject.T;
  (* Return a fixnum; uses cache for [-256..256] *)

PROCEDURE IsInt(x: SchemeObject.T): BOOLEAN;
  (* TRUE iff x is REF INTEGER *)

PROCEDURE IsExactInt(x: SchemeObject.T): BOOLEAN;
  (* TRUE iff REF INTEGER or Mpz.T *)

PROCEDURE IsNumber(x: SchemeObject.T): BOOLEAN;
  (* TRUE for REF INTEGER, Mpz.T, BigInt.T, or SchemeLongReal.T *)

PROCEDURE ToInteger(x: SchemeObject.T): INTEGER;
  (* Extract INTEGER from REF INTEGER or small Mpz.T.
     Checked runtime error if not an exact integer or out of range. *)

PROCEDURE Add(a, b: INTEGER): SchemeObject.T;
  (* Overflow-safe addition; promotes to Mpz.T on overflow *)

PROCEDURE Sub(a, b: INTEGER): SchemeObject.T;
  (* Overflow-safe subtraction; promotes to Mpz.T on overflow *)

PROCEDURE Mul(a, b: INTEGER): SchemeObject.T;
  (* Overflow-safe multiplication; promotes to Mpz.T on overflow *)

PROCEDURE ToMpz(x: SchemeObject.T): Mpz.T;
  (* Convert REF INTEGER or Mpz.T to Mpz.T *)

PROCEDURE MpzToScheme(m: Mpz.T): SchemeObject.T;
  (* Demote Mpz.T to REF INTEGER if it fits, else return Mpz.T *)

PROCEDURE IsZero(x: SchemeObject.T): BOOLEAN;
  (* TRUE iff exact integer equal to zero *)

PROCEDURE IsPositive(x: SchemeObject.T): BOOLEAN;
  (* TRUE iff exact integer > 0 *)

PROCEDURE IsNegative(x: SchemeObject.T): BOOLEAN;
  (* TRUE iff exact integer < 0 *)

PROCEDURE Compare(a, b: SchemeObject.T): INTEGER;
  (* Compare two exact integers. Returns -1, 0, or 1 *)

VAR (* CONST, set in module body *)
  Zero, One, NegOne, Two : SchemeObject.T;

END SchemeInt.
