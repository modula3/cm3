(* Copyright (C) 2016 Peter McKinna. All rights reserved. *)
(* and Jay Krell jay.krell@cornell.edu                    *)
(* See file COPYRIGHT-BSD for details. *)

INTERFACE IEEE;

FROM SimFP IMPORT Uint64,Uint32,GFP;

TYPE
  
  Double <: DoublePublic;
  
  DoublePublic = OBJECT
  METHODS
    init(v : LONGREAL) : Double;
    value() : LONGREAL;    
    asGFP() : GFP;
    asNormalizedGFP() : GFP;
    exponent() : INTEGER;
    significand() : Uint64;
    isDenormal() : BOOLEAN;
    isSpecial() : BOOLEAN;
    isNan() : BOOLEAN;
    isInfinite() : BOOLEAN;
    sign() : INTEGER;
    infinity() : LONGREAL;
    nan() : LONGREAL;
    normalizedBoundaries(VAR outMminus,outMplus : GFP);
    lowerBoundaryIsCloser() : BOOLEAN;
  END;

  Single <: SinglePublic;
  
  SinglePublic = OBJECT
  METHODS
    init(v : REAL) : Single;
    value() : REAL;    
    asGFP() : GFP;
    exponent() : INTEGER;
    significand() : Uint32;
    isDenormal() : BOOLEAN;
    isSpecial() : BOOLEAN;
    isNan() : BOOLEAN;
    isInfinite() : BOOLEAN;
    sign() : INTEGER;
    infinity() : REAL;
    nan() : REAL;    
    normalizedBoundaries(VAR outMminus,outMplus : GFP);
    lowerBoundaryIsCloser() : BOOLEAN;
  END;

(* These can be considered as INTEGER.
 * Arguably making them all LONGINT but that is inconvenient in places.
 *)
TYPE Sign32 = [0 .. 1]; (* 1 bit *)
TYPE Exponent32 = [0 .. 16_FF]; (* 8 bits *)
TYPE Significand32 = [0 .. 16_7FFFFF]; (* 23 bits *)

TYPE Int32 = Uint32;

(* For unfortunate historical reasons a float64 can be considered
 * as having its significand split into two pieces.
 * This is presumably so the parts fit in 32bit INTEGER.
 * These can be considered as INTEGER except Significand64 = LONGINT.
 * Arguably making them all LONGINT but that is inconvenient in places.
 *)
TYPE Sign64 = [0 .. 1]; (* 1 bit *)
TYPE Exponent64 = [0 .. 16_7FF]; (* 11 bits *)
TYPE Significand64_0 = [0 .. 16_FFFFF];
TYPE Significand64_1 = [-16_7fffffff - 1 .. 16_7fffffff];
TYPE Significand64 = [0L .. 16_FFFFFFFFFFFFFL]; (* 52 bits *)

(* These unpacked forms are endian-neutral,
 * to aid endian neutrality of C++ backend output,
 * to have endian neutral bootstrap distributions.
 *
 * That is, there are no bitfields here.
 *)
TYPE Float64TwoPartSignificand = RECORD
    significand1 : Significand64_1 := 0; (* 32 bits *)
    significand0 : Significand64_0 := 0; (* 20 bits *)
    exponent     : Exponent64      := 0; (* 11 bits *)
    sign         : Sign64          := 0;
END;

TYPE Float64OnePartSignificand = RECORD
    significand  : Significand64   := 0L; (* 52 bits *)
    exponent     : Exponent64      := 0;  (* 11 bits *)
    sign         : Sign64          := 0;
END;

TYPE Float32 = RECORD
    significand : Significand32 := 0; (* 23 bits *)
    exponent    : Exponent32    := 0; (* 8 bits *)
    sign        : Sign32        := 0;
END;

PROCEDURE Unpack32 (r:REAL): Float32;
PROCEDURE Unpack64OnePartSignificand (r:LONGREAL): Float64OnePartSignificand;
PROCEDURE Unpack64TwoPartSignificand (r:LONGREAL): Float64TwoPartSignificand;

PROCEDURE Pack32 (READONLY r:Float32): REAL;
PROCEDURE Pack64TwoPartSignificand (READONLY r:Float64TwoPartSignificand): LONGREAL;
PROCEDURE Pack64OnePartSignificand (READONLY r:Float64OnePartSignificand): LONGREAL;

(* These can be provided if/as needed.
 * PROCEDURE SetSign32 (VAR a: REAL; sign: Sign32);
 * PROCEDURE SetExponent32 (VAR a: REAL; exponent: Exponent32);
 * PROCEDURE SetSignificand32 (VAR a: REAL; significand: Significand32);
 *
 * PROCEDURE GetSign32 (a: REAL): Sign32;
 * PROCEDURE GetExponent32 (a: REAL): Exponent32;
 * PROCEDURE GetSignificand32 (a: REAL): Significand32;
 *
 * PROCEDURE SetSign64 (VAR a: LONGREAL; sign: Sign64);
 * PROCEDURE SetExponent64 (VAR a: LONGREAL; exponent: Exponent64);
 * PROCEDURE SetSignificand64_0 (VAR a: LONGREAL; significand0: Significand64_0);
 * PROCEDURE SetSignificand64_1 (VAR a: LONGREAL; significand1: Significand64_1);
 * PROCEDURE SetSignificand64 (VAR a: LONGREAL; significand: Significand64);
 *
 * PROCEDURE GetSign64 (a: LONGREAL): Sign64;
 * PROCEDURE GetExponent64 (a: LONGREAL): Exponent64;
 * PROCEDURE GetSignificand64 (a: LONGREAL): Significand64;
 * PROCEDURE GetSignificand64_0 (a: LONGREAL): Significand64_0;
 * PROCEDURE GetSignificand64_1 (a: LONGREAL): Significand64_1;
 *)

END IEEE.
