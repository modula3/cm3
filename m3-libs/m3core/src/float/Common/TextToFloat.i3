(* Copyright (C) 2016 Peter McKinna. All rights reserved. *)
(* See file COPYRIGHT-BSD for details. *)

INTERFACE TextToFloat;

(* Convert a string to a floating point representation. The three types converted
   are for 32 bit REAL, 64 bit LONGREAL and 128 bit 'EXTENDED' also known as
   quad precision. 
   Based on David Gay's strtodg.c 
   https://github.com/jwiegley/gdtoa
   
   which are generalisations of the routines described in
   
   David M. Gay, "Correctly Rounded Binary-Decimal and
   Decimal-Binary Conversions", Numerical Analysis Manuscript
   No. 90-10, Bell Labs, Murray Hill, 1990;
   http://cm.bell-labs.com/cm/cs/what/ampl/REFS/rounding.ps.gz   
   
*)

IMPORT RealRep,LongRealRep;

(* return codes *)
CONST
  STRTOG_Zero     = 0;
  STRTOG_Normal   = 1;
  STRTOG_Denormal = 2;
  STRTOG_Infinite = 3;
  STRTOG_NaN      = 4;
  STRTOG_NaNbits  = 5; (* Not used *)
  STRTOG_NoNumber = 6;
  STRTOG_Retmask  = 7;

  (* The following may be or-ed into one of the above values. *)

  STRTOG_Neg       = 16_08;
  STRTOG_Inexlo    = 16_10;
  STRTOG_Inexhi    = 16_20;
  STRTOG_Inexact   = 16_30;
  STRTOG_Underflow = 16_40;
  STRTOG_Overflow  = 16_80;

TYPE
  RoundingModes = {RoundZero, RoundNear, RoundUp, RoundDown};

  (* Currently cm3 treats EXTENDED as LONGREAL but we need a 128 bit representation
  Eventually this definition will replace the one in ExtendedRep.i3 *)
  ExtendedRep = RECORD
    significand3 : BITS 32 FOR [-16_7fffffff-1 .. 16_7fffffff] := 0;
    significand2 : BITS 32 FOR [-16_7fffffff-1 .. 16_7fffffff] := 0;
    significand1 : BITS 32 FOR [-16_7fffffff-1 .. 16_7fffffff] := 0;
    significand0 : BITS 16 FOR [0..16_FFFF]       := 0;
    exponent     : BITS 15 FOR [0..16_7FFF]       := 0;
    sign         : BITS  1 FOR [0..1]             := 0;
  END;
  
PROCEDURE StrToReal(in : TEXT; rounding : RoundingModes; VAR real : RealRep.T) : INTEGER;

PROCEDURE StrToLongReal(in : TEXT; rounding : RoundingModes; VAR long : LongRealRep.T) : INTEGER;

PROCEDURE StrToExtended(in : TEXT; rounding : RoundingModes; VAR quad : ExtendedRep) : INTEGER;

END TextToFloat.
