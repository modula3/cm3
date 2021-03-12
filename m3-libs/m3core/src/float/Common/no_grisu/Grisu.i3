(* Copyright (C) 2016 Peter McKinna. All rights reserved. *)
(* See file COPYRIGHT-BSD for details. *)

(* This is a temporary version that always returns FALSE
 * until problems with integrated and C backends are investigated
 * fixed. C backend output fails at runtime, integrated backend
 * fails to compile.
 *)
INTERFACE Grisu;

CONST
  FastDtoaMaximalLength = 17;
  FastDtoaMaximalSingleLength = 9;

TYPE
  FastDtoaMode = {
    FAST_DTOA_SHORTEST,
    FAST_DTOA_SHORTEST_SINGLE,
    FAST_DTOA_PRECISION
  };

  DigitArr = ARRAY OF [0..9];

(* FastDtoa returns FALSE if it cannot return an accurate value in which case 
   another method (such as Dragon4) must be used *)
PROCEDURE FastDtoa(v : LONGREAL;
                   mode : FastDtoaMode;
                   requestedDigits : INTEGER;
                   VAR buffer : DigitArr;
                   VAR length : CARDINAL;
                   VAR decimalPoint : INTEGER) : BOOLEAN;
END Grisu.
