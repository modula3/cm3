(* Copyright (C) 2016 Peter McKinna. All rights reserved. *)
(* See file COPYRIGHT-BSD for details. *)

INTERFACE Grisu;

(* Based on a paper by Florian Loitsch

  http://florian.loitsch.com/publications/dtoa-pldi2010.pdf?attredirects=0
  
  Grisu3 uses extra bits on integer arithmetic to convert a REAL or LONGREAL to ascii
  about 4 times faster than Dragon4 in 99.4% of cases.
*)

(* FastDtoa will produce at most FastDtoaMaximalLength digits. This does not
   include the terminating '\0' character. *)
   
CONST
  FastDtoaMaximalLength = 17;
  (* Same for single-precision numbers. *)
  FastDtoaMaximalSingleLength = 9;

TYPE
  FastDtoaMode = {
    (* Computes the shortest representation of the given input. The returned
       result will be the most accurate number of this length. Longer
       representations might be more accurate.*)
    FAST_DTOA_SHORTEST,
    (* Same as FAST_DTOA_SHORTEST but for single-precision floats. *)
    FAST_DTOA_SHORTEST_SINGLE,
    (* Computes a representation where the precision (number of digits) is
       given as input. The precision is independent of the decimal point.*)
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