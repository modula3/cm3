(*
   Copyright (c) 2008 Generation Capital Ltd.
   All rights reserved.

   Author: Mika Nystrom <mika@alum.mit.edu>
*)

(* $Id$ *)

INTERFACE FancyFmt;
FROM Fmt IMPORT Style, Base;
IMPORT Word, Real AS R, LongReal AS LR, Extended AS ER;

(* 
   The routines in this interface behave exactly like the ones in Fmt,
   except that if plain is set to FALSE (the default), and the results
   contain only decimal digits and an optional decimal point, numbers
   are reformatted with a superRadixComma between each superRadix group
   of integer digits.  If the result contains any other character,
   only the radixPoint is swapped as specified.

   If plain is set to TRUE, the results are passed through. 

   literal (for floating-point styles) overrides plain=FALSE.
*)

PROCEDURE Int(n: INTEGER; base: Base := 10;
              radixPoint := '.';
              superRadixComma := ',';
              superRadix : CARDINAL := 3;
              plain := FALSE): TEXT;

PROCEDURE Unsigned(n: Word.T; base: Base := 16;
                   radixPoint := '.';
                   superRadixComma := ',';
                   superRadix : CARDINAL := 3;
                   plain := FALSE): TEXT;

PROCEDURE Real(
    x: REAL;
    style := Style.Auto;
    prec: CARDINAL := R.MaxSignifDigits - 1;
    literal := FALSE;
    radixPoint := '.';
    superRadixComma := ',';
    superRadix : CARDINAL := 3;
    plain := FALSE)
  : TEXT;

PROCEDURE LongReal(
    x: LONGREAL;
    style := Style.Auto;
    prec: CARDINAL := LR.MaxSignifDigits - 1;
    literal := FALSE;
    radixPoint := '.';
    superRadixComma := ',';
    superRadix : CARDINAL := 3;
    plain := FALSE)
  : TEXT;

PROCEDURE Extended(
    x: EXTENDED;
    style := Style.Auto;
    prec: CARDINAL := ER.MaxSignifDigits - 1;
    literal := FALSE;
    radixPoint := '.';
    superRadixComma := ',';
    superRadix : CARDINAL := 3;
    plain := FALSE)
  : TEXT;
(* Format the floating-point number "x". *)

END FancyFmt.
