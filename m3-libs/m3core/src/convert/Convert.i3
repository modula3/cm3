(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Fri Feb 18 13:17:07 PST 1994 by kalsow    *)
(*      modified on Fri Jan  5 06:59:24 1990 by muller        *)

INTERFACE Convert;

(* Binary/ASCII conversions of numbers.

   This interface provides binary/ASCII conversions for
   INTEGERs, REALs and LONGREALs.

   Index: conversion;  numbers;  ASCII *)

TYPE
  Buffer = ARRAY OF CHAR;
  Base   = [2..16];

TYPE
  Style  = {Flo, AltFlo, Sci, AltSci, Mix};
  (* formatting styles for REALs:
         Flo:       xxx.yyy
         AltFlo:    xxx.yyy             (trailing zeros are suppressed)
         Sci:       xxx.yyy'E'exponent  ('D' for LONGREAL)
         AltSci:    xxx.yyy'E'exponent  (trailing zeros are suppressed)
         Mix:       == AltFlo unles AltSci is shorter;  if AltFlow is
                       selected and there are no zeros after the decimal
                       point, the decimal point is suppressed.
  *)

EXCEPTION Failed;


(*---- Binary to ASCII conversions ----*)

(* The "From" procedures convert binary values to ASCII character strings.
   Each procedure returns the number characters that resulted.  Extra
   space in the buffers is left unmodified.  Failed is raised if the
   supplied buffer is too small to hold the result. *)

PROCEDURE FromInt (VAR buf    : Buffer;
                       value  : INTEGER;
                       base   : Base := 10;
                       prefix : BOOLEAN := FALSE): INTEGER RAISES {Failed};
(* converts value to ASCII in the specified base and stores the result in buf.
   If prefix=TRUE, include the base prefix in the result. *)

PROCEDURE FromUnsigned (VAR buf    : Buffer;
                            value  : INTEGER;
                            base   : Base := 10;
                            prefix : BOOLEAN := FALSE): INTEGER RAISES{Failed};
(* treats value as an unsigned 32-bit number, converts it to ASCII in the
   specified base and stores the result in buf.  If prefix=TRUE, include
   the base prefix in the result. *)

PROCEDURE FromFloat (VAR buf       : Buffer;
                         value     : REAL;
                         precision : INTEGER := 6;
                         style     := Style.Mix): INTEGER   RAISES {Failed};
(* converts value to ASCII in the given style and stores the result in buf.
   The precision is the number of fractional digits.  *)

PROCEDURE FromLongFloat (VAR buf   : Buffer;
                         value     : LONGREAL;
                         precision : INTEGER := 6;
                         style     := Style.Mix): INTEGER   RAISES {Failed};
(* converts value to ASCII in the given style and stores the result in buf.
   The precision is the number of fractional digits.  *)

PROCEDURE FromExtended (VAR buf   : Buffer;
                        value     : EXTENDED;
                        precision : INTEGER := 6;
                        style     := Style.Mix): INTEGER   RAISES {Failed};
(* converts value to ASCII in the given style and stores the result in buf.
   The precision is the number of fractional digits.  *)


(*---- ASCII to binary conversions ---*)

(* The "To" procedures convert ASCII character strings to their
   corresponding binary representations.  The procedures convert
   the maximum number of characters possible.  The number of characters
   actually used is returned in 'used'.  *)

PROCEDURE ToInt (READONLY buf  : Buffer;
                      VAR used : INTEGER;
                          base : Base := 10): INTEGER   RAISES {};
(* converts an integer.  The characters are interpreted in the specified
   base unless an explicit base prefix is in the number. *)

PROCEDURE ToUnsigned (READONLY buf  : Buffer;
                           VAR used : INTEGER;
                               base : Base := 10): INTEGER  RAISES {};
(* converts an unsigned number. The characters are interpreted in the specified
   base unless an explicit base prefix is in the number. *)

PROCEDURE ToFloat (READONLY buf  : Buffer;
                        VAR used : INTEGER): REAL  RAISES {Failed};
(* converts a floating point number. *)

PROCEDURE ToLongFloat (READONLY buf  : Buffer;
                        VAR used : INTEGER): LONGREAL  RAISES {Failed};
(* converts a floating point number. *)

PROCEDURE ToExtended (READONLY buf  : Buffer;
                      VAR used : INTEGER): EXTENDED  RAISES {Failed};
(* converts a floating point number. *)

END Convert.
