(* Copyright (C) 1994, Digital Equipment Corporation                   *)
(* All rights reserved.                                                *)
(* See the file COPYRIGHT for a full description.                      *)
(*                                                                     *)
(* Last modified on Wed Mar 16 12:32:45 PST 1994 by heydon             *)

INTERFACE FmtBufF;

(* A "friends" interface for the "FmtBuf" interface that reveals
   internal types and procedures. *)

IMPORT Fmt, FmtBuf, RealFloat, LongFloat, ExtendedFloat;

TYPE
  Class = { NaN, Inf, Number, Zero };
  IEEEKind = { Single, Double, Extended };

(* The "Class" type is a coarse-grained and precision-independent
   representation of the class of an IEEE floating-point number.
   The "NaN" class includes both quiet and signalling NaN's. The
   "Number" class includes both normal and denormal numbers.

   The "IEEEKind" type enumerates the three IEEE floating-point
   precisions. *)

CONST
  ClassMapReal = ARRAY RealFloat.IEEEClass OF Class{
    Class.NaN, Class.NaN, Class.Inf, Class.Number, Class.Number, Class.Zero};
  ClassMapLong = ARRAY LongFloat.IEEEClass OF Class{
    Class.NaN, Class.NaN, Class.Inf, Class.Number, Class.Number, Class.Zero};
  ClassMapExtd = ARRAY ExtendedFloat.IEEEClass OF Class{
    Class.NaN, Class.NaN, Class.Inf, Class.Number, Class.Number, Class.Zero};

(* The "ClassMapReal", "ClassMapLong", and "ClassMapExtd" arrays
   are maps from the appropriate "IEEEClass" type to the corresponding
   precision-independent "Class" type. *)

TYPE
  NumAttr = RECORD
    class: Class;
    kind: IEEEKind;
    sign: [0..1];
    maxExpDigits: CARDINAL;
    len: CARDINAL;
    exp: INTEGER;
    errorSign: [-1..1];
  END;
  Digits = ARRAY OF [0..9];

(* This interface represents a floating point number by a pair of values
   "(num, digits)" in the set "NumAttr x Digits". "num" contains attributes
   of the number, and "digits" contains the digits of the number. The
   first four fields of "num" are defined by:

|   "class" denotes the class of the number,
|   "kind" denotes the precision of the number,
|   "sign" is the sign bit of the number (0 = positive, 1 = negative), and
|   "maxExpDigits" is the maximum number of base-10 exponent digits
|     required by "kind"-precision real numbers

   If "num.class = Class.Number", then the other fields of "num"
   are the same as those in the "DecimalApprox" structure produced
   by the "ToDecimal" procedure in instantiations of the "Float"
   interface. A "Digits" value contains the same digits as the "digits"
   field of the "DecimalApprox" record; this field has been removed from
   the record so it can be passed READONLY on the stack for efficiency. *)

TYPE
  FmtRec = RECORD
    style: Fmt.Style;
    prec: CARDINAL;
    literal: BOOLEAN;
  END;

(* A "FmtRec" bundles together the three formatting parameters that
   determine how a floating-point value should be formatted. *)

PROCEDURE Float(
    VAR (*OUT*) b: FmtBuf.T;
    READONLY num: NumAttr;
    VAR (*IN*) digits: Digits;
    READONLY fmt: FmtRec)
  : CARDINAL;
(* Format the number "(num, digits)" into the buffer "b" to precision
   "fmt.prec" according to formatting style "fmt.style" and "fmt.literal"
   as defined in the "Fmt" interface. Returns the number of characters
   written into the buffer "b". It is a checked run-time error for "b"
   not to be large enough to hold the result. The contents of "digits" is
   undefined on return. *)

(* The caller must guarantee that the buffer "b" passed to "Float" is
   sufficiently large. The tricky question is to decide how large the
   buffer needs to be. Our goal here is to develop an upper-bound that
   is easy to compute. We proceed by a case analysis.

   We need not consider the "Style.Auto" style, since the width of a
   number produced with "Style.Auto" is at most the maximum of the width
   produced using "Style.Sci" and "Style.Fix".

   A number of type "T" rendered to "prec" digits of precision with
   "Style.Sci" will normally produce a text of length "5 + prec +
   T.MaxExpDigits" (the 5 extra characters are for the leading sign,
   leading digit, decimal point, exponent character, and exponent sign).
   However, in the case that "prec = 0", the width will be 1 larger if
   "literal = TRUE" and 1 smaller otherwise. Also, in the case where
   "literal = TRUE", the special values "Nan" and "Infinity" require 8
   characters in the single-precision case and 12 characters in the
   double- and extended-precision cases. Hence, some good upper-bounds
   for "Style.Sci" are:

|    literal = FALSE:  width <= 5 + prec + T.MaxExpDigits
|    literal = TRUE:   width <= MAX(5 + MAX(prec, 1) + T.MaxExpDigits, 12)

   The width of a number rendered according to "Style.Fix" is a bit more
   difficult to bound. Independent of the "literal" parameter, the values
   "NaN" and "Infinity" require at most 12 characters, and the value zero
   requires at most "MAX(prec, 1) + 5" characters. A non-zero fixpoint
   number is formatted to have the form "[-]DD***D.PP***P[(d|x)0]". There
   are exactly "prec" digits after the decimal, but the number of digits
   before the decimal depends on the magnitude of the number. Suppose the
   base-10 exponent of the number, if rendered in scientific notation, is
   "exp". Then the number of digits before the decimal is given by
   "MAX(exp, 1)". Hence, the maximum width for each class of number
   rendered in "Style.Fix" is: 

|    Nan, Infinity:    width <= 12
|    Zero:             width <= 5 + MAX(prec, 1)
|    All others:       width <= 4 + MAX(prec, 1) + MAX(exp, 1)

   Hence, an overall cautious upper-bound is given by the following
   formula, where "exp" is defined to be zero in the "Nan", "Infinity",
   and "Zero" cases:

|    Style.Fix:        width <= MAX(4 + MAX(prec, 1) + MAX(exp, 1), 12)
*)

END FmtBufF.
