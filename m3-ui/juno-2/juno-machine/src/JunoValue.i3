(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Mar 28 14:48:21 PST 1996 by heydon                   *)
(*      modified on Tue Nov 29 14:25:33 PST 1994 by gnelson                  *)

INTERFACE JunoValue;

(* A JunoValue.T represents a Juno value. A value in Juno is either nil, a
   real number, a text string, or a pair of values.

   This interface also defines the procedure "Unparse" to pretty-print a value
   to a "Wr.T", and the procedure "Equal" to determine if two Juno values are
   equal. *)

IMPORT Wr, Word, Formatter, Real AS R;

TYPE
  T = REFANY;
  (* Null | TEXT | REF Real | REF Pair *)

  Null <: T;
  Pair = RECORD car, cdr: T END;

(* The following three declarations control the precision of Juno-2's
   calculations. Currently they are set to use single precision. *)
   
TYPE
  Real = REAL;

CONST Zero = 0.0;

VAR HalfEps: Real;

(* "Zero" and "HalfEps" are the zero and the relative error for type 
   "Real". That is, the difference between a real number "x" and 
   the result of rounding it to a "Real" is at most "HalfEps*x".
   "HalfEps" is initialized in the body of the "JunoValue" module. *) 

VAR (*CONST*)
  Nil: Null;

(* The Juno value "NIL" is stored uniquely in the global variable "Nil". *)

CONST Prec: CARDINAL = R.MaxSignifDigits - 1;

PROCEDURE Unparse(wr: Wr.T; x: T; width: CARDINAL := 75; prec := Prec)
  RAISES {Wr.Failure};
(* Pretty-print the value "x" to "wr" to a line width of "width". Real numbers
   are unparsed to "prec" digits of precision. *)

PROCEDURE UnparseToFmt(f: Formatter.T; x: T; prec := Prec) RAISES {Wr.Failure};
(* Pretty-print the value "x" to the formatter "f". The formatter is neither
   flushed nor closed by this procedure. Real numbers are unparsed to "prec"
   digits of precision. *)

PROCEDURE Equal(READONLY x, y: T): BOOLEAN;
PROCEDURE Hash(READONLY k: T): Word.T;
(* Equality and hash procedures on Juno values. *)

(* The following procedures return the sine, cosine, tangent, arc-sine,
   arc-cosine, arc-tangent, exponential, natural logarithm, and square root of
   the JunoValue.Real "x". "Ln" returns NaN if "x <= 0.0", and "Sqrt" returns
   NAN if "x < 0.0". *)

PROCEDURE Sin(x: Real): Real;
PROCEDURE Cos(x: Real): Real;
PROCEDURE Tan(x: Real): Real;
PROCEDURE Asin(x: Real): Real;
PROCEDURE Acos(x: Real): Real;
PROCEDURE Atan(y, x: Real): Real; (* arc-tangent of "y/x" *)
PROCEDURE Exp(x: Real): Real;
PROCEDURE Ln(x: Real): Real;
PROCEDURE Sqrt(x: Real): Real;

PROCEDURE RefReal(x: Real): REF Real;
(* Return a "REF Real" with value "x". *)

PROCEDURE NewPoint(x, y: Real): REF Pair;
(* Return a pair of the numeric values "x" and "y". *)

PROCEDURE ListFromVals(READONLY v: ARRAY OF T): T;
(* Return the list containing the values of "v". *)

PROCEDURE IsList(v: T): BOOLEAN;
(* RETURN TRUE iff "v" is a (non-empty) list. *)

END JunoValue.
