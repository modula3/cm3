(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Thu Dec  9 11:29:00 PST 1993 by mcjones    *)
(*      modified on Thu Apr 29 13:58:11 PDT 1993 by muller     *)
(*      modified on Mon Feb 15 15:17:13 PST 1993 by ramshaw    *)

(* The generic interface "Float" provides access to the floating-point
   operations required or recommended by the IEEE floating-point
   standard.  Consult the standard to resolve any fine points in the
   specification of the procedures.  Non-IEEE implementations that
   have values similar to NaNs and infinities should explain how those
   values behave in an implementation guide. (NaN is an IEEE term
   whose informal meaning is ``not a number''.) *)

GENERIC INTERFACE Float(R);

IMPORT FloatMode;

TYPE T = R.T;

PROCEDURE Scalb(x: T; n: INTEGER): T RAISES {FloatMode.Trap};
(* Return $\hbox"x"\cdot 2^{\hbox"n"}$. *)

PROCEDURE Logb(x: T): T RAISES {FloatMode.Trap};
(* Return the exponent of "x".  More precisely, return the unique
   integer $n$ such that the ratio $\hbox"ABS(x) / Base"^{n}$ is in
   the half-open interval "[1..Base)", unless "x" is denormalized, in
   which case return the minimum exponent value for "T". *)

PROCEDURE ILogb(x: T): INTEGER;
(* Like "Logb", but returns an integer, never raises an exception, and
   always returns the $n$ such that $\hbox"ABS(x) / Base"^{n}$ is in
   the half-open interval "[1..Base)", even for denormalized numbers.
   Special cases: it returns "FIRST(INTEGER)" when "x" = 0.0,
   "LAST(INTEGER)" when "x" is plus or minus infinity, and zero when
   "x" is NaN.  *)

PROCEDURE NextAfter(x, y: T): T RAISES {FloatMode.Trap};
(* Return the next representable neighbor of "x" in the direction
   towards "y".  If "x = y", return "x". *)

PROCEDURE CopySign(x, y: T): T;
(* Return "x" with the sign of "y". *)

PROCEDURE Finite(x: T): BOOLEAN;
(* Return "TRUE" if "x" is strictly between minus infinity and plus
   infinity.  This always returns "TRUE" on non-IEEE implementations.
   *)

PROCEDURE IsNaN(x: T): BOOLEAN;
(* Return "FALSE" if "x" represents a numerical (possibly
   infinite) value, and "TRUE" if "x" does not represent a
   numerical value.  For example, on IEEE implementations, returns
   "TRUE" if x is a NaN, "FALSE" otherwise. *)

(*
\index{NaN (not a number)}
*)


PROCEDURE Sign(x: T): [0..1];
(* Return the sign bit "x".  For non-IEEE implementations, this is
   the same as "ORD(x >= 0)"; for IEEE implementations,
   "Sign(-0) = 1" and "Sign(+0) = 0". *)

PROCEDURE Differs(x, y: T): BOOLEAN;
(* Return "(x < y OR y < x)".  Thus, for IEEE implementations,
   "Differs(NaN,x)" is always "FALSE"; for non-IEEE implementations,
   "Differs(x,y)" is the same as "x # y". *)
    
PROCEDURE Unordered(x, y: T): BOOLEAN;
(* Return "NOT (x <= y OR y <= x)". Thus, for IEEE implementations,
   "Unordered(NaN, x)" is always "TRUE"; for non-IEEE implementations,
   "Unordered(x, y)" is always "FALSE".
*)

PROCEDURE Sqrt(x: T): T RAISES {FloatMode.Trap};
(* Return the square root of "T".  This must be correctly rounded if
   "FloatMode.IEEE" is "TRUE". *)

TYPE IEEEClass =
  {SignalingNaN, QuietNaN, Infinity, Normal, Denormal, Zero};

PROCEDURE Class(x: T): IEEEClass;
(* Return the IEEE number class containing "x".  On non-IEEE systems,
   the result will be "Normal" or "Zero". *)

PROCEDURE FromDecimal(
    sign: [0..1];
    READONLY digits: ARRAY OF [0..9];
    exp: INTEGER): T RAISES {FloatMode.Trap};
(* Convert from floating-decimal to type "T". *)

(* \index{floating-point!conversion from decimal}
   \index{decimal conversion!to floating-point}

   Let "F" denote the nonnegative, floating-decimal number

| digits[0] . digits[1] ... digits[LAST(digits)] * 10^exp
| = sum(i, digits[i] * 10^(exp - i))

   The result of "FromDecimal" is the number "(-1)^sign * F", rounded
   to a value of type "T".

   The procedure "FromDecimal" is a floating-point operation, just
   like "+" and "*", in the sense that it rounds its ideal result
   correctly, observing the current rounding mode, and it sets flags
   and raises traps by the usual rules.  On IEEE implementations, it
   returns minus zero when "F" is sufficiently small and "sign=1". *)

TYPE DecimalApprox = RECORD
    class: IEEEClass;
    sign: [0..1];
    len: [1..R.MaxSignifDigits];
    digits: ARRAY[0..R.MaxSignifDigits-1] OF [0..9];
    exp: INTEGER;
    errorSign: [-1..1]
  END;

PROCEDURE ToDecimal(x: T): DecimalApprox;
(* Convert from type "T" to floating-decimal. *)

(* \index{floating-point!conversion to decimal}
   \index{decimal conversion!from floating-point}

   Let "D" denote "ToDecimal(x)".  Then, "D.class = Class(x)" and
   "D.sign = Sign(x)".  The other fields are defined only when
   "D.class" is either "Normal" or "Denormal".  In those cases, the
   values "D.len", "D.digits[0]" through "D.digits[D.len-1]", and
   "D.exp" encode a floating-decimal number "F" with the property that
   "(-1)^D.sign * F" approximates "x" in a sense discussed below.  The
   encoding is such that

| F = digits[0] . digits[1] ... digits[len - 1]  *  10^exp
|   = sum(i, digits[i] * 10^(exp - i))

   and

| ABS(x) = F * (1 + errorSign * epsilon)

   where "epsilon" is small and positive.  In particular, "D.errorSign"
   is "+1", "0", or "-1" according as "ABS(x)" is larger than, equal
   to, or smaller than "F".

   The current rounding mode determines the sense in which the
   floating-decimal number "(-1)^sign * F" approximates "x", but in a
   slightly subtle way.  Define the opposite of a directed rounding
   mode by reversing the direction, as follows:

|      Opp(TowardPlusInfinity) := TowardMinusInfinity
|     Opp(TowardMinusInfinity) := TowardPlusInfinity
|              Opp(TowardZero) := AwayFromZero

   Note that "AwayFromZero" isn't actually a rounding mode, but it is
   clear what it would mean if it were.  For all other rounding modes
   "M", we define "Opp(M) = M".  If the current rounding mode is "M",
   the call "ToDecimal(x)" returns a floating-decimal number that
   "FromDecimal" would convert, under rounding mode "Opp(M)", back to
   "x".  Among all such numbers, the returned value has as few digits
   as possible.  This implies that both "D.digits[0]" and
   "D.digits[D.len-1]" are nonzero.  If there is a tie for having the
   fewest digits, the tying number closest to "x" wins.  If there is
   also a tie for being closest to "x", it must be a two-way tie and
   the number whose last digit is even wins.

   Unlike "FromDecimal", "ToDecimal" never sets a "FloatMode.Flag" and
   never raises "FloatMode.Trap".

   The idea of converting to decimal by retaining just as many digits
   as are necessary to convert back to binary exactly was popularized
   by Guy L.~Steele Jr.\ and Jon L White~\cite{Steele}.  David M.~Gay
   pointed out the importance, in this context, of demanding that the
   conversion to binary handle mid-point cases by a known
   rule~\cite{Gay}.  For example, in IEEE double precision, the
   floating-decimal number "1e23" is precisely halfway between two
   adjacent floating-binary numbers.  If conversion to binary were
   allowed to go either way in such a mid-point case, conversion to
   decimal would have to avoid producing the simple number "1e23",
   producing instead either "1.0000000000000001e23" or
   "9.999999999999999e22".  We believe the idea of combining the
   Steele/White style of automatic precision control with directed
   rounding by using opposite rounding modes, as above, is new with
   Lyle Ramshaw. *)

END Float.
