(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Wed Aug 18 20:33:57 PDT 1993 by heydon     *)
(*      modified on Fri Nov  3 14:14:31 PDT 1989 by muller     *)
(*      modified on Fri Oct 20 11:16:20 PDT 1989 by kalsow     *)
(*      modified on Fri Jan 20 12:42:01 PDT 1989 by glassman   *)
(*      modified on Thu May 21 17:29:38 PDT 1987 by rovner     *)
(*      modified on Sun Jun 22 11:05:15 PDT 1986 by violetta   *)

INTERFACE Math;

(* An interface to the C math library

   Programs that call any of these routines must be linked
   with the math library "-lm".

   The detailed semantics of these procedures are defined by
   your local C math library.  To learn the full story about
   any of these functions (e.g. their domains, ranges and
   accuracies), see the appropriate man page.

   Index: floating point, C math interface; 
          C programming, interface to C math library 
*)

FROM Ctypes IMPORT int;

(*---- miscellaneous useful constants ----*)

CONST
  Pi     = 3.1415926535897932384626433833D0;
  LogPi  = 1.1447298858494001741434273514D0;
  SqrtPi = 1.7724538509055160272981674833D0;
  E      = 2.7182818284590452353602874714D0;
  Degree = 0.017453292519943295769236907684D0;  (* One degree in radians *)


(*---- Exponential and Logarithm functions ----*)

<*EXTERNAL*> PROCEDURE exp (x: LONGREAL): LONGREAL;
(* returns E^x. *)

<*EXTERNAL*> PROCEDURE expm1 (x: LONGREAL): LONGREAL;
(* returns (E^x)-1, even for small x. *)

<*EXTERNAL*> PROCEDURE log (x: LONGREAL): LONGREAL;
(* returns the natural logarithm of x (base E). *)

<*EXTERNAL*> PROCEDURE log10 (x: LONGREAL): LONGREAL;
(* returns the base 10 logarithm of x. *)

<*EXTERNAL*> PROCEDURE log1p (x: LONGREAL): LONGREAL;
(* returns log(1+x), even for small x. *)

<*EXTERNAL*> PROCEDURE pow (x, y: LONGREAL): LONGREAL;
(* returns x^y. *)

<*EXTERNAL*> PROCEDURE sqrt (x: LONGREAL): LONGREAL;
(* returns the square root of x. *)


(*---- Trigonometric functions ----*)

<*EXTERNAL*> PROCEDURE cos (x: LONGREAL): LONGREAL;
(* returns the cosine of x radians. *)

<*EXTERNAL*> PROCEDURE sin (x: LONGREAL): LONGREAL;
(* returns the sine of x radians. *)

<*EXTERNAL*> PROCEDURE tan (x: LONGREAL): LONGREAL;
(* returns the tangent of x radians. *)

<*EXTERNAL*> PROCEDURE acos (x: LONGREAL): LONGREAL;
(* returns the arc cosine of x in radians. *)

<*EXTERNAL*> PROCEDURE asin (x: LONGREAL): LONGREAL;
(* returns the arc sine of x in radians. *)

<*EXTERNAL*> PROCEDURE atan (x: LONGREAL): LONGREAL;
(* returns the arc tangent of x in radians. *)

<*EXTERNAL*> PROCEDURE atan2 (y, x: LONGREAL): LONGREAL;
(* returns the arc tangent of y/x in radians. *)


(*---- Hyperbolic trigonometric functions ----*)

<*EXTERNAL*> PROCEDURE sinh (x: LONGREAL): LONGREAL;
(* returns the hyperbolic sine of x. *)

<*EXTERNAL*> PROCEDURE cosh (x: LONGREAL): LONGREAL;
(* returns the hyperbolic cosine of x. *)

<*EXTERNAL*> PROCEDURE tanh (x: LONGREAL): LONGREAL;
(* returns the hyperbolic tangent of x. *)

<*EXTERNAL*> PROCEDURE asinh (x: LONGREAL): LONGREAL;
(* returns the inverse hyperbolic sine of x *)

<*EXTERNAL*> PROCEDURE acosh (x: LONGREAL): LONGREAL;
(* returns the inverse hyperbolic cosine of x *)

<*EXTERNAL*> PROCEDURE atanh (x: LONGREAL): LONGREAL;
(* returns the inverse hyperbolic tangent of x *)


(*---- Rounding functions ----*)

<*EXTERNAL*> PROCEDURE ceil (x: LONGREAL): LONGREAL;
(* returns the least integer not less than x.
   Note: use the builtin Modula-3 function CEILING. *)

<*EXTERNAL*> PROCEDURE floor (x: LONGREAL): LONGREAL;
(* returns the greatest integer not greater than x.
   Note: use the builtin Modula-3 function FLOOR. *)

<*EXTERNAL*> PROCEDURE rint (x: LONGREAL): LONGREAL;
(* returns the nearest integer value to x.
   Note: the Modula-3 function ROUND may be appropriate. *)

<*EXTERNAL*> PROCEDURE fabs (x: LONGREAL): LONGREAL;
(* returns the absolute value of x.
   Note: use the builtin Modula-3 function ABS. *)


(*---- Euclidean distance functions ----*)

<*EXTERNAL*> PROCEDURE hypot (x, y: LONGREAL): LONGREAL;
(* returns sqrt (x*x + y*y). *)

<*EXTERNAL*> PROCEDURE cabs (z: Complex): LONGREAL;
TYPE Complex = RECORD x, y: LONGREAL END;
(* returns sqrt (z.x*z.x + z.y*z.y) *)


(*---- Floating point representations ----*)

<*EXTERNAL*> PROCEDURE frexp (x: LONGREAL;  VAR exp: int): LONGREAL;
(* returns a value y and sets exp such that x = y * 2^exp,
    where ABS(y) is in the interval [0.5, 1). *)

<*EXTERNAL*> PROCEDURE ldexp (x: LONGREAL; exp: int): LONGREAL;
(* returns x * 2^exp. *)

<*EXTERNAL*> PROCEDURE modf (x: LONGREAL; VAR(*OUT*) i: LONGREAL): LONGREAL;
(* splits the argument "x" into an integer part "i" and a fractional part "f"
   such that "f + i = x" and such that "f" and "i" both have the same sign as
   "x", and returns "f". Although "i" is a LONGREAL, it is set to an integral
   value. *)

(*---- Error functions ----*)

<*EXTERNAL*> PROCEDURE erf (x: LONGREAL): LONGREAL;
(* returns the "error" function of x. *)

<*EXTERNAL*> PROCEDURE erfc (x: LONGREAL): LONGREAL;
(* returns 1.0 - erf(x), even for large x. *) 


(*---- Gamma function ----*)

<*EXTERNAL*> PROCEDURE gamma (x: LONGREAL): LONGREAL;
<*EXTERNAL*> VAR signgam: int;
(* returns log(ABS(Gamma(ABS(x)))).  The sign of Gamma(ABS(X))
   is returned in signgam. *)


(*---- Bessel functions ----*)

<*EXTERNAL*> PROCEDURE j0 (x: LONGREAL): LONGREAL;
(* returns the zero-order Bessel function of first kind on x. *)

<*EXTERNAL*> PROCEDURE j1 (x: LONGREAL): LONGREAL;
(* returns the first-order Bessel function of first kind on x. *)

<*EXTERNAL*> PROCEDURE jn (n: int;  x: LONGREAL): LONGREAL;
(* returns the n th-order Bessel function of first kind on x. *)

<*EXTERNAL*> PROCEDURE y0 (x: LONGREAL): LONGREAL;
(* returns the zero-order Bessel function of second kind on x. *)

<*EXTERNAL*> PROCEDURE y1 (x: LONGREAL): LONGREAL;
(* returns the first-order Bessel function of second kind on x. *)

<*EXTERNAL*> PROCEDURE yn (n: int;  x: LONGREAL): LONGREAL;
(* returns the n th-order Bessel function of second kind on x. *)

(*---- Modulo functions ----*)

<*EXTERNAL*> PROCEDURE fmod (x, y: LONGREAL): LONGREAL;
(* returns the remainder of dividing x by y.
   Note: use the built-in Modula-3 function MOD. *)

<*EXTERNAL*> PROCEDURE drem (x, y: LONGREAL): LONGREAL;
<*EXTERNAL*> PROCEDURE remainder (x, y: LONGREAL): LONGREAL;
(* returns remainder "r = x - n*y", where "n = ROUND(x/y)".
   Note: the Modula-3 functions MOD and ROUND may be appropriate. *)

END Math.

