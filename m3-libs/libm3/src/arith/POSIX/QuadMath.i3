(* Copyright (C) 2022, Peter McKinna *)
(* All rights reserved. *)
(* See the BSD file COPYRIGHT for a full description. *)

INTERFACE QuadMath;

(* An interface to the C quadmath library

   These routines only make sense if EXTENDED is defined as 128 bit
   floating point equivalent to _FLoat128.

   Programs that call any of these routines must be linked with the quadmath
   library "-lquadmath".

   The detailed semantics of these procedures are defined by your local C
   quadmath library.  To learn the full story about any of these functions
   (e.g.  their domains, ranges and accuracies), see the appropriate man
   page.

   Index: floating point, C math interface; C programming, interface to C
   math library *)

FROM Ctypes IMPORT int, int_star, long_int, long_long, const_char_star;

(*---- miscellaneous useful constants ----*)

CONST
  E          = 2.7182818284590452353602874713526625X0; (* e *)
  Log2E      = 1.4426950408889634073599246810018921X0; (* log_2 e *)
  Log10E     = 0.4342944819032518276511289189166051X0; (* log_10 e *)
  Ln2        = 0.6931471805599453094172321214581766X0; (* log_e 2 *)
  Ln10       = 2.3025850929940456840179914546843642X0; (* log_e 10 *)
  Pi         = 3.1415926535897932384626433832795029X0; (* pi *)
  LogPi      = 1.1447298858494001741434273514X0;       (* log pi *)
  SqrtPi     = 1.7724538509055160272981674833X0;       (* pi ^ 0.5 *)
  PiDiv2     = 1.5707963267948966192313216916397514X0; (* pi/2 *)
  PiDiv4     = 0.7853981633974483096156608458198757X0; (* pi/4 *)
  PiInv      = 0.3183098861837906715377675267450287X0; (* 1/pi *)
  Pi2Inv     = 0.6366197723675813430755350534900574X0; (* 2/pi *)
  SqrtPi2Inv = 1.1283791670955125738961589031215452X0; (* 2/sqrt(pi) *)
  Sqrt2      = 1.4142135623730950488016887242096981X0; (* sqrt(2) *)
  Sqrt2Inv   = 0.7071067811865475244008443621048490X0; (* 1/sqrt(2) *)
  Degree     = 0.017453292519943295769236907684X0;     (* One deg in radians *)


(*---- Exponential and Logarithm functions ----*)

<* EXTERNAL *>
PROCEDURE expq (x: EXTENDED): EXTENDED;
(* returns E^x. *)

<* EXTERNAL *>
PROCEDURE expm1q (x: EXTENDED): EXTENDED;
(* returns (E^x)-1, even for small x. *)

<* EXTERNAL *>
PROCEDURE logq (x: EXTENDED): EXTENDED;
(* returns the natural logarithm of x (base E). *)

<* EXTERNAL *>
PROCEDURE log10q (x: EXTENDED): EXTENDED;
(* returns the base 10 logarithm of x. *)

<* EXTERNAL *>
PROCEDURE log1pq (x: EXTENDED): EXTENDED;
(* returns log(1+x), even for small x. *)

<* EXTERNAL *>
PROCEDURE powq (x, y: EXTENDED): EXTENDED;
(* returns x^y. *)

<* EXTERNAL *>
PROCEDURE sqrtq (x: EXTENDED): EXTENDED;
(* returns the square root of x. *)

<* EXTERNAL *>
PROCEDURE cbrtq (x : EXTENDED) : EXTENDED;
(* returns the cube root of x *)

<* EXTERNAL *>
PROCEDURE scalbnq (x : EXTENDED; y : int) : EXTENDED;
(* compute exponent using FLT_RADIX *)

<* EXTERNAL *>
PROCEDURE scalblnq (x : EXTENDED; y : long_int) : EXTENDED;
(* compute exponent using FLT_RADIX *)

<* EXTERNAL *>
PROCEDURE log2q (x : EXTENDED) : EXTENDED;
(* returns base 2 logarithm function *)

<* EXTERNAL *>
PROCEDURE logbq (x : EXTENDED) : EXTENDED;
(* get exponent of the value *)

<* EXTERNAL *>
PROCEDURE ilogbq (x : EXTENDED) : int;
(* get exponent of the value *)


(*---- Trigonometric functions ----*)

<* EXTERNAL *>
PROCEDURE cosq (x: EXTENDED): EXTENDED;
(* returns the cosine of x radians. *)

<* EXTERNAL *>
PROCEDURE sinq (x: EXTENDED): EXTENDED;
(* returns the sine of x radians. *)

<* EXTERNAL *>
PROCEDURE tanq (x: EXTENDED): EXTENDED;
(* returns the tangent of x radians. *)

<* EXTERNAL *>
PROCEDURE acosq (x: EXTENDED): EXTENDED;
(* returns the arc cosine of x in radians. *)

<* EXTERNAL *>
PROCEDURE asinq (x: EXTENDED): EXTENDED;
(* returns the arc sine of x in radians. *)

<* EXTERNAL *>
PROCEDURE atanq (x: EXTENDED): EXTENDED;
(* returns the arc tangent of x in radians. *)

<* EXTERNAL *>
PROCEDURE atan2q (y, x: EXTENDED): EXTENDED;
(* returns the arc tangent of y/x in radians. *)

<* EXTERNAL *>
PROCEDURE sincosq (x : EXTENDED; VAR sinq,cosq : EXTENDED);
(* returns the sin and cos of x simultaneously. *)


(*---- Hyperbolic trigonometric functions ----*)

<* EXTERNAL *>
PROCEDURE sinhq (x: EXTENDED): EXTENDED;
(* returns the hyperbolic sine of x. *)

<* EXTERNAL *>
PROCEDURE coshq (x: EXTENDED): EXTENDED;
(* returns the hyperbolic cosine of x. *)

<* EXTERNAL *>
PROCEDURE tanhq (x: EXTENDED): EXTENDED;
(* returns the hyperbolic tangent of x. *)

<* EXTERNAL *>
PROCEDURE asinhq (x: EXTENDED): EXTENDED;
(* returns the inverse hyperbolic sine of x *)

<* EXTERNAL *>
PROCEDURE acoshq (x: EXTENDED): EXTENDED;
(* returns the inverse hyperbolic cosine of x *)

<* EXTERNAL *>
PROCEDURE atanhq (x: EXTENDED): EXTENDED;
(* returns the inverse hyperbolic tangent of x *)


(*---- Rounding functions ----*)

<* EXTERNAL *>
PROCEDURE ceilq (x: EXTENDED): EXTENDED;
(* returns the least integer not less than x.  Note: use the builtin
   Modula-3 function CEILING. *)

<* EXTERNAL *>
PROCEDURE floorq (x: EXTENDED): EXTENDED;
(* returns the greatest integer not greater than x.  Note: use the builtin
   Modula-3 function FLOOR. *)

<* EXTERNAL *>
PROCEDURE rintq (x: EXTENDED): EXTENDED;
(* returns the nearest integer value to x.  Note: the Modula-3 function
   ROUND may be appropriate. *)

<* EXTERNAL *>
PROCEDURE roundq (x : EXTENDED) : EXTENDED;
(* round-to-nearest integral value, return __float128 *)

<* EXTERNAL *>
PROCEDURE fabsq (x: EXTENDED): EXTENDED;
(* returns the absolute value of x.  Note: use the builtin Modula-3
   function ABS. *)

<* EXTERNAL *>
PROCEDURE finiteq (x : EXTENDED) : int;
(* check finiteness of value *)

<* EXTERNAL *>
PROCEDURE truncq (x : EXTENDED) : EXTENDED;
(* returns round to integer, towards zero *)

<* EXTERNAL *>
PROCEDURE lroundq (x : EXTENDED) : long_int;
(* round to nearest integer value away from zero *)

<* EXTERNAL *>
PROCEDURE llroundq (x : EXTENDED) : long_long;
(* round to nearest integer value away from zero *)

<* EXTERNAL *>
PROCEDURE llrintq (x : EXTENDED) : long_long;
(* round to nearest integer *)

<* EXTERNAL *>
PROCEDURE lrintq (x : EXTENDED) : long_int;
(* round to nearest integer *)

<* EXTERNAL *>
PROCEDURE nearbyintq (x : EXTENDED) : EXTENDED;
(* round to nearest integer *)


(*---- Euclidean distance functions ----*)

<* EXTERNAL *>
PROCEDURE hypotq (x,y : EXTENDED) : EXTENDED;
(* returns sqrt (x*x + y*y). *)

<* EXTERNAL *>
PROCEDURE cabsq (z: Complex): EXTENDED;
TYPE Complex = RECORD x, y: EXTENDED END;
(* returns sqrt (z.x*z.x + z.y*z.y) *)


(*---- Floating point representations ----*)

<* EXTERNAL *>
PROCEDURE frexpq (x: EXTENDED; VAR exp: int): EXTENDED;
(* returns a value y and sets exp such that x = y * 2^exp, where ABS(y) is
   in the interval [0.5, 1). *)

<* EXTERNAL *>
PROCEDURE ldexpq (x: EXTENDED; exp: int): EXTENDED;
(* returns x * 2^exp. *)

<* EXTERNAL *>
PROCEDURE modfq (x: EXTENDED; VAR (*OUT*) i: EXTENDED): EXTENDED;
(* splits the argument "x" into an integer part "i" and a fractional part
   "f" such that "f + i = x" and such that "f" and "i" both have the same
   sign as "x", and returns "f".  Although "i" is an EXTENDED, it is set to
   an integral value. *)


(*---- Error functions ----*)

<* EXTERNAL *>
PROCEDURE erfq (x: EXTENDED): EXTENDED;
(* returns the "error" function of x. *)

<* EXTERNAL *>
PROCEDURE erfcq (x: EXTENDED): EXTENDED;
(* returns 1.0 - erf(x), even for large x. *)


(*---- Gamma function ----*)

<* EXTERNAL *>
PROCEDURE lgammaq (x: EXTENDED): EXTENDED;
(* returns log(ABS(Gamma(ABS(x)))). *)

<* EXTERNAL *>
PROCEDURE tgammaq (x : EXTENDED) : EXTENDED;
(* returns true gamma function *)


(*---- Bessel functions ----*)

<* EXTERNAL *>
PROCEDURE j0q (x: EXTENDED): EXTENDED;
(* returns the zero-order Bessel function of first kind on x. *)

<* EXTERNAL *>
PROCEDURE j1q (x: EXTENDED): EXTENDED;
(* returns the first-order Bessel function of first kind on x. *)

<* EXTERNAL *>
PROCEDURE jnq (n: int; x: EXTENDED): EXTENDED;
(* returns the n th-order Bessel function of first kind on x. *)

<* EXTERNAL *>
PROCEDURE y0q (x: EXTENDED): EXTENDED;
(* returns the zero-order Bessel function of second kind on x. *)

<* EXTERNAL *>
PROCEDURE y1q (x: EXTENDED): EXTENDED;
(* returns the first-order Bessel function of second kind on x. *)

<* EXTERNAL *>
PROCEDURE ynq (n: int; x: EXTENDED): EXTENDED;
(* returns the n th-order Bessel function of second kind on x. *)


(*---- Modulo functions ----*)

<* EXTERNAL *>
PROCEDURE fmodq (x, y: EXTENDED): EXTENDED;
(* returns the remainder of dividing x by y.  Note: use the built-in
   Modula-3 function MOD. *)

<* EXTERNAL *>
PROCEDURE remainderq (x, y: EXTENDED): EXTENDED;
(* returns remainder "r = x - n*y", where "n = ROUND(x/y)".  Note: the
   Modula-3 functions MOD and ROUND may be appropriate. *)

<* EXTERNAL *>
PROCEDURE remquoq (x,y : EXTENDED; z : int_star) : EXTENDED;
(* returns remainder and part of quotient *)


(*---- Miscellaneous ----*)

<* EXTERNAL *>
PROCEDURE fmaq (x,y,z : EXTENDED) : EXTENDED;
(* fused multiply and add *)

<* EXTERNAL *>
PROCEDURE fmaxq (x,y : EXTENDED) : EXTENDED;
(* determine maximum of two values *)

<* EXTERNAL *>
PROCEDURE fminq (x,y : EXTENDED) : EXTENDED;
(* determine minimum of two values *)

<* EXTERNAL *>
PROCEDURE signbitq (x : EXTENDED) : int;
(* returns sign bit *)

<* EXTERNAL *>
PROCEDURE copysignq (x,y : EXTENDED) : EXTENDED;
(* return a value whose magnitude is x but whose sign is y *)

<* EXTERNAL *>
PROCEDURE isinfq (x : EXTENDED) : int;
(* check for infinity *)

<* EXTERNAL *>
PROCEDURE isnanq (x : EXTENDED) : int;
(* check for nan *)

<* EXTERNAL *>
PROCEDURE nanq (x : const_char_star) : EXTENDED;
(* returns quiet nan *)

<* EXTERNAL *>
PROCEDURE fdimq (x,y : EXTENDED) : EXTENDED;
(* positive difference function *)

<* EXTERNAL *>
PROCEDURE nextafterq (x,y : EXTENDED) : EXTENDED;
(* next representable floating-point number *)


END QuadMath.

