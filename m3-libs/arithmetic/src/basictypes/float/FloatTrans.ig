GENERIC INTERFACE FloatTrans(R,RB,RX);
(*Copyright (c) 1996, m3na project

Abstract: Generic wrapper routines for (mainly) transcendent functions

If R.T is
  REAL, then the instantiated module is like Mth
  LONGREAL, then the instantiated module is a wrapper for Math with procedure names conforming to the conventions
  EXTENDED, then the instantiated module may be used for transcendental computations but the precision is only that of LONGREAL because this is the precision of the standard C math library

SqRt should throw an exception if applied to negative numbers
*)

(*==========================*)

TYPE
  T = R.T;

(*=================*)
TYPE
  Ftn    = PROCEDURE(x:T):T;

CONST
  (*---distinguished elements---*)
  Zero        = RB.Zero;
  Half        = RB.Half;
  One         = RB.One;
  MinusOne    = RB.MinusOne;
  Two         = RB.Two;

  E           = FLOAT(2.71828182845904523536028747135266249776X0,T); (* e *)
  LnFour      = FLOAT(1.38629436111989061883446424291635313615X0,T); (* ln(4) *)
  SqRt2ByE    = FLOAT(0.85776388496070679648018964127877247812X0,T); (* sqrt(2/e) *)
  SqRtTwo     = FLOAT(1.41421356237309504880168872420969807857X0,T); (* sqrt(2) *)

  LnTwo       = LnFour*Half; (*FLOAT(0.693147180559945D0,T);*) (* ln(2) *)

  Pi          = FLOAT(3.141592653589793D0,T);
  TwoPi       = FLOAT(6.283185307179586D0,T);
  OneOverPi   = FLOAT(0.318309886183791D0,T);
  TwoOverPi   = FLOAT(0.636619772367581D0,T);
  FourOverPi  = FLOAT(1.273239544735163D0,T);
  LnPi        = FLOAT(1.144729885849400D0,T);  (*ln(pi) *)

  EulerGamma  = FLOAT(0.577215632901532D0,T);  (*Euler's constant "gamma"*)
  GoldenRatio = FLOAT(1.618033988749894D0,T);  (*golden ratio*)
  DegPerRad   = FLOAT(57.29577951308232D0,T);  (*degrees per radian*)
  RadPerDeg   = FLOAT(0.017453292519943D0,T);  (*radians per degree*)

CONST
  Base            = R.Base;
  Precision       = R.Precision;
  MaxFinite       = R.MaxFinite;
  MinPos          = R.MinPos;
  MinPosNormal    = R.MinPosNormal;
  MaxExpDigits    = R.MaxExpDigits;
  MaxSignifDigits = R.MaxSignifDigits;

  (*---boundaries for precision testing---*)
  Tiny = R.MinPos*FLOAT(1000.0,T); (*nearly 0.0*)
  Huge = R.MaxFinite/FLOAT(1000.0,T); (*nearly infinite*)
  (*Eps  = Pow(FLOAT(R.Base,T),-FLOAT(R.Precision,T));  (*approx relative machine precision*)  *)
  (*Eps  = LongFloat.Scalb(One,-R.Precision);*)
  Eps  = RX.Eps;

<*INLINE*> PROCEDURE Abs   (c:T):T;       (*return magnitude*)
<*INLINE*> PROCEDURE AbsSqr(c:T):T;       (*return square of the magnitude*)

(*---- Exponential and Logarithm functions ----*)
<*INLINE*> PROCEDURE Exp   (x: T): T; (*returns e^x*)
<*INLINE*> PROCEDURE Expm1 (x: T): T; (*returns e^(x-1) *)
<*INLINE*> PROCEDURE Ln    (x: T): T; (*returns ln(x) *)
<*INLINE*> PROCEDURE Ln1p  (x: T): T; (*returns ln(1+x) *)
<*INLINE*> PROCEDURE Lb    (x: T): T; (*returns log2(x) *)
<*INLINE*> PROCEDURE Lg    (x: T): T; (*returns log10(x) *)
<*INLINE*> PROCEDURE Log   (x, y: T): T; (*returns log_y(x) *)
<*INLINE*> PROCEDURE Pow   (x, y: T): T; (*returns x^y *)
<*INLINE*> PROCEDURE SqRt  (x: T): T; (*returns square root of x*)

(*---- Trigonometric functions ----*)
<*INLINE*> PROCEDURE Cos     (x: T): T; (*returns the cosine of x radians. *)
<*INLINE*> PROCEDURE Sin     (x: T): T; (*returns the sine of x radians. *)
<*INLINE*> PROCEDURE Tan     (x: T): T; (*returns the tangent of x radians. *)
<*INLINE*> PROCEDURE ArcCos  (x: T): T; (*returns the arc cosine of x in radians. *)
<*INLINE*> PROCEDURE ArcSin  (x: T): T; (*returns the arc sine of x in radians. *)
<*INLINE*> PROCEDURE ArcTan  (x: T): T; (*returns the arc tangent of x in radians. *)
<*INLINE*> PROCEDURE ArcTan2 (y, x: T): T; (*returns the arc tangent of y/x in radians. *)

(*---- Hyperbolic trigonometric functions ----*)

<*INLINE*> PROCEDURE CosH   (x: T): T; (*returns the hyperbolic cosine of x. *)
<*INLINE*> PROCEDURE SinH   (x: T): T; (*returns the hyperbolic sine of x. *)
<*INLINE*> PROCEDURE TanH   (x: T): T; (*returns the hyperbolic tangent of x. *)
<*INLINE*> PROCEDURE ArCosH (x: T): T; (*returns the inverse hyperbolic cosine of x *)
<*INLINE*> PROCEDURE ArSinH (x: T): T; (*returns the inverse hyperbolic sine of x *)
<*INLINE*> PROCEDURE ArTanH (x: T): T; (*returns the inverse hyperbolic tangent of x *)

(*============================*)
(* Other Functions            *)
(*============================*)

<*INLINE*> PROCEDURE Sgn   (x: T): T; (*returns One if x is positive, MinusOne if x is negative, Zero if x is zero *)


(*==========================*)
END FloatTrans.
