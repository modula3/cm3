GENERIC INTERFACE IntegerPower(R);
(*Copyright (c) 1996, m3na project

   Abstract: Fast generic computation of a power with integer exponent *)

(*==========================*)

TYPE
  T = R.T;
  PositiveInteger = [1 .. LAST(CARDINAL)];

PROCEDURE Power (x: T; y: PositiveInteger): T;
(* returns the power x^y with respect to the multiplication of R.  This
   implementation needs only Log2(y) multiplications.  Requires y>0 because
   R may not provide a unique One *)

PROCEDURE MulPower (x, y: T; z: CARDINAL): T;
(* returns the x*y^z with respect to the multiplication of R.  This is as
   fast as Power but works for z=0, too.*)

(*==========================*)
END IntegerPower.
