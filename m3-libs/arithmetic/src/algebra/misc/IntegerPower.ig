GENERIC INTERFACE IntegerPower(R);
(*Arithmetic for Modula-3, see doc for details

   Abstract: Fast generic computation of a power with integer exponent *)

IMPORT NADefinitions AS NA;

(*==========================*)

TYPE
  T = R.T;
  PositiveInteger = [1 .. LAST(CARDINAL)];

PROCEDURE Power (x: T; y: PositiveInteger): T RAISES {NA.Error};
(* returns the power x^y with respect to the multiplication of R.  This
   implementation needs only Log2(y) multiplications.  Requires y>0 because
   R may not provide a unique One *)

PROCEDURE MulPower (x, y: T; z: CARDINAL): T RAISES {NA.Error};
(* returns the x*y^z with respect to the multiplication of R.  This is as
   fast as Power but works for z=0, too.*)

(*==========================*)
END IntegerPower.
