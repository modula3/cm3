GENERIC INTERFACE IntegerPower(R);
(*Copyright (c) 1996, m3na project

Abstract: Fast generic computation of a power with integer exponent

2/17/96  Harry George    Initial version
*)

(*==========================*)

TYPE
  T = R.T;
  PositiveInteger = [1..LAST(CARDINAL)];

PROCEDURE IntegerPower(x:T; y:PositiveInteger):T;
(*returns the power x^y with respect to the multiplication of R.
  This implementation needs only Log2(y) multiplications.*)

(*==========================*)
END IntegerPower.
