INTERFACE LongRealEx;
(*Copyright (c) 1996, m3na project

Abstract: Some things that are forgotten in LongReal.i3

*)

IMPORT LongReal AS R;

(*==========================*)

CONST
  Eps = 1.0D-15;
(*
  Eps := RB.Scalb(One,-R.Precision);
*)

VAR
  CheckSignifBit : [17..17] := R.MaxSignifDigits;
(*
  If the compiler complains here,
  the definition has changed.
*)

(*==========================*)
END LongRealEx.
