INTERFACE ExtendedEx;
(*Copyright (c) 1996, m3na project

Abstract: Some things that are forgotten in Extended.i3

*)

IMPORT Extended AS R;

(*==========================*)

CONST
  Eps = 1.0X-15;
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
END ExtendedEx.
