INTERFACE RealEx;
(*Copyright (c) 1996, m3na project

Abstract: Some things that are forgotten in Real.i3

*)

IMPORT Real AS R;

(*==========================*)

CONST
  Eps = 1.0E-7;
(*
  Eps := RB.Scalb(One,-R.Precision);
*)

VAR
  CheckSignifBit : [9..9] := R.MaxSignifDigits;
(*
  If the compiler complains here,
  the definition has changed.
*)

(*==========================*)
END RealEx.
