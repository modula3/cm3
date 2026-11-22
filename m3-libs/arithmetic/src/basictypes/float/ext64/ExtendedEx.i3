INTERFACE ExtendedEx;
(* Arithmetic for Modula-3, see doc for details

   Abstract: Some things that are forgotten in Extended.i3 *)

IMPORT Extended AS R;


CONST Eps = 1.0X-15;
(*
  Eps := RB.Scalb(One,-R.Precision);
*)

CONST CheckSignifBit: [17 .. 17] = R.MaxSignifDigits;
(*
  If the compiler complains here,
  the definition has changed.
*)

END ExtendedEx.
