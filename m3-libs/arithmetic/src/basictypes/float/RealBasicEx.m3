MODULE RealBasicEx EXPORTS RealBasic;
(*Arithmetic for Modula-3, see doc for details

Abstract: Some things that are forgotten in Real.i3

*)

IMPORT IEEESpecial AS RS;

(*==========================*)
BEGIN
  NegInf := RS.RealNegInf;
  PosInf := RS.RealPosInf;
  Nan    := RS.RealNan;
END RealBasicEx.
