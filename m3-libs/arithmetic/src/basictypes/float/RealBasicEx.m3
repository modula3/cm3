MODULE RealBasicEx EXPORTS RealBasic;
(*Copyright (c) 1996, m3na project

Abstract: Some things that are forgotten in Real.i3

*)

IMPORT IEEESpecial AS RS;

(*==========================*)
BEGIN
  NegInf := RS.RealNegInf;
  PosInf := RS.RealPosInf;
  Nan    := RS.RealNan;
END RealBasicEx.
