MODULE ExtendedBasicEx EXPORTS ExtendedBasic;
(*Copyright (c) 1996, m3na project

Abstract:

*)

IMPORT IEEESpecial AS RS;

(*==========================*)
BEGIN
  NegInf := RS.ExtdNegInf;
  PosInf := RS.ExtdPosInf;
  Nan    := RS.ExtdNan;
END ExtendedBasicEx.
