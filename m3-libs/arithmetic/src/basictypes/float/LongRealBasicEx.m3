MODULE LongRealBasicEx EXPORTS LongRealBasic;
(*Copyright (c) 1996, m3na project

Abstract:

*)

IMPORT IEEESpecial AS RS;

(*==========================*)
BEGIN
  NegInf := RS.LongNegInf;
  PosInf := RS.LongPosInf;
  Nan    := RS.LongNan;
END LongRealBasicEx.
