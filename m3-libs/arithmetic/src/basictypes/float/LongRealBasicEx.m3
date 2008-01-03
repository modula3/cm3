MODULE LongRealBasicEx EXPORTS LongRealBasic;
(* Arithmetic for Modula-3, see doc for details *)

IMPORT IEEESpecial AS RS;

BEGIN
  NegInf := RS.LongNegInf;
  PosInf := RS.LongPosInf;
  Nan := RS.LongNan;
END LongRealBasicEx.
