MODULE ExtendedBasicEx EXPORTS ExtendedBasic;
(*Arithmetic for Modula-3, see doc for details

Abstract:

*)

IMPORT IEEESpecial AS RS;

(*==========================*)
BEGIN
  NegInf := RS.ExtdNegInf;
  PosInf := RS.ExtdPosInf;
  Nan    := RS.ExtdNan;
END ExtendedBasicEx.
