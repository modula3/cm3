(* Copyright (C) 1990, Digital Equipment Corporation.                        *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Feb 25 13:49:00 PST 1994 by kalsow                   *)
(*      modified on Wed Dec  8 16:57:44 PST 1993 by heydon                   *)

UNSAFE MODULE IEEESpecial;

IMPORT RealRep, LongRealRep;

BEGIN
  LOOPHOLE (RealNegInf, RealRep.T) := RealRep.NegInf;
  LOOPHOLE (RealPosInf, RealRep.T) := RealRep.PosInf;
  LOOPHOLE (RealNan,    RealRep.T) := RealRep.Nan;

  LOOPHOLE (LongNegInf, LongRealRep.T) := LongRealRep.NegInf;
  LOOPHOLE (LongPosInf, LongRealRep.T) := LongRealRep.PosInf;
  LOOPHOLE (LongNan,    LongRealRep.T) := LongRealRep.Nan;

  LOOPHOLE (ExtdNegInf, LongRealRep.T) := LongRealRep.NegInf;
  LOOPHOLE (ExtdPosInf, LongRealRep.T) := LongRealRep.PosInf;
  LOOPHOLE (ExtdNan,    LongRealRep.T) := LongRealRep.Nan;
END IEEESpecial.
