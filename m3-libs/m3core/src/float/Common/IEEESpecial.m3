(* Copyright (C) 1990, Digital Equipment Corporation.                        *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Feb 25 13:49:00 PST 1994 by kalsow                   *)
(*      modified on Wed Dec  8 16:57:44 PST 1993 by heydon                   *)
(* Jay Krell jay.krell@cornell.edu                                           *)

UNSAFE MODULE IEEESpecial;

IMPORT IEEE;

CONST Pack32 = IEEE.Pack32;
CONST Pack64 = IEEE.Pack64TwoPartSignificand;
TYPE Float32 = IEEE.Float32;
TYPE Float64 = IEEE.Float64TwoPartSignificand;

BEGIN
  (* See RealRep, LongRealRep in ieee-le, ieee-be directories.
   * The information is duplicated to produce endian neutral C++ bootstraps.
   *)
  RealNegInf := Pack32 (Float32 { sign := 1, exponent := 16_FF });
  RealPosInf := Pack32 (Float32 { sign := 0, exponent := 16_FF });
  RealNan    := Pack32 (Float32 { sign := 0, exponent := 16_FF, significand := 1 });

  LongNegInf := Pack64 (Float64 { sign := 1, exponent := 16_7FF });
  LongPosInf := Pack64 (Float64 { sign := 0, exponent := 16_7FF });
  LongNan    := Pack64 (Float64 { sign := 0, exponent := 16_7FF, significand0 := 1 });

  ExtdNegInf := LOOPHOLE (LongNegInf, EXTENDED);
  ExtdPosInf := LOOPHOLE (LongPosInf, EXTENDED);
  ExtdNan    := LOOPHOLE (LongNan, EXTENDED);
END IEEESpecial.
