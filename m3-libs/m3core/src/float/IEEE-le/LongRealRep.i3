(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Feb 25 12:10:51 PST 1994 by kalsow                   *)
(*      modified on Fri May  7 15:57:56 PDT 1993 by muller                   *)

INTERFACE LongRealRep;

(* This interface describes the layout of IEEE double precision reals
   on little endian machines *)

TYPE
  T = RECORD 
    significand1 : BITS 32 FOR [-16_7fffffff-1 .. 16_7fffffff] := 0;
    significand0 : BITS 20 FOR [0..16_FFFFF]                   := 0;
    exponent     : BITS 11 FOR [0..16_7FF]                     := 0;
    sign         : BITS  1 FOR [0..1]                          := 0;
  END;

CONST
  NegInf = T { sign := 1, exponent := 16_7FF };
  PosInf = T { sign := 0, exponent := 16_7FF };
  Nan    = T { sign := 0, exponent := 16_7FF, significand0 := 1 };

END LongRealRep.
