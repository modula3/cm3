(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Feb 25 12:11:33 PST 1994 by kalsow                   *)
(*      modified on Fri May  7 11:47:22 PDT 1993 by muller                   *)

INTERFACE RealRep;

(* This interface describes the layout of IEEE single precision reals
   on big endian machines *)

TYPE
  T = RECORD 
    sign:        BITS  1 FOR [16_0 .. 16_1]      := 0;
    exponent:    BITS  8 FOR [16_0 .. 16_ff]     := 0;
    significand: BITS 23 FOR [16_0 .. 16_7fffff] := 0;
  END;

CONST
  NegInf = T { sign := 1, exponent := 16_ff };
  PosInf = T { sign := 0, exponent := 16_ff };
  Nan    = T { sign := 0, exponent := 16_ff, significand := 1 };

END RealRep.
