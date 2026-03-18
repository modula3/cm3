(* Copyright (C) 2026 Peter McKinna. All rights reserved. *)
(* See file COPYRIGHT-BSD for details. *)

INTERFACE ExtendedRep;

(* This interface describes the layout of IEEE quad precision reals
   on little endian machines *)

TYPE
  T = RECORD 
    significand3 : BITS 32 FOR [-16_7fffffff-1 .. 16_7fffffff] := 0;
    significand2 : BITS 32 FOR [-16_7fffffff-1 .. 16_7fffffff] := 0;
    significand1 : BITS 32 FOR [-16_7fffffff-1 .. 16_7fffffff] := 0;
    significand0 : BITS 16 FOR [0..16_FFFF]                    := 0;
    exponent     : BITS 15 FOR [0..16_7FFF]                    := 0;
    sign         : BITS  1 FOR [0..1]                          := 0;
  END;

CONST
  NegInf = T { sign := 1, exponent := 16_7FFF };
  PosInf = T { sign := 0, exponent := 16_7FFF };
  Nan    = T { sign := 0, exponent := 16_7FFF, significand0 := 1 };

END ExtendedRep.
