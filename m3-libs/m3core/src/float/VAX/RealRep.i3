(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Feb 25 12:24:01 PST 1994 by kalsow                   *)
(*      modified on Fri May  7 11:47:22 PDT 1993 by muller                   *)

INTERFACE RealRep;

(* This interface describes the layout of VAX single precision reals. *)

TYPE
  T = RECORD (* "f" floating point *)
    fraction0 : BITS  7 FOR [0..63]         := 0;
    exponent  : BITS  8 FOR [0..255]        := 0;
    sign      : BITS  1 FOR [0..1]          := 0;
    fraction1 : BITS 16 FOR [0..64*1024-1]  := 0;
  END;

CONST
  ExponentBias = 128;

CONST
  NegInf = T { sign := 1, exponent := 255 };
  PosInf = T { sign := 0, exponent := 255 };
  Nan    = T { sign := 1, exponent := 0   };

END RealRep.
