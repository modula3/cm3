(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Feb 25 12:10:51 PST 1994 by kalsow                   *)
(*      modified on Fri May  7 15:57:56 PDT 1993 by muller                   *)

INTERFACE ExtendedRep;

(* This interface assumes EXTENDED is LONGREAL *) 

IMPORT LongRealRep;

TYPE
  T = LongRealRep.T;

CONST
  NegInf = T { sign := 1, exponent := 16_7FF };
  PosInf = T { sign := 0, exponent := 16_7FF };
  Nan    = T { sign := 0, exponent := 16_7FF, significand0 := 1 };

END ExtendedRep.
