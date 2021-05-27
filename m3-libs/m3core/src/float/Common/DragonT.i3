(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jun 17 10:40:07 PDT 1994 by kalsow                   *)
(*      modified on Fri May  7 17:17:14 PDT 1993 by muller                   *)

INTERFACE DragonT;

IMPORT Ctypes;

TYPE 
  CutoffMode = {normal, absolute, relative};
  Digit = [0 .. 9];

PROCEDURE F (e              : INTEGER;
             f3, f2, f1, f0 : INTEGER;
             p              : INTEGER;
             cutoffMode     : CutoffMode;
             cutoffPlace    : INTEGER;
  VAR(*OUT*) digits         : ARRAY OF Digit;
  VAR(*OUT*) count          : CARDINAL;
  VAR(*OUT*) exp            : INTEGER);
(* Implements the "Dragon" algorithm to print floating-point numbers.
   For REAL and LONGREAL set f2 and f3 to 0
   The 64 mantissa bits are ((f1 & 16_ffffffff) << 32) + (f0 & 16_ffffffff) 
   For Quad precision EXTENDED
   The 128 mantissa bits are ((f3 & 16_ffffffff) << 96) + (f2 & 16_ffffffff << 64) +                 
                             ((f1 & 16_ffffffff) << 32) + (f0 & 16_ffffffff) *)
END DragonT.

(*
   The Dragon algorithm is described in:

|        "How to Print Floating-Point Numbers Accurately",
|        Guy L. Steele Jr. and Jon L. White, 
|        Proceedings of the ACM Sigplan'90 conference on Programming 
|        Language Design and Implementation, 
|        pp 112-126.

*)
