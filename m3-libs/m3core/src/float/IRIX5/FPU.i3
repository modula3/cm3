(* Copyright (C) 1992, Xerox                                                 *)
(* All rights reserved.                                                      *)

(* Last modified on Wed Oct  5 15:42:58 PDT 1994 by ericv                    *)
(*      modified on Tue Sep 21 15:40:33 PDT 1993 by kalsow                   *)
(*      modified on Fri May  7 14:51:18 PDT 1993 by muller                   *)
(*      modified on Wed Sep 25 00:33:01 1991 by goldberg@xerox.parc.com      *)

INTERFACE FPU;

(*--------------------------------------------------- IEEE classification ---*)

TYPE
  FPClass = { SignalingNaN, QuietNaN,
              PosInfinity,  NegInfinity,
              PosNormal,    NegNormal,
              PosDenormal,  NegDenormal,
              PosZero,      NegZero };

<*EXTERNAL fpclass*> PROCEDURE LongClass (x: LONGREAL): INTEGER;
(* returns the IEEE defined class of its argument -- from ieeefp.h *)

(*----------------------------------------------- control/status register ---*)

(* From sys/fpu.h *)

TYPE
  Flag = BITS 1 FOR BOOLEAN;

TYPE
  ControlStatus = RECORD
    reserved0 : BITS 7 FOR [0..127];
    flush     : Flag;
    condition : Flag;  (* result of most recent compare instruction *)
    reserved1 : BITS 5 FOR [0..31];

    (* exceptions that occurred during the most recent instruction *)
    ex_unimplemented : Flag;
    ex_invalid   : Flag;
    ex_divide0   : Flag;
    ex_overflow  : Flag;
    ex_underflow : Flag;
    ex_inexact   : Flag;

    (* trap enable flags for the exceptions *)
    en_invalid   : Flag;
    en_divide0   : Flag;
    en_overflow  : Flag;
    en_underflow : Flag;
    en_inexact   : Flag;

    (* "sticky" bits, only reset by writing the control register *)
    se_invalid   : Flag;
    se_divide0   : Flag;
    se_overflow  : Flag;
    se_underflow : Flag;
    se_inexact   : Flag;

    rounding_mode : BITS 2 FOR RoundingMode;
  END;

TYPE
  RoundingMode = { ToNearest, ToZero, ToPlusInfinity, ToMinusInfinity };


<*EXTERNAL get_fpc_csr*>
PROCEDURE GetStatus (): INTEGER(*ControlStatus*);
(* returns the current setting of the floating point control registers *)

<*EXTERNAL set_fpc_csr*>
PROCEDURE SetStatus (new: INTEGER): INTEGER(*ControlStatus*);
(* sets the floating point control registers and returns their previous state*)

(* From <ieeefp.h> *)

<*EXTERNAL fpgetround*>
PROCEDURE GetRounding (): INTEGER(*RoundingMode*);
(* returns the current rounding mode *)

<*EXTERNAL fpsetround*>
PROCEDURE SetRounding(new: INTEGER): INTEGER(*RoundingMode*);
(* sets the rounding mode and returns its previous value *)

(* From <math.h> *)

<*EXTERNAL isnan*> PROCEDURE IsNaN (x: LONGREAL): BOOLEAN;
(* return 1 if x is NaN, 0 otherwise. *)

<*EXTERNAL copysign*> PROCEDURE CopySign (x, y: LONGREAL): LONGREAL;
(* return 'x' with the sign of 'y'. *)

<*EXTERNAL drem*> PROCEDURE Remainder (x, y: LONGREAL): LONGREAL;
(* returns the remainder r := x - n*y  where n is the integer nearest the
   exact value of x/y.  Additionally if |n-x/y|=1/2, then n is even.
   Consequently the remainder is computed exactly and |r| < |y|/2.
   Remainder (x, 0.0) and Remainder (infinity, y) produce NaN. *)

<*EXTERNAL finite*> PROCEDURE IsFinite (x: LONGREAL): BOOLEAN;
(* = 1 if -infinity < x < +infinity, otherwise = 0 *)

<*EXTERNAL logb*> PROCEDURE BinaryLog (x: LONGREAL): LONGREAL;
(* for x finite, non-zero, and above the underflow threshold,
   returns the integer valued floating-point number n, such that
   1 < ABS (x) / (2^n) < 2.  Note that BinaryLog (+infinity) = +infinity,
   and BinaryLog (0) = -infinity (and causes a division-by-zero error). *)

<*EXTERNAL ldexp*> PROCEDURE scalb (x: LONGREAL;  n: INTEGER): LONGREAL;
(* returns x * (2^n) *)

<*EXTERNAL*> PROCEDURE sqrt (x: LONGREAL): LONGREAL;
(* returns sqrt (x) *)

END FPU.
