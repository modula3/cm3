(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Fri May  7 14:50:45 PDT 1993 by muller         *)
(*      modified on Wed Sep 25 00:47:04 1991 by kalsow         *)


INTERFACE FPU;

IMPORT Ctypes, Word;

(* This interface defines the DS3100 hardware (and libm.a) defined interface
   to floating-point values *)

(*------------------------------------------------- binary representation ---*)

TYPE
  RealRep = RECORD
    significand: BITS 23 FOR [0 .. 8*1024*1024-1];
    exponent:    BITS  8 FOR [0 .. 255];
    sign:        BITS  1 FOR [0..1];
  END;

CONST
  RealBias = 127;

TYPE
  LongRealRep = RECORD
    significand1 : BITS 32 FOR Word.T;
    significand0 : BITS 20 FOR [0..16_FFFFF];
    exponent     : BITS 11 FOR [0..16_7FF];
    sign         : BITS  1 FOR [0..1];
  END;

CONST
  LongRealBias = 1023;

TYPE
  ExtendedRep = LongRealRep;

CONST
  ExtendedBias = LongRealBias;

(*--------------------------------------------------- IEEE classification ---*)

TYPE
  FPClass = { SignalingNaN, QuietNaN,
              PosInfinity,  NegInfinity,
              PosNormal,    NegNormal,
              PosDenormal,  NegDenormal,
              PosZero,      NegZero };

<*EXTERNAL fp_class_d*> PROCEDURE LongClass (x: LONGREAL): Ctypes.int;
<*EXTERNAL fp_class_f*> PROCEDURE RealClass (x: REAL): Ctypes.int;
(* returns the IEEE defined class of its argument *)

(*----------------------------------------------- control/status register ---*)

TYPE
  Flag = BITS 1 FOR BOOLEAN;

TYPE
  ControlStatus = RECORD
    rounding_mode : BITS 2 FOR RoundingMode;

    (* "sticky" bits, only reset by writing the control register *)
    se_inexact   : Flag;
    se_underflow : Flag;
    se_overflow  : Flag;
    se_divide0   : Flag;
    se_invalid   : Flag;

    (* trap enable flags for the exceptions *)
    en_inexact   : Flag;
    en_underflow : Flag;
    en_overflow  : Flag;
    en_divide0   : Flag;
    en_invalid   : Flag;

    (* exceptions that occurred during the most recent instruction *)
    ex_inexact   : Flag;
    ex_underflow : Flag;
    ex_overflow  : Flag;
    ex_divide0   : Flag;
    ex_invalid   : Flag;
    ex_unimplemented : Flag;

    reserved1 : BITS 5 FOR [0..31];
    condition : Flag;  (* result of most recent compare instruction *)
    reserved0 : BITS 8 FOR [0..255];
  END;

TYPE
  RoundingMode = { ToNearest, ToZero, ToPlusInfinity, ToMinusInfinity };


<*EXTERNAL get_fpc_csr*> PROCEDURE GetStatus (): INTEGER(*ControlStatus*);
(* returns the current setting of the floating point control registers *)

<*EXTERNAL set_fpc_csr*> PROCEDURE SetStatus (new: INTEGER): INTEGER(*ControlStatus*);
(* sets the floating point control registers and returns their previous state*)

<*EXTERNAL swapRM*>PROCEDURE SetRounding(new: INTEGER):INTEGER(*RoundingMode*);
(* sets the rounding mode and returns its previous value *)

<*EXTERNAL swapINX*> PROCEDURE SetInexact (new: INTEGER): INTEGER(*BOOLEAN*);
(* sets the "sticky inexact bit" and returns its old value *)

(*--------------------------------------------- standard? IEEE operations ---*)

<*EXTERNAL isnan*> PROCEDURE IsNaN (x: LONGREAL): INTEGER (*BOOLEAN*);
(* return 1 if x is NaN, 0 otherwise. *)

<*EXTERNAL copysign*> PROCEDURE CopySign (x, y: LONGREAL): LONGREAL;
(* return 'x' with the sign of 'y'. *)

<*EXTERNAL drem*> PROCEDURE Remainder (x, y: LONGREAL): LONGREAL;
(* returns the remainder r := x - n*y  where n is the integer nearest the
   exact value of x/y.  Additionally if |n-x/y|=1/2, then n is even.
   Consequently the remainder is computed exactly and |r| < |y|/2.
   Remainder (x, 0.0) and Remainder (infinity, y) produce NaN. *)

<*EXTERNAL finite*> PROCEDURE IsFinite (x: LONGREAL): INTEGER (*BOOLEAN*);
(* = 1 if -infinity < x < +infinity, otherwise = 0 *)

<*EXTERNAL logb*> PROCEDURE BinaryLog (x: LONGREAL): LONGREAL;
(* for x finite, non-zero, and above the underflow threshold,
   returns the integer valued floating-point number n, such that
   1 < ABS (x) / (2^n) < 2.  Note that BinaryLog (+infinity) = +infinity,
   and BinaryLog (0) = -infinity (and causes a division-by-zero error). *)

<*EXTERNAL*> PROCEDURE scalb (x: LONGREAL;  n: INTEGER): LONGREAL;
(* returns x * (2^n) *)

<*EXTERNAL*> PROCEDURE sqrt (x: LONGREAL): LONGREAL;
(* returns sqrt (x) *)

END FPU.
