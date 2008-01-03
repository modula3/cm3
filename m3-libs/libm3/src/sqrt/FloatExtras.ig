(*
     FloatExtras.ig
        Some useful routines for floating-point
        David Goldberg, Xerox PARC
        goldberg@parc.xerox.com
        November, 1993
*)

(* Copyright (c) 1993 Xerox Corporation.  All rights reserved.

   Use and copying of this software and preparation of derivative works
   based upon this software are permitted.  Any distribution of this
   software or derivative works must comply with all applicable United
   States export control laws.  This software is made available AS IS, and
   Xerox Corporation makes no warranty about the software, its performance
   or its conformity to any specification. *)

(* Last modified on Tue Mar  1 16:28:17 PST 1994 by kalsow   *)
(*      modified on Sun Nov 14 12:55:02 PST 1993 by goldberg *)

GENERIC INTERFACE FloatExtras(Real);

IMPORT FloatMode;

TYPE T = Real.T;

(*
 *  Contains miscellaneous functions useful for Float arithmetic
 *)

TYPE
  (* represents (bits[0] . bits[1] bits[2] ...) * 2^exp *)
  Binary = RECORD
             exp : INTEGER;
             bits: REF ARRAY OF [0..1];
           END;

PROCEDURE ToBinary (x: T): Binary RAISES {FloatMode.Trap};

(*
 * The following procedures raise an IEEE exception, and return the
 *  value prescribed by the standard.  Whether or not they raise a
 *  Modula-3 exception depends on FloatMode.GetBehavior()
 *)

(* returns NaN *)
PROCEDURE RaiseInvalid (): T RAISES {FloatMode.Trap};

(* returns inf *)
PROCEDURE RaiseDivByZero (sign: [-1 .. 1] := 1): T RAISES {FloatMode.Trap};

(*
 * Overflow trap handlers can correct for large overflows.  Call
 * RaiseLargeOverflow when the result is so large that the exponent
 * has wrapped around more than once.  When overflow handlers are enabled
 * this routine will call the handler with a NaN instead of the wrapped result
 *)
PROCEDURE RaiseLargeOverflow (sign: [-1 .. 1] := 1): T
  RAISES {FloatMode.Trap};

PROCEDURE RaiseLargeUnderflow (sign: [-1 .. 1] := 1): T
  RAISES {FloatMode.Trap};

PROCEDURE Round (x: T): INTEGER;
(* rounds according to the current rounding mode *)

PROCEDURE SetFlag (f: FloatMode.Flag) RAISES {FloatMode.Failure};
(* Set a flag.  Potentially more efficient than SetFlags(GetFlags() + SET
   OF Flag{f}) *)

PROCEDURE SetBehaviors (f: SET OF FloatMode.Flag; b: FloatMode.Behavior)
  RAISES {FloatMode.Failure};
(* set behavior for a set of flags *)

END FloatExtras.
