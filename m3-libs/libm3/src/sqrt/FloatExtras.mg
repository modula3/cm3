(*
     FloatExtras.mg
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

(* Last modified on Thu Jan 26 14:19:55 PST 1995 by kalsow   *)
(*      modified on Sun Nov 14 12:56:32 PST 1993 by goldberg *)

GENERIC MODULE FloatExtras(Real, Float);

IMPORT FloatMode;
FROM FloatMode IMPORT RoundingMode, Flag;

CONST
  Zero = FLOAT(0.0, T);
  One  = FLOAT(1.0, T);
  Two  = FLOAT(2.0, T);

PROCEDURE RaiseInvalid (): T
  RAISES {FloatMode.Trap<*NOWARN*>} =
  BEGIN
    RETURN (ZeroFn() / ZeroFn());
  END RaiseInvalid;

PROCEDURE RaiseDivByZero (sign: [-1 .. 1]): T
  RAISES {FloatMode.Trap<*NOWARN*>} =
  BEGIN
    (* XXX: what if sign = 0? *)
    RETURN (FLOAT(sign, T) / ZeroFn());
  END RaiseDivByZero;

(* reduce temptation to compiler to evaluate Inf and Nan at compile time *)
PROCEDURE ZeroFn (): T =
  BEGIN
    RETURN (Zero);
  END ZeroFn;

(*
 *  Should really call dec to binary for next two routines, since standard
 *  requires it to deliver NaN to trap handler when too large to wrap around
 *)
PROCEDURE RaiseLargeOverflow (sign: [-1 .. 1] := 1): T
  RAISES {FloatMode.Trap} =
  BEGIN
    (*
     * In single precision, largest number that can be handled is
     * 1.111...  * 2^(maxexp + alpha) = 1.11...  * 2^(127 + 192) = 2^320
     *
     * In general, 2^(3*maxexp) is big enough.
     *)
    RETURN
      (FLOAT(sign, T) * Float.Scalb(One, 3 * Float.ILogb(Real.MaxFinite)));
  END RaiseLargeOverflow;

PROCEDURE RaiseLargeUnderflow (sign: [-1 .. 1] := 1): T
  RAISES {FloatMode.Trap} =
  BEGIN
    RETURN (FLOAT(sign, T) * Float.Scalb(
              One, 3 * Float.ILogb(Real.MinPosNormal)));
  END RaiseLargeUnderflow;

PROCEDURE Round (x: T): INTEGER =
  BEGIN
    CASE FloatMode.GetRounding() OF
    | RoundingMode.TowardMinusInfinity => RETURN (FLOOR(x));
    | RoundingMode.TowardPlusInfinity  => RETURN (CEILING(x));
    | RoundingMode.TowardZero          => RETURN (TRUNC(x));
    | RoundingMode.NearestElseEven,
      RoundingMode.NearestElseAwayFromZero, (*== VAX*)
      RoundingMode.IBM370,
      RoundingMode.Other               => RETURN (ROUND(x));
    END;
  END Round;

PROCEDURE SetBehaviors (f: SET OF Flag; b: FloatMode.Behavior)
  RAISES {FloatMode.Failure} =
  BEGIN
    FOR i := FIRST(Flag) TO LAST(Flag) DO
      IF i IN f THEN FloatMode.SetBehavior(i, b); END;
    END;
  END SetBehaviors;

(*
 * this should be implemented in terms of machine-dependent primitives,
 * to be more efficient.
 *)
PROCEDURE SetFlag (f: FloatMode.Flag) RAISES {FloatMode.Failure} =
  BEGIN
    WITH flgs = FloatMode.GetFlags() DO
      IF NOT f IN flgs THEN
        EVAL FloatMode.SetFlags(flgs + SET OF FloatMode.Flag{f});
      END;
    END
  END SetFlag;

PROCEDURE ToBinary (x: T): Binary RAISES {FloatMode.Trap} =
  VAR
    res: Binary;
    v  : T;
    buf: ARRAY [0 .. 55] OF [0 .. 1];
    i  : CARDINAL := 0;
  BEGIN
    IF x = Zero THEN
      res.exp := 0;
      res.bits := NEW(REF ARRAY OF [0 .. 1], 1);
      res.bits[0] := 0;
    ELSE
      res.exp := Float.ILogb(x);
      v := Float.Scalb(ABS(x), -res.exp);
      WHILE v # Zero DO
        IF v >= One THEN buf[i] := 1; v := v - One; ELSE buf[i] := 0; END;
        INC(i);
        v := Two * v;
      END;
      res.bits := NEW(REF ARRAY OF [0 .. 1], i);
      res.bits^ := SUBARRAY(buf, 0, i);
    END;
    RETURN res;
  END ToBinary;

BEGIN
END FloatExtras.
