(* Copyright 1989 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Created by stolfi on Thu Jan 26 13:50:19 1989               *)
(* Last modified on Tue Jul 21 16:10:38 PDT 1992 by harrison   *)
(*      modified on Thu Oct 18 20:45:59 PDT 1990 by stolfi     *)

INTERFACE Fuzzy;

(*  Interval arithmetic.

    This interface defines the basic types procedures for interval
    arithmetic. A Fuzzy.T is a closed interval [lo _ hi] of REAL
    numbers (or the empty set).  An interval operation F(I1, I2, ...)
    returns a (hopefully small) interval that contains the result
    of F(x1, x2, ...) for all possible choices of xi in each Ii.

    Index: numerical routines; interval arithmetic; fuzzy arithmetic

*)

(*  NOTE: The routines below (and those in companion interfaces)
    try their best to compensate for floating point rounding errors,
    but do not always succeed.  Clients can compensate for that by
    artificially widening the intervals that are passed to and/or
    returned by these procedures. *)

TYPE
  T = RECORD lo, hi: REAL END;
     (* 
       Represents the set of real numbers between "lo" and "hi", inclusive;
       or the empty set if "lo > hi". *)

  Pair = ARRAY [0..1] OF T;

(* --- Basics --- *)

CONST Empty = T{1.0, 0.0}; (* The empty set *)
      Full = T{FIRST(REAL), LAST(REAL)}; (* The full set *)

(* --- Test --- *)

PROCEDURE IsEmpty(READONLY a: T): BOOLEAN; (* Whether a is empty *)

PROCEDURE Member(n: REAL; READONLY a: T): BOOLEAN;
  (* whether a real is in an interval *)

PROCEDURE Overlap(READONLY a, b: T): BOOLEAN;
  (* whether two intervals overlap *)

PROCEDURE Subset(READONLY a, b: T): BOOLEAN;
  (* wheter a is a subset of b *)

(* --- Transformation --- *)

PROCEDURE Scale (s: REAL; READONLY a: T): T ;
  (* Multiplies all points in "a" by the number "s" *)

PROCEDURE Shift (s: REAL; READONLY a: T): T ;
  (* Increments all points in "a" by the number "s" *)

PROCEDURE Expand (s: REAL; READONLY a: T): T;
  (* Equivalent to Join(Fuzzy.T{s,s}, a) *)

PROCEDURE Add (READONLY a, b: T): T ;
  (* Addition *)

PROCEDURE Sub (READONLY a, b: T): T ;
  (* Subtraction *)

PROCEDURE Mul (READONLY a, b: T): T ;
  (* Multiplication *)

PROCEDURE Sqr (READONLY a: T): T ;
  (* Squaring. FAster, and sometimes tighter than Mul(a,a) *)

PROCEDURE Minus (READONLY a: T): T;
  (* Negates all points in "a". *)

PROCEDURE ClipHi (READONLY a: T; hi: REAL): T;
  (* Chops the top of "a" at "hi". No-op if hi >= a.hi. *)

PROCEDURE ClipLo (READONLY a: T; lo: REAL): T;
  (* Chops the bottom of "a" at "lo". No-op if lo <=  a.lo. *)

PROCEDURE Sqrt (READONLY a: T): T ;
  (* Set square root. Requires a.lo >= 0.0 *)

PROCEDURE Join (READONLY a, b: T): T;
  (* Set union *)

PROCEDURE Meet (READONLY a, b: T): T;
  (* Set intersection *)

PROCEDURE Inset(READONLY a: T; amount: REAL): T ;
  (* Moves both endpoints of "a" inwards by the given amount
    (wich may be negative). *)

PROCEDURE InsetPair(READONLY a: Pair; amount: REAL): Pair ;
  (* Insets a[0], a[1] by the given amount. *)

PROCEDURE Fudge (READONLY a: T; f: REAL := 0.0001): T;
  (* Widens "a" by "f" times its largest point *)

PROCEDURE ToText(READONLY a: T): TEXT;

END Fuzzy.

