(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue May 17 21:10:31 PDT 1994 by mhb                      *)
(*      modified on Tue Jun 16 16:46:26 PDT 1992 by muller                   *)

INTERFACE RealInterval;

(* Pair of REALs [lo,hi)

   An "interval" is a pair of REALs "lo" and "hi" with lo <= hi. It is
   meant to represent the left-closed, right-open segment [lo,hi), i.e. the
   set of REAL points between lo (included) and hi (excluded). Note: there
   are many intervals denoting the empty set.

   Index: geometry, intervals; intervals *)

(* NOTATION: A "null interval" is any pair of REALs (lo,hi) with lo >= hi.
   A "point-like" interval is any pair of REALs (lo,hi) with lo = hi. By
   "empty" we mean an unspecified point-like interval. By <lo,hi> we mean the
   pair (lo,hi) if lo<hi, empty otherwise. By |lo,hi| we mean the pair (lo,hi)
   if lo<hi, (hi,lo) if hi<lo, and empty if lo=hi. *)

(* LAWS: (1) Programs which assume or generate non-point-like null intervals
   are illegal. (2) All empty intervals are identified; programs which
   discriminate between different empty intervals are illegal. *)

IMPORT Interval;

TYPE T = RECORD lo, hi: REAL END;
TYPE Bound = {Lo, Hi};

EXCEPTION Error;

CONST Empty: T = T{ 0.0, -1.0 }; (* A point-like interval *)

(* --- Initialization --- *)

PROCEDURE FromBounds (lo, hi: REAL): T RAISES {};
(* Returns the interval <lo,hi> *)

PROCEDURE FromAbsBounds (lo, hi: REAL): T RAISES {};
(* Returns the interval |lo,hi| *)

PROCEDURE FromBound (lo: REAL; s: REAL): T RAISES {};
(* Returns the interval <lo,lo+s> *)

PROCEDURE FromSize (s: REAL): T RAISES {};
(* Returns the interval <0.0,s> *)

PROCEDURE Floor(a: T): Interval.T;
(* Rounds all points in the interval down to an integer *)

PROCEDURE Round(a: T): Interval.T;
(* Rounds all points in the interval to the nearest integer *)

PROCEDURE Center (READONLY a: T; b: REAL): T RAISES {};
(* Return an interval congruent to a, with middle at b *)

(* --- Selection --- *)

PROCEDURE Size (READONLY a: T): REAL RAISES {};
(* a.hi - a.lo *)

PROCEDURE Middle (READONLY a: T): REAL RAISES {};
(* floor ((a.hi+a.lo)/2.0); an unspecified REAL if empty *)

PROCEDURE PickBound (READONLY a: T; n: REAL): Bound RAISES {};
(* Return the bound of a closest to n (one of them if equidistant) *)

PROCEDURE Project (READONLY a: T; n: REAL): REAL RAISES {};
(* Return the REAL in a that is closest to n. a must be non empty *)

(* --- Transformation --- *)

PROCEDURE Move (READONLY a: T; n: REAL): T RAISES {};
(* The interval <a.lo+n,a.hi+n> *)

PROCEDURE Inset (READONLY a: T; n: REAL): T RAISES {};
(* If a is empty return empty, else return <a.lo+n, a.hi-n> *)

PROCEDURE Change (READONLY a: T; dlo, dhi: REAL): T RAISES {};
(* If a is empty return empty, else return <a.lo+dlo, a.hi+dhi> *)

PROCEDURE MoveBound (x: Bound; READONLY a: T; dn: REAL): T RAISES {};
(* If r is empty return empty, else add dn to the edge x of a *)

PROCEDURE Join (READONLY a, b: T): T RAISES {};
(* the least interval including the union of the points in a and b *)

PROCEDURE Meet (READONLY a, b: T): T RAISES {};
(* the largest interval included in the intersection of a and b *)

PROCEDURE Chop (READONLY a: T; n: REAL; VAR (* out *) b, c: T) RAISES {};
(* Chop an interval in two; b is to the left of c *)

TYPE Partition = ARRAY [0..2] OF T;

PROCEDURE Factor (READONLY a, by: T; VAR (*out*) f: Partition; dn: REAL)
  RAISES {};
(* a is partitioned into 3 pieces f[0]..f[2], where f[1] = Meet (a,by). The
   order of f is such that if i<j then f[i] translated by dn doesn't intersect
   f[j]. (Only the sign of dn affects the order, not its magnitude.) *)

PROCEDURE Mod (n: REAL; READONLY a: T): REAL RAISES {Error};
(* Return the member of a whose distance from n is a multiple of the size of
   a. a must be non-empty *)

(* --- Test --- *)

PROCEDURE Equal (READONLY a, b: T): BOOLEAN RAISES {};
(* Interval equality; all empty intervals are equal *)

PROCEDURE IsEmpty (READONLY a: T): BOOLEAN RAISES {}; (* Whether a is empty *)

PROCEDURE Member (n: REAL; READONLY a: T): BOOLEAN RAISES {};
(* whether an REAL is in an interval *)

PROCEDURE Overlap (READONLY a, b: T): BOOLEAN RAISES {};
(* whether two intervals overlap *)

PROCEDURE Subset (READONLY a, b: T): BOOLEAN RAISES {};
(* whether a is a subset of b *)

(* --- Standard type operations --- *)

PROCEDURE New (READONLY value: T): REF T;
(* Allocates and initializes a new heap value *)

PROCEDURE NewArray (size: CARDINAL;  READONLY value := Empty): REF ARRAY OF T;
(* Allocates a new array from the heap
  and initializes all its elements with the given value *)

PROCEDURE UntracedNew (READONLY value: T): UNTRACED REF T;
(* Allocates and initializes a new untraced value *)

PROCEDURE UntracedNewArray (size: CARDINAL;  READONLY value := Empty):
                                                       UNTRACED REF ARRAY OF T;
(* Allocates a new untraced array from the heap
  and initializes all its elements with the given value *)

PROCEDURE Compare (READONLY a, b: T): INTEGER;
(* == RETURN (-1 if Lt (a, b), 0 if Eq (a, b), +1 o. w.) *)

PROCEDURE Lt (READONLY a, b: T): BOOLEAN;
(* == RETURN (a.lo < b.lo) OR ((a.lo = b.lo) AND (a.hi < b.hi)) *)

PROCEDURE Eq (READONLY a, b: T): BOOLEAN;
(* == RETURN (a = b) *)

(* --- Useful hacks --- *)

PROCEDURE Float(a: Interval.T): T;
(* The set of all reals x such that x >= a.lo and x < a.hi. *)
(* Same as FromBounds(FLOAT(a.lo), FLOAT(a.hi) - epsilon) *)

PROCEDURE Hash (READONLY a: T): INTEGER;
(* == RETURN a suitable hash value  *)

END RealInterval.





