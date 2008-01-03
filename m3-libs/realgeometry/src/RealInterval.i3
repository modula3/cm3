(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Contributed by Michel Dagenais (dagenais@vlsi.polymtl.ca), 1994. *)

(* An "Interval.T" is a contiguous set of REAL.  An interval "a"
   contains an integer "n" if

| a.lo <= n  AND  n < a.hi

   We impose the restriction that if an interval contains no REAL, 
   then it must be equal as a record to "Interval.Empty". *)

INTERFACE RealInterval;

TYPE T = RECORD lo, hi: REAL END;
TYPE Bound = {Lo, Hi};

CONST Empty = T { 0.0, 0.0 }; (* A point-like interval *)
CONST Full  = T {FIRST(REAL), LAST(REAL)}; (* The biggest interval *)

(* --- Initialization --- *)

PROCEDURE FromBounds(lo, hi: REAL): T;
(* If "lo >= hi" then return "Empty", else return "T{lo, hi}". *)

PROCEDURE FromAbsBounds(n, m: REAL): T;
(* Return "FromBounds(MIN(n,m), MAX(n,m))". *)

PROCEDURE FromBound(lo: REAL; s: REAL): T;
(* Return "FromBounds(lo, lo+s)". *)

PROCEDURE FromSize(s: REAL): T;
(* Return "FromBounds(0, s)". *)

PROCEDURE Center(READONLY a: T; n: REAL): T;
(* If "a" is empty then return "Empty", else return "b" such that
   "Size(b) = Size(a)" and "Middle(b) = n".  *)

(* --- Selection --- *)

PROCEDURE Size(READONLY a: T): REAL;
(* Return "a.hi - a.lo". *)

PROCEDURE Middle(READONLY a: T): REAL;
(* Return "(a.hi + a.lo) DIV 2". *)

PROCEDURE PickBound (READONLY a: T; n: REAL): Bound;
(* Return the bound of a closest to n (one of them if equidistant) *)

PROCEDURE Project(READONLY a: T; n: REAL): REAL;
(* Return the element of "a" that is closest to "n".  This
   is a checked runtime error if "a" is empty. *)

(* --- Transformation --- *)

PROCEDURE Move(READONLY a: T; n: REAL): T;
(* Return "FromBounds(a.lo+n, a.hi+n)". *)

PROCEDURE Inset(READONLY a: T; n: REAL): T;
(* If "a" is empty then return "Empty", else return 
   "FromBounds(a.lo + n, a.hi - n)". *)

PROCEDURE Change(READONLY a: T; dlo, dhi: REAL): T;
(* If "a" is empty then return "Empty", else return 
  "FromBounds(a.lo + dlo, a.hi + dhi)". *)

PROCEDURE MoveBound (x: Bound; READONLY a: T; dn: REAL): T;
(* If r is empty return empty, else add dn to the edge x of a *)

PROCEDURE Join(READONLY a, b: T): T;
(* Return the smallest interval containing both "a" and "b". *)

PROCEDURE Meet(READONLY a, b: T): T;
(* Return the largest interval contained in both of "a" and "b". *)

PROCEDURE Chop (READONLY a: T; n: REAL; VAR (* out *) b, c: T);
(* Chop an interval in two; b is to the left of c *)

TYPE Partition = ARRAY [0..2] OF T;

PROCEDURE Factor (READONLY a, by: T; VAR (*out*) f: Partition; dn: REAL)
 ;
(* a is partitioned into 3 pieces f[0]..f[2], where f[1] = Meet (a,by). The
   order of f is such that if i<j then f[i] translated by dn doesn't intersect
   f[j]. (Only the sign of dn affects the order, not its magnitude.) *)

PROCEDURE Mod(n: REAL; READONLY a: T): REAL;
(* Return the member of "a" whose distance from "n" is a multiple of 
   "Size(a)".  This is a checked runtime error if "a" is empty. *)

(* --- Test --- *)

PROCEDURE Equal (READONLY a, b: T): BOOLEAN;
(* Interval equality; all empty intervals are equal *)

PROCEDURE IsEmpty(READONLY a: T): BOOLEAN; 
(* Return whether "a" is empty. *)

PROCEDURE Member(n: REAL; READONLY a: T): BOOLEAN;
(* Return whether "n" is in "a". *)

PROCEDURE Overlap(READONLY a, b: T): BOOLEAN;
(* Return whether "a" and "b" have any element in common. *)

PROCEDURE Subset(READONLY a, b: T): BOOLEAN;
(* Return whether "a" is contained in "b". *)

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

PROCEDURE Hash (READONLY a: T): INTEGER;
(* == RETURN a suitable hash value  *)

END RealInterval.
