(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Wed May 12 12:07:01 PDT 1993 by swart      *)
(*      modified on Mon Nov 18 22:04:34 PST 1991 by gnelson    *)
(*      modified on Thu Nov  2 18:28:29 1989 by muller         *)
(*      modified on Mon Oct  2 09:19:13 1989 by kalsow         *)
(*      modified on Fri Jun 3 16:21:07 PDT 1988 by luca        *)

(* An "Interval.T" is a contiguous set of integers.  An interval "a"
   contains an integer "n" if

| a.lo <= n  AND  n < a.hi

   We impose the restriction that if an interval contains no integers, 
   then it must be equal as a record to "Interval.Empty". *)

INTERFACE Interval;

IMPORT Word;

TYPE T = RECORD lo, hi: INTEGER END;
TYPE Bound = {Lo, Hi};

CONST Empty = T { 0, 0 }; (* A point-like interval *)
CONST Full  = T {FIRST(INTEGER), LAST(INTEGER)}; (* The biggest interval *)

(* --- Initialization --- *)

PROCEDURE FromBounds(lo, hi: INTEGER): T;
(* If "lo >= hi" then return "Empty", else return "T{lo, hi}". *)

PROCEDURE FromAbsBounds(n, m: INTEGER): T;
(* Return "FromBounds(MIN(n,m), MAX(n,m))". *)

PROCEDURE FromBound(lo: INTEGER; s: CARDINAL): T;
(* Return "FromBounds(lo, lo+s)". *)

PROCEDURE FromSize(s: CARDINAL): T;
(* Return "FromBounds(0, s)". *)

PROCEDURE Center(READONLY a: T; n: INTEGER): T;
(* If "a" is empty then return "Empty", else return "b" such that
   "Size(b) = Size(a)" and "Middle(b) = n".  *)

(* --- Selection --- *)

PROCEDURE Size(READONLY a: T): CARDINAL;
(* Return "a.hi - a.lo". *)

PROCEDURE Middle(READONLY a: T): INTEGER;
(* Return "(a.hi + a.lo) DIV 2". *)

PROCEDURE PickBound (READONLY a: T; n: INTEGER): Bound;
(* Return the bound of a closest to n (one of them if equidistant) *)

PROCEDURE Project(READONLY a: T; n: INTEGER): INTEGER;
(* Return the element of "a" that is closest to "n".  This
   is a checked runtime error if "a" is empty. *)

(* --- Transformation --- *)

PROCEDURE Move(READONLY a: T; n: INTEGER): T;
(* Return "FromBounds(a.lo+n, a.hi+n)". *)

PROCEDURE Inset(READONLY a: T; n: INTEGER): T;
(* If "a" is empty then return "Empty", else return 
   "FromBounds(a.lo + n, a.hi - n)". *)

PROCEDURE Change(READONLY a: T; dlo, dhi: INTEGER): T;
(* If "a" is empty then return "Empty", else return 
  "FromBounds(a.lo + dlo, a.hi + dhi)". *)

PROCEDURE MoveBound (x: Bound; READONLY a: T; dn: INTEGER): T;
(* If r is empty return empty, else add dn to the edge x of a *)

PROCEDURE Join(READONLY a, b: T): T;
(* Return the smallest interval containing both "a" and "b". *)

PROCEDURE Meet(READONLY a, b: T): T;
(* Return the largest interval contained in both of "a" and "b". *)

PROCEDURE Chop (READONLY a: T; n: INTEGER; VAR (* out *) b, c: T);
(* Chop an interval in two; b is to the left of c *)

TYPE Partition = ARRAY [0..2] OF T;

PROCEDURE Factor (READONLY a, by: T; VAR (*out*) f: Partition; dn: INTEGER)
 ;
(* a is partitioned into 3 pieces f[0]..f[2], where f[1] = Meet (a,by). The
   order of f is such that if i<j then f[i] translated by dn doesn't intersect
   f[j]. (Only the sign of dn affects the order, not its magnitude.) *)

PROCEDURE Mod(n: INTEGER; READONLY a: T): INTEGER;
(* Return the member of "a" whose distance from "n" is a multiple of 
   "Size(a)".  This is a checked runtime error if "a" is empty. *)

(* --- Test --- *)

PROCEDURE Equal (READONLY a, b: T): BOOLEAN;
(* Interval equality; as all empty intervals must be represented
   as Empty, this is equivalent to a = b. *)

PROCEDURE IsEmpty(READONLY a: T): BOOLEAN; 
(* Return whether "a" is empty. *)

PROCEDURE Member(n: INTEGER; READONLY a: T): BOOLEAN;
(* Return whether "n" is in "a". *)

PROCEDURE Overlap(READONLY a, b: T): BOOLEAN;
(* Return whether "a" and "b" have any element in common. *)

PROCEDURE Subset(READONLY a, b: T): BOOLEAN;
(* Return whether "a" is contained in "b". *)

(* --- Standard type operations --- *)

PROCEDURE Compare (READONLY a, b: T): [-1 .. 1];
(* == RETURN 0 if Equal(a, b), -1 if (a.lo < b.lo) OR ((a.lo = b.lo) AND (a.hi < b.hi)), +1 o.  w.) *)

PROCEDURE Hash (READONLY a: T): Word.T;
(* == RETURN a suitable hash value *)

END Interval.
