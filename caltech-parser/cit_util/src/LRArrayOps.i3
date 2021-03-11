(* $Id$ *)

INTERFACE LRArrayOps;

PROCEDURE Sort(VAR a : ARRAY OF LONGREAL);

PROCEDURE Percentile(READONLY a : ARRAY OF LONGREAL; p : LONGREAL) : LONGREAL;
  (* a must be sorted *)

PROCEDURE Interpolate(READONLY a : ARRAY OF LONGREAL; idx : LONGREAL) : LONGREAL;

PROCEDURE TrimmedMean(READONLY a : ARRAY OF LONGREAL;
                      weight : LONGREAL) : LONGREAL;
  (* a must be sorted.

     TrimmedMean(a,w) returns the mean of the central w proportion of a.
     0 < w <= 1 
  *)

END LRArrayOps.
