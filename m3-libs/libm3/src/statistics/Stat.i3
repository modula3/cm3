(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Mon Aug 26 22:55:30 1991 by kalsow         *)
(*      modified on Thu Nov  2 18:11:42 1989 by muller         *)

(*      modified on Thu Aug  4 08:14:28 1988 by stolfi         *)
(*      modified on Fri Jun 10 14:23:17 PDT 1988 by glassman   *)

INTERFACE Stat;

(* Tools for elementary statistics of a sequence of real quantities
    A tiny set of tools for collecting elementary statistics (count,
    maximum, minimum, mean, and variance) of a sequence of real quantities.
    Index: statistics, collecting; printing, statistics
*)

IMPORT Wr, Thread;

TYPE
  T = RECORD (* A statistics accumulator *)
        num      : LONGREAL;
        mean     : LONGREAL;
        variance : LONGREAL;
        maximum  : REAL;
        minimum  : REAL;
      END;

  (* Typical usage:
  |    VAR st: Stat.T;
  |    BEGIN
  |      Stat.Init(st);
  |      LOOP
  |        weight := ...
  |        Stat.Accum(st, weight);
  |        ...
  |      END;
  |      Wr.PrintF(stderr, "Weight statistics: ");
  |      Stat.Print(stderr, st);
  |      Wr.PrintF(stderr, "\n");
  |    END;
  The mean is arithmetic mean.  The variance is the sum of of
  (x - mean)**2, divided by num (NOT num-1).  Thus, the standard
  deviation is sqrt(variance*num/(num-1)).  *)


(**********************************************************)
(*                                                        *)
(* GATHERING:                                             *)
(*                                                        *)
(**********************************************************)

PROCEDURE Init (VAR s: T);
(* Resets the statistics accumulator s *)

PROCEDURE Accum (VAR s: T; x: REAL);
(* Accumulates another observation x into the statistics accumulator s *)

PROCEDURE Combine (READONLY r, s: T): T;
(* Combines statistics of two disjoint samples into a single sample *)

(**********************************************************)
(*                                                        *)
(* EXTRACTING:                                            *)
(*                                                        *)
(**********************************************************)

PROCEDURE Num (READONLY s: T): REAL;
(* The number of observations in s. Same as FLOAT(s.num). *)

PROCEDURE Max (READONLY s: T): REAL;
(* The maximum observed value. Same as s.maximum. *)

PROCEDURE Min (READONLY s: T): REAL;
(* The minimum observed value. Same as s.minimum. *)

PROCEDURE Mean (READONLY s: T): REAL;
(* The arithmetic mean. Same as FLOAT(s.mean). *)

PROCEDURE Var (READONLY s: T): REAL;
(* The variance, i.e. sum((x[i]-mean)^2)/num. Same as FLOAT(s.variance). *)

PROCEDURE SDev (READONLY s: T): REAL;
(* The standard deviation, sqrt(sum((x[i]-mean)^2)/(num-1)). *)
(* Returns 0.0 if num < 2 *)

PROCEDURE RMS (READONLY s: T): REAL;
(* The root mean square value, sqrt(sum(x[i]^2)/num) *)

(**********************************************************)
(*                                                        *)
(* PRINTING:                                              *)
(*                                                        *)
(**********************************************************)

PROCEDURE Print (wr: Wr.T;  READONLY s: T) RAISES {Wr.Failure, Thread.Alerted};
(* Prints the text string
|     "num: %d  min: %g  max: %g  mean: %g  dev: %g"
   on the given writer.  If num=0, all other entries are omitted.
   The "dev" entry is the standard deviation, and is omitted if num<=1. *)

END Stat.

