(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jul  6 13:41:43 PDT 1995 by heydon                   *)
(*      modified on Fri Jun 17 11:17:47 PDT 1994 by kalsow                   *)
(*      modified on Thu May  6 16:25:33 PDT 1993 by muller                   *)

INTERFACE DragonInt;

(* Internal computations of Dragon require the use of bignums.
   Also, the fraction of a floating-point number can occupy more bits
   than there are in a Word.T.  We use an ad-hoc implementation of
   bignums, because only few of the arithmetic operations are needed,
   and only positive numbers are needed.

   Except for Zero and One, it is an unchecked runtime error to pass
   a DragonInt.T, x, to a method of any DragonInt.Session other than
   the one that returned x.
*)

TYPE
  Session <: REFANY;

PROCEDURE NewSession (): Session;
(* allocates, initializes, and returns a fresh session. *)

PROCEDURE EndSession (s: Session);
(* called at the end of a session for statistics gathering. *)
   
TYPE (*OPAQUE*)
  T = RECORD s, w: INTEGER; END;
  RefInt = REF ARRAY OF INTEGER;
  Int32 = [-16_7FFFFFFF-1 .. 16_7FFFFFFF];  
  RefInt32 = REF ARRAY OF Int32;
  
CONST
  Zero = T {0, 0};
  One  = T {1, 1};

PROCEDURE New (s: Session;  a, b: INTEGER): T;
(* returns a T representing the 64-bit value:
      (a & 16_ffffffff)<<32 + (b & 16_ffffffff) *)

PROCEDURE NewFromArr (s: Session; in : RefInt): T;
(* returns a T representing the array in of 32 bit integers *)

PROCEDURE copy (s: Session;  READONLY a: T): T;
(* returns a copy of a *)

PROCEDURE add (s: Session;  READONLY a,b: T): T;
(* returns the sum of a and b *)

PROCEDURE diff (s: Session;  READONLY a,b: T): T;
(* returns a - b; b is assumes a >= b *)

PROCEDURE mult (s: Session; READONLY a,b: T) : T;
(* returns a * b *)

PROCEDURE compare (s: Session;  READONLY a, b: T): [-1..1];
(* returns -1, 0, or 1 when a < b, a = b, or a > b *)

PROCEDURE max (s: Session;  READONLY a, b: T): T;
(* returns the biggest of a and b *)

PROCEDURE shift (s: Session;  READONLY a: T; n: INTEGER): T;
(* returns a shifted n bits, to the left for n>0, to the right for n<0
    == returns (a * 2^n)  *)

PROCEDURE times2 (s: Session;  READONLY a: T): T;
(* returns 2 * a *)

PROCEDURE timesTenInPlace (s: Session;  VAR a: T): T;
(* modifies a to be 10 * a and returns it *)

PROCEDURE divideTen (s: Session;  READONLY a: T): T;
(* returns a / 10 *)

PROCEDURE divmod (s: Session;  READONLY a, b : T; VAR d: INTEGER): T;
(* assume that a = d * b + res, with d a number between 0 and 9;
   computes d and returns res. *)

PROCEDURE ToArr32(s : Session; a : T; VAR out : RefInt32; VAR len : INTEGER);
(* returns a as an array of 32 bit integers *)


(*----- debugging stats ---*)
VAR
  maxHighWater := 0;
  highWater    : ARRAY [0..100] OF CARDINAL;
  allocates    : ARRAY [0..100] OF CARDINAL;
  nSessions    := 0;
  nAllocates   := 0;

END DragonInt.
