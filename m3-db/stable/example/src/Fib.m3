(* Copyright (C) 1995, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Thu Jan 19 13:58:12 PST 1995 by kalsow     *)

(* \subsection{Object Implementation} *)
MODULE Fib;

REVEAL T = Public BRANDED OBJECT

(* Before using the stable package, it is best to separate the
   stable and transient state of the object to be stabilized.
   For example, 
   "Fib" has two stable state variables.  These two integers
   are used to generate Fibonacci numbers.  They are initialized by
   "initS" and modified by "crank". *)
  first  : INTEGER;
  second : INTEGER;

(* "Fib" has one transient state variable.  This integer corresponds
   to the number of cranks during the current session.  "initT"
   initializes this variable. *)
  times  : INTEGER;

(* The entire implementation of this object is in this module. *)
  OVERRIDES
    init   := Init;
    initS  := InitStable;
    initT  := InitTransient;
    crank  := Crank;
    count  := Count;
  END;

(* "Init" is used to initialize the unstable version of the object.
   It circumvents the stubs from "stablegen" and calls "InitStable"
   and "InitTransient" separately.  The procedure calls "InitStable" 
   before calling "InitTransient", because (in general) the transient 
   state might depend on the stable state but not vice versa.
*)
PROCEDURE Init(fib : T; newFirst : INTEGER; newSecond : INTEGER) : T =
  BEGIN
    InitStable(fib, newFirst, newSecond);
    InitTransient(fib);
    RETURN fib;
  END Init;

(* "InitStable" initializes the two stable state variables.  For the
   standard Fibonacci sequence, set newFirst := 0 and newSecond := 1.
*)
PROCEDURE InitStable(fib : T; newFirst : INTEGER; newSecond : INTEGER) =
  BEGIN
    fib.first  := newFirst;
    fib.second := newSecond;
  END InitStable;

(* "InitTransient" always sets the number of cranks to zero. *)
PROCEDURE InitTransient(fib : T) =
  BEGIN
    fib.times := 0;
  END InitTransient;

(* "Crank" performs the Fibonacci operation and updates the current
   number of cranks.  For convenience, the second argument indicates
   the number of cranks to perform, allowing the calling module to
   avoid having to repeat calls to "Crank".
   This type of optimization is especially important
   for the stable object, because the stable version of "Crank" creates
   a log entry on disk every time the function is called.
*)
PROCEDURE Crank(fib : T; times : INTEGER := 1) : INTEGER =
  VAR temp : INTEGER;
  BEGIN
    FOR Count := 1 TO times DO
      temp := fib.second;
      fib.second := fib.first + temp;
      fib.first := temp;
    END;
    fib.times := fib.times + times;
    RETURN fib.second;
  END Crank;

(* "Count" returns the current number of cranks. *)
PROCEDURE Count(fib : T) : INTEGER =
  BEGIN
    RETURN fib.times;
  END Count;

BEGIN
END Fib.
