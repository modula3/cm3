(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Created September 1989 by Bill Kalsow                       *)
(* Based on Random.def by Mark R. Brown                        *)
(* Last modified on Tue Dec  7 17:33:37 PST 1993 by mcjones    *)
(*      modified on Thu Oct 21 08:12:21 PDT 1993 by kalsow     *)
(*      modified on Tue Jan 30 11:02:59 1990 by muller         *)
(*      modified on Thu Jan 25 21:30:53 PST 1990 by stolfi     *)

(* A "Random.T" (or just a generator) is a pseudo-random number
   generator.
   \index{pseudo-random number}
*)

INTERFACE Random;

TYPE
  T = OBJECT METHODS
    integer(min := FIRST(INTEGER);
      max := LAST(INTEGER)): INTEGER;
    real(min := 0.0e+0; max := 1.0e+0): REAL;
    longreal(min := 0.0d+0; max := 1.0d+0): LONGREAL;
    extended(min := 0.0x+0; max := 1.0x+0): EXTENDED;
    boolean(): BOOLEAN
  END;
  Default <: T OBJECT METHODS
    init(fixed := FALSE): Default
  END;
END Random.

(* Individual generators are unmonitored, and all the operations
   have side effects.
  
   The methods provided by a generator "rand" are:

   The call "rand.integer(a, b)" returns a uniformly distributed
   "INTEGER" in the closed interval "[a..b]".

   The call "rand.real(a, b)" returns a uniformly distributed "REAL"
   in the half-open interval "[a..b)".

   The call "longreal" and "extended" are like "real", but return
   values of the specified types.

   The call "rand.boolean()" returns a random "BOOLEAN" value.

   It is a checked runtime error if "min > max" on any call.

   "NEW(Default).init()" creates and initializes a generator (see
   below for implementation details).  If "fixed" is "TRUE", a
   predetermined sequence is used.  If "fixed" is "FALSE", "init"
   chooses a random seed in such a way that different sequences result
   even if "init" is called many times in close proximity.

\paragraph*{Example.} A good pseudo-random permutation of an array "a"
   can be generated as follows:

| WITH rand = NEW(Random.Default).init() DO
|   FOR i := FIRST(a) TO LAST(a) - 1 DO
|     WITH j = rand.integer(i, LAST(a)) DO
|       `Exchange "a[i]" and "a[j]"`
|     END
|   END
| END

\paragraph*{SRC Modula-3 implementation details.} The object returned
   by a call of "New(Default).init" uses an additive generator based
   on Knuth's Algorithm 3.2.2A (see \cite{Knuth:Vol2}).

*)
