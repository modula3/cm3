(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Created September 1989 by Bill Kalsow                       *)
(* Based on RandPerm.def by Mark Manasse                       *)
(* Last modified on Fri May 21 09:50:57 PDT 1993 by swart      *)
(*      modified on Mon Feb  8 09:22:45 PST 1993 by kalsow     *)
(*      modified on Tue Aug 18 14:42:24 PDT 1992 by mcjones    *)
(*      modified on Thu Jan 25 21:34:26 PST 1990 by stolfi     *)
(*      modified on Thu Nov  2 18:28:01 1989 by muller         *)

(* A "RandomPerm.T" (hereinafter a permutation) represents a
   pseudo-random permutation of a finite set of integers "[0..n-1]",
   that is a bijective (one-one and onto) map from the set "[0..n-1]"
   to itself.
   
   Formally, a permutation "p" has the components:

|  size(p)  `a nonnegative integer`
|  perm(p)  `a permutation of the integers "[0..size(p)-1]"`
|  index(p) `an integer in the range "[0..size(p)]"`

  It is up to the client to serialize access by multiple threads to
  a permutation; the results of concurrent access are undefined.

*)

INTERFACE RandomPerm;

IMPORT Random;

EXCEPTION Exhausted;

TYPE
  T = OBJECT
      METHODS
        size (): CARDINAL;
        (* Returns "size(p)", the number of elements in the permutation. *)

        index (): CARDINAL;
        (* Returns "index(p)", the index of the next element in the
           permutation. *)

        copy (): T;
        (* Returns a new permutation "q" with:

|  size(q)  = size(p)
|  perm(q)  = perm(p)
|  index(q) = index(p)

           and the same allocation type as p. *)

        next (): CARDINAL RAISES {Exhausted};
        (* Returns the next element of the permutation "p".  "next(p)" is
           equivalent to:

|   IF index(p)=size(p) THEN
|     index(p) := 0; RAISE Exhausted
|   END;
|   WITH n = perm(p)(index(p)) DO
|     index(p) := index(p)+1;
|     RETURN n
|   END

           *)
      END;

TYPE
  LowQuality <:
    T OBJECT METHODS init (n: CARDINAL; r: Random.T): LowQuality; END;

  HighQuality <:
    T OBJECT METHODS init (n: CARDINAL; r: Random.T): HighQuality; END;

(* The init methods initialize the type to a permutation "p" with
   "size(p)=n", "index(p)=0", and "perm(p)" containing a pseudo-random
   permutation that depends on "r" and the subtype. *)

(* "HighQuality" permutations use Algorithm 3.4.2P of Knuth's {\it
    Seminumerical Algorithms} (second edition), and thus require space
    "O(n)".  "LowQuality" permutations are not really very random but
    use constant space and work for "n" up to "2^(BITSIZE(INTEGER)-2)-1".  *)

PROCEDURE Fill(VAR(*OUT*) perm: ARRAY OF CARDINAL; r: Random.T);
(* Fill "perm" with a new high quality permutation of the integers
   "[0..NUMBER(perm)-1]".  *)

END RandomPerm.
