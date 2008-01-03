GENERIC INTERFACE Combinatoric(R);
(* Arithmetic for Modula-3, see doc for details

   Abstract: Combinatoric operations *)


TYPE T = R.T;

PROCEDURE Factorial (n: T; ): T;
(* number of permutations, e.g.  there are Factorial(32) possibilities to
   order a cardset *)

PROCEDURE Permutations (READONLY n: ARRAY OF T; ): T;
(* number of permutations with partially irrelevant order permute
   n[0]+...+n[LAST(n)] elements, where the order of n[i] elements is
   irrelevant, e.g.  there are Permutations(ARRAY OF T{2,10,10,10})
   possibilities to set up a Skat game *)

PROCEDURE Arrangements (n, k: T; ): T;
(* choose k from n elements, each element at most once and the order is
   respected *)

PROCEDURE ArrangementsR (n, k: T; ): T;
(* choose k from n elements, each element can be chosen multiple times and
   the order is respected, e.g.  figures consisting of k digits to the base
   n *)

PROCEDURE Combinations (n, k: T; ): T;
(* choose k from n elements, each element at most once and the order is
   ignored, e.g.  the number of subsets of size k of a set of size n, or a
   lottery where 6 numbers in a 7x7 field can be marked *)

PROCEDURE CombinationsR (n, k: T; ): T;
(* choose k from n elements, each element can be chosen multiple times and
   the order is ignored *)

END Combinatoric.
