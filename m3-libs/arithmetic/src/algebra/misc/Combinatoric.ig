GENERIC INTERFACE Combinatoric(R);
(*Copyright (c) 1996, m3na project

Abstract: Combinatoric operations

*)

(*==========================*)

TYPE
  T  = R.T;

(*number of permutations,
  e.g. there are Factorial(32) possibilities to order a cardset*)
PROCEDURE Factorial     (n    : T) : T;
(*number of permutations with partially irrelevant order
  permute n[0]+...+n[LAST(n)] elements,
  where the order of n[i] elements is irrelevant,
  e.g. there are Permutations(ARRAY OF T{2,10,10,10}) possibilities
  to set up a Skat game*)
PROCEDURE Permutations  (READONLY n : ARRAY OF T) : T;
PROCEDURE Arrangements  (n, k : T) : T;
PROCEDURE ArrangementsR (n, k : T) : T;
PROCEDURE Combinations  (n, k : T) : T;
PROCEDURE CombinationsR (n, k : T) : T;

(*==========================*)

END Combinatoric.
