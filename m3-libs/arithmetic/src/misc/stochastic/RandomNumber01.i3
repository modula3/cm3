INTERFACE RandomNumber01;
(*Copyright (c) 1996, m3na project

Abstract: Collection of random number generators
Each returns numbers in range:
     RandomBasic.Min..RandomBasic.Max
which is 0..1, but not including the endpoints.

3/16/96  Harry George  Initial version
*)
IMPORT RandomBasic;

(*==========================*)
TYPE
  DECSRC <: RandomBasic.T;  (*wrapper for DEC SRC Random.Default*)
  ran0   <: RandomBasic.T;  (*inspired by NR92 ran0*)
  ran1   <: RandomBasic.T;  (*inspired by NR92 ran1*)

(*==========================*)
END RandomNumber01.
