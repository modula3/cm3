INTERFACE RandomSubtractiveFibo1;
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
  T <: RandomBasic.TReal;

PROCEDURE New():T;

(*==========================*)
END RandomSubtractiveFibo1.
