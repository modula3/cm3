GENERIC INTERFACE IntegerBasic(I,It);
(*Copyright (c) 1996, m3na project

Abstract: Generic computations on integer types

2/17/96  Harry George    Initial version
*)


(*==========================*)

TYPE T = I.T;

CONST
  Zero        =  0;
  One         =  1;
  Two         =  2;
  MinusOne    = -1;

CONST
  Equal   = It.Equal;
  Compare = It.Compare;

<*INLINE*> PROCEDURE Add(x,y:T):T;  (*return x+y*)
<*INLINE*> PROCEDURE Sub(x,y:T):T;  (*return x-y*)
<*INLINE*> PROCEDURE Neg(x:T):T;    (*return -x *)

<*INLINE*> PROCEDURE Mul(x,y:T):T;  (*return x*y*)
<*INLINE*> PROCEDURE Div(x,y:T):T;  (*return x/y*)
<*INLINE*> PROCEDURE Mod(x,y:T):T;  (*return x mod y*)

(*============================*)
(* Factoring                  *)
(*============================*)  

PROCEDURE IsPrime(n:T):BOOLEAN;
(*is this number a prime number?*)

TYPE
  Power      = RECORD p : T; m : [0..BITSIZE(T)] END;
  PowerArray = REF ARRAY OF Power;
  Array      = REF ARRAY OF T;

PROCEDURE Factor(n:T;      (*factor this number*)
                 ):Array;  (*giving primes*)
(*e.g., factor(24) gives 2^3 * 3^1 or {2,2,2,3}*)

(*
PROCEDURE Factor(n:T;          (*factor this number*)
                 VAR p,m:Array (*giving primes and multiplicity*)
                 ):CARDINAL;   (*and count of factors*)
(*e.g., factor(24) gives 2^3 * 3^1 or:
   p=[2,3]
   m=[3,1]
   return=2;
p and m are created by the procedure.
*)
*)

PROCEDURE GCD(u,v:T):T;
(*returns the greatest common denominator for u and v.*)

(*==========================*)
END IntegerBasic.
