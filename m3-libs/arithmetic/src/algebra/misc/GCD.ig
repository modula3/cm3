GENERIC INTERFACE GCD(R(*,RList*));
(*Copyright (c) 1996, m3na project

Abstract: Generic computation of the greatest common divisor

2/17/96  Harry George    Initial version
*)

FROM xUtils IMPORT Error;

(*==========================*)

TYPE
  T = R.T;

PROCEDURE GCD(u,v:T):T;
(*returns the greatest common divisor for u and v.*)

PROCEDURE LCM(u,v:T):T;
(*returns the least common multiple for u and v.*)

PROCEDURE BezoutGCD(u,v:T; VAR (*OUT*) x,y : T);
(*returns factors x and y such that u*x+v*y=GCD(u,v).*)

PROCEDURE Bezout(u,v,w:T; VAR (*OUT*) x,y : T) RAISES {Error};
(*returns factors x and y such that u*x+v*y=w.
  w must be divisible by GCD(u,v)*)

(*
PROCEDURE MACDecompose(u,v:T; VAR (*OUT*) mac : RList.T) : T;
(*returns the greatest common divisor of u and v
  and writes a list of Multiply&Accumulate operations to 'mac'
  Start with x := GCD(u,v); y := Zero;
  Iterate    y := y + x*mac; Swap(x,y); mac := mac.tail;
  at the end u=x and v=y *)
*)

(*==========================*)
END GCD.
