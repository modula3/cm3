GENERIC INTERFACE GCD(R(*,RList*));
(*Copyright (c) 1996, m3na project

Abstract: Generic computation of the greatest common divisor

2/17/96  Harry George    Initial version
*)

FROM NADefinitions IMPORT Error;

(*==========================*)

TYPE
  T = R.T;

PROCEDURE GCD(u,v:T):T;
(*returns the greatest common divisor for u and v.*)

PROCEDURE LCM(u,v:T):T;
(*returns the least common multiple for u and v.*)

PROCEDURE BezoutGCD(u,v:T; VAR (*OUT*) c : ARRAY [0..1],[0..1] OF T) : T;
(*returns GCD(u,v) and
  'small' factors in the matrix c such that
   c[0,0]*u+c[0,1]*v=GCD(u,v)
   c[1,0]*u+c[1,1]*v=0
   .*)

PROCEDURE Bezout(u,v,w:T; VAR (*OUT*) c : ARRAY [0..1],[0..1] OF T) RAISES {Error};
(*returns 'small' factors in the matrix c such that
   c[0,0]*u+c[0,1]*v=w
   c[1,0]*u+c[1,1]*v=0
  . w must be divisible by GCD(u,v),
  otherwise Err.indivisible is raised. *)

(*no need to instantiate a list type for that purpose*)
TYPE
  MAC =
    REF RECORD
      next   : MAC;
      factor : T;
    END;

PROCEDURE MACDecompose(u,v:T; VAR (*OUT*) mac : MAC) : T;
(*returns the greatest common divisor of u and v
  and writes a list of Multiply&Accumulate operations to 'mac'
  Start with x := GCD(u,v); y := Zero;
  Iterate    y := y + x*mac; Swap(x,y); mac := mac.tail;
  at the end u=x and v=y *)

(*==========================*)
END GCD.
