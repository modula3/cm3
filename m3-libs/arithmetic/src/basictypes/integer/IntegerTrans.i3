INTERFACE IntegerTrans;
(*Arithmetic for Modula-3, see doc for details

Abstract: Generic computations on integer types

2/17/96  Harry George    Initial version
*)


(*==========================*)


(*============================*)
(* Integer Approximations     *)
(*============================*)

PROCEDURE SqRt(N:[0..1073741823]):CARDINAL;
(*returns integer sqrt of N.*)

(*============================*)
(* CORDIC Functions           *)
(*============================*)
CONST
  CordicBits = 16;
  CordicBase = 65536;  (*2^CordicBits*)
  CordicHalf = CordicBase DIV 2;
  RadToCordic= 4.172151340188181D+4; (*0..+pi/2-->cordicbase*)
  CordicToReal=1.52587890625D-5;      (*cordicbase -->0..1*)

TYPE
  Cordic= [0..CordicBase*4];

PROCEDURE SinCos(theta:Cordic;     (*given this angle*)
                 VAR s,c:INTEGER); (*return sin and cos*)
(*E.g.:
  theta:=ROUND(theta_in_radians*RadToCordic);
  sin_cos(theta:=theta,s:=s,c:=c);
  sin_in_radians:=FLOAT(s,REAL64)*UnitPerCordic;
  cos_in_radians:=FLOAT(c,REAL64)*UnitPerCordic;
Of course, in real life you wouldn't be moving in and out
of floating point.  theta would be computed in cordics to begin with.
Thus 100*sin(theta) is obtained by:
  sin_cos(theta:=theta,s:=s,c:=c);
  answer:=Word.RightShift(100*s + CordicHalf),CordicBits);
*)
(*==========================*)
END IntegerTrans.
