MODULE tch05_eval EXPORTS tests;
(*Copyright (c) 1995, Harry George
Abstract: Test driver for Modula-3 rendition of
          Numerical Recipes in C, 1992.

12/27/95  Harry George   Initial version: Ch 5

*)

IMPORT Fmt,nr;
FROM nr IMPORT REAL32,REAL64,COMPLEX;
(*=======================*)
CONST Module = "tch05_eval.";
(*=======================*)
TYPE
  V4 = ARRAY [0..3] OF REAL32;
  V5 = ARRAY [0..4] OF REAL32;
(*-----------------------*)
PROCEDURE test_Padd():BOOLEAN=
CONST
  ftn = Module & "test_Padd";
VAR
  u:=NEW(nr.Vector,5);
  v:=NEW(nr.Vector,4);
  result:=TRUE;
BEGIN
  debug(1,ftn,"begin\n");
  u^:=V5{1.0,2.0,3.0,4.0,5.0};
  v^:=V4{0.1,0.2,0.3,0.4};
  msg("u="   & nr.Ptext(u) & "\n");
  msg("v="   & nr.Ptext(v) & "\n");
  msg("u+v=" & nr.Ptext(nr.Padd(u,v)) & "\n");          
  msg("v+u=" & nr.Ptext(nr.Padd(v,u)) & "\n");          
  RETURN result;
END test_Padd;
(*-----------------------*)
PROCEDURE test_Psub():BOOLEAN=
CONST
  ftn = Module & "test_Psub";
VAR
  u:=NEW(nr.Vector,5);
  v:=NEW(nr.Vector,4);
  result:=TRUE;
BEGIN
  debug(1,ftn,"begin\n");
  u^:=V5{1.0,2.0,3.0,4.0,5.0};
  v^:=V4{0.1,0.2,0.3,0.4};
  msg("u="   & nr.Ptext(u) & "\n");
  msg("v="   & nr.Ptext(v) & "\n");
  msg("u-v=" & nr.Ptext(nr.Psub(u,v)) & "\n");          
  msg("v-u=" & nr.Ptext(nr.Psub(v,u)) & "\n");          
  RETURN result;
END test_Psub;
(*-----------------------*)
PROCEDURE test_Pmul():BOOLEAN=
CONST
  ftn = Module & "test_Pmul";
VAR
  u:=NEW(nr.Vector,5);
  v:=NEW(nr.Vector,4);
  result:=TRUE;
BEGIN
  debug(1,ftn,"begin\n");
  u^:=V5{1.0,2.0,3.0,4.0,5.0};
  v^:=V4{0.1,0.2,0.3,0.4};
  msg("u="   & nr.Ptext(u) & "\n");
  msg("v="   & nr.Ptext(v) & "\n");
  msg("u*v=" & nr.Ptext(nr.Pmul(u,v)) & "\n");          
  msg("v*u=" & nr.Ptext(nr.Pmul(v,u)) & "\n");          
  RETURN result;
END test_Pmul;
(*-----------------------*)
PROCEDURE test_Pdiv():BOOLEAN=
CONST
  ftn = Module & "test_Pdiv";
VAR
  u:=NEW(nr.Vector,5);
  v:=NEW(nr.Vector,4);
  q,r:nr.Vector;
  result:=TRUE;
BEGIN
  debug(1,ftn,"begin\n");
  u^:=V5{1.0,2.0,3.0,4.0,5.0};
  v^:=V4{0.1,0.2,0.3,0.4};
  msg("u="   & nr.Ptext(u) & "\n");
  msg("v="   & nr.Ptext(v) & "\n");
  msg("u/v="); nr.Pdiv(u,v,q,r);
      msg(nr.Ptext(q) & " rem=" & nr.Ptext(r) & "\n");          
  msg("v/u="); nr.Pdiv(v,u,q,r);
      msg(nr.Ptext(q) & " rem=" & nr.Ptext(r) & "\n");          
  RETURN result;
END test_Pdiv;
(*-----------------------*)
PROCEDURE test_ddpoly():BOOLEAN=
CONST
  ftn = Module & "test_ddpoly";
VAR
  p:=NEW(nr.Vector,5);
  nd:=3;
  pd:=NEW(nr.Vector,nd+1);
  x:=1.0;
  result:=TRUE;
BEGIN
  debug(1,ftn,"begin\n");
  p^:=V5{1.0,1.0,1.0,1.0,1.0};
  msg("p="   & nr.Ptext(p) & "\n");
FOR j:=1 TO 5 DO
  x:=FLOAT(j,REAL32);
  msg("x=" & Fmt.Real(x));
  nr.ddpoly(p,x,pd,nd);
  FOR i:=0 TO nd DO
    msg(" d"& Fmt.Int(i) & "=" & Fmt.Real(pd[i]));
  END;
  msg("\n");
END;
  RETURN result;
END test_ddpoly;
(*-----------------------*)
PROCEDURE test_eulsum():BOOLEAN=
CONST
  ftn = Module & "test_eulsum";
VAR
  result:=TRUE;
BEGIN
  RETURN result;
END test_eulsum;
(*-----------------------*)
PROCEDURE test_ratval():BOOLEAN=
CONST
  ftn = Module & "test_ratval";
TYPE
  V3 = ARRAY [0..2] OF REAL64;
  V4 = ARRAY [0..3] OF REAL64;
VAR
  result:=TRUE;
  num:=NEW(nr.dVector,3);
  den:=NEW(nr.dVector,4);
  x,d:REAL64;  
BEGIN
  debug(1,ftn,"begin\n");
  num^:=V3{1.0d0,2.0d0,3.0d0};
  den^:=V4{1.0d0,2.0d0,3.0d0,4.0d0};
  x:=2.5d0;
  d:=nr.ratval(x,num,den);
  msg(nr.dPtext(num)
    & "/" & nr.dPtext(den)
    & "=" & Fmt.LongReal(d) & "\n");
  RETURN result;
END test_ratval;
(*-----------------------*)
PROCEDURE test_quadreal():BOOLEAN=
CONST
  ftn = Module & "test_quadreal";
VAR
  result:=TRUE;
  x1,x2:COMPLEX;
  a,b,c:REAL32;
BEGIN
  debug(1,ftn,"begin\n");
  a:=1.0; b:=2.0; c:=-3.0;
  msg("Solve a*x^2+b*x+c=0 for"
    & " a=" & Fmt.Real(a)
    & " b=" & Fmt.Real(b)
    & " c=" & Fmt.Real(c) & "\n");
    
  nr.quadreal(a,b,c,x1,x2);
  
  msg("x1=" & nr.Ctext(x1)
   & " x2=" & nr.Ctext(x2) & "\n");

  IF NOT verify(ftn,"quadreal",-3.0,x1.re,0.01) THEN
    result:=FALSE;
  END;      
  RETURN result;
END test_quadreal;
(*-----------------------*)
PROCEDURE test_quadcmpx():BOOLEAN=
CONST
  ftn = Module & "test_cmpx";
VAR
  result:=TRUE;
  x1,x2:COMPLEX;
  a,b,c:COMPLEX;
BEGIN
  debug(1,ftn,"begin\n");
  a:=nr.Complex(1.0,1.0);
  b:=nr.Complex(2.0,2.0);
  c:=nr.Complex(-3.0,3.0);
  msg("Solve a*x^2+b*x+c=0 for"
    & " a=" & nr.Ctext(a)
    & " b=" & nr.Ctext(b)
    & " c=" & nr.Ctext(c) & "\n");
    
  nr.quadcmpx(a,b,c,x1,x2);
  
  msg("x1=" & nr.Ctext(x1)
   & " x2=" & nr.Ctext(x2));
      
  RETURN result;
END test_quadcmpx;
(*-----------------------*)
PROCEDURE test_cheby():BOOLEAN=
CONST
  ftn = Module & "test_cheby";
VAR
  cheby:=NEW(nr.ChebyApprox).init(nr.sin);
  cheb1,cheb2:nr.ChebyApprox;
  m:CARDINAL;
  x,y1,y2:REAL32;
  result:=TRUE;
BEGIN
  debug(1,ftn,"begin\n");
  m:=cheby.findm(prec:=0.00001);
  msg("m=" & Fmt.Int(m) & "\n");
  FOR i:=1 TO 9 DO
    x:=-1.0+0.2*FLOAT(i,REAL32);
    y1:=cheby.eval(x,m);
    y2:=nr.sin(x);
    msg("sin(" & Fmt.Real(x,prec:=2)
      & ")= cheby:" & Fmt.Real(y1,prec:=4)
      & " Math32:"  & Fmt.Real(y2,prec:=4) & "\n");
  END;

  msg("Doing derivatives and integrals\n");
  cheb1:=NEW(nr.ChebyApprox).init(nr.sin);
  msg("cheb1 built\n");
  cheby:=cheb1.integral(0);
  msg("cheby = D(cheb1)\n");
  m:=cheby.findm(prec:=0.00001);
  msg("m=" & Fmt.Int(m) & "\n");
  FOR i:=1 TO 9 DO
    x:=-1.0+0.2*FLOAT(i,REAL32);
    y1:=cheby.eval(x,m);
    y2:=nr.sin(x);
    msg("sin(" & Fmt.Real(x,prec:=2)
      & ")= cheby:" & Fmt.Real(y1,prec:=4)
      & " Math32:"  & Fmt.Real(y2,prec:=4) & "\n");
  END;
  
  RETURN result;
END test_cheby;
(*-----------------------*)
PROCEDURE test_evalfun():BOOLEAN=
CONST
  ftn = Module & "test_evalfun";
BEGIN
  (*newline(); EVAL test_eulsum();*)
  (*newline(); EVAL test_ratval();*)
  (*newline(); EVAL test_quadreal();*)
  (*newline(); EVAL test_cheby();*)
  newline(); EVAL test_Padd();
  newline(); EVAL test_Psub();
  newline(); EVAL test_Pmul();
  newline(); EVAL test_Pdiv();
  (*newline(); EVAL test_ddpoly();*)
  RETURN TRUE;
END test_evalfun;
(*=======================*)
BEGIN
END tch05_eval.
