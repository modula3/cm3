MODULE tch05_eval EXPORTS TestS;
(*Copyright (c) 1995, Harry George
Abstract: Test driver for Modula-3 rendition of
          Numerical Recipes in C, 1992.

12/27/95  Harry George   Initial version: Ch 5

*)

IMPORT Fmt,nr;
FROM nr IMPORT REAL32,REAL64,COMPLEX;
(*=======================*)
<*UNUSED*> CONST Module = "tch05_eval.";
(*=======================*)
TYPE
  V4 = ARRAY [0..3] OF REAL32;
  V5 = ARRAY [0..4] OF REAL32;
(*-----------------------*)
PROCEDURE TestPadd():BOOLEAN=
CONST
  ftn = Module & "TestPadd";
VAR
  u:=NEW(nr.Vector,5);
  v:=NEW(nr.Vector,4);
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
  u^:=V5{1.0,2.0,3.0,4.0,5.0};
  v^:=V4{0.1,0.2,0.3,0.4};
  Msg("u="   & nr.Ptext(u) & "\n");
  Msg("v="   & nr.Ptext(v) & "\n");
  Msg("u+v=" & nr.Ptext(nr.Padd(u,v)) & "\n");          
  Msg("v+u=" & nr.Ptext(nr.Padd(v,u)) & "\n");          
  RETURN result;
END TestPadd;
(*-----------------------*)
PROCEDURE TestPsub():BOOLEAN=
CONST
  ftn = Module & "TestPsub";
VAR
  u:=NEW(nr.Vector,5);
  v:=NEW(nr.Vector,4);
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
  u^:=V5{1.0,2.0,3.0,4.0,5.0};
  v^:=V4{0.1,0.2,0.3,0.4};
  Msg("u="   & nr.Ptext(u) & "\n");
  Msg("v="   & nr.Ptext(v) & "\n");
  Msg("u-v=" & nr.Ptext(nr.Psub(u,v)) & "\n");          
  Msg("v-u=" & nr.Ptext(nr.Psub(v,u)) & "\n");          
  RETURN result;
END TestPsub;
(*-----------------------*)
PROCEDURE TestPmul():BOOLEAN=
CONST
  ftn = Module & "TestPmul";
VAR
  u:=NEW(nr.Vector,5);
  v:=NEW(nr.Vector,4);
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
  u^:=V5{1.0,2.0,3.0,4.0,5.0};
  v^:=V4{0.1,0.2,0.3,0.4};
  Msg("u="   & nr.Ptext(u) & "\n");
  Msg("v="   & nr.Ptext(v) & "\n");
  Msg("u*v=" & nr.Ptext(nr.Pmul(u,v)) & "\n");          
  Msg("v*u=" & nr.Ptext(nr.Pmul(v,u)) & "\n");          
  RETURN result;
END TestPmul;
(*-----------------------*)
PROCEDURE TestPdiv():BOOLEAN=
CONST
  ftn = Module & "TestPdiv";
VAR
  u:=NEW(nr.Vector,5);
  v:=NEW(nr.Vector,4);
  q,r:nr.Vector;
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
  u^:=V5{1.0,2.0,3.0,4.0,5.0};
  v^:=V4{0.1,0.2,0.3,0.4};
  Msg("u="   & nr.Ptext(u) & "\n");
  Msg("v="   & nr.Ptext(v) & "\n");
  Msg("u/v="); nr.Pdiv(u,v,q,r);
      Msg(nr.Ptext(q) & " rem=" & nr.Ptext(r) & "\n");          
  Msg("v/u="); nr.Pdiv(v,u,q,r);
      Msg(nr.Ptext(q) & " rem=" & nr.Ptext(r) & "\n");          
  RETURN result;
END TestPdiv;
(*-----------------------*)
PROCEDURE TestDdpoly():BOOLEAN=
CONST
  ftn = Module & "TestDdpoly";
VAR
  p:=NEW(nr.Vector,5);
  nd:=3;
  pd:=NEW(nr.Vector,nd+1);
  x:=1.0;
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
  p^:=V5{1.0,1.0,1.0,1.0,1.0};
  Msg("p="   & nr.Ptext(p) & "\n");
FOR j:=1 TO 5 DO
  x:=FLOAT(j,REAL32);
  Msg("x=" & Fmt.Real(x));
  nr.ddpoly(p,x,pd,nd);
  FOR i:=0 TO nd DO
    Msg(" d"& Fmt.Int(i) & "=" & Fmt.Real(pd[i]));
  END;
  Msg("\n");
END;
  RETURN result;
END TestDdpoly;
(*-----------------------*)
PROCEDURE TestEulsum():BOOLEAN=
CONST
  ftn = Module & "TestEulsum";
VAR
  result:=TRUE;
BEGIN
  RETURN result;
END TestEulsum;
(*-----------------------*)
PROCEDURE TestRatval():BOOLEAN=
CONST
  ftn = Module & "TestRatval";
TYPE
  V3 = ARRAY [0..2] OF REAL64;
  V4 = ARRAY [0..3] OF REAL64;
VAR
  result:=TRUE;
  num:=NEW(nr.dVector,3);
  den:=NEW(nr.dVector,4);
  x,d:REAL64;  
BEGIN
  Debug(1,ftn,"begin\n");
  num^:=V3{1.0d0,2.0d0,3.0d0};
  den^:=V4{1.0d0,2.0d0,3.0d0,4.0d0};
  x:=2.5d0;
  d:=nr.ratval(x,num,den);
  Msg(nr.dPtext(num)
    & "/" & nr.dPtext(den)
    & "=" & Fmt.LongReal(d) & "\n");
  RETURN result;
END TestRatval;
(*-----------------------*)
PROCEDURE TestQuadreal():BOOLEAN=
CONST
  ftn = Module & "TestQuadreal";
VAR
  result:=TRUE;
  x1,x2:COMPLEX;
  a,b,c:REAL32;
BEGIN
  Debug(1,ftn,"begin\n");
  a:=1.0; b:=2.0; c:=-3.0;
  Msg("Solve a*x^2+b*x+c=0 for"
    & " a=" & Fmt.Real(a)
    & " b=" & Fmt.Real(b)
    & " c=" & Fmt.Real(c) & "\n");
    
  nr.quadreal(a,b,c,x1,x2);
  
  Msg("x1=" & nr.Ctext(x1)
   & " x2=" & nr.Ctext(x2) & "\n");

  IF NOT Verify(ftn,"quadreal",-3.0,x1.re,0.01) THEN
    result:=FALSE;
  END;      
  RETURN result;
END TestQuadreal;
(*-----------------------*)
PROCEDURE TestQuadcmpx():BOOLEAN=
CONST
  ftn = Module & "TestCmpx";
VAR
  result:=TRUE;
  x1,x2:COMPLEX;
  a,b,c:COMPLEX;
BEGIN
  Debug(1,ftn,"begin\n");
  a:=nr.Complex(1.0,1.0);
  b:=nr.Complex(2.0,2.0);
  c:=nr.Complex(-3.0,3.0);
  Msg("Solve a*x^2+b*x+c=0 for"
    & " a=" & nr.Ctext(a)
    & " b=" & nr.Ctext(b)
    & " c=" & nr.Ctext(c) & "\n");
    
  nr.quadcmpx(a,b,c,x1,x2);
  
  Msg("x1=" & nr.Ctext(x1)
   & " x2=" & nr.Ctext(x2));
      
  RETURN result;
END TestQuadcmpx;
(*-----------------------*)
PROCEDURE TestCheby():BOOLEAN=
CONST
  ftn = Module & "TestCheby";
VAR
  cheby:=NEW(nr.ChebyApprox).init(nr.sin);
  cheb1,cheb2:nr.ChebyApprox;
  m:CARDINAL;
  x,y1,y2:REAL32;
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
  m:=cheby.findm(prec:=0.00001);
  Msg("m=" & Fmt.Int(m) & "\n");
  FOR i:=1 TO 9 DO
    x:=-1.0+0.2*FLOAT(i,REAL32);
    y1:=cheby.eval(x,m);
    y2:=nr.sin(x);
    Msg("sin(" & Fmt.Real(x,prec:=2)
      & ")= cheby:" & Fmt.Real(y1,prec:=4)
      & " Math32:"  & Fmt.Real(y2,prec:=4) & "\n");
  END;

  Msg("Doing derivatives and integrals\n");
  cheb1:=NEW(nr.ChebyApprox).init(nr.sin);
  Msg("cheb1 built\n");
  cheby:=cheb1.integral(0);
  Msg("cheby = D(cheb1)\n");
  m:=cheby.findm(prec:=0.00001);
  Msg("m=" & Fmt.Int(m) & "\n");
  FOR i:=1 TO 9 DO
    x:=-1.0+0.2*FLOAT(i,REAL32);
    y1:=cheby.eval(x,m);
    y2:=nr.sin(x);
    Msg("sin(" & Fmt.Real(x,prec:=2)
      & ")= cheby:" & Fmt.Real(y1,prec:=4)
      & " Math32:"  & Fmt.Real(y2,prec:=4) & "\n");
  END;
  
  RETURN result;
END TestCheby;
(*-----------------------*)
PROCEDURE TestEvalfun():BOOLEAN=
CONST
  ftn = Module & "TestEvalfun";
BEGIN
  (*NewLine(); EVAL TestEulsum();*)
  (*NewLine(); EVAL TestRatval();*)
  (*NewLine(); EVAL TestQuadreal();*)
  (*NewLine(); EVAL TestCheby();*)
  NewLine(); EVAL TestPadd();
  NewLine(); EVAL TestPsub();
  NewLine(); EVAL TestPmul();
  NewLine(); EVAL TestPdiv();
  (*NewLine(); EVAL TestDdpoly();*)
  RETURN TRUE;
END TestEvalfun;
(*=======================*)
BEGIN
END tch05_eval.
