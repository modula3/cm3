MODULE TestInterp EXPORTS Test;
(*Copyright (c) 1996, m3na project
Abstract: Test driver for interpolation.

12/28/95  Harry George   Initial version
1/29/96   Harry George   converted to m3na format
*)

IMPORT IO,Wr,Fmt,xInterp;
IMPORT xReal64 AS R;
FROM xReal64 IMPORT REAL64,Array,sin,cos;
(*=======================*)
CONST
  Module = "TestInterp.";
VAR
(*=======================*)
(*----------------------*)
PROCEDURE TestLinear():BOOLEAN=
CONST
  ftn = Module & "TestLinear";
VAR
  result:=TRUE;
  n:=100;  n1:=0; nn:=n-1;
  scale:=1.0D0/FLOAT(n,REAL64);
  xa:=NEW(Array,n);
  ya:=NEW(Array,n);
  x,y1,y2,dy,offset:REAL64;
  stepsize:=n DIV 5;
BEGIN
  Debug(1,ftn,"begin\n");

  FOR i:=n1 TO nn DO
    xa[i]:=3.0D0*FLOAT(i,REAL64)*scale; (*range of 0..3.0*)
    ya[i]:=sin(xa[i]);
  END;

  FOR i:=n1 TO nn BY stepsize DO
    x:=xa[i]+0.1D0;
    y1:=sin(x);
    y2:=xInterp.linear(xa^,ya^,x);
    Msg("linear: x=" & R.fmt(x)
    & "\n y1=" & R.fmt(y1)
    & "\n y2=" & R.fmt(y2)
    & "\n dy=" & R.fmt(y2-y1)
    & " relerr=" & R.fmt((y2-y1)/y1)
    & "\n");
  END;
  RETURN result;
END TestLinear;

(*----------------------*)
PROCEDURE TestNewt():BOOLEAN=
CONST
  ftn = Module & "TestNewt";
VAR
  result:=TRUE;
  n:=10;  n1:=0; nn:=n-1;
  scale:=1.0D0/FLOAT(n,REAL64);
  xa:=NEW(Array,n);
  ya:=NEW(Array,n);
  x,y1,y2,dy,offset:REAL64;
  stepsize:=n DIV 5;
BEGIN
  Debug(1,ftn,"begin\n");

  FOR i:=n1 TO nn DO
    xa[i]:=3.0D0*FLOAT(i,REAL64)*scale; (*range of 0..3.0*)
    ya[i]:=sin(xa[i]);
  END;

  offset:=0.1D0;
  FOR i:=n1 TO nn BY stepsize DO
    x:=xa[i]+offset;
    y1:=sin(x);
    y2:=xInterp.newt(xa^,ya^,x,dy);
    Msg("10-point: x=" & R.fmt(x)
    & "\n y1=" & R.fmt(y1)
    & "\n y2=" & R.fmt(y2)
    & "\n dy=" & R.fmt(dy)
    & " relerr=" & R.fmt((y2-y1)/y1)
    & "\n");
  END;

  offset:=0.1D0;
  FOR i:=5 TO 7 DO
    x:=xa[i]+offset;
    y1:=sin(x);
    y2:=xInterp.newt(xa^,ya^,x,dy,start:=5, len:=4);
    Msg("4-point: x=" & R.fmt(x)
    & "\n y1=" & R.fmt(y1)
    & "\n y2=" & R.fmt(y2)
    & "\n dy=" & R.fmt(dy)
    & " relerr=" & R.fmt((y2-y1)/y1)
    & "\n");
  END;

  RETURN result;   
END TestNewt;
(*-------------------------*)
PROCEDURE TestInterp():BOOLEAN=
CONST ftn = Module & "TestInterp";
VAR result:=TRUE;
BEGIN
  NewLine(); EVAL TestLinear();
  NewLine(); EVAL TestNewt();
  RETURN result;
END TestInterp;
(*=======================*)
BEGIN
END TestInterp.
