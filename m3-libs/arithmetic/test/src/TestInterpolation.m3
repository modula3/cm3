MODULE TestInterpolation EXPORTS Test;
(*Copyright (c) 1996, m3na project
Abstract: Test driver for interpolation.

12/28/95  Harry George   Initial version
1/29/96   Harry George   converted to m3na format
*)

IMPORT LongRealBasic  AS R,
       LongRealFmtLex AS RF,
       LongRealInterpolation AS Ip,
       xUtils;
FROM LongRealTrans IMPORT Sin;
(*=======================*)
CONST
  Module = "TestInterpolation.";
(*=======================*)
<*FATAL xUtils.Error*>
(*----------------------*)
PROCEDURE TestLinear():BOOLEAN=
CONST
  ftn = Module & "TestLinear";
VAR
  result:=TRUE;
  n:=100;  n1:=0; nn:=n-1;
  scale:=1.0D0/FLOAT(n,R.T);
  xa:=NEW(REF ARRAY OF R.T,n);
  ya:=NEW(REF ARRAY OF R.T,n);
  x,y1,y2:R.T;
  stepsize:=n DIV 5;
BEGIN
  Debug(1,ftn,"begin\n");

  FOR i:=n1 TO nn DO
    xa[i]:=3.0D0*FLOAT(i,R.T)*scale; (*range of 0..3.0*)
    ya[i]:=Sin(xa[i]);
  END;

  FOR i:=n1 TO nn BY stepsize DO
    x:=xa[i]+0.1D0;
    y1:=Sin(x);
    y2:=Ip.Linear(xa^,ya^,x);
    Msg("linear: x=" & RF.Fmt(x)
    & "\n y1=" & RF.Fmt(y1)
    & "\n y2=" & RF.Fmt(y2)
    & "\n dy=" & RF.Fmt(y2-y1)
    & " relerr=" & RF.Fmt((y2-y1)/y1)
    & "\n");
  END;
  RETURN result;
END TestLinear;

(*----------------------*)
PROCEDURE TestNewton():BOOLEAN=
CONST
  ftn = Module & "TestNewton";
VAR
  result:=TRUE;
  n:=10;  n1:=0; nn:=n-1;
  scale:=1.0D0/FLOAT(n,R.T);
  xa:=NEW(REF ARRAY OF R.T,n);
  ya:=NEW(REF ARRAY OF R.T,n);
  x,y1,y2,dy,offset:R.T;
  stepsize:=n DIV 5;
BEGIN
  Debug(1,ftn,"begin\n");

  FOR i:=n1 TO nn DO
    xa[i]:=3.0D0*FLOAT(i,R.T)*scale; (*range of 0..3.0*)
    ya[i]:=Sin(xa[i]);
  END;

  offset:=0.1D0;
  FOR i:=n1 TO nn BY stepsize DO
    x:=xa[i]+offset;
    y1:=Sin(x);
    y2:=Ip.Newton(xa^,ya^,x,dy);
    Msg("10-point: x=" & RF.Fmt(x)
    & "\n y1=" & RF.Fmt(y1)
    & "\n y2=" & RF.Fmt(y2)
    & "\n dy=" & RF.Fmt(dy)
    & " relerr=" & RF.Fmt((y2-y1)/y1)
    & "\n");
  END;

  offset:=0.1D0;
  FOR i:=5 TO 7 DO
    x:=xa[i]+offset;
    y1:=Sin(x);
    y2:=Ip.Newton(SUBARRAY(xa^,5,4),SUBARRAY(ya^,5,4),x,dy);
    Msg("4-point: x=" & RF.Fmt(x)
    & "\n y1=" & RF.Fmt(y1)
    & "\n y2=" & RF.Fmt(y2)
    & "\n dy=" & RF.Fmt(dy)
    & " relerr=" & RF.Fmt((y2-y1)/y1)
    & "\n");
  END;

  RETURN result;
END TestNewton;
(*-------------------------*)
PROCEDURE TestInterpolation():BOOLEAN=
<*UNUSED*>
CONST ftn = Module & "TestInterpolation";
VAR result:=TRUE;
BEGIN
  NewLine(); EVAL TestLinear();
  NewLine(); EVAL TestNewton();
  RETURN result;
END TestInterpolation;
(*=======================*)
BEGIN
END TestInterpolation.
