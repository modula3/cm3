MODULE TestInteger EXPORTS Test;
(*Copyright (c) 1996, m3na project
Abstract:  Tests for Integer module.

3/6/96    Harry George   Initial version

*)

IMPORT Cardinal32Basic  AS Cd,
       Cardinal32GCD    AS CdG,
       Cardinal32FmtLex AS CdF,
       IntegerTrans     AS IT,
       LongRealBasic    AS R,
       LongRealTrans    AS RT,
       LongRealFmtLex   AS RF,
       NumberTheory     AS NT;
(*=======================*)
CONST
  Module = "TestInteger.";
(*----------------------*)
PROCEDURE TestSqRt():BOOLEAN=
CONST
  ftn = Module & "TestSqRt";
VAR
  result:=TRUE;
  ii,iisqrt:Cd.T;
BEGIN
  Debug(1,ftn,"begin\n");
  FOR i:=0 TO 10 DO
    ii:=i*i; iisqrt:=IT.SqRt(ii);
    Msg("SqRt(" & CdF.Fmt(ii)& ")="
       & CdF.Fmt(i) & "=" & CdF.Fmt(iisqrt)
       & "\n");
  END;

  FOR i:=1111 TO 31000  BY 4321 DO
    ii:=i*i; iisqrt:=IT.SqRt(ii);
    Msg("SqRt(" & CdF.Fmt(ii)& ")="
       & CdF.Fmt(i) & "=" & CdF.Fmt(iisqrt)
       & "\n");
  END;
  RETURN result;
END TestSqRt;
(*----------------------*)
PROCEDURE TestCardinalGCD():BOOLEAN=
CONST
  ftn = Module & "TestCardinalGCD";
VAR
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
  FOR i:=0 TO 100 BY 7 DO
    FOR j:=0 TO 100 BY 13 DO
      Msg("GCD(" & CdF.Fmt(i) & "," & CdF.Fmt(j) & ")="
        & CdF.Fmt(CdG.GCD(i,j))
        & "\n");
    END;
  END;
  RETURN result;
END TestCardinalGCD;
(*----------------------*)
PROCEDURE TestIsPrime():BOOLEAN=
CONST
  ftn = Module & "TestIsPrime";
VAR
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
  FOR i:=46500 TO 47500  DO
    IF NT.IsPrime(i) THEN
      Msg(" " & CdF.Fmt(i));
    END;
  END;
  Msg("\n");
  RETURN result;
END TestIsPrime;
(*----------------------*)
PROCEDURE TestFactor():BOOLEAN=
CONST
  ftn = Module & "TestFactor";
VAR
  result:=TRUE;
  p:NT.PowerArray;
BEGIN
  Debug(1,ftn,"begin\n");
  FOR i:=1111 TO 100000 BY 4321 DO
    Msg(CdF.Fmt(i) & ": ");
    p:=NT.FactorPower(i);
    FOR j:=0 TO LAST(p^) DO
      Msg(CdF.Fmt(p[j].p) & "^" & CdF.Fmt(p[j].exp) & " ");
    END;
    Msg("\n");
  END;
  RETURN result;
END TestFactor;
(*----------------------*)
PROCEDURE TestCordic():BOOLEAN=
CONST
  ftn = Module & "TestCordic";
  n=10;
VAR
  result:=TRUE;
  rad,truesin,truecos,calcsin,calccos:R.T;
  step:=RT.TwoPi/FLOAT(n+1,R.T);
  theta:IT.Cordic;
  s,c:INTEGER;
BEGIN
  Debug(1,ftn,"begin\n");
  rad:=R.Zero;
  FOR i:=1 TO n DO
    rad:=rad+step;
    truesin:=RT.Sin(rad);
    truecos:=RT.Cos(rad);
    Msg("truecos=" & RF.Fmt(truecos)
      &" truesin=" & RF.Fmt(truesin) & "\n");

    theta:=ROUND(rad*IT.RadToCordic);

    IT.SinCos(theta:=theta,s:=s,c:=c);
    calcsin:=FLOAT(s,R.T)*IT.CordicToReal;
    calccos:=FLOAT(c,R.T)*IT.CordicToReal;
    Msg("calccos=" & RF.Fmt(calccos)
      &" calcsin=" & RF.Fmt(calcsin) & "\n\n");
  END;
  RETURN result;
END TestCordic;
(*-------------------------*)
PROCEDURE TestInteger():BOOLEAN=
<*UNUSED*> CONST ftn = Module & "TestInteger";
VAR result:=TRUE;
BEGIN
  NewLine(); EVAL TestFactor();
  NewLine(); EVAL TestCardinalGCD();
  NewLine(); EVAL TestIsPrime();
  NewLine(); EVAL TestSqRt();
  NewLine(); EVAL TestCordic();

  RETURN result;
END TestInteger;
(*=======================*)
BEGIN
END TestInteger.
