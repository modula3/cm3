MODULE TestInteger EXPORTS Test;
(*Copyright (c) 1996, m3na project
Abstract:  TestS for Integer module.

3/6/96    Harry George   Initial version

*)

IMPORT xInteger AS I,
       xReal64  AS R;
(*=======================*)
CONST
  Module = "TestInteger.";
(*----------------------*)
PROCEDURE TestABC():BOOLEAN=
CONST
  ftn = Module & "TestABC";
VAR
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");

  RETURN result;
END TestABC;
(*----------------------*)
PROCEDURE TestSqrt():BOOLEAN=
CONST
  ftn = Module & "TestSqrt";
VAR
  result:=TRUE;
  ii,iisqrt:I.Card32;
BEGIN
  Debug(1,ftn,"begin\n");
  FOR i:=0 TO 10 DO
    ii:=i*i; iisqrt:=I.sqrt(ii);
    Msg("sqrt(" & I.fmt(ii)& ")="
       & I.fmt(i) & "=" & I.fmt(iisqrt)
       & "\n");
  END;

  FOR i:=1111 TO 31000  BY 4321 DO
    ii:=i*i; iisqrt:=I.sqrt(ii);
    Msg("sqrt(" & I.fmt(ii)& ")="
       & I.fmt(i) & "=" & I.fmt(iisqrt)
       & "\n");
  END;
  RETURN result;
END TestSqrt;
(*----------------------*)
PROCEDURE TestGcd():BOOLEAN=
CONST
  ftn = Module & "TestGcd";
VAR
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
  FOR i:=0 TO 100 BY 7 DO
    FOR j:=0 TO 100 BY 13 DO
      Msg("gcd(" & I.fmt(i) & "," & I.fmt(j) & ")="
        & I.fmt(I.gcd(i,j))
        & "\n");
    END;
  END;
  RETURN result;
END TestGcd;
(*----------------------*)
PROCEDURE TestIsprime():BOOLEAN=
CONST
  ftn = Module & "TestIsprime";
VAR
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
  FOR i:=46500 TO 47500  DO
    IF I.isprime(i) THEN
      Msg(" " & I.fmt(i));
    END;
  END;
  Msg("\n");
  RETURN result;
END TestIsprime;
(*----------------------*)
PROCEDURE TestFactor():BOOLEAN=
CONST
  ftn = Module & "TestFactor";
VAR
  result:=TRUE;
  p,m:I.Array;
  count:CARDINAL;
BEGIN
  Debug(1,ftn,"begin\n");
  FOR i:=1111 TO 100000 BY 4321 DO
    Msg(I.fmt(i) & ": ");
    count:=I.factor(i,p,m);
    FOR j:=0 TO count-1 DO
      Msg(I.fmt(p[j]) & "^" & I.fmt(m[j]) & " ");
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
  step:=R.TwoPi/FLOAT(n+1,R.T);
  theta:I.Cordic;
  s,c:INTEGER;
BEGIN
  Debug(1,ftn,"begin\n");
  rad:=R.Zero;
  FOR i:=1 TO n DO
    rad:=rad+step;
    truesin:=R.sin(rad);
    truecos:=R.cos(rad);
    Msg("truecos=" & R.fmt(truecos)
      &" truesin=" & R.fmt(truesin) & "\n");

    theta:=ROUND(rad*I.RadToCordic);

    I.sin_cos(theta:=theta,s:=s,c:=c);
    calcsin:=FLOAT(s,R.T)*I.CordicToReal;
    calccos:=FLOAT(c,R.T)*I.CordicToReal;
    Msg("calccos=" & R.fmt(calccos)
      &" calcsin=" & R.fmt(calcsin) & "\n\n");
  END;
  RETURN result;
END TestCordic;
(*-------------------------*)
PROCEDURE TestInteger():BOOLEAN=
CONST ftn = Module & "TestInteger";
VAR result:=TRUE;
BEGIN
  NewLine(); EVAL TestFactor();
  NewLine(); EVAL TestGcd();
  NewLine(); EVAL TestIsprime();
  NewLine(); EVAL TestSqrt();
  NewLine(); EVAL TestCordic();

  RETURN result;
END TestInteger;
(*=======================*)
BEGIN
END TestInteger.
