MODULE tInteger EXPORTS test;
(*Copyright (c) 1996, m3na project
Abstract:  Tests for Integer module.

3/6/96    Harry George   Initial version

*)

IMPORT xInteger AS I,
       xReal64  AS R;
(*=======================*)
CONST
  Module = "tInteger.";
(*----------------------*)
PROCEDURE test_ABC():BOOLEAN=
CONST
  ftn = Module & "test_ABC";
VAR
  result:=TRUE;
BEGIN
  debug(1,ftn,"begin\n");

  RETURN result;   
END test_ABC;
(*----------------------*)
PROCEDURE test_sqrt():BOOLEAN=
CONST
  ftn = Module & "test_sqrt";
VAR
  result:=TRUE;
  ii,iisqrt:I.Card32;
BEGIN
  debug(1,ftn,"begin\n");
  FOR i:=0 TO 10 DO
    ii:=i*i; iisqrt:=I.sqrt(ii);
    msg("sqrt(" & I.fmt(ii)& ")="
       & I.fmt(i) & "=" & I.fmt(iisqrt)
       & "\n");
  END;

  FOR i:=1111 TO 31000  BY 4321 DO
    ii:=i*i; iisqrt:=I.sqrt(ii);
    msg("sqrt(" & I.fmt(ii)& ")="
       & I.fmt(i) & "=" & I.fmt(iisqrt)
       & "\n");
  END;
  RETURN result;   
END test_sqrt;
(*----------------------*)
PROCEDURE test_gcd():BOOLEAN=
CONST
  ftn = Module & "test_gcd";
VAR
  result:=TRUE;
BEGIN
  debug(1,ftn,"begin\n");
  FOR i:=0 TO 100 BY 7 DO
    FOR j:=0 TO 100 BY 13 DO
      msg("gcd(" & I.fmt(i) & "," & I.fmt(j) & ")="
        & I.fmt(I.gcd(i,j))
        & "\n");
    END;
  END;
  RETURN result;   
END test_gcd;
(*----------------------*)
PROCEDURE test_isprime():BOOLEAN=
CONST
  ftn = Module & "test_isprime";
VAR
  result:=TRUE;
BEGIN
  debug(1,ftn,"begin\n");
  FOR i:=46500 TO 47500  DO
    IF I.isprime(i) THEN
      msg(" " & I.fmt(i));
    END;
  END;
  msg("\n");
  RETURN result;   
END test_isprime;
(*----------------------*)
PROCEDURE test_factor():BOOLEAN=
CONST
  ftn = Module & "test_factor";
VAR
  result:=TRUE;
  p,m:I.Array;
  count:CARDINAL;
BEGIN
  debug(1,ftn,"begin\n");
  FOR i:=1111 TO 100000 BY 4321 DO
    msg(I.fmt(i) & ": ");
    count:=I.factor(i,p,m);
    FOR j:=0 TO count-1 DO
      msg(I.fmt(p[j]) & "^" & I.fmt(m[j]) & " ");
    END;
    msg("\n");
  END; 
  RETURN result;   
END test_factor;
(*----------------------*)
PROCEDURE test_cordic():BOOLEAN=
CONST
  ftn = Module & "test_cordic";
  n=10;
VAR
  result:=TRUE;
  rad,truesin,truecos,calcsin,calccos:R.T;
  step:=R.TwoPi/FLOAT(n+1,R.T);
  theta:I.Cordic;
  s,c:INTEGER;
BEGIN
  debug(1,ftn,"begin\n");
  rad:=R.Zero;
  FOR i:=1 TO n DO
    rad:=rad+step;
    truesin:=R.sin(rad);
    truecos:=R.cos(rad);
    msg("truecos=" & R.fmt(truecos)
      &" truesin=" & R.fmt(truesin) & "\n");

    theta:=ROUND(rad*I.RadToCordic);
    
    I.sin_cos(theta:=theta,s:=s,c:=c);
    calcsin:=FLOAT(s,R.T)*I.CordicToReal;
    calccos:=FLOAT(c,R.T)*I.CordicToReal;
    msg("calccos=" & R.fmt(calccos)
      &" calcsin=" & R.fmt(calcsin) & "\n\n");
  END;
  RETURN result;   
END test_cordic;
(*-------------------------*)
PROCEDURE test_Integer():BOOLEAN=
CONST ftn = Module & "test_Integer";
VAR result:=TRUE;
BEGIN
  newline(); EVAL test_factor();
  newline(); EVAL test_gcd();
  newline(); EVAL test_isprime();
  newline(); EVAL test_sqrt();
  newline(); EVAL test_cordic();
  
  RETURN result;
END test_Integer;
(*=======================*)
BEGIN
END tInteger.
