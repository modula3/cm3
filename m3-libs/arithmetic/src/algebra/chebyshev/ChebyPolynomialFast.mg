GENERIC MODULE ChebyPolynomialFast(R,RT);
(*Copyright (c) 1995, Harry George

Abstract: Implementation of Modula-3 version of
          NR92, ch 5.

12/27/95  Harry George    Initial version
*)
FROM NADefinitions IMPORT Error,Err;
FROM RT IMPORT Cos, Pi;

CONST Module = "ChebyPolynomialFast.";

(*---------------------------*)
PROCEDURE Expand
           (func:Ftn;         (*differentiate polynomial*)
               n:CARDINAL;    (*order*)
               ):T=
VAR
  nr:=FLOAT(n,R.T);
  f:=NEW(T,n+1);  (*we skip f[0]*)
  x,factor,sum,jr,kr:R.T;
  z:=NEW(T,n);
BEGIN
  (*---load up the function values---*)
  FOR k:=1 TO n DO
    kr:=FLOAT(k,R.T);
    x:=Cos(Pi*(kr-RT.Half)/nr);
    f[k]:=func(x);
  END;

  (*---compute coeffs---*)
  factor:=R.Two/nr;
  FOR j:=0 TO n-1 DO
    jr:=FLOAT(j,R.T);
    sum:=R.Zero;
    FOR k:=1 TO n DO
      kr:=FLOAT(k,R.T);
      sum:=sum+f[k]*Cos(Pi*jr*(kr-RT.Half)/nr);
    END;
    z[j]:=factor*sum;
  END;
  RETURN z;
END Expand;
(*--------------------------*)
PROCEDURE FindUpperExp
              (x:T;
            prec:R.T):CARDINAL=
<*UNUSED*> CONST ftn = Module & "FindUpperExp";
VAR
  error:R.T:=R.Zero;
BEGIN
  FOR i:=LAST(x^) TO 0 BY -1 DO
    error:=error+ABS(x[i]);
    IF error>prec THEN
      RETURN i+1;
    END;
  END;
  RETURN 0;
END FindUpperExp;
(*--------------------------*)
PROCEDURE Abort(x:T;          (*abort the expansion*)
                m:CARDINAL;   (*before the m-th term*)
                ):T=
VAR
  z:=NEW(T,m);
BEGIN
  z^:=SUBARRAY(x^,0,m);
  RETURN z;
END Abort;
(*--------------------------*)
PROCEDURE Eval(x:T;           (*eval this polynomial*)
              xi:R.T          (*at this point*)
               ):R.T RAISES {Error}=
<*UNUSED*> CONST ftn = Module & "Eval";
VAR
  dj:=R.Zero;
  djp1:=R.Zero;
  djp2:=R.Zero;
BEGIN
  IF xi < -R.One OR xi > R.One THEN
    (*need -1<x<+1*)
    RAISE Error(Err.out_of_range);
  END;
  FOR j:=LAST(x^) TO 1 BY -1 DO
    dj:=R.Two*xi*djp1-djp2+x[j];
    djp2:=djp1;
    djp1:=dj;
  END;
  RETURN xi*djp1-djp2+RT.Half*x[0];
END Eval;
(*--------------------------*)
PROCEDURE Derive
              (x:T;           (*differentiate polynomial*)
               ):T=
VAR
  n:=NUMBER(x^);
  z:=NEW(T,n-1);
BEGIN
  IF n>=2 THEN
    z[n-2]:=FLOAT(2*(n-1),R.T)*x[n-1];
    IF n>=3 THEN
      z[n-3]:=FLOAT(2*(n-2),R.T)*x[n-2];
      FOR j:=n-4 TO 0 BY -1 DO
        z[j]:=z[j+2]+FLOAT(2*(j+1),R.T)*x[j+1];
      END;
    END;
  END;
  RETURN z;
END Derive;
(*--------------------------*)
PROCEDURE Integrate
              (x:T;           (*differentiate polynomial*)
               ):T=
VAR
  n:=NUMBER(x^);
  z:=NEW(T,n+1);
BEGIN
  IF 0<=n THEN
    z[0]:=R.Zero;
    FOR j:=1 TO n-2 DO
      z[j]:=(x[j-1]-x[j+1])/FLOAT(2*j,R.T);
    END;
    IF 2<=n THEN
      z[n-1]:=x[n-2]/FLOAT(2*(n-1),R.T);
    END;
    IF 1<=n THEN
      z[n  ]:=x[n-1]/FLOAT(2*(n  ),R.T);
    END;
  END;
  RETURN z;
END Integrate;
(*==========================*)
BEGIN
END ChebyPolynomialFast.
