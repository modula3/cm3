MODULE TestRoot EXPORTS Test;
(*Copyright (c) 1996, Harry George
Abstract: Test driver for Modula-3 rendition of
          Numerical Recipes in C, 1992.

1/2/96    Harry George   Initial version
1/28/96   Harry George   converted to m3na
2/17/96   Harry George   Converted to ADT format
*)
FROM xUtils IMPORT Error,Err;
IMPORT Fmt,
       LongRealBasic  AS R,
       LongRealFmtLex AS RF,
       LongRealComplexFast   AS C,
       LongRealComplexFmtLex AS CF,
       Integer32Basic        AS I,
       Integer32RootBasic    AS IR,
       Integer32PolynomialFmtLex AS IPF;
FROM xReal64 IMPORT Array,Ftn;
IMPORT xRoot;
(*=======================*)
CONST
  Module = "TestRoot.";
(*---------------------*)
VAR (*globally visible*)
  r1:=-10.0D0; r2:=2.0D0; r3:=10.0D0;

(*---------------------*)
PROCEDURE myfun(x:R.T):R.T=
BEGIN
  RETURN (x-r1)*(x-r2)*(x-r3);
END myfun;
(*---------------------*)
PROCEDURE myfun2(x:R.T; VAR f,df:R.T)=
BEGIN
  f:=(x-r1)*(x-r2)*(x-r3);
  df:=(x-r2)*(x-r3)+(x-r1)*(x-r3)+(x-r1)*(x-r2);
END myfun2;

(*=========================*)
(* Quadratics              *)
(*=========================*)
(*-----------------------*)
PROCEDURE TestQuadreal():BOOLEAN=
CONST
  ftn = Module & "TestQuadreal";
VAR
  result:=TRUE;
  alpha,beta,x1,x2:C.T;
  a,b,c:R.T;
BEGIN
  Debug(1,ftn,"begin\n");
  a:=1.0D0; b:=2.0D0; c:=-3.0D0;
  Msg("Solve a*x^2+b*x+c=0 for"
    & "\na=" & RF.Fmt(a)
    & "\nb=" & RF.Fmt(b)
    & "\nc=" & RF.Fmt(c)
    & "\n");

  xRoot.quadreal(a,b,c,alpha,beta,x1,x2);

  Msg("alpha=" & CF.Fmt(alpha)
   & " beta=" & CF.Fmt(beta)
   & "\n");
  Msg("x1=" & CF.Fmt(x1)
   & " x2=" & CF.Fmt(x2)
   & "\n");

  RETURN result;
END TestQuadreal;
(*-----------------------*)
PROCEDURE TestQuadcmplx():BOOLEAN=
CONST
  ftn = Module & "TestComplex";
VAR
  result:=TRUE;
  alpha,beta,x1,x2:C.T;
  a,b,c:C.T;
BEGIN
  Debug(1,ftn,"begin\n");
  a:=C.T{re:=1.0D0,im:=1.0D0};
  b:=C.T{re:=2.0D0,im:=2.0D0};
  c:=C.T{re:=3.0D0,im:=3.0D0};
  Msg("Solve a*x^2+b*x+c=0 for"
    & "\na=" & CF.Fmt(a)
    & "\nb=" & CF.Fmt(b)
    & "\nc=" & CF.Fmt(c)
    & "\n");

  xRoot.quadcmpx(a,b,c,alpha,beta,x1,x2);

  Msg("alpha=" & CF.Fmt(alpha)
   & " beta=" & CF.Fmt(beta)
   & "\n");
  Msg("x1=" & CF.Fmt(x1)
   & " x2=" & CF.Fmt(x2)
   & "\n");

  RETURN result;
END TestQuadcmplx;

CONST
  prec3Style = RF.FmtStyle{style:=Fmt.Style.Fix,prec:=3};
  prec5Style = RF.FmtStyle{style:=Fmt.Style.Fix,prec:=5};

(*=========================*)
(* NonLinears              *)
(*=========================*)
(*----------------------*)
PROCEDURE TestBracket_out():BOOLEAN=
CONST
  ftn = Module & "TestBracket_out";
  maxiter=10;
VAR
  result:=TRUE;
  x1,x2:R.T;
BEGIN
  Debug(1,ftn,"begin\n");
  Msg("true roots: r1=" & RF.Fmt(r1)
               & " r2=" & RF.Fmt(r2)
               & " r3=" & RF.Fmt(r3)
               & " maxiter=" & Fmt.Int(maxiter)
               & "\n");
  FOR i:=1 TO 50 DO
    x1:=5.0D0*FLOAT(i,R.T)-50.0D0; x2:=x1+1.0D0;
    Msg("start at x1=" & RF.Fmt(x1,prec3Style)
              & " x2=" & RF.Fmt(x2,prec3Style));
    TRY
      IF xRoot.bracket_out(myfun,x1,x2,maxiter:=maxiter) THEN
         Msg(" end at x1=" & RF.Fmt(x1,prec3Style)
                  & " x2=" & RF.Fmt(x2,prec3Style)
                  & "\n");
      ELSE Msg(" not found\n");
      END;
    EXCEPT
    | Error(err) => EVAL err;
    END;
  END;
  RETURN result;
END TestBracket_out;
(*----------------------*)
PROCEDURE TestBracket_in():BOOLEAN=
CONST
  ftn = Module & "TestBracket_in";
VAR
  result:=TRUE;
  x1,x2:R.T;
  nb:CARDINAL:=5;
  xb1:=NEW(Array,nb);
  xb2:=NEW(Array,nb);
  n,nbtmp:CARDINAL;
BEGIN
  Debug(1,ftn,"begin\n");
  Msg("true roots: r1=" & RF.Fmt(r1)
               & " r2=" & RF.Fmt(r2)
               & " r3=" & RF.Fmt(r3)
               & "\n");
  x1:=-50.0D0; x2:=+50.0D0;
  Msg("start at x1=" & RF.Fmt(x1,prec3Style)
            & " x2=" & RF.Fmt(x2,prec3Style)
            & " nb=" & Fmt.Int(nb)
            & "\n");
  FOR i:=10 TO 100 BY 10 DO
    n:=i; Msg("n=" & Fmt.Int(n) & "\n");
    nbtmp:=nb; (*so we don't overwrite nb*)
    TRY
      IF xRoot.bracket_in(func:=myfun,x1:=x1,x2:=x2,n:=n,
      xb1:=xb1,xb2:=xb2,nb:=nbtmp) THEN
         FOR j:=0 TO nbtmp-1 DO
         Msg(" found  x1=" & RF.Fmt(xb1[j],prec3Style)
                  & " x2=" & RF.Fmt(xb2[j],prec3Style)
                  & "\n");
         END;
      ELSE Msg(" not found\n");
      END;
    EXCEPT
    | Error(err) => EVAL err;
    END;
  END;
  RETURN result;
END TestBracket_in;
(*----------------------*)
PROCEDURE TestBisect():BOOLEAN=
CONST
  ftn = Module & "TestBisect";
VAR
  result:=TRUE;
  x1,x2,tol,root:R.T;
BEGIN
  Debug(1,ftn,"begin\n");
  Msg("true roots: r1=" & RF.Fmt(r1)
               & " r2=" & RF.Fmt(r2)
               & " r3=" & RF.Fmt(r3)
               & "\n");
  x1:=-1.0D0; x2:=2.9D0; tol:=0.001D0;
  Msg("start at x1=" & RF.Fmt(x1,prec3Style)
            & " x2=" & RF.Fmt(x2,prec3Style)
            & " tol=" & RF.Fmt(tol));
  root:=xRoot.bisect(myfun,x1,x2,tol);
  Msg(" found  root=" & RF.Fmt(root,prec3Style)
    & "\n");
  RETURN result;
END TestBisect;
(*----------------------*)
PROCEDURE TestBrent():BOOLEAN=
CONST
  ftn = Module & "TestBrent";
VAR
  result:=TRUE;
  x1,x2,tol,root:R.T;
BEGIN
  Debug(1,ftn,"begin\n");
  Msg("true roots: r1=" & RF.Fmt(r1)
               & " r2=" & RF.Fmt(r2)
               & " r3=" & RF.Fmt(r3)
               & "\n");
  x1:=-12.0D0; x2:=1.0D0; tol:=0.001D0;
  Msg("start at x1=" & RF.Fmt(x1,prec3Style)
            & " x2=" & RF.Fmt(x2,prec3Style)
            & " tol=" & RF.Fmt(tol));
  TRY
    root:=xRoot.brent(myfun,x1,x2,tol:=tol);
  EXCEPT
  | Error(err) => EVAL err;
  ELSE
    Msg("other error\n");
  END;
  Msg(" found  root=" & RF.Fmt(root,prec3Style)
    & "\n");
  RETURN result;
END TestBrent;
(*----------------------*)
PROCEDURE TestNewtraph():BOOLEAN=
CONST
  ftn = Module & "TestNewtraph";
VAR
  result:=TRUE;
  x1,x2,tol,root:R.T;
  maxiter:CARDINAL;
BEGIN
  Debug(1,ftn,"begin\n");
  Msg("true roots: r1=" & RF.Fmt(r1)
               & " r2=" & RF.Fmt(r2)
               & " r3=" & RF.Fmt(r3)
               & "\n");
  x1:=6.0D0; x2:=5.0D0; tol:=0.001D0; maxiter:=15;
FOR i:=0 TO 10 DO
  x2:=x2+1.1D0;
  Msg("start at x1=" & RF.Fmt(x1,prec3Style)
            & " x2=" & RF.Fmt(x2,prec3Style)
            & " tol=" & RF.Fmt(tol)
            & " maxiter=" & Fmt.Int(maxiter));
  TRY
    root:=xRoot.newtraph(myfun2,x1,x2,tol,maxiter);
    Msg(" found  root=" & RF.Fmt(root,prec5Style)
      & "\n");
  EXCEPT
  | Error(err) => CASE err OF
                      | Err.not_bracketed=>Msg(" not bracketed\n");
                      | Err.out_of_range=>Msg(" jumped out\n");
                      | Err.not_converging=>Msg(" not converging\n");
                      ELSE
                        <*ASSERT FALSE*>
                      END;
  ELSE
    Msg(" other error\n");
  END;
END;
  RETURN result;
END TestNewtraph;



(****************************
(*----------------------*)

PROCEDURE TestLaguer():BOOLEAN=
CONST
  ftn = Module & "TestLaguer";
  n=4; m=n-1;
VAR
  result:=TRUE;
  p:=NEW(na.cVector,n);
  x:C.T;
BEGIN
  Debug(1,ftn,"begin\n");
  Msg("true roots: r1=" & RF.Fmt(r1)
               & " r2=" & RF.Fmt(r2)
               & " r3=" & RF.Fmt(r3)
               & "\n");
  p^:=ARRAY [0..m] OF C.T
      {C.T(200.0,0.0), C.T(-100.0,0.0),
       C.T(-2.0,0.0),  C.T(1.0,0.0)};
FOR i:=0 TO 10 DO
  x:=C.T(FLOAT(i,R.T),0.5);
  Msg("start at x=" & CF.Fmt(x,prec3Style));
  TRY
    na.laguer(p,m,x);
    Msg(" found x=" & CF.Fmt(x,prec5Style)
      & "\n");
  EXCEPT
  | Error(err) => CASE err OF
                      | Err.divide_by_zero=>Msg(" divide by zero\n");
                      | Err.out_of_range=>Msg(" jumped out\n");
                      | Err.not_converging=>Msg(" not converging\n");
                      END;
  ELSE
    Msg(" other error\n");
  END;
END;
  RETURN result;
END TestLaguer;
(*----------------------*)
PROCEDURE TestZRoots():BOOLEAN=
CONST
  ftn = Module & "TestZRoots";
  n=4; m=n-1;
VAR
  result:=TRUE;
  p:=NEW(na.cVector,n);
  roots:na.cVector;
BEGIN
  Debug(1,ftn,"begin\n");
  Msg("true roots: r1=" & RF.Fmt(r1)
               & " r2=" & RF.Fmt(r2)
               & " r3=" & RF.Fmt(r3)
               & "\n");
  p^:=ARRAY [0..m] OF C.T
      {C.T(200.0,0.0), C.T(-100.0,0.0),
       C.T(-2.0,0.0),  C.T(1.0,0.0)};
  TRY
    na.zroots(p,roots,polish:=FALSE);
    Msg("\n     raw roots:");
    FOR i:=0 TO m-1 DO
      Msg(" root[" & Fmt.Int(i) & "]="
        & na.Ctext(roots[i],prec:=4));
    END;
    na.zroots(p,roots,polish:=TRUE);
    Msg("\npolished roots:");
    FOR i:=0 TO m-1 DO
      Msg(" root[" & Fmt.Int(i) & "]="
        & na.Ctext(roots[i],prec:=4));
    END;
  EXCEPT
  | Error(err) => CASE err OF
                      | Err.divide_by_zero=>Msg(" divide by zero\n");
                      | Err.out_of_range=>Msg(" jumped out\n");
                      | Err.not_converging=>Msg(" not converging\n");
                      END;
  ELSE
    Msg(" other error\n");
  END;

  RETURN result;
END TestZRoots;
******************************)
(*-----------------------*)
PROCEDURE TestPowerSeq():BOOLEAN=

  PROCEDURE TestPowerPoly(READONLY x:IR.PowerSumSeq) =
  VAR
    p : IR.T;
    y : REF IR.PowerSumSeq;
  BEGIN
    p := IR.FromPowerSumSeq(x);
    Msg(IPF.Fmt(p) & "\n");
    y := IR.ToPowerSumSeq(p);
    FOR j:=0 TO LAST(y^) DO
      Msg(Fmt.Int(y[j]) & ", ");
      <*ASSERT x[j]=y[j]*>
    END;
    Msg("\n");
  END TestPowerPoly;

  PROCEDURE TestPolyPower(x:IR.T) =
  VAR
    y : IR.T;
    s : REF IR.PowerSumSeq;
  BEGIN
    s := IR.ToPowerSumSeq(x);
    FOR j:=0 TO LAST(s^) DO
      Msg(Fmt.Int(s[j]) & ", ");
    END;
    Msg("\n");
    y := IR.FromPowerSumSeq(s^);
    Msg(IPF.Fmt(y) & "\n");
    <*ASSERT IR.Equal(x,y)*>
  END TestPolyPower;

CONST
  ftn = Module & "TestPowerSeq";
VAR
  result:=TRUE;
  
BEGIN
  Debug(1,ftn,"begin\n");
  TestPowerPoly(IR.PowerSumSeq{0,2,0,2,0,2,0,2,0,2,0,2});
  TestPowerPoly(IR.PowerSumSeq{2,2,2,2,2,2});
  TestPowerPoly(IR.PowerSumSeq{3,3,3,3,3,3});
  TestPowerPoly(IR.PowerSumSeq{-2,2,-2,2});
  TestPowerPoly(IR.PowerSumSeq{-1,-1,-1,-1,-1,-1,-1});
  TestPowerPoly(IR.PowerSumSeq{-2,-2,-2,-2,-2,-2,-2});
  TestPowerPoly(IR.PowerSumSeq{4,8,16,32,64});

  VAR
    p : IR.T;
  BEGIN
    p  := IR.New(7);
    p^ := IR.TBody{1,1,1,1,1,1,1,1}; TestPolyPower(p);
    p^ := IR.TBody{729,243,81,27,9,3,1,1}; TestPolyPower(p);
    p  := IR.New(2);
    p^ := IR.TBody{4,4,1}; TestPolyPower(p);
  END;

  RETURN result;
END TestPowerSeq;

(*-------------------------*)
PROCEDURE TestRoot():BOOLEAN=
CONST ftn = Module & "TestCh09_root";
VAR result:=TRUE;
BEGIN
  NewLine(); EVAL TestQuadreal();
  NewLine(); EVAL TestQuadcmplx();
  (*NewLine(); EVAL TestBracket_out();*)
  (*NewLine(); EVAL TestBracket_in();*)
  (*NewLine(); EVAL TestBisect();*)
  (*NewLine(); EVAL TestBrent();*)
  (*NewLine(); EVAL TestNewtraph();*)
  NewLine(); EVAL TestPowerSeq();

  RETURN result;
END TestRoot;
(*=======================*)
BEGIN
END TestRoot.
