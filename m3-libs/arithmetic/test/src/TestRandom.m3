MODULE tRand EXPORTS test;
(*Copyright (c) 1996, m3na project
Abstract:  Tests for Rand module.

3/16/96    Harry George   Initial version (basic structure)
3/17/96    Warren Smith   Normal, Gamma, and Dirichlet
*)
IMPORT xReal64 AS R,
       xInteger AS I;
IMPORT xRand,xRNG01,xRNG02,xStat,Fmt;
FROM xReal64 IMPORT REAL64;

(*=======================*)
CONST
  Module = "tRand.";

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

(*--------------------------*)
PROCEDURE printstats(name:TEXT;
                     data:R.Array)=
  VAR
    r:xStat.StatRec;
  BEGIN
    xStat.describe(data,r);
    msg("\n" & name);
    msg("\n"
     & " min =" & R.fmt(r.min ,prec:=6,style:=Fmt.Style.Fix)
     & " max =" & R.fmt(r.max ,prec:=6,style:=Fmt.Style.Fix)
     & " mean=" & R.fmt(r.avg ,prec:=6,style:=Fmt.Style.Fix)
     & "\n"
     & " sdev=" & R.fmt(r.sdev,prec:=6,style:=Fmt.Style.Fix)
     & " var =" & R.fmt(r.var ,prec:=6,style:=Fmt.Style.Fix)
     & " skew=" & R.fmt(r.skew,prec:=6,style:=Fmt.Style.Fix)
     & " kurt=" & R.fmt(r.kurt,prec:=6,style:=Fmt.Style.Fix)
     & "\n");
END printstats;
(*----------------------*)
PROCEDURE test_engines():BOOLEAN=
CONST
  ftn = Module & "test_engines";
  N = 10000; n1=0; nn=N-1;
VAR
  result:=TRUE;
  decsrc:=NEW(xRNG01.DECSRC).init(); 
  ran0  :=NEW(xRNG01.ran0).init(); 
  ran1  :=NEW(xRNG01.ran1).init();
  slow  :=NEW(xRNG02.slow).init();
  fast  :=NEW(xRNG02.fast).init();

  data:=NEW(R.Array,N);
  (*------------------------*)
  PROCEDURE do_engine(name:TEXT; rand:xRand.RandomGen)=
  BEGIN
    FOR i:= n1 TO nn DO
      data[i]:=rand.uniform();
    END;
    printstats(name,data);  
  END do_engine;
  (*----------------------*)
BEGIN
  debug(1,ftn,"begin\n");

  msg("N=" & I.fmt(N) & "\n");
  
  do_engine("DECSRC",decsrc);  
  do_engine("ran0  ",ran0);  
  do_engine("ran1  ",ran1);  
  do_engine("slow  ",slow);  
  do_engine("fast  ",fast);  

  RETURN result;   
END test_engines;
(*----------------------*)
PROCEDURE test_Uniform():BOOLEAN=
CONST
  ftn = Module & "test_Uniform";
  N = 10000; n1 = 0; nn = N-1;
VAR
  result:=TRUE;
  rand:=NEW(xRNG02.fast).init();
  data1:=NEW(R.Array,N);
  data2:=NEW(R.Array,N);
  data3:=NEW(R.Array,N);
BEGIN
  debug(1,ftn,"begin\n");

  FOR i:=n1 TO nn DO
    data1[i]:=rand.uniform();
    data2[i]:=rand.uniform(min:=-1.0D0, max:=+1.0D0);
    data3[i]:=rand.uniform(min:=+200.0D0, max:=+1000.0D0);
  END;
  printstats("0..1",data1);
  printstats("-1..+1",data2);
  printstats("200..1000",data3);
  
  RETURN result;   
END test_Uniform;
(*----------------------*)
PROCEDURE test_Exponential():BOOLEAN=
CONST
  ftn = Module & "test_Exponential";
  N = 10000; n1=0; nn=N-1;
VAR
  result:=TRUE;
  rand:=NEW(xRNG02.fast).init();
  data1:=NEW(R.Array,N);
BEGIN
  debug(1,ftn,"begin\n");
  FOR i:=n1 TO nn DO
    data1[i]:=rand.exponential();
  END;
  printstats("exponential, mean=1",data1);

  RETURN result;   
END test_Exponential;

(*----------------------*)
PROCEDURE test_Normal():BOOLEAN=
CONST
  ftn = Module & "test_Normal";
  N = 10000; n1=0; nn=N-1;
VAR
  result:=TRUE;
  rand:=NEW(xRNG02.fast).init();
  data1:=NEW(R.Array,N);
BEGIN
  debug(1,ftn,"begin\n");

  FOR i:=n1 TO nn DO
    data1[i]:=rand.gaussian();
  END;
  printstats("Normal (Gaussian): mean=0, var=1",data1);
  RETURN result;   
END test_Normal;
(*----------------------*)
PROCEDURE test_Gamma():BOOLEAN=
CONST
  ftn = Module & "test_Gamma";
  N = 10000; n1=0; nn=N-1;
VAR
  result:=TRUE;
  rand:=NEW(xRNG02.fast).init();
  data1:=NEW(R.Array,N);
  data2:=NEW(R.Array,N);
  data3:=NEW(R.Array,N);
BEGIN
  debug(1,ftn,"begin\n");

  FOR i:=n1 TO nn DO
    data1[i]:=rand.gamma(1.0d0);
    data2[i]:=rand.gamma(2.5d0);
    data3[i]:=rand.gamma(5.1d0);
  END;
  printstats("gamma(1.0)",data1);
  printstats("gamma(2.5)",data2);
  printstats("gamma(5.1)",data3);
  RETURN result;   
END test_Gamma;
(*----------------------*)
PROCEDURE test_Dirichlet():BOOLEAN=
CONST
  ftn = Module & "test_Dirichlet";
  N = 10000; n1=0; nn=N-1;
VAR
  result:=TRUE;
  rand:=NEW(xRNG02.fast).init();
  data1:=NEW(R.Array,N);
  data2:=NEW(R.Array,N);
  data3:=NEW(R.Array,N);
BEGIN
  debug(1,ftn,"begin\n");

  FOR i:=n1 TO nn DO
  END;
  RETURN result;   
END test_Dirichlet;

(*-------------------------*)
PROCEDURE test_Rand():BOOLEAN=
CONST ftn = Module & "test_Rand";
VAR result:=TRUE;
BEGIN
  newline(); EVAL test_engines();
  newline(); EVAL test_Uniform();
  newline(); EVAL test_Exponential();
  newline(); EVAL test_Normal();
  newline(); EVAL test_Gamma();
  newline(); EVAL test_Dirichlet();
  RETURN result;
END test_Rand;
(*=======================*)
BEGIN
END tRand.
