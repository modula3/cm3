MODULE TestRandom EXPORTS Test;
(*Copyright (c) 1996, m3na project
Abstract:  TestS for Rand module.

3/16/96    Harry George   Initial version (basic structure)
3/17/96    Warren Smith   Normal, Gamma, and Dirichlet
*)
IMPORT xReal64 AS R,
       xInteger AS I;
IMPORT xRand,xRNG01,xRNG02,xStat,Fmt;
FROM xReal64 IMPORT REAL64;

(*=======================*)
CONST
  Module = "TestRandom.";

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

(*--------------------------*)
PROCEDURE printstats(name:TEXT;
                     data:R.Array)=
  VAR
    r:xStat.StatRec;
  BEGIN
    xStat.describe(data,r);
    Msg("\n" & name);
    Msg("\n"
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
PROCEDURE TestEngines():BOOLEAN=
CONST
  ftn = Module & "TestEngines";
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
  Debug(1,ftn,"begin\n");

  Msg("N=" & I.fmt(N) & "\n");
  
  do_engine("DECSRC",decsrc);  
  do_engine("ran0  ",ran0);  
  do_engine("ran1  ",ran1);  
  do_engine("slow  ",slow);  
  do_engine("fast  ",fast);  

  RETURN result;   
END TestEngines;
(*----------------------*)
PROCEDURE TestUniform():BOOLEAN=
CONST
  ftn = Module & "TestUniform";
  N = 10000; n1 = 0; nn = N-1;
VAR
  result:=TRUE;
  rand:=NEW(xRNG02.fast).init();
  data1:=NEW(R.Array,N);
  data2:=NEW(R.Array,N);
  data3:=NEW(R.Array,N);
BEGIN
  Debug(1,ftn,"begin\n");

  FOR i:=n1 TO nn DO
    data1[i]:=rand.uniform();
    data2[i]:=rand.uniform(min:=-1.0D0, max:=+1.0D0);
    data3[i]:=rand.uniform(min:=+200.0D0, max:=+1000.0D0);
  END;
  printstats("0..1",data1);
  printstats("-1..+1",data2);
  printstats("200..1000",data3);
  
  RETURN result;   
END TestUniform;
(*----------------------*)
PROCEDURE TestExponential():BOOLEAN=
CONST
  ftn = Module & "TestExponential";
  N = 10000; n1=0; nn=N-1;
VAR
  result:=TRUE;
  rand:=NEW(xRNG02.fast).init();
  data1:=NEW(R.Array,N);
BEGIN
  Debug(1,ftn,"begin\n");
  FOR i:=n1 TO nn DO
    data1[i]:=rand.exponential();
  END;
  printstats("exponential, mean=1",data1);

  RETURN result;   
END TestExponential;

(*----------------------*)
PROCEDURE TestNormal():BOOLEAN=
CONST
  ftn = Module & "TestNormal";
  N = 10000; n1=0; nn=N-1;
VAR
  result:=TRUE;
  rand:=NEW(xRNG02.fast).init();
  data1:=NEW(R.Array,N);
BEGIN
  Debug(1,ftn,"begin\n");

  FOR i:=n1 TO nn DO
    data1[i]:=rand.gaussian();
  END;
  printstats("Normal (Gaussian): mean=0, var=1",data1);
  RETURN result;   
END TestNormal;
(*----------------------*)
PROCEDURE TestGamma():BOOLEAN=
CONST
  ftn = Module & "TestGamma";
  N = 10000; n1=0; nn=N-1;
VAR
  result:=TRUE;
  rand:=NEW(xRNG02.fast).init();
  data1:=NEW(R.Array,N);
  data2:=NEW(R.Array,N);
  data3:=NEW(R.Array,N);
BEGIN
  Debug(1,ftn,"begin\n");

  FOR i:=n1 TO nn DO
    data1[i]:=rand.gamma(1.0d0);
    data2[i]:=rand.gamma(2.5d0);
    data3[i]:=rand.gamma(5.1d0);
  END;
  printstats("gamma(1.0)",data1);
  printstats("gamma(2.5)",data2);
  printstats("gamma(5.1)",data3);
  RETURN result;   
END TestGamma;
(*----------------------*)
PROCEDURE TestDirichlet():BOOLEAN=
CONST
  ftn = Module & "TestDirichlet";
  N = 10000; n1=0; nn=N-1;
VAR
  result:=TRUE;
  rand:=NEW(xRNG02.fast).init();
  data1:=NEW(R.Array,N);
  data2:=NEW(R.Array,N);
  data3:=NEW(R.Array,N);
BEGIN
  Debug(1,ftn,"begin\n");

  FOR i:=n1 TO nn DO
  END;
  RETURN result;   
END TestDirichlet;

(*-------------------------*)
PROCEDURE TestRandom():BOOLEAN=
CONST ftn = Module & "TestRandom";
VAR result:=TRUE;
BEGIN
  NewLine(); EVAL TestEngines();
  NewLine(); EVAL TestUniform();
  NewLine(); EVAL TestExponential();
  NewLine(); EVAL TestNormal();
  NewLine(); EVAL TestGamma();
  NewLine(); EVAL TestDirichlet();
  RETURN result;
END TestRandom;
(*=======================*)
BEGIN
END TestRandom.
