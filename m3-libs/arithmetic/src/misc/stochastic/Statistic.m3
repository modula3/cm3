MODULE Statistic;
(*Copyright (c) 1996, m3na project

Abstract: <describe>

1/1/96  <name>    Initial version
*)
FROM xUtils IMPORT Error,Err;
IMPORT xReal64 AS R;
FROM xReal64 IMPORT REAL64;

CONST Module = "Statistic.";
(*==========================*)
(*----------------------*)
PROCEDURE describe(data:R.Array;
                VAR r:StatRec) RAISES {Error}=
(*using the 2 pass approach*)
CONST ftn = Module & "describe";
VAR
  N:=NUMBER(data^); n1:=FIRST(data^); nn:=LAST(data^);
  n:=FLOAT(N,REAL64);
  sum:=R.Zero; sumdelta:=R.Zero; delta:=R.Zero; tmp:=R.Zero;
BEGIN
  IF N<2 THEN
    (*need >=2 data points for moment*)
    RAISE Error(Err.bad_size);
  END;

  (*---pass 1---*)
  r.min:=+R.HUGE;  r.max:=-R.HUGE;
  FOR i:=n1 TO nn DO
    IF data[i]<r.min THEN r.min:=data[i]; END;
    IF data[i]>r.max THEN r.max:=data[i]; END;
    sum:=sum+data[i];
  END;
  r.avg:=sum/n;

  (*---pass 2---*)
  r.adev:=R.Zero; r.var:=R.Zero; r.skew:=R.Zero; r.kurt:=R.Zero;
  FOR i:=n1 TO nn DO
    delta:=data[i]-r.avg;
    sumdelta:=sumdelta+delta;
    r.adev:=r.adev+ABS(delta);
    tmp:=delta*delta; r.var :=r.var+tmp;
    tmp:=tmp*delta;   r.skew:=r.skew+tmp;
    tmp:=tmp*delta;   r.kurt:=r.kurt+tmp;
  END;
  r.adev:=r.adev/n;

  (*---correct var---*)
  r.var:=(r.var-sumdelta*sumdelta/n)/(n-R.One);

  (*---calculate moments---*)
  r.sdev:=R.sqrt(r.var);
  IF r.var > R.TINY THEN
    r.skew:=r.skew/(n*r.var*r.sdev);
    r.kurt:=(r.kurt/(n*r.var*r.var))-3.0D0;
  ELSE
    r.skew:=R.Zero; r.kurt:=R.Zero;
  END;
END describe;
(*---------------------*)
PROCEDURE avevar(data:R.Array; VAR ave,var:REAL64)=
VAR
  N:=NUMBER(data^); n1:=FIRST(data^); nn:=LAST(data^);
  n:=FLOAT(N,REAL64);
  sum,sumdelta,delta:REAL64;
BEGIN
  sum:=R.Zero;
  FOR i:=n1 TO nn DO
    sum:=sum + data[i];
  END;
  ave:=sum/n;
  sumdelta:=R.Zero; var:=R.Zero;
  FOR i:=n1 TO nn DO
    delta:=data[i]-ave;
    sumdelta:=sumdelta + delta;
    var:=var+sumdelta*sumdelta;
  END;
  var:=(var-sumdelta*sumdelta/n)/(n-R.One);
END avevar;
(*---------------------*)
PROCEDURE ttest(data1,data2:R.Array;
                VAR t,    (*Student's t-test*)
                    prob  (*probability of insignificance*)
                    :REAL64) RAISES {Error}=
(*Given data and data2 equal length R.Arrays,
find t, which shows how close the means are, and
find prob, which is small if this similarity is unlikely to
be due to chance.  Note that their variances need to be
similar.*)
CONST ftn = Module & "ttest";
VAR
  N1:=NUMBER(data1^);   N2:=NUMBER(data2^);
  n1:=FLOAT(N1,REAL64); n2:=FLOAT(N2,REAL64);
  avg1,var1,avg2,var2,sd,df:REAL64;
  vardiff:REAL64;
BEGIN
  avevar(data1,avg1,var1);
  avevar(data2,avg2,var2);
  vardiff:=ABS((var1-var2)/var2);
  IF vardiff>5.0D0 THEN
    RAISE Error(Err.out_of_range);
  END;
  df:=n1+n2-R.Two;
  sd:=R.sqrt(((n1-R.One)*var1+(n2-R.One)*var2)/df*(R.One/n1+R.One/n2));
  t:=ABS((avg1-avg2)/sd);
  prob:=R.betai(0.5D0*df,0.5D0,df/(df+t*t));
END ttest;

(*--------------------*)
PROCEDURE ftest(data1,data2:R.Array;
            VAR f,    (*F value*)
                prob  (*probability of significance*)
                :REAL64) RAISES {Error}=
(*do F-test, returning F and the probability that
a difference between vars is due to chance*)
CONST ftn = Module & "ftest";
VAR
  ave1,ave2,var1,var2,df1,df2:REAL64;
BEGIN
  avevar(data1,ave1,var1);
  avevar(data2,ave2,var2);
  IF var1<R.TINY OR var2<R.TINY THEN
    (*vars cannot = 0*)
    RAISE Error(Err.out_of_range);
  END;
  IF var2>var1 THEN
    f:=var2/var1;
    df1:=FLOAT(NUMBER(data2^)-1,REAL64);
    df2:=FLOAT(NUMBER(data1^)-1,REAL64);
  ELSE
    f:=var1/var2;
    df1:=FLOAT(NUMBER(data1^)-1,REAL64);
    df2:=FLOAT(NUMBER(data2^)-1,REAL64);
  END;
  prob:=R.Two*R.betai(0.5D0*df2,0.5D0*df1,df2/(df2+df1*f));
  IF prob > R.One THEN prob:=R.Two-prob; END;
END ftest;
(*----------------------*)
PROCEDURE chi_sqr1
               (bins:R.Array;     (*actual bin counts*)
                ebins:R.Array;     (*expected bin counts*)
                constraints:CARDINAL:=1;
                VAR df:REAL64;    (*degrees of freedom*)
                VAR chsq:REAL64;  (*chi squared*)
                VAR prob:REAL64   (*probability of significance*)
                ) RAISES {Error}=
(*bins has an integer number of events in each bin, ebins
has the expected number in each bin (possibly non integer),
contraints gives the constraint count which reduces the
df from the number of bins.  chsq then is a measure of the
difference in the bin-by-bin numbers, while prob gives the
significance of that measure.  Big chsq means big difference,
big prob means big chance this large chsq came from pure random
events.
*)
CONST ftn = Module & "chi_sqr1";
VAR
  n:=NUMBER(bins^); n1:=0; nn:=n-1;
  m:=NUMBER(ebins^);
  tmp:REAL64;
BEGIN
  IF m#n THEN
    RAISE Error(Err.bad_size);
  END;

  chsq:=R.Zero;
  FOR i:=n1 TO nn DO
    IF bins[i]<5.0D0 OR ebins[i]<5.0D0 THEN
      RAISE Error(Err.need_more_data);
    END;
    tmp:=bins[i]-ebins[i];
    chsq:=chsq+tmp*tmp/ebins[i];
  END;
  df:=FLOAT(n-constraints,REAL64);
  prob:=R.gamma_q(0.5D0*df,0.5D0*chsq);
END chi_sqr1;
(*----------------------------*)
PROCEDURE chi_sqr2
               (bins1:R.Array;    (*actual bin1 counts*)
                bins2:R.Array;     (*actual bin2 counts*)
                constraints:CARDINAL:=1;
                VAR df:REAL64;    (*degrees of freedom*)
                VAR chsq:REAL64;  (*chi squared*)
                VAR prob:REAL64   (*probability of significance*)
                ) RAISES {Error}=
(*bins1 and bins2 have an integer number of events in each bin,
contraints gives the constraint count which reduces the
df from the number of bins.  chsq then is a measure of the
difference in the bin-by-bin numbers, while prob gives the
significance of that measure.  Big chsq means big difference,
big prob means big chance this large chsq came from pure random
events.
*)
CONST ftn = Module & "chi_sqr2";
VAR
  n:=NUMBER(bins1^); n1:=0; nn:=n-1;
  m:=NUMBER(bins2^);
  tmp:REAL64;
BEGIN
  IF m#n THEN
    RAISE Error(Err.bad_size);
  END;

  chsq:=R.Zero;
  FOR i:=n1 TO nn DO
    IF bins1[i]<5.0D0 OR bins2[i]<5.0D0 THEN
      RAISE Error(Err.need_more_data);
    END;
    tmp:=bins1[i]-bins2[i];
    chsq:=chsq+tmp*tmp/(bins1[i]+bins2[i]);
  END;

  df:=FLOAT(n-constraints,REAL64);
  prob:=R.gamma_q(0.5D0*df,0.5D0*chsq);
END chi_sqr2;

(*==========================*)
BEGIN
END Statistic.
