MODULE TestDaubechies;

IMPORT LongRealBasic             AS R,
       LongRealTrans             AS RT,
       LongRealSignal            AS S,
       LongRealSignalFmtLex      AS SF,
       LongRealDaubechiesWavelet AS Daub,
       IO, Fmt, Thread, Wr;

PROCEDURE Test()=
  VAR
    x : S.T;
  <*FATAL Thread.Alerted, Wr.Failure*>
  BEGIN
    FOR n:=0 TO 10 DO
      x := Daub.FilterAbsSqr(n);
      IO.Put(Fmt.FN("%s: %s\n",
        ARRAY OF TEXT{Fmt.Int(n),x.scale(R.Scalb(1.0D0,4*n-2)).fmt()}));
    END;
  END Test;

BEGIN
END TestDaubechies.
