MODULE TestDaubechies;

IMPORT LongRealSignal            AS S,
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
        ARRAY OF TEXT{Fmt.Int(n),x.fmt()}));
    END;
  END Test;

BEGIN
END TestDaubechies.
