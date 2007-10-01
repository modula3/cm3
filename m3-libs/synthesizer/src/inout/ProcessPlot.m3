MODULE ProcessPlot;

IMPORT Signal;
IMPORT LongRealPLPlot AS PL;
IMPORT Thread;


PROCEDURE Array (READONLY x     : Signal.Array;
                          rate  : LONGREAL;
                          window: CARDINAL;
                          yRange: LONGREAL;     ) =
  VAR t := NEW(Signal.RefArray, window);
  BEGIN
    PL.Init();
    FOR j := FIRST(x) TO LAST(x) BY window DO
      WITH curWindow = MIN(window, NUMBER(x) - j) DO
        FOR k := 0 TO curWindow - 1 DO
          t[k] := FLOAT(j + k, LONGREAL) / rate;
        END;
        PL.SetFGColorDiscr(1);
        PL.SetEnvironment(
          t[0], t[0] + FLOAT(window - 1, LONGREAL) / rate, -yRange, yRange);
        PL.SetFGColorDiscr(2);
        PL.PlotLines(SUBARRAY(t^, 0, curWindow), SUBARRAY(x, j, curWindow));
      END;
    END;
    PL.Exit();
  END Array;

PROCEDURE Stream
  (sig: Signal.T; rate: LONGREAL; window: CARDINAL; yRange: LONGREAL; )
  RAISES {Signal.Error, Thread.Alerted} =
  BEGIN
    MultiStream(ARRAY OF Signal.T{sig}, rate, window, yRange);
  END Stream;

PROCEDURE MultiStream (READONLY sigs  : ARRAY OF Signal.T;
                                rate  : LONGREAL;
                                window: CARDINAL;
                                yRange: LONGREAL;          )
  RAISES {Signal.Error, Thread.Alerted} =
  VAR
    t        := NEW(Signal.RefArray, window);
    x        := NEW(REF ARRAY OF Signal.Array, NUMBER(sigs), window);
    winSizes := NEW(REF ARRAY OF CARDINAL, NUMBER(sigs));
    j        : CARDINAL := 0;
    numActive: CARDINAL := NUMBER(sigs);
  BEGIN
    PL.Init();
    WHILE numActive > 0 DO
      VAR maxWindow: CARDINAL := 0;
      BEGIN
        numActive := NUMBER(sigs);
        FOR channel := FIRST(sigs) TO LAST(sigs) DO
          WITH curWindow = winSizes[channel],
               xc        = x[channel],
               sig       = sigs[channel]      DO
            TRY
              curWindow := 0;
              WHILE curWindow < window DO
                xc[curWindow] := sig.get();
                INC(curWindow);
              END;
            EXCEPT
            | Signal.End => DEC(numActive);
            END;
            maxWindow := MAX(maxWindow, curWindow);
          END;
        END;
        FOR k := 0 TO maxWindow - 1 DO
          t[k] := FLOAT(j + k, LONGREAL) / rate;
        END;
        INC(j, window);
        PL.SetFGColorDiscr(1);
        PL.SetEnvironment(
          t[0], t[0] + FLOAT(window - 1, LONGREAL) / rate, -yRange, yRange);
        (* t[curWindow - 1] as yMax would lead to a smaller window size for
           the last block *)
        FOR i := FIRST(sigs) TO LAST(sigs) DO
          IF winSizes[i] > 0 THEN
            PL.SetFGColorDiscr(i + 2);
            PL.PlotLines(
              SUBARRAY(t^, 0, winSizes[i]), SUBARRAY(x[i], 0, winSizes[i]));
          END;
        END;
      END;
    END;
    FOR channel := FIRST(sigs) TO LAST(sigs) DO sigs[channel].exit(); END;
    PL.Exit();
  END MultiStream;

BEGIN
END ProcessPlot.
