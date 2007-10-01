MODULE TestControl;

IMPORT ProcessOscillatorSine, ProcessControlExponential;
IMPORT Signal;

IMPORT Fmt, Time;

IMPORT UnitTest, UnitTestList, UnitTestAtom;


EXCEPTION SoundsDiffer;

PROCEDURE CheckExponential (SELF: UnitTestAtom.T; ) =
  CONST
    numSamples = 100000;
    center     = 0.01D0;
    depth      = 10.0D0;

  VAR
    input := ProcessOscillatorSine.Do(numSamples, 0.01D0);
    exp0, exp1: Signal.RefArray;

  BEGIN
    WITH startTime = Time.Now() DO
      exp0 := ProcessControlExponential.DoPow(input^, center, depth);
      SELF.message(
        "power: " & Fmt.LongReal(Time.Now() - startTime) & " seconds.\n");
    END;

    WITH startTime = Time.Now() DO
      exp1 := ProcessControlExponential.Do(input^, center, depth);
      SELF.message(
        "log-exp: " & Fmt.LongReal(Time.Now() - startTime) & " seconds.\n");
    END;

    TRY
      FOR j := FIRST(input^) TO LAST(input^) DO
        IF ABS(exp0[j] - exp1[j]) > 1.0D-16 THEN
          SELF.error("Signal differs at " & Fmt.Int(j) & "\n");
          RAISE SoundsDiffer;
        END;
      END;
    EXCEPT
      SoundsDiffer =>
    END;

  END CheckExponential;

PROCEDURE Test (): UnitTest.T =
  BEGIN
    RETURN NEW(UnitTestList.T).init(
             "Control", ARRAY OF
                          UnitTest.T{NEW(UnitTestAtom.T,
                                         test := CheckExponential).init(
                                       "exponential")});
  END Test;

BEGIN
END TestControl.
