MODULE TestSplit;

IMPORT ProcessOscillator, WaveSine, ProcessSplit;
IMPORT Signal;

IMPORT LongRealSeq;
IMPORT Fmt, Random, Thread, Time;

IMPORT UnitTest, UnitTestAtom;

<* FATAL Signal.End, Signal.Error, Thread.Alerted *>

EXCEPTION
  SoundsDiffer;

(* ToDo: Test with sounds which throw End *)
PROCEDURE CheckSplit (SELF: UnitTestAtom.T; ) =
  CONST
    numSplits  = 10;
    numSamples = 100000;
  VAR
    startTime    := Time.Now();
    input        := NEW(ProcessOscillator.T).init(WaveSine.Wave, 0.01D0);
    split        := ProcessSplit.New(input, numSplits);
    rnd          := NEW(Random.Default).init();
    sound        := NEW(REF ARRAY OF LongRealSeq.T, numSplits);
    curNumSplits := numSplits;
  BEGIN
    FOR j := FIRST(sound^) TO LAST(sound^) DO
      sound[j] := NEW(LongRealSeq.T).init(sizeHint := numSamples);
    END;

    FOR j := 0 TO numSplits * numSamples - 1 DO
      WITH k = rnd.integer(0, curNumSplits - 1) DO
        sound[k].addhi(split[k].get());
        (* if the destination buffer is full, swap it to the current end of
           the array *)
        IF sound[k].size() = numSamples THEN
          split[k].exit();
          DEC(curNumSplits);
          VAR swap := split[k];
          BEGIN
            split[k] := split[curNumSplits];
            split[curNumSplits] := swap;
          END;
          VAR swap := sound[k];
          BEGIN
            sound[k] := sound[curNumSplits];
            sound[curNumSplits] := swap;
          END;
        END;
      END;
    END;
    <* ASSERT curNumSplits = 0 *> (* if this check fails, the test
                                     procedure is broken *)

    FOR j := 0 TO numSplits - 1 DO
      (* Check if the test works properly. *)
      <* ASSERT sound[j].size() = numSamples *>
    END;

    (* Check if after all the swaps in 'sound' all references are
       different. *)
    FOR j := 0 TO numSplits - 1 DO
      FOR l := j + 1 TO numSplits - 1 DO
        <* ASSERT sound[j] # sound[l] *>
      END;
    END;

    (* Now, check if all splits contain the right data. *)
    TRY
      input := input.init(WaveSine.Wave, 0.01D0);
      FOR k := 0 TO numSamples - 1 DO
        WITH x = input.get() DO
          FOR j := 0 TO numSplits - 1 DO
            IF sound[j].get(k) # x THEN RAISE SoundsDiffer; END;
          END;
        END;
      END;
    EXCEPT
    | SoundsDiffer => SELF.error("Splitted sounds differ.\n");
    END;
    input.exit();

    SELF.message(Fmt.LongReal(Time.Now() - startTime) & " seconds.\n");
  END CheckSplit;

PROCEDURE Test (): UnitTest.T =
  BEGIN
    RETURN NEW(UnitTestAtom.T, test := CheckSplit).init("split");
  END Test;

BEGIN
END TestSplit.
