MODULE Main;

IMPORT ProcessInOut, ProcessPlay, ProcessSave, ProcessOscillatorSine;
IMPORT Wr, Thread;

<* FATAL Wr.Failure, Thread.Alerted *>
BEGIN
  ProcessSave.Array(
    ProcessOscillatorSine.Do(11025, 0.01D0, 0.0D0)^, 44100.0D0,
    "tone.aiff", wordSize := ProcessInOut.WordSize.Bits8);

  FOR j := 4 TO 8 DO
    ProcessPlay.Array(
      ProcessOscillatorSine.Do(
        11025, 0.0025D0 * FLOAT(j, LONGREAL), 0.0D0)^, 44100.0D0);
  END;
END Main.
