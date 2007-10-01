MODULE Main;

IMPORT ProcessPlay, ProcessOscillator, WaveSine;
IMPORT Signal, Wr, Thread;

<* FATAL Signal.Error, Wr.Failure, Thread.Alerted *>

VAR
  sound := NEW(ProcessOscillator.T).init(WaveSine.Wave, 0.01D0, 0.0D0);

BEGIN
  ProcessPlay.Stream(sound, 44100.0D0);
END Main.
