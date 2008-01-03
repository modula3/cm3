MODULE Main;

IMPORT ProcessPlay,
       ProcessOscillator,
       ProcessOscillatorModulated,
       WaveSine,
       ProcessControlExponential;
IMPORT Signal, Wr, Thread;

<* FATAL Signal.Error, Wr.Failure, Thread.Alerted *>

BEGIN

  ProcessPlay.Stream(
    NEW(ProcessOscillatorModulated.T).init(
      WaveSine.Wave, NEW(ProcessControlExponential.T).init(
                       NEW(ProcessOscillator.T).init(
                         WaveSine.Wave, 1.0D-4, 0.0D0), 0.0099D0, 2.0D0),
      0.0D0), 44100.0D0);

END Main.
