MODULE Main;

IMPORT Signal,
       ProcessPlay,
       ProcessSave,
       WaveSine,
       ProcessOscillator AS Osci,
       ProcessClip       AS Clip;

IMPORT Wr, Thread;

CONST
  SampleRate = 44100.0D0;
  Frequency  = 880.0D0;
  Detune     = 1.001D0;
  Length     = 6000;

<* FATAL Wr.Failure, Thread.Alerted, Signal.Error *>

BEGIN
  WITH sound0 = NEW(Clip.T).init(
                  NEW(Osci.T).init(
                    WaveSine.Wave, Frequency * Detune / SampleRate), Length),
       sound1 = NEW(Osci.T).init(
                  WaveSine.Wave, Frequency / Detune / SampleRate) DO
    ProcessSave.MultiStream(
      ARRAY OF Signal.T{sound0, sound1}, SampleRate, "tone.aiff");
  END;

  WITH sound0 = NEW(Osci.T).init(
                  WaveSine.Wave, Frequency * Detune / SampleRate),
       sound1 = NEW(Osci.T).init(
                  WaveSine.Wave, Frequency / Detune / SampleRate) DO
    ProcessPlay.MultiStream(ARRAY OF Signal.T{sound0, sound1}, SampleRate);
  END;
END Main.
