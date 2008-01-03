MODULE Main;

IMPORT Signal,
       ProcessPlot,
       ProcessLoad,
       WaveSine,
       ProcessOscillatorSine,
       ProcessOscillator       AS Osci,
       ProcessClip             AS Clip;

IMPORT Thread;

CONST
  Size            = 4410;
  SizeDetune      = 4300;
  SampleRate      = 44100.0D0;
  Frequency       = 880.0D0;
  FrequencyDetune = 890.0D0;

<* FATAL Thread.Alerted, Signal.Error *>

BEGIN
  IF FALSE THEN
    (* pre-computed *)
    WITH sound = ProcessOscillatorSine.Do(Size, Frequency / SampleRate) DO
      ProcessPlot.Array(sound^, SampleRate);
    END;
  ELSE
    (* realtime *)
    WITH sound0 = NEW(Clip.T).init(
                    NEW(Osci.T).init(WaveSine.Wave, Frequency / SampleRate),
                    Size),
         sound1 = NEW(Clip.T).init(
                    NEW(Osci.T).init(
                      WaveSine.Wave, FrequencyDetune / SampleRate),
                    SizeDetune) DO
      ProcessPlot.MultiStream(
        ARRAY OF Signal.T{sound0, sound1}, SampleRate);
    END;
    WITH sound = NEW(ProcessLoad.T).init(
                   "/home/thielema/programming/haskell/"
                     & "synthesizer/guitar/Melodie/Melodie5.aiff") DO
      ProcessPlot.Stream(sound, SampleRate, 1180);
    END;
    WITH sound = NEW(Osci.T).init(WaveSine.Wave, Frequency / SampleRate) DO
      ProcessPlot.Stream(sound, SampleRate);
    END;
  END;
END Main.
