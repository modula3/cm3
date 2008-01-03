MODULE Main;

IMPORT ProcessPlay,
       ProcessOscillator,
       ProcessOscillatorModulated,
       WaveSquare,
       WaveSine,
       WaveSaw,
       ProcessClip,
       ProcessAmplifier,
       ProcessComplexLowPass        AS Filter,
       ProcessExponential,
       ProcessControlConstant,
       ProcessControlExponential;
IMPORT Signal, Wr, Thread, Math;

<* FATAL Signal.Error, Wr.Failure, Thread.Alerted *>

TYPE
  Ton = RECORD
          laenge  : CARDINAL;
          tonhoehe: INTEGER;
        END;

CONST
  C = 0;
  D = 2;
  E = 4;
  F = 5;
  G = 7;
  H = 9;

  Lied = ARRAY OF
           Ton{Ton{1, C}, Ton{1, D}, Ton{1, E}, Ton{1, F}, Ton{2, G},
               Ton{2, G}, Ton{1, H}, Ton{1, H}, Ton{1, H}, Ton{1, H},
               Ton{4, G}, Ton{1, H}, Ton{1, H}, Ton{1, H}, Ton{1, H},
               Ton{4, G}, Ton{1, F}, Ton{1, F}, Ton{1, F}, Ton{1, F},
               Ton{2, E}, Ton{2, E}, Ton{1, G}, Ton{1, G}, Ton{1, G},
               Ton{1, G}, Ton{4, C}};

BEGIN
  FOR j := FIRST(Lied) TO LAST(Lied) DO
    ProcessPlay.Stream(
      NEW(ProcessClip.T).init(
        NEW(Filter.T).init(
          NEW(ProcessOscillator.T).init(
            WaveSaw.Wave,
            0.008D0 * Math.pow(
              2.0D0, FLOAT(Lied[j].tonhoehe, LONGREAL) / 12.0D0), 0.0D0),
          NEW(ProcessControlExponential.T).init(
            NEW(ProcessExponential.T).init(50000.0D0), 0.001D0, 150.0D0),
          NEW(ProcessControlConstant.T).init(30.0D0),
          ARRAY OF Filter.Part{Filter.Part.Real}).channel(0),
        11025 * Lied[j].laenge), 44100.0D0);
  END;

  FOR j := FIRST(Lied) TO LAST(Lied) DO
    ProcessPlay.Stream(
      NEW(ProcessClip.T).init(NEW(ProcessAmplifier.T).init(
                                NEW(ProcessOscillatorModulated.T).init(
                                  WaveSquare.Wave,
                                  NEW(ProcessControlExponential.T).init(
                                    NEW(ProcessOscillator.T).init(
                                      WaveSine.Wave, 2.0D-4, 0.0D0),
                                    0.008D0 * Math.pow(
                                      2.0D0,
                                      FLOAT(Lied[j].tonhoehe, LONGREAL)
                                        / 12.0D0), 1.02D0), 0.0D0),
                                NEW(ProcessExponential.T).init(5000.0D0)),
                              11025 * Lied[j].laenge), 44100.0D0);
  END;
END Main.
