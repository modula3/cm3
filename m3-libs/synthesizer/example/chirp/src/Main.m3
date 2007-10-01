MODULE Main;

IMPORT Signal,
       ProcessPlot                AS Plot,
       ProcessLinear              AS Linear,
       ProcessControlConstant     AS ControlConstant,
       ProcessControlExponential  AS ControlExponential,
       ProcessOscillatorModulated AS OsciMod,
       ProcessClip                AS Clip,
       ProcessDelay               AS Delay,
       ProcessSplit               AS Split,
       WaveSine,
       ProcessLowPass             AS LowPass,
       ProcessComplexLowPass      AS CLowPass,
       ProcessStateVariableFilter AS UniFilter,
       ProcessMixer               AS Mixer,
       ProcessAmplifier           AS Amplifier,
       ProcessEcho                AS Echo;

IMPORT Thread;

CONST
  Length            = 100000;
  SampleRate        = 44100.0D0;
  CenterFreq        = 700.0D0;
  FreqRange         = 16.0D0;
  ComplexReso       = 20.0D0;
  StateVariableReso = 0.1D0;
  EchoGain          = 0.9D0;

PROCEDURE Chirp (): Signal.T =
  BEGIN
    RETURN
      NEW(Clip.T).init(
        NEW(OsciMod.T).init(
          WaveSine.Wave,
          NEW(ControlExponential.T).init(
            NEW(Linear.T).init(-1.0D0, 2.0D0 / FLOAT(Length, LONGREAL)),
            CenterFreq / SampleRate, FreqRange)), Length);
  END Chirp;

<* FATAL Thread.Alerted, Signal.Error *>

BEGIN
  (* Plot.Stream(Chirp(), SampleRate, Length + 1); *)

  (* lowpass filter *)
  Plot.Stream(NEW(LowPass.T).init(Chirp(), NEW(ControlConstant.T).init(
                                             CenterFreq / SampleRate)),
              SampleRate, Length + 1);

  (* complex lowpass filter *)
  (* The DC amplification is 1.  The resonance amplification should be
     ComplexReso.  It is not so because we do not input a complex chirp,
     which is an essential difference for a filter with complex
     coefficients.  The filter distinguishes between left-rotating and
     right-rotating chirps. *)
  WITH filter = NEW(CLowPass.T).init(
                  Chirp(),
                  NEW(ControlConstant.T).init(CenterFreq / SampleRate),
                  NEW(ControlConstant.T).init(ComplexReso),
                  ARRAY OF
                    CLowPass.Part{
                    CLowPass.Part.Real, CLowPass.Part.Imaginary}) DO
    Plot.MultiStream(
      ARRAY OF Signal.T{filter.channel(0), filter.channel(1)}, SampleRate,
      Length + 1, ComplexReso);
  END;

  (* complex lowpass filter minus low pass = bandpass *)
  WITH resoLowPass = NEW(CLowPass.T).init(
                       Chirp(), NEW(ControlConstant.T).init(
                                  CenterFreq / SampleRate),
                       NEW(ControlConstant.T).init(ComplexReso),
                       ARRAY OF CLowPass.Part{CLowPass.Part.Real}).channel(
                       0),
       lowPass = NEW(LowPass.T).init(Chirp(), NEW(ControlConstant.T).init(
                                                CenterFreq / SampleRate)) DO
    Plot.Stream(
      NEW(Mixer.T).init(resoLowPass,
                        NEW(Amplifier.T).init(
                          lowPass, NEW(ControlConstant.T).init(-1.0D0))),
      SampleRate, Length + 1, ComplexReso);
  END;

  (* state variable filter *)
  WITH filter = NEW(UniFilter.T).init(
                  Chirp(),
                  NEW(ControlConstant.T).init(CenterFreq / SampleRate),
                  NEW(ControlConstant.T).init(StateVariableReso),
                  ARRAY OF
                    UniFilter.Type{
                    UniFilter.Type.Highpass, UniFilter.Type.Lowpass,
                    UniFilter.Type.Bandpass}) DO
    Plot.MultiStream(
      ARRAY OF
        Signal.T{filter.channel(0), filter.channel(1), filter.channel(2)},
      SampleRate, Length + 1, 1.0D0 / StateVariableReso);
  END;

  (* phaser *)
  WITH chirp = Split.New(Chirp(), 2) DO
    <* FATAL Signal.End *>
    BEGIN
      Plot.Stream(NEW(Mixer.T).init(
                    chirp[0], NEW(Delay.T).init(
                                chirp[1], ROUND(SampleRate / CenterFreq))),
                  SampleRate, Length + 1, 2.0D0);
    END;
  END;

  (* comb filter *)
  Plot.Stream(
    NEW(Echo.T).init(Chirp(), ROUND(SampleRate / CenterFreq), EchoGain),
    SampleRate, Length + 1, 1.0D0 / (1.0D0 - EchoGain));
END Main.
