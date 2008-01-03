MODULE Main;

IMPORT Signal,
       ProcessLoad,
       ProcessPlay,
       ProcessControlConstant     AS ControlConstant,
       ProcessControlExponential  AS ControlExponential,
       ProcessOscillatorSine,
       ProcessOscillator,
       WaveSine,
       ProcessStateVariableFilter AS Filter,
       ProcessComplexLowPass      AS CFilter;

IMPORT Wr, Rd, Thread;

CONST
  FileName       = "Rainbow.aiff";
  StereoFileName = "superfrog.ogg";
  SampleRate     = 44100.0D0;
  BandFreq = 700.0D0;            (* band center frequency of the
                                    unmodulated filter *)
  ModDepth = 1.5D0;              (* modulation depth relative to the band
                                    center frequency *)
  BandWidth = 0.15D0;            (* band width of the filter, reciprocal to
                                    the resonance *)
  ComplexReso = 20.0D0;          (* resonance of the complex lowpass *)
  LFORate = 0.1D0;               (* frequency of the Low Frequency
                                    Oscillator *)

<* FATAL Wr.Failure, Rd.Failure, Thread.Alerted, Signal.Error *>

BEGIN
  IF FALSE THEN
    (* pre-computed *)
    WITH x   = ProcessLoad.Do(FileName)^,
         lfo = ProcessOscillatorSine.Do(NUMBER(x), LFORate / SampleRate)^ DO
      ProcessPlay.Array(
        Filter.Do(
          x, ControlExponential.Do(lfo, BandFreq / SampleRate, ModDepth)^,
          ControlConstant.Do(NUMBER(x), BandWidth)^)[Filter.Type.Lowpass]^,
        SampleRate);
    END;
  ELSE
    (* realtime *)

    (* complex lowpass filter *)
    WITH x = NEW(ProcessLoad.Multi).init(
               2, ARRAY OF CARDINAL{0, 1}, StereoFileName),
         lfo0 = NEW(ControlExponential.T).init(
                  NEW(ProcessOscillator.T).init(
                    WaveSine.Wave, LFORate / SampleRate),
                  BandFreq / SampleRate, ModDepth),
         lfo1 = NEW(ControlExponential.T).init(
                  NEW(ProcessOscillator.T).init(
                    WaveSine.Wave, LFORate / SampleRate, 0.15D0),
                  BandFreq / SampleRate, ModDepth) DO
      ProcessPlay.MultiStream(
        ARRAY OF
          Signal.T{
          NEW(CFilter.T).init(
            x.channel(0), lfo0, NEW(ControlConstant.T).init(ComplexReso),
            ARRAY OF CFilter.Part{CFilter.Part.Real}).channel(0),
          NEW(CFilter.T).init(
            x.channel(1), lfo1, NEW(ControlConstant.T).init(ComplexReso),
            ARRAY OF CFilter.Part{CFilter.Part.Real}).channel(0)},
        SampleRate);
    END;

    (* state variable filter *)
    WITH x = NEW(ProcessLoad.T).init(FileName),
         lfo = NEW(ProcessOscillator.T).init(
                 WaveSine.Wave, LFORate / SampleRate) DO
      ProcessPlay.Stream(
        NEW(Filter.T).init(
          x, NEW(ControlExponential.T).init(
               lfo, BandFreq / SampleRate, ModDepth),
          NEW(ControlConstant.T).init(BandWidth),
          ARRAY OF Filter.Type{Filter.Type.Lowpass}).channel(0), SampleRate);
    END;
  END;
END Main.
