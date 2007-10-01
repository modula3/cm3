MODULE TestModulation;

IMPORT ProcessSplit,
       ProcessOscillator            AS Osci,
       ProcessOscillatorModulated   AS OsciMod,
       WaveSine,
       ProcessControlLinear,
       ProcessControlExponential,
       ProcessFrequencyModulation,
       ProcessPhaseModulation;
IMPORT Signal, SignalControl, Interpolation;
(* IMPORT ProcessPlot, ProcessPlay; *)

IMPORT Math;
IMPORT Fmt;
IMPORT Thread;

IMPORT UnitTest, UnitTestList, UnitTestAtom;

<* FATAL Signal.End, Signal.Error, Thread.Alerted *>

EXCEPTION
  TestFailure;


(* Test the modulated sine oscillator in the following way: Create
   modulated cosine and sine curves.  Together they can be considered as
   real and imaginary of a complex valued curve.  We check that each
   complex value has magnitude 1 and that the complex arguments increases
   like the frequency. *)
PROCEDURE CheckOscillator (SELF: UnitTestAtom.T; ) =
  CONST
    freqTol    = 1.0D-12;
    ampTol     = 1.0D-12;
    numSamples = 20000;
    center     = 1.0D0;
    depth      = 10.0D0;
    osciFreq   = 0.01D0;
    lfoFreq    = 0.0000572D0;

  VAR
    lfo := NEW(Osci.T).init(WaveSine.Wave, lfoFreq);
    controls := ProcessSplit.New(NEW(ProcessControlExponential.T).init(
                                   lfo, osciFreq * center, depth), 3);
    osciSine   := NEW(OsciMod.T).init(WaveSine.Wave, controls[0], 0.0D0);
    osciCosine := NEW(OsciMod.T).init(WaveSine.Wave, controls[1], 0.25D0);
    lastPhase  := Math.atan2(osciCosine.get(), osciSine.get());

  BEGIN
    (*
    ProcessPlot.Stream(osciSine);
    ProcessPlay.Stream(osciSine, 44100.0D0);
    *)
    TRY
      FOR j := 0 TO numSamples - 1 DO
        VAR
          cos       := osciCosine.get();
          sin       := -osciSine.get();
          curPhase  := Math.atan2(cos, sin);
          diffPhase := curPhase - lastPhase;
          amp       := cos * cos + sin * sin;
          ctrl      := 2.0D0 * Math.Pi * controls[2].get();
        BEGIN
          IF diffPhase < 0.0D0 THEN
            diffPhase := diffPhase + 2.0D0 * Math.Pi;
          END;
          lastPhase := curPhase;
          IF diffPhase < 0.0D0 OR ABS(diffPhase - ctrl) > freqTol THEN
            SELF.error("Frequencies differs at " & Fmt.Int(j) & ", "
                         & Fmt.LongReal(diffPhase) & ", "
                         & Fmt.LongReal(ctrl) & "\n");
            RAISE TestFailure;
          END;
          IF ABS(amp - 1.0D0) > ampTol THEN
            SELF.error("Amplitude differs at " & Fmt.Int(j) & ", "
                         & Fmt.LongReal(amp) & "\n");
            RAISE TestFailure;
          END;
        END;
      END;
    EXCEPT
    | TestFailure =>
    END;
  END CheckOscillator;


TYPE
  UnitTestFrequency = UnitTestAtom.T OBJECT
                        diffTol: LONGREAL;
                        ip     : Interpolation.T;
                      OVERRIDES
                        test := CheckFrequency;
                      END;


(* Compare the frequency modulation of a static oscillator with a modulated
   sine oscillator. *)
PROCEDURE CheckFrequency (SELF: UnitTestFrequency; ) =
  CONST
    numSamples = 20000;
    center     = 1.0D0;
    depth      = 10.0D0;
    osciFreq   = 0.01D0;
    lfoFreq    = 0.0000572D0;

  TYPE SignalPair = RECORD a, b: Signal.T;  END;

  PROCEDURE CreateFreqMod
    (phase: LONGREAL; freqModControl, osciControl: SignalControl.T; ):
    SignalPair =
    VAR osci := NEW(Osci.T).init(WaveSine.Wave, osciFreq, phase);
    BEGIN
      RETURN
        SignalPair{NEW(ProcessFrequencyModulation.T).init(
                     osci, freqModControl, SELF.ip),
                   NEW(OsciMod.T).init(WaveSine.Wave, osciControl, phase)};
    END CreateFreqMod;

  VAR
    lfo := NEW(Osci.T).init(WaveSine.Wave, lfoFreq);
    freqModControls := ProcessSplit.New(
                         NEW(ProcessControlExponential.T).init(
                           lfo, center, depth), 2);
    osciControls := ProcessSplit.New(NEW(ProcessControlExponential.T).init(
                                       lfo, osciFreq * center, depth), 2);
    osciSine := CreateFreqMod(0.0D0, freqModControls[0], osciControls[0]);
    osciCosine := CreateFreqMod(
                    0.25D0, freqModControls[1], osciControls[1]);

  BEGIN
    (*
    ProcessPlot.Stream(osciSine);
    ProcessPlay.Stream(osciSine, 44100.0D0);
    *)

    TRY
      FOR j := 0 TO numSamples - 1 DO
        WITH dcos = osciCosine.a.get() - osciCosine.b.get(),
             dsin = osciSine.a.get() - osciSine.b.get()      DO
          IF ABS(dcos * dcos + dsin * dsin) > SELF.diffTol THEN
            SELF.error(
              "Modulated oscillator and "
                & "a modulated signal of an oscillator differs at "
                & Fmt.Int(j) & ", diff cos " & Fmt.LongReal(dcos)
                & ", diff sin " & Fmt.LongReal(dsin) & "\n");
            RAISE TestFailure;
          END;
        END;
      END;
    EXCEPT
    | TestFailure =>
    END;
  END CheckFrequency;



TYPE
  UnitTestPhase = UnitTestAtom.T OBJECT
                    ampTol: LONGREAL;
                    ip    : Interpolation.T;
                  OVERRIDES
                    test := CheckPhase;
                  END;


(* ToDo: compare with phase modulated oscillator *)
(* Very simple test of the phase modulation: Check that the amplitude of a
   complex but phase modulated oscillator is constantly 1. *)
PROCEDURE CheckPhase (SELF: UnitTestPhase; ) =
  CONST
    numSamples = 20000;
    center     = 0.0D0;
    depth      = 200.0D0;
    osciFreq   = 0.01D0;
    lfoFreq    = 0.0000272D0;

  PROCEDURE CreatePhaseMod (phase: LONGREAL; control: SignalControl.T; ):
    Signal.T =
    VAR osci := NEW(Osci.T).init(WaveSine.Wave, osciFreq, phase);
    BEGIN
      RETURN NEW(ProcessPhaseModulation.T).init(
               osci, control, -CEILING(depth), CEILING(depth), SELF.ip);
    END CreatePhaseMod;

  VAR
    lfo := NEW(Osci.T).init(WaveSine.Wave, lfoFreq);
    controls := ProcessSplit.New(
                  NEW(ProcessControlLinear.T).init(lfo, center, depth), 2);
    osciSine   := CreatePhaseMod(0.0D0, controls[0]);
    osciCosine := CreatePhaseMod(0.25D0, controls[1]);

  BEGIN
    (*
    ProcessPlay.Stream(osciSine, 44100.0D0);
    ProcessPlot.Stream(osciSine);
    *)

    TRY
      FOR j := 0 TO numSamples - 1 DO
        VAR
          cos := osciCosine.get();
          sin := -osciSine.get();
          amp := cos * cos + sin * sin;
        BEGIN
          IF ABS(amp - 1.0D0) > SELF.ampTol THEN
            SELF.error("Amplitude differs at " & Fmt.Int(j) & ", "
                         & Fmt.LongReal(amp) & "\n");
            RAISE TestFailure;
          END;
        END;
      END;
    EXCEPT
    | TestFailure =>
    END;
  END CheckPhase;

PROCEDURE Test (): UnitTest.T =
  BEGIN
    RETURN NEW(UnitTestList.T).init(
             "Modulation",
             ARRAY OF
               UnitTest.T{
               NEW(UnitTestAtom.T, test := CheckOscillator).init(
                 "modulated oscillator"),
               NEW(UnitTestList.T).init(
                 "Frequency",
                 ARRAY OF
                   UnitTest.T{NEW(UnitTestFrequency, diffTol := 2.0D-1,
                                  ip := Interpolation.T.Constant).init(
                                "no interpolation"),
                              NEW(UnitTestFrequency, diffTol := 2.0D-1,
                                  ip := Interpolation.T.Linear).init(
                                "linear interpolation"),
                              NEW(UnitTestFrequency, diffTol := 2.0D-1,
                                  ip := Interpolation.T.Cubic).init(
                                "cubic interpolation")}),
               NEW(UnitTestList.T).init(
                 "Phase",
                 ARRAY OF
                   UnitTest.T{NEW(UnitTestPhase, ampTol := 1.0D-11,
                                  ip := Interpolation.T.Constant).init(
                                "no interpolation"),
                              NEW(UnitTestPhase, ampTol := 1.0D-3,
                                  ip := Interpolation.T.Linear).init(
                                "linear interpolation"),
                              NEW(UnitTestPhase, ampTol := 1.0D-6,
                                  ip := Interpolation.T.Cubic).init(
                                "cubic interpolation")})});
  END Test;


BEGIN
END TestModulation.
