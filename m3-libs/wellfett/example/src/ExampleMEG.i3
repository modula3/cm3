INTERFACE ExampleMEG;

IMPORT LongRealScaledSignal AS ScaledSignal;

PROCEDURE Run ();

PROCEDURE PlotReal (READONLY s: ARRAY OF ScaledSignal.T; );

END ExampleMEG.
