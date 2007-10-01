MODULE Main;

IMPORT Signal,
       ProcessLoad,
       ProcessPlay,
       ProcessAmplifier,
       ProcessEcho,
       ProcessControlConstant AS ControlConstant;

IMPORT Wr, Thread;

CONST
  FileName   = "/home/thielema/public_html/Music/superfrog.aiff";
  SampleRate = 44100.0D0;
  Delay      = 0.3D0;
  Volume     = 0.7D0;
  Gain       = 0.5D0;

<* FATAL Thread.Alerted, Wr.Failure, Signal.Error *>

BEGIN
  (* state variable filter *)
  WITH x = NEW(ProcessLoad.T).init(FileName),
       echo = NEW(ProcessAmplifier.T).init(NEW(ProcessEcho.T).init(
                                             x, ROUND(Delay * SampleRate),
                                             Gain),
                                           NEW(ControlConstant.T).init(
                                             Volume)) DO
    ProcessPlay.Stream(echo, SampleRate);
  END;
END Main.
