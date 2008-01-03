MODULE ProcessEcho;

IMPORT Signal,
       ProcessSplit,
       ProcessMixer,
       ProcessAmplifier,
       ProcessDelay,
       ProcessControlConstant;
IMPORT Thread;

REVEAL
  T = Public BRANDED OBJECT
        x: Signal.T;
      OVERRIDES
        init := Init;
        get  := Get;
        exit := Exit;
      END;


PROCEDURE Init
  (SELF: T; x: Signal.T; delay: [1 .. LAST(CARDINAL)]; gain: LONGREAL; ):
  T =
  (* We use the delay really for delay (not for prediction) thus the errors
     caused by reading input cannot occur. *)
  <* FATAL Signal.End, Signal.Error, Thread.Alerted *>
  VAR
    amp   := NEW(ProcessAmplifier.T);
    mixer := NEW(ProcessMixer.T).init(x, amp);
    split := ProcessSplit.New(mixer, 2);
  BEGIN
    amp := amp.init(NEW(ProcessDelay.T).init(split[0], delay),
                    NEW(ProcessControlConstant.T).init(gain));
    SELF.x := split[1];

    RETURN SELF;
  END Init;

PROCEDURE Exit (SELF: T; ) =
  BEGIN
    SELF.x.exit();
    SELF.x := NIL;
  END Exit;


PROCEDURE Get (SELF: T; ): LONGREAL
  RAISES {Signal.End, Signal.Error, Thread.Alerted} =
  BEGIN
    RETURN SELF.x.get();
  END Get;

BEGIN
END ProcessEcho.
