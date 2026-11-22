MODULE Watchdog;
IMPORT Time;
IMPORT Debug;
IMPORT Process;
IMPORT Thread;
IMPORT Usignal;

REVEAL
  T = Public BRANDED Brand OBJECT
    exitAction : Callback;
    deadline   : Time.T;
    maxDelay   : Time.T;
    remaining  : Time.T; (* used only in Suspended state *)
    state := State.Running;
    mu : MUTEX;
    expireAction : Callback;
  OVERRIDES
    init            := Init;
    reset           := Reset;
    pause           := Pause;
    unpause         := Unpause;
    kill            := Kill;
    setExpireAction := SetExpireAction;
  END;

TYPE
  State = { Running, Suspended, Dead };

PROCEDURE Init(t : T; maxDelay : LONGREAL; callback : Callback) : T =
  BEGIN
    t.mu := NEW(MUTEX);
    LOCK t.mu DO
      WITH now = Time.Now() DO
        t.deadline     := now + maxDelay;
        t.maxDelay     := maxDelay;
        t.state        := State.Running;
        t.expireAction := callback
      END
    END;
    EVAL Thread.Fork(NEW(Closure, t := t));
    RETURN t
  END Init;

TYPE
  Closure = Thread.Closure OBJECT
    t : T;
  OVERRIDES
    apply := Apply
  END;

PROCEDURE Apply(cl : Closure) : REFANY =
  BEGIN
    LOOP
      Thread.Pause(1.0d0);
      LOCK cl.t.mu DO
        CASE cl.t.state OF
          State.Running =>
          IF Time.Now() > cl.t.deadline THEN
            IF cl.t.expireAction = NIL THEN
              Debug.Warning("Watchdog expired without expire action set -- Program exiting!!!");
              Process.Exit(1);

              Thread.Pause(10.0d0);

              WITH myId = Process.GetMyID() DO
                EVAL Usignal.kill(myId, 1);
                Thread.Pause(1.0d0);
                EVAL Usignal.kill(myId, 15);
                Thread.Pause(1.0d0);
                EVAL Usignal.kill(myId,  9);
              END;
              
              <*ASSERT FALSE*>
            ELSE
              cl.t.expireAction.do()
            END
          END
        |
          State.Suspended => (* skip *)
        |
          State.Dead => EXIT
        END
      END
    END;
    RETURN NIL
  END Apply;

PROCEDURE Reset(t : T) =
  BEGIN
    LOCK t.mu DO
      <*ASSERT t.state # State.Dead*>
      t.deadline := Time.Now() + t.maxDelay
    END
  END Reset;

PROCEDURE Pause(t : T) =
  BEGIN
    LOCK t.mu DO
      <*ASSERT t.state = State.Running*>
      t.state := State.Suspended;
      t.remaining := t.deadline - Time.Now()
    END
  END Pause;

PROCEDURE Unpause(t : T) =
  BEGIN
    LOCK t.mu DO
      <*ASSERT t.state = State.Suspended*>

      t.deadline := Time.Now() + t.remaining;
      t.state := State.Running
    END
  END Unpause;


PROCEDURE Kill(t : T) =
  BEGIN
    LOCK t.mu DO
      <*ASSERT t.state # State.Dead*>
      t.state := State.Dead
    END
  END Kill;

PROCEDURE SetExpireAction(t : T; to : Callback) =
  BEGIN
    LOCK t.mu DO
      <*ASSERT t.state # State.Dead*>
      t.expireAction := to
    END
  END SetExpireAction;
  
BEGIN END Watchdog.
  
