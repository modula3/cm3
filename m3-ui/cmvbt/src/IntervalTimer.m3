
(* Copyright 1997 Critical Mass, Inc. All Rights Reserved.
   See file COPYRIGHT for full description. *)

MODULE IntervalTimer;
IMPORT Time, Boolean, Thread;

REVEAL 
  Private = Thread.Closure BRANDED OBJECT END;
  T = Public BRANDED OBJECT 
    duration: Time.T;
    mu:       MUTEX;
    thr:      Thread.T;
    dead:     Boolean.T;
  OVERRIDES
    init     := Init;
    shutdown := Shutdown;
    apply    := Apply;
  END;

PROCEDURE Init(self: T; duration: Time.T): T =
  BEGIN
    self.mu := NEW(MUTEX);
    self.duration := duration;
    self.dead := FALSE;
    self.thr   := Thread.Fork (self);
    RETURN self;
  END Init;

PROCEDURE Shutdown(self:T) =
  BEGIN
    LOCK self.mu DO
      self.dead := TRUE;
      Thread.Alert (self.thr);
    END;
  END Shutdown;

PROCEDURE Apply(self: T): REFANY =
  BEGIN
    LOOP
      LOCK self.mu DO
        TRY
          Thread.AlertPause (self.duration);
        EXCEPT
          Thread.Alerted => 
            IF self.dead THEN self.dead := FALSE; END;
            RETURN NIL;
        END;
        self.wakeup();
      END;
    END;
  END Apply;

BEGIN 
END IntervalTimer.
