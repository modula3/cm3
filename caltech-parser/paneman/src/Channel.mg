(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

GENERIC MODULE Channel(Elem, ElemQueue);
(* Channel implemented as a monitored queue *)
IMPORT Thread;
IMPORT Debug;
CONST
  DebugLevel = 100;
REVEAL
  T = Public BRANDED OBJECT
    m: Thread.Mutex;
    nonempty: Thread.Condition;
    queue: ElemQueue.T;
  OVERRIDES
    init := Init;
    send := Send;
    recv := Recv;
  END;

PROCEDURE Init(self: T): T =
  BEGIN
    self.m := NEW(Thread.Mutex);
    self.nonempty := NEW(Thread.Condition);
    self.queue := NEW(ElemQueue.T).init();
    RETURN self;
  END Init;

PROCEDURE Send(self: T; elem: Elem.T) =
  BEGIN
    Debug.S("Channel sending",DebugLevel);
    LOCK self.m DO
      self.queue.put(elem);
    END;
    Thread.Signal(self.nonempty);
  END Send;

PROCEDURE Recv(self: T): Elem.T =
  VAR
    result: Elem.T;
  BEGIN
    Debug.S("Channel receiving",DebugLevel);
    LOCK self.m DO
      Debug.S("Recv got lock: waiting for nonempty",DebugLevel);
      WHILE NOT self.queue.get(result) DO
        Thread.Wait(self.m, self.nonempty);
      END;
    END;
    RETURN result;
  END Recv;

BEGIN
END Channel.
