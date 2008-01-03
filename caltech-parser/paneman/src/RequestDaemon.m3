(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: RequestDaemon.m3,v 1.2 2001-09-19 14:22:13 wagner Exp $ *)

MODULE RequestDaemon;
IMPORT PaneManVBT;
IMPORT PaneManOp;
IMPORT PaneManRequest;
IMPORT Thread;
IMPORT PMRequestChan;
IMPORT StarterScan;
REVEAL
  T = Public BRANDED "RequestDaemon" OBJECT
    pm: PaneManVBT.T;
    t: Thread.T;
    r: PMRequestChan.T;
  OVERRIDES
    init := Init;

    startPane := StartPane;
    print := Print;
    input := Input;
  END;

PROCEDURE Init(self: T; pm: PaneManVBT.T): T =
  VAR
    k := NEW(Closure, daemon := self);
  BEGIN
    self.pm := pm;
    self.r := NEW(PMRequestChan.T).init();
    self.t := Thread.Fork(k);
    RETURN self;
  END Init;

TYPE
  InputCallback = PaneManOp.InputCallback;
  Request = PaneManRequest.T;
  Kind = PaneManRequest.Kind;

PROCEDURE StartPane(self: T; s: StarterScan.T) =
  BEGIN
    self.r.send(Request{kind := Kind.Start, s := s});
  END StartPane;

PROCEDURE Print(self: T; message: TEXT) =
  BEGIN
    self.r.send(Request{kind := Kind.Print, message := message});
  END Print; 

PROCEDURE Input(self: T; prompt, default: TEXT; result: InputCallback) =
  BEGIN
    self.r.send(Request{kind := Kind.Input, message := prompt,
                        default := default, cb := result});
  END Input;

TYPE
  Closure = Thread.Closure OBJECT
    daemon: T;
  OVERRIDES
    apply := Apply;
  END;

PROCEDURE Apply(c: Closure): REFANY =
  VAR
    self := c.daemon;
    r: PaneManRequest.T;
    pm := self.pm;
  BEGIN
    LOOP
      r := self.r.recv();
      LOCK pm.mu DO
        CASE r.kind OF
        | Kind.Start => EVAL pm.setPane(r.s);
        | Kind.Print => pm.print(r.message);
        | Kind.Input => pm.input(r.message, r.default, r.cb);
        END;
      END;
    END;
  END Apply;

BEGIN
END RequestDaemon.
