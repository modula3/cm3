(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: IOPaneMan.m3,v 1.2 2001-09-19 14:22:13 wagner Exp $ *)

MODULE IOPaneMan;
IMPORT TextPane;
IMPORT PaneManOp;
IMPORT Thread;
IMPORT KeyChan;
IMPORT StarterList;
IMPORT StarterScanList;
IMPORT VBT;
IMPORT PaneManVBT;
IMPORT TermIO;

TYPE
  PrivateIO = TermIO.T OBJECT
    paneMan: T;
    callback: PrivateCallback;
    keyChan: KeyChan.T;
  OVERRIDES
    getChar := GetChar;
    getLine := GetLine;
    putLine := PutLine;
    putText := PutText;
  END;

  PrivateCallback = PaneManOp.InputCallback OBJECT
    mu: MUTEX;
    cond: Thread.Condition;
    result: TEXT;
  OVERRIDES
    accept := Accept;
    complete := Complete;
  END;

PROCEDURE Accept(self: PrivateCallback; result: TEXT) =
  BEGIN
    LOCK self.mu DO
      self.result := result;
      Thread.Signal(self.cond);
    END;
  END Accept;

PROCEDURE Complete(<*UNUSED*>self: PrivateCallback;
                   <*UNUSED*>VAR t: TEXT) =
  BEGIN
  END Complete;

REVEAL
  T = Public BRANDED OBJECT
    io: PrivateIO;
  OVERRIDES
    getIO := GetIO;
    init := Init;
    cookedKey := CookedKey;
  END;

PROCEDURE GetIO(self: T): TermIO.T =
  BEGIN
    RETURN self.io;
  END GetIO;

PROCEDURE Init(self: T; s: StarterList.T;
               startingStarters: StarterScanList.T := NIL): PaneManVBT.T =
  VAR
    callback := NEW(PrivateCallback,
                    mu := NEW(MUTEX),
                    cond := NEW(Thread.Condition));
  BEGIN
    self.io := NEW(PrivateIO,
                   paneMan := self,
                   callback := callback,
                   keyChan := NEW(KeyChan.T).init());
    RETURN Public.init(self, s, startingStarters);
  END Init;

PROCEDURE CookedKey(self: T; READONLY key: VBT.KeyRec) =
  BEGIN
    IF key.wentDown AND
      NOT VBT.Modifier.Control IN key.modifiers AND
      key.whatChanged < 256 THEN
      IF NOT ISTYPE(self.getSelectedPane(), TextPane.T) THEN
        self.io.keyChan.send(key);
      END;
    END;
    Public.cookedKey(self, key);
  END CookedKey; 


(* PrivateIO methods *)

PROCEDURE GetChar(self: PrivateIO): CHAR =
  VAR
    key := self.keyChan.recv();
  BEGIN
    <* ASSERT key.wentDown *>
    RETURN VAL(key.whatChanged, CHAR);
  END GetChar;

PROCEDURE GetLine(self: PrivateIO; prompt := ">"): TEXT =
  BEGIN
    LOCK self.callback.mu DO
      LOCK self.paneMan.mu DO
        self.paneMan.input(prompt, "", self.callback);
      END;
      Thread.Wait(self.callback.mu, self.callback.cond);
      RETURN self.callback.result;
    END;
  END GetLine;

PROCEDURE PutLine(self: PrivateIO; t: TEXT) =
  BEGIN
    LOCK self.paneMan.mu DO
      self.paneMan.print(t);
    END;
  END PutLine;

PROCEDURE PutText(<*UNUSED*>self: PrivateIO; <*UNUSED*>t: TEXT) =
  BEGIN
  END PutText;


BEGIN
END IOPaneMan.
