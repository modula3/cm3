(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: KeyDaemon.m3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

MODULE KeyDaemon;
IMPORT VBT;
IMPORT Axis;
IMPORT Thread;
IMPORT KeyRec, KeyList, KeyListExtras, KeyChan;
IMPORT Fmt, Debug, Text;
IMPORT PaneManVBT;
IMPORT StarterScan;
IMPORT Trestle;
(* IMPORT Latin1Key as LK;
   IMPORT KeyboardKey as KK; *)
CONST
  DebugLevel = 90;
TYPE
  KeyClosure = Thread.Closure OBJECT
    daemon: T;
  OVERRIDES
    apply := Apply;
  END;
REVEAL
  T = Public BRANDED OBJECT
    pm: PaneManVBT.T;
    t: Thread.T;
    kc: KeyChan.T;
    eatups: KeyList.T;
  OVERRIDES
    init := Init;
    key := Key;
  END;

PROCEDURE Init(self: T; pm: VBT.T): T =
  VAR
    k := NEW(KeyClosure, daemon := self);
  BEGIN
    Debug.S("Initing the KeyDaemon.", DebugLevel);
    self.pm := NARROW(pm, PaneManVBT.T);
    self.kc := NEW(KeyChan.T).init();
    self.eatups := NIL;
    Debug.S("About to fork thread.", DebugLevel);
    self.t := Thread.Fork(k);
    RETURN self;
  END Init;

PROCEDURE Key(self: T; key: VBT.KeyRec) =
  BEGIN
    self.kc.send(key);
  END Key;

PROCEDURE GetKey(self: T): VBT.KeyRec =
  VAR
    key: KeyRec.T;
  BEGIN
    Debug.S("Called GetKey.", DebugLevel);
    REPEAT
      key := self.kc.recv();
      Debug.S("GetKey1");
    UNTIL NOT KeyListExtras.MemberDelD(self.eatups, key);
    Debug.S("GetKey Done.", DebugLevel);
    RETURN key;
  END GetKey;

PROCEDURE EatKeyUp(self: T; key: VBT.KeyRec) =
  BEGIN
    self.eatups := KeyList.Cons(key, self.eatups);
  END EatKeyUp;

PROCEDURE Func(self: T; char: CHAR) =
  VAR
    pm := self.pm;
  BEGIN
    Debug.S("Function Key.", DebugLevel);
    IF NOT pm.setPane(StarterScan.FromKey(char)) THEN
      Debug.S("Daemon.Func: Not starting new pane.");
      IF char = 'c' THEN
        Trestle.Delete(pm);
      END;
      LOCK pm.mu DO
        CASE char OF
        | 'b' => pm.listPanes();
        | 'f' => pm.inputPath(FALSE);
        | 'w' => pm.inputPath(TRUE);
        | 'o' => pm.rotFocus();
        | '0' => pm.unSplit1();
        | '1' => pm.unSplit();
        | '2' => pm.splitPane(Axis.T.Ver);
        | '3' => pm.splitPane(Axis.T.Hor);
        ELSE
        END;
      END;
    END;
  END Func;

PROCEDURE Apply(k: KeyClosure): REFANY =
  VAR
    self := k.daemon;
    key: VBT.KeyRec;
    char: CHAR;
    func := FALSE;
  BEGIN
    Debug.S("Beginning Daemon Thread.", DebugLevel);
    WHILE TRUE DO
      key := GetKey(self);
      char := KeyRec.LowerChar(key);
      Debug.S("Daemon.Apply sees " & Text.FromChar(char) & " " &
        Fmt.Bool(key.wentDown), DebugLevel);
      IF key.wentDown
        AND (func OR VBT.Modifier.Mod0 IN key.modifiers) THEN
        Func(self, char);
        EatKeyUp(self, key);
        func := ORD(char) < 32;
      ELSIF key.wentDown
        AND char='x'
        AND VBT.Modifier.Control IN key.modifiers THEN
        EatKeyUp(self, key);
        func := TRUE;
      ELSE
        LOCK self.pm.mu DO
          self.pm.cookedKey(key);
        END;
      END;
    END;
    RETURN NIL;
  END Apply;

BEGIN
END KeyDaemon.
