(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: TextPane.m3,v 1.2 2001-09-19 14:22:14 wagner Exp $ *)

MODULE TextPane;
IMPORT Axis;
IMPORT VBT;
IMPORT PaneManVBT;
IMPORT PaneFrame;
IMPORT TextPort;
IMPORT Rd;
IMPORT Wr;
IMPORT Debug;
IMPORT Thread;
CONST
  DebugLevel = 90;
REVEAL
  T = Public BRANDED OBJECT
  OVERRIDES
    setText := SetText;
    getText := GetText;
    read := Read;
    write := Write;
    clone := Clone;
    key := Key;
    hilite := Hilite;
    installPane := InstallPane;
    focusWarn := FocusWarn;
  END;
TYPE
  CivilizedTextPort = TextPort.T OBJECT
    m: MUTEX;
    gotDown: BOOLEAN := FALSE;
    pane: T;
    pm: PaneManVBT.T;
    stealFocusWarning: BOOLEAN := FALSE;
    (* Set if the CivilizedTextPort is about to mysteriously lose the
       keyboard focus even though the pane manager shall not be notified *)
  OVERRIDES
    key := KeyTakenBackFromTheif;
    misc := Misc;
    mouse := Mouse;
    shape := Shape;
  END;

PROCEDURE Shape(<*UNUSED*>self: CivilizedTextPort;
                <*UNUSED*>ax: Axis.T;
                <*UNUSED*>n: CARDINAL): VBT.SizeRange =
  CONST
    size = 512;
    max = 8192;
    min = 16;
  BEGIN
    RETURN VBT.SizeRange{min, MAX(min+1, size), max};
  END Shape;

PROCEDURE Mouse(self: CivilizedTextPort; READONLY cd: VBT.MouseRec) =
  BEGIN
    LOCK(self.m) DO
      IF self.pane.canHilite THEN
        IF cd.clickType = VBT.ClickType.LastUp AND NOT self.gotDown THEN
Debug.S("TP got an up without getting a down. God bless Trestle", DebugLevel);
        ELSE
          LOCK self.pm.mu DO
            self.pm.stealFocusWarning := TRUE;
            IF NOT self.pane.hilited THEN
              self.pm.focusTo(self.pane);       
              Debug.S("TextPane calls FocusTo", DebugLevel);
            END;
          END;
          TextPort.T.mouse(self, cd);
        END;
        IF cd.clickType = VBT.ClickType.FirstDown THEN
          self.gotDown := TRUE;
        ELSIF cd.clickType = VBT.ClickType.LastUp THEN
          self.gotDown := FALSE;
        END;
      END;
    END;
  END Mouse;

PROCEDURE FocusWarn(self: T) =
  VAR
    ctp: CivilizedTextPort := self.pane;
  BEGIN
    LOCK ctp.m DO
      ctp.stealFocusWarning := TRUE;
    END;
  END FocusWarn;

PROCEDURE Misc(self: CivilizedTextPort; READONLY cd: VBT.MiscRec) =
  VAR
    tellThePaneManNow := FALSE;
  BEGIN
    LOCK(self.m) DO
      IF cd.type = VBT.Lost THEN
        IF self.stealFocusWarning THEN
          Debug.S("TextPane: " & 
            self.pane.title &
          " Not notifying of loss because of the focus warning.", DebugLevel);
          self.stealFocusWarning := FALSE;
        ELSE
          tellThePaneManNow := TRUE;
        Debug.S("TextPane: "&self.pane.title&" Notifying of loss.",DebugLevel);
        END;
      ELSE
        Debug.S("Telling the TextPort to take focus", DebugLevel);
      END;
      TextPort.T.misc(self, cd);
    END;
    IF tellThePaneManNow THEN
      self.pm.misc(cd);
    END;
  END Misc;

PROCEDURE KeyTakenBackFromTheif(self: CivilizedTextPort;
                                READONLY key: VBT.KeyRec) =
  BEGIN
    self.pm.key(key);
  END KeyTakenBackFromTheif;

PROCEDURE Key(self: T; READONLY cd: VBT.KeyRec) =
  BEGIN
    TextPort.T.key(self.pane, cd);
    (* shall we update the clones too *)
  END Key;

PROCEDURE NewTP(self: T; t: TEXT): TextPort.T =
  VAR
    textPort := NEW(CivilizedTextPort,
                    pane := self,
                    pm := self.paneMan,
                    m := NEW(MUTEX)).init();
  BEGIN
    IF t # NIL THEN
      TextPort.SetText(textPort, t);
    END;
    RETURN textPort;
  END NewTP;

PROCEDURE InstallPane(self: T; pane: VBT.T) =
  BEGIN
    PaneFrame.T.installPane(self, pane);
    NARROW(pane, CivilizedTextPort).pane := self;
  END InstallPane;

PROCEDURE Read(self: T; rd: Rd.T): VBT.T =
  VAR
    t: TEXT := NIL;
    <* FATAL Rd.Failure, Thread.Alerted *>
  BEGIN
    IF rd # NIL THEN
      t := Rd.GetText(rd, LAST(CARDINAL));
    END;
    RETURN NewTP(self, t);
  END Read;

PROCEDURE Write(self: T; wr: Wr.T) =
  BEGIN
    TRY
      Wr.PutText(wr, TextPort.GetText(self.pane));
    EXCEPT
      Wr.Failure, Thread.Alerted =>
    END;
  END Write;

PROCEDURE Clone(self: T): VBT.T =
  BEGIN
    RETURN NewTP(self, TextPort.GetText(self.pane));
  END Clone;

PROCEDURE SetText(self: T; t: TEXT) =
  BEGIN
    TextPort.SetText(self.pane, t);
  END SetText;

PROCEDURE GetText(self: T): TEXT =
  BEGIN
    RETURN TextPort.GetText(self.pane);
  END GetText;

PROCEDURE Hilite(self: T; state: BOOLEAN) = 
  VAR
    ctp: CivilizedTextPort := self.pane;
  BEGIN
    LOCK ctp.m DO
      (*    IF state THEN
            Debug.S("Told TextPane to take focus", DebugLevel);
            EVAL TextPort.TryFocus(self.pane, 0); 
            END; *)
      IF state THEN
        NARROW(self.paneMan, PaneManVBT.T).stealFocusWarning := TRUE;
        Debug.S("Setting the PaneMan focus warning.", DebugLevel);
      ELSE 
        ctp.stealFocusWarning := TRUE;
        Debug.S("Setting the TextPort focus warning.", DebugLevel);
      END;
    END;
    PaneFrame.T.hilite(self, state);
  END Hilite;

BEGIN
END TextPane.
