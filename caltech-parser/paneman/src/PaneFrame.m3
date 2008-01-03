(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: PaneFrame.m3,v 1.2 2001-09-19 14:22:13 wagner Exp $ *)

MODULE PaneFrame;
IMPORT PaneList,PaneListExtras;
IMPORT VBT, VBTClass;
IMPORT Split, HVSplit, Axis;
IMPORT TextVBT, TextVBTSquat;
IMPORT Rd,Wr,Stdio;
IMPORT FileRd,FileWr;
IMPORT Pathname;
IMPORT OSError, Thread;
IMPORT Debug, Fmt;
IMPORT Starter;
IMPORT PaneManVBT;
CONST
  DebugLevel = 90;
<* FATAL OSError.E, Thread.Alerted, Wr.Failure *>

REVEAL
  T = Public BRANDED OBJECT
  OVERRIDES
    (* frame methods, called by the pane manager *)
    init := Init;
    frameClone := FrameClone;
    frameDiscard := FrameDiscard;
    frameHilite := FrameHilite;
    frameVisible := FrameVisible;
    frameWrite := FrameWrite;

    (* misc methods *)
    installPane := InstallPane;
    frameUnclone := FrameUnclone;
    statusUpdate := StatusUpdate;

    (* pane methods *)
    read := Read;
    clone := PaneClone;
    write := Write;
    discard := PaneDiscard;
    hilite := PaneHilite;
    focusWarn := FocusWarn;
    
    (* VBT methods *)
    mouse := Mouse;
    key := Key;
  END;

(* default frame methods *)

PROCEDURE Init(self: T; path, title, kind: TEXT; paneMan: VBT.T;
               starter: Starter.T; insteadRd: Rd.T := NIL): T =
  VAR
    rd := insteadRd;
  BEGIN
    Debug.S("Init frame: Path = " & path, DebugLevel);
    self.path := path;
    self.kind := kind;
    self.title := title;
    self.paneMan := paneMan;
    self.starter := starter;
    IF rd = NIL AND path # NIL THEN
      TRY
        rd := FileRd.Open(path);
      EXCEPT
        OSError.E =>
      END;
    END;
    self.installPane(self.read(rd));
    <* ASSERT self.pane # NIL *>
    <* ASSERT self.status # NIL *>
    <* ASSERT self.path # NIL *>
    <* ASSERT self.kind # NIL *>
    <* ASSERT self.title # NIL *>
    <* ASSERT self.paneMan # NIL *>
    TRY
      IF rd # NIL THEN
        Rd.Close(rd);
      END;
    EXCEPT
      Rd.Failure =>
    END;
    RETURN self;
  END Init;

PROCEDURE FrameClone(frame: T): T =
  VAR
    clonedPane := frame.clone();
    clone: T;
    parent := frame;
  BEGIN
    <* ASSERT frame.starter # NIL *>
    clone := frame.starter.new();
    Debug.S("PaneFrame.FrameClone", DebugLevel);
    IF NOT frame.primary THEN
      parent := frame.parent;
    END;
(* copy parent fields *)
    clone.path := parent.path;
    clone.kind := parent.kind;
    clone.title := parent.title;
    clone.paneMan := parent.paneMan;
    clone.starter := parent.starter;
(* install and link to parent *)
    clone.primary := FALSE;
    clone.parent := parent;
    parent.clones := PaneList.Cons(clone, parent.clones);
    clone.installPane(clonedPane);
    RETURN clone;
  END FrameClone;

PROCEDURE FrameDiscard(frame: T) =
  BEGIN
    frame.frameUnclone();
    frame.discard();
  END FrameDiscard;

PROCEDURE FrameHilite(frame: T; state: BOOLEAN) =
  BEGIN
    frame.hilited := state;
    frame.statusUpdate();
    frame.hilite(state);
  END FrameHilite;

PROCEDURE FrameVisible(frame: T; state: BOOLEAN) =
  BEGIN
    frame.visible := state;
  END FrameVisible;

PROCEDURE FrameWrite(self: T; newPath: TEXT := NIL) =
  VAR
    wr: Wr.T;
  BEGIN
    IF newPath # NIL THEN
      self.path := newPath;
      self.title := Pathname.Last(newPath);
      self.statusUpdate();
    END;
    IF self.path # NIL THEN
      wr := FileWr.Open(self.path);
      self.write(wr);
      Wr.Close(wr);
    END;
  END FrameWrite; 



(* misc methods *)

PROCEDURE InstallPane(frame: T; pane: VBT.T) =
  BEGIN
    Debug.S("PaneFrame.InstallPane", DebugLevel);
    frame := HVSplit.T.init(frame, Axis.T.Ver, adjustable := FALSE);
    frame.status := NEW(TextVBTSquat.T).init("...");
    frame.pane := pane;
    frame.statusUpdate();
    Split.AddChild(frame, pane, frame.status);
  END InstallPane;

PROCEDURE FrameUnclone(frame: T) =
  VAR
    cur: PaneList.T;
    newParent: T;
  BEGIN
    IF frame.primary THEN
      IF frame.clones # NIL THEN
        newParent := frame.clones.head;
        cur := frame.clones.tail;
        newParent.clones := cur;
        newParent.primary := TRUE;
        WHILE cur # NIL DO
          cur.head.parent := newParent;
          cur := cur.tail;
        END;
      END;
    ELSE
      EVAL PaneListExtras.MemberDelD(frame.parent.clones, frame);
    END;
  END FrameUnclone;

PROCEDURE StatusUpdate(frame: T) =
  CONST
    h = ARRAY BOOLEAN OF TEXT {" ","!"};
    c = ARRAY BOOLEAN OF TEXT {"c"," "};
  BEGIN
    <* ASSERT frame.title # NIL *>
    <* ASSERT frame.kind # NIL *>
    Debug.S("PaneFrame.StatusUpdate: " & frame.title, DebugLevel);
    TextVBT.Put(frame.status,
                frame.title & " (" &
                frame.kind & ")" &
                c[frame.primary] &
                h[frame.hilited]);
  END StatusUpdate;



(* default pane methods *)

PROCEDURE Read(self: T; <*UNUSED*>rd: Rd.T): VBT.T =
  BEGIN
    RETURN TextVBT.New(self.kind & "Pane.Read not implemented");
  END Read;
PROCEDURE PaneClone(<*UNUSED*>self: T): VBT.T =
  BEGIN
    RETURN TextVBT.New("Say no to clones.");
  END PaneClone;
PROCEDURE Write(self: T; <*UNUSED*>wr: Wr.T) =
  BEGIN
    Wr.PutText(Stdio.stdout, self.kind & "Pane.Write not implemented");
  END Write;
PROCEDURE PaneDiscard(<*UNUSED*>self: T) =
  BEGIN
  END PaneDiscard;
PROCEDURE PaneHilite(frame: T; state: BOOLEAN) =
  VAR
    misc: VBT.MiscRec;
  BEGIN
    <* ASSERT frame.pane # NIL *>
    Debug.S("Hilite " & frame.title & " to " & Fmt.Bool(state), DebugLevel);
    IF state THEN
      misc.type := VBT.TakeSelection;
    ELSE
      misc.type := VBT.Lost;
    END;
    misc.time := NARROW(frame.paneMan, PaneManVBT.T).time;
    misc.detail := VBT.NullDetail;
    misc.selection := VBT.KBFocus;
    VBTClass.Misc(frame.pane, misc);
  END PaneHilite;

PROCEDURE FocusWarn(<*UNUSED*>self: T) =
  BEGIN
  END FocusWarn;

(* VBT methods *)

PROCEDURE Mouse(self: T; READONLY cd: VBT.MouseRec) =
  VAR
    paneMan := NARROW(self.paneMan, PaneManVBT.T);
  BEGIN
    IF cd.clickType = VBT.ClickType.FirstDown THEN
      Debug.S("Click in PaneFrame", DebugLevel);
      IF self.canHilite THEN
        paneMan.time := cd.time;
        IF NOT self.hilited THEN
          Debug.S("PaneFrame calls FocusTo", DebugLevel);
          LOCK paneMan.mu DO
            paneMan.focusTo(self);
          END;
        END;
      END;
    END;
    HVSplit.T.mouse(self, cd);
  END Mouse;

PROCEDURE Key(self: T; READONLY key: VBT.KeyRec) =
  BEGIN
    self.pane.key(key);
  END Key;

BEGIN
END PaneFrame.
