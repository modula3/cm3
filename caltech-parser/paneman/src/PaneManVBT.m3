(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

MODULE PaneManVBT;
IMPORT HVSplit, HVBar;
IMPORT PaneFrame;
IMPORT KeyDaemon;
IMPORT RequestDaemon;
IMPORT PaneList, PaneListExtras;
IMPORT VBT;
IMPORT Text;
IMPORT Pixmap;
IMPORT Fmt, Debug;
IMPORT Split, Axis;
IMPORT StarterList;
IMPORT StarterScan;
IMPORT StarterScanList;
IMPORT ListPaneStarter;
IMPORT MiniPane;
IMPORT PathnameUtils;
CONST
  DebugLevel = 90;
<* FATAL Split.NotAChild *>
REVEAL
  T = Public BRANDED OBJECT
    daemon: KeyDaemon.T;
    visPanes, primPanes: PaneList.T;
    (* visPanes.head is selected *)
    (* primPanes: all panes excluding clones *)
    starters: StarterList.T;
    miniPane: MiniPane.T;
    inputPane: PaneFrame.T; (* the pane that caused input to be called *)
    inputPrompt: TEXT;
    inputCallback: InputCallback := NIL;
    pathCallback: PathCallback;
    paneUpdater: PaneUpdater;
  OVERRIDES
    (* anyone calling these methods must lock mu. *)
    focusTo := FocusTo;
    rotFocus := RotFocus;
    splitPane := SplitPane;
    unSplit := UnSplit;
    unSplit1 := UnSplit1;
    setPane := SetPane;
    listPanes := ListPanes;
    inputPath := InputPath;
    cookedKey := CookedKey;
    print := Print;
    input := Input;
    setPaneUpdater := SetPaneUpdater;
    getSelectedPane := GetSelectedPane;

    (* these methods lock mu. *)
    init := Init;
    misc := Misc;
    key := Key;
  END;

TYPE
  Act={Replace, Delete};
  NullPaneUpdater = PaneUpdater OBJECT OVERRIDES apply := NullApply; END;

PROCEDURE NullApply(<*UNUSED*>self: NullPaneUpdater;
                    <*UNUSED*>pane: VBT.T) =
  BEGIN END NullApply;

PROCEDURE SetPaneUpdater(self: T; pu: PaneUpdater) =
  VAR
    cur := self.visPanes;
  BEGIN
    self.paneUpdater := pu;
    WHILE cur # NIL DO
      pu.apply(cur.head);
      cur := cur.tail;
    END;
  END SetPaneUpdater;

PROCEDURE TwoStepReplace(s: Split.T; old, new: VBT.T) =
  VAR
    pred: VBT.T;
  BEGIN
    pred := Split.Pred(s, old);
    Debug.S("2SR: Gonna Delete.", DebugLevel);
    Split.Delete(s, old);
    Debug.S("2SR: Gonna Secret Delete.", DebugLevel);
    Split.Delete(old, new);
    Debug.S("2SR: Gonna Insert.", DebugLevel);
    Split.Insert(s, pred, new);
    Debug.S("2SR: Didn't we make it", DebugLevel);
  END TwoStepReplace;

PROCEDURE FindHead(self: T; action: Act; elem: VBT.T := NIL) =
  VAR
    head := self.visPanes.head;
  PROCEDURE Search(fromnode, parent: VBT.T) =
    VAR
      ch1,ch2: VBT.T;
    BEGIN
      Debug.S("FindHead.Search", DebugLevel);
      TYPECASE fromnode OF
      | T (e) =>
        BEGIN
          <* ASSERT e = self *>
          Debug.S("Looking at first PaneManVBT child", DebugLevel);
          ch1 := Split.Nth(e, 0);
          IF ch1=head THEN
            Debug.S("Found in PaneManVBT", DebugLevel);
            IF action=Act.Replace THEN
              Split.Replace(e, ch1, elem);
            END;
          ELSE
            <* ASSERT ISTYPE(ch1, PaneFrame.T) OR ISTYPE(ch1, HVSplit.T) *>
            Search(ch1, e);
          END;
        END;
      | PaneFrame.T (e) => Debug.S("Leaf: " & e.title, DebugLevel);
      | Split.T (e) =>
        BEGIN
          <*ASSERT e # self *>
          Debug.S("Looking at Split children", DebugLevel);
          ch1 := Split.Nth(e, 0);
          ch2 := Split.Nth(e, 2);
          IF ch1=head THEN
            Debug.S("Found in Split[1]", DebugLevel);
            IF action=Act.Replace THEN
              Split.Replace(e, ch1, elem);
            ELSE
              TwoStepReplace(parent, e, ch2);
              Split.Delete(e, ch1);
            END;
          ELSIF ch2=head THEN
            Debug.S("Found in Split[3]");
            IF action=Act.Replace THEN
              Split.Replace(e, ch2, elem);
            ELSE
              TwoStepReplace(parent, e, ch1);
              Split.Delete(e, ch2);
            END;
          END;
          Search(ch1, e);
          Search(ch2, e);
        END; (* Split.T *)
      ELSE
        <* ASSERT FALSE *>
      END; (* TYPECASE *)
    END Search;
  BEGIN
    Search(self, NIL);
  END FindHead;

PROCEDURE CountFocusable(self: T): INTEGER =
  VAR
    cur := self.visPanes;
    count: INTEGER := 0;
  BEGIN
    WHILE cur # NIL DO
      IF cur.head.canHilite THEN 
        count := count + 1;
        IF count >= 2 THEN
          RETURN count;
        END;
      END;
      cur := cur.tail;
    END;
    RETURN count;
  END CountFocusable;

PROCEDURE RotForward(self: T) = 
  BEGIN
    self.visPanes := PaneList.Cons(
                         PaneListExtras.LastDelD(self.visPanes),
                         self.visPanes);
  END RotForward;

PROCEDURE RotFocus(self: T; reverse: BOOLEAN := FALSE) =
  VAR
    count := CountFocusable(self);
  BEGIN
    IF (count >= 2) OR (count=1 AND NOT self.visPanes.head.hilited) THEN
      IF self.visPanes.head.hilited THEN
        self.visPanes.head.frameHilite(FALSE);
      END;
      REPEAT
        IF reverse THEN
          self.visPanes := PaneList.AppendD(self.visPanes.tail,
                                      PaneList.List1(self.visPanes.head));
        ELSE
          RotForward(self);
        END;
      UNTIL self.visPanes.head.canHilite;
      self.visPanes.head.frameHilite(TRUE);
    END;
  END RotFocus;

PROCEDURE FocusTo(self: T; pane: VBT.T) =
  VAR
    original := self.visPanes.head;
  BEGIN
    IF NOT NARROW(pane, PaneFrame.T).hilited THEN
      WHILE self.visPanes.head # pane DO
        RotForward(self);
        <* ASSERT original # self.visPanes.head *>  (*not found*)
      END;
      Debug.S("Re-hiliting due to a Focus-to", DebugLevel);
      IF original.hilited THEN
        Debug.S("FocusTo unhilites original", DebugLevel);
        original.frameHilite(FALSE);
      END;
      Debug.S("FocusTo hilites pane", DebugLevel);
      self.visPanes.head.frameHilite(TRUE);
    END;
  END FocusTo;

PROCEDURE SwapFocus(self: T; to: PaneFrame.T) =
  BEGIN
    to.canHilite := TRUE;
    IF to # self.visPanes.head THEN
      self.visPanes.head.frameHilite(FALSE);
      self.visPanes := PaneList.Cons(to, PaneListExtras.DeleteD(
                                             self.visPanes, to));
      to.frameHilite(TRUE);
    END;
  END SwapFocus;

PROCEDURE SplitPane(self: T; hv: Axis.T) =
  BEGIN
    IF self.miniPane = self.visPanes.head THEN
      self.print("Cannot split minipane");
    ELSE
      VAR
        ch1 := self.visPanes.head;
        ch2 := ch1.frameClone();
        split := HVSplit.New(hv);
        bar := HVBar.New(size := 1.0, txt := Pixmap.Solid);
      BEGIN
        Debug.S("PaneManVBT.SplitPane", DebugLevel);
        self.visPanes.head.frameHilite(FALSE);
        Debug.S("SplitPane: FindHead");
        <* ASSERT ch1 # NIL *>
        <* ASSERT ch2 # NIL *>
        (* self.paneUpdater.apply(ch2); *)
        FindHead(self, Act.Replace, split);
        Debug.S("SplitPane: AddChild", DebugLevel);
        Split.AddChild(split, ch1, bar, ch2);
        Debug.S("SplitPane: Augment visPanes", DebugLevel);
        self.visPanes := PaneList.Cons(ch2, self.visPanes);
        ch2.frameHilite(TRUE);
        ch2.frameVisible(TRUE);
        Debug.S("Completed SplitPane", DebugLevel);
      END;
    END;
  END SplitPane;

PROCEDURE HidePane(p: PaneFrame.T) =
  BEGIN
    IF p.hilited THEN
      p.frameHilite(FALSE);
      p.frameVisible(FALSE);
      IF NOT p.primary THEN
        p.frameDiscard();
      END;
    END;
  END HidePane;

(* a hack: call this proc to ensure that the focus is reacquired after
   changing which pane is focused. *)
PROCEDURE ReAcquire(self: T) =
  VAR
    cd: VBT.MiscRec;
  BEGIN
    Debug.S("PaneMan: ReAcquire", DebugLevel);
    cd.type := VBT.TakeSelection;
    cd.time := self.time;
    cd.detail := VBT.NullDetail;
    cd.selection := VBT.KBFocus;
    self.visPanes.head.focusWarn();
    MiscNoLock(self, cd);
    self.visPanes.head.hilited := FALSE;
  END ReAcquire;

PROCEDURE UnSplit(self: T) =
  VAR
    base := Split.Nth(self, 0);
    head := self.visPanes.head;
  BEGIN
    IF base = head THEN
    ELSIF head = self.miniPane THEN
    ELSE
(*      head.frameHilite(FALSE);  *)
      FindHead(self, Act.Delete);
      base := Split.Nth(self, 0);
      <* ASSERT base # self.visPanes.head *>
      Split.Replace(self,
                    base,
                    self.visPanes.head);
      Debug.S("UnSplit: Replaced base with head", DebugLevel);
      WHILE self.visPanes.tail # NIL DO
        self.visPanes := self.visPanes.tail;
        IF self.visPanes.head # self.miniPane THEN
          FindHead(self, Act.Delete);
          HidePane(self.visPanes.head);
        END;
      END;
      self.visPanes := PaneList.List2(head, self.miniPane);
      ReAcquire(self);
    END;
  END UnSplit;

PROCEDURE UnSplit1(self: T) =
  VAR
    base := Split.Nth(self, 0);
  BEGIN
    IF base = self.visPanes.head THEN
      self.print("Cannot hide sole visible pane");
    ELSE
      FindHead(self, Act.Delete);
      HidePane(self.visPanes.head);
      self.visPanes := self.visPanes.tail;
      IF self.visPanes.head.canHilite THEN
        self.visPanes.head.frameHilite(TRUE);
      ELSE
        RotFocus(self);
      END;
    END;
  END UnSplit1;

PROCEDURE Misc(self: T; READONLY cd: VBT.MiscRec) =
  BEGIN
    LOCK self.mu DO
      MiscNoLock(self, cd);
    END;
  END Misc;

PROCEDURE MiscNoLock(self: T; READONLY cd: VBT.MiscRec) =
  BEGIN
    self.time := cd.time;
    IF cd.type = VBT.TakeSelection THEN
      TRY
        VBT.Acquire(self, cd.selection, cd.time);
      EXCEPT
        VBT.Error =>
      END;
      self.visPanes.head.frameHilite(TRUE);
    ELSIF cd.type = VBT.Lost THEN
      IF self.stealFocusWarning THEN
  Debug.S("PaneMan: Not unhiliting because of the focus warning.", DebugLevel);
        self.stealFocusWarning := FALSE;
      ELSE
        Debug.S("PaneMan: Unhilighting due to focus loss.", DebugLevel);
        self.visPanes.head.frameHilite(FALSE);
      END;
    ELSE
      HVSplit.T.misc(self, cd);
    END;
  END MiscNoLock; 

PROCEDURE Key(self: T; READONLY key: VBT.KeyRec) =
  VAR
    head := self.visPanes.head;
  BEGIN
    LOCK self.mu DO
      self.time := key.time;
      Debug.S("PaneMan Got Key.", DebugLevel);
      IF head.canHilite AND NOT head.hilited THEN
        (* hack: See ReAcquire. *)
        Debug.S("Noticed head was not hilited. Fixing.");
        head.frameHilite(TRUE);
      END;
      self.daemon.key(key);
    END;
  END Key;

PROCEDURE CookedKey(self: T; READONLY key: VBT.KeyRec) =
  VAR
    cb := self.inputCallback;
    t: TEXT;
    hook := FALSE;
  PROCEDURE Parse(): BOOLEAN =
    BEGIN
      t := self.miniPane.getText();
      IF Text.Equal(Text.Sub(t, 0, Text.Length(self.inputPrompt)),
                    self.inputPrompt) THEN
        t := Text.Sub(t, Text.Length(self.inputPrompt), LAST(INTEGER));
        RETURN TRUE;
      ELSE
        self.print("prompt was changed");
        RETURN FALSE;
      END;
    END Parse;
  BEGIN
    Debug.S("Cooked: " & Fmt.Bool(key.wentDown) &
      Fmt.Int(key.whatChanged), DebugLevel);
    IF key.wentDown AND self.miniPane.hilited AND self.inputCallback # NIL THEN
      CASE key.whatChanged OF
      | 65293, 65241 =>
        hook := TRUE;
        self.focusTo(self.inputPane);
        self.miniPane.canHilite := FALSE;
        self.inputCallback := NIL;
        IF Parse() THEN
          cb.accept(t);
        END;
      | 65289 =>
        hook := TRUE;
        IF Parse() THEN
          cb.complete(t);
          self.miniPane.setText(self.inputPrompt & t);
        END;
      | ORD('g') =>
        IF VBT.Modifier.Control IN key.modifiers THEN
          self.focusTo(self.inputPane);
          self.miniPane.canHilite := FALSE;
          self.inputCallback := NIL;
          self.print("cancel");
        END;
      ELSE
      END;
    END;
    IF NOT hook THEN
      self.visPanes.head.key(key);
    END;
  END CookedKey;

PROCEDURE FindPane(self: T; path: TEXT): PaneFrame.T =
  VAR
    cur: PaneList.T;
  BEGIN
(*    Debug.S("yo"); *)
    IF path # NIL THEN
      cur := self.primPanes;
      WHILE cur # NIL DO
(*        Debug.S(cur.head.path & " & " & path,0); *)
        (* I want Pathname.Equal *)
        IF Text.Equal(cur.head.path, path) THEN
          RETURN cur.head;
        END;
        cur := cur.tail;
      END;
    END;
    RETURN NIL;
  END FindPane;

PROCEDURE SetPane(self: T; from: StarterScan.T): BOOLEAN =
  VAR
    elem: PaneFrame.T;
    path: TEXT;
  BEGIN
    Debug.S("PaneManVBT.SetPane", DebugLevel);
    path := StarterScan.GetPath(from);
    elem := FindPane(self, path);
    IF elem = NIL THEN
      Debug.S("Not an existing path", DebugLevel);
      elem := StarterScan.NewPaneFrame(self.starters, self, from);
    ELSIF elem.visible THEN
      Debug.S("An existing path", DebugLevel);
      IF elem = self.visPanes.head THEN
        elem := NIL;
      ELSE
        elem := elem.frameClone();
      END;
    END;
    IF elem # NIL THEN
      self.paneUpdater.apply(elem);
      FindHead(self, Act.Replace, elem);
      HidePane(self.visPanes.head);
      self.visPanes := PaneList.Cons(elem, self.visPanes.tail);
      self.primPanes := PaneList.Cons(elem, self.primPanes);
      elem.frameHilite(TRUE);
      elem.frameVisible(TRUE);
      RETURN TRUE;
    END;
    RETURN FALSE;
  END SetPane;

PROCEDURE ListPanes(self: T) =
  BEGIN
    IF self.miniPane = self.visPanes.head THEN
      self.print("Cannot list panes in minipane")
    ELSE
      EVAL self.setPane(StarterScan.FromStarter(ListPaneStarter.S,
                                                "All Panes"));
    END;
  END ListPanes;

PROCEDURE Print(self: T; message: TEXT) =
  BEGIN
    (* self.rotFocus(); *)
    self.miniPane.canHilite := FALSE;
    self.miniPane.setText(message);
  END Print;

PROCEDURE Input(self: T; prompt, default: TEXT; result: InputCallback) =
  BEGIN
    IF self.miniPane = self.visPanes.head THEN
      self.print("Command attempted to use minipane while in minipane");
    ELSE
      self.inputPane := self.visPanes.head;
      self.inputPrompt := prompt;
      self.print(prompt & default);
      SwapFocus(self, self.miniPane);
      self.inputCallback := result;
      ReAcquire(self);
    END;
  END Input;

TYPE
  PathCallback = InputCallback OBJECT
    v: T;
    forWrite: BOOLEAN;
  OVERRIDES
    accept := AcceptPath;
    complete := CompletePath;
  END;

PROCEDURE CompletePath(<*UNUSED*>cb: PathCallback; VAR t: TEXT) =
  BEGIN
    t := PathnameUtils.Complete(t);
  END CompletePath;

PROCEDURE AcceptPath(cb: PathCallback; result: TEXT) =
  VAR
    self := cb.v;
  BEGIN
    IF cb.forWrite THEN
      self.visPanes.head.frameWrite(result);
    ELSE
      EVAL self.setPane(StarterScan.FromPath(result));
      ReAcquire(self);
    END;
  END AcceptPath;

PROCEDURE InputPath(self: T; forWrite: BOOLEAN) =
  VAR
    path := self.visPanes.head.path;
    prompt: TEXT;
  BEGIN
    IF forWrite THEN
      prompt := "Write file: ";
    ELSE
      prompt := "Find file: ";
      path := PathnameUtils.SlashedPrefix(path);
    END;
    self.pathCallback.forWrite := forWrite;
    self.input(prompt, path, self.pathCallback);
  END InputPath;

PROCEDURE Init(self: T; starters: StarterList.T;
               startingStarters: StarterScanList.T := NIL): T =
  VAR
    bar := HVBar.New(size := 1.0, txt := Pixmap.Solid);
    docPane: PaneFrame.T;
    starts := startingStarters;
  BEGIN
    self.mu := NEW(MUTEX);
    LOCK self.mu DO
      Debug.S("Initing the PaneMan.");
      EVAL HVSplit.T.init(self, Axis.T.Ver, adjustable := FALSE);
      self.paneUpdater := NEW(NullPaneUpdater);
      self.pathCallback := NEW(PathCallback, v := self);
      self.starters := starters;
      self.request := NEW(RequestDaemon.T).init(self);
      self.daemon := NEW(KeyDaemon.T).init(self);
      self.miniPane := NEW(MiniPane.T).init("No MiniPane Path",
                                            "MiniPane", "MiniPane", self,
                                            NIL);
      self.miniPane.canHilite := FALSE;
      IF starts = NIL THEN
        starts := StarterScanList.List1(StarterScan.Default());
      END;
      docPane := StarterScan.NewPaneFrame(self.starters, self, starts.head);
      starts := starts.tail;
      self.visPanes := PaneList.List2(docPane, self.miniPane);
      self.primPanes := PaneList.List1(docPane);
      self.visPanes.head.hilite(TRUE);
      Split.AddChild(self, docPane, bar, self.miniPane);
      WHILE starts # NIL DO
        SplitPane(self, Axis.T.Ver);
        EVAL SetPane(self, starts.head);
        starts := starts.tail;
      END;
      RETURN self;
    END;
  END Init;

PROCEDURE GetSelectedPane(self: T): VBT.T =
  BEGIN
    RETURN self.visPanes.head;
  END GetSelectedPane;

BEGIN
END PaneManVBT.
