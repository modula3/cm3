(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jul  4 10:00:25 PDT 1995 by mhb                      *)

MODULE TextPortWithButtons;

IMPORT TextPort, PaintOp, VBT, VText, VTDef, Rect;
IMPORT TextPortButton, TextPortButtonSeq, Rd, Thread;

REVEAL
  T = Public BRANDED OBJECT
    buttons: TextPortButtonSeq.T;
    regular, highlight: VText.IntervalOptions;
  OVERRIDES
    init := Init;
    insertButton := InsertButton;
    numberOfButtons := NumberOfButtons;
    getButton := GetButton;
    clearButtons := ClearButtons;
    mouse := Mouse;
  END;

PROCEDURE Init(self: T; readOnly: BOOLEAN): T =
  VAR
    regularScheme :=
       PaintOp.MakeColorScheme(PaintOp.FromRGB(0.425, 0.765, 0.85,
                                               mode := PaintOp.Mode.Accurate),
                               PaintOp.FromRGB(0.0, 0.0, 0.0));
    highlightScheme :=
       PaintOp.MakeColorScheme(PaintOp.FromRGB(0.0, 0.0, 0.0),
                               PaintOp.FromRGB(0.425, 0.765, 0.85,
                                               mode := PaintOp.Mode.Accurate));
  BEGIN
    EVAL TextPort.T.init(self, readOnly := readOnly);
    self.buttons := NEW(TextPortButtonSeq.T).init(1);
    self.regular := VText.MakeIntervalOptions(
                        style := VText.IntervalStyle.BoxStyle,
                        whiteBlack := regularScheme,
                        whiteStroke := regularScheme,
                        leading := regularScheme.bg);
    self.highlight := VText.MakeIntervalOptions(
                          style := VText.IntervalStyle.BoxStyle,
                          whiteBlack := highlightScheme,
                          whiteStroke := highlightScheme,
                          leading := highlightScheme.bg);
    RETURN self
  END Init;

PROCEDURE InsertButton(self: T; b: TextPortButton.T) =
  VAR
    start := TextPort.Index(self);
  <*FATAL VTDef.Error*>
  BEGIN
    TextPort.Insert(self, "  " & b.label & "  ");
    WITH vtext = TextPort.GetVText(self),
         end = TextPort.Index(self) DO
      b.interval := VText.CreateInterval(vtext, start+1, end-1, self.regular);
      self.buttons.addhi(b);
      VText.SwitchInterval(b.interval, VText.OnOffState.On);
    END;
  END InsertButton;

PROCEDURE NumberOfButtons(self: T): CARDINAL =
  BEGIN
    RETURN self.buttons.size()
  END NumberOfButtons;

PROCEDURE GetButton(self: T; i: CARDINAL): TextPortButton.T =
  BEGIN
    RETURN self.buttons.get(i)
  END GetButton;

PROCEDURE ClearButtons(self: T) =
  BEGIN
    self.buttons := NEW(TextPortButtonSeq.T).init(1);
  END ClearButtons;

PROCEDURE Mouse(self: T; READONLY cd: VBT.MouseRec) =
  VAR
    index, left, right: VText.Index;
    button: TextPortButton.T;
    found := FALSE;
    vtext := TextPort.GetVText(self);
  <*FATAL VTDef.Error*>

  PROCEDURE CurrentIndex(): VText.Index =
    VAR
      left, middle, right: VText.Index;
      cage: Rect.T;
    BEGIN
      TRY
        EVAL VText.Pounce(vtext, 0, cd.cp.pt,
                          VText.SelectionMode.CharSelection,
                          left, middle, right, cage);
      EXCEPT Rd.Failure, Rd.EndOfFile, Thread.Alerted => (* skip *)
      END;
      RETURN middle
    END CurrentIndex;

  BEGIN
    index := CurrentIndex();
    FOR i := 0 TO self.buttons.size()-1 DO
      button := self.buttons.get(i);
      TextPortButton.GetExtents(button, left, right);
      IF left <= index AND index <= right THEN
        found := TRUE;
        EXIT
      END;
    END;
    IF found THEN
      CASE cd.clickType OF
      | VBT.ClickType.FirstDown =>
        VText.ChangeIntervalOptions(button.interval, self.highlight);
        TRY VText.Update(vtext) EXCEPT Rd.Failure, Rd.EndOfFile, Thread.Alerted => 
          (* skip *) 
        END;
      | VBT.ClickType.LastUp =>
        VText.ChangeIntervalOptions(button.interval, self.regular);
        button.callback(cd);
        TRY VText.Update(vtext) EXCEPT Rd.Failure, Rd.EndOfFile, Thread.Alerted => 
          (* skip *) 
        END;
      ELSE (* skip *)
      END;
    ELSE
      Public.mouse(self, cd);
    END;
  END Mouse;

BEGIN
END TextPortWithButtons.
