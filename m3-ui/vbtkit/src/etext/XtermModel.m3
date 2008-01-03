(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Thu Mar 18 12:37:51 PST 1993 by meehan *)
<* PRAGMA LL *>

MODULE XtermModel;

IMPORT KeyFilter, PaintOp, Text, TextPort, TextPortClass, VBT, VTDef, VText;

REVEAL
  T = TextPortClass.Model BRANDED OBJECT
        downclick : CARDINAL;
        dragButton: VBT.Button;
        comp      : TextPortClass.Composer
      OVERRIDES
        controlChord := ControlChord;
        copy         := Copy;
        init         := Init;
        mouse        := Mouse;
        optionChord  := OptionChord;
        paste        := Paste;
        position     := Position;
        read         := Read;
        write        := Write;
      END;

CONST
  Primary = TextPort.SelectionType.Primary;
  Source  = TextPortClass.VType.Source;

PROCEDURE Init (m: T; colorScheme: PaintOp.ColorScheme; keyfilter: KeyFilter.T):
  TextPortClass.Model =
  BEGIN
    TRY
      m.selection [Primary] :=
        NEW (TextPortClass.SelectionRecord,
             interval := VText.CreateInterval (
                           vtext := m.v.vtext, indexL := 0, indexR := 0,
                           options := VText.MakeIntervalOptions (
                                        style := VText.IntervalStyle.NoStyle,
                                        whiteBlack := colorScheme,
                                        whiteStroke := colorScheme,
                                        leading := colorScheme.bg)),
             alias := VBT.Source)
    EXCEPT
    | VTDef.Error (ec) => m.v.vterror ("Model Init", ec)
    END;
    m.keyfilter := NEW (TextPortClass.Composer, next := keyfilter);
    RETURN m
  END Init;

PROCEDURE ControlChord (m: T; ch: CHAR; <* UNUSED *> READONLY cd: VBT.KeyRec) =
  BEGIN
    CASE ch OF
    | ' ' =>                     (* Just normalize. *)
    | 'Z' => TextPortClass.Redo (m.v)
    | 'u' =>
        WITH ext = TextPortClass.DeleteToStartOfLine (m.v) DO
          IF ext # TextPort.NotFound THEN m.seek (ext.l) END
        END
    | 'z' => TextPortClass.Undo (m.v)
    ELSE
      (* Don't normalize if unknown chord, including just ctrl itself. *)
      RETURN
    END;
    m.v.normalize (-1)
  END ControlChord;

PROCEDURE OptionChord (m: T; ch: CHAR; READONLY cd: VBT.KeyRec) =
  BEGIN
    CASE ch OF
    | 'c' => m.copy (cd.time)
    | 'v' => m.paste (cd.time)
    | 'x' => m.cut (cd.time)
    ELSE
      (* Don't normalize if unknown chord, including just option itself. *)
      RETURN
    END
  END OptionChord;

PROCEDURE Mouse (m: T; READONLY cd: VBT.MouseRec) =
  CONST
    MODS  = VBT.Modifiers {VBT.Modifier.Control, VBT.Modifier.Option};
    EMPTY = VBT.Modifiers {};
  VAR
    r  : TextPortClass.IRange;
    rec                       := m.selection [Primary];
  BEGIN
    IF NOT m.v.getKFocus (cd.time) THEN RETURN END;
    CASE cd.clickType OF
    | VBT.ClickType.FirstDown =>
        IF cd.modifiers * MODS # EMPTY THEN RETURN END;
        CASE cd.whatChanged OF
        | VBT.Modifier.MouseL =>
            rec.mode :=
              selectionModes [cd.whatChanged, cd.clickCount DIV 2 MOD 3];
            r := TextPortClass.GetRange (m.v, cd.cp, rec.mode);
            IF rec.mode = VText.SelectionMode.CharSelection THEN
              (* Single left-click => Move only the typein point.  Don't
                 highlight, since the highlighted interval IS the Source
                 selection, and we don't want to change that. *)
              m.seek (r.middle);
              m.downclick := r.middle;
              m.dragging := TRUE;
              m.dragButton := VBT.Modifier.MouseL
            ELSIF m.takeSelection (VBT.Source, Primary, cd.time) THEN
              m.highlight (rec, r);
              m.dragging := TRUE;
              m.dragButton := VBT.Modifier.MouseL
            END;
        | VBT.Modifier.MouseR =>
            IF m.takeSelection (VBT.Source, Primary, cd.time) THEN
              r := TextPortClass.GetRange (m.v, cd.cp, rec.mode);
              m.approachingFromLeft :=
                r.left < (rec.anchor.l + rec.anchor.r) DIV 2;
              m.extend (rec, r.left, r.right);
              m.dragging := TRUE;
              m.dragButton := VBT.Modifier.MouseR
            END
        ELSE
        END                      (* CASE cd.whatChanged *)
    | VBT.ClickType.LastUp =>
        IF cd.whatChanged = VBT.Modifier.MouseM THEN m.paste (cd.time) END;
        IF m.dragging THEN
          rec.anchor.l := rec.interval.left ();
          rec.anchor.r := rec.interval.right ();
          m.dragging := FALSE
        END
    ELSE
      m.dragging := FALSE
    END                          (* CASE cd.clickType *)
  END Mouse;

PROCEDURE Position (m: T; READONLY cd: VBT.PositionRec) =
  VAR rec := m.selection [Primary];
  BEGIN
    (* If left-dragging, set the anchor to the point of the downclick, rather
       than extending the anchored range, and acquire Primary, since the mouse
       method didn't. *)
    IF m.dragButton = VBT.Modifier.MouseL AND rec.anchor.l # m.downclick
         AND m.takeSelection (VBT.Source, Primary, cd.time) THEN
      rec.anchor.l := m.downclick;
      rec.anchor.r := m.downclick
    END;
    TextPortClass.Model.position (m, cd)
  END Position;

(***********************  Reading  ****************************)

PROCEDURE Read (m: T; READONLY s: VBT.Selection; time: VBT.TimeStamp): TEXT
  RAISES {VBT.Error} =
  BEGIN
    IF s = VBT.Source AND m.v.owns [Source] THEN
      RETURN m.getSelectedText (Primary)
    ELSE
      RETURN TextPortClass.Model.read (m, s, time)
    END
  END Read;

(***********************  Writing  ****************************)

PROCEDURE Write (m: T; READONLY s: VBT.Selection; time: VBT.TimeStamp; t: TEXT)
  RAISES {VBT.Error} =
  BEGIN
    IF s = VBT.Source AND m.v.owns [Source] THEN
      IF m.selection [Primary].interval.left () >= m.v.typeinStart THEN
        m.putSelectedText (t, Primary)
      ELSE
        RAISE VBT.Error (VBT.ErrorCode.Unwritable)
      END
    ELSE
      TextPortClass.Model.write (m, s, time, t)
    END
  END Write;

(***************** Other things *************************)

PROCEDURE Copy (m: T; time: VBT.TimeStamp) =
  (* This is almost a no-op.  The selection *IS* the highlighted region.*)
  BEGIN
    IF m.selection [Primary].interval.left ()
         # m.selection [Primary].interval.right () THEN
      EVAL m.takeSelection (VBT.Source, Primary, time)
    END
  END Copy;

PROCEDURE Paste (m: T; time: VBT.TimeStamp) =
  BEGIN
    TRY
      WITH t   = m.read (VBT.Source, time),
           p   = m.v.index (),
           len = Text.Length (t)            DO
        IF len # 0 AND m.v.replace (p, p, t) # TextPort.NotFound THEN
          m.select (time, p, p + len)
        END
      END
    EXCEPT
    | VBT.Error (ec) => m.v.vbterror ("Paste", ec)
    END
  END Paste;

VAR
  selectionModes: ARRAY [VBT.Modifier.MouseL .. VBT.Modifier.MouseR],
                    [0 .. 2] OF
                    VText.SelectionMode;

BEGIN
  selectionModes [VBT.Modifier.MouseL, 0] :=
    VText.SelectionMode.CharSelection;
  selectionModes [VBT.Modifier.MouseL, 1] :=
    VText.SelectionMode.WordSelection;
  selectionModes [VBT.Modifier.MouseL, 2] :=
    VText.SelectionMode.LineSelection;
  selectionModes [VBT.Modifier.MouseR, 0] :=
    VText.SelectionMode.CharSelection;
  selectionModes [VBT.Modifier.MouseR, 1] :=
    VText.SelectionMode.WordSelection;
  selectionModes [VBT.Modifier.MouseR, 2] :=
    VText.SelectionMode.LineSelection;

END XtermModel.
