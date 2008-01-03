(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Mon Jan 30 15:19:06 PST 1995 by kalsow *)
(*      modified on Thu Apr 21 17:35:11 PDT 1994 by mhb    *)
(*      modified on Sun May 30 10:46:50 PDT 1993 by meehan *)
<* PRAGMA LL *>

MODULE IvyModel;

IMPORT ASCII, KeyFilter, MTextUnit, PaintOp, Text, TextPort,
       TextPortClass, VBT, VTDef, VText;
FROM TextPortClass IMPORT IRange;

REVEAL
  T = TextPortClass.Model BRANDED OBJECT
        dragButton       : VBT.Button;
        clipboard                       := "";
        sourceInClipboard               := TRUE;
      OVERRIDES
        controlChord := ControlChord;
        copy         := Copy;
        highlight    := Highlight;
        init         := Init;
        misc         := Misc;
        mouse        := Mouse;
        optionChord  := OptionChord;
        position     := Position;
        read         := Read;
        write        := Write;
      END;

CONST
  Primary   = TextPort.SelectionType.Primary;
  Secondary = TextPort.SelectionType.Secondary;
  Source    = TextPortClass.VType.Source;
  Target    = TextPortClass.VType.Target;

VAR
  SwapHighlights := VBT.GetMiscCodeType ("SwapHighlights");
  IvyMove        := VBT.GetMiscCodeType ("IvyMove");

PROCEDURE Init (m: T; colorScheme: PaintOp.ColorScheme; keyfilter: KeyFilter.T):
  TextPortClass.Model =
  BEGIN
    TRY
      m.selection [Primary] :=
        NEW (
          TextPortClass.SelectionRecord, type := Primary, alias := VBT.Target,
          interval := VText.CreateInterval (
                        vtext := m.v.vtext, indexL := 0, indexR := 0,
                        options := VText.MakeIntervalOptions (
                                     style := VText.IntervalStyle.NoStyle,
                                     whiteBlack := colorScheme,
                                     whiteStroke := colorScheme,
                                     leading := colorScheme.bg)));
      m.selection [Secondary] :=
        NEW (TextPortClass.SelectionRecord, type := Secondary,
             alias := VBT.Source,
             interval := VText.CreateInterval (
                           vtext := m.v.vtext, indexL := 0, indexR := 0,
                           options := VText.MakeIntervalOptions (
                                        style := VText.IntervalStyle.NoStyle,
                                        whiteBlack := colorScheme,
                                        whiteStroke := colorScheme,
                                        leading := colorScheme.bg)));
      m.keyfilter := NEW (TextPortClass.Composer, next := keyfilter)
    EXCEPT
    | VTDef.Error (ec) => m.v.vterror ("Model Init", ec)
    END;
    RETURN m
  END Init;

PROCEDURE ControlChord (m: T; ch: CHAR; READONLY cd: VBT.KeyRec) =
  VAR
    time := cd.time;
    v    := m.v;
  PROCEDURE delete (READONLY e: TextPort.Extent) =
    BEGIN
      IF e # TextPort.NotFound THEN m.select (time, e.l, e.r) END
    END delete;
  PROCEDURE toWord (READONLY e: TextPort.Extent; forward := TRUE) =
    BEGIN
      IF e # TextPort.NotFound THEN
        m.select (time, e.l, e.r, Primary,
                  caretEnd := VAL (ORD (forward), VText.WhichEnd))
      END
    END toWord;
  BEGIN
    CASE ch OF
    | ' ' =>                   (* Just normalize. *)
    | ',' => find (m, time, Secondary, TextPortClass.Loc.Next)
    | ';' => ToEndOfLine (m, time)
    | 'a' => delete (TextPortClass.DeletePrevChar (v))
    | 'b' => delete (TextPortClass.DeleteCurrentLine (v))
    | 'c' => delete (TextPortClass.DeleteToStartOfLine (v))
    | 'd' => delete (TextPortClass.DeleteToStartOfWord (v))
    | 'e' => Move (m, time, MoveAndClear)
    | 'f' => delete (TextPortClass.DeleteToEndOfWord (v))
    | 'g' => delete (TextPortClass.DeleteCurrentWord (v))
    | 'h' => ExchangeSelections (m, time);
    | 'i' => toWord (TextPortClass.FindNextWord (v))
    | 'j' => TextPortClass.ToPrevChar (v); sci (m, time)
    | 'k' => TextPortClass.ToNextChar (v); sci (m, time)
    | 'l' => ToStartOfLine (m, time)
    | 'm' => find (m, time, Secondary, TextPortClass.Loc.Prev)
    | 'n' => find (m, time, Primary, TextPortClass.Loc.Next)
    | 'o' => TextPortClass.UpOneLine (v); sci (m, time)
    | 'p' => TextPortClass.DownOneLine (v); sci (m, time)
    | 'q' => m.clear ()
    | 'r' => Swap (m, time)
    | 's' => delete (TextPortClass.DeleteNextChar (v))
    | 'u' => toWord (TextPortClass.FindPrevWord (v), FALSE)
    | 'v' => delete (TextPortClass.DeleteToEndOfLine (v))
    | 'w' => Move (m, time, JustMove)
    | 'y' => TextPortClass.ToOtherEnd (v); sci (m, time)
    | 'z' => TextPortClass.Undo (v)
    | 'Z' => TextPortClass.Redo (v)
    ELSE
      (* Don't normalize if unknown chord, including just ctrl itself. *)
      RETURN
    END;
    m.v.normalize (-1)
  END ControlChord;

PROCEDURE OptionChord (m: T; ch: CHAR; READONLY cd: VBT.KeyRec) =
  VAR
    time := cd.time;
    v    := m.v;
  BEGIN
    CASE ch OF
    | 'c' => m.copy (time)
    | 'm' => find (m, time, Primary, TextPortClass.Loc.Prev)
    | 'n' => find (m, time, Primary, TextPortClass.Loc.First)
    | 'v' => m.paste (time)
    | 'x' => m.cut (time)
    | ASCII.BS, ASCII.DEL =>
        TextPortClass.SwapChars (v);
        WITH index = m.v.index () DO m.select (time, index - 2, index) END
    | ASCII.NL => TextPortClass.InsertNewline (v); sci (m, time)
    ELSE
      (* Don't normalize if unknown chord, including just option itself. *)
      RETURN
    END;
    m.v.normalize (-1)
  END OptionChord;

PROCEDURE Copy (m: T; time: VBT.TimeStamp) =
  CONST name = "Copy";
  VAR t := m.getSelectedText (Primary);
  BEGIN
    IF NOT Text.Empty (t) AND m.takeSelection (VBT.Source, Secondary, time) THEN
      m.clipboard := t;
      m.sourceInClipboard := TRUE;
      TRY
        VText.SwitchInterval (
          m.selection [Secondary].interval, VText.OnOffState.Off)
      EXCEPT
      | VTDef.Error (ec) => m.v.vterror (name, ec)
      END
    END
  END Copy;

PROCEDURE sci (m: T; time: VBT.TimeStamp) = (* Select Current Index *)
  BEGIN
    WITH index = m.v.index () DO m.select (time, index, index) END
  END sci;

PROCEDURE find (m   : T;
                time: VBT.TimeStamp;
                type: TextPort.SelectionType;
                loc : TextPortClass.Loc       ) =
  BEGIN
    IF type = Primary THEN m.copy (time) END;
    m.v.findSource (time, loc)
  END find;

PROCEDURE ExchangeSelections (m: T; time: VBT.TimeStamp) =
  CONST name = "Exchange";
  VAR
    primary   := m.selection [Primary];
    intvl_1   := primary.interval;
    secondary := m.selection [Secondary];
    intvl_2   := secondary.interval;
    pLeft     := intvl_1.left ();
    pRight    := intvl_1.right ();
    sLeft     := intvl_2.left ();
    sRight    := intvl_2.right ();
  BEGIN
    TRY
      IF NOT m.v.owns [Target] THEN RETURN END;
      (* This VBT owns the primary.  Does it own both? *)
      IF m.v.owns [Source] THEN
        IF m.v.index () = pLeft THEN
          m.highlight (primary, IRange {sLeft, sLeft, sRight})
        ELSE
          m.highlight (primary, IRange {sLeft, sRight, sRight})
        END
      ELSE
        (* This is more complex.  Must tell the owner of the secondary that
           this is going to be a swap, and then grab the secondary. *)
        VBT.Put (m.v, VBT.Source, time, SwapHighlights);
        VText.SwitchInterval (intvl_1, VText.OnOffState.Off);
        EVAL m.takeSelection (VBT.Source, Secondary, time)
      END;
      (* Now, move the secondary selection to what used to be the primary
         interval. *)
      m.highlight (secondary, IRange {pLeft, pRight, pRight})
    EXCEPT
    | VTDef.Error (ec) => m.v.vterror (name, ec)
    | VBT.Error (ec) => m.v.vbterror (name, ec)
    END
  END ExchangeSelections;

PROCEDURE Mouse (m: T; READONLY cd: VBT.MouseRec) =
  CONST name = "Mouse";
  VAR type: TextPort.SelectionType;
  BEGIN
    IF (VBT.Modifier.Control IN cd.modifiers)
         OR (VBT.Modifier.Shift IN cd.modifiers) THEN
      type := Secondary
    ELSE
      type := Primary
    END;
    IF type = Primary AND NOT m.v.getKFocus(cd.time) THEN RETURN END;
    VAR
      sel                            := Map[type];
      rec                            := m.selection[type];
      interval                       := rec.interval;
      r       : TextPortClass.IRange;
    BEGIN
      TRY
        CASE cd.clickType OF
        | VBT.ClickType.FirstDown =>
            IF m.takeSelection(sel, type, cd.time) THEN
              IF type = Secondary THEN
                m.sourceInClipboard := FALSE
              END;
              CASE cd.whatChanged OF
              | VBT.Modifier.MouseL, VBT.Modifier.MouseM =>
                  rec.mode :=
                    selectionModes[
                      cd.whatChanged, MIN(cd.clickCount, 4)];
                  IF type = Primary THEN
                    rec.replaceMode := FALSE
                  END;
                  TextPortClass.ChangeIntervalOptions(m.v, rec);
                  r :=
                    TextPortClass.GetRange(m.v, cd.cp, rec.mode);
                  (* Select only the point between characters. *)
                  IF rec.mode = VText.SelectionMode.CharSelection THEN
                    r.left := r.middle;
                    r.right := r.middle
                  END;
                  rec.anchor.l := r.left;
                  rec.anchor.r := r.right;
                  m.highlight(rec, r)
              | VBT.Modifier.MouseR =>
                  IF cd.clickCount < 2 THEN (* single click *)
                    IF interval.left() >= m.v.typeinStart THEN
                      IF type = Primary THEN
                        rec.replaceMode := NOT m.v.readOnly
                      END;
                      TextPortClass.ChangeIntervalOptions(
                        m.v, rec)
                    END
                  ELSIF rec.mode > FIRST(VText.SelectionMode) THEN
                    (* multi-clicking: make the selection-mode
                       smaller. *)
                    DEC(rec.mode)
                  END;
                  r :=
                    TextPortClass.GetRange(m.v, cd.cp, rec.mode);
                  m.approachingFromLeft :=
                    r.left < (rec.anchor.l + rec.anchor.r) DIV 2;
                  m.extend(rec, r.left, r.right)
              ELSE
              END;
              m.dragButton := cd.whatChanged;
              m.dragType := type;
              m.dragging := TRUE
            END
        | VBT.ClickType.LastUp =>
            IF m.dragging THEN
              rec.anchor.l := rec.interval.left();
              rec.anchor.r := rec.interval.right();
              m.dragging := FALSE
            END
        ELSE
          m.dragging := FALSE
        END
      EXCEPT
      | VTDef.Error (ec) => m.v.vterror(name, ec)
      END
    END
  END Mouse;

PROCEDURE Position (m: T; READONLY cd: VBT.PositionRec) =
  VAR
    rec  := m.selection [m.dragType];
    r    := TextPortClass.GetRange (m.v, cd.cp, rec.mode);
  BEGIN
    CASE m.dragButton OF
    | VBT.Modifier.MouseL, VBT.Modifier.MouseM => m.highlight (rec, r)
    | VBT.Modifier.MouseR => m.extend (rec, r.left, r.right)
    ELSE
    END
  END Position;

(***********************  Reading  ****************************)

PROCEDURE Read (m: T; READONLY s: VBT.Selection; time: VBT.TimeStamp): TEXT
  RAISES {VBT.Error} =
  BEGIN
    IF s = VBT.Source AND m.v.owns [Source] THEN
      IF m.sourceInClipboard THEN
        RETURN m.clipboard
      ELSE
        RETURN m.getSelectedText (Secondary)
      END
    ELSIF s = VBT.Target AND m.v.owns [Target] THEN
      RETURN m.getSelectedText (Primary)
    ELSE
      RETURN TextPortClass.Model.read (m, s, time)
    END
  END Read;

(***********************  Writing  ****************************)

PROCEDURE Write (m: T; READONLY s: VBT.Selection; time: VBT.TimeStamp; t: TEXT)
  RAISES {VBT.Error} =
  PROCEDURE write (t: TEXT; type: TextPort.SelectionType) RAISES {VBT.Error} =
    BEGIN
      IF m.selection [type].interval.left () < m.v.typeinStart THEN
        RAISE VBT.Error (VBT.ErrorCode.Unwritable)
      ELSE
        m.putSelectedText (t, type)
      END
    END write;
  BEGIN
    IF s = VBT.Source AND m.v.owns [Source] THEN
      IF m.sourceInClipboard THEN
        m.clipboard := t
      ELSE
        write (t, Secondary)
      END
    ELSIF s = VBT.Target AND m.v.owns [Target] THEN
      write (t, Primary)
    ELSE
      TextPortClass.Model.write (m, s, time, t)
    END
  END Write;

(***************** Other things *************************)

PROCEDURE Misc (m: T; READONLY cd: VBT.MiscRec) =
  BEGIN
    IF cd.type = SwapHighlights THEN
      (* We owned Source, so grab Target and put Source's highlights on it. *)
      VAR
        i     := m.selection [Secondary].interval;
        left  := i.left ();
        right := i.right ();
      BEGIN
        IF m.takeSelection (VBT.Target, Primary, cd.time) THEN
          m.highlight (m.selection [Primary], IRange {left, right, right})
        END
      END
    ELSIF cd.type = IvyMove THEN
      Move (m, cd.time, cd.detail)
    END;
    TextPortClass.Model.misc (m, cd)
  END Misc;

PROCEDURE ToStartOfLine (m: T; time: VBT.TimeStamp) =
  VAR
    index := m.v.index ();
    mtext := m.v.vtext.mtext;
    line  := MTextUnit.LineInfo (mtext, index);
    x     := m.selection [Primary].interval;
  BEGIN
    IF x.left () = line.left AND x.right () = line.right AND index = line.left
         AND line.left > 0 THEN
      line := MTextUnit.LineInfo (mtext, line.left - 1)
    END;
    m.select (time, line.left, line.right, caretEnd := VText.WhichEnd.Left)
  END ToStartOfLine;

PROCEDURE ToEndOfLine (m: T; time: VBT.TimeStamp) =
  VAR
    index := m.v.index ();
    mtext := m.v.vtext.mtext;
    line  := MTextUnit.LineInfo (mtext, index);
    x     := m.selection [Primary].interval;
  BEGIN
    IF x.left () = line.left AND x.right () = line.rightEnd
         AND index = line.rightEnd AND line.rightEnd < m.v.length () THEN
      line := MTextUnit.LineInfo (mtext, line.right)
    END;
    m.select (time, line.left, line.rightEnd)
  END ToEndOfLine;

PROCEDURE Swap (m: T; time: VBT.TimeStamp) =
  BEGIN
    TRY
      WITH primaryValue   = m.read (VBT.Target, time),
           secondaryValue = m.read (VBT.Source, time)  DO
        m.write (VBT.Target, time, secondaryValue);
        m.write (VBT.Source, time, primaryValue)
      END
    EXCEPT
    | VBT.Error (ec) => m.v.vbterror ("Swap", ec)
    END
  END Swap;

CONST
  MoveAndClear = VBT.MiscCodeDetail {0, 1};
  JustMove     = VBT.NullDetail;

PROCEDURE Move (m: T; time: VBT.TimeStamp; READONLY detail: VBT.MiscCodeDetail) =
  VAR v := m.v;
  BEGIN
    TRY
      IF v.owns [Target] THEN
        VAR
          rec  := m.selection [Primary];
          text := m.read (VBT.Source, time);
          p    := rec.cursor;
        BEGIN
          IF v.isReplaceMode () THEN
            m.putSelectedText (text, Primary)
          ELSIF v.replace (p, p, text) # TextPort.NotFound THEN
            INC (p, Text.Length (text));
            m.highlight (rec, IRange {p, p, p})
          END;
          IF detail = MoveAndClear THEN m.write (VBT.Source, time, "") END
        END
      ELSE
        VBT.Put (v, VBT.Target, time, IvyMove, detail)
      END
    EXCEPT
    | VBT.Error (ec) => v.vbterror ("Move", ec)
    END
  END Move;

PROCEDURE Highlight (m  : T;
                     rec: TextPortClass.SelectionRecord;
                     READONLY r: IRange) =
  (* The only difference in this version is the test for
     rec.type. *)
  CONST name = "Highlight";
  BEGIN
    TRY
      VText.MoveInterval (rec.interval, r.left, r.right);
      VText.SwitchInterval (rec.interval, VText.OnOffState.On);
      IF rec.type = Primary THEN
        rec.cursor := r.middle;
        m.seek (r.middle)
      END;
      VBT.Mark (m.v)
    EXCEPT
    | VTDef.Error (ec) => m.v.vterror (name, ec)
    END
  END Highlight;
  

VAR
  Map := ARRAY TextPort.SelectionType OF
           VBT.Selection {VBT.Target, VBT.Source};

VAR
  selectionModes: ARRAY [VBT.Modifier.MouseL .. VBT.Modifier.MouseR],
                    [0 .. 4] OF
                    VText.SelectionMode;

BEGIN

  selectionModes [VBT.Modifier.MouseL, 0] :=
    VText.SelectionMode.CharSelection;
  selectionModes [VBT.Modifier.MouseL, 1] :=
    VText.SelectionMode.CharSelection;
  selectionModes [VBT.Modifier.MouseL, 2] :=
    VText.SelectionMode.LineSelection;
  selectionModes [VBT.Modifier.MouseL, 3] :=
    VText.SelectionMode.LineSelection;
  selectionModes [VBT.Modifier.MouseL, 4] := VText.SelectionMode.AllSelection;

  selectionModes [VBT.Modifier.MouseM, 0] :=
    VText.SelectionMode.WordSelection;
  selectionModes [VBT.Modifier.MouseM, 1] :=
    VText.SelectionMode.WordSelection;
  selectionModes [VBT.Modifier.MouseM, 2] :=
    VText.SelectionMode.ParagraphSelection;
  selectionModes [VBT.Modifier.MouseM, 3] :=
    VText.SelectionMode.ParagraphSelection;
  selectionModes [VBT.Modifier.MouseM, 4] := VText.SelectionMode.AllSelection;

END IvyModel.
