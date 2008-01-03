(* Copyright (C) 1992, Digital Equipment Corporation       *)
(* All rights reserved.                                    *)
(* See the file COPYRIGHT for a full description.          *)
(*                                                         *)
(* Last modified on Mon Jan 30 15:16:24 PST 1995 by kalsow *)
(*      modified on Sun May 30 10:50:33 PDT 1993 by meehan *)
<* PRAGMA LL *>

MODULE EmacsModel;

IMPORT ASCII, ISOChar, KeyboardKey, KeyFilter, KeyTrans, Latin1Key,
       MTextUnit, PaintOp, Rd, Text, TextPort, TextPortClass, Thread, VBT,
       VTDef, VText;
FROM TextPortClass IMPORT IRange;

REVEAL
  T = TextPortClass.Model BRANDED OBJECT
        clipboard                          := "";
        mark     : [-1 .. LAST (CARDINAL)] := -1; (* -1 => not set *)
        downclick: CARDINAL                := 0;
        append                             := FALSE;
        lit                                := FALSE;
      OVERRIDES
        controlChord := ControlChord;
        copy         := Copy;
        highlight    := Highlight;
        init         := Init;
        mouse        := Mouse;
        optionChord  := OptionChord;
        paste        := Paste;
        position     := Position;
        read         := Read;
        seek         := Seek;
        select       := Select;
        write        := Write;
      END;
  EscapeMetaFilter = KeyFilter.T BRANDED OBJECT
                       sawEscape := FALSE
                     OVERRIDES
                       apply := ApplyEMFilter
                     END;

TYPE
  KQFilter = KeyFilter.T OBJECT
               state      := State.Initial
             OVERRIDES
               apply := ApplyKQFilter
             END;
  State = {Initial, SawControlK, SawControlQ};

(* "EmacsModel.T.filter" is a finite-state machine that implements a
   1-character lookahead for control-K (successive control-K's append to the
   clipboard) and control-Q (quoted insert).  "Emacs.T.key" does the same for
   Escape (adding the Option modifier to the KeyRec). *)

CONST
  Primary = TextPort.SelectionType.Primary;
  Source  = TextPortClass.VType.Source;

PROCEDURE Init (m: T; colorScheme: PaintOp.ColorScheme; keyfilter: KeyFilter.T):
  TextPortClass.Model =
  BEGIN
    TRY
      m.selection [Primary] :=
        NEW (TextPortClass.SelectionRecord, type := Primary,
             interval := VText.CreateInterval (
                           vtext := m.v.vtext, indexL := 0, indexR := 0,
                           options := VText.MakeIntervalOptions (
                                        style := VText.IntervalStyle.NoStyle,
                                        whiteBlack := colorScheme,
                                        whiteStroke := colorScheme,
                                        leading := colorScheme.bg)),
             mode := VText.SelectionMode.CharSelection, alias := VBT.NilSel)
    EXCEPT
    | VTDef.Error (ec) => m.v.vterror ("Model Init", ec)
    END;
    m.keyfilter :=
      NEW (EscapeMetaFilter,
           next := NEW (KQFilter, next := NEW (TextPortClass.Composer,
                                               next := keyfilter)));
    RETURN m
  END Init;

PROCEDURE ControlChord (m: T; ch: CHAR; READONLY cd: VBT.KeyRec) =
  CONST name = "Control Key";
  VAR v := m.v;
  BEGIN
    TRY
      CASE ISOChar.Upper [ch] OF
      | ' ', '@' => SetMark (m, v.index ())
      | '_' => TextPortClass.Undo (v)
      | 'A' => TextPortClass.ToStartOfLine (v)
      | 'B' => TextPortClass.ToPrevChar (v)
      | 'D' => EVAL TextPortClass.DeleteNextChar (v)
      | 'E' => TextPortClass.ToEndOfLine (v)
      | 'F' => TextPortClass.ToNextChar (v)
      | 'H' => m.seek (TextPortClass.DeletePrevChar (v).l)
      | 'I' => m.v.ULtabAction (cd)
      | 'J' => m.v.newlineAndIndent ()
      | 'K' => IF NOT v.readOnly THEN Kill (m, v, cd) END
      | 'M' => m.v.ULreturnAction (cd)
      | 'N' => TextPortClass.DownOneLine (v)
      | 'O' => TextPortClass.InsertNewline (v)
      | 'P' => TextPortClass.UpOneLine (v)
        (* Control-Q is handled by the filter method. *)
      | 'R' => v.findSource (cd.time, TextPortClass.Loc.Prev)
      | 'S' => v.findSource (cd.time, TextPortClass.Loc.Next)
      | 'T' => TextPortClass.SwapChars (v)
      | 'V' => TextPortClass.ScrollOneScreenUp (v); RETURN
      | 'W' => m.cut (cd.time)
      | 'Y' => m.paste (cd.time)
      | 'Z' => TextPortClass.ScrollOneLineUp (v); RETURN
      ELSE
        (* Don't normalize if unknown chord, including just ctrl itself. *)
        RETURN
      END
    EXCEPT
    | VTDef.Error (ec) => m.v.vterror (name, ec)
    | Rd.Failure (ref) => m.v.rdfailure (name, ref)
    | Rd.EndOfFile => m.v.rdeoferror (name)
    | Thread.Alerted =>
    END;
    m.v.normalize (-1)
  END ControlChord;

PROCEDURE SetMark (m: T; point: CARDINAL) =
  VAR rec := m.selection [Primary];
  BEGIN
    m.mark := point;
    m.downclick := point;
    rec.anchor.l := point;
    rec.anchor.r := point;
    m.highlight (rec, IRange {point, point, point})
  END SetMark;

PROCEDURE Highlight (         m  : T;
                              rec: TextPortClass.SelectionRecord;
                     READONLY r  : IRange                         ) =
  CONST name = "Highlight";
  BEGIN
    TRY
      VText.MoveInterval (rec.interval, r.left, r.right);
      VText.SwitchInterval (
        rec.interval, VAL (ORD (m.lit), VText.OnOffState));
      VText.MoveCaret (m.v.vtext, r.middle);
      VBT.Mark (m.v)
    EXCEPT
    | VTDef.Error (ec) => m.v.vterror (name, ec)
    | Rd.EndOfFile => m.v.rdeoferror (name)
    | Rd.Failure (ref) => m.v.rdfailure (name, ref)
    | Thread.Alerted =>
    END
  END Highlight;

PROCEDURE Select (m          : T;
                  time       : VBT.TimeStamp;
                  begin      : CARDINAL        := 0;
                  end        : CARDINAL        := LAST (CARDINAL);
                  type                         := Primary;
                  replaceMode                  := FALSE;
                  caretEnd                     := VText.WhichEnd.Right) =
  BEGIN
    m.lit := TRUE;               (* Changes the highlighting *)
    IF caretEnd = VText.WhichEnd.Right THEN
      SetMark (m, begin)
    ELSE
      SetMark (m, MIN (end, m.v.length ()))
    END;
    TextPortClass.Model.select (
      m, time, begin, end, type, replaceMode, caretEnd)
  END Select;

PROCEDURE Seek (m: T; position: CARDINAL) =
  CONST name = "Seek";
  VAR rec := m.selection [Primary];
  BEGIN
    TRY
      VText.MoveCaret (m.v.vtext, position);
      IF m.approachingFromLeft AND position < rec.anchor.r
           OR NOT m.approachingFromLeft AND position <= rec.anchor.l THEN
        VText.MoveInterval (rec.interval, position, rec.anchor.r)
      ELSE
        VText.MoveInterval (rec.interval, rec.anchor.l, position)
      END;
      VBT.Mark (m.v)
    EXCEPT
    | VTDef.Error (ec) => m.v.vterror (name, ec)
    | Rd.EndOfFile => m.v.rdeoferror (name)
    | Rd.Failure (ref) => m.v.rdfailure (name, ref)
    | Thread.Alerted =>
    END
  END Seek;

PROCEDURE Kill (m: T; v: TextPort.T; READONLY cd: VBT.KeyRec) =
  (* Delete to end of line, but also make the deleted text be the source
     selection. *)
  PROCEDURE clip (t: TEXT) =
    BEGIN
      IF m.append THEN
        m.clipboard := m.clipboard & t
      ELSE
        m.clipboard := t
      END
    END clip;
  VAR
    here := v.index ();
    info := MTextUnit.LineInfo (v.vtext.mtext, here);
  BEGIN
    IF NOT m.takeSelection (VBT.Source, Primary, cd.time) THEN (* skip *)
    ELSIF here = info.rightEnd THEN
      (* We're already at the end of line. *)
      clip (v.getText (here, info.right));
      EVAL v.replace (here, info.right, "")
    ELSE
      clip (v.getText (here, info.rightEnd));
      EVAL v.replace (here, info.rightEnd, "")
    END
  END Kill;

PROCEDURE OptionChord (m: T; ch: CHAR; READONLY cd: VBT.KeyRec) =
  CONST name = "Option Key";
  VAR
    ext: TextPort.Extent;
    v                    := m.v;
  BEGIN
    TRY
      CASE ISOChar.Upper [ch] OF
      | '_' => TextPortClass.Redo (v)
      | '<' => m.seek (0)
      | '>' => m.seek (LAST (CARDINAL))
      | 'B' =>
          ext := TextPortClass.FindPrevWord (v);
          IF ext # TextPort.NotFound THEN m.seek (ext.l) END
      | 'D' => EVAL TextPortClass.DeleteToEndOfWord (v)
      | 'F' =>
          ext := TextPortClass.FindNextWord (v);
          IF ext # TextPort.NotFound THEN m.seek (ext.r) END
      | 'H', ASCII.BS, ASCII.DEL => EVAL TextPortClass.DeleteToStartOfWord (v)
      | 'V' => TextPortClass.ScrollOneScreenDown (v); RETURN
      | 'W' => m.copy (cd.time)
      | 'Z' => TextPortClass.ScrollOneLineDown (v); RETURN
      ELSE
        IF cd.whatChanged = KeyboardKey.Left THEN
          OptionChord (m, 'b', cd)
        ELSIF cd.whatChanged = KeyboardKey.Right THEN
          OptionChord (m, 'f', cd)
        ELSE
          (* Don't normalize if unknown chord, including just option
             itself. *)
        END;
        RETURN
      END
    EXCEPT
    | VTDef.Error (ec) => m.v.vterror (name, ec)
    | Rd.Failure (ref) => m.v.rdfailure (name, ref)
    | Rd.EndOfFile => m.v.rdeoferror (name)
    | Thread.Alerted => RETURN
    END;
    m.v.normalize (-1)
  END OptionChord;

PROCEDURE Mouse (m: T; READONLY cd: VBT.MouseRec) =
  VAR
    rec := m.selection [Primary];
    r   := TextPortClass.GetRange (m.v, cd.cp, rec.mode);
  BEGIN
    IF NOT m.v.getKFocus (cd.time) THEN RETURN END;
    IF m.mark = -1 THEN SetMark (m, r.middle) END;
    CASE cd.clickType OF
    | VBT.ClickType.FirstDown =>
        CASE cd.whatChanged OF
        | VBT.Modifier.MouseL => (* Set point *)
            (* Needed in case we start dragging: *)
            m.downclick := r.middle;
            (* Cancel replace-mode and highlighting. *)
            m.lit := FALSE;
            rec.replaceMode := FALSE;
            TRY
              TextPortClass.ChangeIntervalOptions (m.v, rec)
            EXCEPT
            | VTDef.Error (ec) => m.v.vterror ("Mouse", ec)
            END;
            IF cd.clickCount DIV 2 = 1 THEN
              (* double-click => set mark *)
              SetMark (m, r.middle)
            ELSE                 (* Left-click redefines anchor *)
              rec.anchor.l := m.mark;
              rec.anchor.r := m.mark;
              IF r.middle < m.mark THEN
                m.highlight (rec, IRange {r.middle, r.middle, m.mark})
              ELSE
                m.highlight (rec, IRange {m.mark, r.middle, r.middle})
              END
            END;
            m.dragging := TRUE
        | VBT.Modifier.MouseM => m.copy (cd.time)
        | VBT.Modifier.MouseR =>
            m.approachingFromLeft :=
              r.left < (rec.anchor.l + rec.anchor.r) DIV 2;
            m.dragging := TRUE;
            m.lit := TRUE;
            m.extend (rec, r.left, r.right)
        ELSE
          m.dragging := FALSE
        END
    | VBT.ClickType.LastUp =>
        IF m.dragging THEN
          rec.anchor.l := rec.interval.left ();
          rec.anchor.r := rec.interval.right ();
          m.dragging := FALSE
        END
    ELSE
      m.dragging := FALSE
    END                          (* CASE *)
  END Mouse;

PROCEDURE Position (m: T; READONLY cd: VBT.PositionRec) =
  BEGIN
    IF m.mark # m.downclick THEN SetMark (m, m.downclick) END;
    m.lit := TRUE;
    TextPortClass.Model.position (m, cd)
  END Position;

(***********************  Reading  ****************************)

PROCEDURE Read (m: T; READONLY s: VBT.Selection; time: VBT.TimeStamp): TEXT
  RAISES {VBT.Error} =
  BEGIN
    IF s = VBT.Source AND m.v.owns [Source] THEN
      RETURN m.clipboard
    ELSE
      RETURN TextPortClass.Model.read (m, s, time)
    END
  END Read;

(***********************  Writing  ****************************)

PROCEDURE Write (m: T; READONLY s: VBT.Selection; time: VBT.TimeStamp; t: TEXT)
  RAISES {VBT.Error} =
  BEGIN
    IF s = VBT.Source AND m.v.owns [Source] THEN
      m.clipboard := t
    ELSE
      TextPortClass.Model.write (m, s, time, t)
    END
  END Write;

(***************** Other things *************************)

PROCEDURE Copy (m: T; time: VBT.TimeStamp) =
  VAR t := m.getSelectedText (Primary);
  BEGIN
    IF NOT Text.Empty (t) AND m.takeSelection (VBT.Source, Primary, time) THEN
      m.clipboard := t
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

PROCEDURE ApplyEMFilter (self: EscapeMetaFilter; v: VBT.T; cd: VBT.KeyRec) =
  VAR c := cd.whatChanged;
  BEGIN
    IF self.sawEscape THEN
      IF KeyFilter.IsModifier (c) THEN (* skip *)
      ELSE
        cd.modifiers := cd.modifiers + VBT.Modifiers {VBT.Modifier.Option};
        self.sawEscape := FALSE;
        self.next.apply (v, cd)
      END
    ELSIF c = KeyboardKey.Escape OR VBT.Modifier.Control IN cd.modifiers
                                      AND c = Latin1Key.bracketleft THEN
      self.sawEscape := TRUE
    ELSE
      self.next.apply (v, cd)
    END
  END ApplyEMFilter;

PROCEDURE ApplyKQFilter (self: KQFilter; v: VBT.T; cd: VBT.KeyRec) =
  VAR
    tp     : TextPort.T := v;
    m      : T          := tp.m;
    c                   := cd.whatChanged;
    k                   := c = Latin1Key.K OR c = Latin1Key.k;
    q                   := c = Latin1Key.Q OR c = Latin1Key.q;
    control             := VBT.Modifier.Control IN cd.modifiers;
    cK                  := control AND k;
    cQ                  := control AND q;
  BEGIN
    m.append := FALSE;
    CASE self.state OF
    | State.Initial =>
        IF cK THEN
          self.state := State.SawControlK;
          self.next.apply (v, cd)
        ELSIF cQ THEN
          self.state := State.SawControlQ
        ELSE
          self.next.apply (v, cd)
        END
    | State.SawControlK =>
        IF cK THEN
          m.append := TRUE;
          self.next.apply (v, cd)
        ELSIF cQ THEN
          self.state := State.SawControlQ
        ELSIF KeyFilter.IsModifier (c) THEN (* ignore *)
        ELSE
          self.state := State.Initial;
          self.next.apply (v, cd)
        END
    | State.SawControlQ =>
        IF NOT KeyFilter.IsModifier (c) THEN
          TextPort.Insert (tp, Text.FromChar (KeyTrans.TTY (cd)));
          self.state := State.Initial
        END
    END
  END ApplyKQFilter;

BEGIN
END EmacsModel.
