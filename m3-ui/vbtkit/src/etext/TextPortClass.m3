(* Copyright (C) 1991-1992, Digital Equipment Corporation                *)
(* All rights reserved.                                                  *)
(* See the file COPYRIGHT for a full description.                        *)
(*                                                                       *)
(* Last modified on Mon Nov  4 13:11:02 PST 1996 by najork               *)
(*      modified on Mon Jan 30 14:34:33 PST 1995 by kalsow               *)
(*      modified on Wed Sep 15 09:51:02 PDT 1993 by mhb                  *)
(*      modified on Sun May 30 10:12:41 PDT 1993 by meehan               *)
<* PRAGMA LL *>

MODULE TextPortClass;

IMPORT Cursor, Fmt, ISOChar, KeyboardKey, KeyFilter, MText, MTextRd,
       MTextUnit, PaintOp, Rd, RdUtils, Rect, Stdio, Text, TextPort, Thread,
       TypescriptVBT, VBT, VTDef, VText, Wr;

FROM TextPort IMPORT Extent, NotFound;

REVEAL
  Model = PublicModel BRANDED OBJECT
          OVERRIDES
            init            := Init;
            close           := Close;
            arrowKey        := ArrowKey;
            clear           := Clear;
            cut             := Cut;
            extend          := Extend;
            getSelectedText := GetSelectedText;
            getSelection    := GetSelection;
            highlight       := Highlight;
            misc            := Misc;
            paste           := Paste;
            position        := Position;
            putSelectedText := PutSelectedText;
            read            := Read;
            seek            := Seek;
            select          := Select;
            takeSelection   := TakeSelection;
            write           := Write;
          END;

PROCEDURE Init (             m        : Model;
                <* UNUSED *> cs       : PaintOp.ColorScheme;
                             keyfilter: KeyFilter.T          ): Model =
  BEGIN
    m.keyfilter := keyfilter;
    RETURN m
  END Init;

PROCEDURE Close (m: Model) =
  CONST name = "Close";
  VAR v := m.v;
  BEGIN
    VBT.Release (v, VBT.KBFocus);
    VBT.Release (v, VBT.Source);
    VBT.Release (v, VBT.Target);
    TRY
      VText.SwitchCaret (v.vtext, VText.OnOffState.Off);
      FOR t := Primary TO Secondary DO
        IF m.selection [t] # NIL THEN
          VText.DeleteInterval (m.selection [t].interval)
        END
      END
    EXCEPT
    | VTDef.Error (ec) => v.vterror (name, ec)
    | Rd.EndOfFile => v.rdeoferror (name)
    | Rd.Failure (ref) => v.rdfailure (name, ref)
    | Thread.Alerted =>
    END
  END Close;

PROCEDURE Cut (m: Model; time: VBT.TimeStamp) =
  BEGIN
    m.copy (time);
    m.clear ()
  END Cut;

PROCEDURE Clear (m: Model) =
  BEGIN
    m.putSelectedText ("", Primary)
  END Clear;

(*
 * Caret and interval-twiddling
 *)

PROCEDURE ArrowKey (m: Model; READONLY cd: VBT.KeyRec) =
  BEGIN
    CASE cd.whatChanged OF
    | KeyboardKey.Left => ToPrevChar (m.v)
    | KeyboardKey.Right => ToNextChar (m.v)
    | KeyboardKey.Up => UpOneLine (m.v)
    | KeyboardKey.Down => DownOneLine (m.v)
    ELSE <* ASSERT FALSE *>
    END
  END ArrowKey;

PROCEDURE FindNextWord (v: T): Extent =
  VAR
    right := LocateNextWordBoundary (v);
    left  := MTextUnit.StartOfRun (v.vtext.mtext, right);
  BEGIN
    IF left >= 0 THEN RETURN Extent {left, right} ELSE RETURN NotFound END
  END FindNextWord;

PROCEDURE FindPrevWord (v: T): Extent =
  VAR
    left  := LocateNextWordBoundary (v, reverse := TRUE);
    right := MTextUnit.EndOfRun (v.vtext.mtext, left);
  BEGIN
    IF right >= 0 THEN RETURN Extent {left, right} ELSE RETURN NotFound END
  END FindPrevWord;

VAR
  readerLock := NEW (MUTEX);
  reader     := NEW (MTextRd.T); <* LL = readerLock *>

PROCEDURE LocateNextWordBoundary (v: T; reverse := FALSE): CARDINAL =
  VAR
    index       := v.index ();
    rd   : Rd.T;
    c    : CHAR;
    count       := 0;
  BEGIN
    LOCK readerLock DO
      TRY
        rd := reader.init (v.vtext.mtext, index, reverse := reverse);
        REPEAT
          c := Rd.GetChar (rd);
          INC (count);
        UNTIL c IN ISOChar.AlphaNumerics;
        REPEAT
          c := Rd.GetChar (rd);
          INC (count);
        UNTIL NOT c IN ISOChar.AlphaNumerics;
        DEC (count)
      EXCEPT
        Rd.EndOfFile, Rd.Failure, Thread.Alerted =>
      END
    END;
    IF reverse THEN RETURN index - count ELSE RETURN index + count END
  END LocateNextWordBoundary;

PROCEDURE ToPrevChar (v: T) =
  VAR index := v.index ();
  BEGIN
    IF index > 0 THEN v.m.seek (index - 1) END
  END ToPrevChar;

PROCEDURE ToNextChar (v: T) =
  BEGIN
    v.m.seek (v.index () + 1)
  END ToNextChar;

PROCEDURE ToStartOfLine (v: T) =
  BEGIN
    v.m.seek (MTextUnit.LineInfo (v.vtext.mtext, v.index ()).left)
  END ToStartOfLine;

PROCEDURE ToEndOfLine (v: T) =
  BEGIN
    v.m.seek (MTextUnit.LineInfo (v.vtext.mtext, v.index ()).rightEnd)
  END ToEndOfLine;

PROCEDURE ToOtherEnd (v: T) =
  VAR x := v.m.getSelection ();
  BEGIN
    IF v.index () = x.l THEN v.m.seek (x.r) ELSE v.m.seek (x.l) END
  END ToOtherEnd;


(*
 * Vertical movement commands.
 *)

PROCEDURE UpOneLine (v: T) =
  BEGIN
    GoUpDown (v, goUp := TRUE)
  END UpOneLine;

PROCEDURE DownOneLine (v: T) =
  BEGIN
    GoUpDown (v, goUp := FALSE)
  END DownOneLine;

PROCEDURE GoUpDown (v: T; goUp: BOOLEAN) =
  VAR
    mtext                   := v.vtext.mtext;
    e    : MTextUnit.Extent := MTextUnit.LineExtent (mtext, v.index ());
  BEGIN
    (* Vertical movement commands *)
    IF v.lastCmdKind # CommandKind.VertCommand THEN
      v.wishCol := v.index () - e.left
    END;
    v.thisCmdKind := CommandKind.VertCommand;
    IF goUp THEN
      IF e.left = 0 THEN RETURN END;
      e := MTextUnit.LineExtent (mtext, e.left - 1)
    ELSE
      e.left := e.right
    END;
    v.m.seek (
      MIN (e.left + v.wishCol, MTextUnit.LineInfo (mtext, e.left).rightEnd))
  END GoUpDown;

(*
 * Deletion commands.
 *)

PROCEDURE DeletePrevChar (v: T): Extent =
  VAR here := v.index ();
  BEGIN
    IF here > 0 THEN
      RETURN v.replace (here - 1, here, "")
    ELSE
      RETURN NotFound
    END
  END DeletePrevChar;

PROCEDURE DeleteNextChar (v: T): Extent =
  VAR here := v.index ();
  BEGIN
    RETURN v.replace (here, here + 1, "")
  END DeleteNextChar;

PROCEDURE DeleteToEndOfWord (v: T): Extent =
  VAR
    start := v.index ();
    end   := LocateNextWordBoundary (v);
  BEGIN
    RETURN v.replace (start, end, "")
  END DeleteToEndOfWord;

PROCEDURE DeleteToStartOfWord (v: T): Extent =
  VAR
    end   := v.index ();
    start := LocateNextWordBoundary (v, reverse := TRUE);
  BEGIN
    RETURN v.replace (start, end, "")
  END DeleteToStartOfWord;

PROCEDURE DeleteCurrentWord (v: T): Extent =
  PROCEDURE WordAt (mtext: MText.T; index: CARDINAL): Extent =
    (** A word is
        - a run of alphanumerics
        - a run of blanks
        - any other single character
       We find a word such that left <= index < right.
    **)
    VAR e: MTextUnit.Extent;
    BEGIN
      e := MTextUnit.RunExtent (mtext, index, ISOChar.AlphaNumerics);
      IF e.inside THEN
        RETURN Extent {e.left, e.right}
      ELSE
        e := MTextUnit.RunExtent (mtext, index, ISOChar.Spaces);
        IF e.inside THEN
          RETURN Extent {e.left, e.right}
        ELSE
          RETURN Extent {index, index + 1}
        END
      END
    END WordAt;
  VAR extent := WordAt (v.vtext.mtext, v.index ());
  BEGIN
    RETURN v.replace (extent.l, extent.r, "")
  END DeleteCurrentWord;

PROCEDURE DeleteToStartOfLine (v: T): Extent =
  VAR
    here := v.index ();
    left := MTextUnit.StartOfLine (v.vtext.mtext, here);
  BEGIN
    IF here = left THEN
      (* We're already at the start of line; delete one char. *)
      RETURN v.replace (here - 1, here, "")
    ELSE
      RETURN v.replace (left, here, "")
    END
  END DeleteToStartOfLine;

PROCEDURE DeleteToEndOfLine (v: T): Extent =
  VAR
    here := v.index ();
    info := MTextUnit.LineInfo (v.vtext.mtext, here);
  BEGIN
    IF here = info.rightEnd THEN
      (* We're already at the end of line. *)
      RETURN v.replace (here, info.right, "")
    ELSE
      RETURN v.replace (here, info.rightEnd, "")
    END
  END DeleteToEndOfLine;

PROCEDURE DeleteCurrentLine (v: T): Extent =
  VAR
    here := v.index ();
    info := MTextUnit.LineInfo (v.vtext.mtext, here);
  BEGIN
    RETURN v.replace (info.left, info.right, "")
  END DeleteCurrentLine;

(*
 * Other modifications.
 *)

PROCEDURE SwapChars (v: T) =
  (* Swap the two characters to the left of the caret. *)
  VAR
    here                         := v.index ();
    two : ARRAY [0 .. 1] OF CHAR;
  BEGIN
    IF here - 2 < v.typeinStart THEN RETURN END;
    two [1] := MText.GetChar (v.vtext.mtext, here - 2);
    two [0] := MText.GetChar (v.vtext.mtext, here - 1);
    EVAL v.replace (here - 2, here, Text.FromChars (two))
  END SwapChars;

PROCEDURE InsertNewline (v: T) =
  (* Insert a newline without moving the cursor. *)
  VAR here := v.index ();
  BEGIN
    v.m.seek (v.replace (here, here, Wr.EOL).l)
  END InsertNewline;

(*
 * Searching
 *)

PROCEDURE Find (v: T; pattern: TEXT; loc := Loc.Next; ignoreCase := TRUE):
  Extent =
  CONST name = "Find";
  VAR
    len                         := Text.Length (pattern);
    found: INTEGER;
    start                       := v.index ();
    can  : RdUtils.Canonicalize := NIL;
  BEGIN
    IF len = 0 THEN RETURN NotFound END;
    IF ignoreCase THEN can := ToUpperCaseISO END;
    TRY
      CASE loc OF
      | Loc.First, Loc.Next =>
          IF loc = Loc.First THEN start := 0 END;
          LOCK readerLock DO
            EVAL reader.init (v.vtext.mtext, start := start);
            found := RdUtils.Find (reader, pattern, can);
            IF found >= 0 THEN RETURN Extent {found, found + len} END
          END
      | Loc.Prev =>
          LOCK readerLock DO
            EVAL reader.init (v.vtext.mtext, start := start, rangeStart := 0,
                              rangeEnd := start, reverse := TRUE);
            found := RdUtils.Find (reader, TextReverse (pattern), can);
            IF found >= 0 THEN
              RETURN Extent {start - found - len, start - found}
            END                  (* IF *)
          END                    (* LOCK *)
      END                        (* CASE *)
    EXCEPT
    | Rd.Failure (ref) => v.rdfailure (name, ref)
    | Thread.Alerted =>
    END;
    RETURN NotFound
  END Find;

PROCEDURE ToUpperCaseISO (ch: CHAR): CHAR =
  BEGIN
    RETURN ISOChar.Upper [ch]
  END ToUpperCaseISO;

PROCEDURE FindAndSelect (v      : T;
                         pattern: TEXT;
                         time   : VBT.TimeStamp;
                         loc        := Loc.Next;
                         ignoreCase := TRUE      ) =
  CONST
    map = ARRAY Loc OF
            VText.WhichEnd {
            VText.WhichEnd.Right, VText.WhichEnd.Right,
            VText.WhichEnd.Left};
  VAR ext := Find (v, pattern, loc, ignoreCase);
  BEGIN
    IF ext = TextPort.NotFound THEN
      v.notFound ()
    ELSE
      v.m.select (time, ext.l, ext.r, replaceMode := TRUE,
                  caretEnd := map [loc]);
      v.normalize (ext.l)
    END
  END FindAndSelect;

PROCEDURE TextReverse (t: TEXT): TEXT =
  VAR
    buf       := NEW (REF ARRAY OF CHAR, Text.Length (t));
    i         := FIRST (buf^);
    j         := LAST (buf^);
    c  : CHAR;
  BEGIN
    Text.SetChars (buf^, t);
    WHILE i < j DO
      c := buf [i];
      buf [i] := buf [j];
      buf [j] := c;
      INC (i);
      DEC (j)
    END;
    RETURN Text.FromChars (buf^)
  END TextReverse;

PROCEDURE TextLowerCase (t: TEXT): TEXT =
  VAR buf := NEW (REF ARRAY OF CHAR, Text.Length (t));
  BEGIN
    Text.SetChars (buf^, t);
    FOR i := FIRST (buf^) TO LAST (buf^) DO
      buf [i] := ISOChar.Lower [buf [i]]
    END;
    RETURN Text.FromChars (buf^)
  END TextLowerCase;

PROCEDURE GetRange (         v   : T;
                    READONLY cp  : VBT.CursorPosition;
                             mode: VText.SelectionMode ): IRange =
  <* LL = v.mu *>
  CONST name = "GetRange";
  VAR
    whichEnd  : VText.WhichEnd;
    rect      : Rect.T;
    lineNum   : CARDINAL;
    ch        : CHAR;
    atEnd     : BOOLEAN;
    lt, md, rt: CARDINAL;
    e         : MTextUnit.Extent;
  VAR vt := v.vtext;
  BEGIN
    TRY
      VText.PounceLocate (vt, 0, cp.pt, lt, rt, lineNum, ch);
      atEnd := lt = rt;
      IF atEnd AND lt > 0 THEN DEC (lt) END;
      CASE mode OF
      | VText.SelectionMode.ParagraphSelection =>
          (* paragraph strategy differs from VText's strategy *)
          e := MTextUnit.ParagraphExtent (vt.mtext, lt);
          lt := e.left;
          rt := e.right
      | VText.SelectionMode.LineSelection =>
          e := MTextUnit.LineExtent (vt.mtext, lt);
          lt := e.left;
          rt := e.right
      ELSE
        VText.PounceExtend (vt, 0, lt, rt, lineNum, ch, mode)
      END;
      whichEnd := VText.PounceEncage (vt, 0, cp.pt, lt, md, rt, rect);
      VBT.SetCage (vt.vbt, VBT.CageFromRect (rect, cp));
      IF    (   mode = VText.SelectionMode.CharSelection
             OR mode = VText.SelectionMode.WordSelection)
         AND ch # '\n'
         AND (whichEnd = VText.WhichEnd.Right OR atEnd) THEN
        md := rt
      ELSE
        md := lt
      END
    EXCEPT
    | VTDef.Error (ec) => v.vterror (name, ec)
    | Rd.EndOfFile => v.rdeoferror (name)
    | Rd.Failure (ref) => v.rdfailure (name, ref)
    | Thread.Alerted =>
    END;
    RETURN IRange {lt, md, rt}
  END GetRange;


(**************** Scrolling the display ********************)

PROCEDURE Scroll (v: T; delta: INTEGER)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR
    vt := v.vtext;
    p  := VText.CaretIndex (vt);
  BEGIN
    VText.Scroll (vt, 0, delta);
    IF VText.InRegion (vt, 0, p) THEN (* skip *)
    ELSIF delta < 0 THEN
      VText.MoveCaret (vt, VText.UpLines (vt, p, -delta, 0))
    ELSE
      VText.MoveCaret (vt, VText.StartIndex (vt, 0))
    END;
    VBT.Mark (v)
  END Scroll;

PROCEDURE ScrollOneLineUp (v: T)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    Scroll (v, 1)
  END ScrollOneLineUp;

PROCEDURE ScrollOneLineDown (v: T)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    Scroll (v, -1)
  END ScrollOneLineDown;

PROCEDURE ScrollOneScreenUp (v: T)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    Scroll (v, MAX (1, v.vtext.region [0].nLines - 2))
  END ScrollOneScreenUp;

PROCEDURE ScrollOneScreenDown (v: T)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    Scroll (v, -MAX (1, v.vtext.region [0].nLines - 2))
  END ScrollOneScreenDown;

(***************************** Undo  ********************************)

REVEAL
  UndoRec = BRANDED OBJECT
                          begin, end: VText.Index := 0;
                          text                    := "";
                          next, prev: UndoRec     := NIL
                        END;

PROCEDURE AddToUndo (v: T; begin, end: CARDINAL; newText: TEXT) =
  <* LL = v.mu *>
  VAR
    n := Text.Length (newText);
    r := v.cur;
    vv: VBT.T := v; (* ISTYPE demands that v be assignable to TypescriptVBT.T*)
  BEGIN
    IF v.readOnly OR begin = end AND n = 0 OR ISTYPE (vv, TypescriptVBT.T) THEN
      RETURN
    END;
    IF r.prev # NIL AND begin = end AND n = 1 AND r.prev.end = begin
         AND Text.GetChar (newText, 0) IN ISOChar.Graphics THEN
      (* It's straight typing.  Extend the previous record. *)
      INC (r.prev.end)
    ELSE
      r.begin := begin;
      r.end := begin + n;
      r.text := MText.GetText (v.vtext.mtext, begin, end);
      IF r.next = NIL THEN r.next := NEW (UndoRec, prev := r) END;
      v.cur := r.next
    END;
    TraceUndo (v)
  END AddToUndo;

VAR tracingUndo := FALSE; (* For runtime debugging *)

PROCEDURE TraceUndo (v: T) =
  <* LL = v.mu *>
  <* FATAL Wr.Failure, Thread.Alerted *>
  VAR
    r          := v.cur;
    t: TEXT;
    n: INTEGER := 0;
  BEGIN
    IF NOT tracingUndo THEN RETURN END;
    WHILE r.prev # NIL DO r := r.prev; INC (n) END;
    WHILE r.next # NIL DO
      t := r.text;
      IF Text.Length (t) > 20 THEN t := Text.Sub (t, 0, 20) & "..." END;
      IF n = 0 THEN Wr.PutText (Stdio.stderr, "***** ") END;
      Wr.PutText (Stdio.stderr,
                       Fmt.F ("[%s .. %s] = \"%s\"\n", Fmt.Int (r.begin),
                              Fmt.Int (r.end), t));
      r := r.next;
      DEC (n)
    END;
    Wr.PutText (Stdio.stderr, "-------------------\n")
  END TraceUndo;
  
PROCEDURE Undo (v: T) =
  BEGIN
    IF v.cur.prev # NIL THEN v.cur := v.cur.prev; Exchange (v) END
  END Undo;

PROCEDURE Redo (v: T) =
  BEGIN
    IF v.cur.next # NIL THEN Exchange (v); v.cur := v.cur.next END
  END Redo; 

PROCEDURE UndoCount (v: T): CARDINAL =
  <* LL < v.mu *>
  VAR
    n: CARDINAL := 0;
    r: UndoRec;
  BEGIN
    LOCK v.mu DO
      r := v.cur;
      WHILE r.prev # NIL DO INC (n); r := r.prev END;
      RETURN n
    END
  END UndoCount;

PROCEDURE RedoCount (v: T): CARDINAL =
  <* LL < v.mu *>
  VAR
    n: CARDINAL := 0;
    r: UndoRec;
  BEGIN
    LOCK v.mu DO
      r := v.cur;
      WHILE r.next # NIL DO INC(n); r := r.next END;
      RETURN n
    END
  END RedoCount;

PROCEDURE ResetUndo (v: T) =
  <* LL < v.mu *>
  BEGIN
    LOCK v.mu DO v.cur := NEW(UndoRec) END
  END ResetUndo;

PROCEDURE Exchange (v: T) =
  <* LL = v.mu *>
  CONST name = "Undo";
  VAR
    prev := "";
    r    := v.cur;
  BEGIN
    IF r.begin < r.end AND r.begin < v.length () THEN
      prev := v.getText (r.begin, r.end)
    END;
    v.normalize (r.begin);
    TRY
      VText.Replace (v.vtext, r.begin, r.end, r.text)
    EXCEPT
    | VTDef.Error (ec) => v.vterror (name, ec)
    | Rd.EndOfFile => v.rdeoferror (name)
    | Rd.Failure (ref) => v.rdfailure (name, ref)
    | Thread.Alerted =>
    END;
    r.end := r.begin + Text.Length (r.text);
    r.text := prev;
    TraceUndo (v)
  END Exchange;

(********************* Default methods *****************************)

PROCEDURE Misc (m: Model; READONLY cd: VBT.MiscRec) =
  CONST name = "Misc";
  VAR v := m.v;
  PROCEDURE turnOff (vtype: VType)
    RAISES {VTDef.Error} =
    BEGIN
      IF NOT v.owns [vtype] THEN RETURN END;
      v.owns [vtype] := FALSE;
      FOR type := Primary TO Secondary DO
        VAR rec := m.selection [type];
        BEGIN
          IF rec # NIL AND rec.alias = cd.selection THEN
            VText.SwitchInterval (rec.interval, VText.OnOffState.Off)
          END
        END
      END
    END turnOff;
  BEGIN
    TRY
      IF cd.type = VBT.Lost THEN
        IF cd.selection = VBT.KBFocus AND v.owns [Focus] THEN
          v.owns [Focus] := FALSE;
          VText.SwitchCaret (v.vtext, VText.OnOffState.Off);
          v.ULfocus (FALSE, cd.time)
        ELSIF cd.selection = VBT.Source THEN
          turnOff (Source)
        ELSIF cd.selection = VBT.Target THEN
          turnOff (Target)
        END
      ELSIF cd.type = VBT.TakeSelection AND cd.selection = VBT.KBFocus THEN
        EVAL v.getKFocus (cd.time)
      END;
      VBT.Mark (v)
    EXCEPT
    | VTDef.Error (ec) => v.vterror (name, ec)
    | Rd.EndOfFile => v.rdeoferror (name)
    | Rd.Failure (ref) => v.rdfailure (name, ref)
    | Thread.Alerted =>
    END
  END Misc;

PROCEDURE GetSelectedText (m: Model; sel: TextPort.SelectionType): TEXT =
  VAR extent := m.getSelection (sel);
  BEGIN
    IF extent.l = extent.r THEN
      RETURN ""
    ELSE
      RETURN m.v.getText (extent.l, extent.r)
    END
  END GetSelectedText;

PROCEDURE Paste (m: Model; time: VBT.TimeStamp) =
  BEGIN
    TRY
      m.v.insert (m.read (VBT.Source, time))
    EXCEPT
    | VBT.Error (ec) => m.v.vbterror ("Paste", ec)
    END
  END Paste;

PROCEDURE Read (m: Model; READONLY s: VBT.Selection; time: VBT.TimeStamp): TEXT
  RAISES {VBT.Error} =
  BEGIN
    TYPECASE VBT.Read (m.v, s, time).toRef () OF
    | NULL => RAISE VBT.Error (VBT.ErrorCode.WrongType)
    | TEXT (t) => RETURN t
    ELSE
      RAISE VBT.Error (VBT.ErrorCode.WrongType)
    END
  END Read;

PROCEDURE Write (         m   : Model;
                 READONLY s   : VBT.Selection;
                          time: VBT.TimeStamp;
                          t   : TEXT           ) RAISES {VBT.Error} =
  BEGIN
    VBT.Write (m.v, s, time, VBT.FromRef (t))
  END Write; 
  
PROCEDURE Seek (m: Model; position: CARDINAL) =
  CONST name = "Seek";
  BEGIN
    TRY
      VText.MoveCaret (m.v.vtext, position);
      VBT.Mark (m.v)
    EXCEPT
    | VTDef.Error (ec) => m.v.vterror (name, ec)
    | Rd.EndOfFile => m.v.rdeoferror (name)
    | Rd.Failure (ref) => m.v.rdfailure (name, ref)
    | Thread.Alerted =>
    END
  END Seek;

PROCEDURE ChangeIntervalOptions (v: T; rec: SelectionRecord)
  RAISES {VTDef.Error} =
  VAR
    st          := VBT.ScreenTypeOf (v);
    interval    := rec.interval;
    options     := interval.getOptions ();
    replaceMode := rec.type = Primary AND v.isReplaceMode ();
  BEGIN
    IF st = NIL THEN RETURN END;
    options.whiteStroke := PaintOp.bgFg; (* Reset. *)
    options.whiteBlack := PaintOp.bgFg;
    IF st.depth <= 1 THEN        (* monochrome *)
      IF replaceMode THEN
        options.style := VTDef.IntervalStyle.InverseStyle
      ELSIF rec.alias = VBT.Source THEN
        options.style := VTDef.IntervalStyle.ThinUnderlineStyle
      ELSE
        options.style := VTDef.IntervalStyle.UnderlineStyle
      END
    ELSIF replaceMode THEN
      options.style := VTDef.IntervalStyle.HighlightStyle;
      options.whiteBlack := ReplaceColorScheme
    ELSIF rec.alias = VBT.Source THEN
      options.style := VTDef.IntervalStyle.ThinUnderlineStyle;
      options.whiteStroke := SourceColorScheme
    ELSE
      options.style := VTDef.IntervalStyle.UnderlineStyle;
      IF v.readOnly THEN
        options.whiteStroke := ReadOnlyColorScheme
      ELSE
        options.whiteStroke := WritableColorScheme
      END
    END;
    VText.ChangeIntervalOptions (interval, options);
    VBT.Mark (v)
  END ChangeIntervalOptions;

VAR
  ReplaceColorScheme :=          (* black letters on a pale red background *)
  PaintOp.MakeColorScheme (
    bg := PaintOp.FromRGB (r := 1.0, g := 0.7, b := 0.7), fg := PaintOp.Fg);

  (* For underlines, only the .fg field is used. *)

  SourceColorScheme :=           (* green underline *)
  PaintOp.MakeColorScheme (
    fg := PaintOp.FromRGB (r := 0.0, g := 0.8, b := 0.0), bg := PaintOp.Bg);

  ReadOnlyColorScheme :=         (* blue underline *)
  PaintOp.MakeColorScheme (
    fg := PaintOp.FromRGB (r := 0.0, g := 0.0, b := 1.0), bg := PaintOp.Bg);

  WritableColorScheme :=         (* red underline *)
  PaintOp.MakeColorScheme (
    fg := PaintOp.FromRGB (r := 1.0, g := 0.0, b := 0.0), bg := PaintOp.Bg);

PROCEDURE Highlight (m: Model; rec: SelectionRecord; READONLY r: IRange) =
  CONST name = "Highlight";
  BEGIN
    TRY
      VText.MoveInterval (rec.interval, r.left, r.right);
      VText.SwitchInterval (rec.interval, VText.OnOffState.On);
      rec.cursor := r.middle;
      m.seek (r.middle);
      VBT.Mark (m.v)
    EXCEPT
    | VTDef.Error (ec) => m.v.vterror (name, ec)
    END
  END Highlight;
  
PROCEDURE TakeSelection (         m   : Model;
                         READONLY sel : VBT.Selection;
                                  type: TextPort.SelectionType;
                                  time: VBT.TimeStamp           ): BOOLEAN =
  CONST name = "TakeSelection";
  VAR
    v   := m.v;
    rec := m.selection [type];
  PROCEDURE take (vtype: VType): BOOLEAN =
    BEGIN
      IF NOT v.owns [vtype] THEN
        TRY
          VBT.Acquire (v, sel, time);
          IF type = Secondary OR v.getKFocus (time) THEN
            v.owns [vtype] := TRUE;
            IF rec.alias = sel THEN
              VText.SwitchInterval (rec.interval, VText.OnOffState.On)
            END
          ELSE
            VBT.Release (v, sel)
          END;
          VBT.Mark (v)
        EXCEPT
        | VBT.Error (ec) => v.vbterror (name, ec)
        | VTDef.Error (ec) => v.vterror (name, ec)
        END
      END;
      RETURN v.owns [vtype]
    END take;
  BEGIN
    IF sel = VBT.Source THEN
      RETURN take (Source)
    ELSIF sel = VBT.Target THEN
      RETURN take (Target)
    ELSE                         <* ASSERT FALSE *>
    END;
  END TakeSelection;

PROCEDURE Extend (m: Model; rec: SelectionRecord; newL, newR: CARDINAL) =
  BEGIN
    IF m.approachingFromLeft AND newL < rec.anchor.r
         OR NOT m.approachingFromLeft AND newR <= rec.anchor.l THEN
      m.highlight (rec, IRange {newL, newL, rec.anchor.r})
    ELSE
      m.highlight (rec, IRange {rec.anchor.l, newR, newR})
    END
  END Extend;

PROCEDURE Position (m: Model; READONLY cd: VBT.PositionRec) =
  VAR
    rec  := m.selection [m.dragType];
    r    := GetRange (m.v, cd.cp, rec.mode);
  BEGIN
    IF rec.mode = VText.SelectionMode.CharSelection THEN
      m.extend (rec, r.middle, r.middle)
    ELSE
      m.extend (rec, r.left, r.right)
    END
  END Position;

PROCEDURE GetSelection (m: Model; sel := Primary): TextPort.Extent =
  VAR rec := m.selection [sel];
  BEGIN
    IF rec = NIL THEN
      RETURN TextPort.NotFound
    ELSE
      RETURN TextPort.Extent {rec.interval.left (), rec.interval.right ()}
    END
  END GetSelection;

PROCEDURE Select (m          : Model;
                  time       : VBT.TimeStamp;
                  begin      : CARDINAL        := 0;
                  end        : CARDINAL        := LAST (CARDINAL);
                  type                         := Primary;
                  replaceMode                  := FALSE;
                  caretEnd                     := VText.WhichEnd.Right) =
  CONST name = "Select";
  VAR rec := m.selection [type];
  BEGIN
    IF rec = NIL THEN            (* skip *)
    ELSIF rec.alias = VBT.Source
            AND NOT m.takeSelection (VBT.Source, type, time) THEN (* skip *)
    ELSIF rec.alias = VBT.Target
            AND NOT m.takeSelection (VBT.Target, type, time) THEN (* skip *)
    ELSE
      IF type = Primary THEN
        rec.replaceMode := replaceMode AND NOT m.v.readOnly;
        TRY
          ChangeIntervalOptions (m.v, rec)
        EXCEPT
        | VTDef.Error (ec) => m.v.vterror (name, ec)
        END
      END;
      IF caretEnd = VText.WhichEnd.Left THEN
        m.highlight (rec, IRange {begin, begin, end})
      ELSE
        m.highlight (rec, IRange {begin, end, end})
      END
    END
  END Select;

PROCEDURE PutSelectedText (m: Model; t: TEXT; type: TextPort.SelectionType) =
  CONST name = "PutSelectedText";
  VAR
    rec := m.selection [type];
  BEGIN
    IF rec = NIL THEN RETURN END;
    VAR
      interval := rec.interval;
      left     := interval.left ();
      right    := interval.right ();
    BEGIN
      TRY
        IF m.v.replace (left, right, t) = TextPort.NotFound THEN RETURN END;
        (* NB: Replace changes interval! *)
        rec.replaceMode := FALSE;
        ChangeIntervalOptions (m.v, rec);
        VText.MoveInterval (interval, left, left + Text.Length (t))
      EXCEPT
      | VTDef.Error (ec) => m.v.vterror (name, ec)
      END
    END
  END PutSelectedText;

REVEAL
  Composer =
    KeyFilter.ComposeChar BRANDED OBJECT OVERRIDES feedback := Feedback END;

VAR
  cursors := ARRAY BOOLEAN OF
               Cursor.T {Cursor.TextPointer,
                         Cursor.FromName (ARRAY OF TEXT {"XC_exchange"})};
    
PROCEDURE Feedback (<* UNUSED *> c: Composer; v: VBT.T; composing: BOOLEAN) =
  BEGIN
    VBT.SetCursor (v, cursors [composing])
  END Feedback;

BEGIN END TextPortClass.
