(* Copyright (C) 1991-1992, Digital Equipment Corporation                    *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Nov  4 13:20:03 PST 1996 by najork                   *)
(*      modified on Thu Jun 20 16:29:45 PDT 1996 by heydon                   *)
(*      modified on Fri May 17 22:07:11 PDT 1996 by mhb                      *)
(*      modified on Mon Jan 30 14:32:07 PST 1995 by kalsow                   *)
(*      modified on Fri Jun 11 20:02:04 PDT 1993 by meehan                   *)
(*      modified on Thu Oct  2 12:48:00 1992 by nichols@parc.xerox.com       *)
(*      modified on Thu May 14 01:45:33 1992 by steveg                       *)
(*      modified on Thu Mar 14 12:16:11 PST 1991 by brooks                   *)
(*      modified on Thu Feb  7 14:03:27 PST 1991 by chan                     *)
<* PRAGMA LL *>
<* PRAGMA EXPORTED *>

MODULE TextPort;

IMPORT Axis, AnchorBtnVBT, Cursor, Env, Font, ISOChar, KeyboardKey,
       KeyFilter, KeyTrans, MacModel, MText, MTextUnit, PaintOp, Palette,
       Pts, Rd, RdUtils, Rect, Region, ScrollerVBTClass, Split, Stdio, Text,
       TextPortClass, Thread, VBT, VBTKitEnv, VBTRep, VTDef, VText, Wr;

FROM TextPortClass IMPORT IRange;

IMPORT EmacsModel, IvyModel, XtermModel;

CONST
  Backspace = '\010';
  Tab       = '\t';
  Return    = '\n';
  Del       = '\177';
  Primary   = SelectionType.Primary;
  Secondary = SelectionType.Secondary;
  Focus     = TextPortClass.VType.Focus;

REVEAL
  T = TextPortClass.T BRANDED OBJECT
        <* LL = v.mu *>
        modifiedP : BOOLEAN;
        linesShown: CARDINAL;
      OVERRIDES
        (* Exported methods *)
        init           := Init;
        filter         := Filter;
        getFont        := GetFont;
        setFont        := SetFont;
        getColorScheme := GetColorScheme;
        setColorScheme := SetColorScheme;
        getModel       := GetModel;
        setModel       := SetModel;
        getReadOnly    := GetReadOnly;
        setReadOnly    := SetReadOnly;
        getKFocus      := GetKFocus;

        (* Callbacks *)
        returnAction := ReturnAction;
        tabAction    := Insert4spaces;
        focus        := IgnoreFocus;
        modified     := IgnoreModification;
        error        := Error;

        (* VBT.T overrides *)
        key       := Key;
        misc      := Misc;
        mouse     := Mouse;
        position  := Position;
        read      := Read;
        redisplay := Redisplay;
        repaint   := Repaint;
        rescreen  := Rescreen;
        reshape   := Reshape;
        shape     := Shape;
        write     := Write;

        (* Exception handlers *)
        rdeoferror := rdeoferror;
        rdfailure  := rdfailure;
        vbterror   := vbterror;
        vterror    := vterror;

        (* Locked methods *)
        getText          := LockedGetText;
        index            := LockedIndex;
        isReplaceMode    := LockedIsReplaceMode;
        length           := LockedLength;
        normalize        := LockedNormalize;
        replace          := LockedReplace;
        unsafeReplace    := UnsafeReplace;
        insert           := LockedInsert;
        unsafeInsert     := UnsafeInsert;
        newlineAndIndent := LockedNewlineAndIndent;
        findSource       := FindSource;
        notFound         := NotFoundProc;

        (* Unlocked methods for callbacks *)
        ULreturnAction := UnlockedReturnAction;
        ULtabAction    := UnlockedTabAction;
        ULfocus        := UnlockedFocus;
        ULmodified     := UnlockedModified;
        ULerror        := UnlockedError;

      END;

VAR debug, UseDiacritical: BOOLEAN;

PROCEDURE Init (v               : T;
                hMargin, vMargin                      := 0.5;
                font                                  := Font.BuiltIn;
                colorScheme     : PaintOp.ColorScheme := NIL;
                wrap                                  := TRUE;
                readOnly                              := FALSE;
                turnMargin                            := 0.5;
                model                                 := Model.Default ): T =
  CONST PRINTABLE = (ISOChar.All - ISOChar.Controls) + SET OF CHAR {'\t'};
  VAR
    vFont   : VText.VFont;
    vOptions: VText.VOptions;
  BEGIN
    TRY
      IF colorScheme = NIL THEN colorScheme := PaintOp.bgFg END;
      vFont := VText.MakeVFont (
                 font := font, printable := PRINTABLE, whiteTabs := TRUE);
      vOptions := VText.MakeVOptions (
                    vFont := vFont, leftMargin := Pts.FromMM (hMargin),
                    rightMargin := Pts.FromMM (hMargin),
                    turnMargin := Pts.FromMM (turnMargin),
                    topMargin := Pts.FromMM (vMargin), leading := 0.0,
                    whiteBlack := colorScheme, whiteStroke := colorScheme,
                    leftOffset := 0.0, wrap := wrap,
                    eob := FALSE, intervalStylePrecedence := NIL);
      v.font := font;
      v.mu := NEW (MUTEX);
      v.vtext := VText.New (MText.New ("", 256), v, VBT.Domain (v), vOptions);
      v.readOnly := readOnly;
      v.modifiedP := FALSE;
      v.typeinStart := 0;
      v.cur := NEW (TextPortClass.UndoRec);
      LockedSetModel (v, model);
      VBT.SetCursor (v, Cursor.TextPointer);
      RETURN v
    EXCEPT
    | VTDef.Error (ec) => v.vterror ("Init", ec)
    | Rd.EndOfFile => v.rdeoferror ("Init")
    | Rd.Failure (ref) => v.rdfailure ("Init", ref)
    | Thread.Alerted =>
    END;
    RETURN NIL
  END Init;

(***************************  Client Interface  ***************************)

PROCEDURE GetReadOnly (v: T): BOOLEAN =
  BEGIN
    LOCK v.mu DO RETURN v.readOnly END
  END GetReadOnly;

PROCEDURE SetReadOnly (v: T; flag: BOOLEAN) =
  BEGIN
    LOCK v.mu DO v.readOnly := flag; FixIntervals (v) END
  END SetReadOnly;

(* UNUSED
PROCEDURE SetWrap (v: T; wrap: BOOLEAN) =
  BEGIN
    LOCK v.mu DO
      IF v.vtext.vOptions.wrap # wrap THEN
        v.vtext.vOptions.wrap := wrap;
        TRY
          VText.ChangeVOptions (v.vtext, v.vtext.vOptions);
          VBT.Mark (v)
        EXCEPT
        | VTDef.Error (ec) => v.vterror ("SetWrap", ec)
        | Rd.EndOfFile => v.rdeoferror ("SetWrap")
        | Rd.Failure (ref) => v.rdfailure ("SetWrap", ref)
        | Thread.Alerted =>
        END
      END
    END
  END SetWrap;
*)

<* EXPORTED *>
PROCEDURE Length (v: T): CARDINAL =
  BEGIN
    LOCK v.mu DO RETURN v.length () END
  END Length;

PROCEDURE LockedLength (v: T): CARDINAL =
  BEGIN
    RETURN MText.Length (v.vtext.mtext)
  END LockedLength;

<* EXPORTED *>
PROCEDURE GetText (v    : T;
                   begin: CARDINAL := 0;
                   end  : CARDINAL := LAST (CARDINAL)): TEXT =
  <* LL = VBT.mu *>
  BEGIN
    LOCK v.mu DO RETURN v.getText (begin, end) END
  END GetText;

PROCEDURE LockedGetText (v: T; begin, end: CARDINAL): TEXT =
  <* LL = v.mu *>
  BEGIN
    RETURN MText.GetText (v.vtext.mtext, begin, end)
  END LockedGetText;

<* EXPORTED *>
PROCEDURE SetText (v: T; t: TEXT) =
  <* LL <= VBT.mu *>
  BEGIN
    LOCK v.mu DO
      EVAL v.unsafeReplace (0, LAST (CARDINAL), t);
      TRY
        VText.SetStart (v.vtext, 0, 0);
        VBT.Mark (v)
      EXCEPT
      | VTDef.Error (ec) => v.vterror ("SetText", ec)
      | Rd.EndOfFile => v.rdeoferror ("SetText")
      | Rd.Failure (ref) => v.rdfailure ("SetText", ref)
      | Thread.Alerted =>
      END;
    END
  END SetText;

<* EXPORTED *>
PROCEDURE PutText (v: T; t: TEXT) =
  <* LL <= VBT.mu *>
  BEGIN
    LOCK v.mu DO
      EVAL v.unsafeReplace (LAST (CARDINAL), LAST (CARDINAL), t);
      VBT.Mark (v)
    END
  END PutText;

PROCEDURE GetFont (v: T): Font.T =
  BEGIN
    LOCK v.mu DO RETURN v.font END
  END GetFont;

PROCEDURE SetFont (v: T; font: Font.T) =
  (* By the book, we should call ExplodeVText, ExplodeVOptions, ExplodeVFont,
     MakeVFont, and MakeVOptions before calling ChangeVOptions, but we cheat
     by looking at the implementation and consing only a new VFont. *)
  VAR
    vtext   : VText.T;
    vOptions: VText.VOptions;
    vFont   : VText.VFont;
  BEGIN
    LOCK v.mu DO
      IF font = v.font THEN RETURN END;
      vtext := v.vtext;
      vOptions := vtext.vOptions;
      vFont := vOptions.vFontxxx;
      TRY
        vOptions.vFontxxx :=
          VText.MakeVFont (font := font, printable := vFont.vFont.printable,
                           whiteTabs := vFont.vFont.whiteTabs);
        v.font := font;          (* For convenience only *)
        VText.ChangeVOptions (vtext, vOptions);
        SetFontDimensions (v);
        VBT.NewShape (v);
        VBT.Mark (v)
      EXCEPT
      | VTDef.Error (ec) => v.vterror ("SetFont", ec)
      | Rd.EndOfFile => v.rdeoferror ("SetFont")
      | Rd.Failure (ref) => v.rdfailure ("SetFont", ref)
      | Thread.Alerted =>
      END;
    END
  END SetFont;
      
PROCEDURE GetColorScheme (v: T): PaintOp.ColorScheme  =
  BEGIN
    LOCK v.mu DO
      RETURN v.vtext.vOptions.whiteBlack (* one of several choices *)
    END
  END GetColorScheme;

PROCEDURE SetColorScheme (v: T; colorScheme: PaintOp.ColorScheme) =
  CONST name = "SetColorScheme";
  VAR vOptions: VText.VOptions;
  BEGIN
    LOCK v.mu DO
      TRY
        vOptions := v.vtext.vOptions;
        vOptions.whiteBlack := colorScheme;
        vOptions.whiteStroke := colorScheme;
        VText.ChangeVOptions (v.vtext, vOptions);
        IF v.scrollbar # NIL THEN
          ScrollerVBTClass.Colorize (v.scrollbar, colorScheme)
        END;
        VBT.Mark (v)
      EXCEPT
      | VTDef.Error (ec) => v.vterror (name, ec)
      | Rd.EndOfFile => v.rdeoferror (name)
      | Rd.Failure (ref) => v.rdfailure (name, ref)
      | Thread.Alerted =>
      END
    END
  END SetColorScheme;

PROCEDURE GetModel (v: T): [Model.Ivy .. Model.Xterm] =
  BEGIN
    LOCK v.mu DO
      TYPECASE v.m OF
      | IvyModel.T => RETURN Model.Ivy
      | EmacsModel.T => RETURN Model.Emacs
      | XtermModel.T => RETURN Model.Xterm
      | MacModel.T => RETURN Model.Mac
      ELSE                       <* ASSERT FALSE *>
      END
    END
  END GetModel;

PROCEDURE SetModel (v: T; model: Model) =
  BEGIN
    LOCK v.mu DO LockedSetModel (v, model) END
  END SetModel; 

TYPE
  StandardKeyFilter =
    KeyFilter.T OBJECT OVERRIDES apply := ApplyStandardKeyFilter END;

PROCEDURE LockedSetModel (v: T; model: Model) =
  VAR
    cs              := v.vtext.vOptions.whiteBlack;
    f : KeyFilter.T := NEW (StandardKeyFilter);
  BEGIN
    IF v.m # NIL THEN v.m.close () END;
    IF model = Model.Default THEN model := DefaultModel END;
    IF UseDiacritical THEN f := NEW (KeyFilter.Diacritical, next := f) END;
    CASE model OF
    | Model.Ivy => v.m := NEW (IvyModel.T, v := v).init (cs, f)
    | Model.Xterm => v.m := NEW (XtermModel.T, v := v).init (cs, f)
    | Model.Emacs => v.m := NEW (EmacsModel.T, v := v).init (cs, f)
    | Model.Mac => v.m := NEW (MacModel.T, v := v).init (cs, f)
    ELSE                         <* ASSERT FALSE *>
    END;
    FixIntervals (v)
  END LockedSetModel;

PROCEDURE GetKFocus (v: T; t: VBT.TimeStamp): BOOLEAN =
  <* LL = v.mu *>
  CONST name = "GetKFocus";
  BEGIN
    IF NOT v.owns [Focus] THEN
      v.ULfocus (TRUE, t);
      TRY
        VBT.Acquire (v, VBT.KBFocus, t);
        VText.SwitchCaret (v.vtext, VText.OnOffState.On);
        v.owns [Focus] := TRUE
      EXCEPT
      | VBT.Error (ec) => v.vbterror (name, ec)
      | VTDef.Error (ec) => v.vterror (name, ec)
      | Rd.EndOfFile => v.rdeoferror (name)
      | Rd.Failure (ref) => v.rdfailure (name, ref)
      | Thread.Alerted =>
      END
    END;
    RETURN v.owns [Focus]
  END GetKFocus;


PROCEDURE ChangeAllTextPorts (v: VBT.T; newModel := Model.Default) =
  BEGIN
    VAR ch: VBT.T;
    <* FATAL Split.NotAChild *>
    BEGIN
      IF ISTYPE(v, T) THEN
        NARROW(v, T).setModel(newModel);
        VBT.Mark (v);
      ELSIF ISTYPE(v, Split.T) THEN
        ch := Split.Succ(v, NIL);
        WHILE ch # NIL DO
          ChangeAllTextPorts(ch, newModel);
          ch := Split.Succ(v, ch)
        END;
        (* and also ... *)
        IF ISTYPE(v, AnchorBtnVBT.T) THEN
          ChangeAllTextPorts(NARROW(v, AnchorBtnVBT.T).menu, newModel)
        END
      END
    END
  END ChangeAllTextPorts;


PROCEDURE SetFontDimensions (v: T) =
  <* LL = v.mu *>
  BEGIN
    (* metrics := FontClass.FontMetrics(vbt, v.font); *)
    WITH st = VBT.ScreenTypeOf (v) DO
      IF st # NIL THEN
        WITH bounds = Palette.ResolveFont (
                        st, v.font).metrics.maxBounds,
             box = bounds.boundingBox DO
          v.fontHeight := Rect.VerSize (box);
          v.charWidth := bounds.printWidth
          (* not "Rect.HorSize (box)", alas *)
        END
      END
    END
  END SetFontDimensions;

(* UNUSED 
PROCEDURE Width (v: T): CARDINAL =
   (* Return the number of characters that will fit on a line, given the current
   size of "v".  If "v" has no width (because its window is iconic, for
   example), return 0.  If "v"'s font is not fixed-pitch, the result will be
   computed in terms of the width of the widest character in the font. *)
  This code is wrong. It ignores the margins. See TypeinVBT.Shape and
  NumericVBT.Reshape.
  VAR n := Rect.HorSize (VBT.Domain (v));
  BEGIN
    LOCK v.mu DO
      IF v.charWidth = 0 THEN RETURN 0 ELSE RETURN n DIV v.charWidth END
    END
  END Width;
*)
      
(* UNUSED
PROCEDURE Height (v: T): CARDINAL =
  BEGIN
    RETURN v.shape (Axis.T.Ver, 0).pref
  END Height;
*)

(**************** Focus, selections, etc. *****************)

<* EXPORTED *>
PROCEDURE TryFocus (v: T; t: VBT.TimeStamp): BOOLEAN =
  <* LL < v.mu, LL.sup = VBT.mu *>
  BEGIN
    (* Force all pending redisplays: *)
    VBTRep.Redisplay ();
    IF Rect.IsEmpty (VBT.Domain (v)) THEN
      RETURN FALSE
    ELSE
      LOCK v.mu DO
        IF NOT v.getKFocus (t) THEN
          RETURN FALSE
        ELSIF v.m.selection [Primary].alias = VBT.Source
                AND NOT v.m.takeSelection (VBT.Source, Primary, t)
                OR v.m.selection [Primary].alias = VBT.Target
                     AND NOT v.m.takeSelection (VBT.Target, Primary, t) THEN
          VBT.Release (v, VBT.KBFocus);
          v.owns [Focus] := FALSE;
          RETURN FALSE
        ELSE
          VBT.Mark (v);
          RETURN TRUE
        END
      END
    END
  END TryFocus;

<* EXPORTED *>
PROCEDURE HasFocus (v: T): BOOLEAN =
  BEGIN
    LOCK v.mu DO RETURN v.owns [Focus] END
  END HasFocus;

<* EXPORTED *>
PROCEDURE Select (v          : T;
                  time       : VBT.TimeStamp;
                  begin      : CARDINAL        := 0;
                  end        : CARDINAL        := LAST (CARDINAL);
                  sel                          := SelectionType.Primary;
                  replaceMode                  := FALSE;
                  caretEnd                     := VText.WhichEnd.Right   ) =
  BEGIN
    LOCK v.mu DO v.m.select (time, begin, end, sel, replaceMode, caretEnd) END
  END Select;

<* EXPORTED *>
PROCEDURE IsReplaceMode (v: T): BOOLEAN =
  BEGIN
    LOCK v.mu DO RETURN v.isReplaceMode () END
  END IsReplaceMode;

PROCEDURE LockedIsReplaceMode (v: T): BOOLEAN =
  BEGIN
    RETURN NOT v.readOnly AND v.m.selection [Primary].replaceMode
  END LockedIsReplaceMode;

<* EXPORTED *>
PROCEDURE GetSelection (v: T; sel := SelectionType.Primary): Extent =
  BEGIN
    LOCK v.mu DO RETURN v.m.getSelection (sel) END
  END GetSelection;

<* EXPORTED *>
PROCEDURE GetSelectedText (v: T; sel := SelectionType.Primary): TEXT =
  <* LL.sup = VBT.mu *>
  BEGIN
    LOCK v.mu DO RETURN v.m.getSelectedText (sel) END
  END GetSelectedText;

<* EXPORTED *>
PROCEDURE PutSelectedText (v: T; t: TEXT; sel := SelectionType.Primary) =
  <* LL.sup = VBT.mu *>
  BEGIN
    LOCK v.mu DO v.m.putSelectedText (t, sel) END
  END PutSelectedText;

<* EXPORTED *>
PROCEDURE Index (v: T): CARDINAL =
  BEGIN
    LOCK v.mu DO RETURN v.index () END
  END Index;

<* EXPORTED *>
PROCEDURE Seek (v: T; n: CARDINAL) =
  BEGIN
    LOCK v.mu DO v.m.seek (n) END
  END Seek;

PROCEDURE LockedIndex (v: T): CARDINAL =
  BEGIN
    TRY
      RETURN VText.CaretIndex (v.vtext)
    EXCEPT
    | VTDef.Error (ec) => v.vterror ("Index", ec); RETURN 0
    END
  END LockedIndex;

<* EXPORTED *>
PROCEDURE IsVisible (v: T; pos: CARDINAL): BOOLEAN =
  CONST name = "IsVisible";
  BEGIN
    TRY
      RETURN VText.InRegion (v.vtext, 0, pos)
    EXCEPT
    | VTDef.Error (ec) => v.vterror (name, ec)
    | Rd.EndOfFile => v.rdeoferror (name)
    | Rd.Failure (ref) => v.rdfailure (name, ref)
    | Thread.Alerted =>
    END;
    RETURN FALSE
  END IsVisible;

<* EXPORTED *>
PROCEDURE IsModified (v: T): BOOLEAN =
  BEGIN
    RETURN v.modifiedP
  END IsModified;

<* EXPORTED *>
PROCEDURE SetModified (v: T; modified: BOOLEAN) =
  BEGIN
    v.modifiedP := modified
  END SetModified;

<* EXPORTED *>
PROCEDURE GetVText (v: T): VText.T =
  BEGIN
    LOCK v.mu DO RETURN v.vtext END
  END GetVText;

(***************************  Replace & Insert  ****************************)

<* EXPORTED *>
PROCEDURE Replace (v: T; begin, end: CARDINAL; newText: TEXT) =
  BEGIN
    LOCK v.mu DO EVAL v.unsafeReplace (begin, end, newText) END
  END Replace;

PROCEDURE LockedReplace (v: T; begin, end: CARDINAL; newText: TEXT): Extent =
  <* LL = v.mu *>
  BEGIN
    IF v.readOnly OR begin < v.typeinStart THEN
      RETURN NotFound
    ELSE
      RETURN v.unsafeReplace (begin, end, newText)
    END
  END LockedReplace;

PROCEDURE UnsafeReplace (v: T; begin, end: CARDINAL; newText: TEXT): Extent =
  <* LL = v.mu *>
  CONST name = "Replace";
  VAR len := v.length ();
  BEGIN
    begin := MIN (begin, len);
    end := MIN (MAX (begin, end), len);
    IF begin <= end THEN
      TextPortClass.AddToUndo (v, begin, end, newText);
      TRY
        VText.Replace (v.vtext, begin, end, newText);
        IF NOT v.modifiedP THEN v.modifiedP := TRUE; v.ULmodified () END;
        VBT.Mark (v);
        RETURN Extent {begin, begin + Text.Length (newText)}
      EXCEPT
      | VTDef.Error (ec) => v.vterror (name, ec)
      | Rd.EndOfFile => v.rdeoferror (name)
      | Rd.Failure (ref) => v.rdfailure (name, ref)
      | Thread.Alerted =>
      END
    END;
    RETURN NotFound
  END UnsafeReplace;

<* EXPORTED *>
PROCEDURE Insert (v: T; t: TEXT) =
  BEGIN
    IF NOT Text.Empty (t) THEN LOCK v.mu DO v.unsafeInsert (t) END END
  END Insert;

PROCEDURE LockedInsert (v: T; t: TEXT) =
  BEGIN
    IF NOT v.readOnly THEN v.unsafeInsert (t) END
  END LockedInsert;

PROCEDURE UnsafeInsert (v: T; t: TEXT) =
  VAR
    m   := v.m;
    rec := m.selection [Primary];
  VAR p: CARDINAL;
  BEGIN
    IF v.isReplaceMode () THEN
      m.putSelectedText (t, Primary)
    ELSE
      p := v.index ();
      IF p < v.typeinStart THEN p := v.length (); m.seek (p) END;
      EVAL v.unsafeReplace (p, p, t)
    END;
    p := v.index ();
    m.highlight (rec, IRange {p, p, p});
    rec.anchor.l := p;
    rec.anchor.r := p
  END UnsafeInsert;
  
(************************  Shape of current text  *************************)


PROCEDURE Shape (v: T; ax: Axis.T; n: CARDINAL): VBT.SizeRange =
  BEGIN
    IF ax = Axis.T.Ver THEN v.lastNonEmptyWidth := n END;
    RETURN TextPortClass.T.shape(v, ax, n);
  END Shape;

PROCEDURE Reshape (v: T; READONLY cd: VBT.ReshapeRec) =
  CONST name = "Reshape";
  VAR
    newRect                                 := cd.new;
    dividers: ARRAY [0 .. 0] OF VText.Pixels;
  BEGIN
    IF Rect.IsEmpty(newRect) THEN RETURN END;
    VAR
      old := v.lastNonEmptyWidth;
      new := Rect.HorSize(newRect);
    BEGIN
      IF new # old THEN
        VAR
          oldShape := v.shape(Axis.T.Ver, old);
          newShape := v.shape(Axis.T.Ver, new);
        BEGIN
          IF oldShape # newShape THEN VBT.NewShape(v) END
        END
      END
    END;
    LOCK v.mu DO
      IF NOT v.vtext.vOptions.wrap THEN
        newRect.east := LAST(INTEGER) DIV 2
      END;
      TRY
        VText.Move(v.vtext, newRect, cd.saved, dividers);
        VText.Update(v.vtext);
        v.linesShown :=
          1 + VText.WhichLine(v.vtext, 0, cd.new.south);
        (* if it will all fit, normalize to fit *)
        IF Rect.IsEmpty(cd.prev) AND NOT Rect.IsEmpty(cd.new)
             AND VText.LinesBetween(
                   v.vtext, 0, v.length(), v.linesShown)
                   < v.linesShown THEN
          v.normalize(0)         (* Normalize calls
                                    VBT.Mark *)
        ELSE
          VBT.Mark(v)
        END
      EXCEPT
      | VTDef.Error (ec) => v.vterror(name, ec)
      | Rd.EndOfFile => v.rdeoferror(name)
      | Rd.Failure (ref) => v.rdfailure(name, ref)
      | Thread.Alerted =>
      END
    END
  END Reshape;

(* UNUSED
PROCEDURE ShapeInfo (v: T; VAR lineCount, lineLength: INTEGER) =
  (* Return the number of lines in the text and the number of
   characters in the longest line (excluding the newline).  A
   client may use this information in conjunction with
   "VBTShape.Set" to shape "v" to fit its text.  Having done so,
   it should be prepared to do it again on a rescreen to a screen
   of different density. *)
  BEGIN
    LOCK v.mu DO
      VAR
        e      := MTextUnit.Extent {0, 0, TRUE};
        length := v.length ();
      BEGIN
        lineCount := 0;
        lineLength := 0;
        IF length = 0 THEN RETURN END;
        WHILE e.right < length DO
          e := MTextUnit.LineExtent (v.vtext.mtext, e.right);
          INC (lineCount);
          lineLength := MAX (lineLength, e.right - e.left - 1)
        END;
        (* adjust for last line: if ends with \n, increment lineCount;
           otherwise, len of last line is right-left, not right-left-1. *)
        IF MText.GetChar (v.vtext.mtext, length - 1) = Return THEN
          INC (lineCount);
          lineLength := MAX (lineLength, e.right - e.left - 1)
        ELSE
          lineLength := MAX (lineLength, e.right - e.left)
        END                      (* IF *)
      END                        (* BEGIN *)
    END                          (* LOCK *)
  END ShapeInfo;
*)

PROCEDURE Key (v: T; READONLY cd: VBT.KeyRec) =
  VAR OK: BOOLEAN;
  BEGIN
    LOCK v.mu DO
      OK :=
        cd.wentDown AND v.owns [Focus] AND NOT Rect.IsEmpty (VBT.Domain (v))
    END;
    IF OK THEN v.m.keyfilter.apply (v, cd) END
  END Key;

PROCEDURE ApplyStandardKeyFilter (<* UNUSED *> self: StandardKeyFilter;
                                  vbt: VBT.T;
                                  cd : VBT.KeyRec) =
  VAR v: T := vbt;
  BEGIN
    v.filter (cd)
  END ApplyStandardKeyFilter;

PROCEDURE Filter (v: T; cd: VBT.KeyRec) =
  VAR
    ch: CHAR;
    m        := v.m;
  BEGIN
    IF cd.whatChanged = VBT.NoKey THEN RETURN END;
    LOCK v.mu DO
      v.lastCmdKind := v.thisCmdKind;
      v.thisCmdKind := TextPortClass.CommandKind.OtherCommand;
      ch := KeyTrans.Latin1 (cd.whatChanged);

      IF ch = Return THEN
        IF VBT.Modifier.Shift IN cd.modifiers THEN
          v.insert (Wr.EOL)
        ELSIF VBT.Modifier.Option IN cd.modifiers THEN
          TextPortClass.InsertNewline (v)
        ELSE
          v.ULreturnAction (cd)
        END
      ELSIF ch = Tab THEN
        v.ULtabAction (cd)
      ELSIF KeyboardKey.Left <= cd.whatChanged
              AND cd.whatChanged <= KeyboardKey.Down THEN
        m.arrowKey (cd)
      ELSIF VBT.Modifier.Control IN cd.modifiers THEN
        m.controlChord (ch, cd);
        RETURN
      ELSIF VBT.Modifier.Option IN cd.modifiers THEN
        m.optionChord (ch, cd);
        RETURN
      ELSIF ch = Backspace OR ch = Del THEN
        VAR interval := v.m.selection [Primary].interval;
        BEGIN
          (* Treat an empty interval as if it were not replace-mode *)
          IF v.isReplaceMode () AND interval.left () # interval.right () THEN
            m.putSelectedText ("")
          ELSE
            EVAL TextPortClass.DeletePrevChar (v)
          END
        END
      ELSIF ch IN ISOChar.Graphics THEN
        v.insert (Text.FromChar (ch))
      ELSE
        (* including NullKey, for untranslatable keys *)
        RETURN
      END;
      v.normalize ()
    END                          (* LOCK *)
  END Filter;

<* EXPORTED *>
PROCEDURE Newline (v: T) =
  BEGIN
    LOCK v.mu DO IF NOT v.readOnly THEN v.insert (Wr.EOL) END END
  END Newline;

<* EXPORTED *>
PROCEDURE NewlineAndIndent (v: T) =
  BEGIN
    LOCK v.mu DO v.newlineAndIndent () END
  END NewlineAndIndent;

PROCEDURE LockedNewlineAndIndent (v: T) =
  BEGIN
    IF v.readOnly THEN RETURN END;
    VAR
      index := v.index ();
      a     := MTextUnit.LineInfo (v.vtext.mtext, index);
    BEGIN
      IF a.leftMargin = a.rightEnd AND index = a.rightEnd
           AND NOT v.isReplaceMode () THEN
        (* We're at the end of an all-blank line. *)
        EVAL v.replace (a.left, a.left, Wr.EOL)
      ELSIF a.leftMargin = a.rightMargin THEN (* line is all blanks *)
        v.insert (Wr.EOL)
      ELSE
        (* Copy all the leading blanks onto the new line. *)
        v.insert (
          Wr.EOL & MText.GetText (v.vtext.mtext, a.left, a.leftMargin))
      END
    END
  END LockedNewlineAndIndent;

PROCEDURE Repaint (v: T; READONLY rgn: Region.T) =
  CONST name = "Repaint";
  BEGIN
    TRY
      LOCK v.mu DO
        VText.Bad (v.vtext, Region.BoundingBox (rgn));
        VText.Update (v.vtext)
      END
    EXCEPT
    | VTDef.Error (ec) => v.vterror (name, ec)
    | Rd.EndOfFile => v.rdeoferror (name)
    | Rd.Failure (ref) => v.rdfailure (name, ref)
    | Thread.Alerted =>
    END
  END Repaint;

PROCEDURE FixIntervals (v: T) =
  <* LL = v.mu *>
  BEGIN
    TRY
      FOR t := Primary TO Secondary DO
        IF v.m.selection [t] # NIL THEN
          TextPortClass.ChangeIntervalOptions (v, v.m.selection [t])
        END
      END
    EXCEPT
    | VTDef.Error (ec) => v.vterror ("Rescreen", ec)
    END
  END FixIntervals;

PROCEDURE Rescreen (v: T; READONLY cd: VBT.RescreenRec) =
  BEGIN
    LOCK v.mu DO
      VText.Rescreen (v.vtext, cd);
      SetFontDimensions (v);
      FixIntervals (v);
      VBT.NewShape (v)
    END
  END Rescreen;

PROCEDURE Redisplay (v: T) =
  CONST name = "Redisplay";
  BEGIN
    TRY
      LOCK v.mu DO
        VText.Update (v.vtext);
        IF v.scrollbar # NIL THEN v.scrollbar.update () END
      END 
    EXCEPT
    | VTDef.Error (ec) => v.vterror (name, ec)
    | Rd.EndOfFile => v.rdeoferror (name)
    | Rd.Failure (ref) => v.rdfailure (name, ref)
    | Thread.Alerted =>
    END
  END Redisplay;

(****************************  callback methods  ****************************)

PROCEDURE ReturnAction (v: T; <* UNUSED *> READONLY event: VBT.KeyRec) =
  BEGIN
    NewlineAndIndent (v)
  END ReturnAction;

PROCEDURE UnlockedReturnAction (v: T; READONLY event: VBT.KeyRec) =
  BEGIN
    Thread.Release (v.mu);
    TRY v.returnAction (event) FINALLY Thread.Acquire (v.mu) END
  END UnlockedReturnAction; 
  
PROCEDURE Insert4spaces (v: T; <* UNUSED *> READONLY event: VBT.KeyRec) =
  BEGIN
    LOCK v.mu DO v.insert ("    ") END
  END Insert4spaces;

PROCEDURE UnlockedTabAction (v: T; READONLY event: VBT.KeyRec) =
  BEGIN
    Thread.Release (v.mu);
    TRY v.tabAction (event) FINALLY Thread.Acquire (v.mu) END
  END UnlockedTabAction; 
  
(*************************  Miscellany  ************************)

<* EXPORTED *>
PROCEDURE Normalize (v: T; to: INTEGER := -1) =
  BEGIN
    LOCK v.mu DO v.normalize (to) END
  END Normalize;

PROCEDURE LockedNormalize (v: T; to: INTEGER) =
  <* LL = v.mu *>
  CONST name = "Normalize";
  VAR point: CARDINAL;
  BEGIN
    TRY
      IF to < 0 THEN
        point := v.index ()
      ELSE
        point := MIN (to, v.length ())
      END;
      IF NOT VText.InRegion (v.vtext, 0, point) THEN
        VText.SetStart (v.vtext, 0, point, v.linesShown DIV 2)
      END;
      VBT.Mark (v)
    EXCEPT
    | VTDef.Error (ec) => v.vterror (name, ec)
    | Rd.EndOfFile => v.rdeoferror (name)
    | Rd.Failure (ref) => v.rdfailure (name, ref)
    | Thread.Alerted =>
    END
  END LockedNormalize;

PROCEDURE UnlockedFocus (v: T; gaining: BOOLEAN; time: VBT.TimeStamp) =
  BEGIN
    Thread.Release (v.mu);
    TRY v.focus (gaining, time) FINALLY Thread.Acquire (v.mu) END
  END UnlockedFocus;

PROCEDURE IgnoreFocus (<* UNUSED *> v:     T;
                       <* UNUSED *> gaining: BOOLEAN;
                       <* UNUSED *> time   : VBT.TimeStamp) =
  BEGIN
  END IgnoreFocus;

PROCEDURE UnlockedModified (v: T) =
  BEGIN
    Thread.Release (v.mu);
    TRY v.modified () FINALLY Thread.Acquire (v.mu) END
  END UnlockedModified;

PROCEDURE IgnoreModification (<* UNUSED *> v: T)  =
  BEGIN
  END IgnoreModification;

PROCEDURE FindSource (v   : T;
                      time: VBT.TimeStamp;
                      loc        := TextPortClass.Loc.Next;
                      ignoreCase := TRUE                    ) =
  BEGIN
    TRY
      TextPortClass.FindAndSelect (
        v, v.m.read (VBT.Source, time), time, loc, ignoreCase)
    EXCEPT
    | VBT.Error (ec) => v.vbterror ("findSource", ec)
    END
  END FindSource;

PROCEDURE NotFoundProc (<* UNUSED *> v: T) =
  BEGIN
    (* SmallIO.PutChar (SmallIO.stderr, '\007') *)
  END NotFoundProc;

(************************** VBT methods ***************************)

(* In this section, we lock v.mu and relay the method-calls to the Model,
   which handles selections, the mouse, etc. *)

PROCEDURE Misc (v: T; READONLY cd: VBT.MiscRec) =
  BEGIN
    LOCK v.mu DO
      v.m.misc (cd)
    END
  END Misc;

PROCEDURE Read (v: T; s: VBT.Selection; typecode: CARDINAL): VBT.Value
  RAISES {VBT.Error} =
  <* LL.sup <= VBT.mu *>
  BEGIN
    IF typecode # TYPECODE (TEXT) THEN
      RAISE VBT.Error (VBT.ErrorCode.WrongType)
    ELSE
      LOCK v.mu DO RETURN VBT.FromRef (v.m.read (s, 0)) END
    END
  END Read;

PROCEDURE Write (v       : T;
                 s       : VBT.Selection;
                 value   : VBT.Value;
                 typecode: CARDINAL       ) RAISES {VBT.Error} =
  <* LL.sup <= VBT.mu *>
  BEGIN
    LOCK v.mu DO
      IF typecode # TYPECODE (TEXT) THEN
        RAISE VBT.Error (VBT.ErrorCode.WrongType)
      ELSIF v.readOnly THEN
        RAISE VBT.Error (VBT.ErrorCode.Unwritable)
      ELSE
        TYPECASE value.toRef () OF
        | NULL => RAISE VBT.Error (VBT.ErrorCode.WrongType)
        | TEXT (t) => v.m.write (s, 0, t)
        ELSE
          RAISE VBT.Error (VBT.ErrorCode.WrongType)
        END
      END
    END
  END Write;

PROCEDURE Mouse (v: T; READONLY cd: VBT.MouseRec) =
  BEGIN
    LOCK v.mu DO
      (* IF v.getKFocus (cd.time) THEN v.m.mouse (cd) END *)
      v.m.mouse (cd)
    END
  END Mouse;

PROCEDURE Position (v: T; READONLY cd: VBT.PositionRec) =
  BEGIN
    LOCK v.mu DO
      IF NOT v.m.dragging THEN     (* skip *)
      ELSIF cd.cp.gone THEN
        VBT.SetCage (v, VBT.GoneCage)
      ELSE
        v.m.position (cd)
      END
    END
  END Position;

PROCEDURE vbterror (v: T; msg: TEXT; ec: VBT.ErrorCode) =
  BEGIN
    v.ULerror (msg & ": " & TextPortClass.VBTErrorCodeTexts [ec])
  END vbterror;

PROCEDURE vterror (v: T; msg: TEXT; ec: VTDef.ErrorCode) =
  BEGIN
    v.ULerror (msg & ": " & VTDef.ErrorCodeTexts [ec])
  END vterror;

PROCEDURE rdfailure (v: T; msg: TEXT; ref: REFANY) =
  BEGIN
    v.ULerror (msg & ": " & RdUtils.FailureText (ref))
  END rdfailure; 

PROCEDURE rdeoferror (v: T; msg: TEXT) =
  BEGIN
    v.ULerror (msg & ": End of file")
  END rdeoferror;

PROCEDURE UnlockedError (v: T; msg: TEXT) =
  BEGIN
    Thread.Release (v.mu);
    TRY v.error (msg) FINALLY Thread.Acquire (v.mu) END
  END UnlockedError;

VAR errMu := NEW(MUTEX);

PROCEDURE Error (<* UNUSED *> v: T; msg: TEXT) =
  <* FATAL Wr.Failure, Thread.Alerted *>
  BEGIN
    IF debug THEN
      LOCK errMu DO
        Wr.PutText(Stdio.stderr, msg);
        Wr.PutChar(Stdio.stderr, '\n')
      END
    END
  END Error;

(*************************  Module Initialization  ************************)

BEGIN
  WITH s = VBTKitEnv.TextPortModel DO
    IF Text.Equal (s, "ivy") THEN
      DefaultModel := Model.Ivy
    ELSIF Text.Equal (s, "xterm") THEN
      DefaultModel := Model.Xterm
    ELSIF Text.Equal (s, "mac") THEN
      DefaultModel := Model.Mac
    ELSE
      DefaultModel := Model.Emacs
    END
  END;
  debug := Env.Get ("TEXTPORTDEBUG") # NIL;
  WITH s = Env.Get ("KEYBOARD_MODE") DO
    UseDiacritical := s # NIL AND Text.Equal (s, "French")
  END
END TextPort.
