(* Copyright © 1992-1993, Digital Equipment Corporation                *)
(* All rights reserved.                                                   *)
(* See the file COPYRIGHT for a full description.                         *)
(*                                                                        *)
(* Last modified on Mon Jan 23 09:30:58 PST 1995 by kalsow                *)
(*      modified on Fri May 14 00:54:18 PDT 1993 by meehan                *)
<* PRAGMA LL *>

MODULE MacModel;

IMPORT Env, KeyboardKey, KeyFilter, Latin1Key, PaintOp, Rd, Text, TextPort,
       TextPortClass, Thread, VBT, VTDef, VText;

REVEAL
  T = TextPortClass.Model BRANDED OBJECT
        clipboard                 := ""; (* Source selection *)
        screen   : VBT.ScreenID;
      OVERRIDES
        arrowKey     := ArrowKey;
        controlChord := ControlChord;
        copy         := Copy;
        init         := Init;
        mouse        := Mouse;
        optionChord  := OptionChord; (* no-op *)
        paste        := Paste;
        read         := Read;
        write        := Write;
      END;

TYPE
  MacFilter = KeyFilter.T OBJECT
                state := State.Initial
              OVERRIDES
                apply := ApplyMacFilter
              END;
CONST
  Primary = TextPort.SelectionType.Primary;
  Source  = TextPortClass.VType.Source;

VAR
  OptionMod  := VBT.Modifier.Option;
  CommandMod := VBT.Modifier.Control;

PROCEDURE Init (m: T; colorScheme: PaintOp.ColorScheme; filter: KeyFilter.T):
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
             alias := VBT.NilSel, replaceMode := FALSE);
      m.keyfilter := NEW (MacFilter, next := filter);
    EXCEPT
    | VTDef.Error (ec) => m.v.vterror ("Model Init", ec)
    END;
    RETURN m
  END Init;

TYPE
  State = {Initial, E, I, N, U, Grave};
  f = RECORD
        key   : CHAR;
        keysym: VBT.KeySym
      END;
  Table = ARRAY OF f;
  M = ARRAY [State.E .. State.Grave] OF REF Table;

VAR
  Tables     : M;
  OptionTable    := ARRAY CHAR OF VBT.KeySym {VBT.NoKey, ..};

PROCEDURE ApplyMacFilter (self: MacFilter;
                          v   : VBT.T;
                          cd  : VBT.KeyRec ) =
  VAR
    c        := cd.whatChanged;
    s        := self.state;
    ch: CHAR;
  BEGIN
    IF KeyFilter.IsModifier (c) THEN
      (* skip.  It's just another modifier. *)
    ELSIF CommandMod IN cd.modifiers THEN
      cd.modifiers :=
        cd.modifiers + VBT.Modifiers {VBT.Modifier.Control};
      self.next.apply (v, cd)
    ELSIF s # State.Initial THEN
      self.state := State.Initial;
      IF (cd.modifiers = VBT.Modifiers {}
            OR cd.modifiers = VBT.Modifiers {VBT.Modifier.Shift})
           AND Latin1Key.space <= c AND c <= Latin1Key.asciitilde THEN
        ch := VAL (c, CHAR);
        FOR i := FIRST (Tables [s]^) TO LAST (Tables [s]^) DO
          IF ch = Tables [s, i].key THEN
            cd.whatChanged := Tables [s, i].keysym;
            self.next.apply (v, cd);
            RETURN
          END
        END
      END
    ELSIF OptionMod IN cd.modifiers THEN
      IF Latin1Key.space <= c AND c <= Latin1Key.asciitilde THEN
        ch := VAL (c, CHAR);
        cd.whatChanged := OptionTable [ch];
        IF cd.whatChanged < 0 THEN
          self.state := VAL (-cd.whatChanged, State)
        ELSE
          cd.modifiers := VBT.Modifiers {};
          self.next.apply (v, cd)
        END
      ELSE
        self.next.apply (v, cd)
      END
    ELSE
      self.next.apply (v, cd)
    END
  END ApplyMacFilter;
 
CONST
  OptionE = ARRAY OF
              f {f {'a', Latin1Key.aacute}, f {'A', Latin1Key.Aacute},
                 f {'e', Latin1Key.eacute}, f {'E', Latin1Key.Eacute},
                 f {'i', Latin1Key.iacute}, f {'I', Latin1Key.Iacute},
                 f {'o', Latin1Key.oacute}, f {'O', Latin1Key.Oacute},
                 f {'u', Latin1Key.uacute}, f {'U', Latin1Key.Uacute},
                 f {' ', Latin1Key.acute}};
  OptionI = ARRAY OF
              f {
              f {'a', Latin1Key.acircumflex}, f {'A', Latin1Key.Acircumflex},
              f {'e', Latin1Key.ecircumflex}, f {'E', Latin1Key.Ecircumflex},
              f {'i', Latin1Key.icircumflex}, f {'I', Latin1Key.Icircumflex},
              f {'o', Latin1Key.ocircumflex}, f {'O', Latin1Key.Ocircumflex},
              f {'u', Latin1Key.ucircumflex}, f {'U', Latin1Key.Ucircumflex},
              f {' ', Latin1Key.asciicircum}};
  OptionN = ARRAY OF
              f {f {'a', Latin1Key.atilde}, f {'A', Latin1Key.Atilde},
                 f {'n', Latin1Key.ntilde}, f {'N', Latin1Key.Ntilde},
                 f {'o', Latin1Key.otilde}, f {'O', Latin1Key.Otilde},
                 f {' ', Latin1Key.asciitilde}};
  OptionU = ARRAY OF
              f {
              f {'a', Latin1Key.adiaeresis}, f {'A', Latin1Key.Adiaeresis},
              f {'e', Latin1Key.ediaeresis}, f {'E', Latin1Key.Ediaeresis},
              f {'i', Latin1Key.idiaeresis}, f {'I', Latin1Key.Idiaeresis},
              f {'o', Latin1Key.odiaeresis}, f {'O', Latin1Key.Odiaeresis},
              f {'u', Latin1Key.udiaeresis}, f {'U', Latin1Key.Udiaeresis},
              f {'y', Latin1Key.ydiaeresis},
              f {' ', Latin1Key.diaeresis}};
  OptionGrave = ARRAY OF
                  f {f {'a', Latin1Key.agrave}, f {'A', Latin1Key.Agrave},
                     f {'e', Latin1Key.egrave}, f {'E', Latin1Key.Egrave},
                     f {'i', Latin1Key.igrave}, f {'I', Latin1Key.Igrave},
                     f {'o', Latin1Key.ograve}, f {'O', Latin1Key.Ograve},
                     f {'u', Latin1Key.ugrave}, f {'U', Latin1Key.Ugrave},
                     f {' ', Latin1Key.grave}};

CONST
  OPTSHIFT = Table {
               f {'A', Latin1Key.Aring},
               (* B => smallCaps 1? *)
               f {'C', Latin1Key.Ccedilla},
               f {'D', Latin1Key.Icircumflex},
               f {'E', Latin1Key.acute},
               f {'F', Latin1Key.Idiaeresis},
               (* G => italic close double-quote? *)
               f {'H', Latin1Key.Oacute},
               f {'I', Latin1Key.asciicircum},
               f {'J', Latin1Key.Ocircumflex},
               (* K => nothing *)
               f {'L', Latin1Key.Ograve},
               f {'M', Latin1Key.Acircumflex},
               f {'N', Latin1Key.asciitilde},
               f {'O', Latin1Key.Ooblique},
               (* P => nothing *)
               (* Q => OE *)
               (* R => funny % *)
               f {'S', Latin1Key.Iacute},
               (* T => inverted circumflex? *)
               f {'U', Latin1Key.diaeresis},
               (* V => nothing *)
               (* W => double comma? *)
               (* X => backwards cedilla? *)
               f {'Y', Latin1Key.Aacute},
               f {'Z', Latin1Key.cedilla}};


  OPTION = Table
                {f {'a', Latin1Key.aring},
                (* b => nothing *)
                f {'c', Latin1Key.ccedilla},
                (* d => nothing *)
                f {'e', -ORD (State.E)}, (* Special table *)
                (* f => italic f *)
                f {'g', Latin1Key.copyright},
                (* h => raised dot? *)
                f {'i', -ORD (State.I)}, (* Special table *)
                (* j => nothing *)
                f {'k', Latin1Key.degree}, (* Same as Option-8? *)
                f {'l', Latin1Key.notsign},
                f {'m', Latin1Key.mu},
                f {'n', -ORD (State.N)}, (* Special table *)
                f {'o', Latin1Key.oslash},
                (* p => nothing *)
                (* q => oe *)
                f {'r', Latin1Key.registered},
                f {'s', Latin1Key.ssharp},
                (* t => dagger *)
                f {'u', -ORD (State.U)}, (* Special table *)
                (* v => nothing *)
                (* w => nothing *)
                (* x => nothing *)
                f {'y', Latin1Key.yen},
                (* z => nothing *)

                f {'1', Latin1Key.exclamdown},
                (* 2 => trademark *)
                f {'3', Latin1Key.sterling},
                f {'4', Latin1Key.cent},
                (* 5 => nothing *)
                f {'6', Latin1Key.section},
                f {'7', Latin1Key.paragraph},
                (* 8 => bullet *)
                f {'9', Latin1Key.ordfeminine},
                f {'0', Latin1Key.masculine},
                f {'`', -ORD (State.Grave)}, (* Special table *)
                f {'-', Latin1Key.hyphen},
                (* = => nothing *)
                (* [ => open double quote *)
                (* ] => open single quote *)
                f {'\\', Latin1Key.guillemotleft},
                (* ; => 3-dot ellipsis *)
                f {'\'', Latin1Key.ae},
                (* , => nothing *)
                (* . => nothing *)
                f {'/', Latin1Key.division},

               f {'!', Latin1Key.slash},
               f {'@', Latin1Key.currency},
               (* # => small left angle bracket? *)
               (* $ => small right angle bracket? *)
               (* % => fi ligature *)
               (* ^ => fl ligature *)
               (* & => double dagger *)
               f {'*', Latin1Key.degree},
               f {'(', Latin1Key.periodcentered},
               f {')', Latin1Key.comma}, (* ?? *)
               f {'~', Latin1Key.grave},
               (* _ => long dash *)
               f {'+', Latin1Key.plusminus},
               (* { => close double quote *)
               (* } => close single quote *)
               f {'|', Latin1Key.guillemotright},
               f {':', Latin1Key.Uacute},
               f {'"', Latin1Key.AE},
               f {'<', Latin1Key.macron},
               (* >  => breve? *)
               f {'?', Latin1Key.questiondown}};



PROCEDURE ControlChord (m: T; ch: CHAR; READONLY cd: VBT.KeyRec) =
  BEGIN
    CASE ch OF
    | 'c' => m.copy (cd.time)
    | 'v' => m.paste (cd.time)
    | 'x' => m.cut (cd.time)
    | 'Z' => TextPortClass.Redo (m.v)
    | 'z' => TextPortClass.Undo (m.v)
    ELSE
      (* Don't normalize if unknown chord, including just ctrl itself. *)
      RETURN
    END;
    m.v.normalize (-1)
  END ControlChord;

PROCEDURE OptionChord (<* UNUSED *>          m : T;
                       <* UNUSED *>          ch: CHAR;
                       <* UNUSED *> READONLY cd: VBT.KeyRec) =
  BEGIN
  END OptionChord;

PROCEDURE Mouse (m: T; READONLY cd: VBT.MouseRec) =
  VAR
    r  : TextPortClass.IRange;
    rec                       := m.selection [Primary];
  BEGIN
    IF NOT m.v.getKFocus (cd.time) THEN RETURN END;
    m.screen := cd.cp.screen;
    TRY
      CASE cd.clickType OF
      | VBT.ClickType.FirstDown =>
          CASE cd.whatChanged OF
          | VBT.Modifier.MouseL =>
              IF cd.modifiers = VBT.Modifiers {} THEN
                (* Unshifted click => set the selection *)
                rec.mode := VAL (MIN (cd.clickCount DIV 2, 2),
                                 VText.SelectionMode);
                r := TextPortClass.GetRange (m.v, cd.cp, rec.mode);
                m.dragging := TRUE;
                IF rec.mode = VText.SelectionMode.CharSelection THEN
                  r.left := r.middle;
                  r.right := r.middle
                END;
                rec.anchor.l := r.left;
                rec.anchor.r := r.right;
                rec.replaceMode := TRUE;
                TextPortClass.ChangeIntervalOptions (m.v, rec);
                m.highlight (rec, r)
              ELSIF VBT.Modifier.Shift IN cd.modifiers THEN
                (* Shift-click => extend selection *)
                IF rec.interval.left () >= m.v.typeinStart THEN
                  rec.replaceMode := NOT m.v.readOnly;
                  TextPortClass.ChangeIntervalOptions (m.v, rec)
                END;
                r := TextPortClass.GetRange (m.v, cd.cp, rec.mode);
                m.approachingFromLeft :=
                  r.left < (rec.anchor.l + rec.anchor.r) DIV 2;
                m.extend (rec, r.left, r.right);
                m.dragging := TRUE
              END
          ELSE
          END
      | VBT.ClickType.LastUp =>
          IF m.dragging THEN
            rec.anchor.l := rec.interval.left ();
            rec.anchor.r := rec.interval.right ();
            m.dragging := FALSE
          END
      ELSE
        m.dragging := FALSE
      END
    EXCEPT
    | VTDef.Error (ec) => m.v.vterror ("Change Highlight", ec)
    END
  END Mouse;

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
    IF NOT m.v.readOnly THEN
      TextPortClass.Model.paste (m, time);
      CancelHighlight (m)
    END
  END Paste;

PROCEDURE CancelHighlight (m: T) =
  BEGIN
    TRY
      VText.SwitchInterval (
        m.selection [Primary].interval, VText.OnOffState.Off);
    EXCEPT
    | VTDef.Error (ec) => m.v.vterror ("CancelHighlight", ec)
    END
  END CancelHighlight;

PROCEDURE ArrowKey (m: T; READONLY cd: VBT.KeyRec) =
  VAR ch := cd.whatChanged;
  BEGIN
    IF NOT VBT.Modifier.Shift IN cd.modifiers THEN
      CancelHighlight (m);
      TextPortClass.Model.arrowKey (m, cd)
    ELSE
      CONST name = "Arrow Key";
      TYPE End = {Left, Right};
      VAR
        v                                := m.v;
        here                             := v.index ();
        oldr, newr: TextPortClass.IRange;
      PROCEDURE extentToIRange (READONLY x: TextPort.Extent; end: End):
        TextPortClass.IRange =
        BEGIN
          IF end = End.Left THEN
            RETURN TextPortClass.IRange {x.l, x.l, x.r}
          ELSE
            RETURN TextPortClass.IRange {x.l, x.r, x.r}
          END
        END extentToIRange;
      PROCEDURE getIRange (): TextPortClass.IRange
        RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted, VTDef.Error} =
        VAR cp: VBT.CursorPosition;
        BEGIN
          (* We need to synthesize a CursorPosition from the new location, so
             that we can pass it to GetRange. *)
          cp.screen := m.screen;
          cp.gone := FALSE;
          cp.offScreen := FALSE;
          VText.Locate (m.v.vtext, 0, m.v.index (), cp.pt.h, cp.pt.v);
          RETURN TextPortClass.GetRange (
                   m.v, cp, VText.SelectionMode.WordSelection)
        END getIRange;
      BEGIN
        TRY
          WITH ext = m.getSelection (Primary) DO
            oldr := extentToIRange (ext, VAL (ORD (here = ext.r), End))
          END;
          IF VBT.Modifier.Option IN cd.modifiers THEN
            m.selection [Primary].mode := VText.SelectionMode.WordSelection;
            CASE ch OF
            | KeyboardKey.Left =>
                newr :=
                  extentToIRange (TextPortClass.FindPrevWord (m.v), End.Left)
            | KeyboardKey.Right =>
                newr :=
                  extentToIRange (TextPortClass.FindNextWord (m.v), End.Right)
            | KeyboardKey.Up =>
                TextPortClass.UpOneLine (m.v);
                newr := getIRange ()
            | KeyboardKey.Down =>
                TextPortClass.DownOneLine (m.v);
                newr := getIRange ()
            ELSE <* ASSERT FALSE *>
            END
          ELSE
            m.selection [Primary].mode := VText.SelectionMode.CharSelection;
            TextPortClass.Model.arrowKey (m, cd);
            here := v.index ();
            newr := TextPortClass.IRange {here, here, here}
          END;
          m.extend (m.selection [Primary], newr.left, newr.right)
        EXCEPT
        | VTDef.Error (ec) => m.v.vterror (name, ec)
        | Rd.EndOfFile => m.v.rdeoferror (name)
        | Rd.Failure (ref) => m.v.rdfailure (name, ref)
        | Thread.Alerted =>
        END
      END
    END
  END ArrowKey;

PROCEDURE init () =
  CONST
    Xmodnames = ARRAY [VBT.Modifier.Lock .. VBT.Modifier.Mod3] OF
                  TEXT {"lock", "control", "mod1", "mod2",
                        "mod3", "mod4", "mod5"};
  VAR s := Env.Get ("MacOptionModifier");
  BEGIN
    IF s # NIL THEN
      s := TextPortClass.TextLowerCase (s);
      FOR i := FIRST (Xmodnames) TO LAST (Xmodnames) DO
        IF Text.Equal (s, Xmodnames [i]) THEN
          OptionMod := i;
          EXIT
        END
      END
    END;
    s := Env.Get ("MacCommandModifier");
    IF s # NIL THEN
      s := TextPortClass.TextLowerCase (s);
      FOR i := FIRST (Xmodnames) TO LAST (Xmodnames) DO
        IF Text.Equal (s, Xmodnames [i]) THEN
          CommandMod := i;
          EXIT
        END
      END
    END;
    FOR i := FIRST (OPTSHIFT) TO LAST (OPTSHIFT) DO
      OptionTable [OPTSHIFT [i].key] := OPTSHIFT [i].keysym
    END;
    FOR i := FIRST (OPTION) TO LAST (OPTION) DO
      OptionTable [OPTION [i].key] := OPTION [i].keysym
    END;
    Tables [State.E]     := CloneTable (OptionE);
    Tables [State.I]     := CloneTable (OptionI);
    Tables [State.N]     := CloneTable (OptionN);
    Tables [State.U]     := CloneTable (OptionU);
    Tables [State.Grave] := CloneTable (OptionGrave);
  END init;

PROCEDURE CloneTable (READONLY t: Table): REF Table =
  VAR u := NEW (REF Table, NUMBER (t));
  BEGIN
    u^ := t;
    RETURN u;
  END CloneTable;

BEGIN
  init ()
END MacModel.
