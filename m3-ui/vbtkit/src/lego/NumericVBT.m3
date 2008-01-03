(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Sep 28 20:39:55 PDT 1995 by mhb                      *)
(*      modified on Fri Feb 18 18:05:51 PST 1994 by kalsow                   *)
(*      modified on Sun Mar 21 17:24:44 PST 1993 by meehan                   *)
(*      modified on Tue Jun 16 13:08:31 PDT 1992 by muller                   *)
(*      modified on Fri Mar 20 22:43:50 1992 by steveg                       *)
<* PRAGMA LL *>

MODULE NumericVBT;

IMPORT Axis, AnyEvent, Filter, Font, FlexVBT, FloatMode, Fmt, HVSplit, Lex,
       Pixmap, PixmapVBT, Rd, Rect, Shadow, ShadowedFeedbackVBT, ShadowedVBT,
       Text, TextPort, TextPortClass, TextRd, TextureVBT, Thread,
       TrillSwitchVBT, TypeinVBT, VBT, VBTKitResources, VText;

REVEAL
  T = Public BRANDED OBJECT
        (* create-time options: *)
        allowEmpty: BOOLEAN;
        (* changable options: *)
        min, max: INTEGER;
        (* current state: *)
        val   : INTEGER;
        digits: CARDINAL := 0;
        empty : BOOLEAN;
      OVERRIDES
        init     := Init;
        callback := Callback;
      END;
  Typein = TypeinVBT.T BRANDED OBJECT
             v: T
           OVERRIDES
             returnAction := ReturnAction;
             reshape      := Reshape
           END;

TYPE
  State = RECORD
            num  : INTEGER;
            empty: BOOLEAN
          END;

PROCEDURE Init (v         : T;
                min       : INTEGER  := FIRST (INTEGER);
                max       : INTEGER  := LAST (INTEGER);
                allowEmpty: BOOLEAN  := FALSE;
                naked     : BOOLEAN  := FALSE;
                font      : Font.T   := Font.BuiltIn;
                shadow    : Shadow.T := NIL              ): T =
  VAR hsplit, minus, plus: VBT.T;
  BEGIN
    IF shadow = NIL THEN shadow := Shadow.None END;
    GetResources ();
    max := MAX (min, max);
    v.allowEmpty := allowEmpty;
    v.min := min;
    v.max := max;
    IF v.typein = NIL THEN v.typein := NEW (Typein) END;
    v.typein := v.typein.init (FALSE, 0.5, 0.5, font, shadow, wrap := FALSE);
    v.typein.v := v;
    IF min <= 0 AND 0 <= max THEN
      PutCl (v, State {0, allowEmpty})
    ELSE
      PutCl (v, State {min, allowEmpty})
    END;
    IF naked THEN
      EVAL Filter.T.init (v, NEW (ShadowedVBT.T).init (
                               v.typein, shadow, Shadow.Style.Lowered));
    ELSE
      minus := NewPlusMinusVBT (v, -1, shadow, minusOff);
      plus := NewPlusMinusVBT (v, 1, shadow, plusOff);
      hsplit :=
        FlexVBT.FromAxis (
          HVSplit.Cons (
            Axis.T.Hor, minus, VBar (shadow),
            NEW (ShadowedVBT.T).init (v.typein, shadow, Shadow.Style.Raised),
            VBar (shadow), plus),
          Axis.T.Hor, FlexVBT.RigidRange (25.0));
      EVAL Filter.T.init (v, hsplit);
    END;
    RETURN v;
  END Init;


PROCEDURE Callback (<* UNUSED *> v: T; <* UNUSED *> event: AnyEvent.T) =
  BEGIN
  END Callback;

PROCEDURE VBar (shadow: Shadow.T): VBT.T =
  BEGIN
    IF shadow.size # 0.0 THEN 
      RETURN NIL
    ELSE 
      RETURN FlexVBT.FromAxis(TextureVBT.New(shadow.bgFg), 
                                Axis.T.Hor, FlexVBT.RigidRange(1.0))
    END
  END VBar;

PROCEDURE ReturnAction (typein: Typein; READONLY cd: VBT.KeyRec) =
  VAR v := typein.v;
  BEGIN
    PutCl (v, ReadState (v));
    v.callback (AnyEvent.FromKey (cd))
  END ReturnAction;

PROCEDURE Reshape (typein: Typein; READONLY cd: VBT.ReshapeRec) =
  TYPE Pixels = CARDINAL;

  BEGIN
    TypeinVBT.T.reshape (typein, cd);
    IF cd.new = Rect.Empty THEN RETURN END;
    LOCK typein.mu DO
      VAR
        width: Pixels  := Rect.HorSize (VBT.Domain (typein));
        vtext: VText.T := typein.vtext;
        marginSlack: Pixels := vtext.leftMargin + vtext.rightMargin
                                 + 2 * vtext.turnMargin;
      BEGIN
        IF typein.charWidth > 0 AND width > marginSlack THEN
          typein.v.digits := (width - marginSlack) DIV typein.charWidth
        ELSE
          typein.v.digits := 0
        END
      END
    END;
    PutCl (typein.v, ReadState (typein.v))
  END Reshape;

PROCEDURE CheckAndFixValue (v: T) =
  VAR s := ReadState (v);
  BEGIN
    IF s.empty THEN
      IF v.allowEmpty THEN
        v.val := FIRST (INTEGER);
        v.empty := TRUE
      ELSE
        PutCl (v, s)
      END
    ELSIF s.num < v.min OR s.num > v.max THEN
      PutCl (v, s)
    ELSE
      v.val := s.num;
      v.empty := FALSE
    END
  END CheckAndFixValue;

PROCEDURE ReadState (v: T): State =
  VAR contents := TextPort.GetText (v.typein);
  BEGIN
    IF Text.Empty (contents) THEN
      IF v.allowEmpty THEN
        RETURN State {FIRST (INTEGER), TRUE}
      ELSE
        RETURN State {0, FALSE}
      END
    ELSE
      TRY
        (* RETURN State {Scan.Int (StripLeadingBlanks (contents)), FALSE} *)
        RETURN State {Lex.Int (TextRd.New (contents)), FALSE}
      EXCEPT
      | Lex.Error, FloatMode.Trap, Rd.Failure, Thread.Alerted =>
          (* We may have all kinds of illegal characters -- through the
             primary/secondary replacement mechanism, for example.  So we must
             be careful. *)
          RETURN State {v.val, FALSE}
      END
    END
  END ReadState;

(********
PROCEDURE StripLeadingBlanks (t: TEXT): TEXT =
  BEGIN
    FOR i := 0 TO Text.Length (t) - 1 DO
      IF Text.GetChar (t, i) = ' ' THEN (* skip *)
      ELSIF i = 0 THEN
        RETURN t
      ELSE
        RETURN Text.Sub (t, i, LAST (CARDINAL))
      END
    END;
    RETURN ""
  END StripLeadingBlanks;
*************)
  
TYPE
  PlusMinusVBT = TrillSwitchVBT.T BRANDED OBJECT
                   v    : T;
                   delta: INTEGER;
                 OVERRIDES
                   callback := PlusMinus
                 END;

PROCEDURE NewPlusMinusVBT (v       : T;
                           delta   : INTEGER;
                           shadow  : Shadow.T;
                           contents: Pixmap.T  ): PlusMinusVBT =
  VAR
    p := NEW (PixmapVBT.T).init (contents, op:=shadow.bgFg, bg:=shadow.bg);
    f := NEW (ShadowedFeedbackVBT.T).init (p, shadow);
    pm: PlusMinusVBT := NEW (PlusMinusVBT).init (f);
  BEGIN
    pm.v := v;
    pm.delta := delta;
    RETURN pm;
  END NewPlusMinusVBT;

PROCEDURE PlusMinus (pm: PlusMinusVBT; READONLY cd: VBT.MouseRec) =
  VAR v := pm.v;
  BEGIN
    CheckAndFixValue (v);
    IF v.empty THEN RETURN END;
    PutCl (v, State {v.val + pm.delta, FALSE});
    v.callback (AnyEvent.FromMouse (cd))
  END PlusMinus;

PROCEDURE Put (v: T; n: INTEGER) =
  BEGIN
    PutCl (v, State {n, FALSE});
  END Put;

PROCEDURE PutBounds (v: T; min, max: INTEGER) =
  BEGIN
    v.min := min;
    v.max := max;
    PutCl (v, State {v.val, FALSE});
  END PutBounds;

PROCEDURE SetEmpty (v: T) =
  BEGIN
    IF v.allowEmpty THEN PutCl (v, State {0, TRUE}) END;
  END SetEmpty;

PROCEDURE PutCl (v: T; READONLY s: State) =
  BEGIN
    IF s.empty AND v.allowEmpty THEN
      v.empty := TRUE;
      v.val := FIRST (INTEGER);
      TextPort.SetText (v.typein, "")
    ELSE
      v.empty := FALSE;
      v.val := MIN (v.max, MAX (v.min, s.num));
      TextPort.SetText (v.typein, Fmt.Pad (Fmt.Int (v.val), v.digits))
    END
  END PutCl;

PROCEDURE Get (v: T): INTEGER =
  BEGIN
    CheckAndFixValue(v);
    RETURN v.val;
  END Get;

PROCEDURE GetMin (v: T): INTEGER =
  BEGIN
    CheckAndFixValue(v);
    RETURN v.min;
  END GetMin;

PROCEDURE GetMax (v: T): INTEGER =
  BEGIN
    CheckAndFixValue(v);
    RETURN v.max;
  END GetMax;

PROCEDURE IsEmpty (v: T): BOOLEAN =
  BEGIN
    CheckAndFixValue(v);
    RETURN v.empty;
  END IsEmpty;

PROCEDURE TakeFocus (v: T; time: VBT.TimeStamp; alsoSelect: BOOLEAN := TRUE):
  BOOLEAN =
  BEGIN
    IF NOT TextPort.TryFocus (v.typein, time) THEN RETURN FALSE END;
    IF alsoSelect THEN
      TextPort.Select (
        v.typein, time, 0, LAST (CARDINAL), replaceMode := TRUE);
    END;
    RETURN TRUE
  END TakeFocus;

VAR
  rsrcMu := NEW (MUTEX);
  <* LL = rsrcMu *>
  rsrcInit                    := FALSE;

VAR minusOff, plusOff: Pixmap.T;

PROCEDURE GetResources () =
  BEGIN
    LOCK rsrcMu DO
      IF rsrcInit THEN RETURN END;
      minusOff := VBTKitResources.GetPixmap ("minusOff");
      plusOff := VBTKitResources.GetPixmap ("plusOff");
      rsrcInit := TRUE;
    END
  END GetResources;

BEGIN
END NumericVBT.
