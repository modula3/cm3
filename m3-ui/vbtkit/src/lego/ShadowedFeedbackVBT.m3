(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sat Feb  6 18:24:56 PST 1993 by meehan *)
(*      modified on Sat Feb  6 10:48:39 PST 1993 by mhb    *)
(*      modified on Tue Jun 16 13:08:16 PDT 1992 by muller *)
(* modified on Thu Sep 12 10:59:15 PDT 1991 by steveg *)

MODULE ShadowedFeedbackVBT;

IMPORT FeedbackVBT, Filter, HighlightVBT, MultiClass,
       MultiFilter, Pixmap, Point, Rect, Shadow, ShadowedVBT, VBT;

REVEAL
  T = Public BRANDED OBJECT
        shadow        : Shadow.T;
        highlighter   : HighlightVBT.T;
        normalStyles  : ARRAY BOOLEAN OF Shadow.Style;
        excitedStyles : ARRAY BOOLEAN OF Shadow.Style
      OVERRIDES
        init    := Init;
        normal  := Normal;
        excited := Excited;
        reshape := Reshape;
      END;

TYPE
  MC = MultiClass.Filter BRANDED OBJECT
       OVERRIDES
         succ    := Succ;
         pred    := Succ;
         replace := Replace;
       END;

(* Bug: if Shadow.Supported(v, v.shadow) changes on the fly,
   such as during a rescreening, the looks will be wrong because
   there is nothing to tickle the HighlightVBT to change. *)

PROCEDURE Init (v      : T;
                ch     : VBT.T;
                shadow : Shadow.T := NIL;
                onStyle           := Shadow.Style.Lowered;
                onExcitedStyle  := Shadow.Style.Raised;
                offStyle        := Shadow.Style.Raised;
                offExcitedStyle := Shadow.Style.Lowered ): T =
  BEGIN
    IF shadow = NIL THEN shadow := Shadow.None END;
    EVAL FeedbackVBT.T.init(
           v, NEW (ShadowedVBT.T).init (ch, shadow, offStyle));
    v.shadow := shadow;
    v.normalStyles[TRUE] := onStyle;
    v.normalStyles[FALSE] := offStyle;
    v.excitedStyles[TRUE] := onExcitedStyle;
    v.excitedStyles[FALSE] := offExcitedStyle;
    MultiClass.Be(v, NEW(MC));
    IF ch # NIL THEN MultiClass.BeChild(v, ch) END;
    RETURN v
  END Init;

PROCEDURE Reshape (v: T; READONLY cd: VBT.ReshapeRec) =
  VAR ch: VBT.T;
  BEGIN
    FeedbackVBT.T.reshape(v, cd);
    ch := MultiFilter.Child(v);
    IF ch # NIL AND v.highlighter # NIL THEN
      SetHighlighter(v, VBT.Domain(ch), 99999)
    END
  END Reshape;

PROCEDURE Normal (v: T) =
  VAR state := FeedbackVBT.GetState(v);
  BEGIN
    ShadowedVBT.SetStyle(Filter.Child(v), v.normalStyles[state]);
    Highlight(v, state)
  END Normal;

PROCEDURE Excited (v: T) =
  VAR state := FeedbackVBT.GetState(v);
  BEGIN
    ShadowedVBT.SetStyle(Filter.Child(v), v.excitedStyles[state]);
    Highlight(v, NOT state)
  END Excited;

PROCEDURE Highlight (v: T; excite: BOOLEAN) =
  VAR ch := MultiFilter.Child(v);
  BEGIN
    IF excite THEN
      v.highlighter := HighlightVBT.Find(v);
      IF ch # NIL THEN
        SetHighlighter(v, VBT.Domain(ch), 99999)
      END
    ELSE
      SetHighlighter(v, Rect.Empty, 0);
      v.highlighter := NIL
    END
  END Highlight;

PROCEDURE SetHighlighter (v: T; READONLY r: Rect.T; sz: CARDINAL) =
  BEGIN
    IF NOT Shadow.Supported(v.shadow, v) THEN
      HighlightVBT.SetTexture(
        v.highlighter, Pixmap.Solid, Point.Origin, v.shadow.swap);
      HighlightVBT.SetRect(v.highlighter, r, sz)
    END
  END SetHighlighter;

PROCEDURE NewMenu (ch: VBT.T; shadow: Shadow.T := NIL): T =
  BEGIN
    RETURN
      NEW (T).init (ch, shadow, Shadow.Style.Flat, Shadow.Style.Lowered,
                    Shadow.Style.Flat, Shadow.Style.Lowered);
  END NewMenu;

PROCEDURE Replace (m: MC; <* UNUSED *> ch: VBT.T; new: VBT.T) =
  VAR sh := Filter.Child(m.vbt);
  BEGIN
    EVAL Filter.Replace(sh, new)
  END Replace;

PROCEDURE Succ (m: MC; ch: VBT.T): VBT.T =
  VAR sh := Filter.Child(m.vbt);
  BEGIN
    IF ch = NIL THEN RETURN Filter.Child(sh) ELSE RETURN NIL END
  END Succ;

BEGIN
END ShadowedFeedbackVBT.
