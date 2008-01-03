(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sun Mar 21 16:56:59 PST 1993 by meehan                   *)
(*      modified on Tue Feb 16 23:52:50 PST 1993 by mhb                      *)
<* PRAGMA LL *>

MODULE TypeinVBT;

IMPORT Axis, Font, PaintOp, Palette, Rd, Rect, TextPort, TextPortClass,
       Thread, VBT, VBTClass, VTDef, VText;

REVEAL
  T = Public BRANDED OBJECT
        expandOnDemand: BOOLEAN;
        nextTab       : VBT.T
      OVERRIDES
        init         := Init;
        key          := Key;
        returnAction := ReturnAction;
        shape        := Shape;
        tabAction    := TabAction;
      END;

PROCEDURE Init (v               : T;
                expandOnDemand                        := FALSE;
                hMargin, vMargin                      := 0.5;
                font                                  := Font.BuiltIn;
                colorScheme     : PaintOp.ColorScheme := NIL;
                wrap                                  := TRUE;
                readOnly                              := FALSE;
                turnMargin                            := 0.5;
                model := TextPort.Model.Default): T =
  BEGIN
    v.expandOnDemand := expandOnDemand;
    RETURN
      TextPort.T.init (v, hMargin, vMargin, font, colorScheme,
                       wrap AND expandOnDemand, readOnly, turnMargin, model)
  END Init;

PROCEDURE ReturnAction (<* UNUSED *>          v : T;
                        <* UNUSED *> READONLY cd: VBT.KeyRec) =
  BEGIN
  END ReturnAction;

PROCEDURE TabAction (v: T; READONLY event: VBT.KeyRec) =
  BEGIN
    IF v.tabNext # NIL THEN
      VBTClass.Misc (
        v.tabNext, VBT.MiscRec {VBT.TakeSelection, VBT.NullDetail, event.time,
                                VBT.KBFocus});
      TextPort.Select (v, event.time, LAST (CARDINAL));
      TYPECASE v.tabNext OF
      | TextPort.T (vbt) =>
          TextPort.Select (
            vbt, event.time, 0, LAST (CARDINAL), replaceMode := TRUE)
      ELSE
      END
    ELSE
      TextPort.T.tabAction (v, event)
    END
  END TabAction;

PROCEDURE Shape (v: T; ax: Axis.T; n: CARDINAL): VBT.SizeRange =
  <* LL = VBT.mu.SELF *>
  CONST name = "Shape";
  TYPE Pixels = CARDINAL;
  VAR
    marginSlack : Pixels;
    hpref, vpref: Pixels;
    lines       : CARDINAL := 1;
    st                     := VBT.ScreenTypeOf (v);
  BEGIN
    IF st = NIL THEN RETURN VBT.DefaultShape END;
    LOCK v.mu DO
      IF v.fontHeight = 0 THEN   (* ScreenType just became non-NIL *)
        WITH bounds = Palette.ResolveFont (st, v.font).metrics.maxBounds DO
          v.fontHeight := Rect.VerSize (bounds.boundingBox);
          v.charWidth := bounds.printWidth
        END
      END;
      marginSlack :=
        v.vtext.leftMargin + v.vtext.rightMargin + 2 * v.vtext.turnMargin;
      hpref := 30 * v.charWidth + marginSlack;
      IF ax = Axis.T.Hor THEN
        RETURN
          VBT.SizeRange {0, hpref, ROUND (VBT.MMToPixels (v, 99999.0, ax))}
      ELSE                       (* ax = Axis.T.Ver *)
        IF v.lastNonEmptyWidth # 0 THEN hpref := v.lastNonEmptyWidth END;
        IF n = 0 THEN n := hpref END;
        v.lastNonEmptyWidth := n;
        IF NOT v.expandOnDemand THEN
          vpref := v.fontHeight + 2 * v.vtext.topMargin
        ELSE
          (* How many lines would it take to display the whole vtext?  Make
             sure there is room for at least one line (vertically) or vtext
             gets very confused.  If width (res.pref) = 0, then the vtext has
             not been reshaped yet *)
          IF n # 0 THEN
            TRY
              lines := 1 + VText.LinesBetween (
                             v.vtext, 0, LAST (CARDINAL), LAST (CARDINAL),
                             (* fudge n appropriately *)
                             n - marginSlack)
            EXCEPT
            | VTDef.Error (ec) => v.vterror (name, ec)
            | Rd.EndOfFile => v.rdeoferror (name)
            | Rd.Failure (ref) => v.rdfailure (name, ref)
            | Thread.Alerted =>
            END
          END;
          (* How many pixels is that? *)
          vpref := lines * v.vtext.lineSpacing + 2 * v.vtext.topMargin
        END
      END;
      (* v.vtext.lineSpacing is not guaranteed to be reasonable, so we need
         some defensive code. *)
      IF vpref > VBT.DefaultShape.hi THEN
        RETURN VBT.DefaultShape
      ELSE
        RETURN VBT.SizeRange {vpref, vpref, vpref + 1}
      END
    END
  END Shape;

PROCEDURE Key (v: T; READONLY cd: VBT.KeyRec) =
  BEGIN
    IF NOT v.expandOnDemand THEN
      TextPort.T.key(v, cd)
    ELSE
      WITH oldVsizeRange = VBTClass.GetShape(
                             v, Axis.T.Ver, v.lastNonEmptyWidth,
                             FALSE) DO
        TextPort.T.key(v, cd);
        IF VBTClass.GetShape(
             v, Axis.T.Ver, v.lastNonEmptyWidth, FALSE)
             # oldVsizeRange THEN
          (* Scroll back to the top, so we can see the whole
             text. *)
          TextPort.Normalize(v, 0);
          VBT.NewShape(v)
        END
      END
    END
  END Key;

BEGIN END TypeinVBT.
