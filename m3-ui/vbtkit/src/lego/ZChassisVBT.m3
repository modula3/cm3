(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Aug 10 14:36:47 PDT 1994 by mhb                      *)
(*      modified on Fri Mar 12 10:37:56 PST 1993 by meehan                   *)
(*      modified on Tue Jun 16 13:08:00 PDT 1992 by muller                   *)
(*      modified on Fri Mar 27 02:57:32 1992 by steveg                       *)

MODULE ZChassisVBT;

IMPORT Axis, BorderedVBT, Filter, FlexVBT, Font, HVSplit,
       MultiClass, Shadow, ShadowedFeedbackVBT, ShadowedVBT,
       Split, StableVBT, SwitchVBT, TextVBT, TextureVBT, VBT,
       VBTClass, ZChildVBT, ZGrowVBT, ZMoveVBT, ZSplit,
       ZSplitUtils;

REVEAL
  T = Public BRANDED OBJECT
        holder: Filter.T;
      OVERRIDES
        init          := Init;
        initFromEdges := InitFromEdges;
        callback      := Callback;
      END;

TYPE
  MC = MultiClass.Filter OBJECT 
       OVERRIDES 
         replace := Replace; 
       END;

PROCEDURE Init (z       : T;
                ch      : VBT.T;
                title   : VBT.T;
                shadow  : Shadow.T := NIL;
                closable: BOOLEAN  := TRUE;
                open    : BOOLEAN  := TRUE;
                h, v               := 0.5;
                loc                := ZChildVBT.Location.Center;
                type               := ZChildVBT.CoordType.Scaled;
                shaper: ZSplit.ReshapeControl := NIL): T =
  VAR interior := CommonInit (z, ch, title, shadow, closable);
  BEGIN
    EVAL
      ZChildVBT.T.init (z, interior, h, v, loc, type, shaper, open);
    MultiClass.Be (z, NEW (MC));
    MultiClass.BeChild (z, ch);
    RETURN z
  END Init;

PROCEDURE InitFromEdges (v         : T;
                         ch        : VBT.T;
                         title     : VBT.T;
                         w, e, n, s: REAL;
                         shadow    : Shadow.T := NIL;
                         closable  : BOOLEAN  := TRUE;
                         open      : BOOLEAN  := TRUE;
                         type := ZChildVBT.CoordType.Absolute;
                         shaper: ZSplit.ReshapeControl := NIL): T =
  VAR interior := CommonInit (v, ch, title, shadow, closable);
  BEGIN
    EVAL ZChildVBT.T.initFromEdges (v, interior, 
           w, e, n, s, type, shaper, open);
    MultiClass.Be (v, NEW (MC));
    MultiClass.BeChild (v, ch);
    RETURN v
  END InitFromEdges;

CONST BARWIDTH = 0.3;

PROCEDURE CommonInit (v       : T;
                      ch      : VBT.T;
                      title   : VBT.T;
                      shadow  : Shadow.T := NIL;
                      closable: BOOLEAN  := TRUE ): VBT.T =
  VAR close, drag, stretchyDrag, grow, banner, box: VBT.T;
  BEGIN
    IF shadow = NIL THEN shadow := Shadow.None END;
    drag := NEW (ZMoveVBT.T).init (
              NEW (ShadowedFeedbackVBT.T).init (title, shadow));
    stretchyDrag := NEW (FlexVBT.T).init (drag, FlexVBT.Stretchy);
    grow := NEW (ZGrowVBT.T).init (NewBtn ("G", shadow));
    banner := HVSplit.New (Axis.T.Hor);
    IF closable THEN
      close :=
        NEW (SwitchVBT.T, callback := Close).init (NewBtn ("C", shadow));
      Split.AddChild (banner, close, VBar (shadow))
    END;
    Split.AddChild (banner, stretchyDrag, VBar (shadow), grow);
    v.holder := NEW (Filter.T).init (ch);
    box :=
      HVSplit.Cons (
        Axis.T.Ver, banner,
        FlexVBT.FromAxis (
          TextureVBT.New (shadow.fg), Axis.T.Ver, FlexVBT.RigidRange (BARWIDTH)),
        NEW (ShadowedVBT.T).init (v.holder, shadow, Shadow.Style.Raised));
    RETURN StableVBT.New (NEW (BorderedVBT.T).init (box, BARWIDTH, shadow.fg))
  END CommonInit;

PROCEDURE NewBtn (t: TEXT; shadow: Shadow.T): VBT.T =
  BEGIN
    WITH textVBT = TextVBT.New (t) DO
      TextVBT.SetFont (textVBT, Font.BuiltIn, shadow);
      RETURN NEW (ShadowedFeedbackVBT.T).init (textVBT, shadow)
    END
  END NewBtn;


PROCEDURE VBar (shadow: Shadow.T): VBT.T =
  BEGIN
    IF shadow.size # 0.0 THEN
      RETURN NIL
    ELSE
      RETURN FlexVBT.FromAxis (TextureVBT.New (shadow.bgFg), Axis.T.Hor,
                               FlexVBT.RigidRange (BARWIDTH))
    END
  END VBar;

PROCEDURE Close (v: VBT.T; READONLY cd: VBT.MouseRec) =
  VAR zch: T := ZSplitUtils.FindZChild (v);
  BEGIN
    IF zch # NIL THEN ZSplit.Unmap (zch); zch.callback (cd) END;
  END Close;

PROCEDURE Replace (m: MC; <* UNUSED *> ch: VBT.T; new: VBT.T) =
  VAR v: T := m.vbt;
  BEGIN
    EVAL Filter.Replace (v.holder, new)
  END Replace;

PROCEDURE Callback (<* UNUSED *>          v : T;
                    <* UNUSED *> READONLY cd: VBT.MouseRec) =
  BEGIN
  END Callback;

BEGIN
END ZChassisVBT.
