(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Feb 24 14:01:28 PST 1992 by muller   *)
(*      modified on Tue Nov 19  0:50:58 PST 1991 by gnelson  *)
(*      modified on Wed Sep 11 15:54:23 PDT 1991 by msm      *)
<*PRAGMA LL*>

MODULE ButtonTest EXPORTS Main;

IMPORT ButtonVBT, VBT, TextVBT, Filter, ZSplit, TextureVBT, 
  QuickBtnVBT, AnchorBtnVBT, BorderedVBT, HVSplit, MenuBtnVBT, 
  Trestle, PaintOp, Axis, Text, HighlightVBT, Pixmap;

<*FATAL ANY*>

PROCEDURE Flip(b: ButtonVBT.T; <*UNUSED*> READONLY cd: VBT.MouseRec) RAISES {}=
  BEGIN
    WITH ch = Filter.Child(b), txt = TextVBT.Get(ch) DO
      IF Text.Equal(txt, "Alpha") THEN
        TextVBT.Put(ch, "Beta")
      ELSE
        TextVBT.Put(ch, "Alpha")
      END
    END
  END Flip;
  
VAR
 menuBar := ButtonVBT.MenuBar(
   ButtonVBT.New(TextVBT.New("Alpha"), Flip),
   QuickBtnVBT.New(TextVBT.New("Alpha"), Flip),
   AnchorBtnVBT.New(
     ch := TextVBT.New("Anchor"),
     menu := BorderedVBT.New(
               HVSplit.Cons(Axis.T.Ver,
                 MenuBtnVBT.TextItem("Alpha", Flip),
                 MenuBtnVBT.TextItem("Alpha", Flip))), n := 9999),
   AnchorBtnVBT.New(
     ch := TextVBT.New("Anchor"),
     menu := BorderedVBT.New(
               HVSplit.Cons(Axis.T.Ver,
                 MenuBtnVBT.TextItem("Alpha", Flip),
                 MenuBtnVBT.TextItem("Alpha", Flip)))),
   AnchorBtnVBT.New(
     ch := TextVBT.New("Anchor"),
     menu := BorderedVBT.New(
               HVSplit.Cons(Axis.T.Ver,
                 MenuBtnVBT.TextItem("Alpha", Flip),
                 MenuBtnVBT.TextItem("Alpha", Flip)))));
 v := HighlightVBT.New(ZSplit.New(
       HVSplit.Cons(Axis.T.Ver, menuBar, TextureVBT.New(PaintOp.BgFg, Pixmap.Gray))));

BEGIN
  Trestle.Install(v);
  Trestle.AwaitDelete(v)
END ButtonTest.
