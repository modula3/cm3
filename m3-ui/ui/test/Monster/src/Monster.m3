(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman, Mark Manasse and Greg Nelson *)
(* Last modified on Sat Oct 30 12:17:43 PDT 1993 by sfreeman *)
(*      modified on Mon Feb 24 14:01:23 PST 1992 by muller *)
(* modified on Tue Nov 19 0:36:03 PST 1991 by gnelson *)
(* modified on Wed Sep 11 15:55:19 PDT 1991 by msm *)
<*PRAGMA LL*>

MODULE Monster EXPORTS Main;
IMPORT TextVBT, Trestle, HVSplit, Axis, HVBar, Fmt, VBT, HighlightVBT,
       BorderedVBT, Pixmap, PaintOp;

<*FATAL ANY*>

PROCEDURE New (lo, hi: INTEGER; hv: Axis.T): VBT.T =
  BEGIN
    IF hi - lo = 1 THEN
      RETURN BorderedVBT.New(TextVBT.New(Fmt.Int(lo)))
    ELSE
      WITH vh  = Axis.Other[hv],
           mid = (lo + hi) DIV 2 DO
        RETURN
          HVSplit.Cons(hv, New(lo, mid, vh), HVBar.New(), New(mid, hi, vh))
      END
    END
  END New;

VAR
  count := 256;
  v := BorderedVBT.New(HighlightVBT.New(New(0, count, Axis.T.Hor)),
                       BorderedVBT.Default, PaintOp.BgFg, Pixmap.Gray);

BEGIN
  Trestle.Install(v);
  Trestle.AwaitDelete(v)
END Monster.
