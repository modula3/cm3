(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Mar 27 19:33:58 PST 1996 by heydon                   *)
(*      modified on Wed Aug 16 10:03:21 PST 1995 by gnelson                  *)
<* PRAGMA LL *>
<* PRAGMA SPEC *>

MODULE JunoError;

IMPORT JunoRsrc, JunoConfig;
IMPORT TextPort;
IMPORT Trestle, ButtonVBT, TextVBT, Rect, Point, PaintOp, VBT, Axis;
IMPORT   BorderedVBT, VBTClass, Filter, Split, HVSplit;
FROM TrestleComm IMPORT Failure;
IMPORT Rd, Wr, Thread, Rsrc, Text;

<* FATAL Failure, Thread.Alerted *>

PROCEDURE P(
    tp: TextPort.T;
    errmsg: TEXT;
    start, finish: INTEGER := -1;
    time: VBT.TimeStamp := 0) =
  BEGIN
    IF start # -1 AND finish # -1 THEN
      TextPort.Select(tp, time, start, finish, replaceMode := TRUE);
      TextPort.Normalize(tp, start);
      EVAL TextPort.TryFocus(tp, time)
    END;
    Display(tp, errmsg)
  END P;

VAR
  errorOp := PaintOp.MakeColorQuad(PaintOp.Bg,
    PaintOp.FromRGB(1.0, 0.0, 0.0));
  filter := BorderedVBT.New(NEW(VBT.Leaf),
              size := 2.0, op := PaintOp.Bg);
  dialog := ButtonVBT.New(BorderedVBT.New(filter), DoDismiss);

PROCEDURE MsgVBT(errmsg: TEXT): VBT.T =
  VAR
    res := NEW(HVSplit.T).init(Axis.T.Ver);
    i := 0; txt := errmsg & "\n";
  BEGIN
    WHILE i # Text.Length(txt) DO
      VAR j := Text.FindChar(txt, '\n', start := i); BEGIN
        <* ASSERT j # -1 *>
        Split.AddChild(res, NEW(TextVBT.T).init(Text.Sub(txt, i, j-i),
          fnt := JunoConfig.textFont, bgFg := errorOp));
        i := j + 1
      END
    END;
    RETURN res
  END MsgVBT;

<* SPEC Display REQUIRES sup(LL) = VBT.mu *>
 
PROCEDURE Display(v: VBT.T; errmsg: TEXT) =
  BEGIN
    EVAL Filter.Replace(filter, MsgVBT(errmsg));
    VAR pt := Rect.Middle(VBT.Domain(v)); BEGIN
      IF Trestle.ScreenOf(dialog, Point.Origin).trsl = NIL THEN
        Trestle.Attach(dialog)
      END;
      VBTClass.Rescreen(dialog, VBT.ScreenTypeOf(v));
      VAR
        width := dialog.shape(Axis.T.Hor, 0).pref;
        height := dialog.shape(Axis.T.Ver, 0).pref;
      BEGIN
        pt := Point.MoveV(pt, Rect.VerSize(VBT.Domain(v)) DIV 4);
        pt := Point.MoveHV(pt, -(width DIV 2), -(height DIV 2));
        VAR scrn := Trestle.ScreenOf(v, pt); BEGIN
          IF scrn.trsl # NIL THEN
            Trestle.Overlap(dialog, scrn.id, scrn.q);
            Trestle.MoveNear(dialog, NIL)
          END
        END
      END
    END
  END Display;

PROCEDURE DisplayPS(wr: Wr.T; errmsg: TEXT) RAISES {Wr.Failure} =
  BEGIN
    Wr.PutText(wr, "/str (" & errmsg & ") def\n");
    <* FATAL Rd.Failure, Rd.EndOfFile, Thread.Alerted, Rsrc.NotFound *>
    VAR rd: Rd.T := Rsrc.Open("showerror.ps", JunoRsrc.Path); BEGIN
      (* copy PostScript code to "wr" *)
      WHILE NOT Rd.EOF(rd) DO Wr.PutChar(wr, Rd.GetChar(rd)) END;
      Rd.Close(rd)
    END
  END DisplayPS;
   
PROCEDURE DoDismiss(
    <* UNUSED *> ch: ButtonVBT.T;
    <* UNUSED *> READONLY cd: VBT.MouseRec) =
  BEGIN
    Trestle.Delete(dialog)
  END DoDismiss;
    
BEGIN
END JunoError.
