(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman and Stephen Harrison                                    *)
(* Last modified on Thu Jul 29 17:30:10 PDT 1993 by wobber   *)
(*      modified on Wed Jun 23 12:33:03 PDT 1993 by steveg   *)

MODULE MGRd;

IMPORT MText, MTextRd, PaintOp, Rd, RdClass, Rect,
       TextPort, Thread, VBT, VTDef, VText;

REVEAL
  T = Rd.T BRANDED OBJECT
        rd                   : MTextRd.T;
        past, present, future: VText.Interval;
        vbt: VBT.T;
      OVERRIDES
        seek   := Seek;
        length := Length;
        close  := Close;
      END;

PROCEDURE FromTextPort (         tp     : TextPort.T;
                        READONLY present: Style;
                        READONLY past                  := PastStyle;
                        READONLY future                := FutureStyle): T =
  VAR
    mtext   : MText.T;
    vbt     : VBT.T;
    rect    : Rect.T;
    vOptions: VText.VOptions;
    t       : T;
    pastIO := VText.MakeIntervalOptions(
                style := past.intervalStyle,
                whiteBlack :=
                  PaintOp.MakeColorScheme(past.bg, past.fg),
                whiteStroke :=
                  PaintOp.MakeColorScheme(past.bg, past.fg),
                leading := past.bg);
    presentIO := VText.MakeIntervalOptions(
                   style := present.intervalStyle,
                   whiteBlack :=
                     PaintOp.MakeColorScheme(present.bg, present.fg),
                   whiteStroke :=
                     PaintOp.MakeColorScheme(present.bg, present.fg),
                   leading := present.bg);
    futureIO := VText.MakeIntervalOptions(
                  style := future.intervalStyle,
                  whiteBlack :=
                    PaintOp.MakeColorScheme(future.bg, future.fg),
                  whiteStroke :=
                    PaintOp.MakeColorScheme(future.bg, future.fg),
                  leading := future.bg);
    port := TextPort.GetVText(tp);
  <* FATAL VTDef.Error, Rd.Failure, Thread.Alerted *>
  BEGIN
    VText.ExplodeVText(port, mtext, vbt, rect, vOptions);
    t := NEW(T, rd := MTextRd.New(mtext));
    t.vbt := vbt;
    t.past := VText.CreateInterval(port, 0, 0, pastIO);
    VText.SwitchInterval(t.past, VText.OnOffState.On);
    t.present := VText.CreateInterval(port, 0, 1, presentIO);
    VText.SwitchInterval(t.present, VText.OnOffState.On);
    t.future := VText.CreateInterval(port, 1, LAST(INTEGER), futureIO);
    VText.SwitchInterval(t.future, VText.OnOffState.On);
    VBT.Mark(vbt);
    t.buff := NEW(REF ARRAY OF CHAR, 1);
    t.closed := t.rd.closed;
    t.seekable := TRUE;
    t.intermittent := FALSE;
    t.st := 0;
    t.cur := 0;
    t.lo := 0;
    t.hi := 0;
    RETURN t;
  END FromTextPort;

PROCEDURE Length(t: T): INTEGER RAISES {Rd.Failure, Thread.Alerted} =
  BEGIN
    RETURN t.rd.length();
  END Length;

PROCEDURE Close(t: T) RAISES {Rd.Failure, Thread.Alerted} =
  <* FATAL VTDef.Error *>
  BEGIN
    t.rd.close();
    t.closed := TRUE;
    VText.SwitchInterval(t.past, VText.OnOffState.Off);
    VText.SwitchInterval(t.present, VText.OnOffState.Off);
    VText.SwitchInterval(t.future, VText.OnOffState.Off);
    VText.DeleteInterval(t.past);
    VText.DeleteInterval(t.present);
    VText.DeleteInterval(t.future);
  END Close;

PROCEDURE Seek (t: T; n: CARDINAL;
            <* UNUSED *> dontBlock: BOOLEAN): RdClass.SeekResult
  RAISES {Rd.Failure, Thread.Alerted} =
  <* FATAL VTDef.Error *>
  BEGIN
    TRY
      Rd.Seek(t.rd, n);
      VText.MoveInterval(t.past, 0, n);
      VText.MoveInterval(t.present, n, n + 1);
      VText.MoveInterval(t.future, n + 1, LAST(INTEGER));
      VBT.Mark(t.vbt);
      t.cur := n;
      t.lo := n;
      t.hi := n + 1;
      t.buff[0] := Rd.GetChar(t.rd);
    EXCEPT
      Rd.EndOfFile => RETURN RdClass.SeekResult.Eof
    END;
    RETURN RdClass.SeekResult.Ready;
  END Seek;

BEGIN
END MGRd.
