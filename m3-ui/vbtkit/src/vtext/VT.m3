(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified On Mon Dec 21 18:36:10 PST 1992 by meehan                   *)
(*      modified On Tue Jun 16 13:12:47 PDT 1992 by muller                   *)
(*      modified On Tue May 15 17:01:26 PDT 1990 by mcjones                  *)
(*      modified On Fri Dec 4 02:41:30 1987 by jdd                           *)
<* PRAGMA LL *>

MODULE VT;

IMPORT VTInterval, VTMarker, VTView, VTVirtual, VTCaret, VTTexture, Rd,
         Thread, MText, VTDef;

FROM VTDef IMPORT Interval, Marker, T, View;

PROCEDURE New (mtext: MText.T): T RAISES {VTDef.Error} =
  VAR vt: T;
  BEGIN
    IF mtext = NIL THEN RAISE VTDef.Error (VTDef.ErrorCode.IsNil) END;
    vt := NEW (T);
    vt.mutex := NEW (MUTEX);
    vt.closed := FALSE;
    vt.mtext := mtext;
    vt.length := MText.Length (mtext);
    vt.intervals := NIL;
    VTCaret.Init (vt);
    vt.rdDirty := TRUE;
    vt.views := NIL;
    VTTexture.Init ();
    RETURN vt
  END New;

PROCEDURE Replace (vt: T; begin, end: CARDINAL; text: TEXT)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR oldLength: CARDINAL;
  BEGIN
    oldLength := vt.length;
    MText.Replace (vt.mtext, begin, end, text);
    Invalidate (vt, begin, end,
                MText.Length (vt.mtext) - (oldLength - (end - begin)))
  END Replace;


PROCEDURE ReplaceChars (         vt        : T;
                                 begin, end: CARDINAL;
                        READONLY str       : ARRAY OF CHAR)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR oldLength: CARDINAL;
  BEGIN
    oldLength := vt.length;
    MText.ReplaceChars (vt.mtext, begin, end, str);
    Invalidate (
      vt, begin, end, MText.Length (vt.mtext) - (oldLength - (end - begin)))
  END ReplaceChars;


PROCEDURE ReplaceFile (vt        : T;
                       begin, end: CARDINAL;
                       file      : Rd.T;
                       start     : CARDINAL          := 0;
                       numChars  : CARDINAL          := LAST (CARDINAL))
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR oldLength: CARDINAL;
  BEGIN
    oldLength := vt.length;
    MText.ReplaceFile (vt.mtext, begin, end, file, start, numChars);
    Invalidate (vt, begin, end,
                MText.Length (vt.mtext) - (oldLength - (end - begin)))
  END ReplaceFile;


PROCEDURE Close (vt: T) = <* LL = vt.mutex *>
  VAR
    v0, v1: View;
    i0, i1: Interval;
    m0, m1: Marker;
  BEGIN
    vt.closed := TRUE;
    VTCaret.Close (vt);
    v0 := vt.views;
    WHILE v0 # NIL DO v1 := v0.next; VTView.Close (v0); v0 := v1 END;
    i0 := vt.intervals;
    WHILE i0 # NIL DO i1 := i0.next; VTInterval.Close (i0); i0 := i1 END;
    m0 := vt.markers;
    WHILE m0 # NIL DO m1 := m0.next; VTMarker.Close (m0); m0 := m1 END;
    vt.mtext := NIL;
    <* ASSERT (vt.intervals = NIL) *>
    <* ASSERT (vt.markers = NIL) *>
    vt.caret.blinker := NIL;
    (* vt.rd.instance := NIL; vt.rrd.instance := NIL; *)
    <* ASSERT (vt.views = NIL) *>
  END Close;

PROCEDURE Invalidate (vt: T; b, e, l: CARDINAL)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR
    interval: Interval;
    marker  : Marker;
  BEGIN
    vt.rdDirty := TRUE;
    VTVirtual.Change (vt, b, e, b + l);
    (* update caret *)
    IF e <= vt.caret.index THEN
      vt.caret.index := vt.caret.index + (l - (e - b))
    ELSIF b <= vt.caret.index THEN
      vt.caret.index := b + l
    END;
    (* update intervals *)
    interval := vt.intervals;
    WHILE interval # NIL DO
      IF e <= interval.l THEN
        interval.l := interval.l + (l - (e - b))
      ELSIF b <= interval.l THEN
        interval.l := b + l
      END;
      IF e <= interval.r THEN
        interval.r := interval.r + (l - (e - b))
      ELSIF b <= interval.r THEN
        interval.r := b + l
      END;
      interval := interval.next
    END;
    (* update markers *)
    marker := vt.markers;
    WHILE marker # NIL DO
      IF e <= marker.index THEN
        marker.index := marker.index + (l - (e - b))
      ELSIF b <= marker.index THEN
        marker.index := b + l
      END;
      marker := marker.next
    END
  END Invalidate;

BEGIN
END VT.
