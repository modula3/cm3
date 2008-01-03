(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified On Tue Jun 16 13:12:34 PDT 1992 by muller *)
(*      modified On Thu Mar 19 11:24:09 PST 1992 by jdd    *)
(*      modified On Sun Nov 24 18:37:16 PST 1991 by meehan *)
(*      Modified On Tue May 15 17:31:03 PDT 1990 by mcjones *)


(* This module maintains the "virtual" screen structures. The following
   invariants are maintained:

   Up(start.at, 0) = start.at, with secondary results start.min, start.max and
   start.turned. (start.max may be conservative.) If vt^.length > 0, then
   start.at < vt^.length. Changing the buffer keeps start.at valid.

   If line[i].virtualLine.valid, then ComputeLine(line[i].virtualLine.from) =
   line[i].virtualLine.to, with secondary results line[i].virtualLine.max,
   line[i].virtualLine.width, and line[i].virtualLine.turned.

   dirty is true if any line[i].virtualLine.valid is false, or if the
   information is otherwise obsolete. Otherwise, then the line[i] are all in
   order and contiguous, starting at start.at.

   If only the start of the buffer is dirty, then bodyDirty will be FALSE
   and dirty will be TRUE.

   line[0] through line[nLines-1] are defined, as is
   line[nLines].virtualLine.from. When dirty is false, "lines" is the number
   of non-empty elements (an element may be empty if it is at the end of the
   buffer, or if the next character will not fit within the lineWidth). *)

MODULE VTVirtual;

IMPORT Rd, Thread;
IMPORT VTDef, VTRd, VTReal, VTBase;

TYPE
  LineNo = VTDef.LineNo;

PROCEDURE Change (vt: T; begin, oEnd, nEnd: I)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  (* Change notes a change made in the mtext. *)
  VAR
    view: View;
    i   : LineNo;
    d   : INTEGER;
    at  : I;
  BEGIN
    IF (oEnd = begin) AND (nEnd = begin) THEN RETURN; END;
    d := nEnd - oEnd;
    vt.length := vt.length + d;
    view := vt.views;
    WHILE view # NIL DO
      WITH z_126 = view^ DO
        (* check lines *)
        at := z_126.virtual.line [0].virtualLine.from;
        FOR z_127 := 0 TO z_126.nLines - 1 DO
          i := z_127;
          WITH z_128 = z_126.virtual.line [i] DO
            IF z_128.virtualLine.valid THEN
              IF oEnd <= z_128.virtualLine.from THEN
                z_128.virtualLine.from := z_128.virtualLine.from + d;
                z_128.virtualLine.to := z_128.virtualLine.to + d;
                z_128.virtualLine.max := z_128.virtualLine.max + d;
              ELSIF begin < z_128.virtualLine.max THEN
                z_128.virtualLine.valid := FALSE;
                IF begin < z_128.virtualLine.from THEN
                  z_128.virtualLine.from := begin;
                END;
                IF oEnd <= z_128.virtualLine.to THEN
                  z_128.virtualLine.to := z_128.virtualLine.to + d;
                ELSIF begin < z_128.virtualLine.to THEN
                  z_128.virtualLine.to := nEnd;
                END;
                IF oEnd <= z_128.virtualLine.max THEN
                  z_128.virtualLine.max := z_128.virtualLine.max + d;
                ELSIF begin < z_128.virtualLine.max THEN
                  z_128.virtualLine.max := nEnd;
                END;
                Dirtied (view, i, 1);
              END;
              IF z_128.virtualLine.from # at THEN
                Dirtied (view, i, 0);
              END;
              at := z_128.virtualLine.to;
            END;
          END;
        END;
        z_126.virtual.line [z_126.nLines].virtualLine.from := at;
        (* check start *)
        IF oEnd <= z_126.virtual.start.min THEN
          z_126.virtual.start.min := z_126.virtual.start.min + d;
          z_126.virtual.start.max := z_126.virtual.start.max + d;
          z_126.virtual.start.at := z_126.virtual.start.at + d;
        ELSIF begin < z_126.virtual.start.max THEN
          IF oEnd <= z_126.virtual.start.at THEN
            SetStart (view, z_126.virtual.start.at + d);
          ELSIF begin < z_126.virtual.start.at THEN
            SetStart (view, begin);
          ELSE
            SetStart (view, z_126.virtual.start.at);
          END;
        END;
        IF (z_126.virtual.start.at = z_126.vt.length)
             AND (z_126.virtual.start.at > 0) THEN
          SetStart (view, z_126.virtual.start.at - 1);
        END;
        IF z_126.virtual.start.at # z_126.virtual.line [0].virtualLine.from
          THEN
          Dirtied (view, 0, 0, FALSE);
        END;
        (* next view *)
        view := z_126.next;
      END;
    END;
    VTReal.Change (vt, begin, oEnd, nEnd);
  END Change;

PROCEDURE SetStart (view : View;
                    from : I;
                    n    : CARDINAL := 0;
                    force: BOOLEAN  := FALSE)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR at: I;
  BEGIN
    at := view.virtual.start.at;
    IF force AND n = 0 THEN
      view.virtual.start.at := from;
      view.virtual.start.min := from;
      view.virtual.start.max := from;
      IF from = 0 THEN
        view.virtual.start.turned := FALSE;
      ELSE
        VTRd.InitReaderIx(view.vt, from - 1);
        view.virtual.start.turned := Rd.GetChar(view.vt.rd) # '\n';
      END;
    ELSE
      VTBase.Up(view, view.lineWidth, from, n, view.virtual.start);
    END;
    <* ASSERT view.virtual.start.min <= view.virtual.start.at
              AND view.virtual.start.at <= view.virtual.start.max *>
    IF view.virtual.start.at # at THEN Dirtied(view, 0, 0, FALSE); END;
    IF view.virtual.start.at > 0 AND view.virtual.start.at = view.vt.length
         AND NOT force THEN
      VTBase.Up(view, view.lineWidth, view.virtual.start.at - 1, 0,
                view.virtual.start);
      <* ASSERT view.virtual.start.min <= view.virtual.start.at
                AND view.virtual.start.at <= view.virtual.start.max *>
      IF view.virtual.start.at # at THEN Dirtied(view, 0, 0, FALSE); END;
    END;
    VTReal.SetStart(view, view.virtual.start.at, view.virtual.start.turned);
  END SetStart;


PROCEDURE Update (vt: T) RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR view: View;
  BEGIN
    view := vt.views;
    WHILE view # NIL DO UpdateView (view); view := view.next;  END;
  END Update;

PROCEDURE UpdateView (view: View)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR
    o, i, oLines: LineNo;
    at, length  : I;
  BEGIN
    IF NOT view.virtual.dirty THEN RETURN; END;
    length := view.vt.length;
    i := view.virtual.firstDirty;
    o := i;
    oLines := MIN(view.nLines, view.virtual.lines + 1);
    IF i = 0 THEN
      at := view.virtual.start.at;
    ELSE
      at := view.virtual.line[i - 1].virtualLine.to;
    END;
    LOOP
      IF i = view.nLines THEN
        view.virtual.lines := i;
        view.virtual.line[i].virtualLine.from := at;
        EXIT;
      END;
      WHILE (o < oLines)
              AND (NOT view.virtual.line[o].virtualLine.valid
                     OR (view.virtual.line[o].virtualLine.from < at)) DO
        o := o + 1;
      END;
      IF (o < oLines) AND view.virtual.line[o].virtualLine.valid
           AND (view.virtual.line[o].virtualLine.from = at) THEN
        IF (i >= view.virtual.firstAfter) AND (i = o) THEN EXIT; END;
        view.newVirtual.line[i] := view.virtual.line[o];
      ELSE
        WITH line = view.newVirtual.line[i] DO
          line.virtualLine.from := at;
          line.virtualLine.to :=
            VTBase.ComputeLine(
              view, view.lineWidth, at, line.virtualLine.max,
              line.virtualLine.turned, line.virtualLine.width);
          line.virtualLine.valid := TRUE;
        END;
      END;
      WITH line = view.newVirtual.line[i] DO
        at := line.virtualLine.to;
        IF at = line.virtualLine.from THEN
          view.virtual.lines := i;
          i := i + 1;
          EXIT;
        END;
      END;
      i := i + 1;
    END;
    FOR j := view.virtual.firstDirty TO i - 1 DO
      view.virtual.line[j] := view.newVirtual.line[j];
    END;
    FOR i := view.virtual.lines + 1 TO view.virtual.firstAfter DO
      view.virtual.line[i] := view.virtual.line[view.virtual.lines];
    END;
    view.virtual.dirty := FALSE;
    view.virtual.bodyDirty := FALSE;
    view.virtual.firstDirty := view.nLines;
    view.virtual.firstAfter := 0;
  END UpdateView;

PROCEDURE Init (view: View; start: I) RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    view.virtual.start.at := start;
    Bad (view);
  END Init;
(* Like Real.Bad, but invalidates the virtual side. Used on initial setup, or
   when the width has changed. *)

PROCEDURE Bad (view: View) RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR i: INTEGER;
  BEGIN
    WITH z_135 = view^ DO
      FOR z_136 := 0 TO z_135.nLines - 1 DO
        i := z_136;
        z_135.virtual.line[i].virtualLine.valid := FALSE;
      END;
      z_135.virtual.lines := 0;
      z_135.virtual.dirty := TRUE;
      z_135.virtual.bodyDirty := TRUE;
      z_135.virtual.firstDirty := 0;
      z_135.virtual.firstAfter := z_135.nLines;
      SetStart (view, z_135.virtual.start.at);
    END;
  END Bad;

PROCEDURE Resize (view: View; n: CARDINAL) RAISES {} =
  VAR i: CARDINAL;
  BEGIN
    WITH z_137 = view^ DO
      IF z_137.virtual.lines < n THEN
        FOR z_138 := z_137.virtual.lines TO n - 1 DO
          i := z_138;
          z_137.virtual.line[i].virtualLine.valid := FALSE;
        END;
        z_137.virtual.dirty := TRUE;
        z_137.virtual.bodyDirty := TRUE;
        z_137.virtual.firstDirty
          := MIN (z_137.virtual.lines, z_137.virtual.firstDirty);
        z_137.virtual.firstAfter := MAX (n, z_137.virtual.firstAfter);
      ELSIF z_137.virtual.lines > n THEN
        z_137.virtual.lines := n;
        z_137.virtual.firstDirty := MIN (z_137.virtual.firstDirty, n);
        z_137.virtual.firstAfter := MIN (z_137.virtual.firstAfter, n);
      END;
    END;
  END Resize;
(* Utility *)

PROCEDURE Dirtied
  (view: View; i: LineNo; n: CARDINAL; bodyDirty: BOOLEAN := TRUE) RAISES {} =
  BEGIN
    WITH z_139 = view^ DO
      z_139.virtual.dirty := TRUE;
      IF bodyDirty THEN z_139.virtual.bodyDirty := TRUE;  END;
      z_139.virtual.firstDirty := MIN (z_139.virtual.firstDirty, i);
      z_139.virtual.firstAfter := MAX (z_139.virtual.firstAfter, i + n);
    END;
  END Dirtied;

BEGIN
END VTVirtual.
