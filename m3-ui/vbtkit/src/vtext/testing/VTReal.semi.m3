(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Mar  5 20:32:32 PST 1993 by meehan *)
(*      modified on Tue Jun 16 13:12:37 PDT 1992 by muller *)
(*      modified on Fri Mar 27 03:00:51 1992 by steveg *)
(*      modified on Fri Mar 20 10:25:05 PST 1992 by jdd    *)
(*      modified on Thu Jul 11 16:07:43 PDT 1991 by mhb *)

(*      modified on Wed May 16  7:51:26 PDT 1990 by mcjones *)
(*      Modified On Mon Sep 25 15:23:30 PDT 1989 by brooks *)

(* This module maintains the "real" screen structures. The following
   invariants are maintained:

   If line^[i].realLine.valid, the screen line i is an accurate rendering of
   characters line^[i].realLine.from up to but not including
   line^[i].realLine.to.

   dirty is true if any line^[i].realLine.valid is false, or the information
   is otherwise obsolete

   if dirty is false, then the line^[i].realLine.from are all in order

   line^[0] through line^[lines - 1] are defined. when dirty is false, then if
   lines < nLines, then the remaining lines are zero-size. (The allWhiteBelow,
   turned and width fields are defined through nLines - 1.) *)

MODULE VTReal;

IMPORT PaintOp, Point, Rd, Rect, Thread, VBT;
IMPORT VTDef, VTBase, VTCaret, VTInterval, VTMarker, VTVirtual, VTRd,
  VTTexture;

TYPE
  Block = VTDef.Block;
  Coord = VTDef.Coord;
  IntervalStyle = VTDef.IntervalStyle;
  IntervalOptions = VTDef.IntervalOptions;
  LineNo = VTDef.LineNo;
  Marker = VTDef.Marker;
  Tint = VTDef.Tint;
  TriState = VTDef.TriState;
  WhichEnd = VTDef.WhichEnd;
    (* Change notes a change made in the mtext. *)


PROCEDURE Change (vt: T; begin, oEnd, nEnd: I) RAISES {} =
  VAR view: View; i: LineNo; d: INTEGER;
  BEGIN
    IF (oEnd = begin) AND (nEnd = begin) THEN RETURN ;  END;
    d := nEnd - oEnd;
    WITH z_39 = vt^ DO
      view := z_39.views;
      WHILE view # NIL DO
        WITH z_40 = view^ DO
          IF z_40.real.lines > 0 THEN
            i := z_40.real.lines;
            LOOP
              IF i = 0 THEN EXIT;  END;
              i := i - 1;
              WITH z_41 = z_40.real.line[i] DO
                IF z_41.realLine.valid THEN
                  IF oEnd <= z_41.realLine.from THEN
                    z_41.realLine.from := z_41.realLine.from + d;
                    z_41.realLine.to := z_41.realLine.to + d;
                  ELSIF begin < z_41.realLine.to THEN
                    z_41.realLine.valid := FALSE;
                    IF begin < z_41.realLine.from THEN
                      z_41.realLine.from := begin;
                    END;
                    IF oEnd <= z_41.realLine.to THEN
                      z_41.realLine.to := z_41.realLine.to + d;
                    ELSIF begin < z_41.realLine.to THEN
                      z_41.realLine.to := nEnd;
                    END;
                    IF i > 0 THEN
                      Dirtied (view, i - 1, 2);
                    ELSE
                      Dirtied (view, i, 1);
                    END;
                  ELSIF begin = z_41.realLine.to THEN
                    Dirtied (view, i, 1);
                  ELSE
                    EXIT;
                  END;
                END;
              END;
            END;
          END;
          view := z_40.next;
        END;
      END;
    END;
  END Change;

PROCEDURE SetStart (view: View; at: I; turned: BOOLEAN) RAISES {} =
  BEGIN
    WITH z_42 = view^ DO
      IF (z_42.real.start.at # at) OR (z_42.real.start.turned # turned) THEN
        z_42.real.start.at := at;
        z_42.real.start.turned := turned;
        Dirtied (view, 0, 0);
      END;
    END;
  END SetStart;

PROCEDURE Update (vt: T)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR view: View;
  BEGIN
    view := vt.views;
    WHILE view # NIL DO UpdateView (view); view := view.next; END;
  END Update;
  
VAR boolToTriState: ARRAY BOOLEAN OF TriState;

PROCEDURE UpdateView (view: View)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR
    i                    : CARDINAL;
    a, b                 : INTEGER;
    justBad              : BOOLEAN;
    bad, oClip, oTextClip: Rect.T;
    f                    : Rect.Partition;
  BEGIN
    WITH z_43 = view^ DO
      IF (z_43.real.lines > 0)
           AND (z_43.real.line [0].realLine.from # z_43.virtual.start.at)
        THEN
        Dirtied (view, 0, 0);
      END;
      IF Rect.IsEmpty (z_43.rect.bad) THEN
        IF NOT z_43.real.dirty THEN RETURN; END;
        justBad := FALSE;
      ELSE
        Rect.Factor (z_43.rect.bad, z_43.rect.textClip, f, 0, 0);
        FOR z_44 := 0 TO 4 DO
          i := z_44;
          IF i # 2 THEN
            IF NOT Rect.IsEmpty (f [i]) THEN
              VBT.PaintTint (z_43.vbt, f [i], z_43.vOptions.whiteBlack.bg);
            END;
          END;
        END;
        a := MAX ((z_43.rect.bad.north - z_43.rect.text.north)
                    DIV z_43.lineSpacing, 0);
        b := MIN ((z_43.rect.bad.south - 1 - z_43.rect.text.north)
                    DIV z_43.lineSpacing, z_43.nLines - 1);
        FOR z_45 := a TO b DO
          i := z_45;
          WITH z_46 = z_43.real.line [i] DO
            z_46.realLine.valid := FALSE;
            z_46.realLine.width :=
              MAX (MIN (z_43.rect.text.east, z_43.rect.bad.east)
                     - z_43.rect.text.west, z_46.realLine.width);
            z_46.realLine.allWhiteBelow := FALSE;
            z_46.realLine.turned [0] := TriState.Unknown;
            z_46.realLine.turned [1] := TriState.Unknown;
          END;
        END;
        z_43.real.lines := MAX (z_43.real.lines, b + 1);
        z_43.real.dirty := TRUE;
        justBad := z_43.real.firstDirty > z_43.real.firstAfter;
        IF justBad THEN bad := z_43.rect.bad; END;
        z_43.rect.bad := Rect.Empty;
        z_43.real.firstDirty := MIN (a, z_43.real.firstDirty);
        z_43.real.firstAfter := MAX (b + 1, z_43.real.firstAfter);
      END;
      VTCaret.Deactivate (view);
      TRY
        IF justBad THEN
          oClip := z_43.rect.clip;
          z_43.rect.clip := Rect.Meet (z_43.rect.clip, bad);
          oTextClip := z_43.rect.textClip;
          z_43.rect.textClip := Rect.Meet (z_43.rect.textClip, bad);
        END;
        TRY
          IF z_43.virtual.dirty THEN VTVirtual.UpdateView (view); END;
          IF justBad THEN
            IF NOT Rect.IsEmpty (z_43.rect.textClip) THEN
              z_43.real.blocks.n := 0;
              PaintAll (
                view, (z_43.rect.textClip.north - z_43.rect.text.north)
                        DIV z_43.lineSpacing,
                (z_43.rect.textClip.south - z_43.rect.text.north
                   + z_43.lineSpacing - 1) DIV z_43.lineSpacing);
            END;
          ELSE
            a := 0;
            b := z_43.nLines;
            FindBlocks (view, a, b);
            BltBlocks (view);
            PaintAll (view, a, b);
          END;
          IF z_43.virtual.lines < z_43.real.lines THEN
            VBT.PaintTint (
              z_43.vbt,
              Rect.Meet (
                Rect.FromEdges (z_43.rect.full.west, z_43.rect.full.east,
                                z_43.rect.text.north
                                  + z_43.virtual.lines * z_43.lineSpacing,
                                z_43.rect.text.north
                                  + z_43.real.lines * z_43.lineSpacing),
                z_43.rect.clip), z_43.vOptions.whiteBlack.bg);
            FOR z_47 := z_43.virtual.lines TO z_43.real.lines - 1 DO
              i := z_47;
              WITH z_48 = z_43.real.line [i] DO
                z_48.realLine.width := 0;
                z_48.realLine.turned [0] := TriState.False;
                z_48.realLine.turned [1] := TriState.False;
                z_48.realLine.allWhiteBelow := TRUE;
              END;
            END;
            IF z_43.vOptions.eob THEN
              VBT.PaintTint (
                z_43.vbt,
                Rect.Meet (Rect.FromEdges (
                             z_43.rect.full.west, z_43.rect.full.east,
                             z_43.rect.text.north
                               + z_43.real.lines * z_43.lineSpacing,
                             z_43.rect.text.north
                               + (z_43.real.lines + 1) * z_43.lineSpacing),
                           z_43.rect.clip), z_43.vOptions.whiteBlack.bg);
              VBT.PaintTint (
                z_43.vbt,
                Rect.Meet (
                  Rect.FromEdges (
                    z_43.rect.text.west, z_43.rect.text.east,
                    z_43.rect.text.north
                      + z_43.virtual.lines * z_43.lineSpacing,
                    z_43.rect.text.north
                      + z_43.virtual.lines * z_43.lineSpacing + 1),
                  z_43.rect.textClip), z_43.vOptions.whiteBlack.fg);
              WITH z_49 = z_43.real.line [z_43.virtual.lines] DO
                z_49.realLine.width :=
                  z_43.rect.text.east - z_43.rect.text.west;
                z_49.realLine.allWhiteBelow := FALSE;
              END;
            END;
          ELSIF z_43.virtual.lines > z_43.real.lines THEN
            IF z_43.vOptions.eob THEN
              VBT.PaintTint (
                z_43.vbt,
                Rect.Meet (
                  Rect.FromEdges (
                    z_43.rect.text.west, z_43.rect.text.east,
                    z_43.rect.text.north
                      + z_43.virtual.lines * z_43.lineSpacing,
                    z_43.rect.text.north
                      + z_43.virtual.lines * z_43.lineSpacing + 1),
                  z_43.rect.textClip), z_43.vOptions.whiteBlack.fg);
              WITH z_50 = z_43.real.line [z_43.virtual.lines] DO
                z_50.realLine.width :=
                  z_43.rect.text.east - z_43.rect.text.west;
                z_50.realLine.allWhiteBelow := FALSE;
              END;
            END;
          END;
        FINALLY
          IF justBad THEN
            z_43.rect.clip := oClip;
            z_43.rect.textClip := oTextClip;
          END;
        END;
        z_43.real.lines := z_43.virtual.lines;
        FOR z_51 := 0 TO z_43.real.lines - 1 DO
          i := z_51;
          z_43.real.line [i].realLine.valid := TRUE;
        END;
        z_43.real.dirty := FALSE;
        z_43.real.firstDirty := z_43.nLines;
        z_43.real.firstAfter := 0;
      FINALLY
        VTCaret.Reactivate (view);
      END;
    END;
  END UpdateView;

PROCEDURE FindBlocks (view: View; VAR (*INOUT*) a, b: INTEGER) RAISES {} =
  VAR v, o: LineNo; block: Block; f: I;
  BEGIN
    WITH z_52 = view^ DO
      z_52.real.blocks.n := 0;
      o := z_52.real.firstDirty;
      v := MAX (a, z_52.real.firstDirty);
      a := v;
      b := MIN (b, z_52.virtual.lines);
            (* find good blocks to carry over *)
      LOOP
                (* find a valid old line *)
        WHILE (o < z_52.real.lines) AND  NOT z_52.real.line[o].realLine.valid
          DO
          o := o + 1;
        END;
        IF  NOT (o < z_52.real.lines) THEN EXIT;  END;
                (* try to match it to some desired result *)
        f := z_52.real.line[o].realLine.from;
        WHILE (v < b) AND (z_52.virtual.line[v].virtualLine.from < f) DO
          v := v + 1;
        END;
        IF  NOT (v < b) THEN EXIT;  END;
                (* does it carry over? *)
        IF (z_52.virtual.line[v].virtualLine.from = f)
          AND (z_52.virtual.line[v].virtualLine.to
               = z_52.real.line[o].realLine.to) THEN
          block.old := o;
          block.new := v;
          LOOP
            o := o + 1;
            v := v + 1;
            IF  NOT ((o < z_52.real.lines) AND (v < b)) THEN EXIT;  END;
            WITH z_53 = z_52.real.line[o] DO
              IF
                NOT (z_53.realLine.valid
                     AND (z_53.realLine.from
                          = z_52.virtual.line[v].virtualLine.from)
                     AND (z_53.realLine.to
                          = z_52.virtual.line[v].virtualLine.to)) THEN
                EXIT;
              END;
            END;
            IF (o = v) AND  NOT (v < z_52.real.firstAfter) THEN
              b := v;
              EXIT;
            END;
          END;
          block.length := o - block.old;
          z_52.real.blocks.block[z_52.real.blocks.n].block := block;
          z_52.real.blocks.n := z_52.real.blocks.n + 1;
          v := v - 1;
          o := o - 1;
        END;
                (* iterate *)
        o := o + 1;
      END;
    END;
  END FindBlocks;

PROCEDURE BltBlocks (view: View) RAISES {} =
  PROCEDURE Blt (READONLY block: Block) RAISES {} =
    BEGIN
      WITH z_54 = view^ DO
        <* ASSERT block.old >= 0
                AND block.old + block.length <= z_54.nLines
                AND block.new >= 0
                AND block.new + block.length <= z_54.nLines *>
        VBT.Scroll (
          z_54.vbt,
          Rect.Meet (Rect.FromEdges (
                       z_54.rect.full.west, z_54.rect.full.east,
                       z_54.rect.text.north + block.new * z_54.lineSpacing,
                       z_54.rect.text.north
                         + (block.new + block.length) * z_54.lineSpacing),
                     z_54.rect.clip),
          Point.FromCoords (0, (block.new - block.old) * z_54.lineSpacing),
          PaintOp.Copy);
      END;
    END Blt;
  PROCEDURE BltUp (READONLY block: Block) RAISES {} =
    VAR i: INTEGER;
    BEGIN
      Blt (block);
      WITH z_55 = view^ DO
        FOR z_56 := 0 TO block.length - 1 DO
          i := z_56;
          z_55.real.line [block.new + i] := z_55.real.line [block.old + i];
        END;
      END;
    END BltUp;
  PROCEDURE BltDown (READONLY block: Block) RAISES {} =
    VAR i: INTEGER;
    BEGIN
      Blt (block);
      WITH z_57 = view^ DO
        FOR z_58 := block.length - 1 TO 0 BY -1 DO
          i := z_58;
          z_57.real.line [block.new + i] := z_57.real.line [block.old + i];
        END;
      END;
    END BltDown;
  VAR b, bb, start: INTEGER;
  BEGIN
    b := 0;
    WITH z_59 = view^ DO
      WHILE b < z_59.real.blocks.n DO
        (* skip stationary blocks *)
        WHILE (b < z_59.real.blocks.n)
                AND (z_59.real.blocks.block [b].block.old
                       = z_59.real.blocks.block [b].block.new) DO
          b := b + 1;
        END;
        (* blocks to be moved up: do them first to last *)
        start := b;
        WHILE (b < z_59.real.blocks.n)
                AND (z_59.real.blocks.block [b].block.old
                       > z_59.real.blocks.block [b].block.new) DO
          b := b + 1;
        END;
        FOR z_60 := start TO b - 1 DO
          bb := z_60;
          BltUp (z_59.real.blocks.block [bb].block);
        END;
        (* blocks to be moved down: do them last to first *)
        start := b;
        WHILE (b < z_59.real.blocks.n)
                AND (z_59.real.blocks.block [b].block.old
                       < z_59.real.blocks.block [b].block.new) DO
          b := b + 1;
        END;
        FOR z_61 := b - 1 TO start BY -1 DO
          bb := z_61;
          BltDown (z_59.real.blocks.block [bb].block);
        END;
      END;
    END;
  END BltBlocks;

PROCEDURE PaintAll (view: View; l0, l1: CARDINAL)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR
    at     : I;
    b      : CARDINAL;
    rdSet  : BOOLEAN;
    rdIndex: I;
  BEGIN
    WITH z_62 = view^ DO
      VTInterval.Fix (z_62.vt);
      VTMarker.Fix (z_62.vt);
      at := l0;
      rdSet := FALSE;
      FOR z_63 := 0 TO z_62.real.blocks.n - 1 DO
        b := z_63;
        WITH z_64 = z_62.real.blocks.block [b] DO
          PaintGap (view, at, z_64.block.new, rdSet, rdIndex);
          at := z_64.block.new + z_64.block.length;
        END;
      END;
      PaintGap (view, at, l1, rdSet, rdIndex);
    END;
  END PaintAll;

PROCEDURE PaintGap (                view   : View;
                                    l0, l1 : CARDINAL;
                    VAR (* INOUT *) rdSet  : BOOLEAN;
                    VAR (* INOUT *) rdIndex: I         )
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR
    i               : LineNo;
    length          : CARDINAL;
    f, t            : I;
    turned0, turned1: BOOLEAN;
  BEGIN
    IF l0 >= l1 THEN RETURN; END;
    WITH z_65 = view^ DO
      length := z_65.vt.length;
      IF l0 = 0 THEN
        turned0 := z_65.real.start.turned;
      ELSE
        turned0 := z_65.virtual.line [l0 - 1].virtualLine.turned;
      END;
      FOR z_66 := l0 TO z_65.virtual.lines - 1 DO
        i := z_66;
        WITH z_67 = z_65.real.line [i] DO
          WITH z_68 = z_65.virtual.line [i] DO
            IF z_65.vOptions.turnMargin > 0 THEN
              turned0 := (turned0 AND z_65.vOptions.wrap)
                           OR ((z_65.vOptions.leftOffset > 0)
                                 AND (z_68.virtualLine.from < length));
              IF boolToTriState [turned0] # z_67.realLine.turned [0] THEN
                PaintTurn (
                  view, z_65.rect.full.west,
                  z_65.rect.text.north + i * z_65.lineSpacing, turned0);
                z_67.realLine.turned [0] := boolToTriState [turned0];
              END;
            END;
            IF i >= l1 THEN RETURN; END;
            f := MIN (z_68.virtualLine.from, length);
            t := MIN (z_68.virtualLine.to, length);
            PaintLine (view, i, f, t, rdSet, rdIndex);
            turned1 := z_68.virtualLine.turned
                         OR (z_68.virtualLine.width > z_65.lineWidth);
            z_67.realLine.from := z_68.virtualLine.from;
            z_67.realLine.to := z_68.virtualLine.to;
            IF z_65.vOptions.turnMargin > 0 THEN
              IF boolToTriState [turned1] # z_67.realLine.turned [1] THEN
                PaintTurn (
                  view, z_65.rect.full.east - z_65.vOptions.turnMargin,
                  z_65.rect.text.north + i * z_65.lineSpacing, turned1);
                z_67.realLine.turned [1] := boolToTriState [turned1];
              END;
            END;
            turned0 := z_68.virtualLine.turned;
          END;
        END;
      END;
    END;
  END PaintGap;

PROCEDURE PaintLine (                view    : View;
                                     i       : LineNo;
                                     from, to: I;
                     VAR (* INOUT *) rdSet   : BOOLEAN;
                     VAR (* INOUT *) rdIndex : I        )
  RAISES {Rd.Failure, Rd.EndOfFile, Thread.Alerted} =
  CONST BufferSize = 128;
  VAR
    v               : Coord;
    h, oldWidth     : Coord;
    length          : CARDINAL;
    intervalOptions : IntervalOptions;
    at, l, r, until : I;
    chars           : ARRAY [0 .. BufferSize - 1] OF CHAR;
    oldAllWhiteBelow: BOOLEAN;
  VAR
    marker             : Marker;
    leftSide, rightSide: Point.T;
    stroke             : Tint;
  BEGIN
    WITH z_69 = view^ DO
      WITH z_70 = z_69.real.line [i] DO
        v := z_69.rect.text.north + i * z_69.lineSpacing;
        WITH z_71 = z_69.vScreenFont^ DO
          oldAllWhiteBelow := z_70.realLine.allWhiteBelow;
          oldWidth := z_70.realLine.width;
          z_70.realLine.allWhiteBelow := TRUE;
          z_70.realLine.width := 0;
          h := z_69.rect.text.west;
          IF rdSet THEN
            IF rdIndex # from THEN Rd.Seek (z_69.vt.rd, from); END;
          ELSE
            VTRd.InitReaderIx (z_69.vt, from);
            rdIndex := from;
            rdSet := TRUE;
          END;
          at := from;
          IF NOT z_71.vScreenFont.paintOpaque THEN
            VBT.BeginGroup (z_69.vbt, 3 * (to - from));
            WHILE at < to DO
              intervalOptions :=
                VTInterval.CurrentOptions (view, at, l, r);
              until := MIN (to, r);
              length := until - at;
              IF length > BufferSize THEN
                length := BufferSize;
                until := at + length;
              END;
              WITH
                a = Rd.GetSub (z_69.vt.rd, SUBARRAY (chars, 0, length)) DO
                <* ASSERT a = length *>
              END;
              PaintBackgroundTransparent (
                view, h, v, chars, length, intervalOptions,
                oldAllWhiteBelow, z_70.realLine.allWhiteBelow, oldWidth,
                z_70.realLine.width);
              at := until;
            END;
            h := z_69.rect.text.west;
            Rd.Seek (z_69.vt.rd, from);
            at := from;
          END;
          WHILE at < to DO
            intervalOptions := VTInterval.CurrentOptions (view, at, l, r);
            until := MIN (to, r);
            length := until - at;
            IF length > BufferSize THEN
              length := BufferSize;
              until := at + length;
            END;
            WITH a = Rd.GetSub (z_69.vt.rd, SUBARRAY (chars, 0, length)) DO
              <* ASSERT a = length *>
            END;
            IF z_71.vScreenFont.paintOpaque THEN
              PaintSegmentOpaque (
                view, h, v, chars, length, intervalOptions,
                oldAllWhiteBelow, z_70.realLine.allWhiteBelow, oldWidth,
                z_70.realLine.width, at = l, until = r);
            ELSE
              PaintSegmentTransparent (
                view, h, v, chars, length, intervalOptions,
                oldAllWhiteBelow, z_70.realLine.allWhiteBelow, oldWidth,
                z_70.realLine.width);
            END;
            at := until;
          END;
          IF NOT z_71.vScreenFont.paintOpaque THEN
            h := z_69.rect.text.west;
            Rd.Seek (z_69.vt.rd, from);
            at := from;
            WHILE at < to DO
              intervalOptions :=
                VTInterval.CurrentOptions (view, at, l, r);
              until := MIN (to, r);
              length := until - at;
              PaintOverlayTransparent (
                view, h, v, z_69.vt.rd, length, intervalOptions,
                oldAllWhiteBelow, z_70.realLine.allWhiteBelow, oldWidth,
                z_70.realLine.width, at = l, until = r);
              at := until;
            END;
          END;
          IF h < z_69.rect.text.west + oldWidth THEN
            VBT.PaintTint (
              z_69.vbt,
              Rect.Meet (
                Rect.FromEdges (h, z_69.rect.text.west + oldWidth, v,
                                v + z_69.lineSpacing), z_69.rect.textClip),
              z_69.vOptions.whiteBlack.bg);
          END;
          rdIndex := to;
          marker := VTMarker.FirstMarker (z_69.vt, from);
          WHILE (marker # NIL) AND (marker.index < to) DO
            WITH z_72 = marker^ DO
              VTBase.UnsafeLocatePoint (view, z_72.index, leftSide);
              VTBase.UnsafeLocatePoint (view, z_72.index, rightSide, 0);
              rdSet := FALSE;
              <* ASSERT leftSide.h < rightSide.h *>(* zero-width character
                                                      position *)
              stroke := z_72.options.stroke;
              CASE z_72.options.whichEnd OF
              | WhichEnd.Left =>
                  VBT.PaintTint (
                    z_69.vbt,
                    Rect.Meet (Rect.FromEdges (
                                 leftSide.h, leftSide.h + 1, v,
                                 v + (z_71.vScreenFont.box.south
                                        - z_71.vScreenFont.box.north)),
                               z_69.rect.textClip), stroke);
              | WhichEnd.Right =>
                  VBT.PaintTint (
                    z_69.vbt,
                    Rect.Meet (Rect.FromEdges (
                                 rightSide.h - 1, rightSide.h, v,
                                 v + (z_71.vScreenFont.box.south
                                        - z_71.vScreenFont.box.north)),
                               z_69.rect.textClip), stroke);
              END;
              IF z_72.options.top THEN
                VBT.PaintTint (
                  z_69.vbt,
                  Rect.Meet (
                    Rect.FromEdges (leftSide.h, rightSide.h, v, v + 1),
                    z_69.rect.textClip), stroke);
              END;
              IF z_72.options.bottom THEN
                VBT.PaintTint (
                  z_69.vbt,
                  Rect.Meet (Rect.FromEdges (
                               leftSide.h, rightSide.h,
                               v + (z_71.vScreenFont.box.south
                                      - z_71.vScreenFont.box.north) - 1,
                               v + (z_71.vScreenFont.box.south
                                      - z_71.vScreenFont.box.north)),
                             z_69.rect.textClip), stroke);
              END;
              IF (z_72.options.whichEnd = WhichEnd.Left)
                   AND NOT z_72.options.top AND NOT z_72.options.bottom
                THEN
                z_70.realLine.width :=
                  MAX (z_70.realLine.width,
                       leftSide.h + 1 - z_69.rect.text.west);
              ELSE
                z_70.realLine.width :=
                  MAX (
                    z_70.realLine.width, rightSide.h - z_69.rect.text.west);
              END;
              VTMarker.NextMarker (z_72.vt, marker);
            END;
          END;
          IF NOT z_71.vScreenFont.paintOpaque THEN
            VBT.EndGroup (z_69.vbt);
          END;
        END;
      END;
    END;
  END PaintLine;

PROCEDURE PaintSegmentOpaque (                view  : View;
                              VAR (* INOUT *) h     : Coord;
                                              v     : Coord;
                              READONLY        chars : ARRAY OF CHAR;
                                              length: CARDINAL;
                              READONLY intervalOptions : IntervalOptions;
                                       oldAllWhiteBelow: BOOLEAN;
                              VAR (* OUT *) allWhiteBelow: BOOLEAN;
                              oldWidth: Coord;
                              VAR (* OUT *) newWidth: Coord;
                              atStyleStart, atStyleStop0: BOOLEAN)
  RAISES {} =
  VAR
    charht     : Coord;
    h0         : INTEGER (* Coord *);
    ci, ci0    : INTEGER (* CARDINAL *);
    atStyleStop: BOOLEAN;
  PROCEDURE PaintSub (READONLY chars        : ARRAY OF CHAR;
                               start, length: CARDINAL       ) RAISES {} =
    VAR
      refpt: Point.T;
      clip : Rect.T;
    BEGIN
      WITH z_73 = view^ DO
        WITH z_74 = z_73.vScreenFont^ DO
          WITH z_75 = z_74.vScreenFont.vFont^ DO
            IF (h0 < z_73.rect.textClip.east)
                 AND (h > z_73.rect.textClip.west) THEN
              refpt := Point.FromCoords (h0 - z_74.vScreenFont.box.west,
                                         v - z_74.vScreenFont.box.north);
              CASE intervalOptions.style OF
              | IntervalStyle.NoStyle =>
                  VBT.PaintSub (
                    z_73.vbt, z_73.rect.textClip, refpt, z_75.vFont.font,
                    SUBARRAY (chars, start, length),
                    z_73.vOptions.whiteBlack.bgFg);
                  FillLeading (z_73.vOptions.whiteBlack.bg);
              | IntervalStyle.HighlightStyle =>
                  VBT.PaintSub (
                    z_73.vbt, z_73.rect.textClip, refpt, z_75.vFont.font,
                    SUBARRAY (chars, start, length),
                    intervalOptions.whiteBlack.bgFg);
                  FillLeading (intervalOptions.leading);
              | IntervalStyle.InverseStyle =>
                  VBT.PaintSub (
                    z_73.vbt, z_73.rect.textClip, refpt, z_75.vFont.font,
                    SUBARRAY (chars, start, length),
                    intervalOptions.whiteBlack.fgBg);
                  FillLeading (intervalOptions.leading);
              | IntervalStyle.GrayStyle =>
                  VBT.PaintSub (
                    z_73.vbt, z_73.rect.textClip, refpt, z_75.vFont.font,
                    SUBARRAY (chars, start, length),
                    intervalOptions.whiteBlack.bgFg);
                  VBT.PaintTexture (
                    z_73.vbt,
                    Rect.Meet (Rect.FromEdges (h0, h, v, v + charht),
                               z_73.rect.textClip),
                    intervalOptions.whiteBlack.bgTransparent,
                    VTTexture.gray,
                    Point.FromCoords (z_73.rect.text.west, v));
                  FillLeading (intervalOptions.leading);
              | IntervalStyle.UnderlineStyle =>
                  IF z_73.vOptions.leading > 0 THEN
                    clip :=
                      Rect.Meet (Rect.FromEdges (h0, h, v, v + charht - 1),
                                 z_73.rect.textClip);
                    VBT.PaintSub (z_73.vbt, clip, refpt, z_75.vFont.font,
                                  SUBARRAY (chars, start, length),
                                  intervalOptions.whiteBlack.bgFg);
                    VBT.PaintTint (
                      z_73.vbt,
                      Rect.Meet (Rect.FromEdges (
                                   h0, h, v + charht - 1, v + charht + 1),
                                 z_73.rect.textClip),
                      intervalOptions.whiteStroke.fg);
                    IF NOT (oldAllWhiteBelow
                              AND (intervalOptions.leading.op
                                     = z_73.vOptions.whiteBlack.bg.op))
                         AND (z_73.vOptions.leading > 1) THEN
                      VBT.PaintTint (
                        z_73.vbt,
                        Rect.Meet (Rect.FromEdges (
                                     h0, h, v + charht + 1,
                                     v + charht + z_73.vOptions.leading),
                                   z_73.rect.textClip),
                        intervalOptions.leading);
                    END;
                    allWhiteBelow := FALSE;
                  ELSE
                    clip :=
                      Rect.Meet (Rect.FromEdges (h0, h, v, v + charht - 2),
                                 z_73.rect.textClip);
                    VBT.PaintSub (z_73.vbt, clip, refpt, z_75.vFont.font,
                                  SUBARRAY (chars, start, length),
                                  intervalOptions.whiteBlack.bgFg);
                    VBT.PaintTint (z_73.vbt,
                                   Rect.Meet (
                                     Rect.FromEdges (
                                       h0, h, v + charht - 2, v + charht),
                                     z_73.rect.textClip),
                                   intervalOptions.whiteStroke.fg);
                  END;
              | IntervalStyle.ThinUnderlineStyle =>
                  clip :=
                    Rect.Meet (Rect.FromEdges (h0, h, v, v + charht - 1),
                               z_73.rect.textClip);
                  VBT.PaintSub (z_73.vbt, clip, refpt, z_75.vFont.font,
                                SUBARRAY (chars, start, length),
                                intervalOptions.whiteBlack.bgFg);
                  VBT.PaintTint (
                    z_73.vbt,
                    Rect.Meet (
                      Rect.FromEdges (h0, h, v + charht - 1, v + charht),
                      z_73.rect.textClip), intervalOptions.whiteStroke.fg);
                  FillLeading (intervalOptions.leading);
              | IntervalStyle.GrayUnderlineStyle =>
                  clip :=
                    Rect.Meet (Rect.FromEdges (h0, h, v, v + charht - 1),
                               z_73.rect.textClip);
                  VBT.PaintSub (z_73.vbt, clip, refpt, z_75.vFont.font,
                                SUBARRAY (chars, start, length),
                                intervalOptions.whiteBlack.bgFg);
                  VBT.PaintTexture (
                    z_73.vbt,
                    Rect.Meet (
                      Rect.FromEdges (h0, h, v + charht - 1, v + charht),
                      z_73.rect.textClip),
                    intervalOptions.whiteStroke.bgFg, VTTexture.gray,
                    Point.FromCoords (z_73.rect.text.west, v));
                  FillLeading (intervalOptions.leading);
              | IntervalStyle.BoxStyle =>
                  clip := Rect.FromEdges (h0, h, v + 1, v + charht - 1);
                  IF atStyleStart THEN
                    clip := Rect.MoveEdge (clip, Rect.Edge.W, +1);
                  END;
                  IF atStyleStop THEN
                    clip := Rect.MoveEdge (clip, Rect.Edge.E, -1);
                  END;
                  clip := Rect.Meet (clip, z_73.rect.textClip);
                  VBT.PaintSub (z_73.vbt, clip, refpt, z_75.vFont.font,
                                SUBARRAY (chars, start, length),
                                intervalOptions.whiteBlack.bgFg);
                  VBT.PaintTint (
                    z_73.vbt, Rect.Meet (Rect.FromEdges (h0, h, v, v + 1),
                                         z_73.rect.textClip),
                    intervalOptions.whiteStroke.fg);
                  VBT.PaintTint (
                    z_73.vbt,
                    Rect.Meet (
                      Rect.FromEdges (h0, h, v + charht - 1, v + charht),
                      z_73.rect.textClip), intervalOptions.whiteStroke.fg);
                  IF atStyleStart THEN
                    VBT.PaintTint (
                      z_73.vbt, Rect.Meet (Rect.FromEdges (
                                             h0, h0 + 1, v, v + charht),
                                           z_73.rect.textClip),
                      intervalOptions.whiteStroke.fg);
                  END;
                  IF atStyleStop THEN
                    VBT.PaintTint (
                      z_73.vbt,
                      Rect.Meet (Rect.FromEdges (h - 1, h, v, v + charht),
                                 z_73.rect.textClip),
                      intervalOptions.whiteStroke.fg);
                  END;
                  FillLeading (intervalOptions.leading);
              | IntervalStyle.SlugStyle =>
                  VBT.PaintTint (
                    z_73.vbt,
                    Rect.Meet (Rect.FromEdges (h0, h, v, v + charht),
                               z_73.rect.textClip),
                    intervalOptions.whiteStroke.fg);
                  FillLeading (intervalOptions.leading);
              | IntervalStyle.OverlapStyle =>
                  VBT.PaintTexture (
                    z_73.vbt,
                    Rect.Meet (
                      Rect.FromEdges (
                        h0, h, v, v + charht + z_73.vOptions.leading),
                      z_73.rect.textClip), z_73.vOptions.whiteBlack.bgFg,
                    VTTexture.lightGray,
                    Point.FromCoords (z_73.rect.text.west, v));
                  VBT.PaintSub (
                    z_73.vbt, z_73.rect.textClip, refpt, z_75.vFont.font,
                    SUBARRAY (chars, start, length),
                    z_73.vOptions.whiteBlack.transparentFg);
                  allWhiteBelow := FALSE;
              END;
            ELSE
              IF intervalOptions.style # IntervalStyle.NoStyle THEN
                allWhiteBelow := FALSE;
              END;
            END;
            newWidth := h - z_73.rect.text.west;
          END;
        END;
      END;
    END PaintSub;
  PROCEDURE PaintWhite () RAISES {} =
    VAR to: Rect.T;
    BEGIN
      WITH z_76 = view^ DO
        IF (h0 < z_76.rect.textClip.east) AND (h > z_76.rect.textClip.west)
          THEN
          to :=
            Rect.FromEdges (h0, h, v, v + charht + z_76.vOptions.leading);
          IF intervalOptions.style = IntervalStyle.NoStyle THEN
            IF h0 < z_76.rect.text.west + oldWidth THEN
              VBT.PaintTint (z_76.vbt, Rect.Meet (to, z_76.rect.textClip),
                             z_76.vOptions.whiteBlack.bg);
            END;
          ELSE
            CASE intervalOptions.style OF
            | IntervalStyle.HighlightStyle =>
                VBT.PaintTint (
                  z_76.vbt,
                  Rect.Meet (Rect.MoveEdge (
                               to, Rect.Edge.S, -z_76.vOptions.leading),
                             z_76.rect.textClip),
                  intervalOptions.whiteBlack.bg);
                FillLeading (intervalOptions.leading);
            | IntervalStyle.InverseStyle =>
                VBT.PaintTint (
                  z_76.vbt,
                  Rect.Meet (Rect.MoveEdge (
                               to, Rect.Edge.S, -z_76.vOptions.leading),
                             z_76.rect.textClip),
                  intervalOptions.whiteBlack.fg);
                FillLeading (intervalOptions.leading);
            | IntervalStyle.GrayStyle =>
                VBT.PaintTint (
                  z_76.vbt,
                  Rect.Meet (Rect.MoveEdge (
                               to, Rect.Edge.S, -z_76.vOptions.leading),
                             z_76.rect.textClip),
                  intervalOptions.whiteBlack.bg);
                IF z_76.vOptions.leading > 0 THEN
                  VBT.PaintTint (
                    z_76.vbt,
                    Rect.Meet (
                      Rect.FromEdges (h0, h, v + charht,
                                      v + charht + z_76.vOptions.leading),
                      z_76.rect.textClip), intervalOptions.leading);
                END;
            | IntervalStyle.UnderlineStyle =>
                IF z_76.vOptions.leading > 0 THEN
                  VBT.PaintTint (
                    z_76.vbt, Rect.Meet (Rect.MoveEdge (
                                           to, Rect.Edge.S,
                                           -(z_76.vOptions.leading + 1)),
                                         z_76.rect.textClip),
                    intervalOptions.whiteBlack.bg);
                  IF NOT (oldAllWhiteBelow
                            AND (intervalOptions.leading.op
                                   = z_76.vOptions.whiteBlack.bg.op))
                       AND (z_76.vOptions.leading > 1) THEN
                    VBT.PaintTint (
                      z_76.vbt,
                      Rect.Meet (Rect.FromEdges (
                                   h0, h, v + charht + 1,
                                   v + charht + z_76.vOptions.leading),
                                 z_76.rect.textClip),
                      intervalOptions.leading);
                  END;
                  VBT.PaintTint (
                    z_76.vbt,
                    Rect.Meet (Rect.FromEdges (
                                 h0, h, v + charht - 1, v + charht + 1),
                               z_76.rect.textClip),
                    intervalOptions.whiteStroke.fg);
                  allWhiteBelow := FALSE;
                ELSE
                  VBT.PaintTint (
                    z_76.vbt, Rect.Meet (Rect.MoveEdge (
                                           to, Rect.Edge.S,
                                           -(z_76.vOptions.leading + 2)),
                                         z_76.rect.textClip),
                    intervalOptions.whiteBlack.bg);
                  FillLeading (intervalOptions.leading);
                  VBT.PaintTint (
                    z_76.vbt,
                    Rect.Meet (
                      Rect.FromEdges (h0, h, v + charht - 2, v + charht),
                      z_76.rect.textClip), intervalOptions.whiteStroke.fg);
                END;
            | IntervalStyle.ThinUnderlineStyle =>
                VBT.PaintTint (
                  z_76.vbt,
                  Rect.Meet (Rect.MoveEdge (to, Rect.Edge.S,
                                            -(z_76.vOptions.leading + 1)),
                             z_76.rect.textClip),
                  intervalOptions.whiteBlack.bg);
                FillLeading (intervalOptions.leading);
                VBT.PaintTint (
                  z_76.vbt,
                  Rect.Meet (
                    Rect.FromEdges (h0, h, v + charht - 1, v + charht),
                    z_76.rect.textClip), intervalOptions.whiteStroke.fg);
            | IntervalStyle.GrayUnderlineStyle =>
                VBT.PaintTint (
                  z_76.vbt,
                  Rect.Meet (Rect.MoveEdge (to, Rect.Edge.S,
                                            -(z_76.vOptions.leading + 1)),
                             z_76.rect.textClip),
                  intervalOptions.whiteBlack.bg);
                FillLeading (intervalOptions.leading);
                VBT.PaintTexture (
                  z_76.vbt,
                  Rect.Meet (
                    Rect.FromEdges (h0, h, v + charht - 1, v + charht),
                    z_76.rect.textClip), intervalOptions.whiteStroke.bgFg,
                  VTTexture.gray, Point.FromCoords (z_76.rect.text.west, v));
            | IntervalStyle.BoxStyle =>
                to := Rect.FromEdges (h0, h, v + 1, v + charht - 1);
                IF atStyleStart THEN
                  to := Rect.MoveEdge (to, Rect.Edge.W, +1);
                END;
                IF atStyleStop THEN
                  to := Rect.MoveEdge (to, Rect.Edge.E, -1);
                END;
                VBT.PaintTint (
                  z_76.vbt, Rect.Meet (to, z_76.rect.textClip),
                  intervalOptions.whiteBlack.bg);
                FillLeading (intervalOptions.leading);
                VBT.PaintTint (
                  z_76.vbt, Rect.Meet (Rect.FromEdges (h0, h, v, v + 1),
                                       z_76.rect.textClip),
                  intervalOptions.whiteStroke.fg);
                VBT.PaintTint (
                  z_76.vbt,
                  Rect.Meet (
                    Rect.FromEdges (h0, h, v + charht - 1, v + charht),
                    z_76.rect.textClip), intervalOptions.whiteStroke.fg);
                IF atStyleStart THEN
                  VBT.PaintTint (
                    z_76.vbt,
                    Rect.Meet (Rect.FromEdges (h0, h0 + 1, v, v + charht),
                               z_76.rect.textClip),
                    intervalOptions.whiteStroke.fg);
                END;
                IF atStyleStop THEN
                  VBT.PaintTint (
                    z_76.vbt,
                    Rect.Meet (Rect.FromEdges (h - 1, h, v, v + charht),
                               z_76.rect.textClip),
                    intervalOptions.whiteStroke.fg);
                END;
            | IntervalStyle.SlugStyle =>
                VBT.PaintTint (
                  z_76.vbt,
                  Rect.Meet (Rect.FromEdges (h0, h, v, v + charht),
                             z_76.rect.textClip),
                  intervalOptions.whiteStroke.fg);
                FillLeading (intervalOptions.leading);
            | IntervalStyle.OverlapStyle =>
                VBT.PaintTexture (
                  z_76.vbt,
                  Rect.Meet (
                    Rect.FromEdges (
                      h0, h, v, v + charht + z_76.vOptions.leading),
                    z_76.rect.textClip), z_76.vOptions.whiteBlack.bgFg,
                  VTTexture.lightGray,
                  Point.FromCoords (z_76.rect.text.west, v));
                allWhiteBelow := FALSE;
            ELSE <* ASSERT(FALSE) *>
            END;
            newWidth := h - z_76.rect.text.west;
          END;
        ELSE
          IF intervalOptions.style # IntervalStyle.NoStyle THEN
            allWhiteBelow := FALSE;
          END;
          newWidth := h - z_76.rect.text.west;
        END;
      END;
    END PaintWhite;
  PROCEDURE FillLeading (tint: Tint) RAISES {} =
    VAR white: BOOLEAN;
    BEGIN
      WITH z_77 = view^ DO
        white := (tint.op = z_77.vOptions.whiteBlack.bg.op);
        IF NOT (oldAllWhiteBelow AND white) THEN
          IF z_77.vOptions.leading > 0 THEN
            VBT.PaintTint (
              z_77.vbt, Rect.Meet (Rect.FromEdges (
                                     h0, h, v + charht,
                                     v + charht + z_77.vOptions.leading),
                                   z_77.rect.textClip), tint);
            IF NOT white THEN allWhiteBelow := FALSE; END;
          END
        END;
      END;
    END FillLeading;
  VAR
    xx      : INTEGER (* Coord *);
    c       : CHAR;
    escape  : ARRAY [0 .. 3] OF CHAR;
    black   : Tint;
    charClip: Rect.T;
  BEGIN
    WITH z_78 = view^ DO
      WITH z_79 = z_78.vScreenFont^ DO
        WITH z_80 = z_79.vScreenFont.vFont^ DO
          charht :=
            z_79.vScreenFont.box.south - z_79.vScreenFont.box.north;
          h0 := h;
          ci0 := 0;
          ci := 0;
          atStyleStop := FALSE;
          WHILE (ci < length) DO
            c := chars [ci];
            IF c IN z_79.vScreenFont.defined THEN
              h := h + z_79.vScreenFont.width [c];
            ELSE
              IF ci > ci0 THEN PaintSub (chars, ci0, ci - ci0); END;
              ci0 := ci + 1;
              atStyleStart := FALSE;
              IF ci0 = length THEN atStyleStop := atStyleStop0; END;
              h0 := h;
              IF c = '\n' THEN
                h := z_78.rect.text.east;
                PaintWhite ();
              ELSIF (c = '\t') AND ('\t' IN z_80.vFont.printable) THEN
                xx := h - z_78.rect.text.west;
                xx := xx + z_79.vScreenFont.width [' ']
                        + z_79.vScreenFont.width ['\t'] - 1;
                xx := xx - xx MOD z_79.vScreenFont.width ['\t'];
                h := xx + z_78.rect.text.west;
                PaintWhite ();
                IF NOT z_80.vFont.whiteTabs THEN
                  CASE intervalOptions.style OF
                  | IntervalStyle.NoStyle, IntervalStyle.OverlapStyle =>
                      black := z_78.vOptions.whiteBlack.fg;
                  | IntervalStyle.InverseStyle =>
                      black := intervalOptions.whiteBlack.bg;
                  ELSE
                    black := intervalOptions.whiteBlack.fg;
                  END;
                  charClip :=
                    Rect.Meet (Rect.FromEdges (h0, h, v, v + charht),
                               z_78.rect.textClip);
                  VBT.PaintTint (
                    z_78.vbt,
                    Rect.Meet (
                      Rect.FromEdges (
                        h0 + 1, h0 + 2, v - z_79.vScreenFont.box.north - 3,
                        v - z_79.vScreenFont.box.north - 1), charClip),
                    black);
                  VBT.PaintTint (
                    z_78.vbt,
                    Rect.Meet (
                      Rect.FromEdges (
                        h0 + 2, h - 1, v - z_79.vScreenFont.box.north - 1,
                        v - z_79.vScreenFont.box.north), charClip), black);
                  VBT.PaintTint (
                    z_78.vbt,
                    Rect.Meet (
                      Rect.FromEdges (
                        h - 1, h, v - z_79.vScreenFont.box.north - 3,
                        v - z_79.vScreenFont.box.north - 1), charClip),
                    black);
                  IF z_79.vScreenFont.box.south < 0 THEN
                    allWhiteBelow := FALSE;
                  END;
                  newWidth := h - z_78.rect.text.west;
                END;
              ELSE
                escape [0] := '\\';
                escape [1] := VAL (ORD (c) DIV 64 + ORD ('0'), CHAR);
                escape [2] := VAL (ORD (c) DIV 8 MOD 8 + ORD ('0'), CHAR);
                escape [3] := VAL (ORD (c) MOD 8 + ORD ('0'), CHAR);
                h := h + z_79.vScreenFont.width [c];
                PaintSub (escape, 0, 4);
              END;
              h0 := h;
            END;
            ci := ci + 1;
          END;
          atStyleStop := atStyleStop0;
          IF ci > ci0 THEN PaintSub (chars, ci0, ci - ci0); END;
        END;
      END;
    END;
  END PaintSegmentOpaque;

PROCEDURE PaintBackgroundTransparent (                view: View;
                                      VAR (* INOUT *) h   : Coord;
                                                      v   : Coord;
                                      READONLY chars : ARRAY OF CHAR;
                                               length: CARDINAL;
                                      READONLY intervalOptions:
                                         IntervalOptions;  
                                      <* UNUSED *>
                                      oldAllWhiteBelow: BOOLEAN;
                                      VAR (* OUT *) allWhiteBelow: BOOLEAN;
                                      oldWidth: Coord;
                                      VAR (* OUT *) newWidth: Coord)
  RAISES {} =
  VAR
    charht : Coord;
    h0     : INTEGER (* Coord *);
    ci, ci0: INTEGER (* CARDINAL *);
  (* PROCEDURE PaintSub( VAR IN chars: ARRAY OF CHAR; start, length:
     CARDINAL) RAISES {}; VAR refpt: Point.T; clip: Rect.T; BEGIN WITH
     view^ DO WITH vScreenFont^ DO WITH vScreenFont.vFont^ DO IF (h0 <
     rect.textClip.east) AND (h > rect.textClip.west) THEN refpt :=
     Point.FromCoords(h0, v - vScreenFont.box.north); CASE
     intervalOptions.style OF | NoStyle: VBT.PaintSub(vbt, rect.textClip,
     refpt, vFont.font, chars, start, length, vOptions.whiteBlack^.bgFg); |
     HighlightStyle: VBT.PaintSub(vbt, rect.textClip, refpt, vFont.font,
     chars, start, length, intervalOptions.whiteBlack^.bgFg); |
     InverseStyle: VBT.PaintSub(vbt, rect.textClip, refpt, vFont.font,
     chars, start, length, intervalOptions.whiteBlack^.fgbg); | GrayStyle:
     VBT.PaintSub(vbt, rect.textClip, refpt, vFont.font, chars, start,
     length, intervalOptions.whiteBlack^.bgFg); | UnderlineStyle: IF
     vOptions.leading > 0 THEN clip := Rect.Meet( Rect.FromEdges(h0, h, v,
     v + charht - 1), rect.textClip); ELSE clip := Rect.Meet(
     Rect.FromEdges(h0, h, v, v + charht - 2), rect.textClip); END;
     VBT.PaintSub(vbt, clip, refpt, vFont.font, chars, start, length,
     intervalOptions.whiteBlack^.bgFg); | ThinUnderlineStyle,
     GrayUnderlineStyle: clip := Rect.Meet(Rect.FromEdges(h0, h, v, v +
     charht - 1), rect.textClip); VBT.PaintSub(vbt, clip, refpt,
     vFont.font, chars, start, length, intervalOptions.whiteBlack^.bgFg); |
     BoxStyle: clip := Rect.FromEdges(h0, h, v + 1, v + charht - 1); clip
     := Rect.Meet(clip, rect.textClip); VBT.PaintSub(vbt, clip, refpt,
     vFont.font, chars, start, length, intervalOptions.whiteBlack^.bgFg); |
     SlugStyle: | OverlapStyle: VBT.PaintTexture(vbt, Rect.Meet(
     Rect.FromEdges(h0, h, v, v + charht + vOptions.leading),
     rect.textClip), vOptions.whiteBlack^.bgFg, VTTexture.lightGray,
     Point.FromCoords(rect.text.west, v)); VBT.PaintSub(vbt, rect.textClip,
     refpt, vFont.font, chars, start, length,
     vOptions.whiteBlack^.transparentFg); allWhiteBelow := FALSE; END; ELSE
     IF intervalOptions.style # NoStyle THEN allWhiteBelow := FALSE; END;
     END; newWidth := h - rect.text.west; END; END; END; END PaintSub; *)
  PROCEDURE PaintWhite () RAISES {} =
    VAR to: Rect.T;
    BEGIN
      WITH z_81 = view^ DO
        IF (h0 < z_81.rect.textClip.east) AND (h > z_81.rect.textClip.west)
          THEN
          to :=
            Rect.FromEdges (h0, h, v, v + charht + z_81.vOptions.leading);
          IF intervalOptions.style = IntervalStyle.NoStyle THEN
            IF h0 < z_81.rect.text.west + oldWidth THEN
              VBT.PaintTint (z_81.vbt, Rect.Meet (to, z_81.rect.textClip),
                             z_81.vOptions.whiteBlack.bg);
            END;
          ELSE
            CASE intervalOptions.style OF
            | IntervalStyle.HighlightStyle =>
                VBT.PaintTint (
                  z_81.vbt,
                  Rect.Meet (Rect.MoveEdge (
                               to, Rect.Edge.S, -z_81.vOptions.leading),
                             z_81.rect.textClip),
                  intervalOptions.whiteBlack.bg);
            | IntervalStyle.InverseStyle =>
                VBT.PaintTint (
                  z_81.vbt,
                  Rect.Meet (Rect.MoveEdge (
                               to, Rect.Edge.S, -z_81.vOptions.leading),
                             z_81.rect.textClip),
                  intervalOptions.whiteBlack.fg);
            | IntervalStyle.GrayStyle =>
                VBT.PaintTint (
                  z_81.vbt,
                  Rect.Meet (Rect.MoveEdge (
                               to, Rect.Edge.S, -z_81.vOptions.leading),
                             z_81.rect.textClip),
                  intervalOptions.whiteBlack.bg);
            | IntervalStyle.UnderlineStyle =>
                IF z_81.vOptions.leading > 0 THEN
                  VBT.PaintTint (
                    z_81.vbt, Rect.Meet (Rect.MoveEdge (
                                           to, Rect.Edge.S,
                                           -(z_81.vOptions.leading + 1)),
                                         z_81.rect.textClip),
                    intervalOptions.whiteBlack.bg);
                ELSE
                  VBT.PaintTint (
                    z_81.vbt, Rect.Meet (Rect.MoveEdge (
                                           to, Rect.Edge.S,
                                           -(z_81.vOptions.leading + 2)),
                                         z_81.rect.textClip),
                    intervalOptions.whiteBlack.bg);
                END;
            | IntervalStyle.GrayUnderlineStyle,
                IntervalStyle.ThinUnderlineStyle =>
                VBT.PaintTint (
                  z_81.vbt,
                  Rect.Meet (Rect.MoveEdge (to, Rect.Edge.S,
                                            -(z_81.vOptions.leading + 1)),
                             z_81.rect.textClip),
                  intervalOptions.whiteBlack.bg);
            | IntervalStyle.BoxStyle =>
                to := Rect.FromEdges (h0, h, v + 1, v + charht - 1);
                VBT.PaintTint (
                  z_81.vbt, Rect.Meet (to, z_81.rect.textClip),
                  intervalOptions.whiteBlack.bg);
            | IntervalStyle.SlugStyle =>
            | IntervalStyle.OverlapStyle =>
                VBT.PaintTexture (
                  z_81.vbt,
                  Rect.Meet (
                    Rect.FromEdges (
                      h0, h, v, v + charht + z_81.vOptions.leading),
                    z_81.rect.textClip), z_81.vOptions.whiteBlack.bgFg,
                  VTTexture.lightGray,
                  Point.FromCoords (z_81.rect.text.west, v));
                allWhiteBelow := FALSE;
            ELSE <* ASSERT(FALSE) *>
            END;
            newWidth := h - z_81.rect.text.west;
          END;
        ELSE
          IF intervalOptions.style # IntervalStyle.NoStyle THEN
            allWhiteBelow := FALSE;
          END;
          newWidth := h - z_81.rect.text.west;
        END;
      END;
    END PaintWhite;
  VAR
    xx      : INTEGER (* Coord *);
    c       : CHAR;
    escape  : ARRAY [0 .. 3] OF CHAR;
    black   : Tint;
    charClip: Rect.T;
  BEGIN
    WITH z_82 = view^ DO
      WITH z_83 = z_82.vScreenFont^ DO
        WITH z_84 = z_83.vScreenFont.vFont^ DO
          charht :=
            z_83.vScreenFont.box.south - z_83.vScreenFont.box.north;
          h0 := h;
          ci0 := 0;
          ci := 0;
          WHILE (ci < length) DO
            c := chars [ci];
            IF c IN z_83.vScreenFont.defined THEN
              h := h + z_83.vScreenFont.width [c];
            ELSE
              IF ci > ci0 THEN
                (* PaintSub(chars, ci0, ci - ci0); *)
                PaintWhite ();
              END;
              ci0 := ci + 1;
              h0 := h;
              IF c = '\n' THEN
                h := z_82.rect.text.east;
                PaintWhite ();
              ELSIF (c = '\t') AND ('\t' IN z_84.vFont.printable) THEN
                xx := h - z_82.rect.text.west;
                xx := xx + z_83.vScreenFont.width [' ']
                        + z_83.vScreenFont.width ['\t'] - 1;
                xx := xx - xx MOD z_83.vScreenFont.width ['\t'];
                h := xx + z_82.rect.text.west;
                PaintWhite ();
                IF NOT z_84.vFont.whiteTabs THEN
                  CASE intervalOptions.style OF
                  | IntervalStyle.NoStyle, IntervalStyle.OverlapStyle =>
                      black := z_82.vOptions.whiteBlack.fg;
                  | IntervalStyle.InverseStyle =>
                      black := intervalOptions.whiteBlack.bg;
                  ELSE
                    black := intervalOptions.whiteBlack.fg;
                  END;
                  charClip :=
                    Rect.Meet (Rect.FromEdges (h0, h, v, v + charht),
                               z_82.rect.textClip);
                  VBT.PaintTint (
                    z_82.vbt,
                    Rect.Meet (
                      Rect.FromEdges (
                        h0 + 1, h0 + 2, v - z_83.vScreenFont.box.north - 3,
                        v - z_83.vScreenFont.box.north - 1), charClip),
                    black);
                  VBT.PaintTint (
                    z_82.vbt,
                    Rect.Meet (
                      Rect.FromEdges (
                        h0 + 2, h - 1, v - z_83.vScreenFont.box.north - 1,
                        v - z_83.vScreenFont.box.north), charClip), black);
                  VBT.PaintTint (
                    z_82.vbt,
                    Rect.Meet (
                      Rect.FromEdges (
                        h - 1, h, v - z_83.vScreenFont.box.north - 3,
                        v - z_83.vScreenFont.box.north - 1), charClip),
                    black);
                  IF z_83.vScreenFont.box.south < 0 THEN
                    allWhiteBelow := FALSE;
                  END;
                  newWidth := h - z_82.rect.text.west;
                END;
              ELSE
                escape [0] := '\\';
                escape [1] := VAL (ORD (c) DIV 64 + ORD ('0'), CHAR);
                escape [2] := VAL (ORD (c) DIV 8 MOD 8 + ORD ('0'), CHAR);
                escape [3] := VAL (ORD (c) MOD 8 + ORD ('0'), CHAR);
                h := h + z_83.vScreenFont.width [c];
                (* PaintSub(escape, 0, 4); *)
                PaintWhite ();
              END;
              h0 := h;
            END;
            ci := ci + 1;
          END;
          IF ci > ci0 THEN
            (* PaintSub(chars, ci0, ci - ci0); *)
            PaintWhite ();
          END;
        END;
      END;
    END;
  END PaintBackgroundTransparent;

PROCEDURE PaintSegmentTransparent (                view  : View;
                                   VAR (* INOUT *) h     : Coord;
                                                   v     : Coord;
                                   READONLY        chars : ARRAY OF CHAR;
                                                   length: CARDINAL;
                                   READONLY intervalOptions: IntervalOptions;
                                     <* UNUSED *>
                                   oldAllWhiteBelow: BOOLEAN;
                                   VAR (* OUT *) allWhiteBelow: BOOLEAN;
                                     <* UNUSED *>
                                   oldWidth: Coord;
                                   VAR (* OUT *) newWidth: Coord) RAISES {} =
  VAR
    charht : Coord;
    h0     : INTEGER (* Coord *);
    ci, ci0: INTEGER (* CARDINAL *);
  PROCEDURE PaintSub (READONLY chars        : ARRAY OF CHAR;
                               start, length: CARDINAL       ) RAISES {} =
    VAR refpt: Point.T;
    BEGIN
      WITH z_85 = view^ DO
        WITH z_86 = z_85.vScreenFont^ DO
          WITH z_87 = z_86.vScreenFont.vFont^ DO
            IF (h0 < z_85.rect.textClip.east)
                 AND (h > z_85.rect.textClip.west) THEN
              refpt :=
                Point.FromCoords (h0, v - z_86.vScreenFont.box.north);
              CASE intervalOptions.style OF
              | IntervalStyle.NoStyle =>
                  VBT.PaintSub (
                    z_85.vbt, z_85.rect.textClip, refpt, z_87.vFont.font,
                    SUBARRAY (chars, start, length),
                    z_85.vOptions.whiteBlack.transparentFg);
              | IntervalStyle.HighlightStyle, IntervalStyle.GrayStyle,
                  IntervalStyle.UnderlineStyle,
                  IntervalStyle.GrayUnderlineStyle,
                  IntervalStyle.ThinUnderlineStyle,
                  IntervalStyle.BoxStyle =>
                  VBT.PaintSub (
                    z_85.vbt, z_85.rect.textClip, refpt, z_87.vFont.font,
                    SUBARRAY (chars, start, length),
                    intervalOptions.whiteBlack.transparentFg);
              | IntervalStyle.InverseStyle =>
                  VBT.PaintSub (
                    z_85.vbt, z_85.rect.textClip, refpt, z_87.vFont.font,
                    SUBARRAY (chars, start, length),
                    intervalOptions.whiteBlack.transparentBg);
              | IntervalStyle.SlugStyle =>
              | IntervalStyle.OverlapStyle =>
                  VBT.PaintSub (
                    z_85.vbt, z_85.rect.textClip, refpt, z_87.vFont.font,
                    SUBARRAY (chars, start, length),
                    z_85.vOptions.whiteBlack.transparentFg);
              END;
            ELSE
              IF intervalOptions.style # IntervalStyle.NoStyle THEN
                allWhiteBelow := FALSE;
              END;
            END;
            newWidth := h - z_85.rect.text.west;
          END;
        END;
      END;
    END PaintSub;
  VAR
    xx      : INTEGER (* Coord *);
    c       : CHAR;
    escape  : ARRAY [0 .. 3] OF CHAR;
    black   : Tint;
    charClip: Rect.T;
  BEGIN
    WITH z_88 = view^ DO
      WITH z_89 = z_88.vScreenFont^ DO
        WITH z_90 = z_89.vScreenFont.vFont^ DO
          charht :=
            z_89.vScreenFont.box.south - z_89.vScreenFont.box.north;
          h0 := h;
          ci0 := 0;
          ci := 0;
          WHILE (ci < length) DO
            c := chars [ci];
            IF c IN z_89.vScreenFont.defined THEN
              h := h + z_89.vScreenFont.width [c];
            ELSE
              IF ci > ci0 THEN PaintSub (chars, ci0, ci - ci0); END;
              ci0 := ci + 1;
              h0 := h;
              IF c = '\n' THEN
                h := z_88.rect.text.east;
              ELSIF (c = '\t') AND ('\t' IN z_90.vFont.printable) THEN
                xx := h - z_88.rect.text.west;
                xx := xx + z_89.vScreenFont.width [' ']
                        + z_89.vScreenFont.width ['\t'] - 1;
                xx := xx - xx MOD z_89.vScreenFont.width ['\t'];
                h := xx + z_88.rect.text.west;
                IF NOT z_90.vFont.whiteTabs THEN
                  CASE intervalOptions.style OF
                  | IntervalStyle.NoStyle, IntervalStyle.OverlapStyle =>
                      black := z_88.vOptions.whiteBlack.fg;
                  | IntervalStyle.InverseStyle =>
                      black := intervalOptions.whiteBlack.bg;
                  ELSE
                    black := intervalOptions.whiteBlack.fg;
                  END;
                  charClip :=
                    Rect.Meet (Rect.FromEdges (h0, h, v, v + charht),
                               z_88.rect.textClip);
                  VBT.PaintTint (
                    z_88.vbt,
                    Rect.Meet (
                      Rect.FromEdges (
                        h0 + 1, h0 + 2, v - z_89.vScreenFont.box.north - 3,
                        v - z_89.vScreenFont.box.north - 1), charClip),
                    black);
                  VBT.PaintTint (
                    z_88.vbt,
                    Rect.Meet (
                      Rect.FromEdges (
                        h0 + 2, h - 1, v - z_89.vScreenFont.box.north - 1,
                        v - z_89.vScreenFont.box.north), charClip), black);
                  VBT.PaintTint (
                    z_88.vbt,
                    Rect.Meet (
                      Rect.FromEdges (
                        h - 1, h, v - z_89.vScreenFont.box.north - 3,
                        v - z_89.vScreenFont.box.north - 1), charClip),
                    black);
                  IF z_89.vScreenFont.box.south < 0 THEN
                    allWhiteBelow := FALSE;
                  END;
                  newWidth := h - z_88.rect.text.west;
                END;
              ELSE
                escape [0] := '\\';
                escape [1] := VAL (ORD (c) DIV 64 + ORD ('0'), CHAR);
                escape [2] := VAL (ORD (c) DIV 8 MOD 8 + ORD ('0'), CHAR);
                escape [3] := VAL (ORD (c) MOD 8 + ORD ('0'), CHAR);
                h := h + z_89.vScreenFont.width [c];
                PaintSub (escape, 0, 4);
              END;
              h0 := h;
            END;
            ci := ci + 1;
          END;
          IF ci > ci0 THEN PaintSub (chars, ci0, ci - ci0); END;
        END;
      END;
    END;
  END PaintSegmentTransparent;

PROCEDURE PaintOverlayTransparent (                view  : View;
                                   VAR (* INOUT *) h     : Coord;
                                                   v     : Coord;
                                                   rd    : Rd.T;
                                                   length: CARDINAL;
                                   READONLY intervalOptions: IntervalOptions;
                                                 oldAllWhiteBelow: BOOLEAN;
                                   VAR (* OUT *) allWhiteBelow   : BOOLEAN;
                                   <* UNUSED *> oldWidth: Coord;
                                   VAR (* OUT *) newWidth: Coord;
                                   atStyleStart, atStyleStop: BOOLEAN)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR
    charht: Coord;
    h0    : INTEGER (* Coord *);
    buff  : ARRAY [0 .. 131] OF CHAR;
  PROCEDURE PaintStroke () RAISES {} =
    VAR refpt: Point.T;
    BEGIN
      WITH z_91 = view^ DO
        WITH z_92 = z_91.vScreenFont^ DO
          IF (h0 < z_91.rect.textClip.east) AND (h > z_91.rect.textClip.west) THEN
            refpt := Point.FromCoords (h0, v - z_92.vScreenFont.box.north);
            CASE intervalOptions.style OF
            | IntervalStyle.NoStyle =>
            | IntervalStyle.HighlightStyle =>
            | IntervalStyle.InverseStyle =>
            | IntervalStyle.GrayStyle =>
            | IntervalStyle.UnderlineStyle =>
                IF z_91.vOptions.leading > 0 THEN
                  VBT.PaintTint (
                    z_91.vbt,
                    Rect.Meet (
                      Rect.FromEdges (h0, h, v + charht - 1, v + charht + 1),
                      z_91.rect.textClip), intervalOptions.whiteStroke.fg);
                  allWhiteBelow := FALSE;
                ELSE
                  VBT.PaintTint (
                    z_91.vbt, Rect.Meet (Rect.FromEdges (
                                           h0, h, v + charht - 2, v + charht),
                                         z_91.rect.textClip),
                    intervalOptions.whiteStroke.fg);
                END;
            | IntervalStyle.ThinUnderlineStyle =>
                VBT.PaintTint (
                  z_91.vbt, Rect.Meet (Rect.FromEdges (
                                         h0, h, v + charht - 1, v + charht),
                                       z_91.rect.textClip),
                  intervalOptions.whiteStroke.fg);
            | IntervalStyle.GrayUnderlineStyle =>
                VBT.PaintTexture (
                  z_91.vbt, Rect.Meet (Rect.FromEdges (
                                         h0, h, v + charht - 1, v + charht),
                                       z_91.rect.textClip),
                  intervalOptions.whiteStroke.bgFg, VTTexture.gray,
                  Point.FromCoords (z_91.rect.text.west, v));
            | IntervalStyle.BoxStyle =>
                VBT.PaintTint (
                  z_91.vbt, Rect.Meet (Rect.FromEdges (h0, h, v, v + 1),
                                       z_91.rect.textClip),
                  intervalOptions.whiteStroke.fg);
                VBT.PaintTint (
                  z_91.vbt, Rect.Meet (Rect.FromEdges (
                                         h0, h, v + charht - 1, v + charht),
                                       z_91.rect.textClip),
                  intervalOptions.whiteStroke.fg);
                IF atStyleStart THEN
                  VBT.PaintTint (
                    z_91.vbt,
                    Rect.Meet (Rect.FromEdges (h0, h0 + 1, v, v + charht),
                               z_91.rect.textClip),
                    intervalOptions.whiteStroke.fg);
                END;
                IF atStyleStop THEN
                  VBT.PaintTint (
                    z_91.vbt,
                    Rect.Meet (Rect.FromEdges (h - 1, h, v, v + charht),
                               z_91.rect.textClip),
                    intervalOptions.whiteStroke.fg);
                END;
            | IntervalStyle.SlugStyle =>
                VBT.PaintTint (
                  z_91.vbt, Rect.Meet (Rect.FromEdges (h0, h, v, v + charht),
                                       z_91.rect.textClip),
                  intervalOptions.whiteStroke.fg);
            | IntervalStyle.OverlapStyle =>
            END;
          ELSE
            IF intervalOptions.style # IntervalStyle.NoStyle THEN
              allWhiteBelow := FALSE;
            END;
          END;
          newWidth := h - z_91.rect.text.west;
        END;
      END;
    END PaintStroke;
  PROCEDURE PaintLeading () RAISES {} =
    VAR refpt: Point.T;
    BEGIN
      WITH z_93 = view^ DO
        WITH z_94 = z_93.vScreenFont^ DO
          IF (h0 < z_93.rect.textClip.east) AND (h > z_93.rect.textClip.west) THEN
            refpt := Point.FromCoords (h0, v - z_94.vScreenFont.box.north);
            CASE intervalOptions.style OF
            | IntervalStyle.NoStyle =>
            | IntervalStyle.HighlightStyle, IntervalStyle.GrayStyle,
                IntervalStyle.GrayUnderlineStyle,
                IntervalStyle.ThinUnderlineStyle, IntervalStyle.BoxStyle,
                IntervalStyle.SlugStyle =>
                FillLeading (intervalOptions.leading);
            | IntervalStyle.InverseStyle =>
                FillLeading (intervalOptions.leading);
            | IntervalStyle.UnderlineStyle =>
                IF z_93.vOptions.leading > 0 THEN
                  IF NOT (oldAllWhiteBelow
                            AND (intervalOptions.leading.op
                                   = z_93.vOptions.whiteBlack.bg.op))
                       AND (z_93.vOptions.leading > 1) THEN
                    VBT.PaintTint (
                      z_93.vbt,
                      Rect.Meet (
                        Rect.FromEdges (h0, h, v + charht + 1,
                                        v + charht + z_93.vOptions.leading),
                        z_93.rect.textClip), intervalOptions.leading);
                    allWhiteBelow := FALSE;
                  END;
                END;
            | IntervalStyle.OverlapStyle =>
            END;
          ELSE
            IF intervalOptions.style # IntervalStyle.NoStyle THEN
              allWhiteBelow := FALSE;
            END;
          END;
          newWidth := h - z_93.rect.text.west;
        END;
      END;
    END PaintLeading;
  PROCEDURE FillLeading (tint: Tint) RAISES {} =
    VAR white: BOOLEAN;
    BEGIN
      WITH z_95 = view^ DO
        white := (tint.op = z_95.vOptions.whiteBlack.bg.op);
        IF NOT (oldAllWhiteBelow AND white) THEN
          IF z_95.vOptions.leading > 0 THEN
            VBT.PaintTint (
              z_95.vbt,
              Rect.Meet (Rect.FromEdges (h0, h, v + charht,
                                         v + charht + z_95.vOptions.leading),
                         z_95.rect.textClip), tint);
            IF NOT white THEN allWhiteBelow := FALSE; END;
          END
        END;
      END;
    END FillLeading;
  VAR
    xx: INTEGER (* Coord *);
    c : CHAR;
  BEGIN
    WITH z_96 = view^ DO
      WITH z_97 = z_96.vScreenFont^ DO
        WITH z_98 = z_97.vScreenFont.vFont^ DO
          charht := z_97.vScreenFont.box.south - z_97.vScreenFont.box.north;
          h0 := h;
          FOR i := 0 TO length - 1 BY NUMBER (buff) DO
            WITH count = Rd.GetSub (rd, SUBARRAY (buff, 0, MIN (NUMBER (buff),
                                                                length - i))) DO
              FOR j := 0 TO count - 1 DO
                c := buff [j];
                IF c = '\n' THEN
                  h := view.rect.text.east;
                ELSIF c = '\t'
                        AND '\t'
                              IN view.vScreenFont.vScreenFont.vFont^.vFont.printable THEN
                  xx := h - view.rect.text.west;
                  xx := xx + view.vScreenFont.vScreenFont.width [' ']
                          + view.vScreenFont.vScreenFont.width ['\t'] - 1;
                  xx := xx - xx MOD view.vScreenFont.vScreenFont.width ['\t'];
                  h := xx + view.rect.text.west;
                ELSE
                  h := h + view.vScreenFont.vScreenFont.width [c];
                END;
              END;               (* FOR *)
              IF h > h0 THEN PaintStroke (); PaintLeading (); END;
            END (* WITH *)
          END (* FOR *) 
        END (* WITH *)
      END (* WITH *) END (* WITH *)
    END PaintOverlayTransparent;

PROCEDURE PaintTurn (view: View; w, n: Coord; turned: BOOLEAN) RAISES {} =
  VAR turnNW: Point.T;
  BEGIN
    turnNW := Point.FromCoords (w, n);
    WITH z_99 = view^ DO
      WITH z_100 = z_99.vScreenFont^ DO
        VBT.PaintTexture (
          z_99.vbt,
          Rect.Meet (Rect.FromCorner (turnNW, z_99.vOptions.turnMargin,
                                      z_100.vScreenFont.box.south
                                        - z_100.vScreenFont.box.north),
                     z_99.rect.clip), z_99.vOptions.whiteStroke.bgFg,
          VTTexture.turn [turned], turnNW);
      END;
    END;
  END PaintTurn;

PROCEDURE Init (view: View) RAISES {} =
  (* Initialize the Real structure of a View. *)
  BEGIN
    WITH z_101 = view^ DO
      z_101.rect.bad := z_101.rect.clip;
      FOR i := 0 TO z_101.nLines - 1 DO
        WITH z_103 = z_101.real.line [i] DO
          z_103.realLine.width := 0;
          z_103.realLine.allWhiteBelow := TRUE;
          z_103.realLine.turned [0] := TriState.False;
          z_103.realLine.turned [1] := TriState.False;
        END;
      END;
      z_101.real.lines := 0;
      z_101.real.dirty := TRUE;
      z_101.real.firstDirty := 0;
      z_101.real.firstAfter := z_101.nLines;
    END;
  END Init;

PROCEDURE Bad (view: View; READONLY bad: Rect.T) RAISES {} =
  (* Invalidate a rectangle in the image; the rectangle will be cleared and
     redrawn at the next Update. *)
  VAR
    b: Rect.T;
  BEGIN
    WITH z_104 = view^ DO
      b := Rect.Meet (bad, z_104.rect.clip);
      IF NOT Rect.IsEmpty (b) THEN
        z_104.rect.bad := Rect.Join (z_104.rect.bad, b);
      END;
      z_104.real.lines := MIN (z_104.real.lines, z_104.nLines);
      (* this is convenient but inelegant *)
    END;
  END Bad;


PROCEDURE Resize (view: View; n: CARDINAL) RAISES {} =
  VAR i: CARDINAL;
  BEGIN
    WITH z_105 = view^ DO
      IF z_105.real.lines < n THEN
        FOR z_106 := z_105.real.lines TO n - 1 DO
          i := z_106;
          WITH z_107 = z_105.real.line[i] DO
            z_107.realLine.width := 0;
            z_107.realLine.turned[0] := TriState.False;
            z_107.realLine.turned[1] := TriState.False;
            z_107.realLine.allWhiteBelow := TRUE;
          END;
        END;
        z_105.real.dirty := TRUE;
        z_105.real.firstDirty
          := MIN (z_105.real.lines, z_105.real.firstDirty);
        z_105.real.firstAfter := MAX (n, z_105.real.firstAfter);
      ELSE
        z_105.real.lines := n;
        z_105.real.firstDirty := MIN (z_105.real.firstDirty, n);
        z_105.real.firstAfter := MIN (z_105.real.firstAfter, n);
      END;
    END;
  END Resize;
(* Utility *)

PROCEDURE Dirtied (view: View; i: LineNo; n: CARDINAL) RAISES {} =
  BEGIN
    WITH z_108 = view^ DO
      z_108.real.dirty := TRUE;
      z_108.real.firstDirty := MIN (z_108.real.firstDirty, i);
      z_108.real.firstAfter := MAX (z_108.real.firstAfter, i + n);
    END;
  END Dirtied;

BEGIN
  boolToTriState[FALSE] := TriState.False;
  boolToTriState[TRUE] := TriState.True;
END VTReal.
