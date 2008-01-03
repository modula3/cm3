(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified On Mon Dec 21 18:39:26 PST 1992 by meehan *)
(*      modified On Tue Jun 16 13:12:42 PDT 1992 by muller *)
(*      Modified On Tue Dec 18 09:18:23 1990 by jdd *)
<* PRAGMA LL *>

(* Management of VT intervals. *)

MODULE VTInterval;

IMPORT VTDef, VTReal;

REVEAL
  Interval = Private BRANDED OBJECT
               vt     : T;
               options: IntervalOptions;
               state                      := OnOffState.Off
             OVERRIDES
               left       := Left;
               right      := Right;
               getOptions := GetOptions
             END;

PROCEDURE Left (i: Interval): I =
  BEGIN
    RETURN i.l
  END Left;

PROCEDURE Right (i: Interval): I =
  BEGIN
    RETURN i.r
  END Right;

PROCEDURE GetOptions (i: Interval): IntervalOptions =
  BEGIN
    RETURN i.options
  END GetOptions;

PROCEDURE ExplodeInterval (READONLY      interval      : Interval;
                           VAR (* OUT *) indexL, indexR: Index;
                           VAR (* OUT *) options       : IntervalOptions;
                           VAR (* OUT *) state         : OnOffState       ) =
  BEGIN
    indexL := interval.l;
    indexR := interval.r;
    options := interval.options;
    state := interval.state;
  END ExplodeInterval;

PROCEDURE New (vt: T; hl, hr: Index; READONLY options: IntervalOptions):
  Interval =
  VAR
    interval := NEW (Interval, vt := vt, l := hl, r := hr, options := options);
  BEGIN
    Insert (interval);
    RETURN interval;
  END New;

PROCEDURE MakeOptions (style                  : IntervalStyle;
                       whiteBlack, whiteStroke: ColorScheme;
                       leading                : Tint           ):
  IntervalOptions  =
  VAR options: IntervalOptions;
  BEGIN
    options.style := style;
    options.whiteBlack := whiteBlack;
    options.whiteStroke := whiteStroke;
    options.leading := leading;
    RETURN options;
  END MakeOptions;

PROCEDURE Switch (interval: Interval; state: OnOffState)
  RAISES {VTDef.Error} =
  VAR vt := interval.vt;
  BEGIN
    LOCK vt.mutex DO
      IF vt.closed THEN
        RAISE VTDef.Error (VTDef.ErrorCode.Closed)
      ELSE
        LockedSwitch (interval, state)
      END
    END
  END Switch;

PROCEDURE LockedSwitch (interval: Interval; state: OnOffState) =
  <* LL = interval.vt.mutex *>
  BEGIN
    IF interval.state # state THEN
      Invalidate (interval.vt, interval.l, interval.r);
      interval.state := state;
    END
  END LockedSwitch;

PROCEDURE Move (interval: Interval; indexL, indexR: Index)
  RAISES {VTDef.Error} =
  VAR vt := interval.vt;
  VAR oldLeft, oldRight, newLeft, newRight: I;
  BEGIN
    LOCK vt.mutex DO
      IF vt.closed THEN RAISE VTDef.Error (VTDef.ErrorCode.Closed) END;
      newLeft := MIN (indexL, interval.vt.length);
      newRight := MIN (indexR, interval.vt.length);
      IF indexL > interval.vt.length THEN indexL := interval.vt.length; END;
      IF indexR > interval.vt.length THEN indexR := interval.vt.length; END;
      IF newLeft > newRight THEN
        RAISE VTDef.Error (VTDef.ErrorCode.IllegalIndex)
      END;
      oldLeft := interval.l;
      oldRight := interval.r;
      IF newLeft = oldLeft AND newRight = oldRight THEN RETURN END;
      interval.l := newLeft;
      interval.r := newRight;
      IF interval.state = OnOffState.On
           AND interval.options.style # IntervalStyle.NoStyle THEN
        IF newLeft >= oldRight OR newRight <= oldLeft THEN
          Invalidate (interval.vt, oldLeft, oldRight);
          Invalidate (interval.vt, newLeft, newRight);
        ELSE
          IF newLeft > oldLeft THEN
            Invalidate (interval.vt, oldLeft, newLeft);
          ELSIF newLeft < oldLeft THEN
            Invalidate (interval.vt, newLeft, oldLeft);
          END;
          IF newRight > oldRight THEN
            Invalidate (interval.vt, oldRight, newRight);
          ELSIF newRight < oldRight THEN
            Invalidate (interval.vt, newRight, oldRight);
          END
        END
      END
    END
  END Move;

PROCEDURE ChangeOptions (interval: Interval; READONLY options: IntervalOptions)
  RAISES {VTDef.Error} =
  VAR vt := interval.vt;
  BEGIN
    LOCK vt.mutex DO
      IF vt.closed THEN
        RAISE VTDef.Error (VTDef.ErrorCode.Closed)
      ELSIF interval.state = OnOffState.On THEN
        Invalidate (interval.vt, interval.l, interval.r);
      END;
      interval.options := options
    END
  END ChangeOptions;

PROCEDURE Delete (interval: Interval) RAISES {VTDef.Error} =
  VAR vt := interval.vt;
  BEGIN
    LOCK vt.mutex DO
      IF vt.closed THEN RAISE VTDef.Error (VTDef.ErrorCode.Closed) END;
      Close (interval)
    END
  END Delete;

PROCEDURE Close (interval: Interval) = <* LL = interval.vt.mutex *>
  BEGIN
    LockedSwitch (interval, OnOffState.Off);
    Remove (interval)
  END Close;

(* internal VT operations *)
(* Fix bubble-sorts the intervals into order by start. *)

PROCEDURE Fix (vt: T) =
  VAR
    i, ii, iii: Interval;
    needScan  : BOOLEAN;
  BEGIN
    i := vt.intervals;
    needScan := TRUE;
    WHILE needScan DO
      needScan := FALSE;
      i := vt.intervals;
      ii := NIL;
      iii := NIL;
      WHILE i # NIL DO
        IF (ii # NIL) AND (ii.l > i.l) THEN
          IF iii = NIL THEN
            vt.intervals := i;
            ii.next := i.next;
            i.next := ii;
          ELSE
            iii.next := i;
            ii.next := i.next;
            i.next := ii;
          END;
          needScan := TRUE;
          iii := i;
          i := ii.next;
        ELSE
          iii := ii;
          ii := i;
          i := i.next;
        END;
      END;
    END;
  END Fix;

PROCEDURE CurrentOptions (view: View; at: I; VAR (*OUT*) from, to: I):
  IntervalOptions =
  VAR
    interval: Interval;
    opt     : IntervalOptions;
  BEGIN
    opt.style := IntervalStyle.NoStyle;
    from := 0;
    to := view.vt.length;
    interval := view.vt.intervals;
    WHILE interval # NIL DO
      IF interval.state = OnOffState.On THEN
        IF (interval.l <= at) THEN from := MAX (interval.l, from); END;
        IF (interval.r <= at) THEN from := MAX (interval.r, from); END;
        IF (at < interval.l) THEN to := MIN (interval.l, to); END;
        IF (at < interval.r) THEN to := MIN (interval.r, to); END;
        IF (interval.l <= at) THEN
          IF (at < interval.r) THEN
            IF opt.style = IntervalStyle.NoStyle THEN
              opt := interval.options;
            ELSIF interval.options.style = IntervalStyle.NoStyle THEN
            ELSIF (opt.style = IntervalStyle.SlugStyle)
                    OR (opt.style = IntervalStyle.OverlapStyle) THEN
            ELSIF (interval.options.style = IntervalStyle.SlugStyle)
                    OR (interval.options.style = IntervalStyle.OverlapStyle) THEN
              opt := interval.options;
            ELSIF view.vOptions.intervalStylePrecedence # NIL THEN
              IF view.vOptions.intervalStylePrecedence [
                   opt.style, interval.options.style] THEN
              ELSIF view.vOptions.intervalStylePrecedence [
                      interval.options.style, opt.style] THEN
                opt := interval.options;
              ELSE
                opt.style := IntervalStyle.OverlapStyle;
              END;
            ELSE
              opt.style := IntervalStyle.OverlapStyle;
            END;
          END;
        ELSE
          RETURN opt;
        END;
      END;
      interval := interval.next;
    END;
    RETURN opt;
  END CurrentOptions;

(* Internal procedures to manipulate the list of intervals. *)

PROCEDURE Insert (interval: Interval) =
  BEGIN
    interval.next := interval.vt.intervals;
    interval.vt.intervals := interval
  END Insert;

PROCEDURE Remove (interval: Interval) =
  VAR i: Interval;
  BEGIN
    i := interval.vt.intervals;
    IF i = interval THEN
      interval.vt.intervals := i.next;
    ELSE
      WHILE i.next # interval DO i := i.next; END;
      i.next := i.next.next;
    END;
    interval.next := NIL
  END Remove;

(************************************************************************)
(*		              (Utility) 				*)
(************************************************************************)

PROCEDURE Invalidate (vt: T; b, e: I) =
  BEGIN
    VTReal.Change (vt, b, e, e);
  END Invalidate;

BEGIN
END VTInterval.
