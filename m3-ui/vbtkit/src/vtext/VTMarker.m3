(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified On Tue Jun 16 13:12:41 PDT 1992 by muller *)
(*      modified On Mon Oct 14 22:33:01 PDT 1991 by meehan *)
(* Modified On Mon Nov 12 19:55:55 1990 by jdd *)

(* Management of VT markers. *)

MODULE VTMarker;

IMPORT VTReal;

(* exported operations *)

PROCEDURE New (vt: T; head: Index; READONLY options: MarkerOptions): Marker
  RAISES {} =
  VAR marker: Marker;
  BEGIN
    marker := NEW (Marker);
    marker.vt := vt;
    marker.index := MAX (MIN (head, vt.length), 0);
    marker.options := options;
    marker.state := OnOffState.Off;
    Insert (marker);
    RETURN marker
  END New;

PROCEDURE MakeOptions (whichEnd   : WhichEnd;
                       top, bottom: BOOLEAN;
                       stroke     : Tint      ): MarkerOptions RAISES {} =
  VAR markerOptions: MarkerOptions;
  BEGIN
    markerOptions.whichEnd := whichEnd;
    markerOptions.top := top;
    markerOptions.bottom := bottom;
    markerOptions.stroke := stroke;
    RETURN markerOptions
  END MakeOptions;

PROCEDURE Switch (marker: Marker; state: OnOffState) RAISES {} =
  BEGIN
    IF marker.state # state THEN
      CASE marker.options.whichEnd OF
      | WhichEnd.Left =>
          Invalidate (marker.vt, marker.index,
                      MIN (marker.index + 1, marker.vt.length))
      | WhichEnd.Right =>
          Invalidate (marker.vt, MAX (marker.index - 1, 0), marker.index)
      END;
      marker.state := state
    END
  END Switch;

PROCEDURE Move (marker: Marker; h: Index) RAISES {} =
  VAR old, new: I;
  BEGIN
    old := marker.index;
    new := h;
    marker.index := MAX (MIN (new, marker.vt.length), 0);
    IF marker.state = OnOffState.On THEN
      IF (new # old) THEN
        CASE marker.options.whichEnd OF
        | WhichEnd.Left =>
            Invalidate (marker.vt, old, MIN (old + 1, marker.vt.length));
            Invalidate (marker.vt, new, MIN (new + 1, marker.vt.length))
        | WhichEnd.Right =>
            Invalidate (marker.vt, MAX (old - 1, 0), old);
            Invalidate (marker.vt, MAX (new - 1, 0), new)
        END
      END
    END
  END Move;

PROCEDURE ChangeOptions (marker: Marker; READONLY options: MarkerOptions)
  RAISES {} =
  BEGIN
    IF marker.state = OnOffState.On THEN
      IF options.whichEnd # marker.options.whichEnd THEN
        Invalidate (marker.vt, MAX (marker.index - 1, 0),
                    MIN (marker.index + 1, marker.vt.length))
      ELSE
        CASE marker.options.whichEnd OF
        | WhichEnd.Left =>
            Invalidate (marker.vt, marker.index,
                        MIN (marker.index + 1, marker.vt.length))
        | WhichEnd.Right =>
            Invalidate (marker.vt, MAX (marker.index - 1, 0), marker.index)
        END
      END
    END;
    marker.options := options
  END ChangeOptions;

PROCEDURE Close (marker: Marker) RAISES {} =
  BEGIN
    IF marker.vt = NIL THEN RETURN END;
    Switch (marker, OnOffState.Off);
    Remove (marker)
  END Close;

(* internal VT operations *)

PROCEDURE Fix (vt: T) RAISES {} =
  VAR
    m, mm, mmm: Marker;
    needScan  : BOOLEAN;
  BEGIN
    m := vt.markers;
    needScan := TRUE;
    WHILE needScan DO
      needScan := FALSE;
      m := vt.markers;
      mm := NIL;
      mmm := NIL;
      WHILE m # NIL DO
        IF (mm # NIL) AND (mm.index > m.index) THEN
          IF mmm = NIL THEN
            vt.markers := m;
            mm.next := m.next;
            m.next := mm
          ELSE
            mmm.next := m;
            mm.next := m.next;
            m.next := mm
          END;
          needScan := TRUE;
          mmm := m;
          m := mm.next
        ELSE
          mmm := mm;
          mm := m;
          m := m.next
        END
      END
    END
  END Fix;

PROCEDURE FirstMarker (vt: T; at: I): Marker RAISES {} =
  VAR marker: Marker;
  BEGIN
    marker := vt.markers;
    WHILE marker # NIL DO
      WITH z_27 = marker^ DO
        IF (z_27.state = OnOffState.On) AND (z_27.index >= at) THEN
          RETURN marker
        END;
        marker := z_27.next
      END
    END;
    RETURN NIL
  END FirstMarker;

PROCEDURE NextMarker ( <* UNUSED *>vt: T; VAR (* INOUT *) marker: Marker)
  RAISES {} =
  BEGIN
    marker := marker.next;
    WHILE marker # NIL DO
      WITH z_28 = marker^ DO
        IF z_28.state = OnOffState.On THEN RETURN END;
        marker := z_28.next
      END
    END
  END NextMarker;

(* Internal procedures to manipulate the list of markers. *)

PROCEDURE Insert (marker: Marker) RAISES {} =
  BEGIN
    WITH z_29 = marker^ DO
      z_29.next := z_29.vt.markers;
      z_29.vt.markers := marker
    END
  END Insert;

PROCEDURE Remove (marker: Marker) RAISES {} =
  VAR m: Marker;
  BEGIN
    m := marker.vt.markers;
    IF m = marker THEN
      marker.vt.markers := m.next
    ELSE
      WHILE m.next # marker DO m := m.next END;
      m.next := m.next.next
    END;
    m.next := NIL
  END Remove;

(* utility *)
(************************************************************************)
(* (Utility) *)
(************************************************************************)

PROCEDURE Invalidate (vt: T; b, e: I) RAISES {} =
  BEGIN
    VTReal.Change (vt, b, e, e)
  END Invalidate;

BEGIN
END VTMarker.
