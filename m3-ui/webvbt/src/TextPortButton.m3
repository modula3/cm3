(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jul  4 10:00:14 PDT 1995 by mhb                      *)

MODULE TextPortButton;

IMPORT VText;

PROCEDURE GetExtents(b: T; VAR (*out*) left, right: CARDINAL) =
  VAR
    l, r: VText.Index;
    unused1: VText.IntervalOptions;
    unused2: VText.OnOffState;
  BEGIN
    VText.ExplodeInterval(b.interval, l, r, unused1, unused2);
    left := l;  right := r;
  END GetExtents;

BEGIN
END TextPortButton.
