(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Jul 26 17:23:37 PDT 1995 by mhb    *)
(*      modified on Fri May 12 01:31:02 PDT 1995 by msm    *)
(*      modified on Tue Aug  4 17:26:49 PDT 1992 by meehan *)
(*      modified on Tue Jun 16 13:08:32 PDT 1992 by muller *)
(*      modified on Fri Mar 27 02:32:50 1992 by steveg *)
<* PRAGMA LL *>

MODULE ScrollerVBT;

IMPORT Axis, PaintOp, ScrollerVBTClass, VBT;

REVEAL
  Private = ScrollerVBTClass.T BRANDED OBJECT END;
  T = Public BRANDED OBJECT
        min, max, value: INTEGER;
        step, stripe   : CARDINAL;
      OVERRIDES
        init       := Init;
        callback   := Callback;
        scroll     := Scroll;
        autoScroll := Auto;
        thumb      := Thumb;
      END;

PROCEDURE Init (v     : T;
                axis  : Axis.T;
                min   : INTEGER;
                max   : INTEGER;
                colors: PaintOp.ColorQuad;
                step  : CARDINAL            := 1;
                thumb : CARDINAL            := 0  ): T =
  BEGIN
    EVAL ScrollerVBTClass.T.init(v, axis, colors);
    max := MAX(min, max);
    v.min := min;
    v.max := max;
    v.value := (min + max) DIV 2;
    v.step := step;
    v.stripe := MIN(thumb, max - min);
    UpdateScroller(v);
    RETURN v
  END Init;

PROCEDURE Callback (<* UNUSED *>          v : T;
                    <* UNUSED *> READONLY cd: VBT.MouseRec) =
  BEGIN
  END Callback;

PROCEDURE Scroll (         v         : T;
                  READONLY cd        : VBT.MouseRec;
                           part      : INTEGER;
                           height    : INTEGER;
                           towardsEOF: BOOLEAN       ) =
  <* LL = VBT.mu *>
  VAR
    total: INTEGER := MAX(part, MAX(1, height - 1));
    delta: INTEGER := MAX(1, (part * v.stripe + (v.stripe DIV 2))
                               DIV total);
  BEGIN
    IF NOT towardsEOF THEN delta := -delta END;
    IF Project(v, v.value + delta) THEN
      UpdateScroller(v);
      v.callback(cd);
    END;
  END Scroll;

PROCEDURE Auto (         v : T;
                READONLY cd: VBT.MouseRec;
                <* UNUSED *> linesToScroll: CARDINAL;
                             towardsEOF   : BOOLEAN   ) =
  <* LL = VBT.mu *>
  VAR delta: INTEGER := v.step;
  BEGIN
    IF NOT towardsEOF THEN delta := -delta END;
    IF Project(v, v.value + delta) THEN
      UpdateScroller(v);
      v.callback(cd);
    END;
  END Auto;

PROCEDURE Thumb (         v     : T;
                 READONLY cd    : VBT.MouseRec;
                          part  : INTEGER;
                          height: INTEGER       ) =
  <* LL = VBT.mu *>
  VAR total := MAX(part, MAX(1, height - 1));
  BEGIN
    IF Project(v, v.min + part * (v.max - v.min) DIV total) THEN
      UpdateScroller(v);
      v.callback(cd);
    END
  END Thumb;

PROCEDURE UpdateScroller (v: T) =
  <* LL = VBT.mu *>
  VAR
    start  := v.value - v.min;
    length := v.max - v.min;
    end    := start + v.stripe;
  BEGIN
    ScrollerVBTClass.Update(v, start, end, length);
  END UpdateScroller;

PROCEDURE Project (v: T; newValue: INTEGER): BOOLEAN =
  VAR oldValue := v.value;
  BEGIN
    v.value := MIN (MAX (newValue, v.min), v.max - v.stripe);
    RETURN oldValue # v.value
  END Project;

PROCEDURE Put (v: T; n: INTEGER) =
  BEGIN
    IF Project(v, n) THEN UpdateScroller(v) END;
  END Put;

PROCEDURE PutBounds (v    : T;
                     min  : INTEGER;
                     max  : INTEGER;
                     thumb: CARDINAL  := 0) =
  BEGIN
    v.min := min;
    v.max := MAX(min, max);
    v.stripe := MIN(thumb, max - min);
    EVAL Project(v, v.value); (* msm -- 5/12/95 *)
    UpdateScroller(v)
  END PutBounds;

PROCEDURE PutStep (v: T; step: CARDINAL) =
  BEGIN
    v.step := step
  END PutStep;

PROCEDURE Get (v: T): INTEGER =
  BEGIN
    RETURN v.value
  END Get;

PROCEDURE GetMin (v: T): INTEGER =
  BEGIN
    RETURN v.min
  END GetMin;

PROCEDURE GetMax (v: T): INTEGER =
  BEGIN
    RETURN v.max
  END GetMax;

PROCEDURE GetThumb (v: T): CARDINAL =
  BEGIN
    RETURN v.stripe
  END GetThumb;

PROCEDURE GetStep (v: T): CARDINAL =
  BEGIN
    RETURN v.step
  END GetStep;

BEGIN
END ScrollerVBT.


