(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Tue Feb 11 16:23:36 PST 1992 by muller   *)
(*      modified on Sat Nov 23 23:14:33 PST 1991 by gnelson  *)
(*      modified on Wed Sep 11 15:33:00 PDT 1991 by msm      *)
<*PRAGMA LL*>

MODULE PolyRegion;

IMPORT Rect, Region, RegionRep, Point, Interval;

CONST
  Radix = 8;

REVEAL 
  Private = BRANDED REF ARRAY OF RECORD 
    rgn := Region.Empty; 
    count := Radix;
  END;

PROCEDURE JoinRect(VAR pr: T; READONLY rect: Rect.T) =
  VAR carry: Region.T;
  BEGIN
    IF Rect.IsEmpty(rect) THEN RETURN END;
    IF pr.p = NIL THEN 
      IF Rect.IsEmpty(pr.r) THEN pr.r := rect; RETURN END;
      pr.p := NEW(Private, 1);
      pr.p[0].rgn := Region.FromRect(pr.r)
    END;
    pr.r := Rect.Join(pr.r, rect);
    WITH el = pr.p[0] DO
      el.rgn := Region.JoinRect(rect, el.rgn);
      DEC(el.count);
      IF el.count # 0 THEN RETURN END;
      el.count := Radix;
      carry := el.rgn;
      el.rgn := Region.Empty
    END;
    FOR i := 1 TO LAST(pr.p^) DO
      WITH el = pr.p[i] DO
        el.rgn := Region.Join(carry, el.rgn);
        DEC(el.count);
        IF el.count # 0 THEN RETURN END;
        el.count := Radix;
        carry := el.rgn;
        el.rgn := Region.Empty
      END
    END;
    pr.p := NEW(Private, NUMBER(pr.p^) + 1);
    WITH el = pr.p[LAST(pr.p^)] DO
      el.rgn := carry;
      DEC(el.count)
    END
  END JoinRect;

PROCEDURE JoinRgn(VAR pr: T; READONLY rgn: Region.T) =
  VAR carry: Region.T;
  BEGIN
    IF Region.IsEmpty(rgn) THEN RETURN END;
    IF pr.p = NIL THEN 
      IF Rect.IsEmpty(pr.r) AND Region.IsRect(rgn) THEN
        pr.r := rgn.r;
        RETURN
      END;
      pr.p := NEW(Private, 1);
      pr.p[0].rgn := Region.FromRect(pr.r)
    END;
    pr.r := Rect.Join(pr.r, rgn.r);
    carry := rgn;
    FOR i := 0 TO LAST(pr.p^) DO
      WITH el = pr.p[i] DO
        el.rgn := Region.Join(carry, el.rgn);
        DEC(el.count);
        IF el.count # 0 THEN RETURN END;
        el.count := Radix;
        carry := el.rgn;
        el.rgn := Region.Empty
      END
    END;
    pr.p := NEW(Private, NUMBER(pr.p^) + 1);
    WITH el = pr.p[LAST(pr.p^)] DO
      el.rgn := carry;
      DEC(el.count)
    END
  END JoinRgn;

PROCEDURE ToRegion(READONLY pr: T): Region.T =
  VAR res := Region.Empty;
  BEGIN
    IF pr.p = NIL THEN res.r := pr.r; RETURN res END;
    FOR i := 0 TO LAST(pr.p^) DO
      res := Region.Join(res, pr.p[i].rgn)
    END;
    RETURN res
  END ToRegion;

PROCEDURE OverlapRect(READONLY pr: T; READONLY rect: Rect.T): BOOLEAN =
  BEGIN
    IF pr.p = NIL THEN RETURN Rect.Overlap(pr.r, rect) END;
    IF NOT Rect.Overlap(pr.r, rect) THEN RETURN FALSE END;
    FOR i := 0 TO LAST(pr.p^) DO
      IF Region.OverlapRect(rect, pr.p[i].rgn) THEN RETURN TRUE END
    END;
    RETURN FALSE
  END OverlapRect;

PROCEDURE Complement(READONLY pr: T; READONLY rgn: Region.T): Region.T =
  VAR res := rgn;
  BEGIN
    IF pr.p = NIL THEN 
      RETURN Region.Difference(res, Region.FromRect(pr.r)) 
    END;
    FOR i := 0 TO LAST(pr.p^) DO
      res := Region.Difference(res, pr.p[i].rgn)
    END;
    RETURN res
  END Complement;

PROCEDURE Meet(READONLY pr: T; READONLY rgn: Region.T): Region.T =
  VAR res := Region.Empty;
  BEGIN
    IF pr.p = NIL THEN RETURN Region.MeetRect(pr.r, rgn) END;
    FOR i := 0 TO LAST(pr.p^) DO
      res := Region.Join(res, Region.Meet(rgn, pr.p[i].rgn))
    END;
    RETURN res
  END Meet;

PROCEDURE Factor(READONLY t: Region.T; READONLY r: Rect.T; 
  READONLY delta: Point.T; VAR rl: REF ARRAY OF Rect.T): CARDINAL RAISES {} =
  VAR 
    res, prevrow, currow: CARDINAL := 0;
    lo, hi, i, j, k, prevedge: INTEGER;  
    h: RegionRep.HList; tp: RegionRep.VList;
    rvext, rhext, vext, hext: Interval.T;
    dh := (delta.h <= 0); dv := (delta.v <= 0);
  BEGIN
    IF NOT Rect.Overlap(r, t.r) THEN RETURN 0 END;
    tp := t.p;
    IF t.p = NIL THEN
      IF (rl = NIL) OR (NUMBER(rl^) = 0) THEN
        rl := NEW(REF ARRAY OF Rect.T, 1) 
      END;
      rl[0] := Rect.Meet(r, t.r);
      RETURN 1
    END;
    rvext.lo := r.north;
    rvext.hi := r.south;
    rhext.lo := r.west;
    rhext.hi := r.east;
    IF dv THEN
      lo := 0;
      hi := NUMBER(tp^);
      WHILE lo # hi DO
        i := (lo + hi) DIV 2;
        IF tp[i].v.hi > rvext.lo THEN hi := i ELSE lo := i + 1 END
      END
    ELSE
      lo := -1;
      hi := LAST(tp^);
      WHILE lo # hi DO
        i := (lo + hi + 1) DIV 2;
        IF rvext.hi > tp[i].v.lo THEN lo := i ELSE hi := i - 1 END
      END
    END;
    j := lo;
    WHILE (j >= 0) AND (j < NUMBER(tp^)) AND Overlaps(rvext, tp[j].v, dv) DO
      vext := Interval.Meet(tp[j].v, rvext);
      h := tp[j].h;
      IF dh THEN
        lo := 0;
        hi := NUMBER(h^);
        WHILE lo # hi DO
          i := (lo + hi) DIV 2;
          IF h[i].hi > rhext.lo THEN hi := i ELSE lo := i + 1 END
        END
      ELSE
        lo := -1;
        hi := LAST(h^);
        WHILE lo # hi DO
          i := (lo + hi + 1) DIV 2;
          IF rhext.hi > h[i].lo THEN lo := i ELSE hi := i - 1 END
        END
      END;
      i := lo;
      WHILE (i >= 0) AND (i < NUMBER(h^)) AND Overlaps(rhext, h[i], dh) DO
        hext := Interval.Meet(h[i], rhext);
        IF (rl = NIL) OR (res = NUMBER(rl^)) THEN Extend(rl) END;
        rl[res] := Rect.FromIntervals(hext, vext);
        INC(res);
        Advance(i, dh);
      END;
      IF (res # currow) AND (res - currow = currow - prevrow) AND
         (prevedge = TrailEdge(vext, dv)) THEN
         k := prevrow;
         i := currow;
         WHILE (k # currow) AND (rl[k].west = rl[i].west) AND
           (rl[k].east = rl[i].east) DO
           INC(i);
           INC(k)
         END;
         IF k = currow THEN
           FOR k := prevrow TO currow - 1 DO Merge(rl[k], vext, dv) END;
           res := currow;
           currow := prevrow
         END
      END;
      prevrow := currow;
      currow := res;
      prevedge := LeadEdge(vext, dv);
      Advance(j, dv)
    END;
    RETURN res
  END Factor;

PROCEDURE Extend(VAR rl: REF ARRAY OF Rect.T) =
  VAR new: REF ARRAY OF Rect.T;
  BEGIN
    IF (rl = NIL) OR (NUMBER(rl^) = 0) THEN 
      rl := NEW(REF ARRAY OF Rect.T, 4)
    ELSE
      new := NEW(REF ARRAY OF Rect.T, 2*NUMBER(rl^));
      FOR i := 0 TO LAST(rl^) DO
        new[i] := rl[i]
      END;
      rl := new
    END
  END Extend;

<*INLINE*> PROCEDURE Overlaps(READONLY i, j: Interval.T; d: BOOLEAN): BOOLEAN =
  BEGIN
    IF d THEN
      RETURN i.hi > j.lo
    ELSE
      RETURN j.hi > i.lo
    END
  END Overlaps;

<*INLINE*> PROCEDURE LeadEdge(READONLY i: Interval.T; dv: BOOLEAN): INTEGER =
  BEGIN
    IF dv THEN RETURN i.hi ELSE RETURN i.lo END
  END LeadEdge;

<*INLINE*> PROCEDURE TrailEdge(READONLY i: Interval.T; dv: BOOLEAN): INTEGER =
  BEGIN
    IF dv THEN RETURN i.lo ELSE RETURN i.hi END
  END TrailEdge;

<*INLINE*> PROCEDURE Advance(VAR i: INTEGER; d: BOOLEAN) =
  BEGIN
    IF d THEN INC(i) ELSE DEC(i) END
  END Advance;

<*INLINE*> PROCEDURE Merge(VAR r: Rect.T; READONLY vext: Interval.T;
    dv: BOOLEAN) =
  BEGIN
    IF dv THEN
      r.south := vext.hi
    ELSE
      r.north := vext.lo
    END
  END Merge;

BEGIN
END PolyRegion.

