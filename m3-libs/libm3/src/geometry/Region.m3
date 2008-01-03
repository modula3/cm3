(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Feb  9 12:10:05 PST 1993 by msm                      *)
(*      modified on Mon Mar 09 19:36:27 PST 1992 by muller                   *)
(*      modified on Tue Oct 22 21:28:48 PDT 1991 by gnelson                  *)
(*      modified on Tue Dec 11 16:44:31 PST 1990 by steveg                   *)

MODULE Region EXPORTS Region;

IMPORT Word, Axis, Rect, RegionRep, Point, Interval;

TYPE
  HList = RegionRep.HList;(* REF ARRAY OF Interval.T *)
  VList = RegionRep.VList;(* REF ARRAY OF VEntry *)
  RectList = REF ARRAY OF Rect.T;
  RegionList = REF ARRAY OF T;

PROCEDURE FromRect(READONLY r: Rect.T): T RAISES {} =
  VAR res: T;
  BEGIN
    res.r := r;
    res.p := NIL;
    RETURN res
  END FromRect;

PROCEDURE FromPoint(READONLY p: Point.T): T RAISES {} =
  BEGIN
    RETURN FromRect(Rect.FromPoint(p))
  END FromPoint;

PROCEDURE BoundingBox(READONLY t: T): Rect.T RAISES {} =
  BEGIN
    RETURN t.r
  END BoundingBox;

PROCEDURE AddHV(READONLY t: T; dh, dv: INTEGER): T RAISES {} =
  VAR res: T; tp: VList;
  BEGIN
    res.r := Rect.MoveHV(t.r, dh, dv);
    tp := t.p;
    IF tp = NIL THEN
      res.p := NIL
    ELSIF (dv = 0) AND (dh = 0) THEN
      res.p := tp
    ELSE
      res.p :=  NEW(VList, NUMBER(tp^));
      FOR i := 0 TO LAST(tp^) DO
        WITH dest = res.p[i], src = tp[i] DO
          dest.v := Interval.Move(src.v, dv);
          IF dh = 0 THEN
            dest.h := src.h
          ELSE
            dest.h :=  NEW(HList, NUMBER(src.h^));
            FOR j := 0 TO LAST(dest.h^) DO
              dest.h[j] := Interval.Move(src.h[j], dh)
            END
          END
        END
      END
    END;
    RETURN res
  END AddHV;

PROCEDURE Add(READONLY t: T; READONLY p: Point.T): T RAISES {} =
  BEGIN
    RETURN AddHV(t, p.h, p.v)
  END Add;

PROCEDURE AddAxis(READONLY t: T; d: INTEGER; hv: Axis.T): T RAISES {} =
  BEGIN
    IF hv = Axis.T.Hor THEN
      RETURN AddHV(t, d, 0)
    ELSE
      RETURN AddHV(t, 0, d)
    END
  END AddAxis;

PROCEDURE Sub(READONLY t: T; READONLY p: Point.T): T RAISES {} =
  BEGIN
    RETURN AddHV(t, -p.h, -p.v)
  END Sub;

PROCEDURE Inset(READONLY t: T; n: INTEGER): T RAISES {} =
  VAR twoN: INTEGER;
  BEGIN
    twoN := 2 * n + 1;
    IF n < 0 THEN DEC(twoN, 2); n := -n END;
    RETURN AddHV(Place(t, twoN, twoN), n, n)
  END Inset;

TYPE
  BinOp = PROCEDURE(READONLY r1, r2: T): T RAISES {};

PROCEDURE PlaceAxis(READONLY t: T; h: INTEGER; hv: Axis.T): T RAISES {} =
  VAR res, sq: T; p2: INTEGER; meet: BinOp;
  BEGIN
    IF h = 0 THEN RETURN Full END;
    res := t;
    IF h > 0 THEN meet := Meet ELSE h :=  -h; meet := Join END;
    DEC(h);
    sq := t;
    p2 := 1;
    LOOP
      IF Word.And(h, p2) # 0 THEN
        res := meet(AddAxis(res,  -p2, hv), sq);
        DEC(h, p2);
      END;
      IF h = 0 THEN EXIT END;
      sq := meet(sq, AddAxis(sq,  -p2, hv));
      INC(p2, p2)
    END;
    RETURN res
  END PlaceAxis;

PROCEDURE Place(READONLY t: T; h, v: INTEGER): T RAISES {} =
  BEGIN
    RETURN PlaceAxis(PlaceAxis(t, h, Axis.T.Hor), v, Axis.T.Ver)
  END Place;

PROCEDURE FromRects(READONLY r: ARRAY  OF Rect.T): T RAISES {} =
  VAR res: T; 
  BEGIN
    res := Empty;
    FOR i := 0 TO LAST(r) DO res := JoinRect(r[i], res) END;
    RETURN res
  END FromRects;

VAR
  EmptyList: RectList;

PROCEDURE ToRects(READONLY t: T): RectList RAISES {} =
  VAR res: RectList; tp: VList; k: INTEGER;
  BEGIN
    tp := t.p;
    IF Rect.IsEmpty(t.r) THEN
      res := EmptyList;
    ELSIF tp = NIL THEN
      res :=  NEW(RectList, 1);
      res[0] := t.r
    ELSE
      k := 0;
      FOR i := 0 TO LAST(tp^) DO INC(k, NUMBER(tp[i].h^)) END;
      res :=  NEW(RectList, k);
      k := 0;
      FOR i := 0 TO LAST(tp^) DO
        WITH src = tp[i] DO
          FOR j := 0 TO LAST(src.h^) DO
            res[k].north := src.v.lo;
            res[k].south := src.v.hi;
            res[k].west := src.h[j].lo;
            res[k].east := src.h[j].hi;
            INC(k)
          END
        END
      END
    END;
    RETURN res
  END ToRects;

PROCEDURE JoinRect(READONLY r: Rect.T; READONLY t: T): T RAISES {} =
  BEGIN
    RETURN Join(FromRect(r), t)
  END JoinRect;

PROCEDURE MeetRect(READONLY r: Rect.T; READONLY t: T): T RAISES {} =
  BEGIN
    RETURN Meet(FromRect(r), t)
  END MeetRect;

PROCEDURE Flesh(READONLY r: Rect.T): VList RAISES {} =
  VAR rp: VList;
  BEGIN
    rp :=  NEW(VList, 1);
    rp[0].v.lo := r.north;
    rp[0].v.hi := r.south;
    rp[0].h :=  NEW(HList, 1);
    rp[0].h[0].lo := r.west;
    rp[0].h[0].hi := r.east;
    RETURN rp
  END Flesh;

PROCEDURE Skin(rp: VList): VList RAISES {} =
  BEGIN
    IF (rp = NIL) OR ((NUMBER(rp^) = 1) AND (NUMBER(rp[0].h^) = 1)) THEN
      RETURN NIL
    END;
    RETURN rp
  END Skin;

PROCEDURE ComputeBBox(rp: VList): Rect.T RAISES {} =
  VAR res: Rect.T; 
  BEGIN
    IF rp = NIL THEN RETURN Rect.Empty END;
    res.north := rp[0].v.lo;
    res.west := rp[0].h[0].lo;
    res.east := rp[0].h[LAST(rp[0].h^)].hi;
    FOR i := 1 TO LAST(rp^) DO
      WITH src = rp[i] DO
        IF src.h[0].lo < res.west THEN res.west := src.h[0].lo END;
        IF src.h[LAST(src.h^)].hi > res.east THEN
          res.east := src.h[LAST(src.h^)].hi
        END
      END
    END;
    res.south := rp[LAST(rp^)].v.hi;
    RETURN res
  END ComputeBBox;
  
TYPE
  HProc = PROCEDURE(h1, h2: HList): HList RAISES {};

PROCEDURE RegionOp(hp: HProc; rp, sp: VList): VList RAISES {} =
  VAR
    i, j, iv, jv, k, lo, llo: INTEGER;
    ilu, jlu: BOOLEAN;
    ih, jh, kh: HList;
    res, res2: VList;
  BEGIN
    i := 0;
    iv := 0;
    lo := rp[0].v.lo;
    WHILE i < NUMBER(rp^)  DO
      IF rp[i].v.lo # lo THEN INC(iv) END;
      lo := rp[i].v.hi;
      INC(i)
    END;
    iv := NUMBER(rp^) + iv + 1;
    j := 0;
    jv := 0;
    lo := sp[0].v.lo;
    WHILE j < NUMBER(sp^)  DO
      IF sp[j].v.lo # lo THEN INC(jv) END;
      lo := sp[j].v.hi;
      INC(j)
    END;
    jv := NUMBER(sp^) + jv + 1;
    res := NEW(VList, iv + jv - 1);
    i := 0;
    j := 0;
    k := 0;
    ilu := FALSE;
    jlu := FALSE;
    iv := rp[0].v.lo;
    jv := sp[0].v.lo;
    lo := MIN(iv, jv);
    ih := NIL;
    jh := NIL;
    WHILE i < NUMBER(rp^)  OR  j < NUMBER(sp^)  DO
      llo := lo;
      lo := MIN(iv, jv);
      IF (lo > llo) AND ((ih # NIL) OR (jh # NIL)) THEN
        kh := hp(ih, jh);
        IF kh # NIL THEN
          IF (k > 0) AND (res[k - 1].v.hi = llo) AND EqualH(kh, res[k - 1].h)
            THEN
            res[k - 1].v.hi := lo
          ELSE
            res[k].v.lo := llo;
            res[k].v.hi := lo;
            res[k].h := kh;
            INC(k)
          END
        END
      END;
      IF (iv < jv) OR ((iv = jv) AND ilu) THEN
        IF ilu THEN
          INC(i);
          ih := NIL;
          IF i < NUMBER(rp^) THEN
            iv := rp[i].v.lo
          ELSE
            iv := sp[LAST(sp^)].v.hi
          END
        ELSE
          ih := rp[i].h;
          iv := rp[i].v.hi
        END;
        ilu :=  NOT ilu
      ELSE
        IF jlu THEN
          INC(j);
          jh := NIL;
          IF j < NUMBER(sp^) THEN
            jv := sp[j].v.lo
          ELSE
            jv := rp[LAST(rp^)].v.hi
          END
        ELSE
          jh := sp[j].h;
          jv := sp[j].v.hi
        END;
        jlu :=  NOT jlu
      END
    END;
    IF k = 0 THEN RETURN NIL END;
    IF k < NUMBER(res^) THEN
      res2 := res;
      res :=  NEW(VList, k);
      FOR i := 0 TO LAST(res^) DO res[i] := res2[i] END
    END;
    RETURN res
  END RegionOp;
  
(* The following code is a heuristic to get some parallelism.
   threadsStarted and threadsDone are HINTS. When a JoinRegionsInternal
   is called, it looks at the difference to get an idea of the number of
   threads running. If there are not enough, then it makes a new one.
   threadsStarted gets incremented before a new thread is forked.
   threadsDone gets incremented before a thread returns.
   Since they are hints, they can be read and written concurrently. The
   only value that is interesting is their difference. *)

VAR (* CONST *)
  MaxThreads: INTEGER;
  ForkThreshold: INTEGER; (* number of joins before forking *)

VAR
  threadsStarted, threadsDone: INTEGER;

(* ???
TYPE
  JoinRegionsArg = REF RECORD regions: RegionList; firstR, last: INTEGER END;
  JoinRegionsResult = REF T;

PROCEDURE JoinRegionsFork(argAsRefany: REFANY): REFANY =
  VAR result: JoinRegionsResult; arg: JoinRegionsArg;
  BEGIN
    arg := NARROW(argAsRefany, JoinRegionsArg);
    result :=  NEW(JoinRegionsResult);
    WITH z_15 = arg^ DO
      result^ := JoinRegionsInternal(z_15.regions, z_15.firstR, z_15.last);
    END;
    INC(threadsDone);
    RETURN result;
  END JoinRegionsFork;
*)
  
(* merge the regions in the interval [first..last) *)
PROCEDURE JoinRegionsInternal
 (READONLY r: RegionList; first, last: INTEGER): T RAISES {} =
  VAR
    mid: INTEGER;
  BEGIN
    CASE last - first OF
      | 0 => RETURN Empty;
      | 1 => RETURN r[first];
      | 2 => RETURN Join(r[first], r[first + 1]);
      | 3 => RETURN Join(Join(r[first], r[first + 1]), r[first + 2]);
      ELSE
        mid := first +(last - first) DIV 2;
        RETURN
          Join(JoinRegionsInternal(r, first, mid),
                JoinRegionsInternal(r, mid, last));
    END;
  END JoinRegionsInternal;
  
(* for a first go, try to merge the regions pairwise *)
PROCEDURE JoinRegions(READONLY r: RegionList): T RAISES {} =
  BEGIN
    RETURN JoinRegionsInternal(r, 0, NUMBER(r^));
  END JoinRegions;

PROCEDURE Join(READONLY r, s: T): T RAISES {} =
  VAR rp, sp: VList; res: T;
  BEGIN
    IF Subset(r, s) THEN RETURN s END;
    IF Subset(s, r) THEN RETURN r END;
    rp := r.p;
    IF rp = NIL THEN rp := Flesh(r.r) END;
    sp := s.p;
    IF sp = NIL THEN sp := Flesh(s.r) END;
    res.r := Rect.Join(r.r, s.r);
    res.p := Skin(RegionOp(JoinH, rp, sp));
    RETURN res
  END Join;

PROCEDURE JoinH(a, b: HList): HList RAISES {} =
  VAR res: HList; i, j, k, hi: INTEGER;
  BEGIN
    IF SubsetH(a, b) THEN RETURN b END;
    IF SubsetH(b, a) THEN RETURN a END;
    i := 0;
    j := 0;
    k := 1;
    IF a[0].lo < b[0].lo THEN hi := a[0].hi ELSE hi := b[0].hi END;
    WHILE(i < NUMBER(a^)) AND (j < NUMBER(b^)) DO
      IF b[j].lo < a[i].lo THEN
        IF hi < b[j].lo THEN INC(k) END;
        IF hi < b[j].hi THEN hi := b[j].hi END;
        INC(j)
      ELSE
        IF hi < a[i].lo THEN INC(k) END;
        IF hi < a[i].hi THEN hi := a[i].hi END;
        INC(i)
      END
    END;
    WHILE(i < NUMBER(a^)) AND (hi >= a[i].lo) DO
      IF hi < a[i].hi THEN hi := a[i].hi END;
      INC(i)
    END;
    INC(k, NUMBER(a^) - i);
    WHILE(j < NUMBER(b^)) AND (hi >= b[j].lo) DO
      IF hi < b[j].hi THEN hi := b[j].hi END;
      INC(j)
    END;
    INC(k, NUMBER(b^) - j);
    res :=  NEW(HList, k);
    i := 0;
    j := 0;
    k := 0;
    IF a[0].lo < b[0].lo THEN res[0] := a[0] ELSE res[0] := b[0] END;
    WHILE(i < NUMBER(a^)) AND (j < NUMBER(b^)) DO
      IF b[j].lo < a[i].lo THEN
        IF res[k].hi < b[j].lo THEN
          INC(k);
          res[k] := b[j]
        ELSIF res[k].hi < b[j].hi THEN
          res[k].hi := b[j].hi
        END;
        INC(j)
      ELSE
        IF res[k].hi < a[i].lo THEN
          INC(k);
          res[k] := a[i]
        ELSIF res[k].hi < a[i].hi THEN
          res[k].hi := a[i].hi
        END;
        INC(i)
      END
    END;
    WHILE i < NUMBER(a^) DO
      IF res[k].hi < a[i].lo THEN
        INC(k);
        res[k] := a[i]
      ELSIF res[k].hi < a[i].hi THEN
        res[k].hi := a[i].hi
      END;
      INC(i)
    END;
    WHILE j < NUMBER(b^) DO
      IF res[k].hi < b[j].lo THEN
        INC(k);
        res[k] := b[j]
      ELSIF res[k].hi < b[j].hi THEN
        res[k].hi := b[j].hi
      END;
      INC(j)
    END;
    <* ASSERT k = LAST(res^) *> (* Miscounted number of intervals in JoinH *)
    RETURN res
  END JoinH;

PROCEDURE Meet(READONLY r, s: T): T RAISES {} =
  VAR rp, sp: VList; res: T;
  BEGIN
    IF  NOT Rect.Overlap(r.r, s.r) THEN RETURN Empty END;
    IF Subset(r, s) THEN RETURN r END;
    IF Subset(s, r) THEN RETURN s END;
    rp := r.p;
    sp := s.p;
    IF rp = NIL THEN
      IF sp = NIL THEN RETURN FromRect(Rect.Meet(r.r, s.r)) END;
      rp := Flesh(r.r)
    END;
    IF sp = NIL THEN sp := Flesh(s.r) END;
    res.p := RegionOp(MeetH, rp, sp);
    res.r := ComputeBBox(res.p);
    res.p := Skin(res.p);
    RETURN res
  END Meet;

PROCEDURE MeetH(a, b: HList): HList RAISES {} =
  VAR res: HList; i, j, k, hi: INTEGER;
  BEGIN
    IF  NOT OverlapH(a, b) THEN RETURN NIL END;
    IF SubsetH(a, b) THEN RETURN a END;
    IF SubsetH(b, a) THEN RETURN b END;
    i := 0;
    j := 0;
    k := 0;
    hi := MIN(a[0].lo, b[0].lo);
    WHILE(i < NUMBER(a^)) AND (j < NUMBER(b^)) DO
      IF a[i].lo < b[j].lo THEN
        IF a[i].lo < hi THEN INC(k) END;
        IF a[i].hi > hi THEN hi := a[i].hi END;
        INC(i)
      ELSE
        IF b[j].lo < hi THEN INC(k) END;
        IF b[j].hi > hi THEN hi := b[j].hi END;
        INC(j)
      END
    END;
    WHILE(i < NUMBER(a^)) AND (a[i].lo < hi) DO INC(k); INC(i) END;
    WHILE(j < NUMBER(b^)) AND (b[j].lo < hi) DO INC(k); INC(j) END;
    <* ASSERT k # 0 *> (* Strips should overlap *)
    res :=  NEW(HList, k);
    i := 0;
    j := 0;
    k := 0;
    hi := MIN(a[0].lo, b[0].lo);
    WHILE(i < NUMBER(a^)) AND (j < NUMBER(b^)) DO
      IF a[i].lo < b[j].lo THEN
        IF a[i].lo < hi THEN
          res[k].lo := a[i].lo;
          res[k].hi := MIN(hi, a[i].hi);
          INC(k)
        END;
        IF a[i].hi > hi THEN hi := a[i].hi END;
        INC(i)
      ELSE
        IF b[j].lo < hi THEN
          res[k].lo := b[j].lo;
          res[k].hi := MIN(hi, b[j].hi);
          INC(k)
        END;
        IF b[j].hi > hi THEN hi := b[j].hi END;
        INC(j)
      END
    END;
    WHILE(i < NUMBER(a^)) AND (a[i].lo < hi) DO
      res[k].lo := a[i].lo;
      res[k].hi := MIN(hi, a[i].hi);
      INC(k);
      INC(i)
    END;
    WHILE(j < NUMBER(b^)) AND (b[j].lo < hi) DO
      res[k].lo := b[j].lo;
      res[k].hi := MIN(hi, b[j].hi);
      INC(k);
      INC(j)
    END;
    <* ASSERT k = NUMBER(res^) *> (* Miscounted number of intervals in MeetH *)
    RETURN res
  END MeetH;

PROCEDURE Difference(READONLY r, s: T): T RAISES {} =
  VAR rp, sp: VList; res: T;
  BEGIN
    IF  NOT Rect.Overlap(r.r, s.r) OR  NOT Overlap(r, s) THEN RETURN r END;
    IF Subset(r, s) THEN RETURN Empty END;
    rp := r.p;
    sp := s.p;
    IF rp = NIL THEN rp := Flesh(r.r) END;
    IF sp = NIL THEN sp := Flesh(s.r) END;
    res.p := RegionOp(DifferenceH, rp, sp);
    res.r := ComputeBBox(res.p);
    res.p := Skin(res.p);
    RETURN res
  END Difference;

PROCEDURE DifferenceH(a, b: HList): HList RAISES {} =
  VAR res: HList; i, j, k, hi: INTEGER;
  BEGIN
    IF SubsetH(a, b) THEN RETURN NIL END;
    IF  NOT OverlapH(a, b) THEN RETURN a END;
    i := 0;
    j := 0;
    k := 0;
    hi := b[0].lo;
    WHILE(i < NUMBER(a^)) AND (j < NUMBER(b^)) DO
      IF a[i].lo < b[j].hi THEN
        IF a[i].lo < hi THEN INC(k) END;
        IF a[i].hi > hi THEN hi := a[i].hi END;
        INC(i)
      ELSE
        IF b[j].hi < hi THEN INC(k) END;
        INC(j);
        IF (j < NUMBER(b^)) AND (b[j].lo > hi) THEN hi := b[j].lo END;
      END
    END;
    INC(k, NUMBER(a^) - i);
    WHILE(j < NUMBER(b^)) AND (b[j].hi < hi) DO INC(k); INC(j) END;
    <* ASSERT k # 0 *> (* a isn't a subset of b *)
    res :=  NEW(HList, k);
    i := 0;
    j := 0;
    k := 0;
    hi := b[0].lo;
    WHILE(i < NUMBER(a^)) AND (j < NUMBER(b^)) DO
      IF a[i].lo < b[j].hi THEN
        IF a[i].lo < hi THEN
          res[k].lo := a[i].lo;
          res[k].hi := MIN(hi, a[i].hi);
          INC(k)
        END;
        IF a[i].hi > hi THEN hi := a[i].hi END;
        INC(i)
      ELSE
        IF b[j].hi < hi THEN
          res[k].lo := b[j].hi;
          IF j < LAST(b^) THEN
            res[k].hi := MIN(hi, b[j + 1].lo)
          ELSE
            res[k].hi := hi
          END;
          INC(k)
        END;
        INC(j);
        IF (j < NUMBER(b^)) AND (b[j].lo > hi) THEN hi := b[j].lo END;
      END
    END;
    WHILE i < NUMBER(a^) DO res[k] := a[i]; INC(i); INC(k) END;
    WHILE(j < NUMBER(b^)) AND (b[j].hi < hi) DO
      res[k].lo := b[j].hi;
      IF j < LAST(b^) THEN
        res[k].hi := MIN(hi, b[j + 1].lo)
      ELSE
        res[k].hi := hi
      END;
      INC(k);
      INC(j)
    END;
    <* ASSERT k = NUMBER(res^) *>
            (* Miscounted number of intervals in DifferenceH *)
    RETURN res
  END DifferenceH;

PROCEDURE SymmetricDifference(READONLY s, t: T): T RAISES {} =
  BEGIN
    RETURN Join(Difference(s, t), Difference(t, s))
  END SymmetricDifference;

PROCEDURE EqualH(a, b: HList): BOOLEAN RAISES {} =
  BEGIN
    IF a = b THEN RETURN TRUE END;
    IF (a = NIL) OR (b = NIL) OR (NUMBER(a^) # NUMBER(b^)) THEN
      RETURN FALSE
    END;
    FOR i := 0 TO LAST(a^) DO
      IF NOT Interval.Equal(a[i], b[i]) THEN RETURN FALSE END
    END;
    RETURN TRUE
  END EqualH;

PROCEDURE EqualV(a, b: VList): BOOLEAN RAISES {} =
  BEGIN
    IF a = b THEN RETURN TRUE END;
    IF (a = NIL) OR (b = NIL) OR (NUMBER(a^) # NUMBER(b^)) THEN
      RETURN FALSE
    END;
    FOR i := 0 TO LAST(a^) DO
      IF NOT(Interval.Equal(a[i].v, b[i].v) AND EqualH(a[i].h, b[i].h))
        THEN
        RETURN FALSE
      END
    END;
    RETURN TRUE
  END EqualV;

PROCEDURE Equal(READONLY r, s: T): BOOLEAN RAISES {} =
  BEGIN
    RETURN Rect.Equal(r.r, s.r) AND EqualV(r.p, s.p)
  END Equal;

PROCEDURE IsEmpty(READONLY r: T): BOOLEAN RAISES {} =
  BEGIN
    RETURN Rect.IsEmpty(r.r)
  END IsEmpty;

PROCEDURE IsRect(READONLY r: T): BOOLEAN =
  BEGIN RETURN r.p = NIL END IsRect;

PROCEDURE SubsetH(a, b: HList): BOOLEAN RAISES {} =
  VAR i, j: INTEGER;
  BEGIN
    IF (a = NIL) OR (a = b) THEN RETURN TRUE END;
    IF (b = NIL) OR (a[0].lo < b[0].lo) OR (a[LAST(a^)].hi > b[LAST(b^)].hi)
      THEN
      RETURN FALSE
    END;
    IF (NUMBER(b^) = 1) AND (b[0].lo <= a[0].lo)
      AND (b[0].hi >= a[LAST(a^)].hi) THEN
      RETURN TRUE
    END;
    i := 0;
    j := 0;
    WHILE(i < NUMBER(a^)) AND (j < NUMBER(b^)) DO
      IF b[j].hi <= a[i].lo THEN
        INC(j)
      ELSIF (a[i].lo < b[j].lo) OR (a[i].hi > b[j].hi) THEN
        RETURN FALSE
      ELSE
        INC(i)
      END
    END;
    RETURN i = NUMBER(a^)
  END SubsetH;

PROCEDURE Subset(READONLY r, s: T): BOOLEAN RAISES {} =
  VAR i, j: INTEGER; rp, sp: VList;
  BEGIN
    rp := r.p;
    IF rp = NIL THEN RETURN SubsetRect(r.r, s) END;
    IF  NOT Rect.Subset(r.r, s.r) THEN RETURN FALSE END;
    sp := s.p;
    IF sp = NIL THEN RETURN TRUE END;
    i := 0;
    j := 0;
    WHILE(i < NUMBER(rp^)) AND (j < NUMBER(sp^)) DO
      IF rp[i].v.lo < sp[j].v.lo THEN
        RETURN FALSE
      ELSIF sp[j].v.hi <= rp[i].v.lo THEN
        INC(j)
      ELSE
        LOOP
          IF  NOT SubsetH(rp[i].h, sp[j].h) THEN RETURN FALSE END;
          IF (rp[i].v.hi <= sp[j].v.hi) THEN INC(i); EXIT END;
          IF (j = LAST(sp^)) OR (sp[j].v.hi # sp[j + 1].v.lo) THEN
            RETURN FALSE
          END;
          INC(j)
        END
      END
    END;
    RETURN i = NUMBER(rp^)
  END Subset;

PROCEDURE Member(READONLY p: Point.T; READONLY t: T): BOOLEAN RAISES {} =
  BEGIN
    RETURN SubsetRect(Rect.FromPoint(p), t)
  END Member;

PROCEDURE SubsetRect(READONLY r: Rect.T; READONLY t: T): BOOLEAN RAISES {} =
  VAR i, j, lo, hi: INTEGER; h: HList; tp: VList;
  BEGIN
    IF  NOT Rect.Subset(r, t.r) THEN RETURN FALSE END;
    tp := t.p;
    IF Rect.IsEmpty(r) OR (tp = NIL) THEN RETURN TRUE END;
    lo := 0;
    hi := NUMBER(tp^);
    LOOP
      IF lo = hi THEN RETURN FALSE END;
      i :=(lo + hi) DIV 2;
      IF r.north < tp[i].v.lo THEN
        hi := i
      ELSIF r.north >= tp[i].v.hi THEN
        lo := i + 1
      ELSE
        EXIT
      END
    END;
    j := i;
    LOOP
      h := tp[j].h;
      lo := 0;
      hi := NUMBER(h^);
      LOOP
        IF lo = hi THEN RETURN FALSE END;
        i :=(lo + hi) DIV 2;
        IF r.west < h[i].lo THEN
          hi := i
        ELSIF r.west >= h[i].hi THEN
          lo := i + 1
        ELSE
          EXIT
        END
      END;
      IF r.east > h[i].hi THEN RETURN FALSE END;
      IF r.south <= tp[j].v.hi THEN RETURN TRUE END;
      INC(j);
      IF (j > LAST(tp^)) OR (tp[j - 1].v.hi # tp[j].v.lo) THEN
        RETURN FALSE
      END
    END
  END SubsetRect;

PROCEDURE OverlapH(a, b: HList): BOOLEAN RAISES {} =
  VAR i, j: INTEGER;
  BEGIN
    IF (a = NIL) OR (b = NIL) OR (a[LAST(a^)].hi <= b[0].lo)
      OR (b[LAST(b^)].hi <= a[0].lo) THEN
      RETURN FALSE
    END;
    IF a = b THEN RETURN TRUE END;
    i := 0;
    j := 0;
    WHILE(i < NUMBER(a^)) AND (j < NUMBER(b^)) DO
      IF a[i].hi <= b[j].lo THEN
        INC(i)
      ELSIF b[j].hi <= a[i].lo THEN
        INC(j)
      ELSE
        RETURN TRUE
      END
    END;
    RETURN FALSE
  END OverlapH;

PROCEDURE Overlap(READONLY r, s: T): BOOLEAN RAISES {} =
  VAR i, j: INTEGER; rp, sp: VList;
  BEGIN
    IF  NOT Rect.Overlap(r.r, s.r) THEN RETURN FALSE END;
    rp := r.p;
    sp := s.p;
    IF ((rp = NIL) AND (sp = NIL)) OR (rp = sp) THEN RETURN TRUE END;
    IF rp = NIL THEN RETURN OverlapRect(r.r, s) END;
    IF sp = NIL THEN RETURN OverlapRect(s.r, r) END;
    i := 0;
    j := 0;
    WHILE(i < NUMBER(rp^)) AND (j < NUMBER(sp^)) DO
      IF rp[i].v.hi <= sp[j].v.lo THEN
        INC(i)
      ELSIF sp[j].v.hi <= rp[i].v.lo THEN
        INC(j)
      ELSE
        IF OverlapH(rp[i].h, sp[j].h) THEN RETURN TRUE END;
        IF rp[i].v.hi <= sp[j].v.hi THEN INC(i) ELSE INC(j) END
      END
    END;
    RETURN FALSE
  END Overlap;

PROCEDURE OverlapRect(READONLY r: Rect.T; READONLY t: T): BOOLEAN RAISES {} =
  VAR i, j, lo, hi: INTEGER; h: HList; tp: VList;
  BEGIN
    IF  NOT Rect.Overlap(r, t.r) THEN RETURN FALSE END;
    tp := t.p;
    IF tp = NIL THEN RETURN TRUE END;
    lo := 0;
    hi := NUMBER(tp^);
    WHILE lo # hi DO
      i :=(lo + hi) DIV 2;
      IF r.north < tp[i].v.hi THEN hi := i ELSE lo := i + 1 END
    END;
    j := lo;
    WHILE j < NUMBER(tp^) DO
      IF r.south <= tp[j].v.lo THEN RETURN FALSE END;
      h := tp[j].h;
      lo := 0;
      hi := NUMBER(h^);
      WHILE lo # hi DO
        i :=(lo + hi) DIV 2;
        IF r.west < h[i].hi THEN hi := i ELSE lo := i + 1 END
      END;
      IF (lo < NUMBER(h^)) AND (r.east > h[lo].lo) THEN RETURN TRUE END;
      INC(j)
    END;
    RETURN FALSE
  END OverlapRect;

PROCEDURE MaxSubset(READONLY r: Rect.T; READONLY t: T): Rect.T RAISES {} =
  VAR res: Rect.T; i, j, k: INTEGER; tp: VList;
  BEGIN
    IF  NOT Rect.Subset(r, t.r) THEN RETURN Rect.Empty END;
    tp := t.p;
    IF tp = NIL THEN RETURN t.r END;
    i := 0;
    j := NUMBER(tp^);
    WHILE i # j DO
      k :=(i + j) DIV 2;
      IF r.north >= tp[k].v.hi THEN i := k + 1 ELSE j := k END
    END;
    IF (i = NUMBER(tp^))
      OR (r.north < tp[i].v.lo)(* OR (r.north >= tp^[i].v.hi) *) THEN
             (* r.north not contained in a horizontal band or the region *)
      RETURN Rect.Empty
    END;
    j := i + 1;
       (* The bands from i to j-1 are contiguous and contain
           the north boundary of r *)
    WHILE(j # NUMBER(tp^)) AND (r.south > tp[j - 1].v.hi)
      AND (tp[j - 1].v.hi = tp[j].v.lo) DO
      INC(j)
    END;
    IF r.south > tp[j - 1].v.hi THEN RETURN Rect.Empty END;
       (* Bands i to j-1 cover r's vertical extent. *)
    WITH ci = ContainingInterval(Interval.T{r.west,r.east}, tp[i].h) DO
      res.west := ci.lo;
      res.east := ci.hi
    END;
    k := i + 1;
    WHILE k < j DO
      WITH ci = Interval.Meet(Interval.T{res.west,res.east},
                  ContainingInterval(Interval.T{r.west,r.east}, tp[k].h)) DO
        res.west := ci.lo;
        res.east := ci.hi
      END;
      INC(k)
    END;
    IF res.west >= res.east THEN RETURN Rect.Empty END;
    res.north := tp[i].v.lo;
    res.south := tp[j - 1].v.hi;
    DEC(i);
    WHILE (i >= 0) AND (tp[i].v.hi = tp[i + 1].v.lo)
      AND ContainsInterval(Interval.T{res.west,res.east}, tp[i].h) DO
      res.north := tp[i].v.lo;
      DEC(i)
    END;
    WHILE (j < NUMBER(tp^)) AND (tp[j - 1].v.hi = tp[j].v.lo)
      AND ContainsInterval(Interval.T{res.west,res.east}, tp[j].h) DO
      res.south := tp[j].v.hi;
      INC(j)
    END;
    RETURN res
  END MaxSubset;

PROCEDURE Flip (READONLY rg: T; hor, ver: BOOLEAN): T =
  VAR
    res : T;
    j, l: INTEGER;
    hl  : HList;
  BEGIN
    IF NOT hor AND NOT ver THEN RETURN rg END;
    IF hor THEN
      res.r.west := -rg.r.east;
      res.r.east := -rg.r.west
    ELSE
      res.r.west := rg.r.west;
      res.r.east := rg.r.east
    END;
    IF ver THEN
      res.r.north := -rg.r.south;
      res.r.south := -rg.r.north
    ELSE
      res.r.north := rg.r.north;
      res.r.south := rg.r.south
    END;
    IF rg.p = NIL THEN res.p := NIL; RETURN res END;
    res.p := NEW(VList, NUMBER(rg.p^));
    IF ver THEN j := LAST(res.p^) ELSE j := 0 END;
    FOR i := 0 TO LAST(rg.p^) DO
      VAR h := rg.p[i].h;
      BEGIN
        IF hor THEN
          hl := NEW(HList, NUMBER(h^));
          l := LAST(hl^);
          FOR k := 0 TO LAST(h^) DO
            hl[l].lo := -h[k].hi;
            hl[l].hi := -h[k].lo;
            DEC(l)
          END
        ELSE
          hl := h
        END
      END;
      res.p[j].h := hl;
      IF ver THEN
        res.p[j].v.lo := -rg.p[i].v.hi;
        res.p[j].v.hi := -rg.p[i].v.lo;
        DEC(j)
      ELSE
        res.p[j].v := rg.p[j].v;
        INC(j)
      END
    END;
    RETURN res
  END Flip;

PROCEDURE ContainsInterval
 (READONLY x: Interval.T; h: HList): BOOLEAN RAISES {} =
  VAR i, j, k: INTEGER;
  BEGIN
    i := 0;
    j := NUMBER(h^);
    WHILE i # j DO
      k :=(i + j) DIV 2;
      IF h[k].hi > x.lo THEN j := k ELSE i := k + 1 END
    END;
    RETURN (i < NUMBER(h^)) AND (h[i].lo <= x.lo) AND (h[i].hi >= x.hi)
  END ContainsInterval;

PROCEDURE ContainingInterval
 (READONLY x: Interval.T; h: HList): Interval.T RAISES {} =
  VAR i, j, k: INTEGER;
  BEGIN
    i := 0;
    j := NUMBER(h^);
    WHILE i # j DO
      k :=(i + j) DIV 2;
      IF h[k].hi > x.lo THEN j := k ELSE i := k + 1 END
    END;
    IF (i < NUMBER(h^)) AND (h[i].lo <= x.lo) AND (h[i].hi >= x.hi) THEN
      RETURN h[i]
    ELSE
      RETURN Interval.Empty
    END
  END ContainingInterval;

(*???
PROCEDURE Factor
 (READONLY t: T; READONLY r: Rect.T; READONLY delta: Point.T; VAR
   rl: RectList): CARDINAL RAISES {} =
  VAR
    res, prevrow, currow: CARDINAL;
    lo, hi, i, j, k, prevedge: INTEGER;
    h: RegionRep.HList;
    tp: RegionRep.VList;
    rvext, rhext, vext, hext: Interval.T;
    dh, dv: BOOLEAN;
  BEGIN
    IF  NOT Rect.Overlap(r, t.r) THEN RETURN 0 END;
    res := 0;
    prevrow := 0;
    currow := 0;
    dh :=(delta.h <= 0);
    dv :=(delta.v <= 0);
    tp := t.p;
    IF t.p = NIL THEN
      IF (rl = NIL) OR (NUMBER(rl^) = 0) THEN rl :=  NEW(RectList, 1) END;
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
        i :=(lo + hi) DIV 2;
        IF tp[i].v.hi > rvext.lo THEN hi := i ELSE lo := i + 1 END
      END
    ELSE
      lo :=  -1;
      hi := LAST(tp^);
      WHILE lo # hi DO
        i :=(lo + hi + 1) DIV 2;
        IF rvext.hi > tp[i].v.lo THEN lo := i ELSE hi := i - 1 END
      END
    END;
    j := lo;
    WHILE(j >= 0) AND (j < NUMBER(tp^)) AND Overlaps(rvext, tp[j].v, dv) DO
      vext := Interval.Meet(tp[j].v, rvext);
      h := tp[j].h;
      IF dh THEN
        lo := 0;
        hi := NUMBER(h^);
        WHILE lo # hi DO
          i :=(lo + hi) DIV 2;
          IF h[i].hi > rhext.lo THEN hi := i ELSE lo := i + 1 END
        END
      ELSE
        lo := -1;
        hi := LAST(h^);
        WHILE lo # hi DO
          i :=(lo + hi + 1) DIV 2;
          IF rhext.hi > h[i].lo THEN lo := i ELSE hi := i - 1 END
        END
      END;
      i := lo;
      WHILE(i >= 0) AND (i < NUMBER(h^)) AND Overlaps(rhext, h[i], dh) DO
        hext := Interval.Meet(h[i], rhext);
        IF (rl = NIL) OR (res = NUMBER(rl^)) THEN Extend(rl) END;
        rl[res] := Rect.FromIntervals(hext, vext);
        INC(res);
        Advance(i, dh);
      END;
      IF (res # currow) AND (res - currow = currow - prevrow)
        AND (prevedge = TrailEdge(vext, dv)) THEN
        k := prevrow;
        i := currow;
        WHILE(k # currow) AND (rl[k].west = rl[i].west)
          AND (rl[k].east = rl[i].east) DO
          INC(i);
          INC(k)
        END;
        IF k = currow THEN
          FOR l := prevrow TO currow - 1 DO
            Merge(rl[l], vext, dv)
          END;
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

PROCEDURE Extend(VAR rl: RectList) RAISES {} =
  VAR new: RectList; 
  BEGIN
    IF (rl = NIL) OR (NUMBER(rl^) = 0) THEN
      rl :=  NEW(RectList, 4)
    ELSE
      new :=  NEW(RectList, 2 * NUMBER(rl^));
      FOR i := 0 TO LAST(rl^) DO new[i] := rl[i] END;
      rl := new
    END
  END Extend;
  
<*INLINE*>
PROCEDURE Overlaps(READONLY i, j: Interval.T; d: BOOLEAN): BOOLEAN =
  BEGIN
    IF d THEN RETURN i.hi > j.lo ELSE RETURN j.hi > i.lo END
  END Overlaps;
  
<*INLINE*>
PROCEDURE LeadEdge(READONLY i: Interval.T; dv: BOOLEAN): INTEGER RAISES {} =
  BEGIN
    IF dv THEN RETURN i.hi ELSE RETURN i.lo END
  END LeadEdge;
  
<*INLINE*>
PROCEDURE TrailEdge(READONLY i: Interval.T; dv: BOOLEAN): INTEGER RAISES {} =
  BEGIN
    IF dv THEN RETURN i.lo ELSE RETURN i.hi END
  END TrailEdge;
  
<*INLINE*>
PROCEDURE Advance(VAR i: INTEGER; d: BOOLEAN) RAISES {} =
  BEGIN
    IF d THEN INC(i) ELSE DEC(i) END
  END Advance;
  
<*INLINE*>
PROCEDURE Merge
 (VAR r: Rect.T; READONLY vext: Interval.T; dv: BOOLEAN) RAISES {} =
  BEGIN
    IF dv THEN r.south := vext.hi ELSE r.north := vext.lo END
  END Merge;
*)

BEGIN
  EmptyList :=  NEW(RectList, 0);
  threadsStarted := 0;
  threadsDone := 0;
  MaxThreads := 1;
  ForkThreshold := 10;
END Region.

