(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Path.mod, by cgn, mkent and msm, Wed Nov 12 17:58:38 1986 *)
(* Last modified on Fri May  5 10:54:18 PDT 1995 by msm     *)
(*      modified on Thu Jan 19 16:57:00 PST 1995 by kalsow  *)
(*      modified on Wed Apr  6 11:35:34 PDT 1994 by heydon  *)
(*      modified on Wed May 12 13:27:49 PDT 1993 by swart   *)
(*      modified on Tue Feb 11 16:22:05 PST 1992 by muller  *)
(*      modified on Wed Oct 23  0:41:01 PDT 1991 by gnelson *)
(*      modified on Tue Jul 11  9:36:22 PDT 1989 by roberts *)
(*      modified on Fri Sep 16 19:15:52 PDT 1988 by pieper *)
(*      modified on Tue Aug  4 10:26:07 1987 by mkent *)
UNSAFE MODULE Path EXPORTS Path, PathPrivate;
<*PRAGMA LL*>

IMPORT Point, Rect, Word;

CONST InitialSize = 32;

PROCEDURE Freeze(path: T): Lock =
  VAR res: Lock; BEGIN
    IF path.points = NIL THEN RETURN NIL END;
    res := ADR(path.points[0]);
    IF res # path.start THEN
      WITH delta = res - path.start DO
        INC(path.start, delta);
        INC(path.current, delta);
        INC(path.next, delta);
        INC(path.end, delta)
      END
    END;
    RETURN res
  END Freeze;

PROCEDURE Thaw(<*UNUSED*>l: Lock) =
  BEGIN END Thaw;

PROCEDURE ReAllocate(path: T; VAR l: Lock) =
  VAR newPoints: ArrayRef; nl: Lock; BEGIN
    IF path.points = NIL THEN
      newPoints := NEW(ArrayRef, InitialSize);
      nl := ADR(newPoints[0]);
      path.start := nl;
      path.next := nl;
      path.current := nl
    ELSE
      newPoints := NEW(ArrayRef, 2 * NUMBER(path.points^));
      nl := ADR(newPoints[0]);
      SUBARRAY(newPoints^, 0, NUMBER(path.points^)) := path.points^;
      WITH delta = nl - path.start DO
        INC(path.start, delta);
        INC(path.next, delta);
        INC(path.current, delta)
      END
    END;
    path.end := nl + ADRSIZE(Word.T) * NUMBER(newPoints^);
    l := nl;
    path.points := newPoints
  END ReAllocate;

PROCEDURE Reset(path: T) =
  BEGIN
    path.next := path.start;
    path.current := path.start;
    path.curveCount := 0
  END Reset;

CONST 
  LineSize = ADRSIZE (LineRec);
  CurveSize = ADRSIZE (CurveRec);

PROCEDURE MoveTo(path: T; READONLY pt: Point.T) =
  VAR l := Freeze(path); BEGIN
    IF path.end - path.next < LineSize THEN ReAllocate(path, l) END;
    VAR ptr: PLine := path.next; BEGIN
      ptr.ct := Type.Move;
      ptr.p := pt
    END;
    path.current := path.next;
    INC(path.next, LineSize);
    Thaw(l)
  END MoveTo;
      
EXCEPTION FatalError(TEXT); <*FATAL FatalError*>

PROCEDURE LineTo(path: T; READONLY pt: Point.T) =
  VAR l := Freeze(path); BEGIN
    IF path.current = path.next THEN 
      RAISE FatalError("LineTo with no current point")
    END;
    IF path.end - path.next < LineSize THEN ReAllocate(path, l) END;
    VAR ptr: PLine := path.next; BEGIN
      ptr.ct := Type.Line;
      ptr.p := pt
    END;
    INC(path.next, LineSize);
    Thaw(l)
  END LineTo;
      
PROCEDURE Close(path: T) =
  VAR l := Freeze(path); BEGIN
    IF path.current = path.next THEN 
      RAISE FatalError("Close with no current point")
    END;
    IF path.end - path.next < LineSize THEN ReAllocate(path, l) END;
    VAR ptr: PLine := path.next; BEGIN
      ptr.ct := Type.Close;
      ptr.p := LOOPHOLE(path.current, PLine).p
    END;
    INC(path.next, LineSize);
    path.current := path.next;
    Thaw(l)
  END Close;
      
PROCEDURE CurveTo (path: T; READONLY p, q, r: Point.T) =
  VAR l := Freeze(path); BEGIN
    IF path.current = path.next THEN 
      RAISE FatalError("CurveTo with no current point")
    END;
    IF path.end - path.next < CurveSize THEN ReAllocate(path, l) END;
    VAR ptr: PCurve := path.next; BEGIN
      ptr.ct := Type.Curve;
      ptr.p := p;
      ptr.q := q;
      ptr.r := r
    END;
    INC(path.next, CurveSize);
    INC(path.curveCount);
    Thaw(l)
  END CurveTo;

PROCEDURE Map(path: T; map: MapObject) RAISES {Malformed} =
  VAR l := Freeze(path); ptr: PCurve; current: Point.T; BEGIN
    ptr := path.start;
    WHILE ptr < path.next DO
      CASE ptr.ct OF
        Type.Move => 
          map.move(ptr.p); current := ptr.p; INC(ptr, LineSize)
      | Type.Line => 
          map.line(current, ptr.p); 
          current := ptr.p;
          INC(ptr, LineSize)
      | Type.Curve =>
          map.curve(current, ptr.p, ptr.q, ptr.r);
          current := ptr.r;
          INC(ptr, CurveSize)
      | Type.Close =>
          map.close(current, ptr.p);
          INC(ptr, LineSize)
      ELSE
        RAISE Malformed
      END
    END;
    IF ptr # path.next THEN RAISE Malformed END;
    Thaw(l)
  END Map;

PROCEDURE Translate(path: T; READONLY delta: Point.T): T RAISES {Malformed} =
  VAR l := Freeze(path); res := Copy(path); BEGIN
    DTranslate(res, delta);
    Thaw(l);
    RETURN res
  END Translate;
  
PROCEDURE DTranslate(path: T; READONLY delta: Point.T) RAISES {Malformed} =
  VAR ptr: PCurve := path.start; BEGIN
    WHILE ptr < path.next DO
      CASE ptr.ct OF
        Type.Move, Type.Line, Type.Close => 
          ptr.p := Point.Add(ptr.p, delta); INC(ptr, LineSize)
      | Type.Curve =>
          ptr.p := Point.Add(ptr.p, delta);
          ptr.q := Point.Add(ptr.q, delta);
          ptr.r := Point.Add(ptr.r, delta);
          INC(ptr, CurveSize)
      ELSE
        RAISE Malformed
      END
    END;
    IF ptr # path.next THEN RAISE Malformed END
  END DTranslate;

PROCEDURE IsClosed (path: T): BOOLEAN =
  BEGIN
    RETURN path.current = path.next
  END IsClosed;

PROCEDURE IsEmpty (path: T): BOOLEAN =
  BEGIN
    RETURN path.next = path.start
  END IsEmpty;

PROCEDURE CurrentPoint (path: T): Point.T =
  VAR l := Freeze(path); ptr: UNTRACED REF Point.T; res: Point.T; BEGIN
    IF path.next = path.current THEN 
      RAISE FatalError("No currentpoint") 
    END;
    ptr := path.next - ADRSIZE(Point.T);
    res := ptr^;
    Thaw(l);
    RETURN res
  END CurrentPoint;

PROCEDURE Copy (path: T): T =
  BEGIN
    IF path.next # path.start THEN
      (* this code doesn't actually copy the data!
      IF (path.points = NIL) THEN
        RETURN NEW(T, points := NIL, start := path.start, next := path.next,
                   current := path.current, end := path.end,
                   curveCount := path.curveCount);
      END; *)
      WITH l1        = Freeze(path),
           pathWords = (path.next - path.start) DIV ADRSIZE(Word.T),
           res = NEW(
                   T, points := NEW(ArrayRef, MAX(InitialSize, pathWords))),
           l2 = Freeze(res) DO
        res.start := ADR(res.points[0]);
	IF path.points # NIL THEN
          SUBARRAY(res.points^, 0, pathWords) :=
            SUBARRAY(path.points^, 0, pathWords)
	ELSE
	  VAR p,q: UNTRACED REF Word.T;
	  BEGIN
	    p := path.start;
	    q := res.start;
	    FOR i := 1 TO pathWords DO
	      q^ := p^;
	      p := p + ADRSIZE(Word.T);
	      q := q + ADRSIZE(Word.T)
	    END
	  END
	END;
        WITH delta = res.start - path.start DO
          res.next := path.next + delta;
          res.current := path.current + delta
        END;
        res.end := res.start + ADRSIZE(Word.T) * NUMBER(res.points^);
        res.curveCount := path.curveCount;
        Thaw(l1);
        Thaw(l2);
        RETURN res
      END
    ELSE
      RETURN NEW(T)
    END;
  END Copy;

PROCEDURE Flatten(p: T): T RAISES {Malformed} =
  VAR flat: FlatMap;
  BEGIN
    IF p.curveCount = 0 THEN RETURN p END;
    flat := NEW(FlatMap, res := NEW(T));
    Map(p, flat);
    RETURN flat.res
  END Flatten;

TYPE FlatMap = 
  MapObject OBJECT 
    res: T;
  OVERRIDES
    line := FlatLine;
    move := FlatMove;
    close := FlatClose;
    curve := FlatCurve
  END;

PROCEDURE FlatLine(self: FlatMap; <*UNUSED*> 
  READONLY p: Point.T; READONLY q: Point.T) = 
  BEGIN
    LineTo(self.res, q)
  END FlatLine;

PROCEDURE FlatClose(self: FlatMap; <*UNUSED*> READONLY p, q: Point.T) = 
  BEGIN
    Close(self.res)
  END FlatClose;

PROCEDURE FlatMove(self: FlatMap; READONLY q: Point.T) = 
  BEGIN
    MoveTo(self.res, q)
  END FlatMove;

TYPE Bezier = RECORD ph, pv, qh, qv, rh, rv, sh, sv: INTEGER END;

PROCEDURE FlatCurve(self: FlatMap; READONLY pp, qq, rr, ss: Point.T) =
  BEGIN
    NonMonotonicFlatCurve(self, 4*pp.h, 4*pp.v, 4*qq.h, 4*qq.v,
      4*rr.h, 4*rr.v, 4*ss.h, 4*ss.v)
  END FlatCurve;
      
PROCEDURE NonMonotonicFlatCurve(self: FlatMap; 
  ph, pv, qh, qv, rh, rv, sh, sv: INTEGER) =
  VAR st: ARRAY [0..20] OF Bezier; n := 0;
      ah, av, bh, bv, ch, cv, dh, dv, eh, ev, fh, fv: INTEGER;
  BEGIN
    LOOP
      IF ( ph <= qh AND qh <= rh AND rh <= sh
        OR ph >= qh AND qh >= rh AND rh >= sh )
      AND
         ( pv <= qv AND qv <= rv AND rv <= sv
        OR pv >= qv AND qv >= rv AND rv >= sv )
      THEN
        MonotonicFlatCurve(self, ph, pv, qh, qv, rh, rv, sh, sv);
        IF n = 0 THEN RETURN END;
        DEC(n);
        WITH top = st[n] DO
          ph := top.ph; pv := top.pv;
          qh := top.qh; qv := top.qv;
          rh := top.rh; rv := top.rv;
          sh := top.sh; sv := top.sv
        END
      ELSE
        (* subdivide *)
        ah := (ph + qh) DIV 2; av := (pv + qv) DIV 2;
        bh := (qh + rh) DIV 2; bv := (qv + rv) DIV 2;
        ch := (rh + sh) DIV 2; cv := (rv + sv) DIV 2;
        dh := (ah + bh) DIV 2; dv := (av + bv) DIV 2;
        eh := (bh + ch) DIV 2; ev := (bv + cv) DIV 2;
        fh := (dh + eh) DIV 2; fv := (dv + ev) DIV 2;
        IF n = NUMBER(st) THEN
          NonMonotonicFlatCurve(self, ph, pv, ah, av, dh, dv, fh, fv);
          ph := fh; pv := fv; 
          qh := eh; qv := ev; 
          rh := ch; rv := cv
        ELSE
          WITH top = st[n] DO
            top.ph := fh; top.pv := fv;
            top.qh := eh; top.qv := ev; 
            top.rh := ch; top.rv := cv; 
            top.sh := sh; top.sv := sv
          END;
          INC(n);
          qh := ah; qv := av; 
          rh := dh; rv := dv; 
          sh := fh; sv := fv
        END
      END
    END
  END NonMonotonicFlatCurve;

PROCEDURE MonotonicFlatCurve(self: FlatMap; 
    ph, pv, qh, qv, rh, rv, sh, sv: INTEGER) =
  VAR st: ARRAY [0..20] OF Bezier; n := 0;
      res := self.res;  
      ah, av, bh, bv, ch, cv, dh, dv, eh, ev, fh, fv: INTEGER;
  BEGIN
    LOOP
      ah := qh - ph;
      av := qv - pv;
      bh := rh - ph;
      bv := rv - pv;
      ch := sh - ph;
      cv := sv - pv;
      dh := ah * cv - av * ch;
      dv := bh * cv - bv * ch;
      eh := ABS(ch) + ABS(cv);
      IF ABS(dh) <= eh AND ABS(dv) <= eh THEN
        LineTo(res, Point.T{sh DIV 4, sv DIV 4});
        IF n = 0 THEN RETURN END;
        DEC(n);
        WITH top = st[n] DO
          ph := top.ph; pv := top.pv;
          qh := top.qh; qv := top.qv;
          rh := top.rh; rv := top.rv;
          sh := top.sh; sv := top.sv
        END
      ELSE
        (* subdivide *)
        ah := (ph + qh) DIV 2; av := (pv + qv) DIV 2;
        bh := (qh + rh) DIV 2; bv := (qv + rv) DIV 2;
        ch := (rh + sh) DIV 2; cv := (rv + sv) DIV 2;
        dh := (ah + bh) DIV 2; dv := (av + bv) DIV 2;
        eh := (bh + ch) DIV 2; ev := (bv + cv) DIV 2;
        fh := (dh + eh) DIV 2; fv := (dv + ev) DIV 2;
        IF n = NUMBER(st) THEN
          MonotonicFlatCurve(self, ph, pv, ah, av, dh, dv, fh, fv);
          ph := fh; pv := fv; 
          qh := eh; qv := ev; 
          rh := ch; rv := cv
        ELSE
          WITH top = st[n] DO
            top.ph := fh; top.pv := fv;
            top.qh := eh; top.qv := ev; 
            top.rh := ch; top.rv := cv; 
            top.sh := sh; top.sv := sv
          END;
          INC(n);
          qh := ah; qv := av; 
          rh := dh; rv := dv; 
          sh := fh; sv := fv
        END
      END
    END
  END MonotonicFlatCurve;

TYPE
  BBClosure = MapObject OBJECT
    res: Rect.T
  OVERRIDES
    move := BBMove;
    line := BBLine;
    close := BBClose;
    curve := BBCurve
  END;

PROCEDURE BBMove(
    bbc: BBClosure;
    READONLY pt: Point.T) =
  BEGIN
    WITH r = bbc.res DO
      IF Rect.IsEmpty(r) THEN
    	r := Rect.FromPoint(pt)
      ELSE
    	r.west := MIN(r.west, pt.h);
        r.east := MAX(r.east, pt.h+1);
        r.north := MIN(r.north, pt.v);
        r.south := MAX(r.south, pt.v+1)
      END
    END
  END BBMove;

PROCEDURE BBClose(
    <*UNUSED*> bbc: BBClosure;
    <*UNUSED*> READONLY pt1, pt2: Point.T) =
  BEGIN END BBClose;

PROCEDURE BBLine(
    bbc: BBClosure;
    <*UNUSED*> READONLY pt1: Point.T;
    READONLY pt2: Point.T) =
  BEGIN
    WITH r = bbc.res DO
      r.west := MIN(r.west, pt2.h);
      r.east := MAX(r.east, pt2.h+1);
      r.north := MIN(r.north, pt2.v);
      r.south := MAX(r.south, pt2.v+1)
    END
  END BBLine;

PROCEDURE BBCurve(bbc: BBClosure; READONLY p, q, r, s: Point.T) =
  VAR psRect := RectHull(p, s); BEGIN
    IF Rect.Member(q, psRect) AND Rect.Member(r, psRect) THEN
      bbc.res := Rect.Join(bbc.res, psRect)
    ELSE
      VAR ah, av, bh, bv, ch, cv, dh, dv, eh, ev, fh, fv: INTEGER; BEGIN
        (* quadruple and subdivide *)
        ah := (p.h + q.h) * 2; av := (p.v + q.v) * 2;
        bh := (q.h + r.h) * 2; bv := (q.v + r.v) * 2;
        ch := (r.h + s.h) * 2; cv := (r.v + s.v) * 2;
        dh := (ah + bh) DIV 2; dv := (av + bv) DIV 2;
        eh := (bh + ch) DIV 2; ev := (bv + cv) DIV 2;
        fh := (dh + eh) DIV 2; fv := (dv + ev) DIV 2;
        WITH res = bbc.res DO
          JoinPoint(res, p);
          JoinPoint(res, Point.T{ah DIV 4, av DIV 4});
          JoinPoint(res, Point.T{dh DIV 4, dv DIV 4});
          JoinPoint(res, Point.T{fh DIV 4, fv DIV 4});
          JoinPoint(res, Point.T{eh DIV 4, ev DIV 4});
          JoinPoint(res, Point.T{ch DIV 4, cv DIV 4});
          JoinPoint(res, s)
        END
      END
    END
  END BBCurve;

PROCEDURE RectHull(READONLY p, q: Point.T): Rect.T =
  BEGIN
    RETURN Rect.T{
      MIN(p.h, q.h), MAX(p.h, q.h) + 1,
      MIN(p.v, q.v), MAX(p.v, q.v) + 1}
  END RectHull;

PROCEDURE JoinPoint(VAR r: Rect.T; READONLY pt: Point.T) =
(* Requires "NOT Rect.IsEmpty(r)". *)
  BEGIN
    r.west := MIN(r.west, pt.h);
    r.east := MAX(r.east, pt.h + 1);
    r.north := MIN(r.north, pt.v);
    r.south := MAX(r.south, pt.v + 1)
  END JoinPoint;

PROCEDURE BoundingBox(p: T): Rect.T RAISES {Malformed} =
  VAR cl := NEW(BBClosure, res := Rect.Empty); BEGIN
    Map(p, cl);
    RETURN cl.res
  END BoundingBox;

BEGIN
END Path.
