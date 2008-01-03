(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue May 17 21:13:45 PDT 1994 by mhb                      *)
(*      modified on Tue Jun 16 16:46:22 PDT 1992 by muller                   *)

MODULE RealRect;

IMPORT Interval, RealPoint, Axis, RealInterval, Rect;

<* FATAL Error, RealInterval.Error *>

TYPE
  RefT = REF T;
  PtrT = UNTRACED REF T;
  RefArrayT = REF ARRAY OF T;
  PtrArrayT = UNTRACED REF ARRAY OF T;

PROCEDURE FromEdges (w, e, n, s: REAL): T RAISES {} =
  VAR r: T;
  BEGIN
    IF (w >= e) OR (n >= s) THEN RETURN Empty;  END;
    r.west := w;
    r.east := e;
    r.north := n;
    r.south := s;
    RETURN r;
  END FromEdges;

PROCEDURE FromAbsEdges (w, e, n, s: REAL): T RAISES {} =
  VAR r: T;
  BEGIN
    IF (w = e) OR (n = s) THEN RETURN Empty;  END;
    IF (w < e) THEN
      r.west := w;
      r.east := e;
    ELSE
      r.west := e;
      r.east := w;
    END;
    IF (n < s) THEN
      r.north := n;
      r.south := s;
    ELSE
      r.north := s;
      r.south := n;
    END;
    RETURN r;
  END FromAbsEdges;


PROCEDURE Float(READONLY r: Rect.T): T =
  VAR s: T;
  BEGIN
    IF Rect.IsEmpty(r) THEN RETURN Empty END;
    s.west := RealInterval.Float(Interval.T{r.west, r.east}).lo;
    s.east := RealInterval.Float(Interval.T{r.west, r.east}).hi;
    s.north := RealInterval.Float(Interval.T{r.north, r.south}).lo;
    s.south := RealInterval.Float(Interval.T{r.north, r.south}).hi;
    RETURN s
  END Float;


PROCEDURE Floor(READONLY r: T): Rect.T =
  VAR s: Rect.T;
  BEGIN
    IF (r.west > r.east) OR (r.north > r.south) THEN RETURN Rect.Empty END;
    s.west := RealInterval.Floor(RealInterval.T{r.west, r.east}).lo;
    s.east := RealInterval.Floor(RealInterval.T{r.west, r.east}).hi;
    s.north := RealInterval.Floor(RealInterval.T{r.north, r.south}).lo;
    s.south := RealInterval.Floor(RealInterval.T{r.north, r.south}).hi;
    RETURN s
  END Floor;

PROCEDURE Round(READONLY r: T): Rect.T =
  VAR s: Rect.T;
  BEGIN
    IF (r.west > r.east) OR (r.north > r.south) THEN RETURN Rect.Empty END;
    s.west := RealInterval.Round(RealInterval.T{r.west, r.east}).lo;
    s.east := RealInterval.Round(RealInterval.T{r.west, r.east}).hi;
    s.north := RealInterval.Round(RealInterval.T{r.north, r.south}).lo;
    s.south := RealInterval.Round(RealInterval.T{r.north, r.south}).hi;
    RETURN s
  END Round;

PROCEDURE FromPoint (READONLY p: RealPoint.T): T RAISES {} =
  BEGIN
    RETURN FromEdges (p.h, p.h, p.v, p.v);
  END FromPoint;

PROCEDURE FromCorners (READONLY p, q: RealPoint.T): T RAISES {} =
  BEGIN
    RETURN FromAbsEdges (p.h, q.h, p.v, q.v);
  END FromCorners;

PROCEDURE FromCorner (READONLY p: RealPoint.T;
                      hor, ver: REAL): T RAISES {} =
  VAR r: T;
  BEGIN
    IF (hor < 0.0) OR (ver < 0.0) THEN RETURN Empty;  END;
    r.west := p.h;
    r.east := p.h + hor;
    r.north := p.v;
    r.south := p.v + ver;
    RETURN r;
  END FromCorner;

PROCEDURE FromSize (hor, ver: REAL): T RAISES {} =
  VAR r: T;
  BEGIN
    IF (hor < 0.0) OR (ver < 0.0) THEN RETURN Empty;  END;
    r.west := 0.0;
    r.east := hor;
    r.north := 0.0;
    r.south := ver;
    RETURN r;
  END FromSize;

PROCEDURE Center (READONLY r: T; READONLY p: RealPoint.T): T RAISES {} =
  VAR res: T; h, v: REAL;
  BEGIN
    IF (r.west > r.east) OR (r.north > r.south) THEN RETURN Empty END;
    h := p.h - ((r.west + r.east)/2.0);
    v := p.v - ((r.north + r.south)/2.0);
    res.west := r.west + h;
    res.east := r.east + h;
    res.north := r.north + v;
    res.south := r.south + v;
    RETURN res
  END Center;

PROCEDURE FromIntervals (READONLY hor, ver: RealInterval.T): T RAISES {} =
  VAR r: T;
  BEGIN
    IF (hor.lo > hor.hi) OR (ver.lo > ver.hi) THEN RETURN Empty;  END;
    r.west := hor.lo;
    r.east := hor.hi;
    r.north := ver.lo;
    r.south := ver.hi;
    RETURN r;
  END FromIntervals;

PROCEDURE FromAxes (axis: Axis.T; READONLY n, m: RealInterval.T): T RAISES {} =
  VAR r: T;
  BEGIN
    IF (n.lo > n.hi) OR (m.lo > m.hi) THEN RETURN Empty END;
    IF (axis = Axis.T.Hor) THEN
      r.west := n.lo;
      r.east := n.hi;
      r.north := m.lo;
      r.south := m.hi;
    ELSE
      r.west := m.lo;
      r.east := m.hi;
      r.north := n.lo;
      r.south := n.hi;
    END;
    RETURN r
  END FromAxes;

PROCEDURE NorthWest (READONLY r: T): RealPoint.T RAISES {} =
  VAR p: RealPoint.T;
  BEGIN
    p.h := r.west;
    p.v := r.north;
    RETURN p;
  END NorthWest;

PROCEDURE NorthEast (READONLY r: T): RealPoint.T RAISES {} =
  VAR p: RealPoint.T;
  BEGIN
    p.h := r.east;
    p.v := r.north;
    RETURN p;
  END NorthEast;

PROCEDURE SouthWest (READONLY r: T): RealPoint.T RAISES {} =
  VAR p: RealPoint.T;
  BEGIN
    p.h := r.west;
    p.v := r.south;
    RETURN p;
  END SouthWest;

PROCEDURE SouthEast (READONLY r: T): RealPoint.T RAISES {} =
  VAR p: RealPoint.T;
  BEGIN
    p.h := r.east;
    p.v := r.south;
    RETURN p;
  END SouthEast;

PROCEDURE GetVertex (v: Vertex; READONLY r: T): RealPoint.T RAISES {} =
  BEGIN
    IF (r.west >= r.east)(* OR (r.north>=r.south) *)  THEN
      RETURN RealPoint.Origin;
    END;
    CASE v OF
      | Vertex.NW => RETURN NorthWest (r);
      | Vertex.NE => RETURN NorthEast (r);
      | Vertex.SW => RETURN SouthWest (r);
      | Vertex.SE => RETURN SouthEast (r);
    END;
  END GetVertex;

PROCEDURE HorSize (READONLY r: T): REAL RAISES {} =
  BEGIN
    IF (r.west > r.east) OR (r.north > r.south) THEN RETURN 0.0 END;
    RETURN r.east - r.west;
  END HorSize;

PROCEDURE VerSize (READONLY r: T): REAL RAISES {} =
  BEGIN
    IF (r.west > r.east) OR (r.north > r.south) THEN RETURN 0.0;
    ELSE RETURN r.south - r.north END
  END VerSize;

PROCEDURE Size (a: Axis.T; READONLY r: T): REAL RAISES {} =
  BEGIN
    CASE a OF
      | Axis.T.Hor => RETURN HorSize (r);
      | Axis.T.Ver => RETURN VerSize (r);
    END;
  END Size;

PROCEDURE DiagSizeSquare (READONLY r: T): REAL RAISES {} =
  VAR hor, ver: REAL;
  BEGIN 
    IF (r.west > r.east) OR (r.north > r.south) THEN RETURN 0.0 END;
    hor := HorSize (r);
    ver := VerSize (r);
    RETURN hor * hor + ver * ver;
  END DiagSizeSquare;

PROCEDURE Middle (READONLY r: T): RealPoint.T RAISES {} =
  VAR p: RealPoint.T;
  BEGIN
    IF (r.west > r.east)(* OR (r.north > r.south) *)  THEN
      RETURN RealPoint.Origin;
    END;
    p.h := (r.west + r.east)/2.0;
    p.v := (r.north + r.south)/2.0;
    RETURN p;
  END Middle;

PROCEDURE PickEdge (READONLY r: T; READONLY p: RealPoint.T): Edge RAISES {} =
  VAR mid, se, q: RealPoint.T; a, b: REAL;
  BEGIN
    mid := Middle (r);
    se := RealPoint.Sub (SouthEast (r), mid);
    q := RealPoint.Sub (p, mid);
    a := se.v * q.h;
    b := se.h * q.v;
    IF a >= b THEN
      IF -a >= b THEN RETURN Edge.N ELSE RETURN Edge.E END;
    ELSE
      IF -a >= b THEN RETURN Edge.W ELSE RETURN Edge.S END;
    END;
  END PickEdge;

PROCEDURE PickVertex (READONLY r: T; 
                      READONLY p: RealPoint.T): Vertex RAISES {} =
  VAR q: RealPoint.T;
  BEGIN
    q := RealPoint.Sub (p, Middle (r));
    IF q.h > 0.0 THEN
      IF q.v > 0.0 THEN RETURN Vertex.SE ELSE RETURN Vertex.NE END;
    ELSE
      IF q.v > 0.0 THEN RETURN Vertex.SW ELSE RETURN Vertex.NW END;
    END;
  END PickVertex;

PROCEDURE Project (READONLY r: T; READONLY p: RealPoint.T): RealPoint.T RAISES {} =
  VAR res: RealPoint.T;
  BEGIN
    <* ASSERT r.east > r.west *>
    IF (r.west > r.east) OR (r.north > r.south) THEN RAISE Error END;
    res.h := MAX (MIN (p.h, r.east), r.west);
    res.v := MAX (MIN (p.v, r.south), r.north);
    RETURN res
  END Project;

PROCEDURE Add (READONLY r: T; READONLY p: RealPoint.T): T RAISES {Error} =
  VAR s: T;
  BEGIN
    IF (r.west > r.east) OR (r.north > r.south) THEN RAISE Error END;
    s.west := r.west + p.h;
    s.east := r.east + p.h;
    s.north := r.north + p.v;
    s.south := r.south + p.v;
    RETURN s;
  END Add;

PROCEDURE Sub (READONLY r: T; READONLY p: RealPoint.T): T RAISES {Error} =
  VAR s: T;
  BEGIN
    IF (r.west > r.east) OR (r.north > r.south) THEN RAISE Error END;
    s.west := r.west - p.h;
    s.east := r.east - p.h;
    s.north := r.north - p.v;
    s.south := r.south - p.v;
    RETURN s;
  END Sub;

PROCEDURE Move (READONLY r: T; READONLY p: RealPoint.T): T RAISES {Error} =
  VAR s: T;
  BEGIN
    IF (r.west > r.east) OR (r.north > r.south) THEN RAISE Error END;
    s.west := r.west + p.h;
    s.east := r.east + p.h;
    s.north := r.north + p.v;
    s.south := r.south + p.v;
    RETURN s;
  END Move;

PROCEDURE MoveH (READONLY r: T; h: REAL): T RAISES {Error} =
  VAR s: T;
  BEGIN
    IF (r.west > r.east) OR (r.north > r.south) THEN RAISE Error END;
    s.west := r.west + h;
    s.east := r.east + h;
    s.north := r.north;
    s.south := r.south;
    RETURN s;
  END MoveH;

PROCEDURE MoveV (READONLY r: T; v: REAL): T RAISES {Error} =
  VAR s: T;
  BEGIN
    IF (r.west > r.east) OR (r.north > r.south) THEN RAISE Error END;
    s.west := r.west;
    s.east := r.east;
    s.north := r.north + v;
    s.south := r.south + v;
    RETURN s;
  END MoveV;

PROCEDURE MoveHV (READONLY r: T; h: REAL; v: REAL): T RAISES {Error} =
  VAR s: T;
  BEGIN
    IF (r.west > r.east) OR (r.north > r.south) THEN RAISE Error END;
    s.west := r.west + h;
    s.east := r.east + h;
    s.north := r.north + v;
    s.south := r.south + v;
    RETURN s;
  END MoveHV;

PROCEDURE Scale(READONLY r: T; factor: REAL): T RAISES {} =
  VAR s: T;
  BEGIN
    IF (r.west > r.east) OR (r.north > r.south)
    OR (factor = 0.0) THEN
      RETURN Empty
    END;
    IF factor > 0.0 THEN
      s.north := r.north * factor;
      s.south := r.south * factor;
      s.west := r.west * factor;
      s.east := r.east * factor;
    ELSE
      s.south := r.north * factor;
      s.north := r.south * factor;
      s.east := r.west * factor;
      s.west := r.east * factor;
    END;
    RETURN s
  END Scale;

PROCEDURE Inset (READONLY r: T; n: REAL): T RAISES {} =
  VAR s: T;
  BEGIN
    IF (r.west > r.east) OR (r.north > r.south) THEN RETURN Empty;  END;
    s.west := r.west + n;
    s.east := r.east - n;
    s.north := r.north + n;
    s.south := r.south - n;
    RETURN s;
  END Inset;

PROCEDURE Change (READONLY r: T; dw, de, dn, ds: REAL): T RAISES {} =
  VAR s: T;
  BEGIN
    IF (r.west > r.east) OR (r.north > r.south) THEN RETURN Empty;  END;
    s.west := r.west + dw;
    s.east := r.east + de;
    s.north := r.north + dn;
    s.south := r.south + ds;
    RETURN s;
  END Change;

PROCEDURE MoveEdge (READONLY r: T; e: Edge; dn: REAL): T RAISES {} =
  VAR s: T;
  BEGIN
    s := r;
    CASE e OF
      | Edge.W => s.west := r.west + dn;
      | Edge.E => s.east := r.east + dn;
      | Edge.N => s.north := r.north + dn;
      | Edge.S => s.south := r.south + dn;
    END;
    IF (s.west > s.east) OR (s.north > s.south) THEN RETURN Empty;  END;
    RETURN s;
  END MoveEdge;

PROCEDURE MoveVertex (READONLY r: T; v: Vertex; READONLY dp: RealPoint.T): T
  RAISES {} =
  VAR s: T;
  BEGIN
    s := r;
    CASE v OF
    | Vertex.NW => s.west := r.west + dp.h; s.north := r.north + dp.v;
    | Vertex.NE => s.east := r.east + dp.h; s.north := r.north + dp.v;
    | Vertex.SW => s.west := r.west + dp.h; s.south := r.south + dp.v;
    | Vertex.SE => s.east := r.east + dp.h; s.south := r.south + dp.v;
    END;
    IF (s.west > s.east) OR (s.north > s.south) THEN RETURN Empty;  END;
    RETURN s;
  END MoveVertex;

PROCEDURE Stretch (READONLY r: T; axis: Axis.T; lo, hi: REAL): T RAISES {} =
  VAR res: T;
  BEGIN
    IF (r.west > r.east)(* OR (r.north > r.south) *)  THEN RETURN Empty;  END;
    IF axis = Axis.T.Hor THEN
      res.north := r.north;
      res.south := r.south;
      res.west := lo;
      res.east := hi
    ELSE
      res.north := lo;
      res.south := hi;
      res.west := r.west;
      res.east := r.east
    END;
    RETURN res
  END Stretch;

PROCEDURE Join (READONLY r, s: T): T RAISES {} =
  VAR t: T;
  BEGIN
    IF (r.west > r.east)(* OR (r.north> r.south) *)  THEN RETURN s;  END;
    IF (s.west > s.east)(* OR (s.north> s.south) *)  THEN RETURN r;  END;
    t.west := MIN (r.west, s.west);
    t.east := MAX (r.east, s.east);
    t.north := MIN (r.north, s.north);
    t.south := MAX (r.south, s.south);
    RETURN t;
  END Join;

PROCEDURE Meet (READONLY r, s: T): T RAISES {} =
  VAR t: T;
  BEGIN
    t.west := MAX (r.west, s.west);
    t.east := MIN (r.east, s.east);
    IF t.west > t.east THEN RETURN Empty END;
    t.north := MAX (r.north, s.north);
    t.south := MIN (r.south, s.south);
    IF t.north > t.south THEN RETURN Empty;  END;
    RETURN t;
  END Meet;

PROCEDURE Extend (READONLY r: T; READONLY p: RealPoint.T): T RAISES {} =
  BEGIN
    RETURN Join (r, FromPoint (p));
  END Extend;

PROCEDURE Chop (hv: Axis.T; READONLY r: T; n: REAL; VAR s, t: T) RAISES {} =
  VAR
    si, ti: RealInterval.T;
  BEGIN
    s := r;
    t := r;
    IF hv = Axis.T.Hor THEN
      RealInterval.Chop(
        RealInterval.T{r.west, r.east},
        n,
        si,
        ti
      );
      s.west := si.lo; s.east := si.hi;
      t.west := ti.lo; t.east := ti.hi;
      IF s.east < s.west THEN s := Empty END;
      IF t.east < t.west THEN t := Empty END;
    ELSE
      RealInterval.Chop(
        RealInterval.T{r.north, r.south},
        n,
        si,
        ti
      );
      s.north := si.lo; s.south := si.hi;
      t.north := ti.lo; t.south := ti.hi;   
      IF s.south < s.north THEN s := Empty END;
      IF t.south < t.north THEN t := Empty END;
    END
  END Chop;

(* This is what it really means:
     PROCEDURE Factor(READONLY r, by: T; VAR f: Partition; dh, dv: INTEGER)
     RAISES {}; VAR northIndex, westIndex: INTEGER; mid, temp: T; BEGIN IF dh >
     0 THEN westIndex := 3 ELSE westIndex := 1 END; IF dv > 0 THEN northIndex
     := 4 ELSE northIndex := 0 END; Chop(Axis.Ver, r, by.north, f[northIndex],
     temp); Chop(Axis.Ver, temp, by.south, mid, f[4 - northIndex]);
     Chop(Axis.Hor, mid, by.west, f[westIndex], temp); Chop(Axis.Hor, temp,
     by.east, f[2], f[4 - westIndex]); END Factor;
 *)


PROCEDURE Factor (READONLY r, by: T; 
                  VAR f: Partition;  dh, dv: REAL) RAISES {} =
  VAR ix: INTEGER;
       rw, re, rn, rs, bw, be, bn, bs: REAL;
  BEGIN
    WITH z = by DO
      bw := z.west;
      be := z.east;
      bn := z.north;
      bs := z.south
    END;
    WITH z = r DO
      rw := z.west;
      re := z.east;
      rn := z.north;
      rs := z.south
    END;
    IF (bw > be) OR (bn > bs)
    OR (rw > re) OR (rn > rs)
    OR (be < rw) OR (bw > re) OR (bs < rn) OR (bn > rs) THEN
      (* Disjoint: *)
      f[0] := Empty;
      f[1] := Empty;
      f[2] := r;
      f[3] := Empty;
      f[4] := Empty;
      RETURN
    END;

    (* The rectangles have non-empty intersection *)
    (* Chop parts above and below b, leave rest in rn, rs, rw, re: *)
    IF dv > 0.0 THEN ix := 4 ELSE ix := 0 END;
    IF rn < bn THEN
      WITH z = f[ix] DO
        z.west := rw; z.east := re; z.north := rn; 
        (* z.south := RealExtra.PRED(bn) *)
        z.south := bn
      END;
      rn := bn;
    ELSE
      f[ix] := Empty;
    END;
    IF rs > bs THEN
      WITH z = f[4 - ix] DO
        z.west := rw; z.east := re;
        (* z.north := RealExtra.SUCC(bs); *)
        z.north := bs; 
        z.south := rs
      END;
      rs := bs
    ELSE
      f[4 - ix] := Empty
    END;

    (* Chop parts to the left and right of b: *)
    IF dh > 0.0 THEN ix := 3 ELSE ix := 1 END;
    IF rw < bw THEN
      WITH z = f[ix] DO
        z.north := rn; z.south := rs; z.west := rw;
        (* z.east := RealExtra.PRED(bw) *)
        z.east := bw
      END;
      rw := bw;
    ELSE
      f[ix] := Empty
    END;
    IF re > be THEN
      WITH z = f[4 - ix] DO
        z.north := rn; z.south := rs; 
        (* z.west := RealExtra.SUCC(be); *) 
        z.west := be; 
        z.east := re
      END;
      re := be;
    ELSE
      f[4 - ix] := Empty
    END;
    WITH z = f[2] DO
      z.north := rn; z.south := rs; z.west := rw; z.east := re
    END;
  END Factor;


PROCEDURE Mod (READONLY p: RealPoint.T; READONLY r: T): RealPoint.T 
  RAISES {Error} =
  VAR q: RealPoint.T;
  BEGIN
    IF (r.west > r.east) OR (r.north > r.south) THEN RAISE Error END;
    q.h := RealInterval.Mod(p.h, RealInterval.T{r.west, r.west});
    q.v := RealInterval.Mod(p.v, RealInterval.T{r.north, r.south});
    RETURN q
  END Mod;

PROCEDURE Equal (READONLY r, s: T): BOOLEAN RAISES {} =
  BEGIN
    RETURN
    ((r.west = s.west) AND (r.east = s.east) AND (r.north = s.north)
     AND (r.south = s.south))
    OR (((r.west > r.east) OR (r.north > r.south) )
        AND ((s.west > s.east) OR (s.north > s.south) ))
  END Equal;

PROCEDURE IsEmpty (READONLY r: T): BOOLEAN RAISES {} =
  BEGIN
    RETURN (r.west > r.east) OR (r.north > r.south);
  END IsEmpty;

PROCEDURE Member (READONLY p: RealPoint.T; READONLY r: T): BOOLEAN RAISES {} =
  BEGIN
    RETURN (r.west <= p.h) AND (p.h <= r.east) AND (r.north <= p.v)
    AND (p.v <= r.south);
  END Member;

PROCEDURE Overlap (READONLY r, s: T): BOOLEAN RAISES {} =
  BEGIN
    RETURN (MAX (r.west, s.west) <= MIN (r.east, s.east))
    AND (MAX (r.north, s.north) <= MIN (r.south, s.south));
  END Overlap;

PROCEDURE Subset (READONLY r, s: T): BOOLEAN RAISES {} =
  BEGIN
    RETURN (r.west > r.east)
    OR ((r.west >= s.west) AND (r.east <= s.east) AND (r.north >= s.north)
        AND (r.south <= s.south));
  END Subset;

PROCEDURE GlobToLoc (READONLY r: T; 
                     READONLY p: RealPoint.T): RealPoint.T RAISES {} =
  VAR q: RealPoint.T;
  BEGIN
    q.h := p.h - r.west;
    q.v := p.v - r.north;
    RETURN q;
  END GlobToLoc;

PROCEDURE LocToGlob (READONLY r: T; 
                     READONLY p: RealPoint.T): RealPoint.T RAISES {} =
  VAR q: RealPoint.T;
  BEGIN
    q.h := p.h + r.west;
    q.v := p.v + r.north;
    RETURN q;
  END LocToGlob;

PROCEDURE New (READONLY value: T): RefT =
  VAR r: RefT;
  BEGIN
    r := NEW (RefT);
    r^ := value;
    RETURN r;
  END New;

PROCEDURE NewArray
  (size: CARDINAL; READONLY value: T(* := Empty*)): RefArrayT =
  VAR arr: RefArrayT; i: CARDINAL;
  BEGIN
    arr := NEW (RefArrayT, size);
        (* Assumes the allocator initializes to Empty automatically: *)
        (*IF value # Empty THEN *)
    FOR z := 0 TO size - 1 DO i := z; arr[i] := value END;
        (*END;*)
    RETURN arr
  END NewArray;

PROCEDURE UntracedNew (READONLY value: T): PtrT =
  VAR r: PtrT;
  BEGIN
    r := NEW (PtrT);
    r^ := value;
    RETURN r;
  END UntracedNew;

PROCEDURE UntracedNewArray
  (size: CARDINAL; READONLY value: T(* := Empty*)): PtrArrayT =
  VAR arr: PtrArrayT; i: CARDINAL;
  BEGIN
    arr := NEW (PtrArrayT, size);
        (* Assumes the allocator initializes to Empty automatically: *)
        (*IF value # Empty THEN *)
    FOR z := 0 TO size - 1 DO i := z; arr[i] := value END;
        (*END; *)
    RETURN arr
  END UntracedNewArray;

PROCEDURE Compare (READONLY a, b: T): INTEGER =
  BEGIN
    IF (a.west < b.west) THEN RETURN  -1 END;
    IF (a.west > b.west) THEN RETURN  +1 END;
    IF (a.east < b.east) THEN RETURN  -1 END;
    IF (a.east > b.east) THEN RETURN  +1 END;
    IF (a.north < b.north) THEN RETURN  -1 END;
    IF (a.north > b.north) THEN RETURN  +1 END;
    IF (a.south < b.south) THEN RETURN  -1 END;
    IF (a.south > b.south) THEN RETURN  +1 END;
    RETURN 0;
  END Compare;

PROCEDURE Lt (READONLY a, b: T): BOOLEAN =
  BEGIN
    IF (a.west < b.west) THEN RETURN TRUE END;
    IF (a.west > b.west) THEN RETURN FALSE END;
    IF (a.east < b.east) THEN RETURN TRUE END;
    IF (a.east > b.east) THEN RETURN FALSE END;
    IF (a.north < b.north) THEN RETURN TRUE END;
    IF (a.north > b.north) THEN RETURN FALSE END;
    IF (a.south < b.south) THEN RETURN TRUE END;
    IF (a.south > b.south) THEN RETURN FALSE END;
    RETURN FALSE;
  END Lt;

PROCEDURE Eq (READONLY a, b: T): BOOLEAN =
  BEGIN
    RETURN Equal (a, b);
  END Eq;

PROCEDURE Hash (READONLY a: T): INTEGER =
  BEGIN
    RETURN ROUND(100.0 * a.north * a.south);
  END Hash;

BEGIN
END RealRect.


