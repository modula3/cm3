(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(*    Last modified on Wed May 12 12:16:37 PDT 1993 by swart   *)
(*         modified on Mon Jun 29 22:09:37 PDT 1992 by muller  *)
(*         modified on Tue Nov 19 17:48:57 PST 1991 by gnelson *)
(*         modified on Mon Oct  2 10:25:21 1989 by kalsow      *)
(*         modified on Mon Jul 17 01:28:11 1989 by stolfi      *)
(*         modified on Thu Aug 13 18:26:15 PDT 1987 by luca    *)
(*         modified on Thu Mar 5 17:23:12 1987 by msm          *)

MODULE Rect;

IMPORT Word, Point, Axis, Interval;

PROCEDURE FromEdges (w, e, n, s: INTEGER): T RAISES {} =
  VAR r: T;
  BEGIN
    IF (w >= e) OR (n >= s) THEN RETURN Empty;  END;
    r.west := w;
    r.east := e;
    r.north := n;
    r.south := s;
    RETURN r;
  END FromEdges;

PROCEDURE FromAbsEdges (w, e, n, s: INTEGER): T RAISES {} =
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

PROCEDURE FromPoint (READONLY p: Point.T): T RAISES {} =
  BEGIN
    RETURN FromEdges (p.h, p.h + 1, p.v, p.v + 1);
  END FromPoint;

PROCEDURE FromCorners (READONLY p, q: Point.T): T RAISES {} =
  BEGIN
    RETURN FromAbsEdges (p.h, q.h, p.v, q.v);
  END FromCorners;

PROCEDURE FromCorner (READONLY p: Point.T; hor, ver: CARDINAL): T RAISES {} =
  VAR r: T;
  BEGIN
    IF (hor = 0) OR (ver = 0) THEN RETURN Empty;  END;
    r.west := p.h;
    r.east := p.h + hor;
    r.north := p.v;
    r.south := p.v + ver;
    RETURN r;
  END FromCorner;

PROCEDURE FromSize (hor, ver: CARDINAL): T RAISES {} =
  VAR r: T;
  BEGIN
    IF (hor = 0) OR (ver = 0) THEN RETURN Empty;  END;
    r.west := 0;
    r.east := hor;
    r.north := 0;
    r.south := ver;
    RETURN r;
  END FromSize;

PROCEDURE Center (READONLY r: T; READONLY p: Point.T): T RAISES {} =
  VAR res: T; h, v: INTEGER;
  BEGIN
    IF r.west >= r.east THEN RETURN Empty END;
    h := p.h - (r.west + r.east) DIV 2;
    v := p.v - (r.north + r.south) DIV 2;
    res.west := r.west + h;
    res.east := r.east + h;
    res.north := r.north + v;
    res.south := r.south + v;
    RETURN res
  END Center;

PROCEDURE FromIntervals (READONLY hor, ver: Interval.T): T RAISES {} =
  VAR r: T;
  BEGIN
    IF (hor.lo = hor.hi) OR (ver.lo = ver.hi) THEN RETURN Empty;  END;
    r.west := hor.lo;
    r.east := hor.hi;
    r.north := ver.lo;
    r.south := ver.hi;
    RETURN r;
  END FromIntervals;

PROCEDURE FromAxes (axis: Axis.T; READONLY n, m: Interval.T): T RAISES {} =
  VAR r: T;
  BEGIN
    IF (n.lo = n.hi) OR (m.lo = m.hi) THEN RETURN Empty END;
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

PROCEDURE NorthWest (READONLY r: T): Point.T RAISES {} =
  VAR p: Point.T;
  BEGIN
    p.h := r.west;
    p.v := r.north;
    RETURN p;
  END NorthWest;

PROCEDURE NorthEast (READONLY r: T): Point.T RAISES {} =
  VAR p: Point.T;
  BEGIN
    p.h := r.east;
    p.v := r.north;
    RETURN p;
  END NorthEast;

PROCEDURE SouthWest (READONLY r: T): Point.T RAISES {} =
  VAR p: Point.T;
  BEGIN
    p.h := r.west;
    p.v := r.south;
    RETURN p;
  END SouthWest;

PROCEDURE SouthEast (READONLY r: T): Point.T RAISES {} =
  VAR p: Point.T;
  BEGIN
    p.h := r.east;
    p.v := r.south;
    RETURN p;
  END SouthEast;

PROCEDURE GetVertex (v: Vertex; READONLY r: T): Point.T RAISES {} =
  BEGIN
    IF (r.west >= r.east)(* OR (r.north>=r.south) *)  THEN
      RETURN Point.Origin;
    END;
    CASE v OF
      | Vertex.NW => RETURN NorthWest (r);
      | Vertex.NE => RETURN NorthEast (r);
      | Vertex.SW => RETURN SouthWest (r);
      | Vertex.SE => RETURN SouthEast (r);
    END;
  END GetVertex;

PROCEDURE HorSize (READONLY r: T): CARDINAL RAISES {} =
  BEGIN
    RETURN r.east - r.west;
  END HorSize;

PROCEDURE VerSize (READONLY r: T): CARDINAL RAISES {} =
  BEGIN
    RETURN r.south - r.north 
  END VerSize;

PROCEDURE Size (a: Axis.T; READONLY r: T): CARDINAL RAISES {} =
  BEGIN
    CASE a OF
      | Axis.T.Hor => RETURN HorSize (r);
      | Axis.T.Ver => RETURN VerSize (r);
    END;
  END Size;

PROCEDURE DiagSizeSquare (READONLY r: T): CARDINAL RAISES {} =
  VAR hor, ver: INTEGER;
  BEGIN
    hor := HorSize (r);
    ver := VerSize (r);
    RETURN hor * hor + ver * ver;
  END DiagSizeSquare;

PROCEDURE Middle (READONLY r: T): Point.T RAISES {} =
  BEGIN
    RETURN Point.T{(r.west+r.east) DIV 2, (r.north+r.south) DIV 2}
  END Middle;

PROCEDURE PickEdge (READONLY r: T; READONLY p: Point.T): Edge RAISES {} =
  VAR mid, se, q: Point.T; a, b: INTEGER;
  BEGIN
    mid := Middle (r);
    se := Point.Sub (SouthEast (r), mid);
    q := Point.Sub (p, mid);
    a := se.v * q.h;
    b := se.h * q.v;
    IF a >= b THEN
      IF -a >= b THEN RETURN Edge.N ELSE RETURN Edge.E END;
    ELSE
      IF -a >= b THEN RETURN Edge.W ELSE RETURN Edge.S END;
    END;
  END PickEdge;

PROCEDURE PickVertex (READONLY r: T; READONLY p: Point.T): Vertex RAISES {} =
  VAR q: Point.T;
  BEGIN
    q := Point.Sub (p, Middle (r));
    IF q.h > 0 THEN
      IF q.v > 0 THEN RETURN Vertex.SE ELSE RETURN Vertex.NE END;
    ELSE
      IF q.v > 0 THEN RETURN Vertex.SW ELSE RETURN Vertex.NW END;
    END;
  END PickVertex;

PROCEDURE Project (READONLY r: T; READONLY p: Point.T): Point.T RAISES {} =
  VAR res: Point.T;
  BEGIN
    <* ASSERT r.east > r.west *>
    res.h := MAX (MIN (p.h, r.east - 1), r.west);
    res.v := MAX (MIN (p.v, r.south - 1), r.north);
    RETURN res
  END Project;

PROCEDURE Add (READONLY r: T; READONLY p: Point.T): T RAISES {} =
  VAR s: T;
  BEGIN
    IF r.west >= r.east THEN RETURN Empty END;
    s.west := r.west + p.h;
    s.east := r.east + p.h;
    s.north := r.north + p.v;
    s.south := r.south + p.v;
    RETURN s;
  END Add;

PROCEDURE Sub (READONLY r: T; READONLY p: Point.T): T RAISES {} =
  VAR s: T;
  BEGIN
    IF r.west >= r.east THEN RETURN Empty END;
    s.west := r.west - p.h;
    s.east := r.east - p.h;
    s.north := r.north - p.v;
    s.south := r.south - p.v;
    RETURN s;
  END Sub;

PROCEDURE Move (READONLY r: T; READONLY p: Point.T): T RAISES {} =
  VAR s: T;
  BEGIN
    IF r.west >= r.east THEN RETURN Empty END;
    s.west := r.west + p.h;
    s.east := r.east + p.h;
    s.north := r.north + p.v;
    s.south := r.south + p.v;
    RETURN s;
  END Move;

PROCEDURE MoveH (READONLY r: T; h: INTEGER): T RAISES {} =
  VAR s: T;
  BEGIN
    IF r.west >= r.east THEN RETURN Empty END;
    s.west := r.west + h;
    s.east := r.east + h;
    s.north := r.north;
    s.south := r.south;
    RETURN s;
  END MoveH;

PROCEDURE MoveV (READONLY r: T; v: INTEGER): T RAISES {} =
  VAR s: T;
  BEGIN
    IF r.west >= r.east THEN RETURN Empty END;
    s.west := r.west;
    s.east := r.east;
    s.north := r.north + v;
    s.south := r.south + v;
    RETURN s;
  END MoveV;

PROCEDURE MoveHV (READONLY r: T; h: INTEGER; v: INTEGER): T RAISES {} =
  VAR s: T;
  BEGIN
    IF r.west >= r.east THEN RETURN Empty END;
    s.west := r.west + h;
    s.east := r.east + h;
    s.north := r.north + v;
    s.south := r.south + v;
    RETURN s;
  END MoveHV;

PROCEDURE Scale (READONLY r: T; num, den: INTEGER): T RAISES {} =
  VAR s: T;
  BEGIN
    IF r.west >= r.east THEN RETURN Empty END;
    s.north := (r.north * num) DIV den;
    s.south := (r.south * num) DIV den;
    IF s.north >= s.south THEN
      RETURN Empty
    ELSE
      s.west := (r.west * num) DIV den;
      s.east := (r.east * num) DIV den;
      IF s.west >= s.east THEN RETURN Empty END;
      RETURN s
    END;
  END Scale;

PROCEDURE Inset (READONLY r: T; n: INTEGER): T RAISES {} =
  VAR s: T;
  BEGIN
    IF (r.west >= r.east) OR (r.north >= r.south) THEN RETURN Empty END;
    s.west := r.west + n;
    s.east := r.east - n;
    s.north := r.north + n;
    s.south := r.south - n;
    IF (s.west >= s.east) OR (s.north >= s.south) THEN RETURN Empty;  END;
    RETURN s;
  END Inset;

PROCEDURE Change (READONLY r: T; dw, de, dn, ds: INTEGER): T RAISES {} =
  VAR s: T;
  BEGIN
    IF (r.west >= r.east) OR (r.north >= r.south) THEN RETURN Empty;  END;
    s.west := r.west + dw;
    s.east := r.east + de;
    s.north := r.north + dn;
    s.south := r.south + ds;
    IF (s.west >= s.east) OR (s.north >= s.south) THEN RETURN Empty;  END;
    RETURN s;
  END Change;

PROCEDURE MoveEdge (READONLY r: T; e: Edge; dn: INTEGER): T RAISES {} =
  VAR s: T;
  BEGIN
    IF r.west >= r.east THEN RETURN Empty END;
    s := r;
    CASE e OF
      | Edge.W => s.west := r.west + dn;
      | Edge.E => s.east := r.east + dn;
      | Edge.N => s.north := r.north + dn;
      | Edge.S => s.south := r.south + dn;
    END;
    IF (s.west >= s.east) OR (s.north >= s.south) THEN RETURN Empty;  END;
    RETURN s;
  END MoveEdge;

PROCEDURE MoveVertex (READONLY r: T; v: Vertex; READONLY dp: Point.T): T
  RAISES {} =
  VAR s: T;
  BEGIN
    IF r.west >= r.east THEN RETURN Empty END;
    s := r;
    CASE v OF
    | Vertex.NW => s.west := r.west + dp.h; s.north := r.north + dp.v;
    | Vertex.NE => s.east := r.east + dp.h; s.north := r.north + dp.v;
    | Vertex.SW => s.west := r.west + dp.h; s.south := r.south + dp.v;
    | Vertex.SE => s.east := r.east + dp.h; s.south := r.south + dp.v;
    END;
    IF (s.west >= s.east) OR (s.north >= s.south) THEN RETURN Empty;  END;
    RETURN s;
  END MoveVertex;

PROCEDURE Stretch (READONLY r: T; axis: Axis.T; lo, hi: INTEGER): T RAISES {} =
  VAR res: T;
  BEGIN
    IF (r.west >= r.east) OR lo >= hi THEN RETURN Empty;  END;
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
    IF (r.west >= r.east)(* OR (r.north>=r.south) *)  THEN RETURN s;  END;
    IF (s.west >= s.east)(* OR (s.north>=s.south) *)  THEN RETURN r;  END;
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
    IF t.west >= t.east THEN RETURN Empty END;
    t.north := MAX (r.north, s.north);
    t.south := MIN (r.south, s.south);
    IF t.north >= t.south THEN RETURN Empty;  END;
    RETURN t;
  END Meet;

PROCEDURE Extend (READONLY r: T; READONLY p: Point.T): T RAISES {} =
  BEGIN
    RETURN Join (r, FromPoint (p));
  END Extend;

PROCEDURE Chop
  (hv: Axis.T; READONLY r: T; n: INTEGER; VAR s, t: T) RAISES {} =
  BEGIN
    IF (hv = Axis.T.Hor) THEN
      IF n <= r.west THEN
        s := Empty;
        t := r
      ELSIF n >= r.east THEN
        s := r;
        t := Empty
      ELSE
        s.north := r.north;
        s.south := r.south;
        t.north := r.north;
        t.south := r.south;
        s.west := r.west;
        s.east := n;
        t.west := n;
        t.east := r.east
      END
    ELSE
      IF n <= r.north THEN
        s := Empty;
        t := r
      ELSIF n >= r.south THEN
        s := r;
        t := Empty
      ELSE
        s.west := r.west;
        s.east := r.east;
        t.west := r.west;
        t.east := r.east;
        s.north := r.north;
        s.south := n;
        t.north := n;
        t.south := r.south
      END
    END
  END Chop;
        
(* This is what it really means:
     PROCEDURE Factor(VAR IN r, by: T; VAR f: Partition; dh, dv: INTEGER)
     RAISES {}; VAR northIndex, westIndex: INTEGER; mid, temp: T; BEGIN IF dh >
     0 THEN westIndex := 3 ELSE westIndex := 1 END; IF dv > 0 THEN northIndex
     := 4 ELSE northIndex := 0 END; Chop(Axis.Ver, r, by.north, f[northIndex],
     temp); Chop(Axis.Ver, temp, by.south, mid, f[4 - northIndex]);
     Chop(Axis.Hor, mid, by.west, f[westIndex], temp); Chop(Axis.Hor, temp,
     by.east, f[2], f[4 - westIndex]); END Factor;
 *)

PROCEDURE Factor (READONLY r, by: T;  VAR f: Partition;  dh, dv: INTEGER)
  RAISES {} =
  VAR ix, rw, re, rn, rs, bw, be, bn, bs: INTEGER; temp: T;
  BEGIN
    bw := by.west;
    be := by.east;
    bn := by.north;
    bs := by.south;
    rw := r.west;
    re := r.east;
    rn := r.north;
    rs := r.south;
    IF bw >= be OR rw >= re THEN
      f[0] := r;
      FOR z := 1 TO 4 DO f[z] := Empty END;
      RETURN
    END;
    IF dv > 0 THEN ix := 4 ELSE ix := 0 END;
    IF rn < bn THEN
      WITH z = f[ix] DO z.west := rw; z.east := re; z.north := rn END;
      IF bn < rs THEN
        f[ix].south := bn;
        WITH z = temp DO z.west := rw; z.east := re; z.north := bn END;
        IF bs < rs THEN
          temp.south := bs;
          WITH z = f[4 - ix] DO
            z.west := rw;
            z.east := re;
            z.north := bs;
            z.south := rs
          END
        ELSE
          temp.south := rs;
          f[4 - ix] := Empty
        END
      ELSE
        f[ix].south := rs;
        temp := Empty;
        f[4 - ix] := Empty
      END
    ELSE
      f[ix] := Empty;
      IF rn < bs THEN
        WITH z = temp DO z.west := rw; z.east := re; z.north := rn END;
        IF bs < rs THEN
          temp.south := bs;
          WITH z = f[4 - ix] DO
            z.west := rw;
            z.east := re;
            z.north := bs;
            z.south := rs
          END
        ELSE
          temp.south := rs;
          f[4 - ix] := Empty
        END
      ELSE
        temp := Empty;
        f[4 - ix] := r
      END
    END;
    rw := temp.west;
    re := temp.east;
    rn := temp.north;
    rs := temp.south;
    IF rw >= re THEN
      FOR z := 1 TO 3 DO f[z] := Empty END;
      RETURN
    END;
    IF dh > 0 THEN ix := 3 ELSE ix := 1 END;
    IF rw < bw THEN
      WITH z = f[ix] DO z.north := rn; z.south := rs; z.west := rw END;
      IF bw < re THEN
        f[ix].east := bw;
        WITH z = f[2] DO z.north := rn; z.south := rs; z.west := bw END;
        IF be < re THEN
          f[2].east := be;
          WITH z = f[4 - ix] DO
            z.north := rn;
            z.south := rs;
            z.west := be;
            z.east := re
          END
        ELSE
          f[2].east := re;
          f[4 - ix] := Empty
        END
      ELSE
        f[ix].east := re;
        f[2] := Empty;
        f[4 - ix] := Empty
      END
    ELSE
      f[ix] := Empty;
      IF rw < be THEN
        WITH z = f[2] DO z.north := rn; z.south := rs; z.west := rw END;
        IF be < re THEN
          f[2].east := be;
          WITH z = f[4 - ix] DO
            z.north := rn;
            z.south := rs;
            z.west := be;
            z.east := re
          END
        ELSE
          f[2].east := re;
          f[4 - ix] := Empty
        END
      ELSE
        f[2] := Empty;
        f[4 - ix] := temp
      END
    END
  END Factor;

PROCEDURE Mod (READONLY p: Point.T; READONLY r: T): Point.T RAISES {} =
  VAR q: Point.T; hor, ver: Interval.T;
  BEGIN
    <* ASSERT r.west < r.east *>
    hor.lo := r.west;
    hor.hi := r.east;
    ver.lo := r.north;
    ver.hi := r.south;
    q.h := Interval.Mod (p.h, hor);
    q.v := Interval.Mod (p.v, ver);
    RETURN q
  END Mod;

PROCEDURE Equal (READONLY r, s: T): BOOLEAN RAISES {} =
  BEGIN
    RETURN r = s
  END Equal;

PROCEDURE IsEmpty (READONLY r: T): BOOLEAN RAISES {} =
  BEGIN RETURN (r.west >= r.east) END IsEmpty;

PROCEDURE Member (READONLY p: Point.T; READONLY r: T): BOOLEAN RAISES {} =
  BEGIN
    RETURN (r.west <= p.h) AND (p.h < r.east) AND (r.north <= p.v)
    AND (p.v < r.south);
  END Member;

PROCEDURE Overlap (READONLY r, s: T): BOOLEAN RAISES {} =
  BEGIN
    RETURN (MAX (r.west, s.west) < MIN (r.east, s.east))
    AND (MAX (r.north, s.north) < MIN (r.south, s.south));
  END Overlap;

PROCEDURE Subset (READONLY r, s: T): BOOLEAN RAISES {} =
  BEGIN
    RETURN (r.west >= r.east)
    OR ((r.west >= s.west) AND (r.east <= s.east) AND (r.north >= s.north)
        AND (r.south <= s.south));
  END Subset;

PROCEDURE Congruent(READONLY r, s: T): BOOLEAN =
  BEGIN
    RETURN r.east - r.west = s.east - s.west
    AND  r.south - r.north = s.south - s.north
  END Congruent;

PROCEDURE Transpose(READONLY r: T; ax := Axis.T.Ver): T =
  BEGIN
    IF ax = Axis.T.Hor THEN 
      RETURN r
    ELSE 
      RETURN T{r.north,r.south,r.west,r.east}
    END
  END Transpose;
   
PROCEDURE GlobToLoc (READONLY r: T; READONLY p: Point.T): Point.T RAISES {} =
  VAR q: Point.T;
  BEGIN
    q.h := p.h - r.west;
    q.v := p.v - r.north;
    RETURN q;
  END GlobToLoc;

PROCEDURE LocToGlob (READONLY r: T; READONLY p: Point.T): Point.T RAISES {} =
  VAR q: Point.T;
  BEGIN
    q.h := p.h + r.west;
    q.v := p.v + r.north;
    RETURN q;
  END LocToGlob;

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

PROCEDURE Hash (READONLY a: T): Word.T =
  BEGIN
    RETURN Word.Xor (Word.Xor (a.west, a.east), Word.Xor (a.north, a.south));
  END Hash;

BEGIN
END Rect.
