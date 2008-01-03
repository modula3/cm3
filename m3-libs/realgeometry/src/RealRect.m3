(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Contributed by Michel Dagenais (dagenais@vlsi.polymtl.ca), 1994. *)

MODULE RealRect;

IMPORT Word, RealPoint, Axis, RealInterval;

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

PROCEDURE FromPoint (READONLY p: RealPoint.T): T RAISES {} =
  BEGIN
    RETURN FromEdges (p[0], p[0] + 1.0, p[1], p[1] + 1.0);
  END FromPoint;

PROCEDURE FromCorners (READONLY p, q: RealPoint.T): T RAISES {} =
  BEGIN
    RETURN FromAbsEdges (p[0], q[0], p[1], q[1]);
  END FromCorners;

PROCEDURE FromCorner (READONLY p: RealPoint.T; hor, ver: REAL): T RAISES {} =
  VAR r: T;
  BEGIN
    IF (hor <= 0.0) OR (ver <= 0.0) THEN RETURN Empty;  END;
    r.west := p[0];
    r.east := p[0] + hor;
    r.north := p[1];
    r.south := p[1] + ver;
    RETURN r;
  END FromCorner;

PROCEDURE FromSize (hor, ver: REAL): T RAISES {} =
  VAR r: T;
  BEGIN
    IF (hor <= 0.0) OR (ver <= 0.0) THEN RETURN Empty;  END;
    r.west := 0.0;
    r.east := hor;
    r.north := 0.0;
    r.south := ver;
    RETURN r;
  END FromSize;

PROCEDURE Center (READONLY r: T; READONLY p: RealPoint.T): T RAISES {} =
  VAR res: T; h, v: REAL;
  BEGIN
    IF r.west >= r.east THEN RETURN Empty END;
    h := p[0] - (r.west + r.east) / 2.0;
    v := p[1] - (r.north + r.south) / 2.0;
    res.west := r.west + h;
    res.east := r.east + h;
    res.north := r.north + v;
    res.south := r.south + v;
    RETURN res
  END Center;

PROCEDURE FromIntervals (READONLY hor, ver: RealInterval.T): T RAISES {} =
  VAR r: T;
  BEGIN
    IF (hor.lo = hor.hi) OR (ver.lo = ver.hi) THEN RETURN Empty;  END;
    r.west := hor.lo;
    r.east := hor.hi;
    r.north := ver.lo;
    r.south := ver.hi;
    RETURN r;
  END FromIntervals;

PROCEDURE FromAxes (axis: Axis.T; READONLY n, m: RealInterval.T): T RAISES {} =
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

PROCEDURE NorthWest (READONLY r: T): RealPoint.T RAISES {} =
  VAR p: RealPoint.T;
  BEGIN
    p[0] := r.west;
    p[1] := r.north;
    RETURN p;
  END NorthWest;

PROCEDURE NorthEast (READONLY r: T): RealPoint.T RAISES {} =
  VAR p: RealPoint.T;
  BEGIN
    p[0] := r.east;
    p[1] := r.north;
    RETURN p;
  END NorthEast;

PROCEDURE SouthWest (READONLY r: T): RealPoint.T RAISES {} =
  VAR p: RealPoint.T;
  BEGIN
    p[0] := r.west;
    p[1] := r.south;
    RETURN p;
  END SouthWest;

PROCEDURE SouthEast (READONLY r: T): RealPoint.T RAISES {} =
  VAR p: RealPoint.T;
  BEGIN
    p[0] := r.east;
    p[1] := r.south;
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
    RETURN r.east - r.west;
  END HorSize;

PROCEDURE VerSize (READONLY r: T): REAL RAISES {} =
  BEGIN
    RETURN r.south - r.north 
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
    hor := HorSize (r);
    ver := VerSize (r);
    RETURN hor * hor + ver * ver;
  END DiagSizeSquare;

PROCEDURE Middle (READONLY r: T): RealPoint.T RAISES {} =
  BEGIN
    RETURN RealPoint.T{(r.west+r.east) / 2.0, (r.north+r.south) / 2.0}
  END Middle;

PROCEDURE PickEdge (READONLY r: T; READONLY p: RealPoint.T): Edge RAISES {} =
  VAR mid, se, q: RealPoint.T; a, b: REAL;
  BEGIN
    mid := Middle (r);
    se := RealPoint.Sub (SouthEast (r), mid);
    q := RealPoint.Sub (p, mid);
    a := se[1] * q[0];
    b := se[0] * q[1];
    IF a >= b THEN
      IF -a >= b THEN RETURN Edge.N ELSE RETURN Edge.E END;
    ELSE
      IF -a >= b THEN RETURN Edge.W ELSE RETURN Edge.S END;
    END;
  END PickEdge;

PROCEDURE PickVertex (READONLY r: T; READONLY p: RealPoint.T): Vertex RAISES {} =
  VAR q: RealPoint.T;
  BEGIN
    q := RealPoint.Sub (p, Middle (r));
    IF q[0] > 0.0 THEN
      IF q[1] > 0.0 THEN RETURN Vertex.SE ELSE RETURN Vertex.NE END;
    ELSE
      IF q[1] > 0.0 THEN RETURN Vertex.SW ELSE RETURN Vertex.NW END;
    END;
  END PickVertex;

PROCEDURE Project (READONLY r: T; READONLY p: RealPoint.T): RealPoint.T RAISES {} =
  VAR res: RealPoint.T;
  BEGIN
    <* ASSERT r.east > r.west *>
    res[0] := MAX (MIN (p[0], r.east), r.west);
    res[1] := MAX (MIN (p[1], r.south), r.north);
    RETURN res
  END Project;

PROCEDURE Add (READONLY r: T; READONLY p: RealPoint.T): T RAISES {} =
  VAR s: T;
  BEGIN
    IF r.west >= r.east THEN RETURN Empty END;
    s.west := r.west + p[0];
    s.east := r.east + p[0];
    s.north := r.north + p[1];
    s.south := r.south + p[1];
    RETURN s;
  END Add;

PROCEDURE Sub (READONLY r: T; READONLY p: RealPoint.T): T RAISES {} =
  VAR s: T;
  BEGIN
    IF r.west >= r.east THEN RETURN Empty END;
    s.west := r.west - p[0];
    s.east := r.east - p[0];
    s.north := r.north - p[1];
    s.south := r.south - p[1];
    RETURN s;
  END Sub;

PROCEDURE Move (READONLY r: T; READONLY p: RealPoint.T): T RAISES {} =
  VAR s: T;
  BEGIN
    IF r.west >= r.east THEN RETURN Empty END;
    s.west := r.west + p[0];
    s.east := r.east + p[0];
    s.north := r.north + p[1];
    s.south := r.south + p[1];
    RETURN s;
  END Move;

PROCEDURE MoveH (READONLY r: T; h: REAL): T RAISES {} =
  VAR s: T;
  BEGIN
    IF r.west >= r.east THEN RETURN Empty END;
    s.west := r.west + h;
    s.east := r.east + h;
    s.north := r.north;
    s.south := r.south;
    RETURN s;
  END MoveH;

PROCEDURE MoveV (READONLY r: T; v: REAL): T RAISES {} =
  VAR s: T;
  BEGIN
    IF r.west >= r.east THEN RETURN Empty END;
    s.west := r.west;
    s.east := r.east;
    s.north := r.north + v;
    s.south := r.south + v;
    RETURN s;
  END MoveV;

PROCEDURE MoveHV (READONLY r: T; h: REAL; v: REAL): T RAISES {} =
  VAR s: T;
  BEGIN
    IF r.west >= r.east THEN RETURN Empty END;
    s.west := r.west + h;
    s.east := r.east + h;
    s.north := r.north + v;
    s.south := r.south + v;
    RETURN s;
  END MoveHV;

PROCEDURE Scale (READONLY r: T; num, den: REAL): T RAISES {} =
  VAR s: T;
  BEGIN
    IF r.west >= r.east THEN RETURN Empty END;
    s.north := (r.north * num) / den;
    s.south := (r.south * num) / den;
    IF s.north >= s.south THEN
      RETURN Empty
    ELSE
      s.west := (r.west * num) / den;
      s.east := (r.east * num) / den;
      IF s.west >= s.east THEN RETURN Empty END;
      RETURN s
    END;
  END Scale;

PROCEDURE Inset (READONLY r: T; n: REAL): T RAISES {} =
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

PROCEDURE Change (READONLY r: T; dw, de, dn, ds: REAL): T RAISES {} =
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

PROCEDURE MoveEdge (READONLY r: T; e: Edge; dn: REAL): T RAISES {} =
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

PROCEDURE MoveVertex (READONLY r: T; v: Vertex; READONLY dp: RealPoint.T): T
  RAISES {} =
  VAR s: T;
  BEGIN
    IF r.west >= r.east THEN RETURN Empty END;
    s := r;
    CASE v OF
    | Vertex.NW => s.west := r.west + dp[0]; s.north := r.north + dp[1];
    | Vertex.NE => s.east := r.east + dp[0]; s.north := r.north + dp[1];
    | Vertex.SW => s.west := r.west + dp[0]; s.south := r.south + dp[1];
    | Vertex.SE => s.east := r.east + dp[0]; s.south := r.south + dp[1];
    END;
    IF (s.west >= s.east) OR (s.north >= s.south) THEN RETURN Empty;  END;
    RETURN s;
  END MoveVertex;

PROCEDURE Stretch (READONLY r: T; axis: Axis.T; lo, hi: REAL): T RAISES {} =
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

PROCEDURE Extend (READONLY r: T; READONLY p: RealPoint.T): T RAISES {} =
  BEGIN
    RETURN Join (r, FromPoint (p));
  END Extend;

PROCEDURE Chop
  (hv: Axis.T; READONLY r: T; n: REAL; VAR s, t: T) RAISES {} =
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
     PROCEDURE Factor(VAR IN r, by: T; VAR f: Partition; dh, dv: REAL)
     RAISES {}; VAR northIndex, westIndex: REAL; mid, temp: T; BEGIN IF dh >
     0 THEN westIndex := 3 ELSE westIndex := 1 END; IF dv > 0 THEN northIndex
     := 4 ELSE northIndex := 0 END; Chop(Axis.Ver, r, by.north, f[northIndex],
     temp); Chop(Axis.Ver, temp, by.south, mid, f[4 - northIndex]);
     Chop(Axis.Hor, mid, by.west, f[westIndex], temp); Chop(Axis.Hor, temp,
     by.east, f[2], f[4 - westIndex]); END Factor;
 *)

PROCEDURE Factor (READONLY r, by: T;  VAR f: Partition;  dh, dv: REAL)
  RAISES {} =
  VAR ix: INTEGER; rw, re, rn, rs, bw, be, bn, bs: REAL; temp: T;
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
    IF dv > 0.0 THEN ix := 4 ELSE ix := 0 END;
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
    IF dh > 0.0 THEN ix := 3 ELSE ix := 1 END;
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

PROCEDURE Mod (READONLY p: RealPoint.T; READONLY r: T): RealPoint.T RAISES {} =
  VAR q: RealPoint.T; hor, ver: RealInterval.T;
  BEGIN
    <* ASSERT r.west < r.east *>
    hor.lo := r.west;
    hor.hi := r.east;
    ver.lo := r.north;
    ver.hi := r.south;
    q[0] := RealInterval.Mod (p[0], hor);
    q[1] := RealInterval.Mod (p[1], ver);
    RETURN q
  END Mod;

PROCEDURE Equal (READONLY r, s: T): BOOLEAN RAISES {} =
  BEGIN
    RETURN r = s
  END Equal;

PROCEDURE IsEmpty (READONLY r: T): BOOLEAN RAISES {} =
  BEGIN RETURN (r.west >= r.east) END IsEmpty;

PROCEDURE Member (READONLY p: RealPoint.T; READONLY r: T): BOOLEAN RAISES {} =
  BEGIN
    RETURN (r.west <= p[0]) AND (p[0] < r.east) AND (r.north <= p[1])
    AND (p[1] < r.south);
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
   
PROCEDURE GlobToLoc (READONLY r: T; READONLY p: RealPoint.T): RealPoint.T RAISES {} =
  VAR q: RealPoint.T;
  BEGIN
    q[0] := p[0] - r.west;
    q[1] := p[1] - r.north;
    RETURN q;
  END GlobToLoc;

PROCEDURE LocToGlob (READONLY r: T; READONLY p: RealPoint.T): RealPoint.T RAISES {} =
  VAR q: RealPoint.T;
  BEGIN
    q[0] := p[0] + r.west;
    q[1] := p[1] + r.north;
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
    RETURN Word.Xor (
             Word.Xor (TRUNC(a.west), TRUNC(a.east)), 
             Word.Xor (TRUNC(a.north), TRUNC(a.south)));
  END Hash;

BEGIN
END RealRect.

