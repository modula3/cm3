(* $Id$ *)

MODULE AdGrid EXPORTS AdGrid;
IMPORT AdGridChild;
IMPORT LRPoint, LRVector, LRScalarField, AdGridQ, AdGridQSet, AdGridQSetDef;

TYPE
  Child = AdGridChild.T; (* top left, top right, etc. *)
  Dir = AdGridQ.Dir;

CONST 
  UL = Child.UL; UR = Child.UR; LL = Child.LL; LR = Child.LR;
  WhichChild = ARRAY BOOLEAN OF ARRAY BOOLEAN OF Child { 
    ARRAY BOOLEAN OF Child {LL,UL}, 
    ARRAY BOOLEAN OF Child {LR, UR} 
  };
  NoChildren = ARRAY Child OF M { NIL, .. };

TYPE 
  Funcs = REF ARRAY OF LRScalarField.T;
  Vals = ARRAY Child OF REF ARRAY OF LONGREAL;

  M = AdGridQ.T OBJECT
    f : Funcs;  (* pointer to functions stored in this mesh *)
    up : M;     (* pointer to my parent UNUSED ??? *) 
    ll, ur : LRPoint.T;  (* my corners *)
    vals : Vals; (* my values (for EACH f) *)
    children : ARRAY Child OF M := NoChildren; (* my children *)
  METHODS
    maxCorner(master : T) : Child := MaxCorner;
    minCorner(master : T) : Child := MinCorner;
  OVERRIDES
    corner := MCorner;
    getLbound := GetLbound;
    getUbound := GetUbound;
    subdivide := Subdivide;

    neighbor := Neighbor;
    allNeighborsAlongEdge := AllNeighborsAlongEdge;
    getCornerValues := GetCornerValues;
    getCrossings := GetCrossings;
    larger := Larger;
    range := Range;
  END;

PROCEDURE GetCornerValues(m : M; adGridP : REFANY) : ARRAY Child OF LONGREAL =
  VAR
    r : ARRAY Child OF LONGREAL;
  BEGIN
    WITH a = NARROW(adGridP, T) DO
      FOR i := FIRST(Child) TO LAST(Child) DO
        r[i] := m.vals[i][a.me]
      END
    END;
    RETURN r
  END GetCornerValues;

PROCEDURE GetCrossings(m : M; 
                       adGridP : REFANY; 
                       of : LONGREAL): ARRAY Dir OF REF LRPoint.T =

  CONST N = Dir.N; E = Dir.E; 
        W = Dir.W; S = Dir.S; 

  PROCEDURE MixPts(a, b : LRPoint.T; alpha : LONGREAL) : LRPoint.T =
    BEGIN
      RETURN LRPoint.T { alpha * b.x + (1.0d0 - alpha) * a.x ,
                         alpha * b.y + (1.0d0 - alpha) * a.y }
    END MixPts;

  CONST
    Dirs = ARRAY Child OF Dir { W, N, E, S };
  VAR
    r := ARRAY Dir OF REF LRPoint.T { NIL, .. };
    me := NARROW(adGridP,T).me;
  BEGIN
    FOR i := FIRST(Child) TO LAST(Child) DO
      WITH j = VAL((ORD(i) + 1) MOD NUMBER(Child),Child), 
           zi = m.vals[i][me], 
           zj = m.vals[j][me], 
           p = r[Dirs[i]] DO
        IF of <= MAX(zi,zj) AND of >= MIN(zi,zj) THEN
          p := NEW(REF LRPoint.T);
          p^ := MixPts(m.corner(i),m.corner(j), (of - zi) / (zj - zi))
        END
      END
    END;
    RETURN r
  END GetCrossings;

(*
PROCEDURE GetChild(m : M; c : Child) : M = 
  BEGIN RETURN m.children[c] END GetChild;
*)

PROCEDURE UpNeighbor(m : M; dir : Dir) : M =
  CONST N = Dir.N; E = Dir.E; 
        W = Dir.W; S = Dir.S; 
  VAR
    dad := m.up;
    iAm : Child;
    r : M := NIL;
  BEGIN
    IF dad = NIL THEN RETURN NIL END;

    WITH sibs = dad.children DO
      FOR i := FIRST(Child) TO LAST(Child) DO
        IF sibs[i] = m THEN iAm := i END
      END;
      
      CASE iAm OF 
        UL => IF dir = S THEN r:= sibs[LL] ELSIF dir = E THEN r:= sibs[UR] END
      |
        UR => IF dir = S THEN r:= sibs[LR] ELSIF dir = W THEN r:= sibs[UL] END
      | 
        LL => IF dir = N THEN r:= sibs[UL] ELSIF dir = E THEN r:= sibs[LR] END
      | 
        LR => IF dir = N THEN r:= sibs[UR] ELSIF dir = W THEN r:= sibs[LL] END
      END;
      IF r = NIL THEN r := UpNeighbor(dad,dir) END;
      RETURN r
    END
  END UpNeighbor;

(* starting from at in m, find smallest neighboring tile in dir *)
PROCEDURE Neighbor(m : M; at : LRPoint.T; dir : Dir) : AdGridQ.T =
  CONST N = Dir.N; E = Dir.E; 
        W = Dir.W; S = Dir.S; 

  PROCEDURE RecurseX(c : M; x : LONGREAL; dir : Dir) : M =
    VAR 
      next : M;
    BEGIN
      IF c.children = NoChildren THEN RETURN c END;
      
      WITH mmx = (c.ll.x + c.ur.x) / 2.0d0 DO
        IF x < mmx THEN
          IF    dir = N THEN 
            next := c.children[LL]
          ELSIF dir = S THEN
            next := c.children[UL]
          ELSE
            <* ASSERT FALSE *>
          END
        ELSE
          IF    dir = N THEN 
            next := c.children[LR]
          ELSIF dir = S THEN
            next := c.children[UR]
          ELSE
            <* ASSERT FALSE *>
          END
        END
      END;
      RETURN RecurseX(next,x,dir)
    END RecurseX;

  PROCEDURE RecurseY(c : M; y : LONGREAL; dir : Dir) : M =
    VAR
      next : M;
    BEGIN
      IF c.children = NoChildren THEN RETURN c END;
      
      WITH mmy = (c.ll.y + c.ur.y) / 2.0d0 DO
        IF y < mmy THEN
          IF    dir = E THEN 
            next := c.children[LL]
          ELSIF dir = W THEN
            next := c.children[LR]
          ELSE
            <* ASSERT FALSE *>
          END
        ELSE
          IF    dir = E THEN 
            next := c.children[UL]
          ELSIF dir = W THEN
            next := c.children[UR]
          ELSE
            <* ASSERT FALSE *>
          END
        END
      END;
      RETURN RecurseY(next,y,dir)
    END RecurseY;

  VAR
    upNeighbor := UpNeighbor(m,dir);
    r : M;
  BEGIN
    (* if no neighbor at all, that's that. *)
    IF upNeighbor = NIL THEN RETURN NIL END;

    (* else find the closest tile along the line defined by at. *)
    CASE dir OF
      N, S => r := RecurseX(upNeighbor, at.x, dir)
    |
      E, W => r := RecurseY(upNeighbor, at.y, dir)
    END;
    <* ASSERT r.children = NoChildren *>
    RETURN r
 END Neighbor;

PROCEDURE AllLeavesAlongEdge(m : M; dir : Dir) : AdGridQSet.T =
  CONST N = Dir.N; E = Dir.E; 
        W = Dir.W; S = Dir.S; 

  PROCEDURE Recurse(mm : M) =
    BEGIN
      WITH c = mm.children DO
        IF c = NoChildren THEN 
          EVAL set.insert(mm) 
        ELSE
          CASE dir OF 
            N => Recurse(c[UL]); Recurse(c[UR])
          |
            E => Recurse(c[UR]); Recurse(c[LR])
          |
            W => Recurse(c[UL]); Recurse(c[LL])
          |
            S => Recurse(c[LL]); Recurse(c[LR])
          END
        END
      END
    END Recurse;

  VAR
    set := NEW(AdGridQSetDef.T).init();
  BEGIN 
    Recurse(m); 
    RETURN set 
  END AllLeavesAlongEdge;

PROCEDURE AllNeighborsAlongEdge(m : M; dir : Dir) : REFANY =
  CONST N = Dir.N; E = Dir.E; 
        W = Dir.W; S = Dir.S; 

  PROCEDURE XOverlap(a, b : M) : BOOLEAN =
    BEGIN
      WITH au = a.ur.x, bu = b.ur.x, al = a.ll.x, bl = b.ll.x,
           aw = au - al, bw = bu - bl, mw = MIN(aw,bw) DO
        IF au < bu THEN
          RETURN au - MAX(bl,al) > mw/2.0d0
        ELSE
          RETURN bu - MAX(al,bl) > mw/2.0d0
        END
      END
    END XOverlap;

  PROCEDURE YOverlap(a, b : M) : BOOLEAN =
    BEGIN
      WITH au = a.ur.y, bu = b.ur.y, al = a.ll.y, bl = b.ll.y,
           ah = au - al, bh = bu - bl, mh = MIN(ah,bh) DO
        IF au < bu THEN
          RETURN au - MAX(bl,al) > mh/2.0d0
        ELSE
          RETURN bu - MAX(al,bl) > mh/2.0d0
        END
      END
    END YOverlap;

  VAR
    upN := UpNeighbor(m,dir);
    set := NEW(AdGridQSetDef.T).init();
    n : AdGridQ.T;
    pI : AdGridQSet.Iterator;
  BEGIN
    IF upN = NIL THEN 
      RETURN set
    ELSE
      pI := AllLeavesAlongEdge(upN,AdGridQ.OppositeDir[dir]).iterate();
      WHILE pI.next(n) DO
        IF (dir = N OR dir = S) AND XOverlap(m,n) THEN
          EVAL set.insert(n)
        ELSIF (dir = E OR dir = W) AND YOverlap(m,n) THEN
          EVAL set.insert(n)
        END
      END
    END;
    RETURN set
  END AllNeighborsAlongEdge;

PROCEDURE NewVals(n : CARDINAL) : Vals = 
  VAR
    r : Vals;
  BEGIN
    FOR i := FIRST(Child) TO LAST(Child) DO
      r[i] := NEW(REF ARRAY OF LONGREAL, n)
    END;
    RETURN r
  END NewVals;

REVEAL 
  T = Public BRANDED Brand OBJECT
    root : M;      (* pointer to root of mesh *)
    me : CARDINAL; (* index of the function I represent *)
    f : Funcs;     (* pointer to the set of functions evaluated in this mesh *)
    prec := 0.01d0; (* precision ??? *)
    next : T := NIL; (* circular list of Ts that share same mesh *)
  OVERRIDES
    evalP := Eval;
    init := Init;
    setPrec := SetPrec;
    eval := EvalLRSF;
    getQuadsContainingLevel := GetLeafTilesContainingLevel;
    getQuadsContainingLevelOrLower := GetLeafTilesContainingLevelOrLower;
    getQuadsContainingLevelOrHigher := GetLeafTilesContainingLevelOrHigher;
    getAllQuads := GetLeafTiles;
    mapNewLRSF := MapNewLRSF;
  END;

PROCEDURE EvalF(f : REF ARRAY OF LRScalarField.T;
                v : LRVector.T) : REF ARRAY OF LONGREAL =
  VAR
    r := NEW(REF ARRAY OF LONGREAL, NUMBER(f^));
  BEGIN
    FOR i := FIRST(r^) TO LAST(r^) DO
      r[i] := f[i].eval(v)
    END;
    RETURN r
  END EvalF;

PROCEDURE EvalFHint(f : REF ARRAY OF LRScalarField.T;
                v : LRVector.T)  =
  BEGIN
    FOR i := FIRST(f^) TO LAST(f^) DO
      IF f[i].doHints THEN
        f[i].evalHint(v)
      END
    END
  END EvalFHint;

PROCEDURE SetPrec(a : T; prec : LONGREAL) = BEGIN a.prec := prec END SetPrec;

PROCEDURE EvalLRSF(a : T; at : LRVector.T) : LONGREAL =
  BEGIN RETURN a.evalP(LRPoint.T{ at[0], at[1]}, prec := a.prec) END EvalLRSF;

PROCEDURE PointToVector(READONLY p : LRPoint.T) : LRVector.T =
  VAR 
    r := NEW(LRVector.T,2);
  BEGIN
    r[0] := p.x; r[1] := p.y; RETURN r
  END PointToVector;

PROCEDURE MCorner(m : M ; c : Child) : LRPoint.T =
  BEGIN
    WITH llx = m.ll.x,
         urx = m.ur.x,
         lly = m.ll.y,
         ury = m.ur.y,
         xs = ARRAY Child OF LONGREAL { llx, llx, urx, urx }, 
         ys = ARRAY Child OF LONGREAL { lly, ury, ury, lly } DO
      RETURN LRPoint.T { xs[c], ys[c] }
    END
  END MCorner;

PROCEDURE GetLbound(m : M; gridP : REFANY) : LONGREAL =
  BEGIN
    WITH grid = NARROW(gridP,T) DO
      RETURN m.vals[m.minCorner(grid)][grid.me]
    END
  END GetLbound;

PROCEDURE GetUbound(m : M; gridP : REFANY) : LONGREAL =
  BEGIN
    WITH grid = NARROW(gridP,T) DO
      RETURN m.vals[m.maxCorner(grid)][grid.me]
    END
  END GetUbound;

PROCEDURE SubdivideSet(set : AdGridQSet.T; levels : CARDINAL) =
  VAR
    i := set.iterate();
    m : AdGridQ.T;
  BEGIN
    WHILE i.next(m) DO
      <* ASSERT NARROW(m,M).children = NoChildren *>
      SubdivideM1Build(m,levels);
    END;

    i := set.iterate();
    WHILE i.next(m) DO
      SubdivideM2Hint(m,levels)
    END;

    i := set.iterate();
    WHILE i.next(m) DO
      SubdivideM3Eval(m,levels)
    END
  END SubdivideSet;

PROCEDURE Subdivide(m : M; levels : CARDINAL := 1) : REFANY =
  PROCEDURE Recurse(m : M) =
    BEGIN
      IF m.children = NoChildren THEN
        (* base case *)
        EVAL set.insert(m)
      ELSE
        (* recursion case *)
        FOR i := FIRST(Child) TO LAST(Child) DO
          Recurse(m.children[i])
        END
      END
    END Recurse;
  VAR
    set := NEW(AdGridQSetDef.T).init();
  BEGIN
    SubdivideM1Build(m,levels);
    SubdivideM2Hint(m,levels);
    SubdivideM3Eval(m,levels);
    Recurse(m);
    RETURN set
  END Subdivide;

PROCEDURE SubdivideM1Build(m : M; levels : CARDINAL := 1) =
  BEGIN
    IF levels = 0 THEN RETURN END;

    <* ASSERT m.children = ARRAY Child OF M { NIL, .. } *>
    WITH mm = LRPoint.T { (m.ll.x + m.ur.x) / 2.0d0 , 
                          (m.ll.y + m.ur.y) / 2.0d0 } DO
      FOR i := FIRST(Child) TO LAST(Child) DO
        WITH c = m.children[i] DO
          c := NEW(M, up := m, f := m.f);
          CASE i OF
            UL => c.ll := LRPoint.T { m.ll.x, mm.y }; 
                  c.ur := LRPoint.T { mm.x, m.ur.y }
          |
            UR => c.ll := mm; c.ur := m.ur
          |
            LL => c.ll := m.ll; c.ur := mm
          |
            LR => c.ll := LRPoint.T { mm.x, m.ll.y }; 
                  c.ur := LRPoint.T { m.ur.x, mm.y }
          END
        END
      END
    END;
    FOR i := FIRST(Child) TO LAST(Child) DO
      SubdivideM1Build(m.children[i],levels - 1) 
    END
  END SubdivideM1Build;

PROCEDURE SubdivideM2Hint(m : M; levels : CARDINAL := 1) =
  BEGIN
    IF levels = 0 THEN RETURN END;

    FOR i := FIRST(Child) TO LAST(Child) DO
      m.children[i].vals[i] := m.vals[i]
    END;

    WITH mm = LRPoint.T { (m.ll.x + m.ur.x) / 2.0d0 , 
                          (m.ll.y + m.ur.y) / 2.0d0 } DO
      (* fill in new points *)
      WITH f = m.f,
           ml = m.children[UL].corner(LL),
           mr = m.children[LR].corner(UR),
           mb = m.children[LR].corner(LL),
           mt = m.children[UL].corner(UR)
       DO
        EvalFHint(f,PointToVector(ml));
        EvalFHint(f,PointToVector(mr));
        EvalFHint(f,PointToVector(mb));
        EvalFHint(f,PointToVector(mt));
        EvalFHint(f,PointToVector(mm))
      END
    END;
    FOR i := FIRST(Child) TO LAST(Child) DO
      SubdivideM2Hint(m.children[i],levels - 1) 
    END
  END SubdivideM2Hint;

PROCEDURE SubdivideM3Eval(m : M; levels : CARDINAL := 1) =
  BEGIN
    IF levels = 0 THEN RETURN END;

    FOR i := FIRST(Child) TO LAST(Child) DO
      m.children[i].vals[i] := m.vals[i]
    END;

    WITH mm = LRPoint.T { (m.ll.x + m.ur.x) / 2.0d0 , 
                          (m.ll.y + m.ur.y) / 2.0d0 } DO
      (* fill in new points *)
      WITH f = m.f,
           ml = m.children[UL].corner(LL), mlz = EvalF(f,PointToVector(ml)),
           mr = m.children[LR].corner(UR), mrz = EvalF(f,PointToVector(mr)),
           mb = m.children[LR].corner(LL), mbz = EvalF(f,PointToVector(mb)),
           mt = m.children[UL].corner(UR), mtz = EvalF(f,PointToVector(mt)),
           mmz = EvalF(f,PointToVector(mm))
       DO
        m.children[UL].vals[LL] := mlz; m.children[LL].vals[UL] := mlz;
        m.children[UL].vals[UR] := mtz; m.children[UR].vals[UL] := mtz;
        m.children[UR].vals[LR] := mrz; m.children[LR].vals[UR] := mrz;
        m.children[LR].vals[LL] := mbz; m.children[LL].vals[LR] := mbz;
        
        m.children[UL].vals[LR] := mmz;
        m.children[UR].vals[LL] := mmz;
        m.children[LR].vals[UL] := mmz;
        m.children[LL].vals[UR] := mmz
      END
    END;
    FOR i := FIRST(Child) TO LAST(Child) DO
      SubdivideM3Eval(m.children[i],levels - 1) 
    END
  END SubdivideM3Eval;


PROCEDURE Eval(t : T; READONLY at : LRPoint.T; prec : LONGREAL) : LONGREAL =
  PROCEDURE EvalM(m : M) : LONGREAL =
    BEGIN
      WITH mm = LRPoint.T {(m.ll.x+m.ur.x)/2.0d0, (m.ll.y+m.ur.y)/2.0d0},
           wc = WhichChild[at.x > mm.x, at.y > mm.y],
           c = m.children[wc] DO
        IF c # NIL THEN 
          RETURN EvalM(c) 
        ELSE
          (* base case *)
          WITH alpha = (at.x - m.ll.x) / (m.ur.x - m.ll.x),
               beta  = (at.y - m.ll.y) / (m.ur.y - m.ll.y),
               res = beta * 
                       (alpha * m.vals[UR][t.me] + 
                       (1.0d0-alpha)*m.vals[UL][t.me]) +
                     (1.0d0-beta) * 
                       (alpha * m.vals[LR][t.me] + 
                       (1.0d0-alpha)*m.vals[LL][t.me]) DO
            VAR
              maxdiff := 0.0d0;
            BEGIN
              FOR i := FIRST(Child) TO LAST(Child) DO
                maxdiff := MAX(maxdiff, ABS(m.vals[i][t.me]-res))
              END;
              IF maxdiff < prec THEN 
                RETURN res
              ELSE
                SubdivideM1Build(m);
                SubdivideM2Hint(m);
                SubdivideM3Eval(m);
                RETURN EvalM(m)
              END
            END
          END
        END
      END
    END EvalM;

  PROCEDURE Expand(oldRootIs : Child) =
    VAR
      ll, ur : LRPoint.T;
      new : M;
    BEGIN
      CASE oldRootIs OF
        UR =>
          ll := LRPoint.T { 2.0d0*t.root.ll.x - t.root.ur.x, 
                                2.0d0*t.root.ll.y - t.root.ur.y };
          ur := t.root.ur
      |
        LL =>
          ll := t.root.ll;
          ur := LRPoint.T { 2.0d0*t.root.ur.x - t.root.ll.x, 
                                2.0d0*t.root.ur.y - t.root.ll.y };
      ELSE
        <* ASSERT FALSE *>
      END;

      new := NewM(ll, ur, t.f);

      (* subdivide it once *)
      SubdivideM1Build(new);
      SubdivideM2Hint(new);
      SubdivideM3Eval(new);

      t.root.up := new;

      (* and now overwrite the virgin child... *)
      new.children[oldRootIs] := t.root;

      t.root := new;
    END Expand;

  BEGIN
    WITH llx = t.root.ll.x, urx = t.root.ur.x,
         lly = t.root.ll.y, ury = t.root.ur.y DO

      (* first check if we must expand *)
      IF    at.x < llx THEN Expand(Child.UR)
      ELSIF at.x > urx THEN Expand(Child.LL)
      ELSIF at.y < lly THEN Expand(Child.UR)
      ELSIF at.y > ury THEN Expand(Child.LL)
      ELSE                  RETURN EvalM(t.root)
      END;
      RETURN Eval(t,at,prec)
    END
  END Eval;

PROCEDURE NewM(ll, ur : LRPoint.T; f : Funcs) : M =
  VAR
    res := NEW(M, ll := ll, ur := ur, up := NIL, 
               f := f, vals := NewVals(NUMBER(f^)));
  BEGIN
    FOR j := FIRST(f^) TO LAST(f^) DO
      FOR i := FIRST(Child) TO LAST(Child) DO
        res.vals[i][j] := f[j].eval(PointToVector(res.corner(i)))
      END
    END; 
    RETURN res
  END NewM;

PROCEDURE Init(self : T; 
               f : LRScalarField.T; ll, ur : LRPoint.T; initLevels := 0) : T =
  BEGIN
    self.me := 0; (* index of function in f *)
    self.f := NEW(Funcs, 1);
    self.f[0] := f;
    self.root := NewM(ll, ur, self.f);
    self.next := self;
    SubdivideM1Build(self.root,initLevels);
    SubdivideM2Hint(self.root,initLevels);
    SubdivideM3Eval(self.root,initLevels);
    RETURN self
  END Init;

PROCEDURE GetLeafTilesContainingLevel(self : T; 
                                      level : LONGREAL;
                                      set : AdGridQSet.T) : AdGridQSet.T = 

  VAR
    delSet := NEW(AdGridQSetDef.T).init();
    iter : AdGridQSet.Iterator;
    mp : AdGridQ.T;
  BEGIN
    IF set = NIL THEN set := GetLeafTiles(self) END;
    iter := set.iterate();

    WHILE iter.next(mp) DO WITH m = NARROW(mp,M) DO
      IF NOT( level >= m.vals[m.minCorner(self)][self.me] AND 
              level <= m.vals[m.maxCorner(self)][self.me]) THEN
        EVAL delSet.insert(m)
      END
    END END;
    RETURN set.diff(delSet)
  END GetLeafTilesContainingLevel;

PROCEDURE GetLeafTilesContainingLevelOrLower(self : T; 
                                      level : LONGREAL;
                                      set : AdGridQSet.T) : AdGridQSet.T = 

  VAR
    delSet := NEW(AdGridQSetDef.T).init();
    iter : AdGridQSet.Iterator;
    mp : AdGridQ.T;
  BEGIN
    IF set = NIL THEN set := GetLeafTiles(self) END;
    iter := set.iterate();

    WHILE iter.next(mp) DO WITH m = NARROW(mp,M) DO
      IF NOT( level >= m.vals[m.minCorner(self)][self.me]) THEN
        EVAL delSet.insert(m)
      END
    END END;
    RETURN set.diff(delSet)
  END GetLeafTilesContainingLevelOrLower;

PROCEDURE GetLeafTilesContainingLevelOrHigher(self : T; 
                                      level : LONGREAL;
                                      set : AdGridQSet.T) : AdGridQSet.T = 

  VAR
    delSet := NEW(AdGridQSetDef.T).init();
    iter : AdGridQSet.Iterator;
    mp : AdGridQ.T;
  BEGIN
    IF set = NIL THEN set := GetLeafTiles(self) END;
    iter := set.iterate();

    WHILE iter.next(mp) DO WITH m = NARROW(mp,M) DO
      IF NOT(level <= m.vals[m.maxCorner(self)][self.me]) THEN
        EVAL delSet.insert(m)
      END
    END END;
    RETURN set.diff(delSet)
  END GetLeafTilesContainingLevelOrHigher;

PROCEDURE GetLeafTiles(self : T) : AdGridQSet.T = 

  PROCEDURE Recurse(m : M) =
    BEGIN
      IF m.children = NoChildren THEN
        (* base case *)
        EVAL set.insert(m)
      ELSE
        (* recursion case *)
        FOR i := FIRST(Child) TO LAST(Child) DO
          Recurse(m.children[i])
        END
      END
    END Recurse;

  VAR
    set := NEW(AdGridQSetDef.T).init();
  BEGIN
    Recurse(self.root);
    RETURN set
  END GetLeafTiles;

PROCEDURE MaxCorner(m : M; master : T) : Child =
  VAR
    c := UL;
    v := FIRST(LONGREAL);
  BEGIN
    FOR i := FIRST(Child) TO LAST(Child) DO
      IF m.vals[i][master.me] >= v THEN v := m.vals[i][master.me]; c := i END
    END;
    RETURN c
  END MaxCorner;

PROCEDURE MinCorner(m : M; master : T) : Child =
  VAR
    c := UL;
    v := LAST(LONGREAL);
  BEGIN
    FOR i := FIRST(Child) TO LAST(Child) DO
      IF m.vals[i][master.me] <= v THEN v := m.vals[i][master.me]; c := i END
    END;
    RETURN c
  END MinCorner;

PROCEDURE MapNewLRSF(self : T; newf : LRScalarField.T) : T = 

  PROCEDURE Recurse(m : M) =
    VAR
      newVals := NewVals(n);
    BEGIN
      m.f := self.f;
      FOR i := FIRST(Child) TO LAST(Child) DO 
        SUBARRAY(newVals[i]^,0,n - 1) := m.vals[i]^;
        newVals[i][n-1] := newf.eval(PointToVector(m.corner(i)))
      END;
      m.vals := newVals;

      IF m.children # NoChildren THEN
        FOR i := FIRST(Child) TO LAST(Child) DO
          Recurse(m.children[i])
        END
      END
    END Recurse;

  PROCEDURE RecurseHints(m : M) =
    BEGIN
      FOR i := FIRST(Child) TO LAST(Child) DO 
        newf.evalHint(PointToVector(m.corner(i)))
      END;

      IF m.children # NoChildren THEN
        FOR i := FIRST(Child) TO LAST(Child) DO
          RecurseHints(m.children[i])
        END
      END
    END RecurseHints;

  VAR
    n := NUMBER(self.f^) + 1;
    newF := NEW(Funcs, n);
    res : T;
    p : T;
  BEGIN
    SUBARRAY(newF^,0,n - 1) := self.f^;
    newF[n-1] := newf;
    res := NEW(T, root := self.root, me := n - 1, next := self.next);
    self.next := res;

    (* update the Ts that share mesh *)
    p := self;
    REPEAT
      p.f := newF;
      p := p.next
    UNTIL p = self;

    (* update mesh itself *)
    IF newf.doHints THEN RecurseHints(self.root) END;

    Recurse(self.root);
    RETURN res
  END MapNewLRSF;

PROCEDURE Larger(m :M; than : AdGridQ.T) : BOOLEAN = 
  BEGIN 
    WITH tm = NARROW(than,M) DO
      RETURN m.ur.x - m.ll.x > tm.ur.x - tm.ll.x 
    END
  END Larger;

PROCEDURE Range(m : M; adGridP : REFANY) : LONGREAL =
  VAR
    smallest := LAST(LONGREAL);
    largest := FIRST(LONGREAL);
  BEGIN
    WITH me = NARROW(adGridP,T).me DO
      FOR i := FIRST(Child) TO LAST(Child) DO
        WITH vi = m.vals[i][me] DO
          IF vi < smallest THEN smallest := vi END;
          IF vi > largest THEN largest := vi END
        END
      END
    END;
    RETURN largest - smallest
  END Range;

BEGIN END AdGrid.
