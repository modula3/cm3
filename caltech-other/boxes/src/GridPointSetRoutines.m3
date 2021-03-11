(* $Id$ *)

MODULE GridPointSetRoutines;
IMPORT Cost, GridPoint;
IMPORT TextWr,Wr;
IMPORT Thread;
IMPORT CITRandom;
FROM GridPoint IMPORT Layer, ComparX, ComparY;
FROM GridPointSort IMPORT Sort, Comparer;

PROCEDURE NearestPointsSmall(thatSet, inSet : T; 
                             VAR thatP, inP : GridPoint.T; VAR tc : CARDINAL) =
  VAR 
    iter := inSet.iterate();
    tgtPt : GridPoint.T;
    sIter := thatSet.iterate();
    thatA := NEW(REF ARRAY OF GridPoint.T, thatSet.size());
  BEGIN
    tc := LAST(CARDINAL);
    <* ASSERT inSet.size() > 0 AND thatSet.size() > 0 *>
    FOR i := FIRST(thatA^) TO LAST(thatA^) DO
      EVAL sIter.next(thatA[i])
    END;

    WHILE iter.next(tgtPt) DO
      FOR i := FIRST(thatA^) TO LAST(thatA^) DO
        WITH cg = Cost.Greedy(thatA[i], tgtPt) DO
          IF cg < tc THEN tc := cg; thatP := thatA[i]; inP := tgtPt END
        END
      END
    END
  END NearestPointsSmall;

PROCEDURE NearestPointsLarge(s1, s2 : T; VAR s1p, s2p : GridPoint.T; VAR tc : CARDINAL) =

  VAR
    sub1 := Subdivide(s1);
    sub2 := Subdivide(s2);
  BEGIN
    RecurseNS(sub1,sub2,s1p,s2p,tc)
  END NearestPointsLarge;
  
PROCEDURE RecurseNS(k1, k2 : Subdivision; VAR k1p, k2p : GridPoint.T; VAR dist : CARDINAL) =
  BEGIN
    IF ISTYPE(k1.data,SetData) AND ISTYPE(k2.data,SetData) THEN
      WITH k1d = NARROW(k1.data, SetData), k2d = NARROW(k2.data, SetData) DO
        dist := LAST(CARDINAL);
        FOR i := k1d.lo TO k1d.hi DO
          FOR j := k2d.lo TO k2d.hi DO
            WITH p= k1d.set[i], q=k2d.set[j],c = Cost.Greedy(p,q) DO
              IF c < dist THEN
                k1p := p; k2p := q; dist := c
              END
            END
          END
        END
      END
    ELSIF ISTYPE(k2.data, TreeData) THEN
      (* interesting case *)
      WITH k2t = NARROW(k2.data, TreeData) DO
        VAR
          sdl := SomeDistance(k1, k2t.l);
          sdr := SomeDistance(k1, k2t.r);
        BEGIN
          (*
            <* ASSERT MayBeLessOrEqDistance(k1, k2t.l, sdl) *>
            <* ASSERT MayBeLessOrEqDistance(k1, k2t.r,sdr) *>
          *)
          IF    NOT MayBeLessOrEqDistance(k1,k2t.l,sdr) THEN (* r only *)
            RecurseNS(k2t.r, k1, k2p, k1p, dist)
          ELSIF NOT MayBeLessOrEqDistance(k1,k2t.r,sdl) THEN (* l only *)
            RecurseNS(k2t.l, k1, k2p, k1p, dist)
          ELSE
            VAR
              ppr1, ppr2, ppl1, ppl2 : GridPoint.T;
              dr, dl : CARDINAL;
            BEGIN
              RecurseNS(k2t.r, k1, ppr2, ppr1, dr);
              RecurseNS(k2t.l, k1, ppl2, ppl1, dl);
              
              IF dr < dl THEN
                k2p := ppr2; k1p := ppr1; dist := dr
              ELSE
                k2p := ppl2; k1p := ppl1; dist := dl
              END
            END
          END
        END
      END
    ELSIF ISTYPE(k1.data, TreeData) THEN
      RecurseNS(k2, k1, k2p, k1p, dist)
    ELSE
      <* ASSERT FALSE *>
    END
  END RecurseNS;

PROCEDURE SomeDistance(from, outside : Subdivision) : CARDINAL =
  CONST 
    NumToPick = 2;
  VAR
    dist := LAST(CARDINAL);
  BEGIN
    FOR i := 1 TO NumToPick DO
      FOR j := 1 TO NumToPick DO
        WITH p = PickOne(from), q = PickOne(outside), c = Cost.Greedy(p,q) DO
          IF c < dist THEN

            (*
            Debug.Out("SomeDistance.. new points p=" & GridPoint.Format(p) & 
              " q=" & GridPoint.Format(q) & " dist=" & Fmt.Int(c));
            *)

            dist := c
          END
        END
      END
    END;
    RETURN dist
  END SomeDistance;

VAR
  pick := 0;

PROCEDURE PickOne(s : Subdivision) : GridPoint.T =
  VAR
    res : GridPoint.T;
  BEGIN
    TYPECASE s.data OF
      TreeData(sd) => 
        pick := 1-pick;
        IF pick = 0 THEN 
          res := PickOne(sd.l)
        ELSE
          res := PickOne(sd.r)
        END
    |
      SetData(sd) => res := sd.set[sd.lo]
    ELSE
      <* ASSERT FALSE *>
    END;
    <* ASSERT res.x >= s.ll.x *>
    <* ASSERT res.y >= s.ll.y *>
    <* ASSERT res.l >= s.ll.l *>
    <* ASSERT res.x <= s.ur.x *>
    <* ASSERT res.y <= s.ur.y *>
    <* ASSERT res.l <= s.ur.l *>

    RETURN res
  END PickOne;

PROCEDURE NearestPoints(thatSet, inSet : T; VAR thatP, inP : GridPoint.T; VAR tc : CARDINAL) =
  BEGIN
    IF thatSet.size() > MaxArr*MinArrs AND inSet.size() > MaxArr*MinArrs THEN
      NearestPointsLarge(thatSet, inSet, thatP, inP, tc);

(*
      VAR
        ppp, qqq : GridPoint.T;
        ttt : CARDINAL;
      BEGIN
        NearestPointsSmall(thatSet, inSet, ppp,qqq, ttt);
        <* ASSERT ttt = tc *>
      END
*)

    ELSE
      NearestPointsSmall(thatSet, inSet, thatP, inP, tc)
    END
  END NearestPoints;

PROCEDURE NearestPoint(a : GridPoint.T; inSet : T) : GridPoint.T =
  VAR
    iter := inSet.iterate();
    p, res : GridPoint.T;
    cost := LAST(CARDINAL);
  BEGIN
    WHILE iter.next(p) DO
      WITH gc = Cost.Greedy(p,a) DO
        IF gc < cost THEN cost := gc; res := p END;
      END
    END;
    RETURN res
  END NearestPoint;

<*UNUSED*>
PROCEDURE OldCostExtent(a : T) : CARDINAL =
  VAR
    array := NEW(REF ARRAY OF GridPoint.T, a.size());
    res := 0;
    iter := a.iterate();
  BEGIN
    FOR i := FIRST(array^) TO LAST(array^) DO
      EVAL iter.next(array[i])
    END;

    FOR i := FIRST(array^) TO LAST(array^) DO
      FOR j := i + 1 TO LAST(array^) DO
        WITH gc = Cost.Greedy(array[i],array[j]) DO IF gc > res THEN res := gc END END
      END
    END;
    RETURN res
  END OldCostExtent;

PROCEDURE CostExtent(a : T) : CARDINAL =
  VAR
    array := NEW(REF ARRAY OF GridPoint.T, a.size());
    res := 0;
    iter := a.iterate();
  BEGIN
    FOR i := FIRST(array^) TO LAST(array^) DO
      EVAL iter.next(array[i])
    END;

    IF NUMBER(array^) < 30 THEN
      FOR i := FIRST(array^) TO LAST(array^) DO
        FOR j := i + 1 TO LAST(array^) DO
          WITH gc = Cost.Greedy(array[i],array[j]) DO IF gc > res THEN res := gc END END
        END
      END
    ELSE
      FOR i := 1 TO 1000 DO
        LOCK mu DO
          WITH gc = Cost.Greedy(array[rand.integer(FIRST(array^),LAST(array^))],
                                array[rand.integer(FIRST(array^),LAST(array^))]) DO 
            IF gc > res THEN res := gc END 
          END
        END
      END
    END;
    RETURN res
  END CostExtent;

PROCEDURE Format(a : T) : TEXT =
  <* FATAL Thread.Alerted, Wr.Failure *>
  VAR
    iter := a.iterate();
    p : GridPoint.T;
    wr := NEW(TextWr.T).init();
  BEGIN
    Wr.PutText(wr,"{ ");
    WHILE iter.next(p) DO
      Wr.PutText(wr,GridPoint.Format(p));
      Wr.PutChar(wr,' ')
    END;
    Wr.PutText(wr,"}");
    RETURN TextWr.ToText(wr)
  END Format;

TYPE 
  Dim = { X, Y };

  Data = OBJECT END;

  SetData = Data OBJECT lo, hi : CARDINAL; set : REF ARRAY OF GridPoint.T END;
  TreeData = Data OBJECT l, r : Subdivision END;

  Subdivision = OBJECT
    ll, ur : GridPoint.T;
    p : GridPoint.T;
    data : Data;
  END;

CONST OtherDim = ARRAY Dim OF Dim { Dim.Y, Dim.X };

PROCEDURE CorrectOverlaps(VAR al, au, bl, bu : INTEGER) =
  BEGIN
    IF al <= bu AND au >= bl THEN
      al := MAX(al,bl);
      bl := al;

      au := MIN(au,bu);
      bu := au
    END
  END CorrectOverlaps;

TYPE Corners4 = ARRAY [0..3] OF GridPoint.T;

PROCEDURE FourCorners(ll, ur : GridPoint.T) : Corners4 =
  VAR
    res : Corners4;
  BEGIN
    FOR i := 0 TO 3 DO
      IF i MOD 2 = 0 THEN
        res[i].x := ll.x
      ELSE
        res[i].x := ur.x
      END;
      IF (i DIV 2) MOD 2 = 0 THEN 
        res[i].y := ll.y
      ELSE
        res[i].y := ur.y
      END
    END;
    RETURN res
  END FourCorners;

PROCEDURE MayBeLessOrEqDistance(a, b : Subdivision; min : CARDINAL) : BOOLEAN =
  VAR
    all := a.ll; aur := a.ur;
    bll := b.ll; bur := b.ur;
  BEGIN
    (* fix any overlaps in X and Y *)
    CorrectOverlaps(all.x,aur.x,bll.x,bur.x);
    CorrectOverlaps(all.y,aur.y,bll.y,bur.y);
    
    (* what about the layers? -- if they overlap, we have to expand *)

    IF all.l <= bur.l AND aur.l >= bll.l THEN
      all.l := MIN(all.l,bll.l);
      bll.l := all.l;
      aur.l := MAX(aur.l,bur.l);
      bur.l := aur.l
    END;
    
    BEGIN
      WITH ac = FourCorners(all,aur), bc = FourCorners(bll,bur) DO
        FOR i := FIRST(ac) TO LAST(ac) DO
          FOR j := FIRST(bc) TO LAST(bc) DO
            FOR k := MAX(aur.l,bur.l) TO MIN(all.l,bll.l) BY -1 DO
              FOR l := MAX(aur.l,bur.l) TO MIN(all.l,bll.l) BY -1 DO
                IF Cost.Greedy(GridPoint.T {ac[i].x, ac[i].y, k},
                               GridPoint.T {bc[j].x, bc[j].y, l}) <= min THEN
                  RETURN TRUE
                END
              END
            END
          END
        END
      END;
      RETURN FALSE
    END
  END MayBeLessOrEqDistance;

CONST MaxArr = 64; (* := Scan.Int(Env.Get("MAXARR")); *)
CONST MinArrs = 1; (* := Scan.Int(Env.Get("MINARRS")); *)
  
PROCEDURE Subdivide(a : T) : Subdivision =

  PROCEDURE Recurse(lo, hi : CARDINAL; dim : Dim) : Subdivision =
    VAR
      res : Subdivision;
    BEGIN
      IF hi - lo <= MaxArr THEN
        res := NEW(Subdivision, data := NEW(SetData, lo := lo, hi := hi, set := arr));
        res.ll.x := LAST(INTEGER);
        res.ll.y := LAST(INTEGER);
        res.ll.l := LAST(Layer);
        res.ur.x := FIRST(INTEGER);
        res.ur.y := FIRST(INTEGER);
        res.ur.l := FIRST(Layer);

        FOR i := lo TO hi DO
          res.ll.x := MIN(res.ll.x,arr[i].x);
          res.ll.y := MIN(res.ll.y,arr[i].y);
          res.ll.l := MIN(res.ll.l,arr[i].l);

          res.ur.x := MAX(res.ur.x,arr[i].x);
          res.ur.y := MAX(res.ur.y,arr[i].y);
          res.ur.l := MAX(res.ur.l,arr[i].l);
        END;
        res.p := arr[lo]
      ELSE
        VAR
          mid := (lo + hi) DIV 2;
          d : TreeData;
        CONST
          Sorter = ARRAY Dim OF Comparer { ComparX, ComparY };
        BEGIN
          Sort(arr^, lo, hi, Sorter[dim]);
          res := NEW(Subdivision, data := NEW(TreeData,l := Recurse(lo, mid-1, OtherDim[dim]),r := Recurse(mid, hi, OtherDim[dim]) ));
          d := res.data;

          res.ll.x := MIN(d.l.ll.x,d.r.ll.x);
          res.ll.y := MIN(d.l.ll.y,d.r.ll.y);
          res.ll.l := MIN(d.l.ll.l,d.r.ll.l);
          res.ur.x := MAX(d.l.ur.x,d.r.ur.x);
          res.ur.y := MAX(d.l.ur.y,d.r.ur.y);
          res.ur.l := MAX(d.l.ur.l,d.r.ur.l);
          res.p := d.l.p
        END
      END;
      RETURN res
    END Recurse;

  VAR
    arr := NEW(REF ARRAY OF GridPoint.T, a.size());
    iter := a.iterate();
    p : GridPoint.T;
    i := 0;
  BEGIN
    WHILE iter.next(p) DO
      arr[i] := p;
      INC(i)
    END;
    RETURN Recurse(FIRST(arr^), LAST(arr^), Dim.X)
  END Subdivide;

VAR
  mu := NEW(MUTEX);
  rand := NEW(CITRandom.T).init(fixed := TRUE, seed := 42);
BEGIN END GridPointSetRoutines.
