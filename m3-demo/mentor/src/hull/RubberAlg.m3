(* Copyright 1992 Digital Equipment Corporation.            *)
(* Distributed only by permission.                          *)
(* Last modified on Tue Jan 31 14:29:14 PST 1995 by kalsow  *)
(*      modified on Thu Jan  5 21:41:51 PST 1995 by najork  *)
(*      modified on Sat Oct 17 13:01:19 PDT 1992 by ramshaw *)
(*      modified on Tue Jul 28 20:26:21 1992 by saxe        *)

MODULE RubberAlg;

IMPORT Algorithm, AlgTypes, HullAlgClass, HullIE, HullInput, IntList, Site, 
       SiteList, Thread, ZeusPanel;

TYPE 
  T = HullAlgClass.T BRANDED OBJECT 
      nSites: INTEGER;
      nTotalSites: INTEGER;
      wayLeft, skoshLeft, top: INTEGER; (* uid's of auxiliary sites *)
      sites: AlgTypes.Sites;
      lowest: INTEGER;
    OVERRIDES 
      run := Run; 
    END;

PROCEDURE Run (alg: T) RAISES {Thread.Alerted} =
  BEGIN
    TRY PrepareSites(alg);
        FindLowest(alg);
        Scan(alg);
      EXCEPT HullInput.Failure => RETURN
    END;
  END Run;

PROCEDURE PrepareSites (alg: T) RAISES {Thread.Alerted, HullInput.Failure} =

  VAR
    n: INTEGER;
    trueSiteList, auxSiteList: SiteList.T;
    trueSites: AlgTypes.Sites;
  BEGIN
    trueSites := HullInput.GetSites(alg, n);
    IF n < 3 THEN RAISE HullInput.Failure END;
    alg.nSites := n;
    alg.nTotalSites := n + 5;  (* 2 for sentinels, 3 for auxiliaries *)
    alg.sites := NEW(AlgTypes.Sites, alg.nTotalSites);
    FOR i := 1 TO n DO alg.sites[i] := trueSites[i] END;
    alg.wayLeft := n+2;
    alg.sites[alg.wayLeft] := 
      AlgTypes.Site{alg.wayLeft, "<-", -1, 0, AlgTypes.Relativity.Big};
    alg.skoshLeft := n+3;
    alg.sites[alg.skoshLeft] := 
       AlgTypes.Site{alg.skoshLeft, "<", -1, 0, AlgTypes.Relativity.Small};
    alg.top := n+4;
    alg.sites[alg.top] := 
      AlgTypes.Site{alg.top, "Top", alg.sites[1].x, 15000};

    auxSiteList := SiteList.List3 (
                       Site.T {uid  := alg.top,
                               lab  := alg.sites[alg.top].lab,
                               x    := FLOAT(alg.sites[1].x, REAL)/10000.0,
                               y    := 1.5,
                               bool := FALSE},
                       Site.T {uid  := alg.skoshLeft,
                               lab  := alg.sites[alg.skoshLeft].lab,
                               x    := -0.001,
                               y    := 0.0,
                               bool := TRUE},
                       Site.T {uid  := alg.wayLeft,
                               lab  := alg.sites[alg.wayLeft].lab,
                               x    := -2.5,
                               y    := 0.0,
                               bool := TRUE});

    trueSiteList := NIL;
    FOR i := n TO 1 BY -1 DO 
      trueSiteList := SiteList.Cons (
                          Site.T {uid  := i,
                                  lab  := alg.sites[i].lab,
                                  x    := FLOAT(alg.sites[i].x, REAL)/10000.0,
                                  y    := FLOAT(alg.sites[i].y, REAL)/10000.0,
                                  bool := FALSE (* DUMMY VALUE *)},
                          trueSiteList);
    END;
    HullIE.Setup(alg, trueSiteList, auxSiteList);
  END PrepareSites;

                                 (* The three points:           *)
TYPE LR = {Left, Right,          (*  form a triangle.           *)
           Back, Shaft, Front,   (*  collinear, but distinct.   *) 
           Tail, Head, DegenOff, (*  2 coincide, but not all 3. *)
           DegenOn};             (*  all 3 points coincide.     *)

PROCEDURE TestLR(tail, head, new: AlgTypes.Site): LR =
(* Compute the relative orientation of a triple of points, from among 
   the nine possibilities.  In the seven cases where "tail" and "head" 
   do not coincide, we classify "new" with respect to a vector from 
   "tail" to "head".  In the remaining two cases, we merely check 
   whether or not "new" coincides with the common value of "head" 
   and "tail". *) 
  VAR area: INTEGER :=   (head.x - tail.x) * (new.y - tail.y)
                       - (head.y - tail.y) * (new.x - tail.x);
      (* The signed area of the parallelogram spanned by the vectors 
         from "tail" to "head" and from "tail" to "new";  you can 
         think of it either as a 3-by-3 determinant or as the norm 
         of a vector cross product. *) 
  BEGIN
    <* ASSERT tail.rel = AlgTypes.Relativity.Absolute *>
    <* ASSERT head.rel = AlgTypes.Relativity.Absolute *>
    <* ASSERT new.rel = AlgTypes.Relativity.Absolute *>
    IF    area > 0 THEN RETURN LR.Left
    ELSIF area < 0 THEN RETURN LR.Right
    ELSE
      VAR (* The following are Manhattan distances. *)
        distTailHead: INTEGER:=ABS(head.x - tail.x) + ABS(head.y - tail.y);
        distTailNew: INTEGER := ABS(new.x - tail.x) + ABS(new.y - tail.y);
        distHeadNew: INTEGER := ABS(new.x - head.x) + ABS(new.y - head.y);
        max: INTEGER := MAX(MAX(distTailHead, distTailNew), distHeadNew);
      BEGIN
        IF    max = 0            THEN RETURN LR.DegenOn
        ELSIF distTailHead = 0   THEN RETURN LR.DegenOff
        ELSIF distTailNew = 0    THEN RETURN LR.Tail
        ELSIF distHeadNew = 0    THEN RETURN LR.Head
        ELSIF max = distTailHead THEN RETURN LR.Shaft
        ELSIF max = distTailNew  THEN RETURN LR.Front
        ELSIF max = distHeadNew  THEN RETURN LR.Back
        ELSE <* ASSERT FALSE *>
        END;
      END;
    END;
  END TestLR;

PROCEDURE FindLowest (alg: T) =
  VAR
    lowY, lowYsX : INTEGER;
    where: [4 .. 9];
  BEGIN
    alg.lowest := 1;
    lowY := alg.sites[1].y;
    lowYsX := alg.sites[1].x;
    FOR i := 2 TO alg.nSites DO
      IF alg.sites[i].y < lowY      THEN where := 4
      ELSIF alg.sites[i].y > lowY   THEN where := 7
      ELSIF alg.sites[i].x < lowYsX THEN where := 8
      ELSIF alg.sites[i].x > lowYsX THEN where := 5
      ELSE                               where := 9 
      END;
      CASE where OF
      | 4,5 => alg.lowest := i;
               lowY := alg.sites[i].y;
               lowYsX := alg.sites[i].x;
      | 7,8,9 =>
      | 6 => <* ASSERT FALSE *>
      END;
    END;
  END FindLowest;

PROCEDURE Swap(alg: T; i, j: INTEGER) =
  VAR
    temp: AlgTypes.Site;
  BEGIN
    temp := alg.sites[i];
    alg.sites[i] := alg.sites[j];
    alg.sites[j] := temp;
  END Swap;

PROCEDURE Scan(alg: T) RAISES {Thread.Alerted, HullInput.Failure} =
  PROCEDURE At(line: INTEGER) =
    BEGIN where := line END At;
  VAR
    grn, where: INTEGER;
    hullSites, otherSites: IntList.T := NIL;
  BEGIN
    grn := alg.lowest;
    FOR purp := 1 TO alg.nSites DO
      Swap(alg, purp, grn);
      grn := 1;
      FOR blu := purp + 1 TO alg.nSites DO
        CASE TestLR(alg.sites[purp], alg.sites[grn], alg.sites[blu]) OF <*NOWARN*>
        | LR.Right => At(15)
        | LR.Front => At(16)
        | LR.DegenOff => At(17)
        | LR.Left => At(19)
        | LR.Tail => At(20)
        | LR.Shaft => At(21)
        | LR.Head => At(22)
        | LR.DegenOn => At(23)
        END;
        CASE where OF <*NOWARN*>
        | 15, 16, 17 => grn := blu;
        | 19, 20, 21, 22, 23 => 
        END;
      END;
      IF grn = 1 THEN 
        IF purp < 3 THEN RAISE HullInput.Failure END;
        FOR i := purp TO 1 BY -1 DO
          hullSites := IntList.Cons (alg.sites[i].uid, hullSites);
        END;
        FOR i := alg.nSites TO purp+1 BY -1 DO
          otherSites := IntList.Cons (alg.sites[i].uid, otherSites);
        END;
        HullIE.Stretch(alg, hullSites, otherSites);
        HullIE.Snap(alg, hullSites, otherSites);
        HullIE.Shuffle(alg, hullSites, otherSites);
        RETURN;
      END;
    END;
    RAISE HullInput.Failure;
  END Scan;

PROCEDURE New (): Algorithm.T =
  VAR fv := ZeusPanel.NewForm("hullinput.fv");
  BEGIN
    RETURN NEW(T, data := fv).init()
  END New;


BEGIN
  ZeusPanel.RegisterAlg(New, "Rubber Band", "Hull");
END RubberAlg.
