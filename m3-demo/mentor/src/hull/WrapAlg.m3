(* Copyright 1992 Digital Equipment Corporation.            *)
(* Distributed only by permission.                          *)
(* Last modified on Thu Aug 22 16:31:01 PDT 1996 by najork  *)
(*      modified on Thu Jan 19 10:48:55 PST 1995 by mhb     *)
(*      modified on Sat Oct 17 11:50:06 PDT 1992 by ramshaw *)
(*      modified on Tue Jul 28 20:26:21 1992 by saxe        *)

MODULE WrapAlg;

IMPORT Algorithm, AlgTypes, HullAlgClass, HullIE, HullInput, RefList, Site, 
       SiteList, Thread, ZeusCodeView, ZeusPanel;

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

PROCEDURE Run (alg: T) RAISES {Thread.Alerted} =
  BEGIN
    ZeusCodeView.Event(alg, procedureName := "PackageWrap");
    TRY PrepareSites(alg)
      EXCEPT HullInput.Failure => RETURN
    END;
    FindLowest(alg);
    Scan(alg);
  END Run;

PROCEDURE PrepareSites (alg: T) RAISES {Thread.Alerted, HullInput.Failure} =
  VAR
    n: INTEGER;
    trueSiteList, auxSiteList: SiteList.T;
    trueSites: AlgTypes.Sites;
  BEGIN
    trueSites := HullInput.GetSites(alg, n);
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
                                  bool := FALSE (* DUMMY VALUE *) },
                          trueSiteList);
    END;
    HullIE.Setup(alg, trueSiteList, auxSiteList);
  END PrepareSites;

PROCEDURE FindLowest (alg: T) RAISES {Thread.Alerted} =
  PROCEDURE At(line: INTEGER) RAISES {Thread.Alerted} =
    BEGIN ZeusCodeView.Event(alg, line); where := line END At;
  VAR
    lowY, lowYsX, where : INTEGER;
  BEGIN
    At(1); HullIE.SetHalfPlane(alg, alg.wayLeft, 1);
    alg.lowest := 1;
    lowY := alg.sites[1].y;
    lowYsX := alg.sites[1].x;
    FOR i := 2 TO alg.nSites DO
      At(2); HullIE.TestSite(alg, i);
      IF alg.sites[i].y < lowY THEN At(4)
      ELSIF alg.sites[i].y > lowY THEN At(7)
      ELSIF alg.sites[i].x < lowYsX THEN At(8)
      ELSIF alg.sites[i].x > lowYsX THEN At(5)
      ELSE At(9) 
      END;
      CASE where OF
        | 4,5 => At(6); alg.lowest := i;
                lowY := alg.sites[i].y;
                lowYsX := alg.sites[i].x;
                HullIE.MoveHalfPlane(alg, alg.wayLeft, i);
        | 7,8,9 => 
        ELSE <* ASSERT FALSE *>
      END; 
    END;
    At(2); HullIE.ClearTest(alg);
    At(99); HullIE.Confirm(alg, alg.wayLeft, alg.lowest);
  END FindLowest;

PROCEDURE Swap(alg: T; i, j: INTEGER) RAISES {Thread.Alerted} =
  VAR
    temp: AlgTypes.Site;
  BEGIN
    temp := alg.sites[i];
    alg.sites[i] := alg.sites[j];
    alg.sites[j] := temp;
    HullIE.Swap(alg, alg.sites[i].uid, alg.sites[j].uid);
  END Swap;

PROCEDURE Scan(alg: T) RAISES {Thread.Alerted} =
  PROCEDURE At(line: INTEGER) RAISES {Thread.Alerted} =
    BEGIN ZeusCodeView.Event(alg, line); where := line END At;
  VAR
    grn, where: INTEGER;
  BEGIN
    grn := alg.lowest;
    FOR purp := 1 TO alg.nSites DO
      At(10); HullIE.SetTail(alg, alg.sites[purp].uid);
      At(11); Swap(alg, purp, grn);
      At(98); HullIE.ClearHead(alg);
      At(12); grn := 1;
      HullIE.SetHalfPlane(alg, alg.sites[purp].uid, alg.sites[grn].uid);
      FOR blu := purp + 1 TO alg.nSites DO
        At(13);  HullIE.TestSite(alg, alg.sites[blu].uid);
        CASE TestLR(alg.sites[purp], alg.sites[grn], alg.sites[blu]) OF
        | LR.Right => At(15)
        | LR.Front => At(16)
        | LR.DegenOff => At(17)
        | LR.Left => At(19)
        | LR.Tail => At(20)
        | LR.Shaft => At(21)
        | LR.Head => At(22)
        | LR.DegenOn => At(23)
        ELSE <* ASSERT FALSE *>
        END;
        CASE where OF
        | 15, 16, 17 => At(18); 
                        HullIE.MoveHalfPlane(alg, alg.sites[purp].uid,
                                             alg.sites[blu].uid);
                        grn := blu;
        | 19, 20, 21, 22, 23 => 
        ELSE <* ASSERT FALSE *>
        END;
      END;
      At(13); HullIE.ClearTest(alg);
      At(97); HullIE.Confirm(alg, alg.sites[purp].uid, alg.sites[grn].uid);
      At(24); IF grn = 1 THEN At(25); HullIE.ClearHead(alg); RETURN END;
    END;
    At(10); At(26);
  END Scan;

PROCEDURE New (): Algorithm.T =
  VAR 
    fv := ZeusPanel.NewForm ("hullinput.fv");
    cv := RefList.List1 (RefList.List2 ("M3 Code View", "WrapAlg.m3.cv"));
  BEGIN
    RETURN NEW (T, data := fv, codeViews := cv).init ();
  END New;

BEGIN
  ZeusPanel.RegisterAlg(New, "Package Wrap", "Hull");
END WrapAlg.
