(* Copyright 1992 Digital Equipment Corporation.             *)
(* Distributed only by permission.                           *)
(* Last modified on Fri Apr 29 10:54:01 PDT 1994 by najork   *)
(*      modified on Mon Oct 12 22:45:18 PDT 1992 by ramshaw  *)
(*      modified on Tue Jul 28 20:26:21 1992 by saxe         *)
(*      modified on Tue Jul 21 06:24:57 1992 by mhb          *)

MODULE GrahamAlg;

IMPORT Algorithm, FormsVBT, HullAlgClass, HullIE, Random, Rd, Text, Thread, 
       VBT, ZeusPanel;

<*FATAL FormsVBT.Error*>
<*FATAL FormsVBT.Unimplemented*>
<*FATAL Rd.Failure*>

TYPE
  Site = RECORD uid: INTEGER; x, y: LONGREAL; END;
  Sites = REF ARRAY OF Site;  

TYPE 
  T = HullAlgClass.T BRANDED OBJECT 
      n: INTEGER; (* number of sites *)
      sites: Sites;
      lowest: INTEGER;
    OVERRIDES 
      run := Run; 
    END;

VAR
  rand := NEW (Random.Default).init ();

PROCEDURE Run (alg: T) RAISES {Thread.Alerted} =
  VAR
    n: INTEGER;
    s: Text.T;
  BEGIN
    LOCK VBT.mu DO n := FormsVBT.GetInteger(alg.data, "N"); END;
    GenerateSites(alg, n);
    FindLowest(alg);
    LOCK VBT.mu DO s := FormsVBT.GetChoice(alg.data, "sort"); END;
    HullIE.Sorted (alg, 1);
    IF Text.Equal(s, "qs") THEN
      HullIE.Phase (alg, "Sorting by angle with quicksort");
      QuickSortSites(alg, 2, alg.n);
    ELSIF Text.Equal (s, "ss") THEN
      HullIE.Phase (alg, "Sorting by angle with selection sort");
      SelectionSortSites (alg);
    END;
    Scan(alg);
    HullIE.Phase(alg, "Done");
  END Run;

PROCEDURE GenerateSites (alg: T; n: INTEGER) RAISES {Thread.Alerted} =
  VAR
    i: INTEGER;
    x, y: LONGREAL;
  BEGIN
    alg.n := n;
    alg.sites := NEW(Sites, n+1);
    HullIE.Setup (alg, n);
    HullIE.Phase (alg, "Generating data points");
    i := 1;
    WHILE i <= n DO
      x := rand.longreal (-1.0d0, 1.0d0);
      y := rand.longreal (-1.0d0, 1.0d0);
      IF x*x + y*y <= 1.0d0 THEN
        alg.sites[i] := Site{i, x, y};
        HullIE.NewSite (alg, i, x, y);
        i := i + 1;
      END;
    END;
  END GenerateSites;

PROCEDURE FindLowest (alg: T) RAISES {Thread.Alerted} =
  VAR
    lowY: LONGREAL;
  BEGIN
    HullIE.Phase(alg, "Finding lowest point");
    alg.lowest := 1;
    lowY := alg.sites[1].y;
    HullIE.NewLow(alg, 1, lowY);
    FOR i := 2 TO alg.n DO
      HullIE.TryLow(alg, i, alg.sites[i].y);
      IF alg.sites[i].y < lowY THEN
        alg.lowest := i;
        lowY := alg.sites[i].y;
        HullIE.NewLow(alg, i, lowY);
      END;
    END;
    Swap(alg, 1, alg.lowest);
  END FindLowest;

(* Return TRUE if arguments are in strictly counterclockwise order (assuming
   a right-handed coordinate system). *)
PROCEDURE CCW (a, b, c: Site): BOOLEAN =
  BEGIN
    RETURN ( (a.x*(b.y-c.y) + b.x*(c.y-a.y) + c.x*(a.y-b.y)) > 0.0d0);
  END CCW;
           
PROCEDURE Swap(alg: T; i, j: INTEGER) RAISES {Thread.Alerted} =
  VAR
    temp: Site;
  BEGIN
    temp := alg.sites[i];
    alg.sites[i] := alg.sites[j];
    alg.sites[j] := temp;
    HullIE.Swap(alg,i,j);
  END Swap;

PROCEDURE SelectionSortSites (alg: T) RAISES {Thread.Alerted} =
  BEGIN
    FOR i := 2 TO alg.n-1 DO
      HullIE.NewCW(alg, 1, i);
      FOR j := i+1 TO alg.n DO
        HullIE.TryCW(alg, j);
        IF CCW(alg.sites[1], alg.sites[j], alg.sites[i]) THEN
          Swap(alg, i, j);
        END;
      END;
      HullIE.Sorted(alg, i);
    END;
    HullIE.Sorted(alg, alg.n);
  END SelectionSortSites;

PROCEDURE QuickSortSites (alg: T; l, r: INTEGER) RAISES {Thread.Alerted} =
  VAR i, j: INTEGER;
  BEGIN
    HullIE.StartSort(alg, l, r);
    IF r = l THEN HullIE.Sorted(alg, l); END;
    IF r > l THEN
      i := l-1; j := r;
      HullIE.NewCW(alg, 1, r);
      REPEAT
        REPEAT
          i := i + 1;
          HullIE.TryCW(alg, i);
        UNTIL NOT CCW(alg.sites[1], alg.sites[i], alg.sites[r]);
        REPEAT
          j := j - 1;
        HullIE.TryCW(alg, j);
        UNTIL NOT CCW(alg.sites[1], alg.sites[r], alg.sites[j]);
        IF j > i THEN
          Swap(alg, i, j);
        END;
      UNTIL j <= i;
      Swap(alg, r, i);
      HullIE.Sorted(alg, i);
      QuickSortSites (alg, l, i-1);
      QuickSortSites (alg, i+1, r);
    END;
  END QuickSortSites;


PROCEDURE Scan(alg: T) RAISES {Thread.Alerted} =
  VAR
    m: INTEGER;
  BEGIN
    HullIE.Phase(alg, "Scanning hull");
    HullIE.InHull(alg, 1, 2, 2);
    HullIE.InHull(alg, 2, 3, 3);
    m := 2;
    FOR i := 4 TO alg.n DO
      m := m + 2;
      REPEAT
        m := m - 1;
        HullIE.NewCW(alg, i, m);
        HullIE.TryCW(alg, m-1);
      IF CCW(alg.sites[i], alg.sites[m], alg.sites[m-1]) THEN
        HullIE.NotInHull(alg, m);
      END;
      UNTIL NOT(CCW(alg.sites[i], alg.sites[m], alg.sites[m-1]));
      Swap(alg, i, m+1);
      HullIE.InHull(alg, m, m+1, i);
    END;              
    HullIE.InHull(alg, m+1, 1, 1);
  END Scan;

PROCEDURE New (): Algorithm.T =
  VAR fv := ZeusPanel.NewForm("hullinput.fv");
  BEGIN
    RETURN NEW(T, data := fv).init()
  END New;

BEGIN
  ZeusPanel.RegisterAlg(New, "Graham_Scan", "Hull");
END GrahamAlg.
