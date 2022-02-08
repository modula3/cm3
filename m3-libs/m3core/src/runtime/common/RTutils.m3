(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jul 24 10:21:50 PDT 1995 by detlefs                  *)
(*      modified on Thu May  4 15:23:07 PDT 1995 by kalsow                   *)
(*      modified on Tue Jun 16 10:41:32 PDT 1992 by muller                   *)
(*      modified on Sun Mar  1 16:06:32 PST 1992 by meehan                   *)

UNSAFE MODULE RTutils;

IMPORT RTHeapRep, RTType, RTTypeSRC, RTIO, RT0, RTAllocStats;

TYPE
  TypeDesc = RECORD
    total : Stat;
    sites : StatList := NIL;
  END;

  Stat = RECORD
    count : INTEGER := 0;
    size  : INTEGER := 0;
  END;

  Link = RECORD
    children : INTEGER;
    next     : INTEGER;
  END;

TYPE
  R = REF ARRAY OF TypeDesc;
  Map = REF ARRAY OF INTEGER;
  StatList = REF ARRAY OF Stat;
  LinkList = REF ARRAY OF Link;
            
  Visitor = RTHeapRep.RefVisitor OBJECT
              r         : R;
              countSum  := 0;
              sizeSum   := 0
            OVERRIDES
              visit := Walk
            END;

CONST NO_LINK = RTType.NoSuchType;  (* an impossible typecode *)

VAR v := NewVisitor ();
    
PROCEDURE NewVisitor (): Visitor =
  BEGIN
    RETURN NEW (Visitor, r := NEW (R, RTType.MaxTypecode() + 1));
  END NewVisitor;

PROCEDURE Heap (suppressZeros := FALSE;
                presentation := HeapPresentation.ByTypecode;
                byTypeHierarchy := FALSE;
                window := LAST(INTEGER)) =
  BEGIN
    Compute ();
    Report (v, suppressZeros, presentation, byTypeHierarchy, window)
  END Heap;
  
PROCEDURE NewHeap (suppressZeros := FALSE;
                   presentation := HeapPresentation.ByTypecode;
                   byTypeHierarchy := FALSE;
                   window := LAST(INTEGER)) =
  VAR oldv := v;
  BEGIN
    Compute ();
    Report (Delta (v, oldv), suppressZeros, presentation, byTypeHierarchy, window)
  END NewHeap;

PROCEDURE Compute () =
  BEGIN
    v := NewVisitor ();
    RTHeapRep.VisitAllRefs (v)
  END Compute;

PROCEDURE Delta (v1, v2: Visitor): Visitor =
  VAR v := NewVisitor ();
  BEGIN
    v.countSum := v1.countSum - v2.countSum;
    v.sizeSum := v1.sizeSum - v2.sizeSum;
    FOR i := 0 TO LAST (v.r^) DO
      WITH x = v.r[i].total, a = v1.r[i].total, b = v2.r[i].total DO
        x.count := a.count - b.count;
        x.size  := a.size  - b.size;
      END;
    END;
    RETURN v
  END Delta;

PROCEDURE Report (v: Visitor;
                  suppressZeros: BOOLEAN;
                  presentation: HeapPresentation;
                  byTypeHierarchy := FALSE;
                  window: INTEGER) =
  VAR
    nPrinted := 0;
    map := NEW (Map, NUMBER (v.r^));
    sums: R;
    links: LinkList := NIL;
    root: RT0.Typecode;
  BEGIN
    (* report an entry for each distinct type *)
    FOR i := 0 TO LAST (map^) DO map[i] := i; END;
    CASE presentation OF
    | HeapPresentation.ByTypecode  => (*SKIP*)
    | HeapPresentation.ByNumber    => Sort (map, v.r, CompareCount)
    | HeapPresentation.ByByteCount => Sort (map, v.r, CompareSize)
    END;
    RTIO.PutText (
      (* 012345678901234567890123456789012345678901234567890 *)
        "Code   Count   TotalSize  AvgSize  Name\n"
      & "---- --------- --------- --------- --------------------------\n");
    FOR i := 0 TO LAST (v.r^) DO
      IF (nPrinted >= window) THEN EXIT; END;
      WITH tc = map[i], zz = v.r[tc] DO
        IF (zz.total.count > 0) OR (NOT suppressZeros) THEN
          RTIO.PutInt (tc, 4);
          RTIO.PutInt (zz.total.count, 10);
          RTIO.PutInt (zz.total.size, 10);
          IF (zz.total.count = 0)
            THEN RTIO.PutText ("         0");
            ELSE RTIO.PutInt  (zz.total.size DIV zz.total.count, 10);
          END;
          RTIO.PutChar (' ');
          RTIO.PutText (RTTypeSRC.TypecodeName (tc));
          RTIO.PutChar ('\n');
          INC(nPrinted);
          IF (zz.sites # NIL) THEN
            PrintSites (tc, zz, presentation, window);
          END;
        END
      END;
    END;
    RTIO.PutText ("     --------- ---------\n    ");
    RTIO.PutInt  (v.countSum, 10);
    RTIO.PutInt  (v.sizeSum, 10);
    RTIO.PutChar ('\n');
    RTIO.PutChar ('\n');

    (* report an entry for each tree of object types *)
    IF byTypeHierarchy THEN
      root := TYPECODE (ROOT);
      links := FindChildLinks ();
      sums := NEW (R, NUMBER (v.r^));
      SumTrees (sums, v.r, links);
      FOR i := 0 TO LAST (map^) DO map[i] := i; END;
      CASE presentation OF
      | HeapPresentation.ByTypecode  => (*SKIP*)
      | HeapPresentation.ByNumber    => Sort (map, sums, CompareCount)
      | HeapPresentation.ByByteCount => Sort (map, sums, CompareSize)
      END;
      RTIO.PutText ("---- object types (full subtrees) ----\n");
      RTIO.PutText (
	(* 012345678901234567890123456789012345678901234567890 *)
	  "Code   Count   TotalSize  AvgSize  Name\n"
	& "---- --------- --------- --------- --------------------------\n");
      nPrinted := 0;
      FOR i := 0 TO LAST (sums^) DO
	IF (nPrinted >= window) THEN EXIT; END;
	WITH tc = map[i], zz = sums[tc] DO
	  IF (zz.total.count > 0) OR (NOT suppressZeros) THEN
            IF RTType.Supertype (tc) = root THEN
	      PrintTree (sums, links, 0, tc, suppressZeros);
	      RTIO.PutChar ('\n');
	    END;
	    INC(nPrinted);
	  END
	END;
      END;
      RTIO.PutChar ('\n');
    END;

    RTIO.Flush ();
    map := NIL;
  END Report;

PROCEDURE PrintSites (tc : INTEGER;
             READONLY t  : TypeDesc;
                      presentation: HeapPresentation;
                      window: INTEGER) = 
  VAR
    n_sites := NUMBER (t.sites^);
    map := NEW (Map, n_sites);
    site: INTEGER;
    tag: TEXT;
  BEGIN
    FOR k := 0 TO LAST (map^) DO map[k] := k END;
    CASE presentation OF
    | HeapPresentation.ByTypecode, HeapPresentation.ByByteCount =>
           Sort0 (map, t.sites, CompareSize0)
    | HeapPresentation.ByNumber =>
          Sort0 (map, t.sites, CompareCount0)
    END (* CASE *);

    FOR j := 0 TO MIN (n_sites, window)-1 DO
      site := map[j];
      WITH zz = t.sites[site] DO
        IF (zz.count # 0) THEN
          RTIO.PutText("    ");
          RTIO.PutInt(zz.count, 10);
          RTIO.PutInt(zz.size, 10);
          RTIO.PutInt(zz.size DIV zz.count, 10);
          RTIO.PutText("    ");
          FOR k := 0 TO RTAllocStats.siteDepth-1 DO
            tag := RTAllocStats.GetSiteText (tc, site, k);
            IF (tag = NIL) THEN EXIT; END;
            IF (k # 0) THEN
              RTIO.PutText("                    ");
              RTIO.PutText("                  ")
            END;
            RTIO.PutText (tag);
            RTIO.PutChar ('\n');
          END;
        END;
      END;
    END;
    IF (n_sites > 1) AND (window > 1) THEN RTIO.PutChar ('\n'); END;
  END PrintSites;

PROCEDURE FindChildLinks (): LinkList =
  VAR
    n := RTType.MaxTypecode () + 1;
    links := NEW (LinkList, n);
    tc: RT0.Typecode;
  BEGIN
    FOR i := 0 TO n-1 DO
      WITH z = links[i] DO z.children := NO_LINK; z.next := NO_LINK; END;
    END;
    FOR i := 0 TO n-1 DO
      tc := RTType.Supertype (i);
      IF (tc # RTType.NoSuchType) AND (0 <= tc) AND (tc < n) THEN
        WITH parent = links[tc] DO
          links[i].next := parent.children;
          parent.children := i;
        END;
      END;
    END;
    RETURN links;
  END FindChildLinks;

PROCEDURE SumTrees (sums, cnts: R;  links: LinkList) =
  VAR x: INTEGER;
  BEGIN
    FOR i := 0 TO LAST (sums^) DO
      x := links[i].children;
      WHILE (x # NO_LINK) DO
        IF (0 <= x) AND (x <= LAST (cnts^)) THEN
          INC (sums[i].total.count, cnts[x].total.count);
          INC (sums[i].total.size, cnts[x].total.size);
        END;
        x := links[x].next;
      END;
    END;
  END SumTrees;

PROCEDURE PrintTree (sums: R;  links: LinkList;  indent, tc: INTEGER;
                     suppressZeros: BOOLEAN) =
  VAR x: INTEGER;
  BEGIN
    PrintNode (sums, indent, tc, suppressZeros);
    x := links[tc].children;
    WHILE (x # NO_LINK) DO
      PrintTree (sums, links, indent+1, x, suppressZeros);
      x := links[x].next;
    END;
  END PrintTree;

PROCEDURE PrintNode (sums: R;  indent, tc: INTEGER;  suppressZeros: BOOLEAN) =
  BEGIN
    WITH zz = sums[tc] DO
      IF (zz.total.count > 0) OR (NOT suppressZeros) THEN
        RTIO.PutInt (tc, 4);
        RTIO.PutInt (zz.total.count, 10);
        RTIO.PutInt (zz.total.size, 10);
        IF (zz.total.count = 0)
          THEN RTIO.PutText ("         0");
          ELSE RTIO.PutInt  (zz.total.size DIV zz.total.count, 10);
        END;
        RTIO.PutChar (' ');
        WHILE (indent > 0) DO
          RTIO.PutChar (' ');
          RTIO.PutChar (' ');
          DEC (indent);
        END;
        RTIO.PutText (RTTypeSRC.TypecodeName (tc));
        RTIO.PutChar ('\n');
      END;
    END;
  END PrintNode;

PROCEDURE Walk (v    : Visitor;
                tc   : RTType.Typecode;
                ref  : REFANY;
                size : CARDINAL): BOOLEAN =
  VAR n_sites, site: INTEGER;  addr: ADDRESS;  hdr: RTHeapRep.RefHeader;
  BEGIN
    (* total heap *)
    INC (v.countSum);
    INC (v.sizeSum, size);

    WITH zz = v.r[tc] DO
      (* totals for this type *)
      INC (zz.total.count);
      INC (zz.total.size, size);

      (* totals for this type on a per-site basis *)
      n_sites := RTAllocStats.NSites (tc);
      IF (n_sites >= 0) THEN
        IF (zz.sites = NIL) THEN zz.sites := NEW (StatList, n_sites+1); END;
        addr := LOOPHOLE (ref, ADDRESS);
        hdr  := LOOPHOLE (addr - BYTESIZE(RT0.RefHeader), RTHeapRep.RefHeader);
        site := RT0.Spare (hdr);
        INC (zz.sites[site].count);
        INC (zz.sites[site].size, size);
      END;
    END;

    RETURN TRUE
  END Walk;

(*--------------------------------------------------------------- sorting ---*)

PROCEDURE Sort (map: Map;  r: R;  cmp := CompareCount) =
  (* insertion sort such that:  i <= j =>  cmp (r[map[i]], r[map[j]]) <= 0 *)
  VAR n := NUMBER (map^);  j: INTEGER;
  BEGIN
    FOR i := 1 TO n-1 DO
      WITH key = r[map[i]] DO
        j := i-1;
        WHILE (j >= 0) AND cmp (key, r[map[j]]) < 0 DO
          map[j+1] := map[j];
          DEC (j);
        END;
        map[j+1] := i;
      END;
    END;
  END Sort;

PROCEDURE CompareCount (READONLY x, y: TypeDesc): INTEGER =
  BEGIN
    RETURN y.total.count - x.total.count;
  END CompareCount;

PROCEDURE CompareSize (READONLY x, y: TypeDesc): INTEGER =
  BEGIN
    RETURN y.total.size - x.total.size;
  END CompareSize;

PROCEDURE Sort0 (map: Map;  r: StatList;  cmp := CompareCount0) =
  (* insertion sort such that:  i <= j =>  cmp (r[map[i]], r[map[j]]) <= 0 *)
  VAR n := NUMBER (map^);  j: INTEGER;
  BEGIN
    FOR i := 1 TO n-1 DO
      WITH key = r[map[i]] DO
        j := i-1;
        WHILE (j >= 0) AND cmp (key, r[map[j]]) < 0 DO
          map[j+1] := map[j];
          DEC (j);
        END;
        map[j+1] := i;
      END;
    END;
  END Sort0;

PROCEDURE CompareCount0 (READONLY x, y: Stat): INTEGER =
  BEGIN
    RETURN y.count - x.count;
  END CompareCount0;

PROCEDURE CompareSize0 (READONLY x, y: Stat): INTEGER =
  BEGIN
    RETURN y.size - x.size;
  END CompareSize0;

BEGIN 
END RTutils.
