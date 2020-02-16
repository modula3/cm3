(* $Id$ *)

MODULE EntryDB;
IMPORT GridPointEntryTbl, GridPointSet;
IMPORT RectBins;
IMPORT MagPoint, MagLayerRect AS LayerRect;
IMPORT MagRouteLayer AS RouteLayer;
IMPORT GridPoint, MagPointList;
IMPORT RectSet, EndPointStatus;
FROM EndPointStatus IMPORT Dir;
FROM MagicStuff IMPORT WireWidth, GridStep, ViaSize, MetalSpacing;
IMPORT TaggedEntryWays, TaggedEntryWaysList;
IMPORT SimpleGrid, EntryWays, MagRect;
FROM SimpleGrid IMPORT PointType;
IMPORT Conf;
IMPORT Debug;
IMPORT RouteEntries;
IMPORT Env;
IMPORT OSError, Rd, FileRd, Thread, Process, GridPointSetDef;

<* FATAL Conf.LayerNotFound *>

(* debugging options: DoDebug, and env variable DEBUGGPFILE *)
(* search for "BREAK HERE" in this file *)

VAR DoDebug := Debug.DebugThis("EntryDB");
VAR DebugGPFile := Env.Get("DEBUGGPFILE");

REVEAL
  T = Public BRANDED Brand OBJECT
    data : GridPointEntryTbl.T;
    bins : RectBins.T;
  OVERRIDES
    init             := Init;
    get              := GetEntry;
    flushEntries     := FlushEntries;
    entriesOK        := EntriesOK;
    entries        := EntriesOKSet;
    allEntriesOK     := AllEntriesOK;
    someEntryOK      := SomeEntryOK;
    defaultOK        := DefaultOK;
    defaultOKrecomputeAlways        := DefaultOKrecomputeAlways;
    advise := Advise;
  END;

CONST CGP = CheckGridPoint2;

PROCEDURE Init(self : T; bins : RectBins.T) : T = 
  BEGIN 
    self.bins := bins;
    self.data := NEW(GridPointEntryTbl.Default).init();
    RETURN self
  END Init;

VAR gets := 0; gethits := 0; effectiveFlushes := 0;

PROCEDURE GetEntry(self : T; READONLY gp : GridPoint.T;  tag : REFANY;
                   tgtSetArg, otherRectsInSameNetArg : RectSet.T; dir : Dir;
                   VAR way : MagPointList.T; VAR defaultOK : BOOLEAN) : BOOLEAN =

  PROCEDURE InList(VAR res : TaggedEntryWays.T) : BOOLEAN =
    VAR
      wp := waysList;
    BEGIN
      WHILE wp # NIL DO
        IF wp.head.refTag = tag THEN
          res := wp.head; 
          RETURN TRUE 
        END;
        wp := wp.tail
      END;
      RETURN FALSE
    END InList;

  VAR
    waysList : TaggedEntryWaysList.T;
    ways : TaggedEntryWays.T;
    tgtSet := tgtSetArg;
    otherRectsInSameNet := otherRectsInSameNetArg (*.clip(gpr)*);
  BEGIN
    IF DoDebug THEN 
      Debug.Out("EntryDB.GetEntry: gp = " & GridPoint.Format(gp) & " dir = " &
        EndPointStatus.DirName[dir])
    END;

    INC(gets);
    IF self.data.get(gp, waysList) AND InList(ways) THEN 
      INC(gethits);
      way := ways.ways[dir];
      defaultOK := ways.defaultLayoutOK
    ELSE
      ways := TaggedEntryWays.T { tag, tgtSet, otherRectsInSameNet, 
                                  CGP(self, 
                                                 tgtSet, 
                                                 otherRectsInSameNet, 
                                                 gp),
                                  DefaultLayoutOK(self,  
                                                  tgtSet, 
                                                  otherRectsInSameNet, 
                                                  gp) };
      waysList := TaggedEntryWaysList.Cons(ways, waysList);
      EVAL self.data.put(gp, waysList);
      way := ways.ways[dir];
      defaultOK := ways.defaultLayoutOK
    END;
    RETURN way # NIL
  END GetEntry;



PROCEDURE FlushEntries(self : T; READONLY gp : GridPoint.T; radius : [0..1]) =
  VAR 
    dummy : TaggedEntryWaysList.T;
  BEGIN
    EVAL self.data.delete(gp,dummy);
    
    IF radius = 1 THEN
      FOR i := -1 TO 1 DO
        FOR j := -1 TO 1 DO
          VAR
            p := GridPoint.T { gp.x + i, gp.y + j, gp.l };
          BEGIN
            IF DoDebug THEN
              Debug.Out("EntryDB.FlushEntries: flushing " & GridPoint.Format(p))
            END;
            IF self.data.delete(p,dummy) THEN INC(effectiveFlushes) END
          END
        END
      END
    END;
  END FlushEntries;

PROCEDURE ConnectOverlap(READONLY a, b : MagRect.T) : BOOLEAN =
  VAR
    inter : MagRect.T;
  BEGIN
    IF NOT MagRect.Intersection(a,b,inter) THEN RETURN FALSE END;
    
    RETURN inter.ur.x - inter.ll.x >= WireWidth() OR 
           inter.ur.y - inter.ll.y >= WireWidth() 
  END ConnectOverlap;

PROCEDURE DefaultLayoutConnects(routeNetRects : RectSet.T; 
                                READONLY p : GridPoint.T) : BOOLEAN =
  CONST
    ZeroPt    = MagPoint.T { 0, 0 };
  VAR
    ViaRect := MagRect.T { ZeroPt, MagPoint.T { ViaSize(), ViaSize() } };
    rLayer : RouteLayer.T := Conf.LayerLookup(p.l);
    iter := routeNetRects.iterate();
    r : LayerRect.T;
    xbase := p.x * GridStep();
    ybase := p.y * GridStep();
  BEGIN
    (* we ignore DRC problems for now (?) *)
    WHILE iter.next(r) DO
      IF rLayer.connects.member(r.layer) AND 
        ConnectOverlap(MagRect.Shift(r.rect, MagPoint.T { xbase, ybase}), ViaRect) THEN 
        RETURN TRUE
      END
    END;
    RETURN FALSE
  END DefaultLayoutConnects;

(* does the default routing method interfere with existing layout? *)
PROCEDURE DefaultLayoutOK_Old(self : T;                         
                          (* these following are clipped against an area *)
                          (* slightly larger than the GridPoint boundaries.. *)
                          (* the difference is due to maintaining DRC for *)
                          (* all pieces within the gp *)
                          routeNetRects : RectSet.T;
                          
                          (* these should NOT be clipped.. *)
                          otherRects : RectSet.T;  
                          READONLY p : GridPoint.T; ) : BOOLEAN =
  CONST
    ZeroPt    = MagPoint.T { 0, 0 };
  VAR
    (* (* this is what it used to be... *)
    HorizRect := MagRect.T { ZeroPt, MagPoint.T { GridStep(), WireWidth() } };
    VertRect  := MagRect.T { ZeroPt, MagPoint.T { WireWidth(), GridStep() } };
    ViaRect   := MagRect.T { ZeroPt, MagPoint.T { ViaSize(), ViaSize() } };
    *)

    (* i think it's OK to check that the layout is OK up to 3 lambda from
       the edge, because if there's an obstacle in the next gridpoint, that
       should be picked up THERE... *)
    HorizRect := MagRect.T { ZeroPt, MagPoint.T { GridStep() - WireWidth(), WireWidth() } };
    VertRect  := MagRect.T { ZeroPt, MagPoint.T { WireWidth(), GridStep() - WireWidth() } };
    ViaRect   := MagRect.T { ZeroPt, MagPoint.T { ViaSize(), ViaSize() } };

    xbase := p.x * GridStep();
    ybase := p.y * GridStep();
    xtop :=  xbase + GridStep();
    ytop :=  ybase + GridStep();
    interferenceClip := MagRect.T {
      MagPoint.T { xbase - MetalSpacing(), ybase - MetalSpacing() },
      MagPoint.T { xtop  + MetalSpacing(), ytop  + MetalSpacing() } };
    simpleGridRect := MagRect.Shift(interferenceClip, 
                                                 MagPoint.T { xbase, ybase });
    grid := NEW(SimpleGrid.ArrImpl).init(simpleGridRect);

    possRects := self.bins.getOverlappingRectsAllLayers(interferenceClip);

    oR2 := otherRects.intersection(possRects);
    rR2 := routeNetRects.intersection(possRects);

    myRects : RectSet.T := oR2.union(rR2);

    iRectsU : RectSet.T :=
        self.bins.getInterferingRects(LayerRect.T { interferenceClip, 
                                                    Conf.LayerLookup(p.l) }).diff(myRects);

    iRects := iRectsU.clip(interferenceClip);
    iRectsI := iRects.iterate();

    aRect : LayerRect.T;
    verbose := FALSE;
  BEGIN
    IF debugGPS # NIL AND debugGPS.member(p) THEN
      (* BREAK HERE *)
      Debug.Out("EntryDB.DefaultLayoutOK: checking " & GridPoint.Format(p));

      Debug.Out("SimpleGrid rect: " & MagRect.Format(simpleGridRect));

      Debug.Out("\nOverlapping otherRects:");
      DumpRects(otherRects.keepOverlapping(interferenceClip));

      Debug.Out("\nOverlapping routeNetRects:");
      DumpRects(routeNetRects.keepOverlapping(interferenceClip));

      Debug.Out("\npossRects:");
      DumpRects(possRects);

      Debug.Out("\nmyRects:");
      DumpRects(myRects);

      Debug.Out("\niRects (clipped):");
      DumpRects(iRects);

      verbose := TRUE
    END;

    TRY
      IF DoDebug THEN
        Debug.Out("EntryDB.DefaultLayoutOK: checking " & GridPoint.Format(p))
      END;
      
      WHILE iRectsI.next(aRect) DO
        VAR 
          rect := MagRect.Shift(aRect.rect, MagPoint.T { xbase, ybase });
        BEGIN

          grid.drawRect(PointType.Obstacle,rect);

          IF verbose THEN
            Debug.Out("Drawing iRect: " & MagRect.Format(rect)& "\n" & 
              grid.format())
          END
        END
      END;

      IF DoDebug OR verbose THEN
        Debug.Out("EntryDB.CheckDefaultLayoutOK: " & GridPoint.Format(p) & 
          " before bloating and shrinking grid is:\n" & grid.format())
      END;

      grid.bloat(PointType.Obstacle, 
                 SimpleGrid.Dir.N, MetalSpacing());
      grid.bloat(PointType.Obstacle, 
                 SimpleGrid.Dir.E, MetalSpacing());
      
      grid.bloat(PointType.Obstacle, 
                 SimpleGrid.Dir.W, MetalSpacing());
      grid.bloat(PointType.Obstacle, 
                 SimpleGrid.Dir.S, MetalSpacing());
      
      IF DoDebug OR verbose THEN
        Debug.Out("EntryDB.CheckDefaultLayoutOK: " & GridPoint.Format(p) & 
          " after bloating obstacles grid is:\n" & grid.format())
      END;

      IF grid.touches(PointType.Obstacle, HorizRect) OR
        grid.touches(PointType.Obstacle, VertRect)  OR
        grid.touches(PointType.Obstacle, ViaRect) THEN
        RETURN FALSE
      ELSE
        IF DoDebug OR verbose THEN
          Debug.Out("EntryDB.DefaultLayoutOK: marking as DefaultOK: " & 
            GridPoint.Format(p));
        END;
        RETURN TRUE
      END
    FINALLY
      SimpleGrid.Recycle(grid);
      IF verbose THEN
        Debug.Out("DefaultLayoutOK("&GridPoint.Format(p)& (* BREAK HERE *)
          "), verbose, DONE.") 
      END
    END
  END DefaultLayoutOK_Old;

(* does the default routing method interfere with existing layout? *)
PROCEDURE DefaultLayoutOK(self : T;                         
                          (* these following are clipped against an area *)
                          (* slightly larger than the GridPoint boundaries.. *)
                          (* the difference is due to maintaining DRC for *)
                          (* all pieces within the gp *)
                          routeNetRects : RectSet.T;
                          
                          (* these should NOT be clipped.. *)
                          otherRects : RectSet.T;  
                          READONLY p : GridPoint.T; ) : BOOLEAN =

  PROCEDURE Mapper(READONLY q, aRect : LayerRect.T; args : REFANY) =
    BEGIN
      IF NOT otherRects.member(aRect) AND NOT routeNetRects.member(aRect) AND 
        MagRect.Intersection(aRect.rect, interferenceClip, xrect) THEN
        VAR 
          rect := MagRect.Shift(xrect, MagPoint.T { xbase, ybase });
        BEGIN
          
          grid.drawRect(PointType.Obstacle,rect);
          
          IF verbose THEN
            Debug.Out("Drawing iRect: " & MagRect.Format(rect)& "\n" & 
              grid.format())
          END
        END
      END
    END Mapper;

  CONST
    ZeroPt    = MagPoint.T { 0, 0 };
  VAR
    (* i think it's OK to check that the layout is OK up to 3 lambda from
       the edge, because if there's an obstacle in the next gridpoint, that
       should be picked up THERE... *)
    HorizRect := MagRect.T { ZeroPt, MagPoint.T { GridStep() - WireWidth(), WireWidth() } };
    VertRect  := MagRect.T { ZeroPt, MagPoint.T { WireWidth(), GridStep() - WireWidth() } };
    ViaRect   := MagRect.T { ZeroPt, MagPoint.T { ViaSize(), ViaSize() } };

    xbase := p.x * GridStep();
    ybase := p.y * GridStep();
    xtop :=  xbase + GridStep();
    ytop :=  ybase + GridStep();
    interferenceClip := MagRect.T {
      MagPoint.T { xbase - MetalSpacing(), ybase - MetalSpacing() },
      MagPoint.T { xtop  + MetalSpacing(), ytop  + MetalSpacing() } };
    simpleGridRect := MagRect.Shift(interferenceClip, 
                                                 MagPoint.T { xbase, ybase });
    grid := NEW(SimpleGrid.ArrImpl).init(simpleGridRect);

(*
    iter := self.bins.getInterferingRects(LayerRect.T { interferenceClip, 
                                                    Conf.LayerLookup(p.l) }).iterate();
*)

    aRect : LayerRect.T;
    verbose := FALSE;
    xrect : MagRect.T;
  BEGIN
    TRY
      IF DoDebug THEN
        Debug.Out("EntryDB.DefaultLayoutOK: checking " & GridPoint.Format(p))
      END;
      
      self.bins.mapInterferingRects(LayerRect.T { interferenceClip, 
                                                    Conf.LayerLookup(p.l) },
                                    Mapper, NIL);
      
(*
      WHILE iter.next(aRect) DO
        IF NOT otherRects.member(aRect) AND NOT routeNetRects.member(aRect) AND 
         MagRect.Intersection(aRect.rect, interferenceClip, xrect) THEN
          VAR 
            rect := MagRect.Shift(xrect, MagPoint.T { xbase, ybase });
          BEGIN
            
            grid.drawRect(PointType.Obstacle,rect);
            
            IF verbose THEN
              Debug.Out("Drawing iRect: " & MagRect.Format(rect)& "\n" & 
                grid.format())
            END
          END
        END
      END;
*)

      IF DoDebug OR verbose THEN
        Debug.Out("EntryDB.CheckDefaultLayoutOK: " & GridPoint.Format(p) & 
          " before bloating and shrinking grid is:\n" & grid.format())
      END;

      grid.bloat(PointType.Obstacle, 
                 SimpleGrid.Dir.N, MetalSpacing());
      grid.bloat(PointType.Obstacle, 
                 SimpleGrid.Dir.E, MetalSpacing());
      
      grid.bloat(PointType.Obstacle, 
                 SimpleGrid.Dir.W, MetalSpacing());
      grid.bloat(PointType.Obstacle, 
                 SimpleGrid.Dir.S, MetalSpacing());
      
      IF DoDebug OR verbose THEN
        Debug.Out("EntryDB.CheckDefaultLayoutOK: " & GridPoint.Format(p) & 
          " after bloating obstacles grid is:\n" & grid.format())
      END;

      IF grid.touches(PointType.Obstacle, HorizRect) OR
        grid.touches(PointType.Obstacle, VertRect)  OR
        grid.touches(PointType.Obstacle, ViaRect) THEN
        RETURN FALSE
      ELSE
        IF DoDebug OR verbose THEN
          Debug.Out("EntryDB.DefaultLayoutOK: marking as DefaultOK: " & 
            GridPoint.Format(p));
        END;
        RETURN TRUE
      END
    FINALLY
      SimpleGrid.Recycle(grid);
      IF verbose THEN
        Debug.Out("DefaultLayoutOK("&GridPoint.Format(p)& (* BREAK HERE *)
          "), verbose, DONE.") 
      END
    END
  END DefaultLayoutOK;

PROCEDURE DumpRects(s : RectSet.T) =
  VAR
    iter := s.iterate();
    lr : LayerRect.T;
  BEGIN
    WHILE iter.next(lr) DO
      Debug.Out(MagRect.Format(lr.rect) & " on " & 
        NARROW(lr.layer,RouteLayer.T).name)
    END
  END DumpRects;

PROCEDURE FmtMagPtList(l : MagPointList.T) : TEXT =
  VAR
    res := "";
  BEGIN
    IF l = NIL THEN RETURN "NIL" END;

    WHILE l # NIL DO
      res := res & MagPoint.Format(l.head);
      IF l.tail # NIL THEN res := res & ", " END;
      l := l.tail;
    END;
    RETURN res
  END FmtMagPtList;

PROCEDURE AllEntriesOK(self : T; READONLY gp : GridPoint.T;  tag : REFANY;
                 tgtSet, otherRectsInSameNet : RectSet.T) : BOOLEAN =
  VAR 
    dummy : MagPointList.T;
    dBool : BOOLEAN;
  BEGIN
    FOR i := FIRST(Dir) TO LAST(Dir) DO
      IF NOT self.get(gp, tag, tgtSet, otherRectsInSameNet, i, dummy,dBool) THEN
        RETURN FALSE
      END
    END;
    RETURN TRUE
  END AllEntriesOK;


PROCEDURE SomeEntryOK(self : T; READONLY gp : GridPoint.T;  tag : REFANY;
                 tgtSet, otherRectsInSameNet : RectSet.T) : BOOLEAN =
  VAR 
    dummy : MagPointList.T;
    dBool : BOOLEAN;
  BEGIN
    FOR i := FIRST(Dir) TO LAST(Dir) DO
      IF self.get(gp, tag, tgtSet, otherRectsInSameNet, i, dummy,dBool) THEN
        RETURN TRUE
      END
    END;
    RETURN FALSE
  END SomeEntryOK;

PROCEDURE EntriesOK(self : T; READONLY gp : GridPoint.T;  tag : REFANY;
                    tgtSet, 
                    otherRectsInSameNet : RectSet.T) : ARRAY Dir OF BOOLEAN =
  VAR 
    dummy : MagPointList.T;
    dBool : BOOLEAN;
    res : ARRAY Dir OF BOOLEAN;
  BEGIN
    FOR i := FIRST(Dir) TO LAST(Dir) DO
      res[i] := self.get(gp, tag, tgtSet, otherRectsInSameNet, i, dummy, dBool) 
    END;
    RETURN res
  END EntriesOK;

PROCEDURE EntriesOKSet(self : T; READONLY gp : GridPoint.T;  tag : REFANY;
                    tgtSet, 
                    otherRectsInSameNet : RectSet.T) : RouteEntries.T =
  VAR 
    dummy : MagPointList.T;
    dBool : BOOLEAN;
    res := RouteEntries.T {} ;
  BEGIN
    FOR i := FIRST(Dir) TO LAST(Dir) DO
      IF self.get(gp, tag, tgtSet, otherRectsInSameNet, i, dummy, dBool) THEN
        res := res + RouteEntries.T { i }
      END
    END;
    RETURN res
  END EntriesOKSet;

PROCEDURE DefaultOK(self : T; READONLY gp : GridPoint.T;  tag : REFANY;
                 tgtSet, otherRectsInSameNet : RectSet.T) : BOOLEAN =
  VAR
    dummy : MagPointList.T;
    dBool : BOOLEAN;
  BEGIN
    EVAL self.get(gp, tag, tgtSet, otherRectsInSameNet, FIRST(Dir), dummy, dBool);
    RETURN dBool
  END DefaultOK;

PROCEDURE Advise(t : T; s : GridPointSet.T) = 
  VAR
    p : GridPoint.T;
    iter := s.iterate();
    r := NEW(RectSet.T).init();
  BEGIN
    WHILE iter.next(p) DO
      VAR
        rLayer : RouteLayer.T := Conf.LayerLookup(p.l);
        
        xbase := p.x * GridStep();
        ybase := p.y * GridStep();
        
        (* make this a bit conservative for interfering rects *)
        clip := MagRect.T { MagPoint.T { xbase -  GridStep() - 1 , 
                                         ybase -  GridStep() - 1 }, 
                            MagPoint.T { xbase + 2 * GridStep() + 1, 
                                         ybase + 2 * GridStep() + 1 }};
      BEGIN
        EVAL r.insert(LayerRect.T { clip, rLayer } )
      END
    END;
    t.bins.advise(r)
  END Advise;

PROCEDURE DefaultOKrecomputeAlways(self : T; READONLY gp : GridPoint.T;  
                 tgtSet, otherRectsInSameNet : RectSet.T) : BOOLEAN =
  BEGIN
    RETURN DefaultLayoutOK(self, tgtSet, otherRectsInSameNet, gp)
  END DefaultOKrecomputeAlways;

(* check the different ways of entering/leaving the cell *)
(* here we ignore existing rects (except the targets---existing before
   ANY routing was done) and instead assume that we have
   obstacles EVERYWHERE it is permitted by the design rules... *)
PROCEDURE CheckGridPoint2(self : T; 

                         (* these following are clipped against an area *)
                         (* slightly larger than the GridPoint boundaries.. *)
                         (* the difference is due to maintaining DRC for *)
                         (* all pieces within the gp *)
                         routeNetRects : RectSet.T;

                         otherRects : RectSet.T;  

                         p : GridPoint.T) : EntryWays.T =
  VAR
    rLayer : RouteLayer.T := Conf.LayerLookup(p.l);

    res : EntryWays.T; 
    xbase := p.x * GridStep();
    ybase := p.y * GridStep();
    
    (* make this a bit conservative for interfering rects *)
    clip := MagRect.T { MagPoint.T { xbase -  GridStep() - 1 , 
                                     ybase -  GridStep() - 1 }, 
                        MagPoint.T { xbase + 2 * GridStep() + 1, 
                                     ybase + 2 * GridStep() + 1 }};

    (* must remember that the SimpleGrid clips in its own coordinates.. *)
    (* this is not really necessary, though. *)
    grid := NEW(SimpleGrid.ArrImpl).init(MagRect.Shift(clip, 
                                                 MagPoint.T { xbase, ybase }),
                                         initType := PointType.Obstacle
    );

    allRectsClip := self.bins.getInterferingRects(LayerRect.T { clip, rLayer }).clip(clip);
    (* the rects I want *)
    routeNetRectsClip := routeNetRects.clip(clip);
    otherRectsClip := otherRects.clip(clip);

    myRectsI := routeNetRectsClip.iterate();

    aRect : LayerRect.T;
  BEGIN
    TRY
    IF DoDebug THEN
      Debug.Out("EntryDB.CheckGridPoint2: checking " & GridPoint.Format(p));
      Debug.Out("EntryDB.CheckGridPoint2: clipping against " & 
        MagRect.Format(clip));
      VAR
        myI := routeNetRectsClip.iterate();
      BEGIN
        Debug.Out("EntryDB.CheckGridPoint2: my rects:");
        WHILE myI.next(aRect) DO
          Debug.Out("EntryDB.CheckGridPoint2: " & LayerRect.Format(aRect))
        END
      END
    END;

    (* draw in the rects *)
    WHILE myRectsI.next(aRect) DO 
      IF rLayer.connects.member(aRect.layer) THEN
        VAR 
          rect := MagRect.Shift(aRect.rect, MagPoint.T { xbase, ybase });
        BEGIN
          grid.drawRect(PointType.Target,rect)
        END
      END
    END;

    (* set up bloating and shrinking for the rects *)
    
    IF DoDebug THEN
      Debug.Out("EntryDB.CheckGridPoint2: " & GridPoint.Format(p) & 
        " before bloating and shrinking grid is:\n" & grid.format())
    END;


    (* copy the grid, 1 for each connect dir. *)
    FOR i := FIRST(Dir) TO LAST(Dir) DO 
      VAR
        StartPts := ARRAY Dir OF MagPoint.T { 
        MagPoint.T { WireWidth() - 1, GridStep() },
        MagPoint.T { GridStep()+2, 0 },
        MagPoint.T { +1, 0 },
        MagPoint.T { WireWidth() - 1, -1 },
        
        (* hmm.. the vertical entries aren't entirely satisfying.. *)
        MagPoint.T { WireWidth() - 1, WireWidth() - 1 },
        MagPoint.T { WireWidth() - 1, WireWidth() - 1 } };

        testGrid := grid.copy();
        startPt := StartPts[i];
      BEGIN
        TRY
        (* draw in hypothetical layout in neighboring cell;
           this is the layout that must exist in that cell in order to
           make the entering link *)
        IF DoDebug THEN
          Debug.Out("EntryDB.CheckGridPoint2: " & GridPoint.Format(p) & 
            " testGrid before adding neighbor points' rects is (entry from "&
            EndPointStatus.DirName[i] &"):\n" & 
            testGrid.format())
        END;

        CASE i OF
          Dir.N =>
            testGrid.drawRect(PointType.Target, 
               MagRect.T { MagPoint.T { 0, GridStep() },
                           MagPoint.T { ViaSize(), GridStep() + ViaSize() }})
        |
          Dir.E =>
            (* is this permissible??? it spills one lambda into THIS cell! *)
            testGrid.drawRect(PointType.Target, 
               MagRect.T { MagPoint.T { GridStep() - 1, 0 },
                           MagPoint.T { GridStep() + ViaSize(), ViaSize() }})
        | 
          Dir.W =>
            testGrid.drawRect(PointType.Target, 
               MagRect.T { MagPoint.T { -GridStep(), 0 },
                           MagPoint.T { -GridStep() + ViaSize(), ViaSize() }});
            testGrid.drawRect(PointType.Target, 
               MagRect.T { MagPoint.T { -GridStep(), 0 },
                           MagPoint.T { 0, WireWidth() }})
        |
          Dir.S =>
            testGrid.drawRect(PointType.Target, 
               MagRect.T { MagPoint.T { 0, -GridStep() },
                           MagPoint.T { WireWidth(), 0 }})
        | 
          Dir.U, Dir.D => (* is this right? *)
            testGrid.drawRect(PointType.Target,
               MagRect.T { MagPoint.T { 0, 0 }, 
                           MagPoint.T { ViaSize(), ViaSize() }})
        END;
        IF DoDebug THEN
          Debug.Out("EntryDB.CheckGridPoint2: " & GridPoint.Format(p) & 
            " testGrid after adding neighbor points' rects is:\n" & 
            testGrid.format())
        END;

        (* bloat "my" stuff NEWS to push away any obstacles *)
        testGrid.bloat(PointType.Target, SimpleGrid.Dir.N, MetalSpacing());
        testGrid.bloat(PointType.Target, SimpleGrid.Dir.E, MetalSpacing());
        testGrid.bloat(PointType.Target, SimpleGrid.Dir.W, MetalSpacing());
        testGrid.bloat(PointType.Target, SimpleGrid.Dir.S, MetalSpacing());

        IF DoDebug THEN
          Debug.Out("EntryDB.CheckGridPoint2: " & GridPoint.Format(p) & 
            " testGrid after bloating my stuff is:\n" & 
            testGrid.format())
        END;

        (* shrink obstacles SW by wirewidth to get rid of any non-permissible
           bridges *)
        testGrid.shrink(PointType.Obstacle, SimpleGrid.Dir.S, WireWidth());
        testGrid.shrink(PointType.Obstacle, SimpleGrid.Dir.W, WireWidth());

        IF DoDebug THEN
          Debug.Out("EntryDB.CheckGridPoint2: " & GridPoint.Format(p) & 
            " testGrid after shrinking potential obstacles is:\n" & 
            testGrid.format())
        END;

        (* bloat obstacles SW again to get them back *)
        testGrid.bloat(PointType.Obstacle, SimpleGrid.Dir.S, WireWidth());
        testGrid.bloat(PointType.Obstacle, SimpleGrid.Dir.W, WireWidth());

        IF DoDebug THEN
          Debug.Out("EntryDB.CheckGridPoint2: " & GridPoint.Format(p) & 
            " testGrid after bloating potential obstacles is:\n" & 
            testGrid.format())
        END;


        (* now we have all permissible obstacles, forget the bloated targets *)
        testGrid.zap(PointType.Target, PointType.Space);

        (* draw in the ACTUAL obstacles *)
        VAR
          obsRectsI := allRectsClip.iterate();
        BEGIN
          WHILE obsRectsI.next(aRect) DO 
            IF rLayer.connects.member(aRect.layer) THEN
              VAR 
                rect := MagRect.Shift(aRect.rect, MagPoint.T { xbase, ybase });
              BEGIN
                testGrid.drawRect(PointType.Obstacle,rect)
              END
            END
          END
        END;

        IF DoDebug THEN
          Debug.Out("EntryDB.CheckGridPoint2: " & GridPoint.Format(p) & 
            " testGrid with REAL obstacles is:\n" & 
            testGrid.format())
        END;

        VAR
          otherRectsI := otherRectsClip.iterate();
        BEGIN
          WHILE otherRectsI.next(aRect) DO 
            IF rLayer.connects.member(aRect.layer) THEN
              VAR 
                rect := MagRect.Shift(aRect.rect, MagPoint.T { xbase, ybase });
              BEGIN
                testGrid.drawRect(PointType.Space,rect)
              END
            END
          END
        END;

        (* re-draw my targets *)
        testGrid.copyPointsFrom(grid,SimpleGrid.TypeSet { PointType.Target } );
        
        IF DoDebug THEN
          Debug.Out("EntryDB.CheckGridPoint2: " & GridPoint.Format(p) & 
            " testGrid after redrawing my rects is:\n" & 
            testGrid.format())
        END;


        (* set up bloating and shrinking for the rects *)
        
        IF DoDebug THEN
          Debug.Out("EntryDB.CheckGridPoint: " & GridPoint.Format(p) & 
            " before bloating and shrinking grid is:\n" & testGrid.format())
        END;
        
        testGrid.shrink(PointType.Target, SimpleGrid.Dir.W, WireWidth() - 1);
        testGrid.shrink(PointType.Target, SimpleGrid.Dir.N, WireWidth() - 1);
        
        IF DoDebug THEN
          Debug.Out("EntryDB.CheckGridPoint: " & GridPoint.Format(p) & 
            " after shrinking target grid is:\n" & testGrid.format())
        END;
        (* and now bloat the obstacles, real and imagined ... *)

        testGrid.bloat(PointType.Obstacle, 
                   SimpleGrid.Dir.N, MetalSpacing());
        testGrid.bloat(PointType.Obstacle, 
                   SimpleGrid.Dir.E, MetalSpacing() + WireWidth() - 1);
        testGrid.bloat(PointType.Obstacle, 
                   SimpleGrid.Dir.W, MetalSpacing());
        testGrid.bloat(PointType.Obstacle, 
                   SimpleGrid.Dir.S, MetalSpacing() + WireWidth() - 1);


        IF DoDebug THEN
          Debug.Out("EntryDB.CheckGridPoint2: " & GridPoint.Format(p) & 
            " testGrid ready for routing is:\n" & 
            testGrid.format())
        END;


        (* and try to route! *)
        IF NOT testGrid.route(startPt, 
                              MagPoint.T {-WireWidth(),
                                           -WireWidth()  },
                              MagPoint.T { GridStep() + 1, GridStep() - 1 },
                              PointType.Target,
                              res[i]) THEN
          res[i] := NIL 
        END;

        IF DoDebug THEN
          Debug.Out("EntryDB.CheckGridPoint2: path entering from \"" & 
            EndPointStatus.DirName[i] & "\": "& FmtMagPtList(res[i]));
        END
      FINALLY
        SimpleGrid.Recycle(testGrid)
      END
      END
        
      END; (* FOR *)

    (**********************************************************************)
    (**********************************************************************)
    (**********************************************************************)
    (**********************************************************************)
    (**********************************************************************)

    (* special checks for vertical entries *)

    (* HOW DO WE DO THIS WITH THE NEW STYLE OF CHECKING??? *)
    (* USE THE OLD METHOD?? *)

    (* we will need to pick up the existing other rects for that *)

    SimpleGrid.Recycle(grid);
    grid := NEW(SimpleGrid.ArrImpl).init(MagRect.Shift(clip, 
                                                 MagPoint.T { xbase, ybase }));

    VAR
      (* the rects I want *)
      myRectsI := routeNetRectsClip.iterate();

      (* irrelevant rects *)
      otherRectsI := otherRectsClip.iterate();
      (* the obstacles *)
      (* note that other rects in the same net are not to be thought of *)
      (* as obstacles.. *)
      obsRectsI := allRectsClip.iterate();
    BEGIN

    IF DoDebug THEN
      Debug.Out("EntryDB.CheckGridPoint: checking " & GridPoint.Format(p));
      Debug.Out("EntryDB.CheckGridPoint: clipping against " & 
        MagRect.Format(clip));
      VAR
        obI := allRectsClip.iterate();
        myI := routeNetRectsClip.iterate();
        otI := otherRectsClip.iterate();
      BEGIN
        Debug.Out("EntryDB.CheckGridPoint: obstacles:");
        WHILE obI.next(aRect) DO
          Debug.Out("EntryDB.CheckGridPoint: " & LayerRect.Format(aRect))
        END;
        Debug.Out("EntryDB.CheckGridPoint: my rects:");
        WHILE myI.next(aRect) DO
          Debug.Out("EntryDB.CheckGridPoint: " & LayerRect.Format(aRect))
        END;
        Debug.Out("EntryDB.CheckGridPoint: other rects:");
        WHILE otI.next(aRect) DO
          Debug.Out("EntryDB.CheckGridPoint: " & LayerRect.Format(aRect))
        END;
      END
    END;

    (* draw in the rects *)
    WHILE obsRectsI.next(aRect) DO 
      IF rLayer.connects.member(aRect.layer) THEN
        VAR 
          rect := MagRect.Shift(aRect.rect, MagPoint.T { xbase, ybase });
        BEGIN
          grid.drawRect(PointType.Obstacle,rect)
        END
      END
    END;

    WHILE otherRectsI.next(aRect) DO 
      IF rLayer.connects.member(aRect.layer) THEN
        VAR 
          rect := MagRect.Shift(aRect.rect, MagPoint.T { xbase, ybase });
        BEGIN
          grid.drawRect(PointType.Space,rect)
        END
      END
    END;

    WHILE myRectsI.next(aRect) DO 
      IF rLayer.connects.member(aRect.layer) THEN
        VAR 
          rect := MagRect.Shift(aRect.rect, MagPoint.T { xbase, ybase });
        BEGIN
          grid.drawRect(PointType.Target,rect)
        END
      END
    END;

    (* set up bloating and shrinking for the rects *)
    
    IF DoDebug THEN
      Debug.Out("EntryDB.CheckGridPoint: " & GridPoint.Format(p) & 
        " before bloating and shrinking grid is:\n" & grid.format())
    END;

    IF res[Dir.U] # NIL OR res[Dir.D] # NIL THEN
      VAR

        ViaRect := MagRect.T { MagPoint.T { -WireWidth() , 
                                            -WireWidth()},
                              MagPoint.T { ViaSize() + WireWidth(), 
                                           ViaSize() + WireWidth() } };


      BEGIN
        IF DoDebug THEN Debug.Out(  grid.format()) END;

        IF grid.touches(PointType.Obstacle, ViaRect) THEN

          IF DoDebug THEN
            Debug.Out("EntryDB.CheckGridPoint2: NIL-ing up and down entries into " & GridPoint.Format(p) 
            & " because of touching ViaRect...");
            Debug.Out("EntryDB.CheckGridPoint2: grid is:\n" & grid.format())
          END;

          res[Dir.U] := NIL;
          res[Dir.D] := NIL
        END
      END
    END
    END;

    RETURN res
  FINALLY
    SimpleGrid.Recycle(grid)
  END
  END CheckGridPoint2;

VAR
  debugGPS : GridPointSet.T := NIL;
BEGIN 
  IF DebugGPFile # NIL THEN
    TRY
      VAR
        rd := FileRd.Open(DebugGPFile);
      BEGIN
        TRY
          debugGPS := NEW(GridPointSetDef.T).init();
          
          LOOP
            VAR
              line := Rd.GetLine(rd);
            BEGIN
              EVAL debugGPS.insert(GridPoint.Parse(line))
            END
          END
        EXCEPT
          Rd.EndOfFile => Rd.Close(rd)
        END
      END
    EXCEPT
      Rd.Failure, GridPoint.ParseError, OSError.E, Thread.Alerted =>
        Process.Crash("EntryDB: problems reading debug file \"" &
          DebugGPFile & "\"")
    END
  END

END EntryDB.
