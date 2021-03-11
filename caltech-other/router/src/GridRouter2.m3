(* $Id$ *)

MODULE GridRouter2;
IMPORT MagCell, MagCellExtendable;
IMPORT TextMagLayerTbl AS TextLayerTbl, MagRouter;
IMPORT GridPointCompsTbl, RouteComponents, Components;
IMPORT LabelListSet, Route, MagLabelList AS LabelList, GridPoint, LongrealPQ;
IMPORT ComponentsSet, ComponentsSetDef;
IMPORT MagSubCell, MagTransform;
IMPORT EntryDB, RectBins;
IMPORT MagLayerRect AS LayerRect, Conf, IntPair, Bins, Filled, Debug;
FROM MagicStuff IMPORT GridStep, MetalSpacing;
IMPORT MagRect;
IMPORT Wr, OSError;
IMPORT Process;
IMPORT RouteID, RouteIDCompsTbl;
IMPORT Thread;
IMPORT GridPointSet, GridPointSetDef;
IMPORT ForbiddenSteps;
IMPORT GridPointRouteIDTbl;
IMPORT GridPointList;
IMPORT Env, RTRefStats;
IMPORT RouterDeferralList, RouterDeferral;
IMPORT Random, Fmt;
IMPORT RouterDeferralClass;

FROM EndPointStatus IMPORT Dir;
IMPORT MagicStuff;
IMPORT GridPointCube;

(**********************************************************************)
(**********************************************************************)
(**********************                           *********************)
(**********************  various debugging flags  *********************)
(**********************                           *********************)
(**********************************************************************)
(**********************************************************************)

VAR DoDebug := Debug.DebugThis("GridRouter2");
(* turn on verbose debugging *)

VAR DoMemReporting := Env.Get("DEBUGROUTERMEMORY") # NIL;
(* turn on memory reporting *)

VAR KeepDeferralRoutes := Env.Get("KEEPDEFERRALROUTES") # NIL;
(* keep deferral dummy routes in the output *)

(**********************************************************************)
(**********************************************************************)
(**********************                           *********************)
(**********************                           *********************)
(**********************                           *********************)
(**********************************************************************)
(**********************************************************************)

CONST MultiplierMultiplier = 1.5d0;
CONST DefaultMultiplier = 10.0d0;

EXCEPTION RoutingEmpty;

REVEAL 
  T = Public BRANDED Brand OBJECT
    layout, wiringCell, deferralCell : MagCell.T;
    layerDB : TextLayerTbl.T;
    resultMethod : ResultMethod;
    comps : GridPointCompsTbl.T;
    pq : LongrealPQ.T;
    entryDB : EntryDB.T;
    binnedLayout : RectBins.T;
    binsIter : Bins.Iterator := NIL; (* protected by locking mu *)
    mu : MUTEX;
    compsById : RouteIDCompsTbl.T;
    specialTbl : GridPointRouteIDTbl.T;
    delWr : Wr.T := NIL;

    deferrals : RouterDeferralList.T := NIL;
    defSubCellName : TEXT;
    keepInternalDeferredWiring : BOOLEAN;
  METHODS

    (* route the longest link in the MST associated with toRoute;
       return the Route.T data structure describing the newly constructed
       route *)
    routeIt(toRoute : MyComps; nonObs, nonEndPointsInNetForCurrentRoute : GridPointSet.T;
            forbiddenSteps : ForbiddenSteps.T;
            origBBox : GridPointCube.T;
            respectBBox : BOOLEAN) : Route.T RAISES {Route.NotFound, RoutingEmpty} := RouteIt;
    
    (* perform ripups associated with a newly-routed route;
       returns affected nets *)
    doRipups(rte : MyRoute) : ComponentsSet.T RAISES { Wr.Failure, Thread.Alerted } := DoRipups;

    (* reschedule nets affected by ripping-up *)
    rescheduleRipups(toReschedule : ComponentsSet.T) := RescheduleRipups;
  OVERRIDES
    initDelWr := InitDelWr;
    initResultMethod := InitResultMethod;
    init := Init;
    run := Run;
    setDeferrals := SetDeferrals;
  END;

TYPE
  MyComps = RouteComponents.Default OBJECT
    mult := DefaultMultiplier; (* multiplier *)
    labels : LabelList.T; (* just to keep track of failed routes *)
    deferred : BOOLEAN;
    deferral : RouterDeferral.T;
  END;

  MyRoute = Route.Default OBJECT
    rtr : T;
    comps : RouteComponents.T;
  OVERRIDES
    ripUpCost := RouteRipUpCost;
    myID := RouteMyID;
  END;

PROCEDURE RouteMyID(r : MyRoute) : RouteID.T =
  BEGIN RETURN r.comps.id() END RouteMyID;

PROCEDURE RouteRipUpCost(m : MyRoute; r : RouteID.T) : LONGREAL =
  VAR
    c : RouteComponents.T;
    x : BOOLEAN;
  BEGIN 
    x := m.rtr.compsById.get(r,c);
    <* ASSERT x *>
    
    RETURN c.approxSize() * NARROW(c,MyComps).mult 
  END RouteRipUpCost;

PROCEDURE InitDelWr(t : T; delWr: Wr.T) : T =
  BEGIN t.delWr := delWr; RETURN t END InitDelWr;

PROCEDURE InitResultMethod(self : T; 
                           resultMethod : ResultMethod) : T =
  BEGIN
    self.resultMethod := resultMethod;
    RETURN self
  END InitResultMethod;

PROCEDURE Init(self : T; 
               layout       : MagCell.T;
               layerDBArg   : TextLayerTbl.T) : MagRouter.T =
  BEGIN
    self.mu := NEW(MUTEX);
    self.compsById := NEW(RouteIDCompsTbl.Default).init();
    self.layout := layout;
    self.layerDB := layerDBArg;
    RETURN self
  END Init;


VAR
  rMu := NEW(MUTEX);
  rand := NEW(Random.Default).init();

PROCEDURE ToGridPointCube(READONLY rect : MagRect.T) : GridPointCube.T =
  VAR
    Step := GridStep();
    res : GridPointCube.T;
  BEGIN
    res.ll.x := rect.ll.x DIV Step;
    res.ll.y := rect.ll.y DIV Step;
    res.ll.l := FIRST(GridPoint.Layer);
    res.ur.x := rect.ur.x DIV Step;
    res.ur.y := rect.ur.y DIV Step;
    res.ur.l := LAST(GridPoint.Layer);
    RETURN res
  END ToGridPointCube;

PROCEDURE RandomText() : TEXT =
(* return a random text consisting of a leading alpha and the rest hex-numerics *)
  VAR
    res := "R";
  BEGIN
    LOCK rMu DO
      FOR i := 0 TO 3 DO
        res := res & Fmt.Pad(
                         Fmt.Int(rand.integer(min:=0,max:=16_ffff),base:=16),
                         4,
                         padChar := '0')
      END;
      RETURN res
    END
  END RandomText;

PROCEDURE RouteIt(t : T; toRoute : MyComps; 
                  nonObs, nonEndPointsInNetForCurrentRoute : GridPointSet.T;
                  forbiddenSteps : ForbiddenSteps.T;
                  origBBox : GridPointCube.T;
                  respectBBox : BOOLEAN) : Route.T RAISES { Route.NotFound, RoutingEmpty } =
  VAR
    comps := toRoute.longest();
    rte := NEW(MyRoute, rtr := t, comps := toRoute);
  BEGIN
    (* set targets as unfilled so the router can route to them
       also set other points in net as unfilled so router can route
       through them if necessary *)
    
    IF comps.c1.size() = 0 OR comps.c2.size() = 0 THEN
      (* nothing to do *)
      RAISE RoutingEmpty
    END;
    rte.computeRouteBetweenSets(comps.c1, comps.c2, 
                  multiplier := FLOAT(toRoute.mult),
                  nonObstacles := nonObs,
                  forbiddenSteps := forbiddenSteps,
                  keepOuts := NEW(KeepOutSet, specialTbl := t.specialTbl, curId := rte.myID()),
                  pointsThatCostZero := nonEndPointsInNetForCurrentRoute,
                  BBox := origBBox,
                  respectBBox := respectBBox
                  );
    <* ASSERT rte.ripUpStuff() # NIL *>

    RETURN rte
  END RouteIt;

TYPE
  CompsPQElt = LongrealPQ.Elt OBJECT
    comps : MyComps
  END;

CONST DefSpacingFromLayout = 10 (* gridpoints *);

PROCEDURE Run(self : T; labelsSet, failedNodes : LabelListSet.T;
              respectBBox : BOOLEAN
	      ) =
  <* FATAL LongrealPQ.Empty *>
  <* FATAL Thread.Alerted *>
  <* FATAL MagCellExtendable.NoSuchSubcell *>
  VAR
    failed := NEW(ComponentsSetDef.T).init();
    goes := 0;
    origBBox := self.layout.getBBox();
  BEGIN
    (* make wiring cell, if applicable *)
    CASE self.resultMethod OF 
    | ResultMethod.UseSubCell =>
      VAR 
        sub : MagSubCell.T;
      BEGIN
        IF NOT self.layout.getSubCell(WiringCellId, sub) THEN
          sub := MagSubCell.T {
            NEW(MagCell.Labelled).init(self.layout.getName() & "_wiring"),
            MagTransform.Unitary, 
            WiringCellId,
            box := self.layout.getBBox() };
          
          self.layout.addSub(sub)
        END;
        self.wiringCell := sub.c;
      END
    | ResultMethod.DumpInSelf =>
      self.wiringCell := self.layout
    END;

    self.binnedLayout := NEW(RectBins.T).init(self.layout);
    self.entryDB := NEW(EntryDB.T).init(self.binnedLayout);

    self.layout.flatClipMap2(MarkLayerRect, self);

    (* handle deferrals *)
    IF self.deferrals # NIL THEN
      self.defSubCellName := "SUBCELL_" & RandomText();

      VAR
        sub : MagSubCell.T;
      BEGIN
        sub := MagSubCell.T {
          NEW(MagCell.Labelled).init(self.layout.getName() & "_"  & self.defSubCellName),
          MagTransform.Unitary, 
          self.defSubCellName,
          box := self.layout.getBBox() 
        };
        
        self.layout.addSub(sub);
        self.deferralCell := sub.c;
      END;

      (* draw targets *)
      CONST
        XStep = ARRAY Dir OF [-1..1] { 1, 1, -1, 1, 0, 0 };
      VAR
        bbox := origBBox;
        llG := GridPoint.T { bbox.ll.x DIV GridStep(),
                             bbox.ll.y DIV GridStep(),
                             1 };
        urG := GridPoint.T { bbox.ur.x DIV GridStep(),
                             bbox.ur.y DIV GridStep(),
                             1 };
        cur := ARRAY Dir OF GridPoint.T {
          GridPoint.T { llG.x, urG.y + DefSpacingFromLayout, 1 }, (* N *)
          GridPoint.T { urG.x + DefSpacingFromLayout, llG.y, 1 }, (* E *)
          GridPoint.T { llG.x - DefSpacingFromLayout, llG.y, 1 }, (* W *)
          GridPoint.T { llG.x, llG.y - DefSpacingFromLayout, 1 }, (* S *)
          GridPoint.T { 0, 0,  (* shld never be used *)      1 }, (* U *)
          GridPoint.T { 0, 0,  (* shld never be used *)      1 }  (* D *)
        };
        p := self.deferrals;
        targ : CARDINAL := 0;
      BEGIN
        (* hmm make sure to update labels, too... *)
        WHILE p # NIL DO
          VAR
            newLabs : LabelList.T := NIL;
            myDefs := NEW(GridPointSetDef.T).init();
          BEGIN
            (* make a new target in each listed cardinal dir *)
            
            (* but first, quite arbitrarily choose to do it to the WEST
               if no dir is listed. *)
            IF p.head.exitDirs = SET OF Dir {} THEN
              p.head.exitDirs := SET OF Dir { Dir.W }
            END;
            
            FOR i := FIRST(Dir) TO LAST(Dir) DO
              IF i IN p.head.exitDirs THEN
                newLabs := LabelList.Cons(MagicStuff.DrawATarget(self.deferralCell, cur[i], "TARGET_" & Fmt.Int(targ)), newLabs);
                
                EVAL myDefs.insert(cur[i]);
                
                (* The moving finger writes 
                   And having writ... *)
                cur[i].x := cur[i].x + XStep[i]
              END
            END;

            (* remember deferrals and labels *)
            p.head.gridPoints := myDefs;

            (* attach new labels to old list; big Q: can we change
               JUST the list in p.head and have that change be reflected in
               all the lists in labelsSet.  I think we can but it's ugly :(
            *)
            
            VAR
              lp := p.head.labels;
            BEGIN
              (* if lp is NIL we're dead anyhow---that would mean there are
                 no targets INSIDE the cell but we still want the node to
                 be routable OUTSIDE the cell (pure nonsense) *)
              <* ASSERT p # NIL *> 
              WHILE lp.tail # NIL DO
                lp := lp.tail 
              END;
              
              (* lp points to last record in list; p.tail = NIL *)

              (* stitch on new labels *)
              lp.tail := newLabs
            END
          END; (* VAR *)
          p := p.tail
        END (* WHILE p # NIL *)
      END
    END;

    self.specialTbl := NEW(GridPointRouteIDTbl.Default).init();

    self.comps := NEW(GridPointCompsTbl.Default).init();
    self.pq := NEW(LongrealPQ.Default).init();

    (* set up components *)

    VAR
      iter := labelsSet.iterate();
      ll : LabelList.T;
      nodeComp : MyComps;
      n := 0;
    BEGIN
      WHILE iter.next(ll) DO
        IF ll # NIL THEN
          INC(n);
          IF DoMemReporting AND n MOD 20 = 0 THEN
            RTRefStats.ReportReachable()
          END;
          
          nodeComp := NEW(MyComps, labels := ll, deferred := FALSE).init(
                                                   self.layout,
                                                   self.wiringCell,
                                                   ll,
                                                   self.layerDB,
                                                   self.entryDB,
                                                   self.binnedLayout,
                                                   self.specialTbl,
                                                   self.delWr);
          
          (* set deferral *)
          VAR
            p := self.deferrals;
          BEGIN
            WHILE p # NIL DO
              IF p.head.labels = ll THEN nodeComp.deferred := TRUE END;
              p := p.tail
            END
          END;
          
          EVAL self.compsById.put(nodeComp.id(),nodeComp);
          
          IF nodeComp.size() > 1 THEN
            self.pq.insert(NEW(CompsPQElt, comps := nodeComp,
                               priority:=-nodeComp.longest().distanceEstimate()))
          END
        END
      END
    END;
    
    (* prepare fixed obstacles *)

    (* run router *)

    WHILE self.pq.size() > 0 DO
      VAR
        min : CompsPQElt := self.pq.deleteMin();
        affected : ComponentsSet.T;
        rte : MyRoute;
        nonObs := min.comps.nonObstaclesForCurrentRoute();
        nonEndPointsInNetForCurrentRoute := 
            min.comps.nonEndPointsInNetForCurrentRoute();
        forbiddenSteps := min.comps.forbiddenStepsForCurrentRoute();
        thisOneSucceeded : BOOLEAN;
        actualRoute : GridPointList.T;
        respectBBoxForThis : BOOLEAN;
      BEGIN
          TRY
            REPEAT
              TRY
                thisOneSucceeded := TRUE;

                (* should we respect the bbox here? *)
                
                IF respectBBox THEN
                  (* respect if not a deferral *)
                  respectBBoxForThis := NOT NARROW(min.comps,MyComps).deferred
                ELSE
                  respectBBoxForThis := FALSE
                END;

                Debug.Out("Compute Route...");
                (* compute route *)
                rte := self.routeIt(min.comps, nonObs, 
                                    nonEndPointsInNetForCurrentRoute, 
                                    forbiddenSteps,
                                    ToGridPointCube(origBBox),
                                    respectBBoxForThis
                                    );
                
                Debug.Out("Do Ripups...");
                (* do ripups *)
                IF rte.ripUpStuff().size() > 0 THEN 
                  affected := self.doRipups(rte);
                  self.rescheduleRipups(affected)
                END;
                
                
                (* tell min.comps that it has a new connection *)
                (* it may be modified if it loops around back to itself... *)
                VAR
                  gpl := rte.gridPointList();
                BEGIN
                  <* ASSERT gpl # NIL *>
                  actualRoute := min.comps.connect(gpl)
                END;
                
                (* commit route *)
                rte.commitFinishedRoute(actualRoute);
                IF DoMemReporting AND goes MOD 20 = 0 THEN
                  RTRefStats.ReportReachable()
                END;
                INC(goes)
              EXCEPT
                Components.CantConnectThose(p) =>
                  thisOneSucceeded := FALSE;
                  Debug.Out("Adding pair to forbidden steps: " & 
                    GridPoint.Format(p.p1) & " <--> " & GridPoint.Format(p.p2));
                  forbiddenSteps := forbiddenSteps.copy();
                  forbiddenSteps.addPair(p.p1, p.p2)
                | 
                  Wr.Failure =>
                    Process.Crash("I/O Error writing to .deleted file.")
              END
            UNTIL thisOneSucceeded;
            
            
            VAR
              pp := rte.gridPointList();
            BEGIN
              WHILE pp # NIL DO
                EVAL self.comps.put(pp.head,min.comps);
                pp := pp.tail
              END
            END;
            
            (* IF this net not yet completely connected,
               THEN update priority & put it back on the queue *)
            IF min.comps.size() > 1 THEN
              min.priority := -min.comps.longest().distanceEstimate();
              self.pq.insert(min)
            END;
            
          EXCEPT
            (* route failed *)
            Route.NotFound => 
              Debug.Out("Route failed!!!");
              EVAL failed.insert(min.comps)
          |
            RoutingEmpty =>
              Debug.Out("Routing empty nets!!!!")
          END
      END
    END;

    IF self.deferrals # NIL AND NOT KeepDeferralRoutes THEN
      (* remove deferral layout *)
      VAR
        p := self.deferrals;
      BEGIN
        (* search for component corresponding to each deferred route *)
        WHILE p # NIL DO
          VAR
            iter := self.compsById.iterate();
            id : RouteID.T;
            c : RouteComponents.T;
            mc : MyComps;
            found := FALSE;
          BEGIN
            WHILE iter.next(id,c) DO
              mc := c;
              IF mc.labels = p.head.labels THEN
                IF self.keepInternalDeferredWiring THEN
                  (* new method follows... *)
                  <* FATAL Wr.Failure *>
                  VAR 
                    pIter := p.head.gridPoints.iterate(); 
                    gp : GridPoint.T;
                  BEGIN
                    WHILE pIter.next(gp) DO
                      c.deleteAllEndPointNeighbors(gp)
                    END;
                    EVAL c.commitRipups(writeToDelWr := FALSE)
                  END
                ELSE
                  c.deleteAllPoints()
                END;

                found := TRUE;
                EXIT
              END
            END;
            <* ASSERT found *>
          END;
          p := p.tail
        END
      END;
      
      (* remove deferral cell! *)
      self.layout.delSub(self.defSubCellName)
    ELSIF self.deferrals # NIL AND KeepDeferralRoutes THEN
      TRY self.deferralCell.write() EXCEPT ELSE END
    END;

    (* if the wiring cell is empty, DELETE IT (this keeps bounding boxes
       from getting messed up) *)

    IF self.resultMethod = ResultMethod.UseSubCell AND 
       self.wiringCell.empty() THEN
      self.layout.delSub(WiringCellId)
    END;
    
    (* dump wiring layout *)
    TRY
      self.wiringCell.tightenBBox();
      self.wiringCell.write()
    EXCEPT
      OSError.E, Wr.Failure =>
      Process.Crash("Problems writing wiring layout subcell!")
    END;
    

    (* reformat failed *)
    VAR
      iter := failed.iterate();
      s : RouteComponents.T;
    BEGIN
      WHILE iter.next(s) DO
        EVAL failedNodes.insert(NARROW(s,MyComps).labels)
      END
    END
  END Run;

PROCEDURE DoRipups(self : T; rte : MyRoute) : ComponentsSet.T RAISES { Thread.Alerted, Wr.Failure } =
  (* rip things up *)
  VAR
    stuff := rte.ripUpStuff();
    gp : GridPoint.T;
    iter := stuff.iterate();
    affected := NEW(ComponentsSetDef.T).init();
    x : BOOLEAN;
    comp : RouteComponents.T;
  BEGIN
    WHILE iter.next(gp) DO
      (* find components affected and reschedule *)
      x := self.comps.get(gp,comp); <* ASSERT x *>
      comp.deleteAPoint(gp);
      EVAL affected.insert(comp)
    END;

    (* commit rip-ups; might be no-op *)
    VAR
      aIter := affected.iterate();
    BEGIN
      WHILE aIter.next(comp) DO 
        EVAL comp.commitRipups();

        (* and increase multiplier *)
        VAR
          c : MyComps := comp;
        BEGIN
          c.mult := c.mult * MultiplierMultiplier
        END
      END
    END;

    RETURN affected
  END DoRipups;
  
PROCEDURE RescheduleRipups(self : T; affectedArg : ComponentsSet.T) =
  (* reschedule all that are affected *)
  <* FATAL LongrealPQ.Empty *>
  VAR
    affected := affectedArg.copy();
    newPQ := NEW(LongrealPQ.Default).init();
    elt : CompsPQElt;
  BEGIN

    (* first reschedule what's already on the PQ *)
    WHILE self.pq.size() > 0 DO
      elt := self.pq.deleteMin();
      IF affected.member(elt.comps) THEN
        elt.priority := elt.comps.longest().distanceEstimate();
        EVAL affected.delete(elt.comps)
      END;
      newPQ.insert(elt)
    END;
    
    (* insert all the ones not already on the PQ (i.e., completely
       finished routes that were ripped up) *)
    VAR
      iter := affected.iterate();
      comps : RouteComponents.T;
    BEGIN
      WHILE iter.next(comps) DO
        IF comps.size() > 1 THEN
          newPQ.insert(NEW(CompsPQElt, 
                           priority := comps.longest().distanceEstimate(),
                           comps := comps))
        END
      END
    END;
    
    self.pq := newPQ
  END RescheduleRipups;

PROCEDURE Bloat(READONLY rect : MagRect.T) : MagRect.T =
  VAR
    res : MagRect.T;
  BEGIN
    (* hmm... *)
    res.ll.x := rect.ll.x + 1;
    res.ll.y := rect.ll.y + 1;
    res.ur.x := rect.ur.x + MetalSpacing() - 1;
    res.ur.y := rect.ur.y + MetalSpacing() - 1;
    RETURN res
  END Bloat;

PROCEDURE MarkLayerRect(rect : LayerRect.T; args : REFANY) =
  VAR
    self : T := args; (* NARROW *)
    obsRLayers := Conf.GetObstructedRoutingLayers(rect.layer);
    ip : IntPair.T;
  BEGIN
    LOCK self.mu DO
      Bins.ComputeIterator(GridStep(), Bloat(rect.rect), self.binsIter);

      WHILE self.binsIter.next(ip) DO
      VAR
        obp := obsRLayers;
      BEGIN
        WHILE obp # NIL DO
          (* here we may already have it filled.. *)
          WITH gp = GridPoint.T { ip.k1, ip.k2, obp.head } DO
            IF NOT Filled.Marked(gp) THEN 

              IF DoDebug THEN
                Debug.Out("RoutingGrid.MarkLayerRect: Marking GridPoint as filled: " & GridPoint.Format(gp) & " with: (RouteID.Nil) ");
              END;

              Filled.MarkAsRigid(gp);

              (* also update EntryDB ... *)
              self.entryDB.flushEntries(gp)
            END
          END;
          obp := obp.tail
        END
      END
    END
    END
  END MarkLayerRect;

TYPE 
  KeepOutSet = GridPointSet.T OBJECT
    specialTbl : GridPointRouteIDTbl.T;
    curId : RouteID.T;
  OVERRIDES
    member := KOSMember
  END;

PROCEDURE KOSMember(s : KeepOutSet; p : GridPoint.T) : BOOLEAN =
  VAR
    q := p;
    id : RouteID.T;
  BEGIN
    IF p.l # 2 THEN RETURN FALSE END;
    
    q.l := 1;
    IF NOT s.specialTbl.get(q,id) THEN RETURN FALSE END;

    RETURN id # s.curId
  END KOSMember;
    

PROCEDURE SetDeferrals(t : T; defs : RouterDeferralList.T; 
                       keepInternalDeferredWiring : BOOLEAN) =
  BEGIN
    t.deferrals := defs;
    t.keepInternalDeferredWiring := keepInternalDeferredWiring
  END SetDeferrals;

BEGIN END GridRouter2.




