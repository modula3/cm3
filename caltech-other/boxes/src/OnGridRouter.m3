(* $Id$ *)

MODULE OnGridRouter;
IMPORT Wr, Bbox;
IMPORT IntSet, IntSetDef, GridPointSetSeq, GridPointSetRoutines;
IMPORT OnGridComponents, GridPointRouteIDTbl, LongrealPQ, SimpleRoute, Route;
IMPORT GridPoint, GridPointSet, GridPointSetDef, Debug, GridPointSeq;
IMPORT GridPointCube;
IMPORT RouteID;
IMPORT Components;
IMPORT Thread;
IMPORT Process;
IMPORT IntCompRouteTbl, CompRoute;
IMPORT Fmt;
IMPORT Filled;
IMPORT FileWr;
IMPORT OSError;

TYPE State = { New, Open, Closed };
TYPE NetArr = REF ARRAY OF GridPointSetSeq.T;
TYPE CompsArr = REF ARRAY OF OnGridComponents.T;
TYPE NonObsArr = REF ARRAY OF GridPointSet.T;

CONST MultiplierMultiplier = 1.5d0;
CONST DefaultMultiplier = 10.0d0;

EXCEPTION RoutingEmpty;

<* FATAL Thread.Alerted *>

REVEAL
  T = Public BRANDED Brand OBJECT
    maxNet : [-1..LAST(CARDINAL)] := -1;
    state := State.New;
    delWr : Wr.T := NIL;
    bbox : Bbox.T;
    deferrals : IntSet.T := NIL;
    nets : NetArr;
    nonObs : NonObsArr;
    comps : CompsArr;
    specialTbl : GridPointRouteIDTbl.T;
    quitThreshold := 0.0d0;
    quitFailures := LAST(CARDINAL);
    quitFileName : TEXT := NIL;
  METHODS
    doRipups(rte : MyRoute) : IntSet.T RAISES { Wr.Failure, Thread.Alerted } := DoRipups;
    (* perform ripups associated with a newly-routed route;
       returns affected nets *)

    rescheduleRipups(toReschedule : IntSet.T; VAR pq : LongrealPQ.T) := RescheduleRipups;
    (* reschedule nets affected by ripping-up *)
  OVERRIDES
    init := Init;
    close := Close;
    setDeferrals := SetDeferrals;
    addNet := AddNet;
    run := Run;
    getRoutes := GetRoutes;
    setQuittingRules := SetQuittingRules;
  END;

VAR dLevel := Debug.GetLevel();

PROCEDURE Init(t : T; bbox : Bbox.T; delWr : Wr.T := NIL) : T =
  BEGIN
    <* ASSERT t.state = State.New *>
    t.delWr := delWr;
    t.bbox := bbox;
    t.nets := NEW(NetArr, 0);
    t.nonObs := NEW(NonObsArr, 0);
    INC(t.state);
    RETURN t
  END Init;

PROCEDURE Close(t : T) =
  BEGIN
    <* ASSERT t.state = State.Open *>
    INC(t.state)
  END Close;

PROCEDURE SetDeferrals(t : T; d : IntSet.T) =
  BEGIN
    <* ASSERT t.state = State.Open *>
    t.deferrals := d.copy()
  END SetDeferrals;

PROCEDURE AddNet(t : T; numbered : CARDINAL; net : GridPointSetSeq.T;
                 extraNonObstacles : GridPointSet.T) =
  BEGIN
    t.maxNet := MAX(numbered, t.maxNet);

    <* ASSERT t.state = State.Open *>
    IF numbered > LAST(t.nets^) THEN
      VAR
        new := NEW(NetArr, MAX(numbered + 1,NUMBER(t.nets^)*3 DIV 2));
      BEGIN
        FOR i := FIRST(new^) TO LAST(new^) DO
          new[i] := NIL
        END;
        SUBARRAY(new^,0,NUMBER(t.nets^)) := t.nets^;
        t.nets := new
      END;
      VAR
        new := NEW(NonObsArr, MAX(numbered + 1,NUMBER(t.nonObs^)*3 DIV 2));
      BEGIN
        FOR i := FIRST(new^) TO LAST(new^) DO
          new[i] := NIL
        END;
        SUBARRAY(new^,0,NUMBER(t.nonObs^)) := t.nonObs^;
        t.nonObs := new
      END
    END;
    t.nets[numbered] := net;
    t.nonObs[numbered] := extraNonObstacles
  END AddNet;

PROCEDURE Run(t : T; respectBBox : BOOLEAN;
        (* OUT *)VAR failed : IntSet.T) =
  <* FATAL LongrealPQ.Empty *>

  PROCEDURE WriteQuitFile() =
    BEGIN
          TRY
            VAR
              wr := FileWr.Open(t.quitFileName);
            BEGIN
              TRY
                Wr.PutText(wr, "QUITFILE " & t.quitFileName & "\n");
                Wr.PutText(wr, "VELOCITYTHRESHOLD " & Fmt.LongReal(t.quitThreshold) & "\n");
                Wr.PutText(wr, "VELOCITY " & Fmt.LongReal(v.v) & "\n");
                Wr.PutText(wr, "FAILURETHRESHOLD " & Fmt.Int(t.quitFailures) & "\n");
                
                Wr.PutText(wr, "ACTUALFAILURES " & Fmt.Int(failed.size()) & "\n");
                FOR i := 0 TO ripupSeq.size() - 1 DO
                  Wr.PutText(wr, "R " & GridPoint.Format(ripupSeq.get(i)) & "\n")
                END;
                Wr.PutText(wr, "END\n");
              FINALLY
                Wr.Close(wr)
              END
            END
          EXCEPT
            Wr.Failure, OSError.E => 
              Process.Crash("I/O error writing quitfile")
          END    
    END WriteQuitFile;

  VAR
    pq : LongrealPQ.T;
    v : SimpleRoute.Velocity;
    ripupSeq := NEW(GridPointSeq.T).init();
  BEGIN
    failed := NEW(IntSetDef.T).init();
    <* ASSERT t.state = State.Closed *>

    IF t.deferrals # NIL THEN
      (* set up targets for deferrals *)
      CONST
        DefSpacingFromLayout = 10;
      VAR
        iter := t.deferrals.iterate();
        d : INTEGER;
        p := GridPoint.T { t.bbox.llx - DefSpacingFromLayout, t.bbox.lly, 1 };
      BEGIN
        WHILE iter.next(d) DO
          (* add new component to each deferred net *)
          VAR
            set := NEW(GridPointSetDef.T).init();
          BEGIN
            EVAL set.insert(p);
            t.nets[d].addhi(set)
          END;

          (* and mark it *)
          Filled.MarkAsRigid(p);

          DEC(p.x)
        END
      END
    END;

    t.specialTbl := NEW(GridPointRouteIDTbl.Default).init();

    pq := NEW(LongrealPQ.Default).init();

    t.comps := NEW(CompsArr, NUMBER(t.nets^));

    FOR i := FIRST(t.nets^) TO LAST(t.nets^) DO
      IF t.nets[i] # NIL THEN
        VAR
          comps := NEW(MyComps).init(t.nets[i], t.delWr,
                                     i, t.specialTbl);
        BEGIN
          t.comps[i] := comps;
          IF comps.size() > 1 THEN 
            pq.insert(NEW(PQElt, index := i,
                          priority := -comps.longest().distanceEstimate()))
          END
        END
      END
    END;

    WHILE pq.size() > 0 DO
      VAR
        min : PQElt := pq.deleteMin();
        respectBBoxForThis : BOOLEAN;
        rte := NEW(MyRoute, rtr := t, comps := t.comps[min.index]);
        comps := rte.comps.longest();
        nonObs := rte.comps.nonObstaclesForCurrentRoute();
        mybbox := GridPointCube.T { 
         GridPoint.T { t.bbox.llx, t.bbox.lly, FIRST(GridPoint.Layer) },
         GridPoint.T { t.bbox.urx, t.bbox.ury, LAST(GridPoint.Layer) } };
      BEGIN

        (* ok... add to nonObs the points that are in OUR nonObs from the
           input file but are NOT taken up by another route... *)
        VAR
          iter := t.nonObs[min.index].iterate();
          p : GridPoint.T;
        BEGIN
          WHILE iter.next(p) DO
            IF NOT Filled.Marked(p) OR Filled.MarkedRoute(p) = min.index OR Filled.MarkedRoute(p) = RouteID.Nil THEN
              EVAL nonObs.insert(p)
            END
          END
        END;

        TRY
          respectBBoxForThis := respectBBox AND 
          (t.deferrals = NIL OR NOT t.deferrals.member(min.index));

          IF dLevel > 0 THEN
            Debug.Out("Compute route... net " & Fmt.Int(min.index));
            Debug.Out("Route SET 1:\n" &GridPointSetRoutines.Format(comps.c1));
            Debug.Out("Route set 2:\n" &GridPointSetRoutines.Format(comps.c2))
          END;
          
          IF comps.c1.size() = 0 OR comps.c2.size() = 0 THEN
            RAISE RoutingEmpty 
          END;

          rte.computeRouteBetweenSets(comps.c1, comps.c2, 
                                      multiplier := FLOAT(rte.comps.mult),
                                      nonObstacles := nonObs,
                                      keepOuts := NEW(KeepOutSet, specialTbl := t.specialTbl, curId := rte.myID()),
                                      BBox := mybbox,
                                      respectBBox := respectBBoxForThis,
                                      velocity := v,
                                      ripupSeq := ripupSeq);
          <* ASSERT rte.ripUpStuff() # NIL *> (* ??? *)

         Debug.Out("Do Ripups...");
          (* do ripups *)
          IF rte.ripUpStuff().size() > 0 THEN 
            VAR
              affected := t.doRipups(rte);
            BEGIN
              t.rescheduleRipups(affected, pq)
            END
          END;

          (* tell min.comps that it has a new connection *)
          (* it may be modified if it loops around back to itself... *)
          <* FATAL Components.CantConnectThose *> (* not in this rtr *)
          VAR
            gpl := rte.gridPointList();
            actualRoute := t.comps[min.index].connect(gpl);
          BEGIN
            
            IF dLevel > 0 THEN
              VAR
                d := "Connecting points... ";
                p := gpl;
              BEGIN
                WHILE p # NIL DO
                  d := d & GridPoint.Format(p.head) & " ";
                  p := p.tail
                END;
                Debug.Out(d & "\n")
              END
            END;

            (* commit route *)
            rte.commitFinishedRoute(actualRoute);
          END;

          VAR
            deleted := NEW(GridPointSetDef.T).init(); (* non-art points *)
          BEGIN
            t.comps[min.index].cleanOutNonArts(deleted);
          END;

          (* IF this net not yet completely connected,
             THEN update priority & put it back on the queue *)
          IF dLevel > 0 THEN
            Debug.Out("t.comps["& Fmt.Int(min.index) & "].size() = " & Fmt.Int(t.comps[min.index].size()));
            Debug.Out("t.comps["& Fmt.Int(min.index) & "].activeEndPoints() = " & GridPointSetRoutines.Format(t.comps[min.index].activeEndPoints()))
          END;

          IF t.comps[min.index].size() > 1 THEN
            min.priority := -t.comps[min.index].longest().distanceEstimate();
            pq.insert(min)
          END
        EXCEPT
          (* route failed *)
          Route.NotFound => 
            Debug.Out("Route failed!!!");
            EVAL failed.insert(min.index)
        |
          RoutingEmpty =>
            Debug.Out("Routing empty nets!!!!")
        | 
          Wr.Failure => Process.Crash("I/O error writing to .deleted file.")
        END;

        (* check for quitting *)
        IF v.v <= t.quitThreshold OR failed.size() >= t.quitFailures THEN
          Debug.Out("Quitting because of routing congestion!");
          Debug.Out("v.v = " & Fmt.LongReal(v.v));
          Debug.Out("failed.size = " & Fmt.Int(failed.size()));

          WriteQuitFile();

          Process.Exit(1)
        END
      END
    END;

    IF failed.size() > 0 THEN WriteQuitFile() END
  END Run;

TYPE 
  PQElt = LongrealPQ.Elt OBJECT
    index : CARDINAL;
  END;

  MyComps = OnGridComponents.T OBJECT
    mult := DefaultMultiplier;
  END;

  MyRoute = SimpleRoute.T OBJECT
    rtr : T;
    comps : MyComps;
  OVERRIDES
    ripUpCost := RouteRipUpCost;
    myID := RouteMyID;
  END;

PROCEDURE RouteMyID(r : MyRoute) : RouteID.T =
  BEGIN RETURN r.comps.id() END RouteMyID;

PROCEDURE RouteRipUpCost(m : MyRoute; r : RouteID.T) : LONGREAL = 
  VAR
    c : MyComps;
  BEGIN
    c := m.rtr.comps[r];
    
    RETURN c.approxSize() * c.mult
  END RouteRipUpCost;

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
    IF p.l # 2 THEN RETURN FALSE END;  (* funny optimization... *)
    
    q.l := 1;
    IF NOT s.specialTbl.get(q,id) THEN RETURN FALSE END;

    RETURN id # s.curId
  END KOSMember;
    

PROCEDURE DoRipups(t : T; rte : MyRoute) : IntSet.T RAISES { Wr.Failure, Thread.Alerted } =
  VAR
    stuff := rte.ripUpStuff();
    gp : GridPoint.T;
    iter := stuff.iterate();
    affected := NEW(IntSetDef.T).init();
    comp : INTEGER;
  BEGIN
    WHILE iter.next(gp) DO
      (* find components affected and reschedule *)
      IF dLevel > 0 THEN
        Debug.Out("DoRipups: deleting at " & GridPoint.Format(gp))
      END;

      comp := Filled.MarkedRoute(gp);

      (* comp's being -1 here would mean that the routines before
         failed to clean out the endpoints from the ripups.  that wouldn't
         be right. *)
      <* ASSERT comp # -1 *>

      t.comps[comp].deleteAPoint(gp);
      EVAL affected.insert(comp)
    END;

    (* commit rip-ups; might be no-op *)
    VAR
      aIter := affected.iterate();
      s : GridPointSet.T;
    BEGIN
      WHILE aIter.next(comp) DO 
        s := t.comps[comp].commitRipups();
        (* and increase multiplier *)
        VAR
          c : MyComps := t.comps[comp];
        BEGIN
          c.mult := c.mult * MultiplierMultiplier
        END
      END
    END;

    RETURN affected
  END DoRipups;

PROCEDURE RescheduleRipups(t : T; toReschedule : IntSet.T; VAR pq : LongrealPQ.T) =
  (* reschedule all that are affected *)
  <* FATAL LongrealPQ.Empty *>
  VAR
    affected := toReschedule.copy();
    newPQ := NEW(LongrealPQ.Default).init();
    elt : PQElt;
  BEGIN

    (* first reschedule what's already on the PQ *)
    WHILE pq.size() > 0 DO
      elt := pq.deleteMin();
      IF affected.member(elt.index) THEN
        elt.priority := t.comps[elt.index].longest().distanceEstimate();
        EVAL affected.delete(elt.index)
      END;
      newPQ.insert(elt)
    END;
    
    (* insert all the ones not already on the PQ (i.e., completely
       finished routes that were ripped up) *)
    VAR
      iter := affected.iterate();
      c : INTEGER;
    BEGIN
      WHILE iter.next(c) DO
        IF t.comps[c].size() > 1 THEN
          newPQ.insert(NEW(PQElt, 
                           priority := t.comps[c].longest().distanceEstimate(),
                           index := c))
        END
      END
    END;
    
    pq := newPQ
  END RescheduleRipups;


PROCEDURE GetRoutes(t : T) : IntCompRouteTbl.T =
  VAR
    tbl := NEW(IntCompRouteTbl.Default).init();
  BEGIN
    FOR i := FIRST(t.comps^) TO t.maxNet DO
      VAR
        c := t.comps[i];
      BEGIN
        EVAL tbl.put(i,NEW(CompRoute.T, activeEndPoints := c.activeEndPoints(),
                           allPoints := c.allPoints()))
      END
    END;
    RETURN tbl
  END GetRoutes;

PROCEDURE SetQuittingRules(t : T; threshold : LONGREAL; failures : CARDINAL;
                           fileName : TEXT) =
  BEGIN
    t.quitThreshold := threshold;
    t.quitFailures := failures;
    t.quitFileName := fileName
  END SetQuittingRules;

BEGIN END OnGridRouter.






