(* $Id$ *)

MODULE Route;

IMPORT GridPoint,Step, GridPointCube;
IMPORT Filled;
IMPORT LocPQ;
IMPORT Loc, GridPointList, GridPointSetRoutines AS GridPointSet;
IMPORT GridPointSetDef;
IMPORT Cost;
IMPORT Debug,Fmt;
IMPORT RipUp;
IMPORT RouteState;
IMPORT ForbiddenSteps;
IMPORT TextRefTbl;
IMPORT IntPair;
IMPORT RouteID;
IMPORT Wr, FileWr;
IMPORT Thread, OSError;
IMPORT Env;

VAR debugAll := Debug.DebugThis("Route");
VAR debugRip := debugAll OR Env.Get("DEBUGRIPUPS") # NIL;
VAR dLevel := Debug.GetLevel();

<* FATAL Cost.OutOfMemory *>

REVEAL
  Default = Public BRANDED Brand OBJECT 
    attrTbl : TextRefTbl.T := NIL;
    gpl : GridPointList.T;
    graveyard : GridPointSet.T;
    from, to : GridPoint.T;
    finalCost := -1;

    (* stuff to carry through rip-up phase *)
    destPt : GridPoint.T;
    multiplier : REAL;
    nonObstacles : GridPointSet.T;
  OVERRIDES
    gridPointList := TGridPointList;
    addAttribute := AddAttribute;
    getAttribute := GetAttribute;
    deleteAttribute := DeleteAttribute;
    computeRouteBetweenSets := RouteSets;
    commitFinishedRoute := Commit;
    ripUpStuff := RipUpStuff;
    getCost := GetCost;
  END;

PROCEDURE Commit(rte : Default; actual : GridPointList.T) =
  BEGIN
    rte.gpl := actual;
    IF debugAll THEN
      VAR
        p := actual;
      BEGIN
        Debug.Out("Route.Commit: committing route:");
        WHILE p # NIL DO
          Debug.Out(GridPoint.Format(p.head) & ARRAY BOOLEAN OF TEXT { "",
                                                                       " [marked]"} [Filled.Marked(p.head)]);
          p := p.tail;
        END
      END
    END;
    MarkPointList(rte, rte.gpl, rte.nonObstacles)
  END Commit;

PROCEDURE GetCost(t : Default) : CARDINAL = BEGIN RETURN t.finalCost END GetCost;

PROCEDURE RipUpStuff(t : Default) : GridPointSet.T =
  BEGIN RETURN t.graveyard END RipUpStuff;

PROCEDURE AddAttribute(self : Default; aName : TEXT; attr : REFANY) =
  VAR
    x : BOOLEAN;
  BEGIN
    Debug.Out("Route.AddAttribute: adding attrib. \"" & aName & "\"");
    IF self.attrTbl = NIL THEN 
      self.attrTbl := NEW(TextRefTbl.Default).init() 
    END;
    x := self.attrTbl.put(aName,attr);
    IF x THEN 
      Debug.Error("Attempting to add already-known attribute \"" & 
        aName & "\"")
    END
  END AddAttribute;

PROCEDURE GetAttribute(self : Default; aName : TEXT) : REFANY =
  VAR
    attr : REFANY;
  BEGIN
    Debug.Out("Route.GetAttribute: getting attrib. \"" & aName & "\"");
    IF self.attrTbl = NIL OR NOT self.attrTbl.get(aName,attr) THEN 
      Debug.Error("Attempting to get nonexistent attribute \"" & 
        aName & "\"")
    END;
    RETURN attr
  END GetAttribute;

PROCEDURE DeleteAttribute(self : Default; aName : TEXT) =
  VAR
    attr : REFANY;
  BEGIN
    Debug.Out("Route.DeleteAttribute: deleting attrib. \"" & aName & "\"");
    IF self.attrTbl = NIL OR NOT self.attrTbl.delete(aName,attr) THEN 
      Debug.Error("Attempting to delete nonexistent attribute \"" & 
        aName & "\"")
    END
  END DeleteAttribute;

PROCEDURE TGridPointList(self : Default) : GridPointList.T =
  BEGIN RETURN self.gpl END TGridPointList;
  
PROCEDURE BackWards(VAR p : GridPoint.T; 
                    READONLY tgt : GridPoint.T) : BOOLEAN =
  (* find minimum neighbor that is in the backward path *)
  VAR
    res : BOOLEAN;
  BEGIN 
    WITH prev = Cost.GetRec(p,tgt).previous DO
      res := NOT GridPoint.Equal(p, prev);
      p := prev
    END;
    RETURN res
  END BackWards;

PROCEDURE RouteIter(t : Default;
                    q : LocPQ.T; to : GridPoint.T;
                    nonObstacles : GridPointSet.T; 
                    VAR count,cost : INTEGER;
                    VAR finalState : RouteState.T;
                    absoluteMaxCost : CARDINAL;
                    toSet : GridPointSet.T;

                    (* destPt is the actual endpoint of the route *)
                    VAR destPt : GridPoint.T;
                    forbiddenSteps : ForbiddenSteps.T;
                    VAR neighs : Step.Iterator;
                    READONLY boundingBox : BoundingBox;
                    keepOuts : GridPointSet.T;
                    pointsThatCostZero : GridPointSet.T;
                    READONLY absoluteMaxBBox : GridPointCube.T;
                    respectAbsMaxBBox : BOOLEAN;
                    VAR eltRecycleBin : REFANY) : BOOLEAN RAISES { LocPQ.Empty } =
  VAR
    oldElt := q.deleteMin();
    next := oldElt.priority;
    routeState : RouteState.T := next.state;
    oldRec := Cost.GetRec(next.point,to);
  BEGIN
    
    oldElt.priority.state := eltRecycleBin;
    eltRecycleBin := oldElt;

    <* ASSERT routeState # NIL *>

    (* better thing before?  then just ignore the new one *)
    IF oldRec.minCost <= next.minCost THEN RETURN FALSE END;

    (* can't be any ripups at endpoint so we can test this first... *)
    IF next.point = to OR toSet # NIL AND toSet.member(next.point) THEN
      cost := next.minCost;
      Debug.Out("Route done! cost = " & Fmt.Int(cost));
      finalState := next.state;
      
      destPt := next.point;
      Cost.Mark(next.point, next.prev, to, next.minCost);
      RETURN TRUE
    END;

    (* first check if there is an obstacle that can't be ripped up *)
    IF NOT nonObstacles.member(next.point) AND Filled.Marked(next.point) AND Filled.MarkedRoute(next.point) = RouteID.Nil THEN
      RETURN FALSE
    END;

    (* check if there is something there. *)
    (* if there is we need to rip up *)

    IF debugAll AND Filled.Marked(next.point) AND NOT nonObstacles.member(next.point) THEN
      Debug.Out("Searching " & GridPoint.Format(next.point) & " and found it marked... nonObstacles.member("&GridPoint.Format(next.point)&") = " & Fmt.Bool(nonObstacles.member(next.point)))
    END;

    IF NOT nonObstacles.member(next.point) AND Filled.Marked(next.point) THEN
      IF NOT routeState.rippedUpAnywhere(Filled.MarkedRoute(next.point)) THEN
        IF dLevel > 0 THEN
          Debug.Out("Scheduling rip-up at " & GridPoint.Format(next.point) &
            " marked route = " & Fmt.Int(Filled.MarkedRoute(next.point)))
        END;
        <* ASSERT routeState # NIL *>
        routeState := routeState.addRipUp(Filled.MarkedRoute(next.point),
                                          next.point, first := TRUE);

        (* re-schedule this route with the new, actual cost... *)

        VAR
          actualCost := next.minCost - Cost.MinRipUpCost + 
                            RipUp.Cost(t,Filled.MarkedRoute(next.point),
                                       routeState.previous() (* N.B. we have already added the rip-up at this point *));
        BEGIN
          IF dLevel > 19 THEN
            Debug.Out("Route.RouteIter: actual cost with rip-up = " & Fmt.Int(actualCost), 20)
          END;
          IF actualCost <= absoluteMaxCost THEN
            IF dLevel > 19 THEN
              Debug.Out("Route.RouteIter: inserting rip-up path into q", 20)
            END;
            VAR
              newLoc := next;
            BEGIN
              (* update old record to account for rip-up cost *)
              newLoc.minCost := actualCost;
              newLoc.state := routeState;
              VAR
                elt : LocPQ.Elt;
              BEGIN
                IF eltRecycleBin = NIL THEN
                  elt := NEW(LocPQ.Elt)
                ELSE
                  elt := eltRecycleBin;
                  eltRecycleBin := elt.priority.state;
                END;
                elt.priority := newLoc;
                q.insert(elt)
              END
            END
          END
        END;
        IF dLevel > 0 THEN
          Debug.Out("routeState has " & 
            Fmt.Int(routeState.numRipUps()) & " rip-ups")
        END;
        RETURN FALSE



      ELSE
        (* already marked that we have ripped up that particular route,
           just remember the new point *)
        IF dLevel > 0 THEN
          Debug.Out("routeState.rippedUpHere(Filled.MarkedRoute(next.point),next.point) = " & Fmt.Bool(routeState.rippedUpHere(Filled.MarkedRoute(next.point),
                                       next.point)));
        END;
        IF NOT routeState.rippedUpHere(Filled.MarkedRoute(next.point),
                                       next.point) THEN
          routeState := routeState.addRipUp(Filled.MarkedRoute(next.point),
                                            next.point)
        END
      END
    END;

    (* if we get here, we did not have to reschedule a rip-up, and
       next.minCost represents an ACTUAL cost of stepping to this point *)
    (* it is better, so mark it. *)

    Cost.Mark(next.point, next.prev, to, next.minCost);

    (* now look at the neighbors and continue routing *)

    Step.Neighbors(next.point, to, nonObstacles, 
                   pointsThatCostZero,
                   routeState, forbiddenSteps, neighs,
                   absoluteMaxBBox,
                   respectAbsMaxBBox);

    VAR 
      cand : Step.T; 
    BEGIN
      WHILE neighs.next(cand) DO
        
        INC(count);
        (* is it ok to move there---is it within the space and uncovered? *)
        VAR 
          loc := Cost.GetRec(cand.p,to);

          (* the total distance to cand is the distance to next 
             plus the cost of the step.

             Here we add the special extra term for steps outside the
             bounding box...
          *)
          outsideBboxMultiplier := FLOAT(DistanceOutsideBbox(cand.p,boundingBox)) *
          Cost.OutOfBoundsIncreaseRate;
          
          newDistance := next.minCost + 
                             cand.cost * ROUND(1.0 + outsideBboxMultiplier); 
          hadIt := loc.minCost <= newDistance;
        BEGIN
          (* we can mark it if:
             (1) we don't have it
         and (2) it's not a keepout
         and (3) the newDistance is less than infinity
           *)
             
          IF NOT hadIt AND 
            (keepOuts = NIL OR NOT keepOuts.member(cand.p)) AND 
            newDistance <= absoluteMaxCost THEN

            <* ASSERT next.point # cand.p *>
            
            (* add to priority queue *)
            VAR
              elt : LocPQ.Elt;
            BEGIN
              IF eltRecycleBin = NIL THEN
                elt := NEW(LocPQ.Elt)
              ELSE
                elt := eltRecycleBin;
                eltRecycleBin := elt.priority.state;
              END;
              elt.priority := Loc.T{ next.point,
                                     cand.p, newDistance, 
                                     routeState,
                                     Cost.Greedy(cand.p,to) };
              q.insert(elt)
            END
          END
        END
      END
    END;
    RETURN FALSE
  END RouteIter;

TYPE BoundingBox = RECORD ll, ur : IntPair.T END;

     (* how far outside boundingBox is p in Manhattan space? *)
PROCEDURE DistanceOutsideBbox(p : GridPoint.T; 
                              bb : BoundingBox) : CARDINAL =
  VAR
    res := 0;
  BEGIN
    IF p.x < bb.ll.k1 THEN 
      res := res + bb.ll.k1 - p.x 
    END;
    IF p.x > bb.ur.k1 THEN 
      res := res + p.x - bb.ur.k1 
    END;

    IF p.y < bb.ll.k2 THEN 
      res := res + bb.ll.k2 - p.y 
    END;
    IF p.y > bb.ur.k2 THEN 
      res := res + p.y - bb.ur.k2
    END;

    RETURN res
  END DistanceOutsideBbox;

TYPE 
  MyLocPQ = LocPQ.Default OBJECT
    to : GridPoint.T;
  END;

PROCEDURE RouteDriver(rte : Default;
                      fromSet, toSet, nonObstacles : GridPointSet.T;
                      forbiddenSteps : ForbiddenSteps.T;
                      multiplier : REAL;
                      keepOuts : GridPointSet.T;
                      pointsThatCostZero : GridPointSet.T;
                      READONLY absoluteMaxBBox : GridPointCube.T;
                      respectAbsMaxBBox : BOOLEAN) RAISES{ NotFound } =
  VAR
    destPt : GridPoint.T;
    from := rte.from;
    to := rte.to;
    q1, q2 : MyLocPQ := NEW(MyLocPQ).init();
    count := 0;
    totCost : INTEGER;
    iters := 0;
    greedyCost := Cost.Greedy(from,to);
    finalState : RouteState.T;

    absoluteMaxCost := MIN(Cost.MaxMaxCost, 
                           ROUND(FLOAT(greedyCost) * Cost.MaxCostOverGreedy) +
                           Cost.BaseCost);
    neighs : Step.Iterator := NIL;
    boundingBox : BoundingBox;
    recycleBin : REFANY := NIL;
  BEGIN
    (* compute bounding box of routes *)
    VAR 
      fi := fromSet.iterate();
      ti := toSet.iterate();
      p : GridPoint.T;
    BEGIN
      (* if fromSet is size 0 there will be no routing, but we'll worry
         about that somewhere else; this is just a stupid bounding-box
         calculation... *)
      IF fromSet.size() > 0 THEN
        (* pick first point in fromSet *)
        EVAL fi.next(p);

        (* set up initial bounding box *)
        boundingBox.ll.k1 := p.x;
        boundingBox.ll.k2 := p.y;
        boundingBox.ur := boundingBox.ll;
        
        WHILE fi.next(p) DO
          boundingBox.ll.k1 := MIN(p.x,boundingBox.ll.k1);
          boundingBox.ll.k2 := MIN(p.y,boundingBox.ll.k2);
          boundingBox.ur.k1 := MAX(p.x,boundingBox.ur.k1);
          boundingBox.ur.k2 := MAX(p.y,boundingBox.ur.k2)
        END;
        WHILE ti.next(p) DO
          boundingBox.ll.k1 := MIN(p.x,boundingBox.ll.k1);
          boundingBox.ll.k2 := MIN(p.y,boundingBox.ll.k2);
          boundingBox.ur.k1 := MAX(p.x,boundingBox.ur.k1);
          boundingBox.ur.k2 := MAX(p.y,boundingBox.ur.k2)
        END
      END
    END;
    
    (* call InitRoute to clear out old crud from tables *)
    Cost.InitRoute();
    IF Filled.Marked(from) AND NOT nonObstacles.member(from) OR 
      Filled.Marked(to) AND NOT nonObstacles.member(to) OR to = from THEN 
      RAISE NotFound("Illegal starting point(s).")
    END;

    IF debugAll THEN 
      Debug.Out("Greedy cost = " & Fmt.Int(greedyCost))
    END;
(*
    Cost.Mark(from,from,from,to,0);
    Cost.Mark(to,to,to,from,0);
*)

    q1.to := to;
    q1.insert(NEW(LocPQ.Elt, 
                  priority := Loc.T { from, from, 0,
                                      NEW(RouteState.T),
                                      Cost.Greedy(from, to)}));
    
    q2.to := from;
    q2.insert(NEW(LocPQ.Elt, 
                  priority := Loc.T { to, to, 0,
                                      NEW(RouteState.T),
                                      Cost.Greedy(to,from)}));
    
    
    
    IF fromSet # NIL THEN
      VAR
        fIter := fromSet.iterate();
        p : GridPoint.T;
      BEGIN
        WHILE fIter.next(p) DO 
          IF p # from THEN
            q1.insert(NEW(LocPQ.Elt, 
                          priority := Loc.T { p, p, 0,
                                              NEW(RouteState.T), Cost.Greedy(p,to) }))
          END;
        END
      END
    END;
    
    IF toSet # NIL THEN
      VAR
        tIter := toSet.iterate();
        p : GridPoint.T;
      BEGIN
        WHILE tIter.next(p) DO 
          IF p # to THEN
            q2.insert(NEW(LocPQ.Elt, 
                          priority := Loc.T { p, p, 0,
                                              NEW(RouteState.T), Cost.Greedy(p,from) }))
          END
        END
      END
    END;
    
    TRY LOOP
      (* take a step in the direction from -> to *)

      (* Debug.Out("RouteIter from -> to",20); *)
      IF RouteIter(rte,q1,to,nonObstacles, count,totCost,
                   finalState,absoluteMaxCost, toSet, destPt, forbiddenSteps, 
                   neighs, boundingBox, keepOuts, pointsThatCostZero,
                   absoluteMaxBBox, respectAbsMaxBBox, recycleBin) THEN 
        IF finalState = NIL THEN
          Debug.Out("Forward path: finalState = NIL")
        ELSE
          IF dLevel > 0 THEN
            Debug.Out("Forward path: finalState has " & 
              Fmt.Int(finalState.numRipUps()) & " rip-ups")
          END
        END;
        EXIT 
      END;

      (* the second queue is used JUST to make sure we don't get     *)
      (* stuck in a corner..                                         *)
      (* every 20 iters. of the first queue, we take a step here.    *)
      (* chances are, if we are stuck in a corner, then this will    *)
      (* die quickly...                                              *)
      IF iters MOD 20 = 0 THEN 
        (* Debug.Out("RouteIter to -> from",20); *)
        IF RouteIter(rte,q2,from,nonObstacles,count,totCost,
                     finalState,absoluteMaxCost, fromSet, destPt, forbiddenSteps, neighs, boundingBox, keepOuts, pointsThatCostZero,
                   absoluteMaxBBox, respectAbsMaxBBox, recycleBin) THEN
          Debug.Out("Reverse path!");
          
        IF finalState = NIL THEN
          Debug.Out("Reverse path: finalState = NIL")
        ELSE
          IF dLevel > 0 THEN
            Debug.Out("Reverse path: finalState has " & 
              Fmt.Int(finalState.numRipUps()) & " rip-ups")
          END
        END;

        (* record that we flipped the path *)
        VAR
          tmp := rte.from;
        BEGIN
          rte.from := rte.to;
          rte.to := tmp
        END;

        EXIT
      END
      END;

      INC(iters)
    END EXCEPT 
      LocPQ.Empty => 
        Debug.Out("RAISING NotFound -- No route exists");
        RAISE NotFound("No route exists") 
    END; 
    
    Debug.Out("Route: iters = " & Fmt.Int(iters) & ", " &
      Fmt.Int(count) & " positions examined, cost = " &
      Fmt.Int(totCost) & ".");

    (* make adjustments demanded by finalState *)
    rte.graveyard := finalState.ripUpPoints();
    
    rte.destPt := destPt;
    rte.multiplier := multiplier;
    rte.nonObstacles := nonObstacles;
    
    IF debugRip THEN
      (* write some stuff to the debug file *)
      <* FATAL Thread.Alerted, Wr.Failure *>
      VAR
        iter := finalState.firstRipUpIterate();
        id : RouteID.T;
        p : GridPoint.T;
      BEGIN
        Wr.PutText(ripWr, "RIPUPS FOR ROUTE " & GridPoint.Format(to) & GridPoint.Format(from) & "\n");
        WHILE iter.next(id,p) DO
          VAR
            fromd1 := ABS(from.x - p.x) + ABS(from.y - p.y);
            tod1 := ABS(to.x - p.x) + ABS(to.y - p.y);
          BEGIN
            Wr.PutText(ripWr, "ripup @ " & GridPoint.Format(p) & 
              " id: " & RouteID.Format(id) &
              " from: " & GridPoint.Format(from) & " to: " & GridPoint.Format(to) & " lesserD= " &
              Fmt.Int(MIN(fromd1,tod1)) & "\n");
            Wr.Flush(ripWr)
          END
        END
      END
    END;

    DoBackSearch(rte);

    (* check to make sure everything is A-OK *)
    VAR
      g := rte.gridPointList();
      rips := rte.ripUpStuff().copy();
      marked : BOOLEAN;
      markedRoute : RouteID.T;
      nonObsMember : BOOLEAN;
    BEGIN
      WHILE g # NIL DO
        marked := Filled.Marked(g.head);
        nonObsMember := nonObstacles.member(g.head);
        IF marked AND NOT nonObsMember THEN
          markedRoute := Filled.MarkedRoute(g.head);
          IF markedRoute # rte.myID() THEN
            <* ASSERT rips.delete(g.head) *>
          END
        END;
        g := g.tail
      END;
      <* ASSERT rips.size() = 0 *>
    END;
    
    RETURN
  END RouteDriver;

VAR mu := NEW(MUTEX);

PROCEDURE RouteSets(self : Default; fromSet, toSet : GridPointSet.T; 
                    nonObstacles : GridPointSet.T;
                    forbiddenSteps : ForbiddenSteps.T;
                    multiplier : REAL;          
                    READONLY BBox : GridPointCube.T;
                    respectBBox : BOOLEAN;
                    keepOuts : GridPointSet.T;
                    pointsThatCostZero : GridPointSet.T) RAISES { NotFound } = 
  VAR
    (* some representative points *)
    from, to : GridPoint.T;
  BEGIN 
    LOCK mu DO
      Step.DoSpecials := TRUE;
      (* set up sets so that fromSet is larger than toSet *)
      (* this is a hack to account for the heuristic nature of the *)
      (* directional search... *)

      IF GridPointSet.CostExtent(fromSet) < GridPointSet.CostExtent(toSet) THEN
        VAR
          temp : GridPointSet.T; 
        BEGIN temp := fromSet; fromSet := toSet; toSet := temp END
      END;
      
      (* if there are no points in the sets, then there is no work to do.. *)
      IF fromSet.size() = 0 OR toSet.size() = 0 THEN 
        self.graveyard := NEW(GridPointSetDef.T).init();
        self.gpl := NIL; 
        RETURN 
      END;
      
      VAR
        d : CARDINAL;
      BEGIN
        GridPointSet.NearestPoints(fromSet,toSet, from, to, d)
      END;
      
      self.from := from;
      self.to := to;


      RouteDriver(self, fromSet, toSet, nonObstacles, 
                  forbiddenSteps, multiplier, keepOuts,
                  pointsThatCostZero, BBox, respectBBox);
      
      (* DoBackSearch(self); *)
      <* ASSERT self.graveyard # NIL *>
      RETURN
    END
  END RouteSets;

PROCEDURE DoBackSearch(rte : Default) =

  (* derive straight segments from list of neighboring points *)
  (* XXX searches FROM to TO from... a bit confusing, eh? *)
  (* should be FROM tgt TO src.. *)
  VAR
    startAt := rte.destPt;
    multiplier := rte.multiplier;
    pointList : GridPointList.T := NIL;
    tgt := rte.to;

    (* to and startAt will be the same if we are not searching sets. *)
    (* if we are searching sets, then they may be different, since our *)
    (* search may not have terminated at the "target point" of the search *)
      
    to := startAt;
    from := rte.from;
  PROCEDURE ConsItOn(p : GridPoint.T) =
    BEGIN
      <* ASSERT NOT GridPointList.Member(pointList,p) *>
      pointList := GridPointList.Cons(p,pointList)
    END ConsItOn;

   BEGIN
    (* get ready to record the chosen path *)
    WITH rec = Cost.GetRec(to,tgt) DO
      (* make sure that recorded cost came from the right path.. *)
      (*<* ASSERT rec.point = from *> *)
      
      rte.finalCost := ROUND(FLOAT(rec.minCost)*multiplier)
    END;

    (* do backward search *)
    (* this isn't right when we can have multiple starting points *)
    <* ASSERT NOT GridPoint.Equal(to,from) *>
    
    ConsItOn(to);
    WHILE BackWards(to, tgt) DO ConsItOn(to) END;

    rte.gpl :=  pointList;
  END DoBackSearch;

PROCEDURE MarkPointList(rte : Default; gplp : GridPointList.T; 
                        nonObstacles : GridPointSet.T) =
  VAR
    text := "";
    doDebug := Debug.GetLevel() > 0;
  BEGIN
    WHILE gplp # NIL DO
      IF doDebug THEN
        text := text & " " & GridPoint.Format(gplp.head)
      END;
      
      (* mark segment as taken *)
      IF gplp.tail # NIL THEN
        (* we need to mark every point except the one that the
           segments have in common *)
        VAR
          headPt := gplp.head;
          tailPt := gplp.tail.head;
        BEGIN
          (* if this is the last segment, then get the last point, too *)
          IF gplp.tail.tail = NIL THEN
            Filled.MarkSegment(headPt, tailPt, rte.myID(), nonObstacles)
          ELSE
            IF    headPt.x < tailPt.x THEN 
              DEC(tailPt.x)
            ELSIF headPt.y < tailPt.y THEN
              DEC(tailPt.y)
            ELSIF headPt.l < tailPt.l THEN
              DEC(tailPt.l)
            ELSIF headPt.x > tailPt.x THEN
              INC(tailPt.x)
            ELSIF headPt.y > tailPt.y THEN
              INC(tailPt.y)
            ELSIF headPt.l > tailPt.l THEN
              INC(tailPt.l)
            ELSE 
              Debug.Error("Attempting to mark " & GridPoint.Format(headPt) &
                " --> " & GridPoint.Format(tailPt))
            END;
            Filled.MarkSegment(headPt, tailPt, rte.myID(), nonObstacles)
          END
        END
      END;
      gplp := gplp.tail
    END;
    Debug.Out(text & "\n");

  END MarkPointList;

PROCEDURE SetRipUpFilename(fn : TEXT) =
  BEGIN
    TRY
      IF debugRip THEN
        TRY Wr.Close(ripWr) EXCEPT ELSE END;
        ripWr := FileWr.Open(fn)
      END
    EXCEPT
      OSError.E => (* skip *)
    END
  END SetRipUpFilename;

VAR
  ripWr : Wr.T := NIL;
BEGIN 
  IF debugRip THEN
    TRY 
      ripWr := FileWr.Open("x.ripups")
    EXCEPT
      OSError.E => (* skip *)
    END
  END
END Route.

