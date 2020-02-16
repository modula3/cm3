(* $Id$ *)

MODULE RouteComponents;
IMPORT MagCell, MagCellExtendable, MagLabelList AS LabelList;
IMPORT MagSession, GridPointSessionTbl;
IMPORT GridPointSet, GridPointSetDef, GridPointSetRoutines;
IMPORT TextMagLayerTbl AS TextLayerTbl;
IMPORT MagLayerRect AS LayerRect;
IMPORT Debug, Text, MagRouteLayer AS RouteLayer, Fmt;
IMPORT Process, ConfPrivate, RouteLayerIntTbl AS RouteLayerTbl;
IMPORT GridPointRectSetTbl, MagRect, RectSet, RectSetSeq, RectBins, GridPoint;
IMPORT Bins;
IMPORT GridPoint_GridPointSetTbl;
IMPORT GridPointList;
IMPORT EntryDB;
FROM MagicStuff IMPORT GridStep, MetalSpacing;
IMPORT GridPointSetSeq, TwoComponents, GridPointSetMST;
IMPORT Compass;
IMPORT MagicStuff;
IMPORT RouteID;
IMPORT Filled;
IMPORT GridPointRouteIDTbl;
IMPORT Wr;
IMPORT ForbiddenSteps;
IMPORT GridPointEntriesTbl;
IMPORT MagLayer, MagLayerRect;

IMPORT MagPointList;
FROM EndPointStatus IMPORT EndPointDir, Dir;
IMPORT EndPointStatus;
IMPORT IO;

IMPORT Env;
IMPORT Thread;
FROM Components IMPORT ComponentIterator, CantConnectThose, GPPair;

VAR DoDebug := Debug.DebugThis("RouteComponents");

VAR AllAssertions := Env.Get("NOROUTERASSERTIONS") = NIL;

TYPE DirSet = SET OF Dir;

EXCEPTION Again;

REVEAL
  Default = DefaultPublic BRANDED Brand OBJECT
    delWr : Wr.T;
    totLengthEstimate := -1000.0d0; (* ridiculous value *)
    myid : RouteID.T;
    (* the wiringSession is all the layout that has to do with long-range
       wiring *)
    wiringSession : MagSession.T := NIL;

    (* the endPtSessionTbl holds the layout needed to get to the actual
       target rects from the grid *)
    endPtSessionTbl : GridPointSessionTbl.T;

    (* stuff... *)

    layerDB : TextLayerTbl.T;
    cell : MagCell.T;
    wiringCell : MagCell.T;
    binnedLayout : RectBins.T;

    (* the endPointCands are the initial endpoint candidates (on externally provided 
       layout) *)
    endPointCands : GridPointSet.T;
    endPointComponents : GridPointSetSeq.T; (* starting components *)

    (* the endPointsActive are the currently active endPoints *)
    endPointsActive : GridPointSet.T;
    
    (* the endPointsRects are layout fragments that are keyed on the 
       initial endpoints *)
    endPointsTargetRects : GridPointRectSetTbl.T;
    allTargetRects : RectSet.T; (* all the target rects, read-only *)

    currentRectsAllComps : RectSet.T; (* all currently-existing rects *)
    
    (* there are N components; each has M_i GridPoints *)
    components : GridPointSetSeq.T;

    (* there are \Sigma M_i GridPoints; each has one of N components *)
    ptCompTbl : GridPoint_GridPointSetTbl.T;

    entryDB : EntryDB.T;

    (* when points are deleted, they are put in the pending set
       until the deletions are committed *)
    pendingDeletions : GridPointList.T := NIL;

    (* this thing is updated stupidly, when the caller needs it... *)
    forbiddenSteps : ForbiddenSteps.T := NIL;

  METHODS
    (* connect pre-existing layout to std. location in given 
       gridpt endPt; nextPt being the next point *)
    
    
    connectEndPt(READONLY endPt, nextPt : GridPoint.T) := ConnectEndPt;

    (* paint link from one to its neighbor *)
    paintStdLink(READONLY p, q : GridPoint.T) := PaintStdLink;

    (* make default connections for everything in gpl *)
    connectListWithDefault(gpl : GridPointList.T);

    mergeEm(s1, s2 : GridPointSet.T; l : GridPointList.T) := MergeEm;
    
    (* get the neighbors of p that are in the set.
       NB. p does NOT need to be in the set itself *)
    connectedNeighbors(READONLY p : GridPoint.T) : GridPointSet.T := ConnectedNeighbors;

    (* starting from from, go away from prev 
       until we hit an endPoint or forkPoint *)
    unravel(READONLY prev, from : GridPoint.T) : GridPointList.T := Unravel;

    (* do all the deletion steps for given pt *)
    doDelete(READONLY p : GridPoint.T; writeToDelWr : BOOLEAN) RAISES { Thread.Alerted, Wr.Failure } := DoDelete;

    (* find a connected component by searching neighbors starting from p *)
    (* p must currently be part of the net *)
    searchComponent(READONLY p : GridPoint.T) : GridPointSet.T := SearchComponent;

    (* get the endpoints connected to endPoint through pre-existing layout;
       includes endPoint itself (the entire connected component, that is) *)
    getConnectedEndPoints(READONLY endPoint : GridPoint.T) : GridPointSet.T := GetConnectedEndPoints;

    (* is p a currently reachable endpoint? *)
    currentlyReachableEndPoint(READONLY target, neighbor : GridPoint.T) : BOOLEAN := CurrentlyReachableEndPoint;

    (* check invariants having to do with component tables *)
    checkComponentInvariants(noSingletons := FALSE;
                             doSearchConsistency := FALSE) := CheckComponentInvariants;

    (* is this pt a member of one of the components? *)
    haveIt(READONLY q : GridPoint.T) : BOOLEAN := HaveIt;

  OVERRIDES

    approxSize := ApproxSize;
    size := Size;

    longest := GetLongestPathInMST;

    pointSet := GetPointSet;
    deleteAPoint := DeleteAPoint;
    deleteAllEndPointNeighbors := DeleteAllEndPointNeighbors;
    init := Init;
    connect := Connect;
    commitRipups := CommitRipups;
    id := GetId;

    nonObstaclesForCurrentRoute := NonObstaclesForCurrentRoute;
    nonEndPointsInNetForCurrentRoute := NonEndPointsInNetForCurrentRoute;
    forbiddenStepsForCurrentRoute := ForbiddenStepsForCurrentRoute;

    deleteAllPoints := DeleteAllPoints;
    iterateComponents := IterateComponents;
  END;

PROCEDURE GetId(t : Default) : RouteID.T = BEGIN RETURN t.myid END GetId;

PROCEDURE Size(t : Default) : CARDINAL =
  BEGIN RETURN t.components.size() END Size;

PROCEDURE DoDelete(t : Default; READONLY p : GridPoint.T; writeToDelWr : BOOLEAN) RAISES { Wr.Failure, Thread.Alerted } =
  VAR
    gps : GridPointSet.T;
    x : BOOLEAN;
    s : MagSession.T;
  BEGIN
    IF t.delWr # NIL AND writeToDelWr THEN
      Wr.PutText(t.delWr, "delete " & Fmt.Int(p.x * GridStep()) & " " & 
                   Fmt.Int(p.y * GridStep())   & " " & Fmt.Int((p.x + 1) * GridStep()) & " " &
                   Fmt.Int((p.y + 1) * GridStep()) & 
                   " [layer " & Fmt.Int(p.l) & "]\n") ;
      Wr.Flush(t.delWr)
    END;

    Debug.Out("DoDelete("&GridPoint.Format(p) & ")");

    t.checkComponentInvariants(doSearchConsistency := FALSE);

    (* if endpoint, don't take it out of components *)
    IF t.endPointCands.member(p) THEN
      x := t.endPointsActive.delete(p);
      <* ASSERT x *>
      (* delete endPoints' layout that is to be deleted *)
      
      x := t.endPtSessionTbl.delete(p,s);
      <* ASSERT x *>
      
      t.wiringCell.rollbackSession(s);
      t.entryDB.flushEntries(p, radius := 1);
      DebugRollbackSession(s);
      t.binnedLayout.flushStaleSession(s)
      
    ELSE
      (* even if it's not an endpoint, it can still interfere with
         endpoint routing in its vicinity *)
      t.entryDB.flushEntries(p, radius := 1);

      x := t.ptCompTbl.delete(p, gps);
      <* ASSERT x *>
    
      x := gps.delete(p);
      <* ASSERT x *>
      Filled.UnMark(p);
    END;

    t.checkComponentInvariants(doSearchConsistency := FALSE)
  END DoDelete;

PROCEDURE Unravel(t : Default; READONLY prev, from : GridPoint.T) : GridPointList.T =
  VAR
    n := t.connectedNeighbors(from);
  BEGIN
    EVAL n.delete(prev);
    
    IF n.size() > 1 THEN
      RETURN GridPointList.List1(from)
    ELSIF t.endPointCands.member(from) THEN
      <* ASSERT t.endPointsActive.member(from) *>
      RETURN GridPointList.List1(from)
    ELSE
      <* ASSERT n.size() = 1 *>
      VAR
        next : GridPoint.T;
        x : BOOLEAN;
      BEGIN
        
        x := n.iterate().next(next); <* ASSERT x *>
        RETURN GridPointList.Cons(from,Unravel(t,from,next))
      END
    END
  END Unravel;


PROCEDURE HaveIt(t : Default; READONLY q : GridPoint.T) : BOOLEAN = 
  BEGIN
    FOR i := 0 TO t.components.size() - 1 DO
      IF t.components.get(i).member(q) THEN RETURN TRUE END
    END;
    RETURN FALSE
  END HaveIt;


(* what points would a point at p be connected to? (p need not be present!) *)
PROCEDURE ConnectedNeighbors(t : Default; READONLY p : GridPoint.T) : GridPointSet.T =

  PROCEDURE ProcCand(READONLY c : GridPoint.T) =
    BEGIN
      (* p and c would be connected iff:
         c is in a component
         NEITHER is an endpoint
   or    c is not an inactive endpoint AND the step is not forbidden *)

      IF NOT t.haveIt(c) THEN RETURN END;

      VAR
        pIsEndCand := t.endPointCands.member(p);
        cIsEndCand := t.endPointCands.member(c);
        pIsActive  := t.endPointsActive.member(p);
        cIsActive  := t.endPointsActive.member(c);
        p2cIsForbidden := t.forbiddenSteps.isForbidden(p,c);
      BEGIN
        
        (* case 1. neither is an end cand.  they must be connected *)
        IF NOT pIsEndCand AND NOT cIsEndCand THEN
          EVAL res.insert(c) 

        (* case 2. p OR c OR both are end candidates; only connected if
           not forbidden ... *)
        ELSIF NOT p2cIsForbidden THEN
          
          (* 2. i. c is end cand, p is not *)
          IF cIsEndCand AND cIsActive AND NOT pIsEndCand THEN
            EVAL res.insert(c)
          (* 2. ii. p is end cand, c is not *)
          ELSIF pIsEndCand AND pIsActive AND NOT cIsEndCand THEN
            EVAL res.insert(c)
          (* 2. iii. c and p both end cands (probably can't happen) *)
          ELSIF pIsEndCand AND cIsEndCand AND pIsActive AND cIsActive THEN
            EVAL res.insert(c)
          END

        END
      END
    END ProcCand;

  VAR
    res := NEW(GridPointSetDef.T).init();
  BEGIN
    
    FOR i := FIRST(Compass.Dir) TO LAST(Compass.Dir) DO
      ProcCand(GridPoint.T { p.x + Compass.Step[i].x,
                                    p.y + Compass.Step[i].y,
                                    p.l })
    END;
    IF p.l > FIRST(GridPoint.Layer) THEN
      ProcCand( GridPoint.T { p.x, p.y, p.l - 1 })
    END;
    IF p.l < LAST(GridPoint.Layer) THEN
      ProcCand( GridPoint.T { p.x, p.y, p.l + 1 })
    END;
    RETURN res
  END ConnectedNeighbors;

PROCEDURE GetLongestPathInMST(t : Default) : TwoComponents.T =
  
  PROCEDURE Cleanup(s : GridPointSet.T) : GridPointSet.T =
    VAR
      res := NEW(GridPointSetDef.T).init();
      iter := s.iterate();
      gp : GridPoint.T;
      x : BOOLEAN;
      endPtRects : RectSet.T;
    BEGIN
      WHILE iter.next(gp) DO
        IF t.endPointCands.member(gp) THEN
          x := t.endPointsTargetRects.get(gp,endPtRects);
          <* ASSERT x *>
          IF t.entryDB.someEntryOK(gp,t,endPtRects,t.currentRectsAllComps) THEN
            EVAL res.insert(gp)
          END
        ELSE
          EVAL res.insert(gp) (* all others are always reachable, right? *)
        END
      END;
      RETURN res
    END Cleanup;

  VAR
    mst : GridPointSetMST.T;
    s1, s2 : GridPointSet.T;

    len : LONGREAL;
  BEGIN
    t.checkComponentInvariants(noSingletons := TRUE);
    mst := NEW(GridPointSetMST.T).init(t.components);    
    mst.getLink(0,s1,s2,len);


    (* clean up s1 and s2 so that they contain only CURRENTLY REACHABLE 
       endPoints *)

    s1 := Cleanup(s1);
    s2 := Cleanup(s2);

    RETURN NEW(TwoComponents.T).init(s1,s2,len)
  END GetLongestPathInMST;

PROCEDURE SearchComponent(t : Default; READONLY p : GridPoint.T) : GridPointSet.T =

  PROCEDURE Recurse(READONLY q : GridPoint.T) =
    BEGIN
      IF NOT res.member(q) THEN
        EVAL res.insert(q);
        
        VAR 
          n := t.connectedNeighbors(q);
          iter : GridPointSet.Iterator;
          r : GridPoint.T;
          s : GridPointSet.T;
          x : BOOLEAN;
        BEGIN

          IF t.endPointCands.member(q) THEN
            (* add all the other candidates that are connected through
               pre-existing layout... *)
            s := t.getConnectedEndPoints(q);

            x := s.delete(q);  <* ASSERT x *>
            
            n := n.unionD(s)
          END;

          iter := n.iterate();
          WHILE iter.next(r) DO 

            IF AllAssertions THEN
              (* DEBUGGING *)
              IF t.pointSet().member(q) AND 
                (NOT t.endPointCands.member(q) OR t.endPointsActive.member(q)) 
               THEN
                VAR
                  m := t.connectedNeighbors(r);
                BEGIN
                  IF t.endPointCands.member(r) THEN
                    m := m.unionD(t.getConnectedEndPoints(r));
                    EVAL m.delete(r)
                  END;
                  <* ASSERT m.member(q) *>
                END
              END;
            END;

            (* do work *)
            Recurse(r) 
          END
        END
      END
    END Recurse;

  VAR
    res := NEW(GridPointSetDef.T).init();
  BEGIN
    Recurse(p);
    RETURN res
  END SearchComponent;

PROCEDURE DeleteAllEndPointNeighbors(t : Default; ep : GridPoint.T) =
(* delete all the non-endpoints neighboring an endpoint;
   call commitripups after this *)
  VAR
    iter := t.connectedNeighbors(ep).iterate();
    p : GridPoint.T;
  BEGIN
    <* ASSERT t.endPointCands.member(ep) *>
    <* ASSERT t.endPointsActive.member(ep) *>
    WHILE iter.next(p) DO
      IF NOT t.endPointCands.member(p) THEN
        t.deleteAPoint(p)
      END
    END
  END DeleteAllEndPointNeighbors;

(* commit pending ripups *)
PROCEDURE CommitRipups(t : Default; writeToDelWr : BOOLEAN) : GridPointSet.T RAISES { Thread.Alerted, Wr.Failure } =
  VAR
    deleted := NEW(GridPointSetDef.T).init();
    p : GridPoint.T;
    caNeighbors : GridPointSet.T;
    touchedButNotDeletedEndPoints := NEW(GridPointSetDef.T).init();
  BEGIN
    t.checkComponentInvariants(noSingletons := TRUE);

    WHILE t.pendingDeletions # NIL DO
      caNeighbors := NIL;  (* just for error checking *)

      (* take first point off the list *)
      p := t.pendingDeletions.head;
      t.pendingDeletions := t.pendingDeletions.tail;

      IF NOT deleted.member(p) THEN
        
        Debug.Out("About to delete " & GridPoint.Format(p));

        Debug.Out("EndPointCands are:");
        VAR
          iter := t.endPointCands.iterate();
          c : GridPoint.T;
        BEGIN
          WHILE iter.next(c) DO Debug.Out(GridPoint.Format(c)) END
        END;
            

        Debug.Out("SearchComponent(p="&GridPoint.Format(p)&") =  " &
          GridPointSetRoutines.Format(t.searchComponent(p)));

        (* ---------- DELETE p ---------- *)
        t.doDelete(p, writeToDelWr);
        EVAL deleted.insert(p);

        (* break component that used to contain p up into one for each
           neighbor of p *)
        VAR
          iter : GridPointSet.Iterator;
          q : GridPoint.T;
          ca : REF ARRAY OF GridPointSet.T;
          i := 0;
          oldComponent, newPts : GridPointSet.T;
          x : BOOLEAN;
          union := NEW(GridPointSetDef.T).init(); (* debugging *)
        BEGIN
          newPts := t.connectedNeighbors(p);
          caNeighbors := t.connectedNeighbors(p);
          iter := newPts.iterate();
          ca := NEW(REF ARRAY OF GridPointSet.T, newPts.size());


          WHILE iter.next(q) DO
            ca[i] := t.searchComponent(q);

            Debug.Out(GridPoint.Format(q) & " -> ca[" & Fmt.Int(i) & "]:");
            Debug.Out(GridPointSetRoutines.Format(ca[i]));
            Debug.Out("\n");

            INC(i);
          END;

          (* if things are "overly connected," two or more of the components
             could be the same... *)
          VAR
            merged : BOOLEAN;
          BEGIN
            REPEAT
              merged := FALSE;
              TRY
                FOR i := 0 TO LAST(ca^) DO
                  FOR j := i + 1 TO LAST(ca^) DO
                    IF ca[i].intersection(ca[j]).size() > 0 THEN
                      <* ASSERT ca[i].equal(ca[j]) *>
                      Debug.Out("ca["&Fmt.Int(i)&"] and ca["&Fmt.Int(j)&"] the same: MERGING!");
                      merged := TRUE;
                    
                      (* swap out ca[j] *)
                      VAR
                        new := NEW(REF ARRAY OF GridPointSet.T, NUMBER(ca^)-1);
                        end := NUMBER(new^)-j;
                      BEGIN
                        SUBARRAY(new^,0,j) := SUBARRAY(ca^,0,j);
                        SUBARRAY(new^,j,end) := SUBARRAY(ca^,j+1,end);
                        ca := new;
                        RAISE Again
                      END
                    END
                  END
                END
              EXCEPT 
                Again => (* skip *)
              END
            UNTIL NOT merged
          END;

          (* debugging ... *)
          FOR i := 0 TO LAST(ca^) DO
            FOR j := i + 1 TO LAST(ca^) DO
              VAR
                n := ca[i].intersection(ca[j]);
              BEGIN
                <* ASSERT n.size() = 0 *>
              END
            END;
            
            union := union.unionD(ca[i])
          END;


          (* replace old component with ca[0] in comp. sequence *)
          x := t.ptCompTbl.get(q,oldComponent); <* ASSERT x *>

          Debug.Out("\noldComponent : " & GridPointSetRoutines.Format(oldComponent));

          (* p has already been deleted from oldComponent *)
          <* ASSERT NOT oldComponent.member(p) *>

          (* should therefore have the same points in the two... *)
          VAR
            i1 := union.iterate();
            i2 := oldComponent.iterate();
            ppp : GridPoint.T;
          BEGIN
            WHILE i1.next(ppp) DO <* ASSERT oldComponent.member(ppp) *> END;
            WHILE i2.next(ppp) DO <* ASSERT union.member(ppp) *> END
          END;
          
          (* check that the endpoints come along nicely *)
          VAR
            ui := union.intersection(t.endPointCands);
            oi := oldComponent.intersection(t.endPointCands);
          BEGIN
            <* ASSERT ui.equal(oi) *>
          END;

          (* delete all the points from the ptCompTbl *)
          VAR
            iter := oldComponent.iterate();
            dummy : GridPointSet.T;
            x : BOOLEAN;
          BEGIN
            WHILE iter.next(q) DO
              x := t.ptCompTbl.delete(q,dummy); <* ASSERT x *>
              <* ASSERT dummy = oldComponent *>
            END
          END;
          
          x := FALSE;
          FOR i := 0 TO t.components.size() DO
            IF t.components.get(i) = oldComponent THEN
              t.components.put(i,ca[0]); x := TRUE; EXIT
            END
          END;
          <* ASSERT x *>
          
          FOR i := 1 TO LAST(ca^) DO
            t.components.addhi(ca[i])
          END;

          (* and update the points *)
          FOR i := 0 TO LAST(ca^) DO
            VAR
              iter := ca[i].iterate();
            BEGIN
              WHILE iter.next(q) DO
                x := t.ptCompTbl.put(q,ca[i]); <* ASSERT NOT x *>
              END
            END
          END;

          t.checkComponentInvariants()
          
        END;
        

        (* start deleting in all directions from p until we hit an
           endPoint or forkPoint *)
        VAR
          iter := caNeighbors.iterate();
          q : GridPoint.T;
          r : GridPointList.T;
        BEGIN
          WHILE iter.next(q) DO
            IF deleted.member(q) THEN
              Debug.Out("Was going to unravel from " & GridPoint.Format(q) & 
                ", but we already deleted it....!")

            (* weird stuff can happen in close quarters... we may wind up
               with something that doesn't unravel straight but yet contains
               only a single endpoint *)
            ELSIF t.searchComponent(q).intersection(t.endPointsActive).size() <= 1 THEN
              (* blow away the whole component *)
              VAR
                iter := t.searchComponent(q).iterate();
                ppp : GridPoint.T;
              BEGIN
                WHILE iter.next(ppp) DO
                  IF NOT t.endPointCands.member(ppp) OR 
                     t.endPointsActive.member(ppp) THEN 
                    t.doDelete(ppp, writeToDelWr); 
                    EVAL touchedButNotDeletedEndPoints.delete(ppp);
                    EVAL deleted.insert(ppp)
                  END;
                END
              END
            ELSE
              Debug.Out("Unravelling from " & GridPoint.Format(q) & "..." );
              
              r := t.unravel(p,q);
              (* now delete (from components or from active endpoints, as 
                 applicable) all points in r except the last one if it is
                 a forkPoint *)
              WHILE r.tail # NIL DO
                t.doDelete(r.head,writeToDelWr);
                EVAL deleted.insert(r.head);
                r := r.tail
              END;
              
              (* don't delete fork points;
                 do "delete" endpoints---but that really only removes
                 the special layout, not the point from the component *)
              IF t.connectedNeighbors(r.head).size() = 0 THEN
                t.doDelete(r.head, writeToDelWr);
                EVAL touchedButNotDeletedEndPoints.delete(r.head);
                EVAL deleted.insert(r.head)
              ELSIF t.endPointCands.member(r.head) THEN
                EVAL touchedButNotDeletedEndPoints.insert(r.head)
              END
            END
          END            
        END
      END
    END;

    (* clean out any components of size 0 *)
    VAR 
      i := 0; 
    BEGIN
      (* if any component is of size 0, remove the last comp and put it
         in the size 0's place *)
      WHILE i < t.components.size()-1 DO
        IF t.components.get(i).size() = 0 THEN
          t.components.put(i,t.components.remhi())
        END;
        INC(i)
      END;

      (* well, except the last one, just remove it if it is size 0... *)
      IF t.components.get(t.components.size()-1).size() = 0 THEN
        EVAL t.components.remhi()
      END
    END;

    t.checkComponentInvariants(noSingletons := TRUE,
                               doSearchConsistency := TRUE);

    (* delete all standard layout *)

    t.wiringCell.rollbackSession(t.wiringSession);
    DebugRollbackSession(t.wiringSession);

    IF DoDebug THEN t.wiringCell.debugDumpData() END;

    t.binnedLayout.flushStaleSession(t.wiringSession);

    (* current rects revert to: original target rects PLUS rects for
       each active endPoint *)
    t.currentRectsAllComps := t.allTargetRects.copy();
    VAR
      iter := t.endPtSessionTbl.iterate();
      p : GridPoint.T;
      s : MagSession.T;
    BEGIN
      WHILE iter.next(p,s) DO
        EVAL t.currentRectsAllComps.unionD(s.rectSet())
      END
    END;

    (* REPAINT all the standard layout *)
    t.wiringSession := t.wiringCell.newSession();

    (* we paint the non-endpoints and let the endpoints take care of
       themselves (I think this works, only case that might be screwed up
       is if a point can have an endpoint as a neighbor without having been
       connected to it... Hmmm... can that even happen?) *)
    VAR
      iter := t.pointSet().diff(t.endPointCands).iterate();
      gp : GridPoint.T;
      painted := NEW(GridPointSetDef.T).init();
    BEGIN
      WHILE iter.next(gp) DO
        EVAL painted.insert(gp);

        (* and paint links to neighbors *)
        VAR
          jter := t.connectedNeighbors(gp).diff(painted).iterate();
          q : GridPoint.T;
        BEGIN
          WHILE jter.next(q) DO 
            IF NOT t.endPointCands.member(q) OR
              EndPointDir(gp,q) IN DirSet { Dir.N, Dir.E, Dir.D, Dir.U } THEN
              t.paintStdLink(gp,q) 
            END
          END
        END
      END
    END;

    (* fix up endpoints' layout *)
    VAR
      iter := touchedButNotDeletedEndPoints.iterate();
      p : GridPoint.T;
      s : MagSession.T;
      x : BOOLEAN;
    BEGIN
      WHILE iter.next(p) DO
        x := t.endPtSessionTbl.delete(p,s);
        <* ASSERT x *>
        
        Debug.Out("Deleting connect session for touched-but-not-deleted e.p. "&
          GridPoint.Format(p));

        IF DoDebug THEN t.wiringCell.debugDumpData() END;
        t.wiringCell.rollbackSession(s);
        IF DoDebug THEN t.wiringCell.debugDumpData() END;

        t.entryDB.flushEntries(p, radius := 1);
        DebugRollbackSession(s);
        t.binnedLayout.flushStaleSession(s);
        
        VAR
          jter := t.connectedNeighbors(p).iterate();
          q : GridPoint.T;
        BEGIN
          WHILE jter.next(q) DO
            t.connectEndPt(p,q)
          END
        END
      END
    END;
    t.checkComponentInvariants(noSingletons := TRUE);
    RETURN NIL
  END CommitRipups;

PROCEDURE GetPointSet(t : Default) : GridPointSet.T =
  VAR 
    s := NEW(GridPointSetDef.T).init();
  BEGIN
    FOR i := 0 TO t.components.size() - 1 DO
      EVAL s.unionD(t.components.get(i))
    END;
    RETURN s
  END GetPointSet;

PROCEDURE MergeEm(t : Default; s1, s2 : GridPointSet.T; ll : GridPointList.T) =
  VAR
    newSet := s1.union(s2);
    l := ll;
  BEGIN
    WHILE l # NIL DO EVAL newSet.insert(l.head); l := l.tail END;

    Debug.Out("MergeEm: newSet = " & GridPointSetRoutines.Format(newSet));

    (* update components *)
    VAR
      n := NEW(GridPointSetSeq.T).init();
    BEGIN
      FOR i := 0 TO t.components.size() - 1 DO
        VAR 
          c := t.components.get(i);
        BEGIN
          IF c # s1 AND c # s2 THEN
            n.addhi(c)
          END
        END
      END;
      n.addhi(newSet);
      t.components := n
    END;

    (* update points lookup table *)
    VAR 
      iter := newSet.iterate();
      gp : GridPoint.T;
    BEGIN
      WHILE iter.next(gp) DO
        EVAL t.ptCompTbl.put(gp,newSet)
      END
    END;

    t.checkComponentInvariants(doSearchConsistency := FALSE)

  END MergeEm;

PROCEDURE Connect(t : Default; gpl : GridPointList.T) : GridPointList.T RAISES { CantConnectThose } =
  VAR
    hComp, tComp : GridPointSet.T := NIL;
    headPt := gpl.head;
    headNextPt := gpl.tail.head;
    tailPt := GridPointList.Nth(gpl,GridPointList.Length(gpl)-1);
    tailPrevPt := GridPointList.Nth(gpl,GridPointList.Length(gpl)-2);

  <*UNUSED*>
  VAR
    debugGPL := gpl; (* print debugGPL from debugger if you want to*)
  BEGIN

    (***********************************************************************)
    (***********************************************************************)
    (***************** CHECK SOME ASSERTIONS *******************************)
    (***********************************************************************)
    (***********************************************************************)
    
    t.checkComponentInvariants();
    (* get first and last points and their components *)
    EVAL t.ptCompTbl.get(headPt,hComp);
    EVAL t.ptCompTbl.get(tailPt,tComp);


    IF AllAssertions THEN
      (* first, headPt and tailPt must already be in the net... *)
      <* ASSERT hComp # NIL *>
      <* ASSERT tComp # NIL *>
      
      (* ...and they must not already be connected *)
      <* ASSERT hComp.intersection(tComp).size() = 0 *>
    END;
    
    (***********************************************************************)
    (***********************************************************************)
    (***************** PICK ACTUAL STARTING AND ENDING POINTS **************)
    (***********************************************************************)
    (***********************************************************************)
    
    (* we need to check for a route that doubles back on itself... 
       we keep that part of the route that goes from the LAST member
       of hComp till the first member of a component that's not hComp
    *)
    VAR
      pp := gpl;
      comp : GridPointSet.T;
    BEGIN
      WHILE pp # NIL DO
        IF t.ptCompTbl.get(pp.head,comp) AND comp = hComp THEN
          gpl := pp;
          EXIT
        END;
        pp := pp.tail
      END;

      WHILE pp # NIL DO
        IF t.ptCompTbl.get(pp.head,comp) AND comp # hComp THEN
          pp.tail := NIL
        END;
        pp := pp.tail
      END
    END;

    (***********************************************************************)
    (***********************************************************************)
    (***************** RECALCULATE HEADPT, TAILPT, AND FRIENDS *************)
    (***********************************************************************)
    (***********************************************************************)

    headPt := gpl.head;
    headNextPt := gpl.tail.head;
    tailPt := GridPointList.Nth(gpl,GridPointList.Length(gpl)-1);
    tailPrevPt := GridPointList.Nth(gpl,GridPointList.Length(gpl)-2);

    EVAL t.ptCompTbl.get(headPt,hComp);
    EVAL t.ptCompTbl.get(tailPt,tComp);


    (***********************************************************************)
    (***********************************************************************)
    (***************** CHECK SOME ASSERTIONS (AGAIN) ***********************)
    (***********************************************************************)
    (***********************************************************************)
    
    IF AllAssertions THEN
      (* first, headPt and tailPt must already be in the net... *)
      <* ASSERT hComp # NIL *>
      <* ASSERT tComp # NIL *>
      
      (* ...and they must not already be connected *)
      <* ASSERT hComp.intersection(tComp).size() = 0 *>
    END;
    

    IF AllAssertions THEN

      (* no connecting through endPointCands... *)
      (* only first and last can be endPointCands... *)
      (* no already-connected things should be in the path *)
      VAR
        pp := gpl.tail;
      BEGIN
        WHILE pp.tail # NIL DO
          <* ASSERT NOT t.endPointCands.member(pp.head) *>
          <* ASSERT NOT t.haveIt(pp.head) *>
          pp := pp.tail
        END
      END
    END;

    IF t.endPointCands.member(headPt) AND NOT t.currentlyReachableEndPoint(headPt,headNextPt) THEN
        RAISE CantConnectThose(GPPair { headPt, headNextPt })
    END;

    IF t.endPointCands.member(tailPt) AND NOT t.currentlyReachableEndPoint(tailPt,tailPrevPt) THEN
      RAISE CantConnectThose(GPPair { tailPt, tailPrevPt })
    END;

    
    (* DONE CHECKING ASSERTIONS---MAKE ACTUAL MODIFICATIONS! *)

    IF t.currentlyReachableEndPoint(headPt,headNextPt) THEN 
      t.connectEndPt(headPt, headNextPt);
      EVAL t.endPointsActive.insert(headPt)
    END;

    IF t.currentlyReachableEndPoint(tailPt,tailPrevPt) THEN 
      t.connectEndPt(tailPt, tailPrevPt);       
      EVAL t.endPointsActive.insert(tailPt)
    END;

    (* for the following few bits of code, assume we are connecting
       points 1 2 3 4 5, in that order... *)
    (* if we have points 1 2 3 4 5, make links
       2-3, 3-4 ... *)

    VAR
      pp := gpl.tail;
    BEGIN
      WHILE pp # NIL AND pp.tail # NIL AND pp.tail.tail # NIL DO
        t.paintStdLink(pp.head,pp.tail.head);
        pp := pp.tail
      END
    END;

    (* if we have at least three points in the gpl, we may have to do
       special things to connect to the endpoints *)
    (* if the endpoints aren't going to pre-existing layout, we have to
       route to the standard location in those GridPoints.

       if they ARE going to existing layout, then:
       (1) if only two points, we don't need to do anything (they will
           route properly by themselves)
       (2) if more than two points, then we have to add layout if we 
           are exiting the headNextPt or tailPrevPt to the north or east or
           up or down *)
    IF gpl.tail.tail # NIL THEN
      IF NOT t.endPointCands.member(headPt) OR
        EndPointDir(headNextPt,headPt) IN DirSet { Dir.N, Dir.E, Dir.D, Dir.U } THEN
        t.paintStdLink(headPt,headNextPt)
      END;
      
      IF NOT t.endPointCands.member(tailPt) OR 
        EndPointDir(tailPrevPt,tailPt) IN DirSet { Dir.N, Dir.E, Dir.D, Dir.U }THEN
        t.paintStdLink(tailPrevPt,tailPt)
      END
    END;

    t.checkComponentInvariants(doSearchConsistency := FALSE);
    (* merge everything together *)
    (*t.mergeEm(hComp, tComp, gpl);*)
    VAR
      newSet := hComp.union(tComp);
      l := gpl;
    BEGIN
      WHILE l # NIL DO EVAL newSet.insert(l.head); l := l.tail END;
      
      Debug.Out("Connect/MergeEm: newSet = " & GridPointSetRoutines.Format(newSet));
      
      (* update components *)
      VAR
        n := NEW(GridPointSetSeq.T).init();
      BEGIN
        FOR i := 0 TO t.components.size() - 1 DO
          VAR 
            c := t.components.get(i);
          BEGIN
            IF c # hComp AND c # tComp THEN
              n.addhi(c)
            END
          END
        END;
        n.addhi(newSet);
        t.components := n
      END;
      
      (* update points lookup table *)
      VAR 
        iter := newSet.iterate();
        gp : GridPoint.T;
      BEGIN
        WHILE iter.next(gp) DO
          EVAL t.ptCompTbl.put(gp,newSet)
        END
      END;

      t.checkComponentInvariants(doSearchConsistency := FALSE)
    END;

    (* we should see if we've pulled any other components (by coming near
       them) and if so merge them in too *)
    
    VAR 
      s := t.searchComponent(headPt);
      n := NEW(GridPointSetSeq.T).init();
      ptComponent : GridPointSet.T;
      x := t.ptCompTbl.get(headPt,ptComponent);
    BEGIN
      <* ASSERT x *>
      Debug.Out("Connect/Spurious merge: s = " & GridPointSetRoutines.Format(s));
      IF s.size() # ptComponent.size() THEN
        Debug.Out("Connect: s # newSet!!! CORRECTING!!!");
        FOR i := 0 TO t.components.size() - 1 DO
          VAR 
            c := t.components.get(i);
          BEGIN
            IF c.intersection(s).size() = 0 THEN
              n.addhi(c)
            ELSE
              <* ASSERT c.diff(s).size() = 0 *> (* s includes c *)
            END
          END
        END;
        n.addhi(s);
        t.components := n;

        (* update points lookup table *)
        VAR 
          iter := s.iterate();
          gp : GridPoint.T;
        BEGIN
          WHILE iter.next(gp) DO
            EVAL t.ptCompTbl.put(gp,s)
          END
        END
      END
    END;

    t.checkComponentInvariants();
    RETURN gpl
  END Connect;

PROCEDURE DeleteAPoint(t : Default; gp : GridPoint.T) =
  BEGIN 
    <* ASSERT t.pointSet().member(gp) *>
    t.pendingDeletions := GridPointList.Cons(gp,t.pendingDeletions)
  END DeleteAPoint;

PROCEDURE Init(t : Default; 
               initLayout : MagCell.T; 
               emptyWiringCell : MagCell.T;
               targets : LabelList.T;
               layerDB : TextLayerTbl.T;
               entryDB : EntryDB.T;
               binnedLayout : RectBins.T;
               specialEndPtsTbl : GridPointRouteIDTbl.T;
               delWr : Wr.T;
               gridPointEntries : GridPointEntriesTbl.T
               ) : T =
  VAR
    rectSetSeq : RectSetSeq.T;
  BEGIN

    t.delWr := delWr;
    t.cell := initLayout;
    t.wiringCell := emptyWiringCell;
    t.endPtSessionTbl :=  NEW(GridPointSessionTbl.Default).init();
    t.binnedLayout := binnedLayout;
    t.entryDB := entryDB;
    t.layerDB := layerDB;
    t.endPointsActive  := NEW(GridPointSetDef.T).init();
    t.endPointCands  := NEW(GridPointSetDef.T).init();
    t.allTargetRects := NEW(RectSet.T).init();

    t.wiringSession := t.wiringCell.newSession();


    t.endPointsTargetRects := NEW(GridPointRectSetTbl.Default).init();
    t.components := NEW(GridPointSetSeq.T).init();
    t.endPointComponents := NEW(GridPointSetSeq.T).init();
    t.ptCompTbl := NEW(GridPoint_GridPointSetTbl.Default).init();

    t.myid := RouteID.New();

    rectSetSeq := LabelListToRectSetSeq(t.binnedLayout, t.layerDB,targets,
                                        warmUpBins := TRUE);

    (* we have rectSetSeq.size() initial components *)
    (* the rectSetSeq gives us some initial starting points *)

    (* union all the rects first (we will need this below) *)
    FOR i := 0 TO rectSetSeq.size() - 1 DO
      EVAL t.allTargetRects.unionD(rectSetSeq.get(i))
    END;
    t.currentRectsAllComps := t.allTargetRects.copy();

    (* we really ought to fight with other nets for endpoints here... *)
    FOR i := 0 TO rectSetSeq.size() - 1 DO
      VAR
        rs := rectSetSeq.get(i);
        cands := GetCandidateEndpoints(rs);
        iter : GridPointSet.Iterator;
        gp : GridPoint.T;
      BEGIN
        (* print warning if the rectSetSeq can't be reached *)

        IF cands.size() = 0 THEN
          Debug.Warning("NO REACHABLE LAYOUT FOR ROUTING NET CONSISTING OF:");
          VAR
            iter := rs.iterate();
            r : LayerRect.T;
          BEGIN
            WHILE iter.next(r) DO
              Debug.Warning("MEMBER RECT " & MagRect.Format(r.rect) & " (" & NARROW(r.layer, RouteLayer.T).name & ")")
            END
          END
        END;

        (* initialize components *)
        t.components.addhi(cands);

        (* remember pertinent details about endpoints *)


        (* delete those endpoints that have NO entries... 

           we do this for two reasons: first of all, why remember points 
           that could never be used?  secondly, the points that are in 
           endPointsCands are going to be used to build forbiddenSteps,
           and we don't want to make forbidden steps with irrelevant
           gridpoints....

        *)
        
        VAR
          iter := cands.iterate();
          bad : GridPointList.T := NIL;
          p : GridPoint.T;
        BEGIN
          WHILE iter.next(p) DO
            IF NOT t.entryDB.someEntryOK(p,t,rs,t.currentRectsAllComps) THEN
              bad := GridPointList.Cons(p,bad)
            END
          END;
          
          WHILE bad # NIL DO
            EVAL cands.delete(bad.head); bad := bad.tail
          END
        END;
        
        (* first, remember that they ARE endpoints *)
        EVAL t.endPointCands.unionD(cands);

        (* secondly, remember which rects go where *)
        iter := cands.iterate();
        WHILE iter.next(gp) DO
          EVAL t.endPointsTargetRects.put(gp,rs);
          EVAL t.ptCompTbl.put(gp,cands);
        END;

        IF gridPointEntries # NIL THEN
          VAR 
            iter := cands.iterate();
          BEGIN
            WHILE iter.next(gp) DO
              EVAL gridPointEntries.put(gp, t.entryDB.entries(gp,t,rs,t.currentRectsAllComps))
            END
          END
        END;

        (* now remember special gridpoints---gridpoints that are 
           (1) the ONLY legal entry into a given component;
           (2) on metal1
           (3) are blocked everywhere except a top entry
        *)
        IF specialEndPtsTbl # NIL THEN
          VAR
            pts := 0;
          BEGIN
            iter := cands.iterate();
            WHILE iter.next(gp) DO
              IF t.entryDB.someEntryOK(gp,t,rs,
                         t.currentRectsAllComps) THEN
                INC(pts)
              END
            END;
            IF pts = 1 THEN
              VAR
                entries := t.entryDB.entriesOK(gp,t,rs,t.currentRectsAllComps);
              BEGIN
                IF gp.l = 1 AND 
                  entries[Dir.N] = FALSE AND
                  entries[Dir.E] = FALSE AND 
                  entries[Dir.W] = FALSE AND 
                  entries[Dir.S] = FALSE THEN
                    EVAL specialEndPtsTbl.put(gp,t.myid)
                END
              END
            END
          END
        END

      END
    END;

    FOR i := 0 TO t.components.size() - 1 DO
      t.endPointComponents.addhi(t.components.get(i).copy())
    END;
    
    (* compute an estimate for length *)
    t.totLengthEstimate := 
        NEW(GridPointSetMST.T).init(t.components).totalWeight();

    InitForbiddenSteps(t);

    (* debugging *)
    Debug.Out("Initializing set for label \"" & targets.head.name & "\"...");
    FOR i := 0 TO t.endPointComponents.size() - 1 DO
      Debug.Out("RouteComponents.Init: endPointComponent[" & Fmt.Int(i) & 
        "]: " & GridPointSetRoutines.Format(t.endPointComponents.get(i)))
    END;

    t.checkComponentInvariants();
    RETURN t
  END Init;

PROCEDURE ApproxSize(t : Default) : LONGREAL =
  BEGIN RETURN t.totLengthEstimate END ApproxSize;

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

PROCEDURE GetCandidateEndpoints(rs : RectSet.T) : GridPointSet.T =
  VAR
    iter := rs.iterate();
    cands := NEW(GridPointSetDef.T).init();
    rect : LayerRect.T;
  BEGIN
    WHILE iter.next(rect) DO
      (* compute candidate points *)
      IF DoDebug THEN
        Debug.Out("RouteComponents.GetCandidateEndpoints: Checking rect " & 
          MagRect.Format(rect.rect) & " layer: " & 
          NARROW(rect.layer,RouteLayer.T).name)
      END;

      (* we should compute a candidate GridPointList directly instead *)
      (* this would let us do it 3D; this would be much faster. *)
      VAR 
        pairList := Bins.Compute(GridStep(), Bloat(rect.rect));
        rlIter : RouteLayerTbl.Iterator;
        rLayer : RouteLayer.T;
        rLevel : INTEGER;
      BEGIN
        WHILE pairList # NIL DO
          rlIter := ConfPrivate.RouteLayerIterate();
          WHILE rlIter.next(rLayer,rLevel) DO
            IF rLayer.connects.member(rect.layer) THEN
              VAR
                gp := GridPoint.T { pairList.head.k1, 
                                    pairList.head.k2, 
                                    rLevel };
              BEGIN
                EVAL cands.insert(gp)
              END
            END
          END;
          pairList := pairList.tail
        END
      END
    END;
    RETURN cands
  END GetCandidateEndpoints;

PROCEDURE LabelListToRectSetSeq(binnedLayout : RectBins.T;
                                layerDB : TextLayerTbl.T;
                                labels : LabelList.T;
                                warmUpBins : BOOLEAN) : RectSetSeq.T =
  VAR
    res := NEW(RectSetSeq.T).init();
    seedRect : LayerRect.T;
  BEGIN

    IF warmUpBins THEN
      (* warm up rectbins... *)
      VAR
        set := NEW(RectSet.T).init();
        p := labels;
        l : MagLayer.T;
      BEGIN
        WHILE p # NIL DO
          IF layerDB.get(p.head.layer, l) THEN
            EVAL set.insert(MagLayerRect.T { p.head.rect, l})
          END;
          p := p.tail
        END;
        binnedLayout.advise(set)
      END
    END;
    
    IF DoDebug THEN Debug.Out("LabelListToRectSetSeq CALLED") END;
    WHILE labels # NIL DO

      VAR
        lab := labels.head;
        gotIt := FALSE;
      BEGIN
        (* find a seed rectangle---i.e., one that touches the label *)
        IF Text.Equal(lab.layer, "space") THEN
          Process.Crash("GridRouterMain.LabelListToRectSetSeq: attempting to "&
            "route to a label on SPACE: \"" & lab.name & "\" at " & 
            MagRect.Format(lab.rect))
        END;
        TRY
          seedRect := binnedLayout.getATouchingRect(layerDB, lab)
        EXCEPT
          RectBins.NoConnectedRects =>
            Process.Crash("Trouble finding seed rectangle for label @ " &
              MagRect.Format(lab.rect) & " on layer \"" & lab.layer & "\" called \"" & lab.name & "\"!")
        END;
        
        IF DoDebug THEN
          Debug.Out("LabelListToRectSetSeq: seedRect @ " & MagRect.Format(seedRect.rect) & " on " & NARROW(seedRect.layer,RouteLayer.T).name)
        END;

        (* Scan the existing Sequence to see if we already have the seed *)
        FOR i := 0 TO res.size() - 1 DO
          IF res.get(i).member(seedRect) THEN 
            IF DoDebug THEN
              Debug.Out("LabelListToRectSetSeq: already had " & 
                MagRect.Format(seedRect.rect) & " on " & 
                NARROW(seedRect.layer,RouteLayer.T).name)
            END;

            gotIt := TRUE; 
            EXIT 
          END
        END;

        (* nope, compute closure and add to Sequence *)
        IF NOT gotIt THEN
          IF DoDebug THEN
            Debug.Out("Closure #" & Fmt.Int(res.size()))
          END;

          VAR 
            closure := binnedLayout.getConnectedClosure(seedRect);
          BEGIN
            res.addhi(closure);
            IF DoDebug THEN
              VAR
                iter := closure.iterate();
                lr : MagLayerRect.T;
              BEGIN
                WHILE iter.next(lr) DO
                  Debug.Out("Adding " & MagRect.Format(lr.rect) & " on " & 
                    NARROW(lr.layer,RouteLayer.T).name)
                END
              END
            END
          END
        END
      END;

      labels := labels.tail
    END;
    IF DoDebug THEN Debug.Out("LabelListToRectSetSeq RETURNING") END;
    RETURN res
  END LabelListToRectSetSeq;

PROCEDURE PaintStdLink(t : Default; READONLY p, q : GridPoint.T) =
  VAR
    rSet := MagicStuff.ConnRects(p,q);
  BEGIN
    Debug.Out("PaintStdLink("&GridPoint.Format(p)&" -> "&GridPoint.Format(q)&")");
    EVAL t.currentRectsAllComps.unionD(rSet);
    t.wiringCell.sessAddLayerRectSet(t.wiringSession,rSet);
    t.entryDB.flushEntries(p, radius := 1);
    t.entryDB.flushEntries(q, radius := 1);
    DebugAddRectSet(rSet);
    t.binnedLayout.addRectSet(rSet)
  END PaintStdLink;

PROCEDURE ForbiddenStepsForCurrentRoute(t : Default) : ForbiddenSteps.T =
  BEGIN
    RETURN t.forbiddenSteps 
  END ForbiddenStepsForCurrentRoute;

PROCEDURE InitForbiddenSteps(t : Default) =
  VAR
    res := NEW(ForbiddenSteps.T).init();
    iter := t.endPointCands.iterate();
    p : GridPoint.T;
  BEGIN
    (* steps into and out of endpoints that aren't connecting are forbidden! *)
    WHILE iter.next(p) DO
      VAR
        endPtRects : RectSet.T;
        x : BOOLEAN;
        entriesOK : EndPointStatus.T;
      BEGIN
        x := t.endPointsTargetRects.get(p,endPtRects);
        <* ASSERT x *>
        entriesOK := t.entryDB.entriesOK(p,t, 
                                         endPtRects, 
                                         t.currentRectsAllComps);

        (* don't know about this, but I think we only need to record
           forbiddenSteps for points that have ANY entries.  If no entries,
           then they aren't going to be targets in the first place! And
           forbiddenSteps is only sensible for endpoints, not intermediate
           points, anyhow... *)
        IF t.endPointCands.member(p) THEN
          FOR i := FIRST(entriesOK) TO LAST(entriesOK) DO
            IF NOT entriesOK[i] THEN
              VAR
                neigh : GridPoint.T;
              BEGIN
                IF EndPointStatus.Neighbor(p,i,neigh) THEN
                  IF DoDebug THEN
                    Debug.Out("Adding " & GridPoint.Format(p) & " <-> " &
                      GridPoint.Format(neigh) & " to self.forbiddenSteps.")
                  END;
                  res.addPair(p,neigh)
                END
              END
            END
          END
        END

      END
    END;
    t.forbiddenSteps := res
  END InitForbiddenSteps;

PROCEDURE NonEndPointsInNetForCurrentRoute(t : Default) : GridPointSet.T =
  VAR
    iter := t.pointSet().iterate();
    p : GridPoint.T;
    s := NEW(GridPointSetDef.T).init();
  BEGIN
    WHILE iter.next(p) DO
      IF NOT t.endPointCands.member(p) THEN
        EVAL s.insert(p)
      END
    END;
    RETURN s
  END NonEndPointsInNetForCurrentRoute;

PROCEDURE NonObstaclesForCurrentRoute(t : Default) : GridPointSet.T =
  VAR
    iter := t.pointSet().iterate();
    p : GridPoint.T;
    nonobs := NEW(GridPointSetDef.T).init();
  BEGIN
    (* check that comps is part of t *)
    
    (* unmark the following points:
       (1) endPoints that are reachable
       (2) points anywhere else in the components *)

    WHILE iter.next(p) DO
      IF NOT t.endPointCands.member(p) THEN
        EVAL nonobs.insert(p)
      ELSE
        VAR
          endPtRects : RectSet.T;
          x : BOOLEAN;
        BEGIN
          x := t.endPointsTargetRects.get(p,endPtRects);
          <* ASSERT x *>

          (* an endPoint can block its own route in two ways: first of
             all the actual rects we target for routing can block, and
             we need to remember it is not an obstacle; but secondly,
             it may block routes in other ways, and we need to mark it
             as open for routing (even though we may not be able to
             connect to it!)---someEntryOK takes care of the first
             case; defaultOK takes care of the second. *)

          VAR
            someEntryOK := t.entryDB.someEntryOK(p,t,
                                   
                                   (* this next argument should really be
                                      the CURRENTLY EXISTING layout, but
                                      it should be OK as having only the 
                                      original layout... *)
                                   endPtRects,
                                   t.currentRectsAllComps);
            defaultOK := t.entryDB.defaultOK(p, t,
                                             endPtRects, 
                                             t.currentRectsAllComps);
          BEGIN
            IF someEntryOK OR defaultOK THEN
              EVAL nonobs.insert(p)
            END
          END
        END
      END
    END;
    
    RETURN nonobs
  END NonObstaclesForCurrentRoute;

(* defaultEndpoints is points that can be reached using just the
   grid-aligned routing algorithm, without special endpoint fixups... *)
PROCEDURE DefaultEndpoints(targets : LabelList.T;
                           layerDB : TextLayerTbl.T;
                           entryDB : EntryDB.T;
                           binnedLayout : RectBins.T;
                           nonObstacles : GridPointSet.T;
                           warmUpBins : BOOLEAN) : GridPointSetSeq.T =
  VAR
    res := NEW(GridPointSetSeq.T).init();
    rectSetSeq : RectSetSeq.T;
    allTargetRects := NEW(RectSet.T).init();
  BEGIN
    rectSetSeq := LabelListToRectSetSeq(binnedLayout, layerDB,targets, warmUpBins);  

    FOR i := 0 TO rectSetSeq.size() - 1 DO
      EVAL allTargetRects.unionD(rectSetSeq.get(i))
    END;

    (* try to speed things up a bit *)
    VAR
      allCands := NEW(GridPointSetDef.T).init();
    BEGIN
      FOR i := 0 TO rectSetSeq.size() - 1 DO
        VAR
          rs := rectSetSeq.get(i);
        BEGIN
          allCands := allCands.unionD(GetCandidateEndpoints(rs))
        END
      END;
      entryDB.advise(allCands)
    END;
      
    (* we really ought to fight with other nets for endpoints here... *)
    FOR i := 0 TO rectSetSeq.size() - 1 DO
      VAR
        rs := rectSetSeq.get(i);
        cands := GetCandidateEndpoints(rs);
      BEGIN

        IF DoDebug THEN
          Debug.Out("Closure #" & Fmt.Int(i));
          Debug.Out("Candidate endpoints:\n" & 
            GridPointSetRoutines.Format(cands))
        END;

        (* delete ones for which default layout is not OK *)
        VAR
          iter := cands.iterate();
          bad : GridPointList.T := NIL;
          p : GridPoint.T;
        BEGIN
          WHILE iter.next(p) DO
            IF NOT entryDB.defaultOKrecomputeAlways(p,rs,allTargetRects) OR NOT EntryDB.DefaultLayoutConnects(rs, p) THEN

              IF DoDebug THEN
                Debug.Out("GridPoint " & GridPoint.Format(p) & 
                  " bad, will delete.")
              END;

              bad := GridPointList.Cons(p,bad)
            ELSIF DoDebug THEN
                Debug.Out("GridPoint " & GridPoint.Format(p) & " OK, keeping.")
            END
          END;
          
          WHILE bad # NIL DO
            EVAL cands.delete(bad.head); bad := bad.tail
          END
        END;

        (* print warning if the rectSetSeq can't be reached *)
        IF cands.size() = 0 THEN
          Debug.Warning("NO REACHABLE LAYOUT FOR ROUTING NET CONSISTING OF:");
          VAR
            iter := rs.iterate();
            r : LayerRect.T;
          BEGIN
            WHILE iter.next(r) DO
              Debug.Warning("MEMBER RECT " & MagRect.Format(r.rect) & " (" & NARROW(r.layer, RouteLayer.T).name & ")")
            END
          END
        END;
        
        res.addhi(cands.copy());

        (* scan neighboring points for non-obstacles *)

          
        IF nonObstacles # NIL THEN

          PROCEDURE ProcCand(q : GridPoint.T) =
            BEGIN
              IF entryDB.defaultOKrecomputeAlways(q,rs,allTargetRects) THEN
                EVAL nonObstacles.insert(q)
              ELSE
                EVAL fails.insert(q)
              END
            END ProcCand;

          VAR
            iter := cands.iterate();
            p, cand : GridPoint.T;
            fails := NEW(GridPointSetDef.T).init();
          BEGIN
            WHILE iter.next(p) DO
              FOR i := FIRST(Compass.Dir) TO LAST(Compass.Dir) DO
                cand := GridPoint.T { p.x + Compass.Step[i].x,
                                       p.y + Compass.Step[i].y,
                                       p.l };
                IF NOT nonObstacles.member(cand) AND NOT fails.member(cand) THEN
                  ProcCand(cand)
                END
              END
            END
          END
        END
      END
    END; (* FOR *)

    RETURN res
  END DefaultEndpoints;

PROCEDURE ConnectEndPt(t : Default; READONLY endPt, nextPt : GridPoint.T) =
  VAR
    (* entry directions *)
    endDir := EndPointStatus.EndPointDir(endPt, nextPt);

    endEntry : MagPointList.T;
    dBool, x : BOOLEAN;

    existingEndRects : RectSet.T;
  BEGIN
    (* get original layout at endpoint *)

    x := t.endPointsTargetRects.get(endPt,existingEndRects);
    <* ASSERT x *>

    
    (* we are going to mutate existingEndRects, so copy it first *)
    existingEndRects := existingEndRects.copy();

    (* is it already connected? if so, add session rects to target! 
       if not, create new session.


       must NEVER have more than one session for a given e.p. *)

    VAR 
      endRects : RectSet.T;
      session : MagSession.T := NIL; 
    BEGIN
      IF t.endPtSessionTbl.get(endPt,session) THEN
        VAR
          iter := session.rectSet().iterate();
          r : LayerRect.T;
        BEGIN
          WHILE iter.next(r) DO
            EVAL existingEndRects.insert(r)
          END
        END
      ELSE
        session := t.wiringCell.newSession();
        x := t.endPtSessionTbl.put(endPt,session);
        <* ASSERT NOT x *>
      END;

      (* connect to them, treating all the rects of the net as other stuff
         we can use *)
      Debug.Out("RouteComponents.ConnectEndPt: connecting endpoint at " &
      GridPoint.Format(endPt));
      x := t.entryDB.get(endPt, t, existingEndRects, t.currentRectsAllComps, 
                         endDir, endEntry, dBool);
      
      endRects := MagicStuff.ConnectTo(endEntry, endPt, endDir);

      EVAL t.currentRectsAllComps.unionD(endRects);
      t.wiringCell.sessAddLayerRectSet(session,endRects);
      t.entryDB.flushEntries(endPt, radius := 1);
      
      DebugAddRectSet(endRects);
      t.binnedLayout.addRectSet(endRects)
    END
  END ConnectEndPt;

(* debugging stuff... *)

PROCEDURE DebugAddRectSet(s : RectSet.T) =
  VAR
    iter := s.iterate();
    rect : LayerRect.T;
  BEGIN
    WHILE iter.next(rect) DO
      Debug.Out(Fmt.F(":box %s %s %s %s; paint %s",
                      Fmt.Int(rect.rect.ll.x),
                      Fmt.Int(rect.rect.ll.y),
                      Fmt.Int(rect.rect.ur.x),
                      Fmt.Int(rect.rect.ur.y),
                      NARROW(rect.layer, RouteLayer.T).name))
    END
  END DebugAddRectSet;

PROCEDURE DebugRollbackSession(s : MagSession.T) =
  VAR
    iter := s.iterate();
     rect : LayerRect.T;
  BEGIN
    WHILE iter.next(rect) DO
        Debug.Out(Fmt.F(":box %s %s %s %s; erase %s",
                        Fmt.Int(rect.rect.ll.x),
                        Fmt.Int(rect.rect.ll.y),
                        Fmt.Int(rect.rect.ur.x),
                        Fmt.Int(rect.rect.ur.y),
                        NARROW(rect.layer, RouteLayer.T).name))
    END
  END DebugRollbackSession;

PROCEDURE CurrentlyReachableEndPoint(t : Default; READONLY p, next : GridPoint.T) : BOOLEAN =
  VAR
    x, dBool : BOOLEAN;
    endPtRects : RectSet.T;
    endDir := EndPointStatus.EndPointDir(p, next);

    endEntry : MagPointList.T;
  BEGIN
    IF NOT t.endPointCands.member(p) THEN RETURN FALSE END;
    x := t.endPointsTargetRects.get(p,endPtRects);
    <* ASSERT x *>
    RETURN t.entryDB.get(p, t, endPtRects, t.currentRectsAllComps, 
                       endDir, endEntry, dBool)
  END CurrentlyReachableEndPoint;

PROCEDURE CheckComponentInvariants(t : Default; noSingletons : BOOLEAN;
                                   doSearchConsistency : BOOLEAN) =

  PROCEDURE HaveComponentWith(READONLY p : GridPoint.T) : GridPointSet.T =
    BEGIN
      FOR i := 0 TO t.components.size() - 1 DO
        VAR
          s := t.components.get(i);
        BEGIN
          IF s.member(p) THEN RETURN s END
        END 
      END;
      <* ASSERT FALSE *>
    END HaveComponentWith;

  VAR
    allComponentPoints := NEW(GridPointSetDef.T).init();
  BEGIN
    IF NOT AllAssertions THEN RETURN END;

    FOR i := 0 TO t.components.size() - 1 DO
      EVAL allComponentPoints.unionD(t.components.get(i));

      VAR
        s := t.components.get(i);
        iter := s.iterate();
        p : GridPoint.T;
        d : GridPointSet.T;
        x : BOOLEAN;
      BEGIN
        WHILE iter.next(p) DO
          x := t.ptCompTbl.get(p,d);
(*
          <* ASSERT x *>
          <* ASSERT d = s *> (* same references! *)
*)
          IF NOT x OR d # s THEN
            Process.Crash("Components problem---look for layout too close in the vicinity of " & 
              GridPoint.Format(p) & "; Magic box " & Fmt.Int(p.x * GridStep()) & " " & 
                   Fmt.Int(p.y * GridStep())   & " " & Fmt.Int((p.x + 1) * GridStep()) & " " &
                   Fmt.Int((p.y + 1) * GridStep()) & 
                   " [layer " & Fmt.Int(p.l) & "]\n")
          END
        END
      END
    END;

    VAR
      iter := t.ptCompTbl.iterate();
      p : GridPoint.T;
      s : GridPointSet.T;
    BEGIN
      WHILE iter.next(p,s) DO
        <* ASSERT s.member(p) *>
        
        <* ASSERT HaveComponentWith(p) = s *>
        
      END
    END;

    (* components have to be disjoint *)

    FOR i := 0 TO t.components.size() - 1 DO
      FOR j := i + 1 TO t.components.size() - 1 DO
        VAR
          n := t.components.get(i).intersection(t.components.get(j));
        BEGIN
          <* ASSERT n.size() = 0 *>
        END
      END
    END;

    (**********************************************************************)
    (**********************************************************************)
    (**********************                       *************************)
    (**********************  endpoint invariants  *************************)
    (**********************                       *************************)
    (**********************************************************************)
    (**********************************************************************)
    
    (* no endpoints not in a component! *)
    VAR
      iter := t.endPointsActive.iterate();
      p : GridPoint.T;
      dummy : MagSession.T;
      comp : GridPointSet.T;
    BEGIN
      WHILE iter.next(p) DO
        (* if it's active, it's in a component! *)
        <* ASSERT allComponentPoints.member(p) *>
        
        (* and it has a layout session *)
        <* ASSERT t.endPtSessionTbl.get(p,dummy) *>

        (* and it's a candidate *)
        <* ASSERT t.endPointCands.member(p) *>

        (* and the component is larger than 1 point *)
        comp := HaveComponentWith(p);

        <* ASSERT comp.member(p) *>
        IF noSingletons THEN <* ASSERT comp.size() > 1 *> END
      END
    END;

    (* non-active endpoints *)
    VAR
      iter := t.endPointCands.diff(t.endPointsActive).iterate();
      p : GridPoint.T;
      dummy : MagSession.T;
    BEGIN
      WHILE iter.next(p) DO
        (* all endpt candidates are in a component! *)
        <* ASSERT allComponentPoints.member(p) *>
        
        <* ASSERT NOT t.endPtSessionTbl.get(p,dummy) *>
      END
    END;

    (* check searchComponent *)
    IF doSearchConsistency THEN
      VAR
        iter := allComponentPoints.iterate();
        p : GridPoint.T;
        searched := NEW(GridPointSetDef.T).init();
      BEGIN
        WHILE iter.next(p) DO
          IF NOT searched.member(p) THEN
            VAR 
              x : BOOLEAN;
              comp, search : GridPointSet.T;
            BEGIN
              x := t.ptCompTbl.get(p,comp); <* ASSERT x *>
              search := t.searchComponent(p);
              
              searched := searched.unionD(search);
              
              VAR
                i1 := comp.iterate();
                i2 := search.iterate();
                ppp : GridPoint.T;
              BEGIN

                Debug.Out("doSearchConsistency! p = " & GridPoint.Format(p));
                Debug.Out("comp = " & GridPointSetRoutines.Format(comp));
                Debug.Out("search = " & GridPointSetRoutines.Format(search));
                WHILE i1.next(ppp) DO <* ASSERT search.member(ppp) *> END;
                WHILE i2.next(ppp) DO <* ASSERT comp.member(ppp) *> END
              END
            END
          END
        END
      END
    END
  END CheckComponentInvariants;

PROCEDURE GetConnectedEndPoints(t : Default; READONLY p : GridPoint.T) : GridPointSet.T =
  BEGIN
    FOR i := 0 TO t.endPointComponents.size() - 1 DO
      VAR c := t.endPointComponents.get(i); BEGIN
        IF c.member(p) THEN RETURN c.copy() END
      END
    END;
    <* ASSERT FALSE *>
  END GetConnectedEndPoints;

PROCEDURE DeleteAllPoints(t : Default) =
  <* FATAL Thread.Alerted, Wr.Failure *>
  (* doDelete can't raise an exception since it doesn't write *)
  VAR
    pts := t.pointSet().copy();
    p : GridPoint.T;
    iter : GridPointSet.Iterator;
  BEGIN

    (* pts has all the points
       actually active points are
       
       pts LESS endPointCands
           UNION endPointsActive *)

    EVAL pts.diffD(t.endPointCands);
    EVAL pts.unionD(t.endPointsActive);

    iter := pts.iterate();

    WHILE iter.next(p) DO
      t.doDelete(p, writeToDelWr := FALSE)
    END;

    (* delete all standard layout *)

    t.wiringCell.rollbackSession(t.wiringSession);
    DebugRollbackSession(t.wiringSession);

    IF DoDebug THEN t.wiringCell.debugDumpData() END;

    t.binnedLayout.flushStaleSession(t.wiringSession);

    (* current rects revert to original targets; 
       there are no active endPoints *)
    t.currentRectsAllComps := t.allTargetRects.copy();

  END DeleteAllPoints;




TYPE 
  DefComponentIterator = ComponentIterator BRANDED Brand & "Component Iterator" OBJECT
    i : CARDINAL := 0;
    t : Default;
  OVERRIDES
    next := CINext
  END;


PROCEDURE CINext(ci : DefComponentIterator; VAR comp : GridPointSet.T) : BOOLEAN =
  BEGIN
    IF ci.i >= ci.t.size() THEN RETURN FALSE END;

    comp := ci.t.endPointComponents.get(ci.i);
    INC(ci.i);

    RETURN TRUE
  END CINext;

PROCEDURE IterateComponents(self : T) : ComponentIterator =
  BEGIN RETURN NEW(DefComponentIterator, t := self) END IterateComponents;

BEGIN 
  IF AllAssertions THEN
    IO.Put("RouteComponents assertions turned ON.  You may get a substantial performance improvement by turning them OFF.  (Set the NOROUTERASSERTIONS environment variable to do this.)\n")
  END
END RouteComponents.
