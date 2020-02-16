(* $Id$ *)

MODULE OnGridComponents;
IMPORT Filled,Wr,GridPointSetSeq,GridPointRouteIDTbl;
IMPORT RouteID, GridPointList, GridPointSet, GridPointIntTbl, GridPoint;
IMPORT GridPointSetDef, IntSet, IntSetDef;
IMPORT Compass;
IMPORT Debug, Env;
IMPORT Thread;
IMPORT GridPointSetRoutines;
IMPORT Fmt;
IMPORT TwoComponents, IntGridPointSetTbl, GridPointSetMST;
IMPORT GridPointRefTbl;

VAR DoDebug := Debug.DebugThis("OnGridComponents");
VAR AllAssertions := Env.Get("ALLROUTERASSERTIONS") # NIL;
VAR dLevel := Debug.GetLevel();

<* FATAL Thread.Alerted, Wr.Failure *>

REVEAL 
  T = Public BRANDED Brand OBJECT
    delWr : Wr.T := NIL;
    initComponents : GridPointSetSeq.T;
    myId : RouteID.T;
    pendingDeletions : GridPointList.T := NIL;
    endPointCands : GridPointSet.T;

    compTree : REF ARRAY OF CARDINAL;
    (* union find structure *)

    initCompMember, currCompMember : GridPointIntTbl.T;
    (* initCompMember says which component a gridpoint was initially a member
       of (used for endpoints and constant during routing) *)
    (* currCompMember is an index into comp tree for all connected points *)

    totLengthEstimate := -1000.0d0; (* ridiculous value *)

  METHODS

    debugDumpComponents() := DebugDumpComponents;

    recalcComponents(writeToDelWr : BOOLEAN; deleted : GridPointSet.T) RAISES { Thread.Alerted, Wr.Failure } := RecalcComponents;

    doDelete(READONLY p : GridPoint.T; writeToDelWr : BOOLEAN) RAISES { Thread.Alerted, Wr.Failure } := DoDelete;
    (* do all the deletion steps for given pt *)

    getConnectedEndPoints(READONLY p : GridPoint.T) : GridPointSet.T := GetConnectedEndPoints;
    (* get all the endpoints connected thru pre-existing layout to a 
       given ENDPOINT *)

    connectedNeighbors(READONLY p : GridPoint.T) : GridPointSet.T := ConnectedNeighbors;
    (* get the neighbors of p that are in the set.
       NB. p does NOT need to be in the set itself *)

    unravel(READONLY prev, from : GridPoint.T) : GridPointList.T := Unravel;
    (* starting from from, go away from prev
       until we hit an endPoint or forkPoint *)

    searchComponent(READONLY p : GridPoint.T) : GridPointSet.T := 
        SearchComponent;
    (* find a connected component by searching neighbors starting from p *)
    (* p must currently be part of the net *)

    haveIt(READONLY q : GridPoint.T) : BOOLEAN := HaveIt;
    (* is this pt a member of one of the components? *)

    compSet() : IntSet.T := CompSet;
    (* root components right now *)

    rootComp(c : CARDINAL) : CARDINAL := RootComp;

    components() : GridPointSetSeq.T := Components;
    (* return the components in SOME ORDER (not a stable order) *)
  OVERRIDES
    approxSize := ApproxSize;
    init := Init;
    id := GetId;
    deleteAPoint := DeleteAPoint;
    connect := Connect;
    size := Size;
    commitRipups := CommitRipups;
    longest := Longest;
    nonObstaclesForCurrentRoute := PointSet;
    allPoints := AllPoints;
    activeEndPoints := ActiveEndPoints;
    pointSet := PointSet;
    cleanOutNonArts := CleanOutNonArts;
  END;

PROCEDURE DebugDumpComponents(t : T) =
  VAR
    tbl := NEW(IntGridPointSetTbl.Default).init();
    iter := t.currCompMember.iterate();
    p : GridPoint.T;
    c : INTEGER;
    s : GridPointSet.T;
  BEGIN
    WHILE iter.next(p,c) DO
      c := t.rootComp(c);
      IF NOT tbl.get(c,s) THEN
        s := NEW(GridPointSetDef.T).init();
      END;
      EVAL s.insert(p);
      EVAL tbl.put(c,s)
    END;
    
    VAR
      jter := tbl.iterate();
    BEGIN
      WHILE jter.next(c,s) DO
        Debug.Out("Component " & Fmt.Int(c));
        Debug.Out(GridPointSetRoutines.Format(s))
      END
    END;

    VAR
      str := "compTree = [ ";
    BEGIN
      FOR i := FIRST(t.compTree^) TO LAST(t.compTree^) DO
        str := str & Fmt.Int(t.compTree[i]) & " "
      END;
      Debug.Out(str & "]")
    END
  END DebugDumpComponents;

PROCEDURE PointSet(t : T) : GridPointSet.T =
  VAR
    s := NEW(GridPointSetDef.T).init();
    iter := t.currCompMember.iterate();
    dmmy : INTEGER;
    p : GridPoint.T;
  BEGIN 
    WHILE iter.next(p,dmmy) DO EVAL s.insert(p) END;
    RETURN s
  END PointSet;

PROCEDURE Init(t : T; 
               initComponents : GridPointSetSeq.T;
               delWr : Wr.T;
               id : RouteID.T;
               (* OUT *) specialEndPtsTbl : GridPointRouteIDTbl.T := NIL) : T=
  BEGIN
    t.delWr := delWr;
    t.initComponents := initComponents;
    t.myId := id;

    (* initialize auxiliary data structures *)
    t.endPointCands := NEW(GridPointSetDef.T).init();
    FOR i := 0 TO initComponents.size()-1 DO
      EVAL t.endPointCands.unionD(initComponents.get(i))
    END;

    t.initCompMember := NEW(GridPointIntTbl.Default).init();
    t.currCompMember := NEW(GridPointIntTbl.Default).init();

    t.compTree := NEW(REF ARRAY OF CARDINAL, initComponents.size());

    FOR i := 0 TO initComponents.size()-1 DO
      t.compTree[i] := i;
      VAR
        iter := initComponents.get(i).iterate();
        p : GridPoint.T;
      BEGIN
        WHILE iter.next(p) DO
          EVAL t.initCompMember.put(p,i);
          EVAL t.currCompMember.put(p,i)
        END
      END
    END;
    
    (* initialize special end points table... *)

    FOR i := 0 TO initComponents.size()-1 DO
      VAR
        c := initComponents.get(i);
      BEGIN
        IF c.size() = 1 THEN
          VAR
            iter := c.iterate();
            p : GridPoint.T;
            x := iter.next(p);
            okToPaintOver := FALSE;
          BEGIN
            <* ASSERT x *>
            <* ASSERT NOT iter.next(p) *>

            FOR i := FIRST(Compass.Dir) TO LAST(Compass.Dir) DO
              IF NOT Filled.Marked(GridPoint.T { p.x + Compass.Step[i].x,
                                                 p.y + Compass.Step[i].y,
                                                 p.l }) THEN
                okToPaintOver := TRUE;
                EXIT
              END
            END;

            IF NOT okToPaintOver AND p.l = 1 THEN
              p.l := 2;
              EVAL specialEndPtsTbl.put(p,id)
            END
          END
        END
      END
    END;
    VAR
      mst := NEW(GridPointSetMST.T).init(t.components());
    BEGIN
      t.totLengthEstimate := mst.totalWeight()
    END;
    RETURN t
  END Init;

PROCEDURE ApproxSize(t : T) : LONGREAL =
  BEGIN RETURN t.totLengthEstimate END ApproxSize;

PROCEDURE Components(t : T) : GridPointSetSeq.T =
  VAR
    comps := NEW(IntGridPointSetTbl.Default).init();
    iter := t.currCompMember.iterate();
    gp : GridPoint.T;
    idx : INTEGER;
    s : GridPointSet.T;
  BEGIN
    WHILE iter.next(gp,idx) DO
      idx := t.rootComp(idx);
      IF NOT comps.get(idx,s) THEN
        s := NEW(GridPointSetDef.T).init();
      END;
      EVAL s.insert(gp);
      EVAL comps.put(idx,s)
    END;

    VAR
      iter := comps.iterate();
      res := NEW(GridPointSetSeq.T).init();
    BEGIN
      WHILE iter.next(idx, s) DO
        res.addhi(s)
      END;
      RETURN res
    END
  END Components;

PROCEDURE Longest(t : T) : TwoComponents.T =
  VAR
    mst := NEW(GridPointSetMST.T).init(t.components());
    s1, s2 : GridPointSet.T;
    len : LONGREAL;
  BEGIN
    mst.getLink(mst.size()-1,s1,s2,len);
    RETURN NEW(TwoComponents.T).init(s1,s2,len)
  END Longest;

PROCEDURE GetId(t : T) : RouteID.T = BEGIN RETURN t.myId END GetId;

PROCEDURE DeleteAPoint(t : T; gp : GridPoint.T) =
  BEGIN
    IF AllAssertions THEN
    <* ASSERT t.pointSet().member(gp) *>
    END;
    t.pendingDeletions := GridPointList.Cons(gp,t.pendingDeletions)
  END DeleteAPoint; 

PROCEDURE Unravel(t : T; READONLY prev, from : GridPoint.T) : GridPointList.T =
  VAR
    n := t.connectedNeighbors(from);
  BEGIN
    EVAL n.delete(prev);
    
    IF n.size() > 1 THEN
      RETURN GridPointList.List1(from)
    ELSIF t.endPointCands.member(from) THEN
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

PROCEDURE HaveIt(t : T; READONLY q : GridPoint.T) : BOOLEAN =
  VAR
    dummy : INTEGER;
  BEGIN
    RETURN t.currCompMember.get(q,dummy)
  END HaveIt;

PROCEDURE GetConnectedEndPoints(t : T; READONLY p : GridPoint.T) : GridPointSet.T =
  VAR
    i : INTEGER;
    x := t.initCompMember.get(p,i);
  BEGIN
    <* ASSERT x *>
    RETURN t.initComponents.get(i).copy()
  END GetConnectedEndPoints;

PROCEDURE SearchComponent(t : T; READONLY p : GridPoint.T) : GridPointSet.
T =

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
                (NOT t.endPointCands.member(q) )
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

(* what points would a point at p be connected to? (p need not be present!) *)
PROCEDURE ConnectedNeighbors(t : T; READONLY p : GridPoint.T) : GridPointSet.T =  
    
  PROCEDURE ProcCand(READONLY c : GridPoint.T) =
    BEGIN
      (* p and c would be connected iff:
         c is in a component AND
         (c is not an endpoint OR c is an active endpoint) *)

      IF t.haveIt(c) (*AND 
        (NOT t.endPointCands.member(c) OR t.endPointActives.member(c)) *)THEN 
        EVAL res.insert(c) 
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

PROCEDURE Connect(t : T; gpl : GridPointList.T) : GridPointList.T =
  VAR
    pp := gpl;
    minComp := LAST(CARDINAL);
    compsMerged := NEW(IntSetDef.T).init();
    initSize, finalSize : CARDINAL;
  BEGIN
    IF DoDebug THEN
      initSize := t.size()
    END;

    IF DoDebug THEN 
      Debug.Out("Before connect:");
      t.debugDumpComponents() 
    END;
    
    WHILE pp # NIL DO
      IF t.endPointCands.member(pp.head) THEN
      ELSE
      END;
      VAR
        thisComp : INTEGER;
      BEGIN
        IF t.currCompMember.get(pp.head,thisComp) THEN
          thisComp := t.rootComp(thisComp);
          EVAL compsMerged.insert(thisComp);

          IF dLevel > 0 THEN
            Debug.Out("Merged component: " & Fmt.Int(thisComp) & " (p=" & GridPoint.Format(pp.head) & ")")
          END;

          minComp := MIN(minComp, thisComp)
        END
      END;
      pp := pp.tail
    END;

    <* ASSERT minComp < LAST(CARDINAL) *>

    (* merge components *)
    VAR
      iter := compsMerged.iterate();
      c : INTEGER;
    BEGIN
      WHILE iter.next(c) DO
        t.compTree[c] := minComp
      END
    END;

    pp := gpl;
    WHILE pp # NIL DO
      VAR
        <*NOWARN*>x := t.currCompMember.put(pp.head,minComp);
      BEGIN
        (* again, i don't think we can make any assertions at this 
           point... *)
        (* IF x THEN <* ASSERT t.endPointCands.member(pp.head) *> END *)
      END;
        
      pp := pp.tail
    END;

    (* ok.. there's a problem here...

       notwithstanding all the clever code above, remember that
       the end result will actually connect ALL neighbors.  sets
       may be connected even though they are ONLY NEIGHBORS and
       don't actually overlap.... :-/ *)
       
    VAR
      deleted := NEW(GridPointSetDef.T).init();
    BEGIN
      t.recalcComponents(t.delWr # NIL, deleted);
      
      <* ASSERT deleted.size() = 0 *>
    END;

    IF DoDebug THEN 
      Debug.Out("After connect:");
      t.debugDumpComponents() 
    END;

    IF DoDebug THEN 
      finalSize := t.size();
      <* ASSERT finalSize < initSize *>
    END;

    RETURN gpl (* oh, whatever.. we'll clean it up later *)
  END Connect;

PROCEDURE CleanOutNonArts(t : T; deleted : GridPointSet.T; writeToDelWr : BOOLEAN) =
  VAR
    siz := t.size();
  BEGIN
    IF DeleteNonArts(t, writeToDelWr, deleted) THEN
      t.recalcComponents(writeToDelWr, deleted)
    END;
    <* ASSERT t.size() = siz *>
  END CleanOutNonArts;
    

PROCEDURE CompSet(t : T) : IntSet.T =
  VAR
    comps := NEW(IntSetDef.T).init();
  BEGIN
    FOR i := FIRST(t.compTree^) TO LAST(t.compTree^) DO
      EVAL comps.insert(t.rootComp(i))
    END;
    RETURN comps
  END CompSet;

PROCEDURE Size(t : T) : CARDINAL =
  BEGIN RETURN t.compSet().size() END Size;

PROCEDURE RootComp(t : T; c : CARDINAL) : CARDINAL =
  VAR
    d := c;
  BEGIN
    WHILE d # t.compTree[d] DO d := t.compTree[d] END;

    WHILE t.compTree[c] # d DO 
      VAR
        e := t.compTree[c]; 
      BEGIN
        t.compTree[c] := d; 
        c := e 
      END
    END;

    RETURN d
  END RootComp;

PROCEDURE CommitRipups(t : T; writeToDelWr : BOOLEAN)  : GridPointSet.T RAISES { Thread.Alerted, Wr.Failure } =
  VAR
    deleted, compsTouched := NEW(GridPointSetDef.T).init();
    p : GridPoint.T;
  BEGIN
    WHILE t.pendingDeletions # NIL DO
      p := t.pendingDeletions.head;
      IF NOT deleted.member(p) THEN
        
        IF dLevel > 0 THEN
          Debug.Out("About to delete " & GridPoint.Format(p))
        END;

        Debug.Out("EndPointCands are:");
        VAR
          iter := t.endPointCands.iterate();
          c : GridPoint.T;
        BEGIN
          WHILE iter.next(c) DO Debug.Out(GridPoint.Format(c)) END
        END;
            
        VAR
          oldComp := t.searchComponent(p);
        BEGIN
          IF dLevel > 0 THEN
            Debug.Out("SearchComponent(p="&GridPoint.Format(p)&") =  " &
              GridPointSetRoutines.Format(oldComp))
          END;
          EVAL compsTouched.unionD(oldComp)
        END;

        (* ---------- DELETE p ---------- *)
        IF dLevel > 0 THEN
          Debug.Out("t.doDelete(" & GridPoint.Format(p) & ") [first point]")
        END;

        t.doDelete(p, writeToDelWr);
        EVAL deleted.insert(p);

        (* start unravelling... *)
        VAR
          neighbors := t.connectedNeighbors(p);
          iter := neighbors.iterate();
          q : GridPoint.T;
        BEGIN
          WHILE iter.next(q) DO
            DeleteWhileSinglePath(t, q, deleted, writeToDelWr)
          END
        END
      END;
      t.pendingDeletions := t.pendingDeletions.tail
    END;
    t.recalcComponents(writeToDelWr, deleted);

    VAR
      siz := t.size();
    BEGIN
      IF DeleteNonArts(t, writeToDelWr, deleted) THEN
        t.recalcComponents(writeToDelWr, deleted)
      END;
      <* ASSERT t.size() = siz *>
    END;

    RETURN deleted
  END CommitRipups;

PROCEDURE DoDelete(t : T; READONLY p : GridPoint.T; 
                   writeToDel : BOOLEAN) RAISES { Thread.Alerted, Wr.Failure }=
  BEGIN
    IF t.delWr # NIL AND writeToDel THEN
      Wr.PutText(t.delWr, "delete " & Fmt.Int(p.x) & " " &
                   Fmt.Int(p.y)   &
                   " [layer " & Fmt.Int(p.l) & "]\n") ;
      Wr.Flush(t.delWr)
    END;
    IF dLevel > 0 THEN
      Debug.Out("DoDelete("&GridPoint.Format(p) & ")")
    END;
    IF t.endPointCands.member(p) THEN

    ELSE
      VAR
        comp : INTEGER;
        x := t.currCompMember.delete(p,comp);
      BEGIN
        <* ASSERT x *>
      END;
      Filled.UnMark(p)
    END;
  END DoDelete;

PROCEDURE DeleteWhileSinglePath(t : T; READONLY p : GridPoint.T;
                                deleted : GridPointSet.T;
                                writeToDel : BOOLEAN) RAISES { Thread.Alerted,
                                                               Wr.Failure } =
  TYPE
    Status = { Continue, Stop };
  VAR
    neighbors := t.connectedNeighbors(p);
    iter := neighbors.iterate();
    status := Status.Stop;
    q : GridPoint.T;
  BEGIN

    Debug.Out("DeleteWhileSinglePath(..., " & GridPoint.Format(p) & ", ...)");
    IF deleted.member(p) THEN RETURN END;

    (* we keep deleting until we hit an articulation point *)
    IF neighbors.size() = 1 THEN
      Debug.Out("t.doDelete(" & GridPoint.Format(p) & ") [single neighbor]");
      t.doDelete(p, writeToDel);
      EVAL deleted.insert(p);
      status := Status.Continue;
    END;
    
    IF status # Status.Stop THEN
        Debug.Out("Calling recursively DeleteWhileSinglePath(..., " & GridPoint.Format(q) & ", ...)");
      WHILE iter.next(q) DO
        DeleteWhileSinglePath(t, q, deleted, writeToDel)
      END
    END;
    Debug.Out("Returning from DeleteWhileSinglePath(..., " & GridPoint.Format(p) & ", ...)");

  END DeleteWhileSinglePath;
    
PROCEDURE AllPoints(t : T) : GridPointSet.T =
  BEGIN
    RETURN t.pointSet()
  END AllPoints;

PROCEDURE ActiveEndPoints(t : T) : GridPointSet.T = 
  BEGIN
    RETURN t.endPointCands
  END ActiveEndPoints;

PROCEDURE RecalcComponents(t : T; writeToDelWr : BOOLEAN;
                           deleted : GridPointSet.T) RAISES { Wr.Failure, Thread.Alerted } =
  VAR
    iter := t.initCompMember.iterate();
    p : GridPoint.T;
    idx : INTEGER;
    got := NEW(GridPointSetDef.T).init();
  BEGIN
    WHILE iter.next(p,idx) DO
      (* got a seed *)
      IF NOT got.member(p) THEN
        VAR
          c := t.searchComponent(p);
          siz := c.size();
          citer := c.iterate();
          pp : GridPoint.T;
          jdx : INTEGER;
        BEGIN
          EVAL got.unionD(c);

          WHILE citer.next(pp) DO
            IF t.initCompMember.get(pp,jdx) AND deleted.member(pp) THEN
              IF siz > 1 THEN

                (* "un-delete" the point... *)
                EVAL deleted.delete(pp)
              END;
              idx := MIN(idx,jdx)
            END
          END;

          citer := c.iterate();
          WHILE citer.next(pp) DO
            EVAL t.currCompMember.put(pp,idx);
            IF t.initCompMember.get(pp,jdx) THEN
              t.compTree[jdx] := idx
            END
          END
        END
      END
    END;

    VAR
      floating := t.pointSet().diff(t.endPointCands).diff(got);
      iter := floating.iterate();
      p : GridPoint.T;
    BEGIN
      (* delete everything that's floating... *)
      WHILE iter.next(p) DO 
        t.doDelete(p, writeToDelWr); EVAL deleted.insert(p) 
      END
    END;
  END RecalcComponents;

PROCEDURE DeleteNonArts(t : T; writeToDelWr : BOOLEAN; deleted : GridPointSet.T) : BOOLEAN RAISES { Thread.Alerted, Wr.Failure } =
  (* look for non-articulation points *)
  VAR
    iter := t.initCompMember.iterate();
    done := TRUE;
    idx : INTEGER;
    p, ap : GridPoint.T;
    didSome := FALSE;
    seen := NEW(GridPointSetDef.T).init();
  BEGIN
    WHILE iter.next(p,idx) DO
      IF NOT seen.member(p) THEN
        REPEAT
          done := TRUE;
          IF FindANonArticulationPoint(t, p, t.endPointCands, seen, ap) THEN
            didSome := TRUE;
            Debug.Out("Deleting non-art. point " & GridPoint.Format(ap));
            <* ASSERT NOT t.endPointCands.member(ap) *>
  
            t.doDelete(ap, writeToDelWr); EVAL deleted.insert(ap);
            
            IF dLevel > 9 THEN
              VAR
                siz := t.size();
              BEGIN
                t.recalcComponents(writeToDelWr, deleted);
                IF t.size() # siz THEN
                  Debug.Out("t.size() = " & Fmt.Int(t.size()) & " oldSiz = " & Fmt.Int(siz));
                  t.debugDumpComponents();
                  <* ASSERT FALSE *>
                END
              END
            END;

            done := FALSE
          END
        UNTIL done;
      END
    END;
    RETURN didSome
  END DeleteNonArts;

TYPE
  TreeNode = OBJECT
    p : GridPoint.T;
    depth : CARDINAL;
    neighbors : List;
    low := LAST(CARDINAL); (* low-water mark for art. pt alg *)
  END;

  List = OBJECT
    node : TreeNode;
    next : List;
  END;

PROCEDURE DFS(t : T; start : GridPoint.T; track, tracked : GridPointSet.T) : TreeNode =

  PROCEDURE Recurse(p, from : GridPoint.T; depth : CARDINAL) : TreeNode =
    VAR
      res : REFANY;
    BEGIN
      IF NOT got.get(p,res) THEN

        IF track.member(p) THEN EVAL tracked.insert(p) END;

        res := NEW(TreeNode, p := p, depth := depth, neighbors := NIL);
  
        EVAL got.put(p,res);

        PROCEDURE ProcCand(READONLY c : GridPoint.T) =
          BEGIN
            (* p AND c would be connected iff:
               c is in a component AND
               (c is not an endpoint OR c is an active endpoint) *)
            
            IF t.haveIt(c) AND c # from THEN 
              NARROW(res,TreeNode).neighbors := NEW(List, 
                                                    node := Recurse(c, p, depth + 1), 
                                                    next := NARROW(res,TreeNode).neighbors)

            END
    
          END ProcCand;

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
        END
      END;

      RETURN res
    END Recurse;

  CONST 
    NoWhere = GridPoint.T { FIRST(INTEGER), FIRST(INTEGER), FIRST(GridPoint.Layer) };
  VAR
    got := NEW(GridPointRefTbl.Default).init();
  BEGIN
    RETURN Recurse(start,NoWhere,0)
  END DFS;

PROCEDURE FindANonArticulationPoint(t : T; start : GridPoint.T; ignore, ignoreSeen : GridPointSet.T; VAR result : GridPoint.T) : BOOLEAN =
  VAR
    dfstree : TreeNode;
  BEGIN
    dfstree := DFS(t, start, ignore, ignoreSeen);
    TraverseMark(dfstree);    

    (* case 0. null tree *)
    IF dfstree = NIL THEN RETURN FALSE END;

    (* case 1. root is nonart *)
    IF NOT ignore.member(dfstree.p) AND (dfstree.neighbors = NIL OR dfstree.neighbors.next = NIL) THEN
      Debug.Out("FindANonArticulationPoint: root is non-art at " & GridPoint.Format(dfstree.p));
      result := dfstree.p;
      RETURN TRUE
    END;

    (* case 2. search for non-root non-art. point *)
    RETURN NonArt(dfstree, ignore, result)
  END FindANonArticulationPoint;

PROCEDURE TraverseMark(t : TreeNode) =
  VAR
    n := t.neighbors;
  BEGIN
    WHILE n # NIL DO
      IF n.node.depth < t.depth THEN
        (* back edge *)
        t.low := MIN(t.low, n.node.depth)
      ELSIF n.node.depth = t.depth + 1 THEN
        (* tree edge---preorder traversal *)
        TraverseMark(n.node); 
        t.low := MIN(t.low, n.node.low)
      ELSIF n.node.depth = t.depth THEN
        <* ASSERT FALSE *>
      ELSIF n.node.depth > t.depth + 1 THEN
        (* other end of back edge---ignore *)
      END;
      n := n.next
    END;
  END TraverseMark;

PROCEDURE NonArt(t : TreeNode; ignore : GridPointSet.T; VAR result : GridPoint.T) : BOOLEAN =
  VAR 
    p := t.neighbors;
    success := TRUE;
  BEGIN
    (* do all chilren loop back?  then we're it! *)
    WHILE p # NIL DO
      IF p.node.depth > t.depth AND p.node.low >= t.depth THEN success := FALSE END;
      p := p.next
    END; 
    IF success AND NOT ignore.member(t.p) THEN result := t.p; RETURN TRUE END;

    (* search children *)
    p := t.neighbors;
    WHILE p # NIL DO
      IF p.node.depth = t.depth + 1 THEN
        IF NonArt(p.node, ignore, result) THEN RETURN TRUE END
      END;
      p := p.next
    END;
    RETURN FALSE
  END NonArt;

BEGIN END OnGridComponents.
