(* $Id$ *)

MODULE CurrentlyRoutedTbl;
IMPORT GridPointList, GridPoint, GridPointStatus;
IMPORT RouteTag;
IMPORT RouteTagPointSetTbl;
IMPORT GridPointSet, GridPointSetDef;
IMPORT Debug, Fmt;

VAR DoDebug := Debug.DebugThis("CurrentlyRoutedTbl");

REVEAL
  T = Public BRANDED Brand OBJECT
    byTagTbl : RouteTagPointSetTbl.T;
  OVERRIDES
    addPointList := AddPointList;
    delPointList := DelPointList;
    gotSomething := GotSomething;
    init := Init;
    getPointsByTag := GetPointsByTag;
  END;

PROCEDURE GetPointsByTag(self : T; t : RouteTag.T) : GridPointSet.T =
  VAR
    res : GridPointSet.T;
  BEGIN
    IF NOT self.byTagTbl.get(t, res) THEN 
      res := NEW(GridPointSetDef.T).init()
    END;
    RETURN res
  END GetPointsByTag;

PROCEDURE GotSomething(self : T; p : GridPoint.T) : BOOLEAN =
  VAR
    dummy : GridPointStatus.T;
  BEGIN
    RETURN self.get(p,dummy)
  END GotSomething;

PROCEDURE Init(self : T; defSiz : CARDINAL) : Super =
  VAR
    res := Super.init(self, defSiz);
  BEGIN
    self.byTagTbl := NEW(RouteTagPointSetTbl.Default).init();
    RETURN res
  END Init;

PROCEDURE AddPointList(self : T; gpl : GridPointList.T; tag : RouteTag.T) =
  VAR 
    op : GridPointList.T := NIL;
    p := gpl;
    hadStatus : BOOLEAN;
    oldStatus, opOldStatus : GridPointStatus.T;
    pointSet : GridPointSet.T;
  BEGIN
    IF NOT self.byTagTbl.get(tag,pointSet) THEN
      pointSet := NEW(GridPointSetDef.T).init();
      EVAL self.byTagTbl.put(tag,pointSet)
    END;

    WHILE p # NIL DO
      (* check that p.head and op.head are neighbors *)
      <* ASSERT op = NIL OR GridPoint.AreNeighbors(op.head,p.head) *>

      hadStatus := self.get(p.head,oldStatus);
      
      (* it is either the first thing at this point, OR
         the starting point of the routed path, OR
         the ending point of the routed path *)
         
      <* ASSERT op = NIL OR p.tail = NIL OR NOT hadStatus *>

      (* if we had something it better be for the same route, or else
         we're shorting things together *)
      
      IF hadStatus THEN <* ASSERT oldStatus.getTag().equal(tag) *> END;

      IF DoDebug THEN 
        Debug.Out("CurrentlyRoutedTbl tagging " & GridPoint.Format(p.head) &
          " \"" & tag.format()&"\"") 
      END;

      EVAL pointSet.insert(p.head);

      IF NOT hadStatus THEN
        oldStatus := NEW(GridPointStatus.T).init(p.head,
                                                 op = NIL OR p.tail = NIL,
                                                 tag := tag);
        EVAL self.put(p.head,oldStatus)
      END;

      (* except for first point, add neighbor data *)
      IF op # NIL THEN
        oldStatus.addANeighbor(op.head);
        opOldStatus.addANeighbor(p.head)
      END;

      op := p; opOldStatus := oldStatus;
      p := p.tail
    END
  END AddPointList;

PROCEDURE DelPointList(self : T; gpl : GridPointList.T) =
  VAR 
    op : GridPointList.T := NIL;
    p := gpl;
    hadStatus : BOOLEAN;
    oldStatus, opOldStatus : GridPointStatus.T;
  BEGIN
    WHILE p # NIL DO
      (* check that p.head and op.head are neighbors *)
      <* ASSERT op = NIL OR GridPoint.AreNeighbors(op.head,p.head) *>

      hadStatus := self.get(p.head,oldStatus);

      <* ASSERT hadStatus *>

      (* except for first point, delete neighbor data *)
      IF op # NIL THEN
        Debug.Out("CurrentlyRoutedTbl shrinking status of " & 
          GridPoint.Format(op.head) & " and of "&
          GridPoint.Format(p.head));
        oldStatus.delANeighbor(op.head);
        opOldStatus.delANeighbor(p.head);

        (* if it shrinks to zero, delete it *)
        IF opOldStatus.size() = 0 THEN
          VAR
            dummy : GridPointStatus.T;
          BEGIN
            IF DoDebug THEN 
              Debug.Out("CurrentlyRoutedTbl deleting " & 
                GridPoint.Format(op.head)) 
            END;
            EVAL self.delete(op.head,dummy)
          END
        ELSE 
          IF DoDebug THEN
              Debug.Out("CurrentlyRoutedTbl not deleting " & 
                GridPoint.Format(op.head) & " (status at "&Fmt.Int(opOldStatus.size())&")")
          END
        END
      END;

      op := p; opOldStatus := oldStatus;
      p := p.tail
    END;

    (* if last one shrinks to zero, delete it *)
    IF opOldStatus.size() = 0 THEN
      VAR
        dummy : GridPointStatus.T;
      BEGIN
        IF DoDebug THEN 
          Debug.Out("CurrentlyRoutedTbl deleting (last) " & 
            GridPoint.Format(op.head)) 
        END;
        EVAL self.delete(op.head,dummy)
      END
    ELSE 
      IF DoDebug THEN
        Debug.Out("CurrentlyRoutedTbl not deleting " & 
          GridPoint.Format(op.head) & " (status at "&Fmt.Int(opOldStatus.size())&")") 
      END
    END
  END DelPointList;

BEGIN END CurrentlyRoutedTbl.
