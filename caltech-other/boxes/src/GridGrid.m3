(* $Id$ *)

MODULE GridGrid;
IMPORT GridPoint, GridTbl;
IMPORT GridPointList;
IMPORT Debug;

VAR DoDebug := Debug.DebugThis("GridGrid");

REVEAL 
  T = Public BRANDED Brand OBJECT
    p : GridPoint.T;
    neighs : GridPointList.T := NIL;
    tag : REFANY;
  OVERRIDES
    neighbors := Neighbors;
  END;

VAR 
  mu := NEW(MUTEX);
  tbl := NEW(GridTbl.Default).init();

PROCEDURE Neighbors(s : T) : GridPointList.T =
  BEGIN RETURN s.neighs END Neighbors;

PROCEDURE PointStatus(READONLY p : GridPoint.T) : T =
  VAR
    t : T := NIL;
  BEGIN
    EVAL tbl.get(p,t);
    RETURN t
  END PointStatus;

PROCEDURE MarkNeighbors(READONLY p, q : GridPoint.T; tag : REFANY) =

  PROCEDURE Mark(READONLY a , b : GridPoint.T) =
    VAR
      s : T;
    BEGIN
      IF DoDebug THEN
        Debug.Out("GridGrid.MarkNeighbors: marking " & GridPoint.Format(a) &
          " -> " & GridPoint.Format(b))
      END;
      IF NOT tbl.get(a,s) THEN
        s := NEW(T, p := a, tag := tag);
        EVAL tbl.put(a,s)
      END;
      <* ASSERT s.p = a *>
      <* ASSERT s.tag = tag *>
      <* ASSERT NOT GridPointList.Member(s.neighs,b) *>
      s.neighs := GridPointList.Cons(b,s.neighs)
    END Mark;

  BEGIN
    LOCK mu DO Mark(p,q); Mark(q,p) END
  END MarkNeighbors;

BEGIN END GridGrid.
