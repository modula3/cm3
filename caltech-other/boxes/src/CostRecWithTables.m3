(* $Id$ *)

MODULE CostRecWithTables EXPORTS Cost;
IMPORT CostRec;
IMPORT GridPoint,GridPointPair;
IMPORT Loc;
IMPORT CostRecTbl;
IMPORT Step;
IMPORT Debug, Fmt;

VAR
  tbl : CostRecTbl.T;
  inftyPt := GridPoint.T { FIRST(INTEGER), FIRST(INTEGER), 
                           FIRST(GridPoint.Layer) };

  DoDebug := Debug.DebugThis("CostRec");

PROCEDURE GetRec(READONLY p, tgt : GridPoint.T) : CostRec.T =
  VAR
    res : CostRec.T;
  BEGIN
    IF tbl.get(GridPointPair.T { p, tgt }, res) THEN 
      IF DoDebug THEN
        Debug.Out("CostRecWithTables.GetRec: p=" & GridPoint.Format(p) &
          " tgt=" & GridPoint.Format(tgt) &
          " previous=" & GridPoint.Format(res.previous) &
          " minCost=" & Fmt.Int(res.loc.minCost))
      END;      
      RETURN res 
    ELSE
      (* what's this good for? *)
      RETURN CostRec.T { inftyPt, inftyPt, LAST(INTEGER) }
    END
  END GetRec;

PROCEDURE InitRoute() =
  BEGIN tbl := NEW(CostRecTbl.Default).init() END InitRoute;

PROCEDURE Mark(from, p, previous, tgt : GridPoint.T; d : INTEGER) =
  BEGIN 
    IF DoDebug THEN
      Debug.Out("CostRecWithTables.Mark: from=" & GridPoint.Format(from) &
                                       " p=" & GridPoint.Format(p) &
                                       " previous=" & GridPoint.Format(previous) &
                                       " tgt=" & GridPoint.Format(tgt) &
                                       " d=" & Fmt.Int(d));
    END;
    EVAL tbl.put(GridPointPair.T { p, tgt }, 
                 CostRec.T { from, previous, d }) 
  END Mark;

PROCEDURE MarkAsSearched(READONLY p, tgt : GridPoint.T) =
  VAR
    x : CostRec.T;
  BEGIN
    IF tbl.get(GridPointPair.T { p, tgt }, x) THEN
      x.searched := TRUE;
      EVAL tbl.put(GridPointPair.T { p, tgt }, x)
    END
  END MarkAsSearched;

BEGIN END CostRecWithTables.
