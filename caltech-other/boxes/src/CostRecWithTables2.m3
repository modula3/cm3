(* $Id$ *)

MODULE CostRecWithTables2 EXPORTS Cost;
IMPORT GridPointRefTbl;
IMPORT GridPoint, CostRec, GridPointCostRecTbl;

VAR tbl : GridPointRefTbl.T;

CONST inftyPt = GridPoint.T { FIRST(INTEGER), FIRST(INTEGER), 
                              FIRST(GridPoint.Layer) };


PROCEDURE GetRec(READONLY p, tgt : GridPoint.T) : CostRec.T =
  VAR
    res : CostRec.T;
    ttbl : REFANY;
  BEGIN
    IF tbl.get(tgt, ttbl) AND 
       NARROW(ttbl,GridPointCostRecTbl.T).get(p, res) THEN
      RETURN res
    ELSE
      RETURN CostRec.T { inftyPt, LAST(INTEGER) }
    END
  END GetRec;

PROCEDURE Mark(p, previous, tgt : GridPoint.T; d : INTEGER) RAISES { OutOfMemory }=
  VAR
    ttbl : REFANY;
  BEGIN
    IF NOT tbl.get(tgt, ttbl) THEN
      ttbl := NEW(GridPointCostRecTbl.Default).init();
      EVAL tbl.put(tgt, ttbl)
    END;
    WITH t = NARROW(ttbl,GridPointCostRecTbl.T) DO

      IF memLimit # 0 AND t.size() >= memLimit THEN RAISE OutOfMemory END;

      EVAL t.put(p, CostRec.T { previous, d });
      costRecSizeHWM := MAX(costRecSizeHWM, t.size())
    END
  END Mark;

VAR memLimit : CARDINAL;

PROCEDURE InitRoute(memLimitArg : CARDINAL) =
  BEGIN 
    memLimit := memLimitArg;
    tbl := NEW(GridPointRefTbl.Default).init();
    costRecSizeHWM := 0
  END InitRoute;

BEGIN END CostRecWithTables2.
