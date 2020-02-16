(* $Id$ *)

MODULE ForbiddenSteps;
IMPORT GridPoint_GridPointSetTbl AS PPSetTbl;
IMPORT GridPoint, GridPointSet, GridPointSetDef;

REVEAL 
  T = Public BRANDED Brand OBJECT 
    data : PPSetTbl.T;
  OVERRIDES
    init        := Init;
    addPair     := AddPair;
    isForbidden := IsForbidden;
    copy        := Copy;
  END;

PROCEDURE Init(self : T) : T = 
  BEGIN self.data := NEW(PPSetTbl.Default).init(); RETURN self END Init;

PROCEDURE AddPair(self : T; READONLY a, b : GridPoint.T) = 

  PROCEDURE AddIt(READONLY x, y : GridPoint.T) = 
    VAR
      xs : GridPointSet.T;
    BEGIN
      IF NOT self.data.get(x,xs) THEN 
        xs := NEW(GridPointSetDef.T).init();
        EVAL self.data.put(x,xs)
      END;
      EVAL xs.insert(y)
    END AddIt;

  BEGIN AddIt(a,b); AddIt(b,a) END AddPair;

PROCEDURE IsForbidden(self : T; READONLY a, b : GridPoint.T) : BOOLEAN =
  VAR
    s : GridPointSet.T;
  BEGIN
    IF NOT self.data.get(a,s) OR NOT s.member(b) THEN 
      RETURN FALSE
    ELSE
      RETURN TRUE
    END
  END IsForbidden;

PROCEDURE Copy(t : T) : T =
  VAR
    res := NEW(T).init();
    iter := t.data.iterate();
    gp : GridPoint.T;
    gps : GridPointSet.T;
  BEGIN
    WHILE iter.next(gp,gps) DO
      EVAL res.data.put(gp,gps.copy())
    END;
    RETURN res
  END Copy;

BEGIN END ForbiddenSteps.
