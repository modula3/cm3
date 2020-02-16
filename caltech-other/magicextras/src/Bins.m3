(* $Id$ *)

MODULE Bins;
IMPORT MagRect AS Rect, IntPairList, IntPair, IntPairSet;

PROCEDURE Compute(step : CARDINAL; rect : Rect.T) : IntPairList.T =
  VAR
    minx := (rect.ll.x - 1) DIV step;
    miny := (rect.ll.y - 1) DIV step;
    maxx := (rect.ur.x)     DIV step;
    maxy := (rect.ur.y)     DIV step;
    res : IntPairList.T := NIL;
  BEGIN
    FOR i := minx TO maxx DO
      FOR j := miny TO maxy DO
        res := IntPairList.Cons( IntPair.T { i , j } , res )
      END
    END;
    RETURN res
  END Compute;

PROCEDURE ComputeArray(step : CARDINAL; 
                       rect : Rect.T) : REF ARRAY OF IntPair.T =
  VAR
    minx := (rect.ll.x - 1) DIV step;
    miny := (rect.ll.y - 1) DIV step;
    maxx := (rect.ur.x)     DIV step;
    maxy := (rect.ur.y)     DIV step;
    n := (maxx - minx + 1) * (maxy - miny + 1);
    res := NEW(REF ARRAY OF IntPair.T, n);
  BEGIN
    FOR i := minx TO maxx DO
      FOR j := miny TO maxy DO
        res[(i - minx) * (maxy - miny + 1) + j - miny] := IntPair.T { i , j }
      END
    END;
    RETURN res
  END ComputeArray;
    
PROCEDURE ComputeAndAddToSet(step : CARDINAL; 
                       rect : Rect.T; set : IntPairSet.T) =
  VAR
    minx := (rect.ll.x - 1) DIV step;
    miny := (rect.ll.y - 1) DIV step;
    maxx := (rect.ur.x)     DIV step;
    maxy := (rect.ur.y)     DIV step;
  BEGIN
    FOR i := minx TO maxx DO
      FOR j := miny TO maxy DO
        EVAL set.insert(IntPair.T { i , j })
      END
    END
  END ComputeAndAddToSet;
    
(************************************************************************)

REVEAL
  Iterator = PublicIterator BRANDED "Bins Iterator" OBJECT
    array : REF ARRAY OF IntPair.T := NIL;
    i, n : INTEGER := -1;
  OVERRIDES
    next := IterNext;
  END;
 
PROCEDURE IterNext(self : Iterator; VAR s : IntPair.T) : BOOLEAN =
  BEGIN
    IF self.i = -1 THEN 
      <* ASSERT FALSE *> 
    ELSIF self.i = self.n THEN
      RETURN FALSE
    ELSE
      s := self.array[self.i];
      INC(self.i);
      RETURN TRUE
    END
  END IterNext;  

PROCEDURE ComputeIterator(step : CARDINAL; rect : Rect.T;
                          VAR res : Iterator) =
  VAR
    minx := (rect.ll.x - 1) DIV step;
    miny := (rect.ll.y - 1) DIV step;
    maxx := (rect.ur.x)     DIV step;
    maxy := (rect.ur.y)     DIV step;
    n := (maxx - minx + 1) * (maxy - miny + 1);
  BEGIN
    IF res = NIL OR res.array = NIL OR NUMBER(res.array^) < n THEN
      res := NEW(Iterator, array := NEW(REF ARRAY OF IntPair.T, n))
    END;

    FOR i := minx TO maxx DO
      FOR j := miny TO maxy DO
        res.array[(i - minx) * (maxy - miny + 1) + j - miny] := 
            IntPair.T { i , j }
      END
    END;
    res.n := n;
    res.i := 0
  END ComputeIterator;

BEGIN END Bins.
