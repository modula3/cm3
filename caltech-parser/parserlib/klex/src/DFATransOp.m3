(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: DFATransOp.m3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

MODULE DFATransOp;
IMPORT DFATrans;
IMPORT DFATransList;
IMPORT DFATransIntTbl;
IMPORT IntInt;
IMPORT IntIntTbl;
IMPORT IntIntArraySort;

PROCEDURE GetTarget(t: T; c: CHAR): INTEGER =
  VAR
    cur := t;
    trans: DFATrans.T;
  BEGIN
    WHILE cur # NIL DO
      trans := cur.head;
      IF c >= trans.keyBegin AND c <= trans.keyEnd THEN
        RETURN trans.target;
      END;
      cur := cur.tail;
    END;
    <* ASSERT c = '\000' *>
    <* ASSERT FALSE *>
  END GetTarget;

PROCEDURE CheckEqual(a, b: T): BOOLEAN =
  BEGIN
    FOR i := VAL(1, CHAR) TO LAST(CHAR) DO
      IF GetTarget(a, i) # GetTarget(b, i) THEN
        RETURN FALSE;
      END;
    END;
    RETURN TRUE;
  END CheckEqual;

PROCEDURE Overlap(keyBegin, keyEnd: CHAR; t: T): BOOLEAN =
  VAR
    cur := t;
  BEGIN
    <* ASSERT keyBegin <= keyEnd *>
    WHILE cur # NIL DO
      IF MAX(keyBegin, cur.head.keyBegin) <= MIN(keyEnd, cur.head.keyEnd) THEN
        RETURN TRUE;
      END;
      cur := cur.tail;
    END;
    RETURN FALSE;
  END Overlap;

(*on entry assume t is sorted decreasing by key*)
PROCEDURE Simplify(t: T): T =
  VAR
    count := NEW(IntIntTbl.Default).init();
    countSort: REF ARRAY OF IntInt.T;
    cur := t;
    targ, n: INTEGER;
    iter: IntIntTbl.Iterator;
    numTargs: INTEGER := 0;
    lastSeg, thisSeg: DFATrans.T;
    result, paintedSegs, prevPaintedSegs: T := NIL;
  BEGIN
    WHILE cur # NIL DO
      targ := cur.head.target;
      IF count.get(targ, n) THEN
        EVAL count.put(targ, n+1);
      ELSE
        EVAL count.put(targ, 1);
        INC(numTargs);
      END;
      cur := cur.tail;
    END;
    iter := count.iterate();
    countSort := NEW(REF ARRAY OF IntInt.T, numTargs);
    FOR i := 0 TO numTargs -1 DO
      EVAL iter.next(targ, n);
      countSort[i] := IntInt.T{key := n, value := targ};
    END;
    <* ASSERT NOT iter.next(targ, n) *>
    IntIntArraySort.Sort(countSort^);
    FOR i := numTargs-1 TO 0 BY -1 DO
      targ := countSort[i].value;
      cur := t;
      lastSeg.target := -1;
      WHILE cur # NIL DO
        IF cur.head.target = targ THEN
          thisSeg := cur.head;
          IF lastSeg.target = -1 THEN
            lastSeg := thisSeg;
          ELSIF Overlap(thisSeg.keyEnd,
                        lastSeg.keyBegin,
                        prevPaintedSegs) THEN
             result := DFATransList.Cons(lastSeg, result);
             lastSeg := thisSeg;
          ELSE
            lastSeg.keyBegin := thisSeg.keyBegin;
          END;
          paintedSegs := DFATransList.Cons(thisSeg, paintedSegs);
        END;
        cur := cur.tail;
      END;
      prevPaintedSegs := paintedSegs;
      IF lastSeg.target # -1 THEN
        result := DFATransList.Cons(lastSeg, result);
      END;
    END;
    <* ASSERT CheckEqual(result, t) *>
    RETURN result;
  END Simplify;

PROCEDURE Tally(table: DFATransIntTbl.T; t: T) =
  VAR
    cur := t;
    n: INTEGER;
  BEGIN
    WHILE cur # NIL DO
      n := 0;
      EVAL table.get(cur.head, n);
      EVAL table.put(cur.head, n+1);
      cur := cur.tail;
    END;
  END Tally;

PROCEDURE Unordered(a, b: DFATrans.T): BOOLEAN =
  BEGIN
    IF MAX(a.keyBegin, b.keyBegin) <= MIN(a.keyEnd, b.keyEnd) THEN
      RETURN FALSE; (* intervals do not commute *)
    END;
    RETURN a.prio > b.prio;
  END Unordered;

PROCEDURE Sort(table: DFATransIntTbl.T; t: T) =
  VAR
    cur := t;
    ordered: BOOLEAN;
    temp: DFATrans.T;
  BEGIN
    <* ASSERT t # NIL *>
    WHILE cur # NIL DO
      IF NOT table.get(cur.head, cur.head.prio) THEN
        <* ASSERT FALSE *>
      END;
      cur := cur.tail;
    END;
    REPEAT
      ordered := TRUE;
      cur := t;
      WHILE cur.tail # NIL DO
        IF Unordered(cur.head, cur.tail.head) THEN
          temp := cur.head;
          cur.head := cur.tail.head;
          cur.tail.head := temp;
          ordered := FALSE;
        END;
        cur := cur.tail;
      END;
    UNTIL ordered;
  END Sort;

BEGIN
END DFATransOp.
