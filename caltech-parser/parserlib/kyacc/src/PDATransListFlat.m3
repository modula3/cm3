(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: PDATransListFlat.m3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

MODULE PDATransListFlat;
IMPORT PDATrans;
IMPORT PDATransIntTbl;
IMPORT PDATransList;
IMPORT PDATransListIntTbl;
IMPORT PDATransTally;
IMPORT PDATransTallyArraySort;
IMPORT RuleList;
IMPORT Fmt;
IMPORT FmtTable;
IMPORT Wr;
FROM Stdio IMPORT stderr;
IMPORT Thread;
<* FATAL Wr.Failure, Thread.Alerted *>

PROCEDURE SortTrans(VAR tr: PDATransList.T; tally: PDATransIntTbl.T) =
  VAR
    len := PDATransList.Length(tr);
    a := NEW(REF ARRAY OF PDATransTally.T, len);
    cur := tr;
  BEGIN
    FOR i := 0 TO LAST(a^) DO
      IF NOT tally.get(cur.head, a[i].key) THEN <* ASSERT FALSE *> END;
      a[i].tr := cur.head;
      cur := cur.tail;
    END;
    <* ASSERT cur = NIL *>
    PDATransTallyArraySort.Sort(a^);
    FOR i := LAST(a^) TO 0 BY -1 DO
      cur := PDATransList.Cons(a[i].tr, cur);
    END;
    tr := cur;
  END SortTrans;

PROCEDURE Flatten(VAR a: T) =
  VAR
    tally := NEW(PDATransIntTbl.Default).init(LAST(a^));
    tails := NEW(PDATransListIntTbl.Default).init(LAST(a^));
    count: INTEGER;
    cur: PDATransList.T;
    moreStates: T;
    numStates: INTEGER := 0;
    ID, i: INTEGER;
  BEGIN
    (* get transition frequencies *)
    FOR j := 1 TO LAST(a^) DO
      cur := a[j];
      WHILE cur # NIL DO
        count := 0;
        EVAL tally.get(cur.head, count);
        INC(count);
        EVAL tally.put(cur.head, count);
        cur := cur.tail;
        INC(numStates);
      END;
    END;

    (* sort transition lists by increasing frequency, and collect tails *)
    moreStates := NEW(T, numStates + 1);
    SUBARRAY(moreStates^, 0, LAST(a^)+1) := a^;
    numStates := LAST(a^);
    i:= 1;
    WHILE i <= numStates DO
      SortTrans(moreStates[i], tally);
      cur := moreStates[i];
      IF cur.tail = NIL THEN
        ID := 0;
      ELSIF NOT tails.get(cur.tail, ID) THEN
        INC(numStates);
        ID := numStates;
        EVAL tails.put(cur.tail, ID);
        moreStates[ID] := cur.tail;
      END;        
      cur.tail := PDATransList.List1(
                      PDATrans.T{code := -2,
                                 kind := PDATrans.ActKind.Jump,
                                 target := ID});
      INC(i);
    END;
    a := NEW(T, numStates + 1);
    a^ := SUBARRAY(moreStates^, 0, numStates+1);
    a[0] := NIL;
  END Flatten;

PROCEDURE Warn(message: TEXT) =
  BEGIN
    Wr.PutText(stderr, message & "\n");
  END Warn;

PROCEDURE UnReducedWarning(a: T; rules: RuleList.T) =
  VAR
    numRules := RuleList.Length(rules);
    used := NEW(REF ARRAY OF BOOLEAN, numRules+1);
    state: PDATrans.T;
  BEGIN
    FOR j := 0 TO numRules DO
      used[j] := FALSE;
    END;
    FOR i := 1 TO LAST(a^) DO
      <* ASSERT a[i].tail.tail = NIL *>
      state := a[i].head;
      CASE state.kind OF
      | PDATrans.ActKind.Accept, PDATrans.ActKind.ShiftAccept =>
        used[0] := TRUE; <* ASSERT state.target = 0 *>
      | PDATrans.ActKind.Reduce, PDATrans.ActKind.ShiftReduce =>
        used[state.target] := TRUE;
      ELSE
      END;
    END;
    FOR j := 0 TO numRules DO
      IF NOT used[j] THEN
        IF j = 0 THEN
          Warn("No start symbol accepted");
        ELSE
          Warn("Rule never reduced: " & RuleList.Nth(rules, j-1).name);
        END;
      END;
    END;
  END UnReducedWarning; 

PROCEDURE Format(a: T; aCode, saCode, defSymCode: INTEGER): TEXT =
  VAR
    fmt := NEW(FmtTable.T).init();
    state: PDATrans.T;
    jump: PDATrans.T;
    key, actCode: INTEGER;
  BEGIN
    FOR i := 1 TO LAST(a^) DO
      <* ASSERT a[i].tail.tail = NIL *>
      state := a[i].head;
      jump := a[i].tail.head;
      <* ASSERT jump.kind = PDATrans.ActKind.Jump *>
      CASE state.kind OF
      | PDATrans.ActKind.Shift => actCode := state.target;
      | PDATrans.ActKind.Accept => actCode := aCode;
      | PDATrans.ActKind.Reduce => actCode := aCode + state.target;
      | PDATrans.ActKind.ShiftAccept => actCode := saCode;
      | PDATrans.ActKind.ShiftReduce => actCode := saCode + state.target;
      ELSE
        <* ASSERT FALSE *>
      END;
      key := state.code;
      IF key = -1 THEN
        key := defSymCode;
      END;
      fmt.putText("S{" & Fmt.Int(key) & "," &
        Fmt.Int(actCode) & "," & Fmt.Int(jump.target) & "}");
    END;
    RETURN fmt.toText();
  END Format;

BEGIN
END PDATransListFlat.
