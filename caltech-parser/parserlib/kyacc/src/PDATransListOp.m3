(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: PDATransListOp.m3,v 1.2 2001-09-19 15:13:58 wagner Exp $ *)

MODULE PDATransListOp;
IMPORT PDATrans;
IMPORT PDATransList;
IMPORT PDATransListF;
IMPORT PDATransListIntTbl;
IMPORT IntIntTbl;

IMPORT Term, Fmt;

CONST
  (* can this action be performed by default? *)
  Defable = SET OF PDATrans.ActKind{PDATrans.ActKind.Reduce,
                                    PDATrans.ActKind.Accept};

PROCEDURE Simplify(a: T): T =
  VAR
    scores := NEW(IntIntTbl.Default).init();
    cur := a;
    tr: PDATrans.T;
    score: INTEGER;
    hiScorer: PDATrans.T;
    hiScore: INTEGER := 0;
    result: T;
  BEGIN
    WHILE cur # NIL DO
      tr := cur.head;
      IF tr.kind IN Defable THEN
        score := 0;
        EVAL scores.get(tr.target, score);
        INC(score);
        EVAL scores.put(tr.target, score);
        IF score > hiScore THEN
          hiScorer := tr;
          hiScore := score;
        END;
      END;
      cur := cur.tail;
    END;
    IF hiScore = 0 THEN RETURN PDATransList.ReverseD(a); END;
    hiScorer.code := -1;
    cur := a;
    WHILE cur # NIL DO
      tr := cur.head;
      IF (NOT tr.kind IN Defable) OR tr.target # hiScorer.target THEN
        result := PDATransList.Cons(tr, result);
      END;
      cur := cur.tail;
    END;
    result := PDATransList.Cons(hiScorer, result);
    RETURN result;
  END Simplify;

PROCEDURE PrintArray(a: REF ARRAY OF T; len: INTEGER := 0) =
  VAR
    actual := len;
  BEGIN
    IF actual = 0 THEN actual := LAST(a^); END;
    FOR i := 1 TO actual DO
      Term.WrLn(Fmt.Int(i) & ": " &
        PDATransListF.Format(a[i]));
    END;
  END PrintArray;

PROCEDURE Collapsable(a: T): BOOLEAN =
  BEGIN
    RETURN a.tail = NIL AND a.head.kind IN Defable;
  END Collapsable;

PROCEDURE MergeStates(VAR a: REF ARRAY OF T) =
  VAR
    cur: T;
    result: REF ARRAY OF T;
    numStates: INTEGER := LAST(a^);
    newNumStates: INTEGER;
    renumber := NEW(REF ARRAY OF INTEGER, numStates+1);
    ID, i, iOriginal: INTEGER;
    canonID: PDATransListIntTbl.T;
    fixed: BOOLEAN;
  PROCEDURE ReNumberShifts() =
    BEGIN
      numStates := newNumStates;
      FOR i := 1 TO numStates DO
        cur := a[i];
        WHILE cur # NIL DO
          IF cur.head.kind = PDATrans.ActKind.Shift THEN
            cur.head.target := renumber[cur.head.target];
          END;
          cur := cur.tail;
        END;
      END;
      (*      Term.WrLn("renumbered: ");PrintArray(a, numStates); *)
    END ReNumberShifts;
  BEGIN
    (* Term.WrLn("start with: ");PrintArray(a, numStates); *)
    REPEAT
      fixed := TRUE;
      canonID := NEW(PDATransListIntTbl.Default).init();
      newNumStates := numStates;
      i := 1;
      iOriginal := 1;
      WHILE i <= newNumStates DO
        ID := i;
        IF canonID.get(a[i], ID) THEN
          fixed := FALSE;
          (* Term.WrLn("found " & PDATransListF.Format(a[i]) &
            " with ID = " & Fmt.Int(ID)); *)
        ELSE
          EVAL canonID.put(a[i], ID);
          (* Term.WrLn("putting " & PDATransListF.Format(a[i]) &
            " with ID = " & Fmt.Int(ID)); *)
        END;
        renumber[iOriginal] := ID;
        IF i = ID THEN
          INC(i);
          iOriginal := i;
        ELSE
(*
          Term.Wr("   ");
          IF iOriginal # i THEN
            Term.Wr("re-");
          END;
          Term.WrLn("renumber " & Fmt.Int(iOriginal) & " -> " & Fmt.Int(ID) &
            ". Delete: renumber " & Fmt.Int(newNumStates) &
            " -> " & Fmt.Int(i));
*)
          iOriginal := newNumStates;
          a[i] := a[iOriginal];
          DEC(newNumStates);
        END;
      END;
      ReNumberShifts();
    UNTIL fixed;
    
    (* make anonymous reductions *)
    FOR i := 1 TO numStates DO
        cur := a[i];
        WHILE cur # NIL DO
          IF cur.head.kind = PDATrans.ActKind.Shift THEN
            WITH targ = a[cur.head.target] DO
              IF Collapsable(targ) THEN
                cur.head := PDATrans.PreShift(targ.head, cur.head.code);
              END;
            END;
          END;
          cur := cur.tail;
        END;      
    END;
    newNumStates := numStates;
    i := 2; (* don't eliminate the start state! *)
    WHILE i <= newNumStates DO
      IF Collapsable(a[i]) THEN
        a[i] := a[newNumStates];
        renumber[newNumStates] := i;
        DEC(newNumStates);
      ELSE
        INC(i);
      END;
    END;
    ReNumberShifts();

    result := NEW(REF ARRAY OF T, numStates+1);
    result^ := SUBARRAY(a^, 0, numStates+1);
    (* PrintArray(result, numStates); *)
    a := result;
  END MergeStates;

BEGIN
END PDATransListOp.
