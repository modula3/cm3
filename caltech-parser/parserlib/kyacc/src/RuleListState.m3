(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

MODULE RuleListState;
IMPORT Pos;
IMPORT Mark;
IMPORT MarkList;
IMPORT MarkListF;
IMPORT MarkListSort;
IMPORT MarkBoolTbl;
IMPORT Sym;
IMPORT Rule;
IMPORT RuleList;
IMPORT IntRuleListTbl;
IMPORT PDATrans;
IMPORT Fmt;
IMPORT TextTextTbl;

(* IMPORT Term; *)

TYPE
  StartStatus = {FirstState, SingleStartSym, None};
REVEAL
  T = Public BRANDED OBJECT
    call: IntRuleListTbl.T := NIL; (* rules called by a symbol code *)
    marksList: MarkList.T := NIL;
    marksTab: MarkBoolTbl.T := NIL;
    finish: Rule.T := NIL; (* first rule to reduce to complete, if exists *)
    start := StartStatus.None; (* detect single start symbol *)
    prev: T := NIL; symName: TEXT; (* for debugging *)
    warnings: TextTextTbl.T;
  END;

PROCEDURE ClearMarksTable(self: T; est: INTEGER) =
  BEGIN
(*    Term.WrLn(Fmt.Int(est)); *)
    self.marksTab := NEW(MarkBoolTbl.Default).init(est);
  END ClearMarksTable;

(*
PROCEDURE TestMark(mark: Mark.T; a, b, c, d, e: INTEGER): BOOLEAN =
  BEGIN
    RETURN Rule.Number(mark.current.rule) = a AND
           mark.current.index = b AND
           Rule.Number(mark.return.rule) = c AND
           mark.return.index = d AND
           Rule.Number(mark.first) = e;
  END TestMark;

PROCEDURE DebugMark(mark1, mark2: Mark.T; when: TEXT) =
  BEGIN
    IF TestMark(mark2, 9, 0, 6, 2, 0) OR
TestMark(mark2, 10, 1, 6, 2, 0) OR
TestMark(mark2, 10, 0, 6, 2, 0)
THEN
      Term.WrLn("DebugMark: " & Mark.Format(mark1) & " yielded " &
        Mark.Format(mark2) & " in " & when);
    END;
  END DebugMark;
*)


(* add a mark to table, chasing epsilon calls/returns *)
PROCEDURE AddMark(self: T; READONLY mark: Mark.T) =
  VAR
    bool: BOOLEAN;
    cur: RuleList.T;
    callSym: Sym.T;
    first: Rule.T;
    return: Pos.T;
  BEGIN
(*          IF mark.current.rule.number = 10 AND
            mark.current.index = 2 THEN
            Term.WrLn("Debug2: " & Mark.Format(mark));
            <* ASSERT mark.current.cell = NIL *>
            <* ASSERT mark.return.rule # NIL *>
            <* ASSERT mark.current.rule # NIL *>
            first := mark.first;
            IF first = NIL THEN
              first := mark.current.rule;
            END;
            Term.WrLn("Should get: " &
              Mark.Format(Mark.T{current := mark.return,
                                 return := Pos.Error,
                                 first := first,
                                 baseRule := mark.return.rule}));
          END; 
*)
    IF mark.current.cell = NIL THEN
      (* return without adding to table *)
      IF mark.return.rule = NIL THEN
        <* ASSERT mark.return.index # -1 *>
        self.finish := mark.current.rule;
      ELSE
        first := mark.first;
        IF first = NIL THEN
          first := mark.current.rule;
        END;

(*        DebugMark(mark, Mark.T{current := mark.return,
                             return := Pos.Error, (* disable further steps *)
                             first := first,
                             baseRule := mark.return.rule}, "return");
*)

        AddMark(self, Mark.T{current := mark.return,
                             return := Pos.Error, (* disable further steps *)
                             first := first,
                             baseRule := mark.return.rule});
      END;
    ELSIF NOT self.marksTab.get(mark, bool) THEN
      (* add to table *)
      EVAL self.marksTab.put(mark, TRUE);
      
      (* call 0 or more rules *)
      callSym := mark.current.cell.head;
      cur := NIL;
      EVAL self.call.get(Sym.GetCode(callSym), cur);
      return := Pos.Advance(mark.current);
      IF return.cell = NIL THEN
        return := mark.return; (* tail recursion *)
      END;

    (*  Term.WrLn("Searching for pos " & Pos.Format(mark.current) &
        ", code " & Fmt.Int(Sym.GetCode(callSym))); *)

      WHILE cur # NIL DO

(* DebugMark(mark, Mark.T{current := Pos.Zero(cur.head),
                             return := return,
                             first := mark.first,
                             baseRule := mark.baseRule}, "call"); *)

        AddMark(self, Mark.T{current := Pos.Zero(cur.head),
                             return := return,
                             first := mark.first,
                             baseRule := mark.baseRule});
        cur := cur.tail;
      END;
    END;
  END AddMark;

(* copy marks table to a list and sort it *)
PROCEDURE BuildMarksList(self: T; VAR est: INTEGER) =
  VAR
    iter := self.marksTab.iterate();
    mark: Mark.T;
    bool: BOOLEAN;
    newEst: INTEGER;
  BEGIN
    newEst := 0;
    self.marksList := NIL;
    WHILE iter.next(mark, bool) DO
      self.marksList := MarkList.Cons(mark, self.marksList);      
      INC(newEst);
    END;
    self.marksList := MarkListSort.SortD(self.marksList);
    est := (newEst + est) DIV 2;
  END BuildMarksList;

PROCEDURE New(r: RuleList.T; warnings: TextTextTbl.T): T =
  VAR
    self := NEW(T, start := StartStatus.FirstState,
                warnings := warnings);
    cur := r;
    rule: Rule.T;
    key: INTEGER;
    value: RuleList.T;
  BEGIN
    self.call := NEW(IntRuleListTbl.Default).init(RuleList.Length(r));
    (* build call table *)
    WHILE cur # NIL DO
      rule := cur.head;
      key := Sym.GetCode(rule.return);
      value := NIL;
      EVAL self.call.get(key, value);
      EVAL self.call.put(key, RuleList.Cons(rule, value));
      cur := cur.tail;
    END;
    (* set inital marks *)
    cur := r;
    WHILE cur # NIL DO
      rule := cur.head;
      IF Sym.IsStart(rule.return) THEN
        self.marksList := MarkList.Cons(Mark.T{current := Pos.Zero(rule),
                                               return := Pos.Null,
                                               first := NIL,
                                               baseRule := rule},
                                        self.marksList);
      END;
      cur := cur.tail;
    END;
    self.marksList := MarkListSort.SortD(self.marksList);
    RETURN self;
  END New;

PROCEDURE Expand(self: T; VAR est: INTEGER) =
  VAR
    cur := self.marksList;
  BEGIN
    ClearMarksTable(self, est);
    WHILE cur # NIL DO
      AddMark(self, cur.head);
      cur := cur.tail;
    END;
    BuildMarksList(self, est);
  END Expand;

PROCEDURE Step(from: T; code: INTEGER; symName: TEXT): Action =
  VAR
    next := NEW(T, call := from.call, prev := from,
                symName := symName, warnings := from.warnings);
    cur := from.marksList;
    mark: Mark.T;
    prefin := from.finish;
    precShift: Rule.T; (* rule whose precedence we compare *)
    leftoverShift: BOOLEAN := FALSE; (* newly started shift w/o compare prec*)
    highestShift: Rule.T := NIL;
    highestReduce: Rule.T := NIL;
    redConflict: Rule.T := NIL;
    dummy: RuleList.T;
    noMatch: BOOLEAN;
  BEGIN
    WHILE cur # NIL DO
      mark := cur.head;
      IF Sym.GetCode(mark.current.cell.head) = code THEN
        IF mark.return.index # -1 THEN
          next.marksList := MarkList.Cons(Mark.Advance(mark), next.marksList);
        END;

        CASE Rule.Compare(mark.first, highestReduce) OF
        | 1 =>
          highestReduce := mark.first;
          redConflict := NIL;
        | 0 =>
          redConflict := mark.first;
        | -1 =>
        END;
        
        IF mark.first = NIL THEN
          (* if no reductions are necessary for this rule... *)
          (* Term.WrLn(mark.current.rule.name & " required no reductions."); *)

(*          IF mark.current.index # 0 THEN
            precShift := mark.current.rule;
            <* ASSERT precShift = mark.baseRule *>
          ELSIF mark.return.rule # NIL AND mark.return.index > 0 THEN
            precShift := mark.baseRule;
          ELSE
            <* ASSERT mark.return.rule = NIL *>
            precShift := NIL;precShift := mark.baseRule;
          END;
*)
          
          precShift := mark.baseRule;
<* ASSERT precShift # NIL *>
 
(*
         DebugMark(mark, Mark.Advance(mark), "step(" & Fmt.Int(code) & ")");

          IF mark.current.rule.number = 10 AND
            Sym.GetCode(mark.current.cell.head) = 262 THEN
            Term.WrLn("Debug: " & Mark.Format(mark) & "->" & 
              Mark.Format(Mark.Advance(mark)));
          END; 
*)
          IF Rule.Compare(precShift, highestShift) > 0 THEN
            highestShift := precShift;
          END;
          leftoverShift := TRUE;
        END;
      END;
      cur := cur.tail;
    END;
    next.marksList := MarkListSort.SortD(next.marksList);

(*
Term.WrLn("Step with code " & Fmt.Int(code) & " yields " & Format(next));
IF highestReduce # NIL THEN Term.WrLn("reduceRule = "&highestReduce.name);END;
IF highestShift # NIL THEN Term.WrLn("shiftRule = "&highestShift.name);END;
 *)

    noMatch := highestReduce = NIL AND highestShift = NIL;
    IF noMatch THEN
      IF from.start = StartStatus.SingleStartSym THEN
        RETURN Action{PDATrans.ActKind.Accept};
      END;
      IF prefin # NIL AND NOT from.call.get(code, dummy) THEN
        (* reduce to the finish if we can and we see errors ahead *)
        (* tis an error to see an error ahead when lookahead is not a token *)
        RETURN Action{PDATrans.ActKind.Reduce, rule := prefin};
      END;
      IF leftoverShift THEN
        RETURN Action{PDATrans.ActKind.Shift, next := next};
      END;
    END;
    IF from.call.get(code, dummy) THEN
      <* ASSERT dummy.head # NIL *>
      <* ASSERT Sym.GetCode(dummy.head.return) = code *>

(*      Term.WrLn("code " & Fmt.Int(code) &
          "is not a token, returned by " & Fmt.Int(dummy.head.number));
      Term.WrLn(Rule.Format(dummy.head, "%debug")); *)

      (* always shift reduced symbols, if not an error *)
      IF from.start = StartStatus.FirstState THEN
        IF Sym.IsStart(dummy.head.return) THEN
          next.start := StartStatus.SingleStartSym;
          (* shift single start symbol regardless of error *)
          RETURN Action{PDATrans.ActKind.Shift, next := next};
        END;
      END;
      IF NOT noMatch THEN
        IF precShift = NIL THEN
          RETURN Action{PDATrans.ActKind.Error};
        ELSE
          RETURN Action{PDATrans.ActKind.Shift, next := next};
        END;
      END;
    END;
    IF noMatch THEN
      RETURN Action{PDATrans.ActKind.Error};
    END;
(*    Term.WrLn("Comparing shift/reduce"); *)
    CASE Rule.Compare(highestReduce, highestShift, TRUE) OF
    | -1 =>
      RETURN Action{PDATrans.ActKind.Shift, next := next};
    | 0 =>
      ConflictWarn(next, highestReduce, highestShift, "shift");
    | 1 =>
    END;
    IF redConflict # NIL AND highestReduce # redConflict THEN
      ConflictWarn(next, highestReduce, redConflict, "reduce");        
    END;
    RETURN Action{PDATrans.ActKind.Reduce, rule := highestReduce};
  END Step; 

PROCEDURE History(self: T): TEXT =
  VAR
    acc := "";
    cur := self;
  BEGIN
    WHILE cur.prev # NIL DO
      acc := cur.symName & " " & acc;
      cur := cur.prev;
    END;
    RETURN acc;
  END History;

PROCEDURE ConflictWarn(self: T; a, b: Rule.T; bKind: TEXT) =
  VAR
    key := "Reduce " & a.name & ", or " & bKind & " " & b.name & "?";
    val := " (" & History(self) & ")";
  BEGIN
    IF NOT self.warnings.get(key, val) THEN
      EVAL self.warnings.put(key, val);
    END;
  END ConflictWarn;

PROCEDURE Equal(a, b: T): BOOLEAN =
  BEGIN
    IF a.finish # b.finish THEN RETURN FALSE END;
    IF a.start # b.start THEN RETURN FALSE END;
    RETURN MarkListF.Equal(a.marksList, b.marksList);
  END Equal;

PROCEDURE Hash(self: T): INTEGER =
  BEGIN
    RETURN Rule.Number(self.finish) + 3*MarkListF.Hash(self.marksList);
  END Hash;

PROCEDURE Format(self: T): TEXT =
  BEGIN
    RETURN Fmt.Int(Rule.Number(self.finish)) & "/" &
           MarkListF.Format(self.marksList) & "\n H:" & History(self);
  END Format;

BEGIN
END RuleListState.
