(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: DFA.m3,v 1.2 2001-09-19 15:05:08 wagner Exp $ *)

MODULE DFA;
IMPORT DFATrans;
IMPORT DFATransList;
IMPORT DFATransListF;
IMPORT DFATransListTbl;
IMPORT DFATransOp;
IMPORT DFATransIntTbl;
IMPORT DFAState;
IMPORT DFAStateList;
IMPORT NFA;
IMPORT NFAState;
IMPORT NFAStateTbl;
IMPORT Fmt;
IMPORT Term;
IMPORT Text;

PROCEDURE FromNFA(a: NFA.T): T =
  VAR
    result := NEW(T);
  BEGIN
    BuildStatesFromNFA(result, a);
    BuildStatesArray(result);
    BuildFirst(result);
    BuildTrans(result);
    BuildTransArray(result);
    RETURN result;
  END FromNFA;

PROCEDURE BuildStatesFromNFA(result: T; a: NFA.T) =
  VAR
    boundary: DFAStateList.T := NIL;
    cur: DFAStateList.T;
    table: NFAStateTbl.T;
    ID: INTEGER;
    nextNstate: NFAState.T;
    c, cEnd: CHAR;
  PROCEDURE VisitNextNstate(): INTEGER =
    VAR
      nextDstate: DFAState.T;
    BEGIN
      IF NFAState.Dead(nextNstate) THEN
        RETURN 0
      ELSE
        INC(result.numStates);
        EVAL table.put(nextNstate, result.numStates);
        nextDstate := NEW(DFAState.T,
                          ID := result.numStates,
                          src := nextNstate);
        boundary := DFAStateList.Cons(nextDstate, boundary);
        result.states := DFAStateList.Cons(nextDstate, result.states);
        RETURN result.numStates;
      END;
    END VisitNextNstate;
  BEGIN
    table := NEW(NFAStateTbl.Default).init(NFA.AssignIDs(a));
    nextNstate := NFAState.New(a);
    EVAL VisitNextNstate();
    WHILE boundary # NIL DO
      cur := boundary;
      boundary := NIL;
      REPEAT
        cur.head.output := NFAState.Output(cur.head.src);
        (* Term.WrLn("DFA Output: " & Fmt.Int(cur.head.output)); *)

        
        (*
        targets := NFAState.Steps(cur.head.src);
        FOR c := FIRST(CHAR) TO LAST(CHAR) DO
          nextNstate := targets[c];
          IF nextNstate = NIL THEN
            ID := 0;
          ELSIF NOT table.get(nextNstate, ID) THEN
            ID := VisitNextNstate();
          END;
          cur.head.next[c] := ID;
        END;
        *)

        c := FIRST(CHAR);
        REPEAT
          INC(c);
          nextNstate := NFAState.Step(cur.head.src, c, cEnd);
          (*
            IF NFAState.Output(nextNstate) # 0 THEN
            Term.Wr("When in state " &
            NFAState.Format(cur.head.src) & "[" & Fmt.Int(cur.head.ID) &
            "] and we see '" & Fmt.Char(c) &
            "' then we go to state " & NFAState.Format(nextNstate));
            END;
          *)
          IF NOT table.get(nextNstate, ID) THEN
            ID := VisitNextNstate();
            (*
              IF NFAState.Output(nextNstate) # 0 THEN
              Term.WrLn(" which is not in the table so we" &
              " put it there and give it ID=" & Fmt.Int(ID));
              END;
              ELSIF NFAState.Output(nextNstate) # 0 THEN
              Term.WrLn(" which is in the table and has" &
              " ID=" &Fmt.Int(ID));
            *)
          END;

          cur.head.next := DFATransList.Cons(
                               DFATrans.T{keyBegin := c,
                                          keyEnd := cEnd,
                                          target := ID},
                               cur.head.next);

          (* cur.head.next[c] := ID;          
             WHILE c < cEnd DO
             INC(c);
             cur.head.next[c] := ID;
             END; *)

          c := cEnd;
        UNTIL c = LAST(CHAR);

        cur := cur.tail;
      UNTIL cur = NIL;
    END;
    result.states := DFAStateList.ReverseD(result.states);
  END BuildStatesFromNFA;

(* Build a.statesArray, simplify transition lists and collect stats on them *)
PROCEDURE BuildStatesArray(a: T) =
  VAR
    states := NEW(REF ARRAY OF DFAState.T, a.numStates+1);
    cur := a.states;
    transTable := NEW(DFATransIntTbl.Default).init(a.numStates);
  BEGIN
    FOR i := 1 TO a.numStates DO
      states[i] := cur.head;
      states[i].next := DFATransOp.Simplify(states[i].next);
      DFATransOp.Tally(transTable, states[i].next);
      cur := cur.tail;
      <* ASSERT states[i].ID = i *>
    END;
    FOR i := 1 TO a.numStates DO
      DFATransOp.Sort(transTable, states[i].next);
    END;
    a.statesArray := states;
    a.states := NIL; (* we don't need the list now that we have the array *)
  END BuildStatesArray;

PROCEDURE BuildFirst(a: T) =
  VAR
    cur := a.statesArray[1].next;
  BEGIN
    a.first['\000'] := 0;
    FOR i := VAL(1, CHAR) TO LAST(CHAR) DO
      a.first[i] := DFATransOp.GetTarget(cur, i);
    END;
  END BuildFirst;

PROCEDURE CanOmitFirstState(a: T): BOOLEAN =
  VAR
    cur: DFATransList.T;
  BEGIN
    FOR i := 1 TO a.numStates DO
      cur := a.statesArray[i].next;
      WHILE cur # NIL DO
        IF cur.head.target = 1 THEN
          RETURN FALSE;
        END;
        cur := cur.tail;
      END;
    END;
    RETURN TRUE;
  END CanOmitFirstState;

(* Recognize and merge multihop transitions *)
PROCEDURE BuildTrans(a: T) =
  VAR
    states := a.statesArray;
    trans: ARRAY [1..ORD(LAST(CHAR))] OF DFATransList.T;
    numTrans: INTEGER;
    cur: DFATransList.T;
    table := NEW(DFATransListTbl.Default).init(a.numStates);
    nextID: INTEGER;
    startIndex: INTEGER := 1;
  BEGIN
    IF CanOmitFirstState(a) THEN
      startIndex := 2;
      cur := states[1].next;
      cur.head := DFATrans.T{'\000','\000',0,0};
      cur.tail := NIL;
    END;
    FOR i := startIndex TO a.numStates DO
      cur := states[i].next;
      numTrans := 0;
      WHILE cur # NIL DO
        INC(numTrans);
        trans[numTrans] := cur; (* the whole local sublist *)
        cur := cur.tail;
      END;
      trans[numTrans].head.prio := 0;
      FOR j := numTrans TO 2 BY -1 DO
        cur := trans[j];
        IF NOT table.get(cur, nextID) THEN
          INC(a.numTrans);
          a.trans := DFATransList.Cons(cur.head, a.trans);
          nextID := a.numTrans;
          EVAL table.put(cur, nextID);
        END;
        trans[j-1].head.prio := nextID; (*prio points to next*)
      END;
      states[i].next.tail := NIL;
      (* we don't need the local sublist now that we have the transList *)
    END;
    a.trans := DFATransList.ReverseD(a.trans);
  END BuildTrans;

PROCEDURE BuildTransArray(a: T) =
  VAR
    trans := NEW(REF ARRAY OF DFATrans.T, a.numTrans+1);
    cur := a.trans;
  BEGIN
    FOR i := 1 TO a.numTrans DO
      trans[i] := cur.head;
      cur := cur.tail;
    END;
    a.transArray := trans;
    a.trans := NIL; (* we don't need the list now that we have the array *)
  END BuildTransArray;

PROCEDURE Format(a: T) =
  VAR
    cur: DFAState.T;
  BEGIN
    Term.Wr("\n\n*** first ***\n\n");
    FOR i := FIRST(CHAR) TO LAST(CHAR) DO
      Term.WrLn(Fmt.Int(ORD(i))&": "&Fmt.Int(a.first[i]));
    END;
    Term.Wr("\n\n\n*** states ***\n\n");
    FOR i := 1 TO a.numStates DO
      cur := a.statesArray[i];
      Term.WrLn(Fmt.Int(i)&": "& Fmt.Int(cur.output) & "/" &
        DFATransListF.Format(cur.next));
    END;
    Term.Wr("\n\n\n*** transitions ***\n\n");
    FOR i := 1 TO a.numTrans DO
      Term.WrLn(Fmt.Int(i)&": " & DFATrans.Format(a.transArray[i]));
    END;      
  END Format;

PROCEDURE Test(a: T) =
  VAR
    states := a.statesArray;
    trans := a.transArray;
    tr: DFATrans.T;
    curState: INTEGER;
    input, t: TEXT;
    c: CHAR;
    curTrans: INTEGER;
    hops: INTEGER;
  BEGIN
    Term.MakeRaw(TRUE);
    Format(a);
    Term.WrLn("DFA Test.");
    Term.WrLn("numStates = " & Fmt.Int(a.numStates));
    Term.WrLn("State 1 output = " & Fmt.Int(states[1].output));
    c := Term.GetCharD();
    Term.WrLn("First transition lookup.");
    curState := a.first[c];
    t := Text.FromChar(c);
    input := t;
    WHILE curState # 0 DO
      Term.WrLn("curState = " & Fmt.Int(curState));
      IF states[curState].output # 0 THEN
        Term.WrLn("Output: " & Fmt.Int(states[curState].output));
      END;
      Term.Wr(input);
      c := Term.GetCharD();
      t := Text.FromChar(c);
      input := input & t;
      Term.WrLn(t);

      hops := 1;
      tr := states[curState].next.head;
      IF c >= tr.keyBegin AND c <= tr.keyEnd THEN
        curState := tr.target;
      ELSE
        curTrans := tr.prio;
        WHILE curTrans # 0 DO
          INC(hops);
          tr := trans[curTrans];
          IF c >= tr.keyBegin AND c <= tr.keyEnd THEN
            curState := tr.target;
            curTrans := 0;
          ELSE
            curTrans := tr.prio;
          END;
        END;
      END;
      Term.WrLn("Hops: " & Fmt.Int(hops));
    END;
    Term.WrLn("Reject.");
  END Test; 

BEGIN
END DFA.
