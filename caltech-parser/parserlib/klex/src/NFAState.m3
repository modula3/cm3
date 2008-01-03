(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: NFAState.m3,v 1.2 2001-09-19 15:05:08 wagner Exp $ *)

MODULE NFAState;
IMPORT NFA;
IMPORT NFANode;
IMPORT NFANodeList;
IMPORT NFANodeListF;
IMPORT NFANodeListSort;
IMPORT Word;
IMPORT Fmt,Term,Text;
REVEAL
  T = BRANDED REF RECORD
    s: NFANodeList.T := NIL;
    out: INTEGER := NFANode.NoOutput;
    markedNodes: NFANodeList.T := NIL;
  END;
  
PROCEDURE DeleteRedundant(s: NFANodeList.T) =
  VAR
    cur := s;
  BEGIN
    WHILE cur # NIL DO
      WHILE cur.tail # NIL AND cur.head = cur.tail.head DO
        cur.tail := cur.tail.tail;
      END;
      cur := cur.tail;
    END;
  END DeleteRedundant;
  
PROCEDURE UnmarkNodes(nodes: NFANodeList.T) =
  VAR
    cur := nodes;
  BEGIN
    WHILE cur # NIL DO
      <* ASSERT cur.head.marked *>
      cur.head.marked := FALSE;
      cur := cur.tail;
    END;
  END UnmarkNodes;
  
PROCEDURE Canonical(s: T) =
  BEGIN
    IF s.s # NIL THEN
      s.s := NFANodeListSort.SortD(s.s);
      DeleteRedundant(s.s);
    END;
    UnmarkNodes(s.markedNodes);
    s.markedNodes := NIL;
  END Canonical;
  
PROCEDURE AddNodeChasingEpsilons(s: T; node: NFANode.T) =
  BEGIN
    IF node = NIL OR node.marked THEN
    ELSIF node.keyBegin = NFANode.Epsilon THEN
      node.marked := TRUE;
      s.markedNodes := NFANodeList.Cons(node, s.markedNodes);
      AddNodeChasingEpsilons(s, node.targ1);
      AddNodeChasingEpsilons(s, node.targ2);
      s.out := MIN(s.out, node.output);
(*      IF s.out # NFANode.NoOutput THEN
        Term.WrLn("NFAState outputs " & Fmt.Int(s.out));
      END; *)
    ELSE
      s.s := NFANodeList.Cons(node, s.s);
    END;
  END AddNodeChasingEpsilons;
  
PROCEDURE New(n: NFA.T): T =
  VAR
    result := NEW(T);
  BEGIN
    AddNodeChasingEpsilons(result, n.start);
    Canonical(result);
    RETURN result;
  END New;
  
PROCEDURE Step(from: T; key: CHAR; VAR keyEnd: CHAR): T =
  VAR
    result := NEW(T);
    cur := from.s;
    node: NFANode.T;
  BEGIN
    keyEnd := LAST(CHAR);
    WHILE cur # NIL DO
      node := cur.head;
      <* ASSERT node.keyBegin # NFANode.Epsilon *>
      IF key < node.keyBegin THEN
        keyEnd := MIN(keyEnd, VAL(ORD(node.keyBegin) - 1, CHAR));
      ELSIF key <= node.keyEnd THEN
        keyEnd := MIN(keyEnd, node.keyEnd);
        result.out := MIN(result.out, node.output);
        AddNodeChasingEpsilons(result, node.targ1);
        AddNodeChasingEpsilons(result, node.targ2);
      END;
      cur := cur.tail;
    END;
    Canonical(result);
    RETURN result;
  END Step;

(*
PROCEDURE Steps(from: T): Targets =
  VAR
    to: T;
    result := NEW(Targets);
    cur := from.s;
    node: NFANode.T;
    matches: ARRAY CHAR OF NFANodeList.T;
  BEGIN
    FOR i := FIRST(CHAR) TO LAST(CHAR) DO
      matches[i] := NIL;
    END;
    WHILE cur # NIL DO
      node := cur.head;
      <* ASSERT node.key # NFANode.Epsilon *>
      matches[node.key] := NFANodeList.Cons(node, matches[node.key]);
      cur := cur.tail;
    END;
    FOR i := FIRST(CHAR) TO LAST(CHAR) DO
      cur := matches[i];
      IF cur = NIL THEN
        result[i] := NIL;
      ELSE
        to := NEW(T);
        result[i] := to;
        WHILE cur # NIL DO
          node := cur.head;
          to.out := MIN(to.out, node.output);
          AddNodeChasingEpsilons(to, node.targ1);
          AddNodeChasingEpsilons(to, node.targ2);
          cur := cur.tail;
        END;
        Canonical(result[i]);
      END;
    END;
    RETURN result;
  END Steps;
*)

PROCEDURE Dead(s: T): BOOLEAN = BEGIN
  RETURN s=NIL OR (s.s = NIL AND s.out = NFANode.NoOutput); END Dead;
PROCEDURE Output(s: T): INTEGER = BEGIN
  IF s = NIL OR s.out = NFANode.NoOutput THEN RETURN 0;
  ELSE RETURN s.out; END;
END Output;

PROCEDURE Equal(s, t: T): BOOLEAN =
  BEGIN
    IF s.out # t.out THEN
      RETURN FALSE;
    END;
    RETURN NFANodeListF.Equal(s.s, t.s);
  END Equal;

PROCEDURE Hash(s: T): Word.T =
  BEGIN
    RETURN NFANodeListF.Hash(s.s)*5 + s.out;
  END Hash;

PROCEDURE Format(s: T): TEXT =
  VAR
    result: TEXT;
  BEGIN
    result := NFANodeListF.Format(s.s);
    IF s.out # NFANode.NoOutput THEN
      result := result & "." & Fmt.Int(s.out);
    END;
    RETURN result;
  END Format;

PROCEDURE Test(a: NFA.T) =
  VAR
    curState: T;
    input, t: TEXT := "";
    c: CHAR;
  BEGIN
    EVAL NFA.AssignIDs(a);
    curState := New(a);
    Term.MakeRaw(TRUE);
    Term.WrLn("NFA Test.");
    WHILE NOT Dead(curState) DO
      Term.WrLn("curState = " & Format(curState));
      IF curState.out # NFANode.NoOutput THEN
        Term.WrLn("Output: " & Fmt.Int(curState.out));
      END;
      Term.Wr(input);
      c := Term.GetCharD();
      t := Text.FromChar(c);
      input := input & t;
      Term.WrLn(t);
      curState := Step(curState, c, c);
    END;
    Term.WrLn("Reject.");
  END Test;
  
BEGIN
END NFAState.
