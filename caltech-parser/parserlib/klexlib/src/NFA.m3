(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: NFA.m3,v 1.2 2001-09-19 15:04:09 wagner Exp $ *)

MODULE NFA;
IMPORT Interval;
IMPORT CharRange;
IMPORT Text;
IMPORT NFANode;
TYPE
  Node = NFANode.T;
  NodeArray = REF ARRAY OF Node;

PROCEDURE AssignIDs(a: T): INTEGER =
  VAR
    cur := a.start;
    ID := 0;
  BEGIN
    WHILE cur # NIL DO
      cur.ID := ID;
      INC(ID);
      cur := cur.next;
    END;
    RETURN ID;
  END AssignIDs;

PROCEDURE InitReplacements(old: Node; news: NodeArray) =
  VAR
    new: Node;
  PROCEDURE Replace(n: Node): Node =
    BEGIN
      IF n = NIL THEN
        RETURN NIL
      ELSE
        RETURN news[n.ID];
      END;
    END Replace;
  BEGIN
    IF old # NIL THEN
      new := Replace(old);
      new.targ1 := Replace(old.targ1);
      new.targ2 := Replace(old.targ2);
      new.keyBegin := old.keyBegin;
      new.keyEnd := old.keyEnd;
      new.output := old.output;
      new.next := Replace(old.next);
      InitReplacements(old.next, news);
    END;
  END InitReplacements;

PROCEDURE Copy(a: T): T =
  VAR
    result := NEW(T);
    nodes: NodeArray;
  BEGIN
    <* ASSERT a.end.next = NIL *>
    <* ASSERT a.start # NIL *>
    nodes := NEW(NodeArray, AssignIDs(a));
    FOR i := 0 TO LAST(nodes^) DO
      nodes[i] := NEW(Node);
    END;
    InitReplacements(a.start, nodes);
    result.start := nodes[a.start.ID];
    result.end := nodes[a.end.ID];
    <* ASSERT result.end.next = NIL *>
    <* ASSERT result.start # NIL *>
    RETURN result;
  END Copy;

PROCEDURE FromChar(c: CHAR): T =
  VAR
    result := NEW(T);
  BEGIN
    result.end := NEW(Node, keyBegin := c, keyEnd := c);
    result.start := result.end;
    RETURN result;
  END FromChar;

PROCEDURE Empty(): T =
  BEGIN
    RETURN FromChar('\000');
  END Empty;

PROCEDURE Concat(a, b: T): T =
  BEGIN
    <* ASSERT a.end.next = NIL *>
    <* ASSERT a.start # NIL *>
    <* ASSERT b.end.next = NIL *>
    <* ASSERT b.start # NIL *>
    <* ASSERT a.end.targ2 = NIL *>
    a.end.targ2 := b.start;
    a.end.next :=  b.start;
    a.end := b.end;
    <* ASSERT a.end.next = NIL *>
    <* ASSERT a.start # NIL *>
    RETURN a;
  END Concat;

PROCEDURE Or(a, b: T; endCap: BOOLEAN := TRUE): T =
  VAR
    start := NEW(Node);
    end: Node;
  BEGIN
    <* ASSERT a.end.next = NIL *>
    <* ASSERT a.start # NIL *>
    <* ASSERT b.end.next = NIL *>
    <* ASSERT b.start # NIL *>
    
    (* append nodeLists *)
    start.next := a.start;
    a.end.next := b.start;
    
    (* set start targets *)
    start.targ1 := a.start;
    start.targ2 := b.start;
    <* ASSERT a.end.targ2 = NIL *>
    <* ASSERT b.end.targ2 = NIL *>

    (* set new start *)
    a.start := start;
    IF endCap THEN
      end := NEW(Node);
      (* set end targets *)
      a.end.targ2 := end;
      b.end.targ2 := end;
      (* set new end *)
      a.end := end;
      (* append nodeLists *)
      b.end.next := end;
    ELSE
      a.end := b.end;
    END;
    <* ASSERT a.start # NIL *>
    <* ASSERT a.end.next = NIL *>
    RETURN a;
  END Or; 

PROCEDURE Plus(a: T): T =
  BEGIN
    IF a.end.targ1 = NIL THEN
      a.end.targ1 := a.start;
    ELSE
      <* ASSERT a.end.targ2 = NIL *>
      a.end.targ2 := NEW(Node, targ1 := a.start);
      a.end.next := a.end.targ2;
      a.end := a.end.targ2;
    END;
    RETURN a;
  END Plus;

PROCEDURE Output(a: T; code: INTEGER): T =
  BEGIN
    <* ASSERT a.end.output = NFANode.NoOutput *>
    a.end.output := code;
    RETURN a;
  END Output;


(* extra constructors *)

PROCEDURE FromString(s: TEXT): T =
  VAR
    result: T := Empty();
  BEGIN
    FOR i := 0 TO Text.Length(s)-1 DO
      result := Concat(result, FromChar(Text.GetChar(s, i)));
    END;
    RETURN result;
  END FromString;

PROCEDURE FromRange(c: CharRange.T): T =
  VAR
    block: T;
    result: T := NIL;
    keyBegin, keyEnd: CHAR;
  BEGIN
    keyBegin := VAL(1, CHAR);
    REPEAT
      WHILE keyBegin # LAST(CHAR) AND NOT keyBegin IN c DO
        INC(keyBegin);
      END;
      IF keyBegin IN c THEN
        keyEnd := keyBegin;
        WHILE keyEnd # LAST(CHAR) AND keyEnd IN c DO
          INC(keyEnd);
        END;
        block := FromChar(keyBegin);
        keyBegin := keyEnd; (* for the next block *)
        IF NOT keyEnd IN c THEN
          DEC(keyEnd);
        END;
        block.start.keyEnd := keyEnd;
        IF result = NIL THEN
          result := block;
        ELSE
          result := Or(result, block);
        END;
      END;
    UNTIL keyBegin = LAST(CHAR);

(*    FOR i := FIRST(CHAR) TO LAST(CHAR) DO
      IF i IN c THEN
        IF result = NIL THEN
          result := FromChar(i);
        ELSE
          result := Or(result, FromChar(i));
        END;
      END; 
    END; *)

    RETURN result;
  END FromRange;

PROCEDURE Rept(a: T; count: Interval.T): T =
  BEGIN
    IF count.lo = 1 AND count.hi >= OrMore DIV 2 THEN
      RETURN Plus(a);
    ELSIF count.lo = 1 AND count.hi=1 THEN
      RETURN a;
    ELSIF count.lo # 0 THEN
      RETURN Concat(Copy(a), Rept(a, Interval.T{count.lo-1,count.hi-1}));
    ELSIF count.hi >= OrMore DIV 2 THEN
      RETURN Or(Empty(), Plus(a));
    ELSIF count.hi <= 0 THEN
      RETURN Empty();
    ELSE
      RETURN Or(Empty(), Concat(Copy(a),
                                Rept(a, Interval.T{0, count.hi-1})));
    END;
  END Rept;

BEGIN
END NFA.
