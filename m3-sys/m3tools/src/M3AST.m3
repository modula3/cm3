(* Copyright 1995-96 Critical Mass, Inc. All rights reserved.    *)

MODULE M3AST;

PROCEDURE NumChildren (t: T;  n: NodeIndex): CARDINAL =
  VAR
    cnt   : CARDINAL := 0;
    i     : CARDINAL := n + 1;
    limit : CARDINAL := n + t.nodes[n].width;
  BEGIN
    WHILE (i < limit) DO
      INC (cnt);
      i := i + t.nodes[i].width;
    END;
    RETURN cnt;
  END NumChildren;

PROCEDURE GetChildren (t: T;  n: NodeIndex;  VAR ch: ARRAY OF NodeIndex): CARDINAL =
  VAR
    cnt   : CARDINAL := 0;
    i     : CARDINAL := n + 1;
    limit : CARDINAL := n + t.nodes[n].width;
  BEGIN
    WHILE (i < limit) DO
      IF (cnt < NUMBER (ch)) THEN ch[cnt] := i; END;
      INC (cnt);
      i := i + t.nodes[i].width;
    END;
    RETURN cnt;
  END GetChildren;

PROCEDURE NthChild (t: T;  n: NodeIndex;  ch: CARDINAL): NodeIndex =
  VAR
    i     : CARDINAL := n + 1;
    limit : CARDINAL := n + t.nodes[n].width;
  BEGIN
    WHILE (i < limit) AND (ch > 0) DO
      i := i + t.nodes[i].width;
      DEC (ch);
    END;
    RETURN i;
  END NthChild;

BEGIN
  FOR i := FIRST (OpMap) TO LAST (OpMap) DO <*ASSERT OpMap[i].op = i *> END;
END M3AST.
