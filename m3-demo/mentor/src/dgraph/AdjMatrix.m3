(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE AdjMatrix;

IMPORT Rd, Sx, Atom, RefList, Text, Thread, R2;

REVEAL T = Public BRANDED OBJECT
    data: REF ARRAY OF ARRAY OF BOOLEAN;
    coords: REF ARRAY OF R2.T;
    N: INTEGER;
  OVERRIDES
    init := Init;
    initFromRd := InitFromRd;
    name := Name;
    coord := Coord;
    putEdge := PutEdge;
    getEdge := GetEdge;
    nVertices := NVertices;
    edgeIter := EdgeIter;
  END; (* object *)

REVEAL
  Iter = IterPublic BRANDED OBJECT
    t: T;
    last_i, last_j: INTEGER := 0;
  OVERRIDES
    next := EdgeNext;
  END;

PROCEDURE Init(t: T; n: INTEGER; selfEdges := FALSE): T =
  BEGIN
    WITH d = NEW(REF ARRAY OF ARRAY OF BOOLEAN, n, n) DO
      t.data := d;
      t.N := n;
      FOR i := 0 TO n-1 DO
        FOR j := 0 TO n-1 DO
          IF i=j AND selfEdges THEN d[i, j] := TRUE ELSE d[i,j] := FALSE END;
        END; (* for *)
      END; (* for *)
    END; (* with *)
    WITH cds = NEW(REF ARRAY OF R2.T, n) DO
      t.coords := cds;
      FOR i := 0 TO n-1 DO
        cds[i] := R2.Origin;
      END; (* for *)
    END; (* with *)
    RETURN t;
  END Init;

PROCEDURE InitFromRd(t: T; rd: Rd.T): T RAISES {Thread.Alerted} =
  <*FATAL Rd.EndOfFile, Sx.ReadError *>
  VAR 
    list := NARROW (Sx.Read(rd), RefList.T);
    nV := RefList.Length(list.head);
    coords := NARROW(list.tail.head, RefList.T);
    edges := NARROW(list.tail.tail.head, RefList.T);
    vN := 0;
  BEGIN
    EVAL Init(t, nV);
    WHILE edges # NIL DO
      WITH edge = NARROW(edges.head, RefList.T) DO
        (* edge is a list of two SxSymbols.T denoting the vertices *)
(* edge might be a list of two Atom.T or a list of two TEXT *)
        VAR v1: Atom.T := edge.head;
            v2: Atom.T := edge.tail.head;
            n1 := Atom.ToText (v1);
            n2 := Atom.ToText (v2);
        BEGIN
          t.data [VIndex(n1), VIndex(n2)] := TRUE;
        END;
      END; (* with *)
      edges := edges.tail;
    END; (* while *)
    WHILE coords # NIL DO
      WITH coord = NARROW(coords.head, RefList.T) DO
        (* coord is a list of two REF REALs denoting the coords *)
        VAR v1: REF REAL := coord.head;
            v2: REF REAL := coord.tail.head;
        BEGIN
          t.coords[vN] := R2.T{v1^, v2^};
        END;
      END;
      INC(vN);
      coords := coords.tail;
    END; (* while *)
    RETURN t;
  END InitFromRd;

EXCEPTION BadVIndex; 

PROCEDURE VIndex(n: TEXT): INTEGER =
  <* FATAL BadVIndex *>
  BEGIN
    WITH ch = Text.GetChar(n, 0) DO
      IF ch >= 'A' AND ch <= 'Z' THEN
        RETURN ORD(ch) - ORD('A');
      ELSE
        RAISE BadVIndex; 
      END; (* if *)
    END; (* with *)
  END VIndex;

PROCEDURE Name(<*UNUSED*> t: T; i: INTEGER): TEXT =
  BEGIN
    RETURN Text.FromChar(VAL(ORD('A') + i, CHAR));
  END Name;

PROCEDURE Coord(t: T; i: INTEGER): R2.T =
  BEGIN
    RETURN t.coords[i];
  END Coord;


PROCEDURE PutEdge(t: T; i, j: INTEGER; b: BOOLEAN) =
  BEGIN
    t.data[i, j] := b;
  END PutEdge;

PROCEDURE GetEdge(t: T; i, j: INTEGER): BOOLEAN =
  BEGIN
    RETURN t.data[i, j];
  END GetEdge;

PROCEDURE NVertices(t: T): INTEGER =
  BEGIN
    RETURN t.N;
  END NVertices;

PROCEDURE EdgeIter(t: T): Iter =
  BEGIN
    RETURN NEW(Iter, t := t);  
  END EdgeIter;

PROCEDURE EdgeNext(s: Iter; VAR i, j: INTEGER): BOOLEAN =
  BEGIN
    WHILE s.last_i < s.t.N DO
      WHILE s.last_j < s.t.N DO
        IF GetEdge(s.t, s.last_i, s.last_j) THEN
          i := s.last_i; j := s.last_j;
          INC(s.last_j);
          RETURN TRUE;
        END;
        INC(s.last_j);
      END; (* while *)
      INC(s.last_i); s.last_j := 0;
    END; (* while *)
    RETURN FALSE;
  END EdgeNext;

PROCEDURE ToText(<*UNUSED*> t: T): TEXT =
  BEGIN
    RETURN "adjacency matrix"
  END ToText;

PROCEDURE RCToText(rcset: RCSet): TEXT =
    VAR t: TEXT := "";
  BEGIN
    IF RC.Row IN rcset THEN t := t & "row"; END;
    IF RC.Column IN rcset THEN t := t & " column"; END;
    RETURN t;
  END RCToText;



BEGIN

END AdjMatrix.
