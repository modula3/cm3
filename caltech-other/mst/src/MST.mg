(* $Id$ *)

GENERIC MODULE MST(Elem, ElemSeq);
IMPORT IntPair;
IMPORT TwoIndexedLongreal AS Index;
IMPORT TwoIndexedLongrealArraySort AS IndexSort;
IMPORT CardPairSet, CardPair;

TYPE
  Data = BRANDED REF ARRAY OF IntPair.T;
  DMatrix = BRANDED REF ARRAY OF Index.T;
  MMatrix = BRANDED REF ARRAY OF CARDINAL;

REVEAL
  T = Public BRANDED Brand OBJECT
    mysize : CARDINAL := 0;

    data : Data := NIL;
    marked : MMatrix := NIL;
    matrix : DMatrix := NIL;

    maxLength : LONGREAL;
    elems : REF ARRAY OF Elem.T;
  OVERRIDES
    init := Init;
    maxLinkLength := MaxLinkLength;
    getLink := GetLink;
    size := Size;
    totalWeight := TotalWeight;
  END;

PROCEDURE Size(self : T) : CARDINAL = BEGIN RETURN self.mysize END Size;

PROCEDURE GetLink(self : T; index : CARDINAL; VAR from, to : Elem.T; VAR length : LONGREAL) =
  BEGIN WITH entry = self.data[index] DO 
    from := self.elems[entry.k1]; to := self.elems[entry.k2];
    length := Elem.Distance(from,to)
  END END GetLink;

PROCEDURE Init(self : T; seq : ElemSeq.T; edges : CardPairSet.T := NIL;
               edgeAr : REF ARRAY OF ARRAY OF CARDINAL;
               edgeCounts : REF ARRAY OF CARDINAL) : T = 
  VAR
    n := seq.size();
    marked : MMatrix;
    matrix : DMatrix;
    p := 0;
    e := 0;
  BEGIN 

    IF self.marked = NIL OR NUMBER(self.marked^) < n THEN
      self.marked := NEW(MMatrix, n)
    END;
    marked := self.marked;

    (* initialize elems *)
    IF self.elems = NIL OR NUMBER(self.elems^) < n THEN
      self.elems := NEW(REF ARRAY OF Elem.T, n)
    END;
    FOR i := 0 TO n - 1 DO self.elems[i] := seq.get(i) END;

    (* initialize marked *)
    FOR i := 0 TO n - 1 DO marked[i] := i END;

    IF edges = NIL AND edgeAr = NIL THEN
      e := (n * (n - 1)) DIV 2;
      
      IF self.matrix = NIL OR NUMBER(self.matrix^) < e THEN
        self.matrix := NEW(DMatrix, e) 
      END;
      matrix := self.matrix;

      (* initialize distances *)
      FOR i := 0 TO n - 1 DO
        FOR j := 0 TO i - 1 DO
          matrix[p] := Index.T { i, j, Elem.Distance(seq.get(i), seq.get(j)) };
          INC(p)
        END
      END
    ELSIF edges # NIL THEN
      e := edges.size();

      IF self.matrix = NIL OR NUMBER(self.matrix^) < e THEN
        self.matrix := NEW(DMatrix, e) 
      END;
      matrix := self.matrix;

      VAR
        iter := edges.iterate();
        pair : CardPair.T;
      BEGIN
        WHILE iter.next(pair) DO
          matrix[p] := Index.T { pair.k1, pair.k2, 
                                 Elem.Distance(seq.get(pair.k1), seq.get(pair.k2)) };
          INC(p)
        END
      END
    ELSIF edgeAr # NIL AND edgeCounts # NIL THEN
      (* count edges first *)
      FOR i := 0 TO n-1 DO
        e := e + edgeCounts[i]
      END;
      IF self.matrix = NIL OR NUMBER(self.matrix^) < e THEN
        self.matrix := NEW(DMatrix, e) 
      END;
      matrix := self.matrix;

      FOR i := 0 TO n-1 DO
        FOR j := 0 TO edgeCounts[i]-1 DO
          matrix[p] := Index.T { i, edgeAr[i,j],
                                 Elem.Distance(seq.get(i), seq.get(edgeAr[i,j])) };
          INC(p)
        END
      END
    ELSE
      <* ASSERT FALSE *>
    END;

    (* Kruskal's algorithm.  CLR, p. 504. *)

    (* sort "matrix" *)
    IndexSort.Sort(matrix^, e);

    IF self.data = NIL OR NUMBER(self.data^) < n-1 THEN
      self.data := NEW(Data, n - 1)
    END;

    self.mysize := n - 1;
    p := 0;
    FOR k := 0 TO e-1 DO
      WITH iMark = marked[matrix[k].x], jMark = marked[matrix[k].y] DO
        IF iMark # jMark THEN 
          (* merge trees *)
          self.data[p] := IntPair.T { matrix[k].x, matrix[k].y };
          self.maxLength := matrix[k].d;
          VAR
            changeMark := MAX(iMark, jMark);
            newMark := MIN(iMark, jMark);
          BEGIN
(*
            FOR i := 0 TO n-1 DO
*)

            FOR i := changeMark TO n-1 DO
              IF marked[i] = changeMark THEN marked[i] := newMark END
            END
          END;
          IF p = n - 2 THEN EXIT END;
          INC(p)
        END
      END
    END;

(*

  THIS CHECK ONLY HOLDS IF CONNECTED
  
    (* check that it worked out.. :) *)
    FOR i := 0 TO n - 2 DO
      <* ASSERT marked[i] = 0 *>
    END;
*)

    RETURN self 
  END Init;

PROCEDURE MaxLinkLength(self : T) : LONGREAL =
  BEGIN RETURN self.maxLength END MaxLinkLength;

PROCEDURE TotalWeight(self : T) : LONGREAL =
  VAR
    res := 0.0d0;
    d1, d2 : Elem.T; (* dummies *)
    l : LONGREAL;
  BEGIN
    FOR i := 0 TO self.size() - 1 DO
      self.getLink(i,d1,d2,l);
      res := res + l
    END;
    RETURN res
  END TotalWeight;

BEGIN END MST.
