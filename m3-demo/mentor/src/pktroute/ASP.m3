(* Copyright 1993 Digital Equipment Corporation.                             *)
(* Distributed only by permission.                                           *)
(*                                                                           *)
(* Last modified on Mon May  2 16:15:53 PDT 1994 by najork                   *)
(*      modified on Fri Aug 13 12:14:07 PDT 1993 by heydon                   *)

MODULE ASP;

IMPORT IntPQ, RealPQ, IEEESpecial, IntRefTbl, Graph, GraphRep;

<* FATAL IntPQ.Empty,  IntPQ.NotInQueue *>
<* FATAL RealPQ.Empty, RealPQ.NotInQueue *>

REVEAL
  Unweighted = UnweightedPub BRANDED OBJECT
    min: REF ARRAY OF ARRAY OF INTEGER;
  OVERRIDES
    dist := UnweightedDist;
  END;

  Weighted = WeightedPub BRANDED OBJECT
    min: REF ARRAY OF ARRAY OF REAL;
  OVERRIDES
    dist := WeightedDist;
  END;

TYPE
  IntElt = IntPQ.Elt BRANDED OBJECT value: INTEGER END;
  RealElt = RealPQ.Elt BRANDED OBJECT value: INTEGER END;

PROCEDURE UnweightedDist(uw: Unweighted; READONLY from, to: CARDINAL): INTEGER=
  BEGIN RETURN uw.min[from, to] END UnweightedDist;

PROCEDURE WeightedDist(w: Weighted; READONLY from, to: CARDINAL): REAL =
  BEGIN RETURN w.min[from, to] END WeightedDist;

PROCEDURE UnweightedFromGraph(g: Graph.Sparse): Unweighted =
  VAR
    res := NEW(Unweighted, min := NEW(REF ARRAY OF ARRAY OF INTEGER,
      g.nodeCnt, g.nodeCnt));
    pq := NEW(IntPQ.Default);
    tbl := NEW(IntRefTbl.Default);	 (* node id -> IntElt map *)

  (* Invariant: tbl.get(node, elt) => elt.value = node *)

  PROCEDURE FromNode(
      VAR (*INOUT*) min: ARRAY OF INTEGER;
      src: CARDINAL) =
  (* Sets "min[i]" to contain the length of the shortest path from node "src"
     to node "i". If there is no path from "src" to "i", then "min[i] = -1".
     Requires that "min[i]" is initially -1 for "i" in the interval
     "[FIRST(min)..LAST(min)]". *)

    PROCEDURE Update(adjNode: CARDINAL; newP: INTEGER) =
    (* If the node "adjNode" is in "tbl", then set its priority to "newP" if
       "newP" is a "higher" priority than its current one; otherwise, add
       "adjNode" to "tbl" and insert it in "pq" with priority "newP". *)
      VAR ref: REFANY; BEGIN
        IF tbl.get(adjNode, ref) THEN
          (* "adjNode" is already in the Frontier *)
          VAR adjElt: IntElt := ref; BEGIN
            (* change priority of "adjElt" if it is better than current one *)
            IF pq.pCompare(newP, adjElt.priority) = -1 THEN
              pq.change(adjElt, newP)
            END
          END
        ELSE
          (* "adjNode" currently Unvisited; add it to the Frontier *)
          VAR elt := NEW(IntElt, priority := newP, value := adjNode); BEGIN
            pq.insert(elt);
            EVAL tbl.put(adjNode, elt)
          END
        END
      END Update;

    BEGIN
      EVAL pq.init(sizeHint := g.nodeCnt);
      EVAL tbl.init(sizeHint := g.nodeCnt);
      VAR elt := NEW(IntElt, priority := 0, value := src); BEGIN
        pq.insert(elt);
        EVAL tbl.put(src, elt)
      END;
      WHILE pq.size() # 0 DO
        VAR elt: IntElt := pq.deleteMin(); BEGIN
          min[elt.value] := elt.priority;
          VAR curr := g.adj[elt.value]; BEGIN
            WHILE curr # NIL DO
              IF min[curr.node] = -1 THEN (* node in Fringe or Unvisited set *)
                Update(curr.node, elt.priority + 1)
              END;
              curr := curr.next
            END
          END
        END
      END
    END FromNode;

  BEGIN
    FOR i := 0 TO g.nodeCnt - 1 DO
      FOR j := 0 TO g.nodeCnt - 1 DO
        res.min[i, j] := -1
      END;
      FromNode(res.min[i], i)
    END;
    RETURN res
  END UnweightedFromGraph;

PROCEDURE WeightedFromGraph(g: Graph.Sparse): Weighted =
  VAR
    res := NEW(Weighted, min := NEW(REF ARRAY OF ARRAY OF REAL,
      g.nodeCnt, g.nodeCnt));
    pq := NEW(RealPQ.Default);
    tbl := NEW(IntRefTbl.Default);	 (* node id -> RealElt map *)

  (* Invariant: tbl.get(node, elt) => elt.value = node *)

  PROCEDURE FromNode(
      VAR (*INOUT*) min: ARRAY OF REAL;
      src: CARDINAL) =
  (* Sets "min[i]" to contain the length of the shortest path from node "src"
     to node "i". If there is no path from "src" to "i", then "min[i] =
     IEEESpecial.RealPosInf". Requires that "min[i]" is initially 
     "IEEESpecial.RealPosInf" for "i" in the interval 
     "[FIRST(min)..LAST(min)]". *)

    PROCEDURE Update(adjNode: CARDINAL; newP: REAL) =
    (* If the node "adjNode" is in "tbl", then set its priority to "newP" if
       "newP" is a "higher" priority than its current one; otherwise, add
       "adjNode" to "tbl" and insert it in "pq" with priority "newP". *)
      VAR ref: REFANY; BEGIN
        IF tbl.get(adjNode, ref) THEN
          (* "adjNode" is already in the Frontier *)
          VAR adjElt: RealElt := ref; BEGIN
            (* change priority of "adjElt" if it is better than current one *)
            IF pq.pCompare(newP, adjElt.priority) = -1 THEN
              pq.change(adjElt, newP)
            END
          END
        ELSE
          (* "adjNode" currently Unvisited; add it to the Frontier *)
          VAR elt := NEW(RealElt, priority := newP, value := adjNode); BEGIN
            pq.insert(elt);
            EVAL tbl.put(adjNode, elt)
          END
        END
      END Update;

    BEGIN
      EVAL pq.init(sizeHint := g.nodeCnt);
      EVAL tbl.init(sizeHint := g.nodeCnt);
      VAR elt := NEW(RealElt, priority := 0.0, value := src); BEGIN
        pq.insert(elt);
        EVAL tbl.put(src, elt)
      END;
      WHILE pq.size() # 0 DO
        VAR elt: RealElt := pq.deleteMin(); BEGIN
          min[elt.value] := elt.priority;
          VAR curr := g.adj[elt.value]; BEGIN
            WHILE curr # NIL DO
              IF min[curr.node] = IEEESpecial.RealPosInf THEN
                Update(curr.node, elt.priority + curr.weight)
              END;
              curr := curr.next
            END
          END
        END
      END
    END FromNode;

  BEGIN
    FOR i := 0 TO g.nodeCnt - 1 DO
      FOR j := 0 TO g.nodeCnt - 1 DO
        res.min[i, j] := IEEESpecial.RealPosInf
      END;
      FromNode(res.min[i], i)
    END;
    RETURN res
  END WeightedFromGraph;

BEGIN
END ASP.
