(* Copyright (C) 1996, Digital Equipment Corporation            *)
(* All rights reserved.                                         *)
(* See the file COPYRIGHT for a full description.               *)
(* Last modified on Thu Aug 22 16:31:12 PDT 1996 by detlefs     *)

GENERIC MODULE DiGraph(NodeVal, EdgeVal);
(* The DiGraph type is parameterized over the types of the nodes and the
   edges. *) 

IMPORT List, Wr, Word, RAToRATable2;

IMPORT Thread;
<*FATAL Wr.Failure, Thread.Alerted*>

TYPE
  NodeValRef = REF NodeVal.T;

TYPE
  Node = REF RECORD
    value: NodeVal.T;
    succ, pred: List.T (* Of Edge *);
    mark: BOOLEAN; (* Useful in traversals... *)
  END;
  NodeArr = REF ARRAY OF Node;

  Edge = REF RECORD
    from, to: Node;
    value: EdgeVal.T;
    nextValue : EdgeVal.T (* used in transitive closure *);
  END;

  AddEdgeAction = REF RECORD e: Edge; END (* RECORD *);
  AddNodeAction = REF RECORD n: NodeVal.T; END (* RECORD *);
  DelEdgeAction = REF RECORD e: Edge; END (* RECORD *);
  DelNodeAction = REF RECORD n: NodeVal.T; END (* RECORD *);
  ChangeEdgeAction = REF RECORD e: Edge; oldVal: EdgeVal.T; END (* RECORD *);


  Generation = REF RECORD
    num: CARDINAL;
    actions: List.T (* OF one of the action types above. *) := NIL;
  END (* RECORD *);


REVEAL
  T = TPublic BRANDED OBJECT
    nodeTbl: RAToRATable2.T;
    nodes: CARDINAL := 0;
    edges: CARDINAL := 0;

    (* data structures for restoring the state. *)
    gen: CARDINAL := 0;
    gens: List.T (* OF Generation *);
    curGen: Generation;

   METHODS
    nodeValToNode(nodeVal: NodeVal.T; addNodes: BOOLEAN): Node
        RAISES { NoSuchNode } := NodeValToNode;
    makeNodeArray(): NodeArr := MakeNodeArray;

   OVERRIDES
    new := New;
    nodeSize := NodeSize;
    edgeSize := EdgeSize;
    nodeExists := NodeExists;
    addNode := AddNode;
    deleteNode := DeleteNode;
    edgeExists := EdgeExists;
    getEdge := GetEdge;
    edgeValue := EdgeValue;
    addEdge := AddEdge;
    deleteEdge := DeleteEdge;
    setEdge := SetEdge;
    changeEdge := ChangeEdge;
    nSucc := NSucc;
    getSuccN := GetSuccN;
    getSuccIter := GetSuccIter;
    nPred := NPred;
    getPredN := GetPredN;
    getPredIter := GetPredIter;
    mapOverEdges := MapOverEdges;
    mapOverNodes := MapOverNodes;
    transitiveClose := TransitiveClose;
    addEdgeAndClose := AddEdgeAndClose;
    printAsMatrix := PrintAsMatrix;

    push: Push;
    pop: Pop;
  END;


TYPE
  NodeIterImpl = NodeIter BRANDED OBJECT
    list: List.T;        (* Uniterated remainder of edge list. *)
    toNotFrom: BOOLEAN;  (* TRUE IF this is a 'succ' iter, FALSE if 'pred' *)
   OVERRIDES
    next := NodeIterNext;
  END (* OBJECT *);



PROCEDURE New(self: T): TPublic =
  BEGIN
    IF self = NIL THEN
      RETURN NEW(T, nodeTbl := RAToRATable2.New(
                                   hashProc := NodeValRefHash,
                                   equalProc := NodeValRefEqual));
    ELSE
      self.nodeTbl := RAToRATable2.New(
                          hashProc := NodeValRefHash,
                          equalProc := NodeValRefEqual);
      self.nodes := 0;
      self.edges := 0;

      self.curGen := NEW(Generation, num := self.gen);
      List.Push(self.gens, self.curGen);
      RETURN self;
    END (* IF *);
  END New;

PROCEDURE NodeValRefHash(key: REFANY): Word.T =
  BEGIN
    RETURN NodeVal.Hash(NARROW(key, NodeValRef)^, LAST(INTEGER));
  END NodeValRefHash;
    
PROCEDURE NodeValRefEqual(key1, key2: REFANY): BOOLEAN =
  BEGIN
    RETURN NodeVal.Equal(NARROW(key1, NodeValRef)^,
                         NARROW(key2, NodeValRef)^);
  END NodeValRefEqual;
    
               
(* Should be INLINE *)
PROCEDURE NodeSize(self: T): CARDINAL =
  BEGIN
    RETURN self.nodes;
  END NodeSize;


(* Should be INLINE *)
PROCEDURE EdgeSize(self: T): CARDINAL =
  BEGIN
    RETURN self.edges;
  END EdgeSize;


PROCEDURE NodeExists(self: T; nodeVal: NodeVal.T): BOOLEAN =
  VAR dummyVal: REFANY;
  BEGIN
    WITH nvr = NEW(NodeValRef) DO
      nvr^ := nodeVal;
      RETURN self.nodeTbl.in(nvr, dummyVal);
    END (* WITH *);
  END NodeExists;


PROCEDURE AddNode(self: T; nodeVal: NodeVal.T) RAISES { DupNode } =
  BEGIN AddNodeWork(self, nodeVal); END AddNode;

PROCEDURE AddNodeWork(self: T; nodeVal: NodeVal.T; record: BOOLEAN := TRUE)
    RAISES { DupNode } =
  VAR
    n: Node;
    dummy: BOOLEAN;
  BEGIN
    IF self.nodeExists(nodeVal) THEN RAISE DupNode END;
    n := NEW(Node, value := nodeVal, succ := NIL, pred := NIL);
    WITH nvr = NEW(NodeValRef) DO
      nvr^ := nodeVal;
      dummy := self.nodeTbl.put(nvr, n);
      <*ASSERT NOT dummy*>
    END (* WITH *);
    INC(self.nodes);

    IF record THEN
      List.Push(self.curGen.actions, NEW(AddNodeAction, n := nodeVal));
    END (* IF *);
  END AddNodeWork;



PROCEDURE DeleteNode(self: T; nodeVal: NodeVal.T) RAISES { NoSuchNode } =
  BEGIN DeleteNodeWork(self, nodeVal); END DeleteNode;

PROCEDURE DeleteNodeWork(self: T; nodeVal: NodeVal.T;
                         record: BOOLEAN := TRUE) RAISES { NoSuchNode } =
  VAR
    node: Node;
    edge: Edge;
    preds, succs: List.T (* Of Edge *);
    dummy: BOOLEAN;
    resultRA: REFANY;
  BEGIN
    (* This raises an exception if the node doesn't exist. *)
    node := self.nodeValToNode(nodeVal, FALSE);
    IF record THEN
      List.Push(self.curGen.actions, NEW(DelNodeAction, n := nodeVal));
    END (* IF *);

    (* Delete node from the 'succs' list of each of its predecessors. *)
    preds := node^.pred;
    WHILE preds # NIL DO
      edge := NARROW(List.First(preds), Edge);
      dummy := DeleteFromEdgeList(edge^.from^.succ, FALSE, node, record);
      <*ASSERT dummy*>
      DEC(self.edges);
      preds := List.Tail(preds);
    END;
    (* ...and also from the 'preds' list of each of its successors. *)
    succs := node^.succ;
    WHILE succs # NIL DO
      edge := NARROW(List.First(succs), Edge);
      dummy := DeleteFromEdgeList(edge^.to^.pred, TRUE, node, record);
      <*ASSERT dummy*>
      DEC(self.edges);
      succs := List.Tail(succs);
    END;

    WITH nvr = NEW(NodeValRef) DO
      nvr^ := nodeVal;
      dummy := self.nodeTbl.delete(nvr, resultRA);
      (* If NodeValToNode said it was there, it ought to be there. *)
      <*ASSERT dummy*>
    END (* WITH *);
    DEC(self.nodes);
  END DeleteNode;


(* INTERNAL *)

(* Returns a NodeArr (Array.T OF Node) of all the nodes.  If 'cp' is
   non-NIL, uses it to sort the array. *)

TYPE RefList = REF List.T;

PROCEDURE MakeNodeArray(self: T): NodeArr =
  VAR
    rnl: REF List.T;
    nl: List.T;
    newArr: NodeArr;
    i: CARDINAL;
    dummyKey: REFANY;
    dummyVal: REFANY;
    dummyBool: BOOLEAN;
  BEGIN
    rnl := NEW(RefList); rnl^ := NIL;
    EVAL self.nodeTbl.enumerate(RepackEnumerateProc, rnl, <*NOWARN*>
                                dummyKey, dummyVal);
    <*ASSERT NOT dummyBool*>
    nl := rnl^;
    nl := List.SortD(nl, NodeCompare, NIL);
    newArr := NEW(NodeArr, List.Length(nl));
    i := 0;
    WHILE nl # NIL DO
      newArr[i] := NARROW(List.First(nl), Node);
      INC(i);
      nl := nl^.tail;
    END (* WHILE *);
    RETURN newArr;
  END MakeNodeArray;



PROCEDURE NodeCompare(<*UNUSED*> closure: REFANY;
                      node1Ref, node2Ref: REFANY): [-1..1] =
  VAR
    node1, node2: Node;
  BEGIN
    node1 := NARROW(node1Ref, Node);
    node2 := NARROW(node2Ref, Node);
    RETURN NodeVal.Compare(node1^.value, node2^.value);
  END NodeCompare;


PROCEDURE RepackEnumerateProc(nodeListRef: REFANY;
                              <*UNUSED*> nvra: REFANY;
                              VAR nra: REFANY): BOOLEAN =
  VAR
    nodeList: RefList (* OF Node *) := NARROW(nodeListRef, RefList);
    node: Node := NARROW(nra, Node);
  BEGIN
    List.Push(nodeList^, node);
    RETURN FALSE;
  END RepackEnumerateProc;


(* EXTERNAL *)

PROCEDURE AddEdge(self: T;
                  node1: NodeVal.T; edgeVal: EdgeVal.T; node2: NodeVal.T;
                  addNodes: BOOLEAN := FALSE) =
  BEGIN AddEdgeWork(self, node1, edgeVal, node2, addNodes); END AddEdge;

PROCEDURE AddEdgeWork(self: T;
                      node1: NodeVal.T; edgeVal: EdgeVal.T; node2: NodeVal.T;
                      addNodes: BOOLEAN := FALSE;
                      record: BOOLEAN := TRUE)
    RAISES { NoSuchNode, DupEdge } =
  VAR
    newEdge: Edge;
    fromNode, toNode: Node;
    edgeDummy: Edge;
  BEGIN
    (* These raise NoSuchNode when necessary. *)
    fromNode := self.nodeValToNode(node1, addNodes);
    toNode := self.nodeValToNode(node2, addNodes);

    (* Check to see if an edge exists... *)
    IF FindEdge(fromNode, toNode, edgeDummy) THEN RAISE DupEdge END;
    newEdge := NEW(Edge, value := edgeVal, from := fromNode, to := toNode);
    List.Push(fromNode^.succ, newEdge);
    List.Push(toNode^.pred, newEdge);

    IF record THEN
      List.Push(self.curGen.actions, NEW(AddEdgeAction, e := newEdge));
    END (* IF *);

    INC(self.edges);
  END AddEdge;


(* INTERNAL *)
(* If addNodes is FALSE, and either of self.nodeExists(node1) or
   self.nodeExists(node2) is FALSE, then raises "NoSuchNode."  Otherwise, adds
   nodes corresponding to the values 'node1' and 'node2' to 'g' if no
   such nodes already exist, and returns those nodes in 'fromNode' and
   'toNode', respectively.
*)
PROCEDURE NodeValToNode(self: T; nodeVal: NodeVal.T;
                        addNodes: BOOLEAN): Node
    RAISES { NoSuchNode } =
  VAR nodeRA: REFANY;
  BEGIN
    WITH nvr = NEW(NodeValRef) DO
      nvr^ := nodeVal;
      IF NOT self.nodeTbl.in(nvr, nodeRA) THEN
        IF addNodes THEN
          TRY self.addNode(nodeVal) EXCEPT DupNode => <*ASSERT FALSE*> END;
          RETURN NARROW(self.nodeTbl.get(nvr), Node); <*NOWARN*>
        ELSE
          RAISE NoSuchNode;
        END (* IF *);
      ELSE
        RETURN NARROW(nodeRA, Node);
      END (* IF *);
    END (* WITH *);
  END NodeValToNode;


(* EXTERNAL *)

PROCEDURE EdgeExists(self: T; node1, node2: NodeVal.T): BOOLEAN =
  VAR
    fromNode, toNode: Node;
    edgeDummy: Edge;
  BEGIN
    TRY
      fromNode := self.nodeValToNode(node1, FALSE);
      toNode := self.nodeValToNode(node2, FALSE);
    EXCEPT
    | NoSuchNode => RETURN FALSE;
    END;
    RETURN FindEdge(fromNode, toNode, edgeDummy);
  END EdgeExists;


PROCEDURE GetEdge(self: T; node1, node2: NodeVal.T;
                  VAR ev: EdgeVal.T): BOOLEAN =
  VAR
    fromNode, toNode: Node;
    edge: Edge;
  BEGIN
    TRY
      fromNode := self.nodeValToNode(node1, FALSE);
      toNode := self.nodeValToNode(node2, FALSE);
    EXCEPT
    | NoSuchNode => RETURN FALSE;
    END;
    IF NOT FindEdge(fromNode, toNode, edge) THEN
      RETURN FALSE;
    ELSE
      ev := edge.value;
      RETURN TRUE;
    END (* IF *);
  END GetEdge;


(* INTERNAL *)

(* Requires that 'fromNode' and 'toNode' are nodes in 'g'.  If no edge
   exists between 'fromNode' and 'toNode', returns FALSE; if such an edge
   does exist, return TRUE and the value of that edge in 'edgeVal'.
*)
PROCEDURE FindEdge(fromNode, toNode: Node;
                   VAR (*OUT*) edge: Edge): BOOLEAN =
  VAR
    succs: List.T (* OF Edge *);
  BEGIN
    succs := fromNode^.succ;
    WHILE succs # NIL DO
      edge := NARROW(List.First(succs), Edge);
      IF edge^.to = toNode THEN RETURN TRUE; END;
      succs := List.Tail(succs);
    END;
    RETURN FALSE;
  END FindEdge;


(* EXTERNAL *)

PROCEDURE EdgeValue(self: T; node1, node2: NodeVal.T): EdgeVal.T
    RAISES { NoSuchNode, NoSuchEdge } =
  VAR
    fromNode, toNode: Node;
    edge: Edge;
  BEGIN
    (* These raise NoSuchNode. *)
    fromNode := self.nodeValToNode(node1, FALSE);
    toNode := self.nodeValToNode(node2, FALSE);
    IF NOT FindEdge(fromNode, toNode, edge) THEN
      RAISE NoSuchEdge;
    ELSE
      RETURN edge^.value;
    END;
  END EdgeValue;


PROCEDURE DeleteEdge(self: T; node1, node2: NodeVal.T)
    RAISES { NoSuchNode, NoSuchEdge } =
  BEGIN DeleteEdgeWork(self, node1, node2); END DeleteEdge;

PROCEDURE DeleteEdgeWork(self: T; node1, node2: NodeVal.T;
                         record: BOOLEAN := TRUE)
    RAISES { NoSuchNode, NoSuchEdge } =
  VAR
    fromNode, toNode: Node;
    foundFrom, foundTo: BOOLEAN;
  BEGIN
    (* These raise NoSuchNode. *)
    fromNode := self.nodeValToNode(node1, FALSE);
    toNode := self.nodeValToNode(node2, FALSE);

    foundFrom := DeleteFromEdgeList(fromNode^.succ, FALSE, toNode, record);
    foundTo := DeleteFromEdgeList(toNode^.pred, TRUE, fromNode, record);
    IF foundFrom THEN
      <*ASSERT foundTo*>
      DEC(self.edges);
    ELSE
      <*ASSERT NOT foundTo*>
      RAISE NoSuchEdge;
    END;
  END DeleteEdge;


(* INTERNAL *)

(* Attempts to deletes an edge whose "target" is 'targetNode' from
   'realEdges'.  If 'targetIsFromNode' is TRUE, "target" is interpreted
   to mean the "from" field of an edge, else the "to" field.  Returns
   TRUE iff found and deleted a matching edge. *)

PROCEDURE DeleteFromEdgeList(VAR realEdges: List.T (* Of Edge *);
                             targetIsFromNode: BOOLEAN;
                             targetNode: Node;
                             record: BOOLEAN): BOOLEAN =
  VAR
    i: CARDINAL;
    edges: List.T (* Of Edge *);
    edge: Edge;
  BEGIN
    IF realEdges = NIL THEN RETURN FALSE; END;
    i := 0;
    edges := realEdges;
    WHILE edges # NIL DO
      edge := NARROW(List.First(edges), Edge);
      IF targetIsFromNode AND (edge^.from = targetNode) THEN
        IF record THEN
          List.Push(self.curGen.actions, NEW(DelEdgeAction, e := edge));
        END (* IF *);
        IF i = 0 THEN
          realEdges := List.Tail(edges);
        ELSE
          List.SetNthTail(realEdges, i, List.Tail(edges));
        END;
        RETURN TRUE;
      ELSIF (NOT targetIsFromNode) AND (edge^.to = targetNode) THEN
        IF record THEN
          List.Push(self.curGen.actions, NEW(DelEdgeAction, e := edge));
        END (* IF *);
        IF i = 0 THEN
          realEdges := List.Tail(edges);
        ELSE
          List.SetNthTail(realEdges, i, List.Tail(edges));
        END;
        RETURN TRUE;
      END;
      edges := List.Tail(edges);
      INC(i);
    END;
    RETURN FALSE;
  END DeleteFromEdgeList;


(* EXTERNAL *)

PROCEDURE ChangeEdge(self: T; node1: NodeVal.T;
                     edgeVal: EdgeVal.T; node2: NodeVal.T)
            RAISES { NoSuchNode, NoSuchEdge } =
  BEGIN ChangeEdgeWork(self, node1, edgeVale, node2); END ChangeEdge;

PROCEDURE ChangeEdgeWork(self: T; node1: NodeVal.T;
                         edgeVal: EdgeVal.T; node2: NodeVal.T;
                         record: BOOLEAN := TRUE)
            RAISES { NoSuchNode, NoSuchEdge } =
  VAR
    fromNode, toNode: Node;
    edge: Edge;
  BEGIN
    (* These raise NoSuchNode. *)
    fromNode := self.nodeValToNode(node1, FALSE);
    toNode := self.nodeValToNode(node2, FALSE);
    IF NOT FindEdge(fromNode, toNode, edge) THEN
      RAISE NoSuchEdge;
    ELSE
      IF record THEN
        List.Push(self.curGen.actions, NEW(ChangeEdgeAction, e := edge,
                                           oldVal := edge^.value));
      END (* IF *);
      edge^.value := edgeVal;
    END;
  END ChangeEdge;


PROCEDURE SetEdge(self: T; node1: NodeVal.T;
                     edgeVal: EdgeVal.T; node2: NodeVal.T)
            RAISES { NoSuchNode } =
  VAR
    fromNode, toNode: Node;
    edge: Edge;
  BEGIN
    (* These raise NoSuchNode. *)
    fromNode := self.nodeValToNode(node1, FALSE);
    toNode := self.nodeValToNode(node2, FALSE);
    IF NOT FindEdge(fromNode, toNode, edge) THEN
      edge := NEW(Edge, value := edgeVal, from := fromNode, to := toNode);
      List.Push(fromNode^.succ, edge);
      List.Push(toNode^.pred, edge);
      INC(self.edges);

      List.Push(self.curGen.actions, NEW(AddEdgeAction, e := edge));
    ELSE
      List.Push(self.curGen.actions, NEW(ChangeEdgeAction, e := edge,
                                         oldVal := edge^.value));
      edge^.value := edgeVal;
    END;
  END SetEdge;


PROCEDURE NSucc(self: T; nodeVal: NodeVal.T): CARDINAL
    RAISES { NoSuchNode } =
  BEGIN
    RETURN List.Length(self.nodeValToNode(nodeVal, FALSE)^.succ);    
  END NSucc;
  

PROCEDURE GetSuccN(self: T; nodeVal: NodeVal.T; n: CARDINAL): NodeVal.T
    RAISES { NoSuchNode, RangeFault } =
  VAR
    node: Node;
  BEGIN
    node := self.nodeValToNode(nodeVal, FALSE);
    IF (n < 0) OR (n >= List.Length(node^.succ)) THEN
      RAISE RangeFault;
    ELSE
      RETURN NARROW(List.Nth(node^.succ, n), Edge)^.to^.value;
    END;
  END GetSuccN;

PROCEDURE GetSuccIter(self: T; nodeVal: NodeVal.T): NodeIter
    RAISES { NoSuchNode } =
  VAR
    node: Node;
    ni: NodeIter;
  BEGIN
    node := self.nodeValToNode(nodeVal, FALSE);
    ni := NEW(NodeIterImpl, toNotFrom := TRUE, list := node^.succ);
    RETURN ni;
  END GetSuccIter;


PROCEDURE NPred(self: T; nodeVal: NodeVal.T): CARDINAL
    RAISES { NoSuchNode } =
  BEGIN
    RETURN List.Length(self.nodeValToNode(nodeVal, FALSE)^.pred);    
  END NPred;

PROCEDURE GetPredN(self: T; nodeVal: NodeVal.T; n: CARDINAL): NodeVal.T
    RAISES { NoSuchNode, RangeFault } =
  VAR
    node: Node;
  BEGIN
    node := self.nodeValToNode(nodeVal, FALSE);
    IF (n < 0) OR (n >= List.Length(node^.pred)) THEN
      RAISE RangeFault;
    ELSE
      RETURN NARROW(List.Nth(node^.pred, n), Edge)^.from^.value;
    END;
  END GetPredN;

PROCEDURE GetPredIter(self: T; nodeVal: NodeVal.T): NodeIter
    RAISES { NoSuchNode } =
  VAR
    node: Node;
    ni: NodeIter;
  BEGIN
    node := self.nodeValToNode(nodeVal, FALSE);
    ni := NEW(NodeIterImpl, toNotFrom := FALSE, list := node^.pred);
    RETURN ni;
  END GetPredIter;



PROCEDURE NodeIterNext(self: NodeIterImpl; VAR next: NodeVal.T): BOOLEAN =
  VAR
    edge: Edge;
  BEGIN
    IF self.list = NIL THEN RETURN FALSE; END;
    edge := NARROW(List.First(self.list), Edge);
    self.list := List.Tail(self.list);
    IF self.toNotFrom THEN
      next := edge^.to^.value;
    ELSE
      next := edge^.from^.value;
    END;
    RETURN TRUE;
  END NodeIterNext;


(*==================== Whole-Graph Iteration ====================*)

PROCEDURE SetMarks(g: T; b: BOOLEAN := FALSE) =
  VAR dummyKey, dummyValue: REFANY;
  BEGIN
    WITH rb = NEW(REF BOOLEAN) DO
      rb^ := b;
      EVAL g.nodeTbl.enumerate(SetMarksWork, <*NOWARN*>
                               rb, dummyKey, dummyValue);
    END (* WITH *);
  END SetMarks;

PROCEDURE SetMarksWork(ra: REFANY; <*UNUSED*> key: REFANY;
                       VAR nodeRA: REFANY): BOOLEAN =
  BEGIN
    WITH node = NARROW(nodeRA, Node), rb = NARROW(ra, REF BOOLEAN) DO
      node.mark := rb^;
    END (* WITH *);
    RETURN FALSE;
  END SetMarksWork;

TYPE
  EMPRefRec = REF RECORD emp: EdgeMapProc; END (* RECORD *);

PROCEDURE MapOverEdges(self: T; emp: EdgeMapProc) RAISES ANY =
  VAR dummyKey, dummyValue: REFANY;
  BEGIN
    SetMarks(self, FALSE);
    WITH empRR = NEW(EMPRefRec, emp := emp) DO
      EVAL self.nodeTbl.enumerate(DfsEdges, empRR, dummyKey, dummyValue);
    END (* WITH *);
    SetMarks(self, FALSE);
  END MapOverEdges;

PROCEDURE DfsEdges(ra: REFANY; <*UNUSED*> key: REFANY;
                   VAR nodeRA: REFANY): BOOLEAN RAISES ANY =
  BEGIN
    WITH node = NARROW(nodeRA, Node),
         empRR = NARROW(ra, EMPRefRec) DO
      DfsEdgesMap(node, empRR.emp);
    END (* WITH *);
    RETURN FALSE;
  END DfsEdges;      

PROCEDURE DfsEdgesMap(n: Node; emp: EdgeMapProc) RAISES ANY =
  VAR succs: List.T (* OF Edge *);
  BEGIN
    IF n.mark THEN RETURN;
    ELSE
      n.mark := TRUE;
      succs := n.succ;
      WHILE succs # NIL DO
        WITH e = NARROW(List.First(succs), Edge) DO
          emp(n.value, e.value, e.to.value);
          DfsEdgesMap(e.to, emp);
        END (* WITH *);
        succs := succs.tail;
      END (* WHILE *)
    END (* IF *);
  END DfsEdgesMap;


PROCEDURE MapOverNodes(self: T; nmp: NodeMapProc) RAISES ANY =
  VAR
    nodes: List.T (* OF Node *);
  BEGIN
    nodes := self.nodeTbl.toKeyList();
    WHILE nodes # NIL DO
      nmp(NARROW(List.First(nodes), NodeValRef)^);
      nodes := List.Tail(nodes);
    END (* WHILE *);
  END MapOverNodes;

(*
PROCEDURE DfsNodes(ra: REFANY; <*UNUSED*> key: REFANY;
                   VAR nodeRA: REFANY): BOOLEAN RAISES ANY =
  BEGIN
    WITH node = NARROW(nodeRA, Node),
         nmpRR = NARROW(ra, NMPRefRec) DO
      DfsNodesMap(node, nmpRR.proc);
    END (* WITH *);
    RETURN FALSE;
  END DfsNodes;

PROCEDURE DfsNodesMap(n: Node; nmp: NodeMapProc) RAISES ANY =
  VAR succs: List.T (* OF Edge *);
  BEGIN
    IF n.mark THEN RETURN;
    ELSE
      n.mark := TRUE;
      nmp(n.value);
      succs := n.succ;
      WHILE succs # NIL DO
        WITH e = NARROW(List.First(succs), Edge) DO
          DfsNodesMap(e.to, nmp);
        END (* WITH *);
        succs := succs.tail;
      END (* WHILE *)
    END (* IF *);
  END DfsNodesMap;
*)

(*====================== Transitive closure ======================*)
(* Modifies 'g' so that the final value of 'g' is the transitive closure
   of the initial value.   If all of etPlus, etTimes, etPlusIdent, and
   etTimesIdent are NIL, then edge with value NIL is added between nodes
   'n1' and 'n2' iff no edge connected them in the original value of 'g',
   but a path between 'n1' and 'n2' did exist in that original value.
   If any of the optional arguments are non-NIL, all must be, and they
   must form a "closed semi-ring" on the edge type.  We then run algorithm
   5.5, p. 198, "The Design and Analysis of Computer Algorithms", by Aho,
   Hopcroft, and Ullman, Addison-Wesley, 1974.
*)


PROCEDURE TransitiveClose(self: T; csr: ClosedSemiRing)
    RAISES ANY =
  VAR
    nodei, nodej, nodek: Node;
    edge, kkedge, ikedge, ijedge, kjedge: Edge;
    kkValClosure, ikVal, oldijVal, newijVal, kjVal: EdgeVal.T;
    succs: List.T (* OF Edge *);
    nodeArr: NodeArr;
    nNodes: CARDINAL;
  BEGIN
    (* Repack the array so we can index the nodes. *)
    IF csr = NIL THEN
      IF NOT tcDefaultsSet THEN RAISE BadSemiGroup END;
      csr := DefaultCSR;
    END (* IF *);
    nodeArr := self.makeNodeArray();
    nNodes := self.nodeSize();
    (* I'm going to code up an algorithm that assumes a sparse graph,
       where most of the values are represented by the lack of an edge
       (which corresponds to csr.etPlusIdent).  We might want to measure the
       number of edges against the number of nodes, and decide whether to
       do a "dense" version, in which we allocate an n^2 array... *)
    FOR k := 0 TO nNodes-1 DO
      nodek := nodeArr[k];
      
      IF NOT FindEdge(nodek, nodek, kkedge) THEN
        kkValClosure := csr.etClosure(csr.etTimesIdent);
      ELSE
        kkValClosure := csr.etClosure(kkedge^.value);
      END;

      FOR i := 0 TO nNodes-1 DO
        nodei := nodeArr[i];

        IF NOT FindEdge(nodei, nodek, ikedge) THEN
          ikVal := csr.etPlusIdent;
        ELSE
          ikVal := ikedge^.value;
        END;

        FOR j := 0 TO nNodes-1 DO
          nodej := nodeArr[j];
          IF NOT FindEdge(nodei, nodej, ijedge) THEN
            oldijVal := csr.etPlusIdent;
          ELSE
            oldijVal := ijedge^.value;
          END;

          IF NOT FindEdge(nodek, nodej, kjedge) THEN
            kjVal := csr.etPlusIdent;
          ELSE
            kjVal := kjedge^.value;
          END;
    
          newijVal := csr.etPlus(oldijVal,
                                 csr.etTimes(ikVal, csr.etTimes(kkValClosure,
                                                                kjVal)));
          IF (newijVal # csr.etPlusIdent) THEN
            (* There needs to be an edge... *)
            IF (oldijVal = csr.etPlusIdent) THEN
              (* ...but there was no edge before, so make one. *)
              ijedge := NEW(Edge);
              (* To make sure rest of this iteration is right. *)
              ijedge^.value := csr.etPlusIdent;
              ijedge^.nextValue := newijVal;
              ijedge^.from := nodei;
              ijedge^.to := nodej;
              List.Push(nodei^.succ, ijedge);
              List.Push(nodej^.pred, ijedge);
              INC(self.edges);

              List.Push(self.curGen.actions, NEW(AddEdgeAction, e := ijedge));
            ELSE
              (* ...and there is. *)
              ijedge^.nextValue := newijVal;

            END;
          END;
        END;
      END;

      (* Now update the 'values' of the edges to the 'nextValues.' *)
      FOR i := 0 TO nNodes-1 DO
        nodei := nodeArr[i];
        succs := nodei^.succ;
        WHILE succs # NIL DO
          edge := NARROW(List.First(succs), Edge);

          List.Push(self.curGen.actions,
                    NEW(ChangeEdgeAction, e := edge, oldVal := edge.value));

          edge^.value := edge^.nextValue;
          succs := List.Tail(succs);
        END;
      END
    END;
  END TransitiveClose;


PROCEDURE AddEdgeAndClose(self: T;
                          n1: NodeVal.T; ev: EdgeVal.T; n2: NodeVal.T;
                          csr: ClosedSemiRing) RAISES ANY =
  VAR
    oldVal, newVal: EdgeVal.T;
  BEGIN
    IF NOT self.getEdge(n1, n2, oldVal) THEN
      oldVal := csr.etPlusIdent;
    END (* IF *);
    newVal := csr.etPlus(oldVal, ev);
    IF NOT EdgeVal.Equal(oldVal, newVal) THEN
      self.setEdge(n1, newVal, n2); <*NOWARN*>
      CloseOnPreds(self, newVal, n1, n2, csr);
      CloseOnSuccs(self, newVal, n1, n2, csr);
    END (* IF *);
  END AddEdgeAndClose;


PROCEDURE CloseOnPreds(self: T; newVal: EdgeVal.T; n1, n2: NodeVal.T; 
                       csr: ClosedSemiRing) RAISES ANY =
  VAR
    ni: NodeIter := self.getPredIter(n1); <*NOWARN*>
    pred: NodeVal.T;
    oldEdge, predEdge, newEdge: EdgeVal.T;
  BEGIN
    WHILE ni.next(pred) DO
      predEdge := self.edgeValue(pred, n1); <*NOWARN*>
      IF NOT self.getEdge(pred, n2, oldEdge) THEN
        oldEdge := csr.etPlusIdent;
      END (* IF *);
      newEdge := csr.etPlus(oldEdge, csr.etTimes(predEdge, newVal));
      IF newEdge # csr.etPlusIdent THEN
        IF pred = n2 THEN
          (* We have a cycle! Set the edges between n1 and n2 to the closure
             of the edge we we about to add. *)
          WITH closeVal = csr.etClosure(newEdge) DO
            self.setEdge(n1, closeVal, n2);
            self.setEdge(n2, closeVal, n1);
          END (* WITH *);
        ELSE
          self.addEdgeAndClose(pred, newEdge, n2, csr);
        END (* IF *);
      END (* IF *);
    END (* WHILE *);
  END CloseOnPreds;
      
PROCEDURE CloseOnSuccs(self: T; newVal: EdgeVal.T; n1, n2: NodeVal.T; 
                       csr: ClosedSemiRing) RAISES ANY =
  VAR
    ni: NodeIter := self.getSuccIter(n2); <*NOWARN*>
    succ: NodeVal.T;
    oldEdge, succEdge, newEdge: EdgeVal.T;
  BEGIN
    WHILE ni.next(succ) DO
      succEdge := self.edgeValue(n2, succ); <*NOWARN*>
      IF NOT self.getEdge(n1, succ, oldEdge) THEN
        oldEdge := csr.etPlusIdent;
      END (* IF *);
      newEdge := csr.etPlus(oldEdge, csr.etTimes(newVal, succEdge));
      IF newEdge # csr.etPlusIdent THEN
        IF n1 = succ THEN
          (* We have a cycle! Set the edges between n1 and n2 to the closure
             of the edge we we about to add. *)
          WITH closeVal = csr.etClosure(newEdge) DO
            self.setEdge(n1, closeVal, n2);
            self.setEdge(n2, closeVal, n1);
          END (* WITH *);
        ELSE
          self.addEdgeAndClose(n1, newEdge, succ, csr);
        END (* IF *);
      END (* IF *);
    END (* WHILE *);
  END CloseOnSuccs;
      

(*******************************************************************)

PROCEDURE PrintAsMatrix(self: T; wr: Wr.T;
                        np: NodePrintProc;
                        ep: EdgePrintProc;
                        between, colWidth: CARDINAL;
                        absentEV: EdgeVal.T) =
  VAR
    nodei, nodej: Node;
    edge: Edge;
    nodeArr: NodeArr;
    nNodes: CARDINAL;
  BEGIN
    (* Repack the array so we can index the nodes. *)
    nodeArr := self.makeNodeArray();
    nNodes := self.nodeSize();
    (* Print the top line *)
    FOR i := 1 TO colWidth+1 DO Wr.PutChar(wr, ' '); END;
    FOR i := 0 TO nNodes-1 DO
      FOR j := 1 TO between DO Wr.PutChar(wr, ' '); END;
      nodei := nodeArr[i];
      np(wr, nodei^.value, colWidth);
    END;
    Wr.PutChar(wr, '\n');
    FOR i := 1 TO colWidth+between DO Wr.PutChar(wr, ' '); END;
    Wr.PutChar(wr, '+');
    FOR i := 1 TO nNodes*colWidth + (nNodes-1)*between DO
      Wr.PutChar(wr, '-');
    END;
    Wr.PutChar(wr, '\n');

    FOR i := 0 TO nNodes-1 DO
      nodei := nodeArr[i];
      np(wr, nodei^.value, colWidth);
      FOR j := 1 TO between DO Wr.PutChar(wr, ' '); END;
      Wr.PutChar(wr, '|');
      FOR j := 0 TO nNodes-1 DO
        nodej := nodeArr[j];
        IF FindEdge(nodei, nodej, edge) THEN
          ep(wr, TRUE, edge.value, colWidth);
        ELSE
          ep(wr, FALSE, absentEV, colWidth);
        END;
        FOR k := 1 TO between DO Wr.PutChar(wr, ' '); END;
      END;
      Wr.PutChar(wr, '\n');
    END;
  END PrintAsMatrix;


(* These routines provide the simplest form of transitive closure algorithm,
   the default described in the comments above.  If you don't want labelled
   edges, use REFANY as the edge type. *)

VAR
  tcDefaultsSet: BOOLEAN;
  DefETTimesIdent, DefETPlusIdent: EdgeVal.T;

PROCEDURE SetTCDefaults(timesIdent, plusIdent: EdgeVal.T) = 
  BEGIN
    DefETTimesIdent := timesIdent;
    DefETPlusIdent := plusIdent;
    DefaultCSR.etTimesIdent := timesIdent;
    DefaultCSR.etPlusIdent := plusIdent;
    tcDefaultsSet := TRUE;
  END SetTCDefaults;


PROCEDURE EdgeAnd(ev1, ev2: EdgeVal.T): EdgeVal.T =
  BEGIN
    IF (ev1 = DefETPlusIdent) OR (ev2 = DefETPlusIdent) THEN
      RETURN DefETPlusIdent;
    ELSE
      RETURN DefETTimesIdent;
    END;
  END EdgeAnd;

PROCEDURE EdgeOr(ev1, ev2: EdgeVal.T): EdgeVal.T =
  BEGIN
    IF (ev1 # DefETPlusIdent) OR (ev2 # DefETPlusIdent) THEN
      RETURN DefETTimesIdent;
    ELSE
      RETURN DefETPlusIdent;
    END;
  END EdgeOr;

PROCEDURE EdgeTrue(<*UNUSED*> ev: EdgeVal.T): EdgeVal.T =
  BEGIN
    RETURN DefETTimesIdent;
  END EdgeTrue;



(* Push and pop procedures. *)

PROCEDURE Push(self: T): CARDINAL =
  BEGIN
    INC(self.gen);
    self.curGen := NEW(Generation, num := self.gen);
    List.Push(self.gens, self.curGen);
    RETURN self.gen;
  END Push;

PROCEDURE Pop(self: T): CARDINAL RAISES { EmptyStack } =
  VAR acts: List.T;
  BEGIN
    IF self.gens = NIL THEN RAISE EmptyStack; END (* IF *);
    WITH cur = List.Pop(self.gens) DO
      acts := cur.actions;
      WHILE acts # NIL DO
        WITH act = acts.first DO
          TYPECASE act OF
          | AddEdgeAction(ae) =>
              DeleteEdgeWork(self, ae.e.from.value, ae.e.to.value, FALSE);
          | AddNodeAction(an) =>
              DeleteNodeWork(self, an.n, FALSE);
          | DelEdgeAction(de) =>
              AddEdgeWork(self, de.e.from.value, de.e.value, de.e.to.value,
                          addNodes := FALSE, record := FALSE);
          | DelNodeAction(dn) =>
              AddNodeWork(self, dn.n, FALSE);
          | ChangeEdgeAction(ce) =>
              ChangeEdgeWork(self, ce.e.from.value, ce.oldVal,
                             ce.e.to.value, FALSE);
          ELSE
              <*ASSERT FALSE*>
          END (* TYPECASE *);
        END (* WITH *);
        acts := acts.tail;
      END (* WHILE *);
    END (* WITH *);

    DEC(self.gen);
    IF self.gens = NIL THEN
      self.curGen = NIL;
    ELSE
      self.curGen := List.First(self.gens);
    END (* IF *);
    RETURN self.gen;
  END Pop;
    


VAR
  InitDone: BOOLEAN;
  
PROCEDURE Init() =
  BEGIN
    IF InitDone THEN RETURN END;
    tcDefaultsSet := FALSE;
    DefaultCSR := NEW(ClosedSemiRing,
                      etPlus := EdgeOr, etTimes := EdgeAnd,
                      etClosure := EdgeTrue);
    InitDone := TRUE;
  END Init;

BEGIN
  Init();
END DiGraph.


