(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* File: DiGraph.def                                           *)
(* Last modified on Mon Oct 26 23:33:15 PST 1992 by detlefs    *)

GENERIC INTERFACE DiGraph(NodeVal, EdgeVal);

(* A parameterized directed graph abstraction.

   Requires:
     NodeVal.T: TYPE
     NodeVal.Hash: PROCEDURE(nv: NodeVal.T; lessThan: CARDINAL): CARDINAL;
     NodeVal.Equal: PROCEDURE(nv1, nv2: NodeVal.T): BOOLEAN;
     NodeVal.Compare: PROCEDURE(nv1, nv2: NodeVal.T): [-1..1];

     NodeVal.Equal(n1, n2) => NodeVal.Hash(n1, m) = NodeVal.Hash(n2, m)
     NodeVal.Equal(n1, n2) <=> NodeVal.Compare(n1, n2) = 0


   A DiGraph.T represents a directed graph whose nodes and edges are 
   labelled with values of types NodeVal and EdgeVal, respectively. 
   A DiGraph.T is initially empty; clients are allowed to add and 
   delete nodes and edges.  Observer functions allow clients to obtain 
   the nth predecessor or successor of a node, or to iterate over 
   all the predecessors or successors.  The most interesting function 
   provided by DiGraph is TransitiveClose; this is a generalized 
   transitive closure algorithm that can, with appropriate arguments, 
   add sufficient edges to transitively close a graph or, 
   alternatively, compute the shortest paths in a graph whose edges 
   are labelled with integers.

   The representation of DiGraph.T requires that the NodeVal type 
   provides hash and comparison operations.  A hash table maps NodeVal
   values into nodes in the graph.  Nodes contain lists of edges to 
   successors and predecessors.  Because of this edge-list 
   implementation, this implementation of DiGraph is biased towards 
   sparse graphs.

   I hope eventually to have this interface support a number of 
   interesting graph algorithms, and to modify it to use the new 
   Vesta/Vulcan method of parameterization.  Any contributions of 
   work on this code is strongly welcomed.

   Index: graph; directed graph; network; transitive closure; relation
*)


IMPORT Wr;

EXCEPTION
  NoSuchNode; NoSuchEdge; DupNode; DupEdge; RangeFault; BadSemiGroup;

TYPE
  (* These are used in transitive closure. *)
  EdgeProcBinary = PROCEDURE(ev1: EdgeVal.T; ev2: EdgeVal.T): EdgeVal.T
                       RAISES ANY;
  EdgeProcUnary = PROCEDURE(ev: EdgeVal.T): EdgeVal.T RAISES ANY;

  ClosedSemiRing = REF RECORD
    etPlus, etTimes: EdgeProcBinary;
    etClosure: EdgeProcUnary;
    etPlusIdent, etTimesIdent: EdgeVal.T;
  END (* RECORD *);

  (* These are used in printing. *)
  (* A PrintProc should take a node or edge and print it on 'wr', and print
     exactly 'width' characters on 'wr', truncating or padding with blanks as
     necessary. *)
  NodePrintProc = PROCEDURE(wr: Wr.T; nv: NodeVal.T; width: CARDINAL);
  EdgePrintProc = PROCEDURE(wr: Wr.T; exists: BOOLEAN; ev: EdgeVal.T;
                            width: CARDINAL);
  (* The 'exists' argument indicates whether an edge exists. *)

VAR
  DefaultCSR: ClosedSemiRing;

TYPE
  EdgeMapProc = PROCEDURE(n1: NodeVal.T; e: EdgeVal.T; n2: NodeVal.T)
                    RAISES ANY;
  NodeMapProc = PROCEDURE(n: NodeVal.T) RAISES ANY;


EXCEPTION EmptyStack;

TYPE
  TPublic = OBJECT
   METHODS
    new(): TPublic;
    (* Creates a new, empty graph (one with no nodes or edges.) *)

    nodeSize(): CARDINAL;
    (* Returns the number of nodes in 'self'. *)

    edgeSize(): CARDINAL;
    (* Returns the number of edges in 'self'. *)

    nodeExists(nv: NodeVal.T): BOOLEAN;
    (* Returns TRUE iff there exists a node 'n' in the 'g' such that
       NodeVal.Equal(n, nv) = TRUE. *)

    addNode(nv: NodeVal.T) RAISES { DupNode };
    (* If self.nodeExists(nv), raises NodeExists and does not modify 'self'.
       Otherwise, adds a node with value 'nv' (and no successors or
       predecessors) to 'self'.
    *)
    
    deleteNode(nv: NodeVal.T) RAISES { NoSuchNode};
    (* If self.nodeExists(nv), deletes the node associated with 'nv'
       and all incoming edges to and outgoing edges from that node from
       'self'.  Otherwise, raises NoSuchNode and does not modify 'self'.
    *)

    addEdge(node1: NodeVal.T; edgeVal: EdgeVal.T; node2: NodeVal.T;
            addNodes: BOOLEAN := FALSE)
        RAISES { NoSuchNode, DupEdge };
    (* If 'addNodes' is FALSE, and either of self.nodeExists(node1) or
       self.nodeExists(node2) is FALSE, then raises NoSuchNode.  If 'addNodes'
       is TRUE, adds 'node1' and/or 'node2' to 'self' if necessary to
       ensure that self.nodeExists(node1) and self.nodeExists(node2).  Next, if
       self.edgeExists(node1, node2), raises EdgeExists ('self' is not
       modified in this case, since if a node was added in the first
       step then there could not have been an edge between the input
       nodes.).  Otherwise, creates such an edge in 'self' and gives
       it the value 'edgeVal.'
    *)

    edgeExists(node1, node2: NodeVal.T): BOOLEAN;
    (* If self.nodeExists(node1) and self.nodeExists(node2) and an edge is
       presently defined between the nodes associated with those values in
       'self', returns TRUE; otherwise, returns FALSE.
    *)

    getEdge(node1, node2: NodeVal.T; VAR (*OUT*) ev: EdgeVal.T): BOOLEAN;
    (* If self.nodeExists(node1) and self.nodeExists(node2) and an edge is
       presently defined between the nodes associated with those values in
       'self', returns TRUE and sets 'ev' to the value of that edge; otherwise,
       returns FALSE.
    *)

    edgeValue(node1, node2: NodeVal.T): EdgeVal.T
        RAISES { NoSuchNode, NoSuchEdge };
    (* If NOT self.nodeExists(node1) or self.nodeExists(node2), raises
       NoSuchNode and does not modify 'self'.  If the nodes exist but
       NOT self.edgeExists(node1, node2), raises NoSuchEdge and does not
       modify 'self'.  Otherwise, if the nodes and an edge exist, returns the
       value associated with that edge.
    *)


    deleteEdge(node1, node2: NodeVal.T) RAISES { NoSuchNode, NoSuchEdge };
    (* If NOT self.nodeExists(node1) or NOT self.nodeExists(node2), raises
       NoSuchNode and does not modify 'self'.  If the nodes exist but
       NOT self.edgeExists(node1, node2), raises NoSuchEdge and does not
       modify 'self'.  Finally, if the nodes and an edge exist, deletes the
       edge between the nodes.
    *)


    setEdge(node1: NodeVal.T; edgeVal: EdgeVal.T; node2: NodeVal.T)
        RAISES { NoSuchNode };
    (* If NOT self.nodeExists(node1) or NOT self.nodeExists(node2), raises
       NoSuchNode and does not modify 'self'.  If the nodes exist but
       NOT self.edgeExists(node1, node2), creates a new edge between 'node1'
       and 'node2' with value 'edgeVal.'  If an edge already exists between the
       nodes, sets its value to 'edgeVal.'
    *)

    changeEdge(node1: NodeVal.T; edgeVal: EdgeVal.T; node2: NodeVal.T)
        RAISES { NoSuchNode, NoSuchEdge };
    (* If NOT self.nodeExists(node1) or NOT self.nodeExists(node2), raises
       NoSuchNode and does not modify 'self'.  If the nodes exist but
       NOT self.edgeExists(node1, node2), raises NoSuchEdge and does not
       modify 'self'.  Finally, if the nodes and an edge exist,
       changes the value of the edge to 'edgeVal.'
    *)

    nSucc(nodeVal: NodeVal.T): CARDINAL RAISES { NoSuchNode };
    (* If self.nodeExists(nodeVal), returns the number of successors
       of that node; otherwise, signals NoSuchNode.
    *)

    getSuccN(nodeVal: NodeVal.T; n: CARDINAL): NodeVal.T
        RAISES { NoSuchNode, RangeFault };
    (* If self.nodeExists(nodeVal) and that node has 'n'-1 or more successors,
       returns the NodeVal associated with the 'n'-th (0-based) successor.
       If NOT self.nodeExists(nodeVal), raises NoSuchNode; if n < 0 or n >= the
       number of successors of the node associated with 'nodeVal', raises
       RangeFault.
    *)

    getSuccIter(nodeVal: NodeVal.T): NodeIter RAISES { NoSuchNode };
    (* If self.nodeExists(nodeVal), returns a NodeIter that will yield all the
       successors of that node (assuming that the graph is not modified during
       the iteration.)  If NOT self.nodeExists(nodeVal), raises NoSuchNode.
    *)


    nPred(nodeVal: NodeVal.T): CARDINAL RAISES { NoSuchNode };
    (* If self.nodeExists(nodeVal), returns the number of successors
       of that node; otherwise, signals NoSuchNode.
    *)

    getPredN(nodeVal: NodeVal.T; n: CARDINAL): NodeVal.T
        RAISES { NoSuchNode, RangeFault };
    (* If self.nodeExists(nodeVal) and that node has 'n'-1 or more
       predecessors, returns the NodeVal associated with the 'n'-th
       (0-based) predecessor.  If NOT self.nodeExists(nodeVal), raises
       NoSuchNode; if n < 0 or n >= the number of predecessors of the
       node associated with 'nodeVal', raises RangeFault.
    *)


    getPredIter(nodeVal: NodeVal.T): NodeIter RAISES { NoSuchNode };
    (* If self.nodeExists(nodeVal), returns a NodeIter that will yield all the
       predecessors of that node (assuming that the graph is not
       modified during the iteration.)  If NOT
       self.nodeExists(nodeVal), raises NoSuchNode.
    *)

    mapOverNodes(nmp: NodeMapProc) RAISES ANY;
    (* Applies 'nmp' to every Node in 'self.' *)

    mapOverEdges(emp: EdgeMapProc) RAISES ANY;
    (* Applies 'emp' to every edge in 'self.' *)

    transitiveClose(csr: ClosedSemiRing := NIL) RAISES ANY;
    (* Modifies 'self' so that the final value of 'self' is the
       transitive closure of the initial value.   If all of 'etPlus',
       'etTimes', 'etPlusIdent', and 'etTimesIdent' are NIL, then edge
       with value NIL is added between nodes 'n1' and 'n2' iff no edge
       connected them in the original value of 'self', but a path
       between 'n1' and 'n2' did exist in that original graph.

       If any of the optional arguments are non-NIL, all must be, and they
       must form a "closed semi-ring" on the edge type.  See Section
       5.5, p. 198, "The Design and Analysis of Computer Algorithms", by Aho,
       Hopcroft, and Ullman, Addison-Wesley, 1974 for a complete explanation.

       To give a concrete example, assume that the edge values are
       positive integers, and we want to compute the shortest paths between
       nodes.  Here we use MIN as the 'Plus' operation of the
       semi-ring, addition as the 'Times' operation, and +infinity as
       the identity of the Plus operation.  More specifically:

       'etPlusIdent': -1 (conceptually representing +infinity)

       'etTimesIdent': 0 (since addition is the Times operation)

       'etPlus': Returns the sum of 'e1' and 'e2', unless one of 'e1'
       or 'e2' is -1 ('etTimesIdent'), in which case returns -1.

       'etTimes': the MIN function.  Returns the smaller of 'e1' and 'e2',
       unless one of those is -1 (etTimesIdent), in which case it returns the
       other.

       'etClosure': In general, should be
         etPlus(etTimesIdent, e, etTimes(e, e), etTimes(e, etTimes(e, e)), ...)
       which in this specific case is
         MIN(0, e, e+e, e+e+e, ...)
       or 0.

       The default arguments invoke the same general algorithm with EdgeProcs
       specialized to simply add extra NIL-valued edges to transitively close
       the graph.  If EdgeProcs are provided and the procedure can detect that
       they violate the specification, raises BadSemiGroup.
    *)

    addEdgeAndClose(n1: NodeVal.T; ev: EdgeVal.T; n2: NodeVal.T;
                    csr: ClosedSemiRing) RAISES ANY;
    (* "Conceptually" adds an edge of value 'ev' between 'n1' and 'n2', and
       transitively closes the graph according to the closed semi-ring 'csr'.
       That is, uses the "Plus" operation of csr to compute the new value of
       the edge from the old one, if any, and added edge value, and propogates
       any changes made through the graph.
    *)

    printAsMatrix(wr: Wr.T;
                  np: NodePrintProc; ep: EdgePrintProc;
                  between, colWidth: CARDINAL;
                  absentEV: EdgeVal.T);
    (* Prints 'self' in adjacency matrix form.  That is, prints a
       matrix whose rows and columns are labelled with node values and
       whose cells are labelled with edge values.  printAsMatrix
       writes its output to 'wr', and calls 'np' and 'ep' to do the
       printing.  'np' is called with 'wr', a NodeVal, and 'colwidth'
       as arguments.  'ep' is called with 'wr', a BOOLEAN that is TRUE
       IFF there is an edge between the nodes corresponding by the
       cell in the matrix, the EdgeVal for that cell if it exists, and
       'colWidth'.  The order of the nodes in the rows and columns is
       determined by the NodeVal.Compare passed to New. 
    *)


    (* These methods allow the user to establish "save points" and efficiently
       return the graph state as of those points.  The states are labelled with
       CARDINAL values; push and pop return the new state numbers (these return
       values might be useful for some trace messages in a backtracking search.)
    *)
    push(): CARDINAL;
    pop(): CARDINAL RAISES { EmptyStack };

  END (* OBJECT *);
  T <: TPublic;

  (* Node Iterators. *)    
  NodeIter = OBJECT
   METHODS
    next(VAR nv: NodeVal.T): BOOLEAN;
    (* If there are Nodes in 'self' that have not yet been yielded, returns
       TRUE and sets 'next' to the next node to be yielded.  Otherwise, the
       iteration is complete and FALSE is returned.
    *)
  END (* OBJECT *);

PROCEDURE SetTCDefaults(timesIdent, plusIdent: EdgeVal.T);
(* Sets the defaults to be used when the csr argument to
   transitiveClose is NIL.  'timesIdent' should be the value
   representing the presence of an edge, 'plusIdent' the absence of an
   edge.
*)


END DiGraph.


