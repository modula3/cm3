(*--------------------------------------------------------------------------*)
INTERFACE DependencyGraph;

IMPORT Pathname, Wr;
IMPORT StdDepGraphNode, StdDepGraphNodeSeq;

(*--------------------------------------------------------------------------*)
VAR paranoid := FALSE;

(*--------------------------------------------------------------------------*)
TYPE
  (*
    The UpdateClosure defines two predicates that are used to determine
    the condition that a node has to be updated, which is if (a) it does 
    not exist or (b) any of its predecessors in the graph is newer.
  *)
  UpdateClosure = OBJECT
  METHODS
    exists(node : StdDepGraphNode.T) : BOOLEAN;
    newer(pred, node : StdDepGraphNode.T) : BOOLEAN;
  END;

  (* 
     This is the public interface of a dependency graph. Nodes are
     referenced by their names (which must be unique) and may be added
     via addElem(). Nodes have an associated action (which is
     considered to be able to `update' the node) and may be `phony'
     (in which case the node will always be considered to need an
     update). 

     Dependencies are added by addDependency() and removed by
     removeDependency(); they may also be extracted from a makefile
     dependency line (like `a: b c d') by addMakefileDependencies() or
     read directly from a depend file (as constructed by gcc -M -MG,
     for example) by AddFromDependFile().

     If the graph is acyclic, topologicalSort() returns an ordered
     sequence of all the nodes (if not, a checked runtime error
     occurs). 

     nodesToBeUpdated(target) returns an ordered sequence of nodes
     that need to be updated in order to update node `target'.

  *)

  Public = OBJECT
  METHODS
    init(udc : UpdateClosure) : T;
    save(fn : Pathname.T) : BOOLEAN;
    load(fn : Pathname.T) : BOOLEAN;
    saveAsText(fn : Pathname.T) : BOOLEAN;
    loadAsText(fn : Pathname.T) : BOOLEAN;
    dump(wr : Wr.T);
    reset();
    addElem(n : TEXT; action : TEXT := NIL; phony := FALSE);
    addDependency(source, dest : TEXT);
    removeDependency(source, dest : TEXT);
    addMakefileDependencies(deps : TEXT);
    addFromDependFile(fn : Pathname.T) : BOOLEAN;
    nodes() : StdDepGraphNodeSeq.T;
    nodeExists(name : TEXT) : BOOLEAN;
    getNode(name : TEXT) : StdDepGraphNode.T;
    topologicalSort() : StdDepGraphNodeSeq.T;
    nodesToBeUpdated(target : TEXT) : StdDepGraphNodeSeq.T;
    dependingNodes(start : TEXT) : StdDepGraphNodeSeq.T;
    nodeDependencies(start : TEXT) : StdDepGraphNodeSeq.T;
    setUpdateClosure(udc : UpdateClosure);
  END;

  T <: Public;

(*--------------------------------------------------------------------------*)
PROCEDURE New(udc : UpdateClosure) : T;
  (* Create a new empty dependency graph. *)

(*--------------------------------------------------------------------------*)
PROCEDURE Init(self : T; udc : UpdateClosure) : T;
  (* Initialize a dependency graph. *)

(*--------------------------------------------------------------------------*)
PROCEDURE SetUpdateClosure(self : T; udc : UpdateClosure);
  (* Define a new update closure for the graph. *)

(*--------------------------------------------------------------------------*)
PROCEDURE Save(self : T; fn : Pathname.T) : BOOLEAN;
  (* Save the actual dependency graph in a file named `fn'. Return TRUE
     on success. *)

(*--------------------------------------------------------------------------*)
PROCEDURE Load(self : T; fn : Pathname.T) : BOOLEAN;
  (* Load a new dependency graph from the file `fn'. Return TRUE on 
     success. *)

(*--------------------------------------------------------------------------*)
PROCEDURE SaveAsText(self : T; fn : Pathname.T) : BOOLEAN;
  (* Save the actual dependency graph in a file named `fn' in ASCII
     representation. Return TRUE on success. *)

(*--------------------------------------------------------------------------*)
PROCEDURE LoadAsText(self : T; fn : Pathname.T) : BOOLEAN;
  (* Load a new dependency graph from the file `fn' in ASCII representation. 
     Return TRUE on success. *)

(*--------------------------------------------------------------------------*)
PROCEDURE Dump(self : T; wr : Wr.T);
  (* Dump the actual dependency graph onto stream `wr'. *)

(*--------------------------------------------------------------------------*)
PROCEDURE Reset(self : T);
  (* Reset all hit markers in the dependency graph. *)

(*--------------------------------------------------------------------------*)
PROCEDURE AddElem(self : T; n: TEXT; action : TEXT := NIL; phony := FALSE);
  (* Add a new element to the dependency graph. If it is a derived file,
     a command to create or update it should be supplied in `action',
     otherwise `action' must be NIL. *)

(*--------------------------------------------------------------------------*)
PROCEDURE AddDependency(self : T; source, dest: TEXT);
  (* Add a new dependency: `dest' depends on `source'. *)

(*--------------------------------------------------------------------------*)
PROCEDURE RemoveDependency(self : T; source, dest : TEXT);
  (* Remove any dependencies from `source' to `dest'. *)

(*--------------------------------------------------------------------------*)
PROCEDURE AddMakefileDependencies(self : T; deps : TEXT);
  (* Add all dependencies from `dep', which lists them in Makefile style. *)

(*--------------------------------------------------------------------------*)
PROCEDURE AddFromDependFile(self : T; fn : Pathname.T) : BOOLEAN;
  (* Add all dependencies from makefile `fn'. Return TRUE if the file has
     been read successfully. *)
 
(*--------------------------------------------------------------------------*)
PROCEDURE TopologicalSort(self : T) : StdDepGraphNodeSeq.T;
  (* Return a topologically sorted sequence of nodes, if possible. *)

(*--------------------------------------------------------------------------*)
PROCEDURE Nodes(self : T) : StdDepGraphNodeSeq.T;
  (* Return the list of all nodes of the graph. *)

(*--------------------------------------------------------------------------*)
PROCEDURE NodesToBeUpdated(self : T; target : TEXT) : StdDepGraphNodeSeq.T;
  (* Return the list of nodes must be updated to update `target'. *)

(*--------------------------------------------------------------------------*)
PROCEDURE DependingNodes(self : T; start : TEXT) : StdDepGraphNodeSeq.T;
  (* Return the list of nodes that are dependend on node `start'. *)

(*--------------------------------------------------------------------------*)
PROCEDURE NodeDependencies(self : T; start : TEXT) : StdDepGraphNodeSeq.T;
  (* Return the list of nodes that node `start' depends on. *)

(*--------------------------------------------------------------------------*)
PROCEDURE NodeExists(self : T; f : TEXT) : BOOLEAN;

(*--------------------------------------------------------------------------*)
PROCEDURE GetNode(self : T; f : TEXT) : StdDepGraphNode.T;


END DependencyGraph.
