(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jun 16 16:46:31 PDT 1992 by muller                   *)

INTERFACE GraphData;

(* This is a standard format for passing around the description of a graph.
   These are graphs of the type that may include two or more distinct edges
   connecting the same pair of vertices, so an edge is known by a vertex
   number and an edge number within that node, rather than simply by a pair of
   vertices. For convenience of implementation of the intended clients, the
   number of edges to a single vertex is limited.

   The data structure is a REF array of node records, of which each
   record is assumed to actually represent a node.  Nodes are known
   by their indices in this array.
*)

IMPORT Text, RealPoint, RealRect, Rd;

CONST
  MaxEdge = 10;

TYPE
  Vertex = CARDINAL;
  EdgeIndex = [0..MaxEdge];

  PointArray = REF ARRAY OF RealPoint.T;

  EdgeRecord = RECORD
    dest: Vertex;
    destEdge: EdgeIndex;
    present: BOOLEAN;  (* FALSE => this is an empty slot, no edge here *)
    segPoints: PointArray;  (* bend points for segmented edges *)
    etc: REFANY;
  END;

  VertexRecord = RECORD
    name: Text.T;
    x, y: REAL;  (* coordinates for a visual representation *)
    api: REAL;
    edge: ARRAY EdgeIndex OF EdgeRecord;
    present: BOOLEAN;  (* FALSE => this is an empty slot, no vertex here *)
    etc: REFANY;
  END;

  T = REF ARRAY OF VertexRecord;


PROCEDURE Copy(g: T): T;
(* Copy a GraphData structure, copying every REF in it except for
   the name and etc fields, which are transferred by reference, 
   not copied. *)

PROCEDURE Expand(g: T; size: CARDINAL): T;
(* If NUMBER(g^) < size, then return a new GraphData structure, of which
   the first NUMBER(g^) fields are copied from the old one.  Else
   return the old one unchanged.  This copying differs from Copy in that
   the PointArray structures are transferred by reference, not copied. *)

PROCEDURE CountNodes(g: T): CARDINAL;
(* Count the number of nodes with present = TRUE *)

PROCEDURE BoundingBox(g: T): RealRect.T;
(* Compute the bounding box of all "present" vertices and segPoints
   in g. *)

PROCEDURE EdgeExists(g: T; a, b: CARDINAL): BOOLEAN;
(* Determine whether there is at least one edge between points
   a and b. *)

PROCEDURE RevPoints(in: PointArray): PointArray;
(* Produce a PointArray like <in> but in reverse order *)

PROCEDURE ExtractString(VAR line: Text.T): Text.T;

PROCEDURE ReadGraph(rd: Rd.T):T;

END GraphData.










