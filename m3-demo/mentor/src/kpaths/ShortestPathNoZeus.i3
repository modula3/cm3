(* Copyright 1993 by Digital Equipment Corp. *)

(* This interface provides an algorithm for computing shortest paths in weighted
   directed graphs. *)
   
INTERFACE ShortestPath;

IMPORT RefList, Atom;

TYPE 
  T <: Public;
  Public = OBJECT METHODS
    init(k: CARDINAL := 1): T;
    addVertex(rl: RefList.T);
    addEdge(rl: RefList.T);
    shortestPath(source, destination: Atom.T; rank: CARDINAL := 0): RefList.T;
    weight(edge: Atom.T): REAL
  END;
    

(* The call "NEW(T).init(k)" returns a new empty graph with {\it rank limit} "k".
   The "ShortestPath.T" object is unmonitored.

   The call "sp.addVertex(rl)" adds to "sp" a vertex whose name (as an atom) is 
   the first element of "rl".  The second and third elements of "rl", if present, 
   are the suggested "x" and "y" coordinates for the vertex (these are
   useful if the implementation is being animated).

   The call "sp.addEdge(rl)" add to "sp" an edge whose name (as an atom) is
   the first element of "rl".  The source and destination of the the edge
   are the atoms for the source and destination vertices of the edge.
   The fourth element of "rl", if present, is the cost of traversing the edge
   (as a REAL).  If it is absent, the cost is "1.0".  It is a checked runtime
   error if the weight is negative.

   The call "sp.shortestPath(s, d, j)" returns the "j"th shortest path
   ("j=0" means shortest) from "s" to "d" as an alternating list of
   vertex names and edge names, starting with "s" and ending with "d".
   The method returns "NIL" if there is no path from "s" to "d".

   Repeated calls to the "shortestPath" method are inexpensive provided
   that they have the same source vertex and that "j" is less than the
   rank limit and that the graph hasn't changed.

   The call "sp.weight(e)" returns the weight of the edge named "e",
   or "-1" if there is no edge named "e".
   
*)
   
END ShortestPath.
