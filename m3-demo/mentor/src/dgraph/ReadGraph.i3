(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE ReadGraph;

IMPORT AdjMatrix, Algorithm, Thread;

PROCEDURE In(alg: Algorithm.T): AdjMatrix.T RAISES {Thread.Alerted};
(* returns a graph as an adjacency matrix (somehow) *)

END ReadGraph.
