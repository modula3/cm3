(* Copyright 1993 Digital Equipment Corporation.                             *)
(* Distributed only by permission.                                           *)
(*                                                                           *)
(* Last modified on Fri Aug  6 21:04:03 PDT 1993 by heydon                   *)

INTERFACE Topology;

IMPORT AlgGreedy, Graph, Rd, Thread;

TYPE
  Kind = { Grid, Torus, Butterfly, FromFile };

PROCEDURE FromName(nm: TEXT): Kind;
(* Return the topology type corresponding to the name "nm". The valid names
   are the same as the names of the "Topology.T" enumerations, except that
   they begin with lower-case letters. *)

PROCEDURE NewGrid(
    alg: AlgGreedy.T;
    w, h: CARDINAL;
    maxQueueSize: CARDINAL;
    bounded: BOOLEAN)
  : Graph.T
  RAISES {Thread.Alerted};
(* Return a new graph whose topology is a grid with "h" rows and "w" columns.
   Each node is connected to its north, east, south, and west neighbors.
   "MaxQueueSize" is the maximum size of the queue at each node if "bouned" is
   true; otherwise, the queues are unbounded, but "maxQueueSize" is the
   maximum expected queue size. Generate the interesting events to construct
   this graph. *)

PROCEDURE NewTorus(
    alg: AlgGreedy.T;
    w, h: CARDINAL;
    maxQueueSize: CARDINAL;
    bounded: BOOLEAN)
  : Graph.T
  RAISES {Thread.Alerted};
(* Like "NewGrid", but "w" + "h" extra edges are added so the topology is a
   toroidal grid. *)

PROCEDURE NewButterfly(
    alg: AlgGreedy.T;
    dim: CARDINAL;
    maxQueueSize: CARDINAL;
    bounded: BOOLEAN)
  : Graph.T
  RAISES {Thread.Alerted};
(* Return a new graph whose topology is a butterfly with dimension "dim". The
   resulting graph is a grid of "dim + 1" columns and "2^dim" rows. There is
   an edge between "(i, j)" and "(i', j')" iff "i + 1 = i'" and either "j =
   j'" or the binary values for "j" and "j'" differ in the "i"th bit. The
   procedure generates the interesting events to construct the graph. *)

EXCEPTION BadGraph(TEXT);
(* The argument to the "BadGraph" exception is an error message describing why
   the graph cannot be created. *)

PROCEDURE NewFromFile(
    alg: AlgGreedy.T;
    rd: Rd.T;
    maxQueueSize: CARDINAL;
    bounded: BOOLEAN)
  : Graph.T
  RAISES {BadGraph, Thread.Alerted};
(* Return a new graph defined by the next s-expression in "rd". "MaxQueueSize"
   and "bounded" are as in the "NewGrid" procedure above. Generate the
   interesting events to construct this graph. *)

END Topology.
