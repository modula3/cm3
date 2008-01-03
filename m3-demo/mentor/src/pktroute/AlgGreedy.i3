(* Copyright 1993 Digital Equipment Corporation.                             *)
(* Distributed only by permission.                                           *)
(*                                                                           *)
(* Last modified on Sun Aug  8 15:06:51 PDT 1993 by heydon                   *)

INTERFACE AlgGreedy;

IMPORT Packet, Graph, ASP, RefIntArray;
IMPORT PktRouteAlgClass;
IMPORT Thread, Random;

TYPE
  QueueSz = RefIntArray.T;
  (* If "qSz: QueueSz", then "qSz[i]" is the number of packets at node "i". *)

  T <: TPub;
  TPub = PktRouteAlgClass.T BRANDED OBJECT
    graph: Graph.T;			 (* underlying topology *)
    unweighted: ASP.Unweighted;		 (* all shortest unweighted paths *)
    random: Random.Default;		 (* random number generator *)
    maxQSz: INTEGER;			 (* -1 => unbounded *)
    qSz: QueueSz;			 (* # of pkts at each node *)
  METHODS
    newPkt(): Packet.T			 (* create a new packet *)
  END;

PROCEDURE Run(alg: T) RAISES {Thread.Alerted};
(* The procedure to invoke to run the algorithm. *)

END AlgGreedy.
