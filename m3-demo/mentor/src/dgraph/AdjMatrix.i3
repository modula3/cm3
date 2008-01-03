(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)


INTERFACE AdjMatrix;

IMPORT Rd, R2, Thread;

TYPE
  T <: Public;
  Public = OBJECT
  METHODS
    init(n: INTEGER; selfEdges := FALSE): T;
    initFromRd(rd: Rd.T): T RAISES {Thread.Alerted};
    name(i: INTEGER): TEXT;
    coord(i: INTEGER): R2.T;
    putEdge(i, j: INTEGER; b: BOOLEAN);
    getEdge(i, j: INTEGER): BOOLEAN;
    nVertices(): INTEGER;
    edgeIter(): Iter;
  END; (* object *)

TYPE
  Iter <: IterPublic;
  IterPublic = OBJECT
  METHODS
     next(VAR i, j: INTEGER): BOOLEAN;
  END;

PROCEDURE ToText(t: T): TEXT RAISES {};
(* render the array as a TEXT!. Required by Zeus. *)

TYPE RC = {Row, Column}; RCSet = SET OF RC;
CONST Row = RCSet{RC.Row}; Column = RCSet{RC.Column};

PROCEDURE RCToText(rcset: RCSet): TEXT RAISES {};
(* ditto for an "rcset" *)

END AdjMatrix.

