(* Copyright 95 Digital Equipment Corporation.
   Digital Internal Use Only
   Last modified on Wed Feb  1 15:58:09 PST 1995 by detlefs
*)

INTERFACE NullEdgeType;

TYPE
  T = { Present, Absent, Bottom };

  (* Equivalent to DiGraph(NullEdgeType).ClosedSemiRing *)
  CSR = OBJECT
   plusIdent, bottom: T;
   METHODS
    init(): CSR;
    plus(ev1, ev2: T): T;
    times(ev1, ev2: T): T;
    closure(ev: T): T;
  END (* RECORD *);
  CSRCycles <: CSR;
  CSRNoCycles <: CSR;

VAR
  cyclesOK: CSRCycles;
  cyclesNO: CSRNoCycles;

END NullEdgeType.
