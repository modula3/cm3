(* Copyright 95 Digital Equipment Corporation.
   Digital Internal Use Only
   Last modified on Thu Feb  2 00:32:18 PST 1995 by detlefs
*)

INTERFACE POEdgeType;

TYPE
  T = { GT, GE, EQ, Absent, Bottom };

  CSRPublic = OBJECT
    plusIdent, bottom: T;
   METHODS
    init(): CSRPublic;
    plus(ev1, ev2: T): T;
    times(ev1, ev2: T): T;
    closure(ev: T): T;
  END (* OBJECT *);
  CSR <: CSRPublic;

VAR csr: CSR;

END POEdgeType.
