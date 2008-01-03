(* Copyright 95 Digital Equipment Corporation.
   Digital Internal Use Only
   Last modified on Thu Feb  2 15:45:16 PST 1995 by detlefs
*)

MODULE POEdgeType;

REVEAL
  CSR = CSRPublic BRANDED OBJECT
   OVERRIDES
    init := Init;
    plus := Plus;
    times := Times;
    closure := Closure;
  END (* OBJECT *);

PROCEDURE Init(self: CSR): CSRPublic =
  BEGIN
    self.plusIdent := T.Absent;
    self.bottom := T.Bottom;
    RETURN self
  END Init;

TYPE
  Row = ARRAY T OF T;
  Mat = ARRAY T OF Row;

CONST
  PlusTbl = 
    Mat{(* GT *)     Row{T.GT,     T.GT,     T.Bottom, T.GT,     T.Bottom},
        (* GE *)     Row{T.GT,     T.GE,     T.EQ,     T.GE,     T.Bottom},
        (* EQ *)     Row{T.Bottom, T.EQ,     T.EQ,     T.EQ,     T.Bottom},
        (* Absent *) Row{T.GT,     T.GE,     T.EQ,     T.Absent, T.Bottom},
        (* Bottom *) Row{T.Bottom, T.Bottom, T.Bottom, T.Bottom, T.Bottom}};
  TimesTbl = 
    Mat{(* GT *)     Row{T.GT,     T.GT,     T.GT,     T.Absent, T.Bottom},
        (* GE *)     Row{T.GT,     T.GE,     T.GE,     T.Absent, T.Bottom},
        (* EQ *)     Row{T.GT,     T.GE,     T.EQ,     T.Absent, T.Bottom},
        (* Absent *) Row{T.Absent, T.Absent, T.Absent, T.Absent, T.Bottom},
        (* Bottom *) Row{T.Bottom, T.Bottom, T.Bottom, T.Bottom, T.Bottom}};
  ClosureTbl =
    Row{T.Bottom, T.EQ, T.EQ, T.EQ, T.Bottom};

PROCEDURE Plus(<*UNUSED*> self: CSR; e1, e2: T): T =
  BEGIN RETURN PlusTbl[e1, e2]
  END Plus;

PROCEDURE Times(<*UNUSED*> self: CSR; e1, e2: T): T =
  BEGIN RETURN TimesTbl[e1, e2]
  END Times;

PROCEDURE Closure(<*UNUSED*> self: CSR; e: T): T =
  BEGIN RETURN ClosureTbl[e]
  END Closure;

BEGIN
  csr := NEW(CSR).init();
END POEdgeType.
