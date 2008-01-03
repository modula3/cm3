(* Copyright 95 Digital Equipment Corporation.
   Digital Internal Use Only
   Last modified on Wed Feb  1 16:00:03 PST 1995 by detlefs
*)

MODULE NullEdgeType;

TYPE
  CSRPrivate = CSR OBJECT
   OVERRIDES
    init := Init;
    plus := Plus;
    times := Times;
  END (* OBJECT *);

REVEAL
  CSRCycles = CSRPrivate BRANDED OBJECT
   OVERRIDES
    closure := ClosureCyclesOK;
  END (* OBJECT *);
  CSRNoCycles = CSRPrivate BRANDED OBJECT
   OVERRIDES
    closure := ClosureNoCycles;
  END (* OBJECT *);

PROCEDURE Init(self: CSRPrivate): CSR =
  BEGIN
    self.plusIdent := T.Absent;
    self.bottom := T.Bottom;
    RETURN self
  END Init;

PROCEDURE Plus(<*UNUSED*> self: CSR; e1, e2: T): T =
  BEGIN
    IF (e1 = T.Bottom) OR (e2 = T.Bottom) THEN
      RETURN T.Bottom
    ELSIF (e1 = T.Present) OR (e2 = T.Present) THEN
      RETURN T.Present
    ELSE
      RETURN T.Absent
    END
  END Plus;

PROCEDURE Times(<*UNUSED*> self: CSR; e1, e2: T): T =
  BEGIN
    IF (e1 = T.Bottom) OR (e2 = T.Bottom) THEN
      RETURN T.Bottom
    ELSIF (e1 = T.Present) AND (e2 = T.Present) THEN
      RETURN T.Present
    ELSE
      RETURN T.Absent
    END
  END Times;

PROCEDURE ClosureCyclesOK(<*UNUSED*> self: CSR; e: T): T =
  BEGIN
    IF e = T.Bottom THEN RETURN e
    ELSE RETURN T.Present
    END (* IF *)
  END ClosureCyclesOK;

PROCEDURE ClosureNoCycles(<*UNUSED*> self: CSR; e: T): T =
  BEGIN
    CASE e OF
    | T.Bottom, T.Present => RETURN T.Bottom
    | T.Absent => RETURN T.Present
    END (* CASE *)
  END ClosureNoCycles;

BEGIN
  cyclesOK := NEW(CSRCycles).init();
  cyclesNO := NEW(CSRNoCycles).init();
END NullEdgeType.
