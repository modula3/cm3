(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: Queue.mg,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

GENERIC MODULE Queue(Elem);
REVEAL
  T = Public BRANDED OBJECT
    n: CARDINAL;
    a: REF ARRAY OF Elem.T;
    back, front: CARDINAL;
  OVERRIDES
    init := Init;
    get := Get;
    put := Put;
  END;

PROCEDURE Init(self: T): T =
  BEGIN
    self.n := 16;
    self.a := NEW(REF ARRAY OF Elem.T, self.n);
    self.back := 0;
    self.front := 0;
    RETURN self;
  END Init;

PROCEDURE DecMod(VAR i: CARDINAL; n: CARDINAL) =
  BEGIN
    IF i = 0 THEN
      i := n-1;
    ELSE
      DEC(i);
    END;
  END DecMod;

PROCEDURE NegSize(self: T): CARDINAL =
  BEGIN
    RETURN (self.back-self.front+self.n) MOD self.n;
  END NegSize;

PROCEDURE Get(self: T; VAR e: Elem.T): BOOLEAN =
  BEGIN
    IF NegSize(self) = 0 THEN
      RETURN FALSE;
    ELSE
      DecMod(self.front, self.n);
      e := self.a[self.front];
      RETURN TRUE;
    END;
  END Get;

PROCEDURE Put(self: T; e: Elem.T) =
  VAR
    old: REF ARRAY OF Elem.T;
  BEGIN
    IF NegSize(self) = -1 THEN
      old := self.a;
      self.a := NEW(REF ARRAY OF Elem.T, self.n * 2);
      SUBARRAY(self.a^, 0, self.n) := old^;
      SUBARRAY(self.a^, self.n, self.n) := old^;
      INC(self.n, self.n);
      IF NegSize(self) = -1 THEN
        INC(self.front, NUMBER(old^));
      END;
    END;
    DecMod(self.back, self.n);
    self.a[self.back] := e;
  END Put;

BEGIN
END Queue.
