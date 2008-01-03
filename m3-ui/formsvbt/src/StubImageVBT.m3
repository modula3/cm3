(* Copyright 1994 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Wed Oct  5 21:18:50 PDT 1994 by mhb       *)

MODULE StubImageVBT;

IMPORT PaintOp;
IMPORT StubImages AS Images;

REVEAL T = Public BRANDED OBJECT
  OVERRIDES
    init := Init;
    put := Put;
    get := Get;
  END;

<*NOWARN*> PROCEDURE Init(v: T; pm: Images.T; bg: PaintOp.T): T =
  BEGIN
    RETURN v
  END Init;

<*NOWARN*> PROCEDURE Put(v: T; pm: Images.T; bg: PaintOp.T) =
  BEGIN
  END Put;

<*NOWARN*> PROCEDURE Get(v: T): Images.T =
  BEGIN
    RETURN NIL
  END Get;

BEGIN
END StubImageVBT.
