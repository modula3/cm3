(* Copyright 1994 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Wed Oct  5 21:18:04 PDT 1994 by mhb       *)

MODULE StubImageRd;

IMPORT PaintOp, VBT, Rd;

REVEAL T = Public BRANDED OBJECT
  OVERRIDES
    init := Init;
  END;

<*NOWARN *> PROCEDURE Init(t: T; rd: Rd.T; start, length: CARDINAL;
<*NOWARN *>   op: PaintOp.T := PaintOp.Copy;st: VBT.ScreenType := NIL; gamma: REAL := 1.0):
  T = BEGIN RETURN t END Init;

BEGIN
END StubImageRd.
