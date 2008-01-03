(* Copyright 1994 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Wed Oct  5 21:18:55 PDT 1994 by mhb       *)

INTERFACE StubImageVBT;

IMPORT PaintOp, VBT;
IMPORT StubImages AS Images;

TYPE T <: Public;

TYPE Public = VBT.Leaf OBJECT
  METHODS
    init(pm: Images.T; bg: PaintOp.T): T;
    put(pm: Images.T; bg: PaintOp.T);
    get(): Images.T;
  END;

END StubImageVBT.
