(* Copyright 1990 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Wed Oct  5 21:17:48 PDT 1994 by mhb       *)

INTERFACE StubImageRd;

IMPORT PaintOp, Rd, VBT;
IMPORT StubImages AS Images;

TYPE
  T <: Public;
  Public = Images.T OBJECT
           METHODS
             init (rd           : Rd.T;
                   start, length: CARDINAL;
                   op           : PaintOp.T  := PaintOp.Copy;
                   st   : VBT.ScreenType := NIL;
                   gamma: REAL           := 1.0  ): T;
           END;

END StubImageRd.
