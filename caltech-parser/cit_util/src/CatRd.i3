INTERFACE CatRd;
IMPORT Rd;
TYPE
  T = Rd.T;
PROCEDURE Cat(r0, r1: T): T;
(* future implementations might actually be efficient. *)
END CatRd.
