(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Oct 31 09:33:32 PST 1994 by heydon                   *)

MODULE Egraph;

IMPORT Equiv;

REVEAL T = Public BRANDED "Egraph.T" OBJECT OVERRIDES init := Init END;

PROCEDURE Init (e: T): T =
  BEGIN
    EVAL Equiv.T.init(e);
    RETURN e
  END Init;

BEGIN
END Egraph.
