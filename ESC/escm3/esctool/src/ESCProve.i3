(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

(* Wrapper around the Simplify Prover interface for ESC/M3. *)

INTERFACE ESCProve;

IMPORT RefList;

TYPE
  Result = OBJECT
    valid: BOOLEAN;
    labels: RefList.T;     (* of Atom.T -- counterexample labels *)
  END;

PROCEDURE Init();
(* Initialize the prover and load esc.ax axioms. *)

PROCEDURE Check(bgPush: RefList.T; vc: RefList.T): Result;
(* Assert background predicate, prove the VC, return result. *)

END ESCProve.
