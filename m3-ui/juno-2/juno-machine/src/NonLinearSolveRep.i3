(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Dec  8 15:44:34 1994 by gnelson                      *)
(*      modified on Mon Oct 31 18:31:26 PST 1994 by isard                    *)
(*      modified on Sun Sep 25 18:04:13 PDT 1994 by heydon                   *)

INTERFACE NonLinearSolveRep;

(* Reveals the "type" field of a "NonLinearSolve.Constraint". *)

IMPORT RedundantSolve AS NonLinearSolve;

TYPE
  ConType = { Plus, Minus, Halve, Times, Sin, Cos, Atan, MultTan, Exp };
  ConstraintRep = NonLinearSolve.ConstraintPub OBJECT
    type: ConType
  END;

REVEAL
  NonLinearSolve.Constraint <: ConstraintRep;

REVEAL
  NonLinearSolve.Constraint =
    ConstraintRep BRANDED "NonLinearSolve.Constraint" OBJECT
      availLink: NonLinearSolve.Constraint
    END;

VAR
  conAvail, conInUse: NonLinearSolve.Constraint := NIL;

END NonLinearSolveRep.
