(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

INTERFACE NFAState;
IMPORT NFA;
IMPORT Word;
CONST
  Brand = "NFAState";
TYPE
  T <: REFANY;
(* An NFAState.T represents a collection of NFANode.T's
   which are the nodes occupied after an input sequence *)

(* Targets = REF ARRAY CHAR OF T; *)

(* Don't forget to call NFA.AssignIDs(a) before calling New*)
PROCEDURE New(a: NFA.T): T;           (* the start state *)

PROCEDURE Step(s: T; key: CHAR; VAR keyEnd: CHAR): T;
  (* copy and advance to the next state.
     same results will apply up to key=keyEnd. *)

(* PROCEDURE Steps(s: T): Targets; (* low-performance version of Step.
  Some elements in the returned array can be NIL, signifying Dead *) *)

PROCEDURE Dead(s: T): BOOLEAN; (* Are there no more transitions to be made? *)
PROCEDURE Output(s: T): INTEGER;  (* lowest output from last step, or 
                                  0 if none *)

PROCEDURE Equal(s, t: T): BOOLEAN;
PROCEDURE Hash(s: T): Word.T;

PROCEDURE Test(a: NFA.T);
PROCEDURE Format(s: T): TEXT;

END NFAState.
