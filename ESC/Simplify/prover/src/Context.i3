(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed May 15 22:16:48 PDT 2002 by saxe                     *)
(*      modified on Wed Oct 30 15:44:28 PST 1996 by detlefs                  *)

(* This interface represents a resettable formula.  Call this formula
   "F"; "by {\it resettable}, we mean that we also represent a stack
   "SF" of previous values of "F", and provide the ability to return
   to them.  "F" may or may not be satisfiable.  If the variable "sat"
   is "FALSE", then "F" is unsatisfiable; otherwise, the
   satisfiability of "F" is not yet known.

   The abstract formula "F" may have an infinite set of consequences.
   Let the {\it manifest context} be finite subset of these
   consequences that is explicitly represented by a set of cooperating
   decision procedures in the implementation.  These decision
   procedures can find the manifest context to be unsatisfiable, in
   which case "F" is also unsatisfiable.  In addition, the
   implementation also explicitly represents sets of unit and non-unit
   consequences (call these "lits" and "clauses", respectively) that
   have not yet been added to the manifest context.  Later in this
   interface, we will provide procedures which manipulate the
   explicitly represented consequences of "F".
 *)

INTERFACE Context;

IMPORT PredSx, AF, Prover;

VAR rs: PredSx.RenameState;

(* Each logical theory "Th" is embodied by a different interface, and
   provides a resettable global conjunction "Th.C" of literals of its
   theory.

   The context includes a "PredSx.RenameState" "fs" whose value is
   saved and restored by "Push" and "Pop".
*)

PROCEDURE Init();
(* "C := TRUE, SC := []". *)

PROCEDURE Assert(lit: AF.Lit);
(* Sets "F" to "F AND lit". *)

VAR pushes: CARDINAL; (* The number of saved contexts on the stack *)


PROCEDURE Push();
(* Save the current state of the conjunction "C".  That is, set
   "SC:hipush(C)".  Increments "pushes".)
*)

PROCEDURE Pop(incrementScores := TRUE);
(* Restore "C" to its last saved state.  That is, set "C :=
   SC:hipop()".  Set "Context.poppedASubproof" to "TRUE" if
   popping out of a goal subproof (i.e., lastrightmost case split)
   and to "FALSE" otherwise.  Decrements "pushes".)
*)

PROCEDURE InGoalSubproof(): BOOLEAN;
(* Returns "FALSE" if there are any "rightmost" case splits remaining
   to be performed on the current path.  Returns "TRUE" if at least
   one non-rightmost case split has been performed on the current
   path.  (Ideally, "InGoalSubproof" would start returning "TRUE"
   immediately after the last rightmost case split on any path,
   but in paractice it may take a while to determine that no
   rightmost splits remain.)
*)

PROCEDURE InLabelSubproof(): BOOLEAN;
(* Returns "TRUE" iff
   (a) the current set of asserted atom labels includes any label whose
       name contains the character '@' (this condition is encoded in
       "LabelName.anyAtSignLabels"), or
   (b) "InGoalSubproof()" would return "TRUE"
*)


PROCEDURE Top(kind := Prover.ResKind.Counterexample;
              labelsOnly: BOOLEAN := FALSE;
              includeSimplex := TRUE): Prover.ProveRes;
(* Return a "p" such that "p.context" is "C" represented as a list of
   S-expressions representing literals, and "p.lbls" is the list of
   atomic labels asserted in the satisfying context.  Attempts to
   eliminate identities from the list. "Top" modifies "C".  If "labelsOnly"
   is "TRUE", then "Top" computes "p.lbls" as usual, but "p.context" will
   be "NIL".  If "includeSimplex" is "FALSE" the "p.context" will not
   include Simplex inequalites (and, we think, "C" will then be unmodified).
*)

PROCEDURE ResetStats();
(* Reinitialize statistics counters. *)

PROCEDURE FinishStats();
(* Finalizes statistics counters. *)

PROCEDURE Stats();
(* Print out some performance statistics on standard error. *)

TYPE
  Ops = { AssertLits,
          ScanClauses,
          UnitMatch,
          SelStoreDist,
          RestrictedNUMatch,
          GetSplit,
          TightenBounds };
  OpEnabledArr = ARRAY Ops OF BOOLEAN;

VAR
  inGoalSubproof: BOOLEAN;
  fruitlessSplitLimit: INTEGER;
  quiescenceDepth: INTEGER;
  opsEnabled: OpEnabledArr;
  sat: BOOLEAN;
  tightenBoundsUseful: BOOLEAN;
    (* "TRUE" if the "tightenBounds" tactic has ever led directly to a
       contradiction at a point in the search tree that is both
         (a) below the last point where "tightenBounds" has been
             tried on the current path, and
         (b) within the same same goal subproof as the current
             context.
    *)
  poppedASubproof: BOOLEAN;

PROCEDURE AssertLits();
(* Adds all the literals in "lits" to the manifest context, stopping
   (and setting "sat" to "FALSE") if "F" is found to be
   unsatsifiable. *)

PROCEDURE ScanClauses();
(* Scans "clauses" for clauses that can be reduced to units, adding
   them to "lits", or to empty clause, in which case it sets "sat" to
   "FALSE" and stops. *)

PROCEDURE UnitMatch() RAISES {Prover.Timeout};
(* May add unit consequences of "F" to "lits". *)

PROCEDURE SelStoreDist();
(* If any term of the form "select(store(m, i, v), j)" is present in
   the egraph, where "i" is known to be distinct from "j", propagates
   an equality between that term and "select(m, j)". *)  

PROCEDURE RestrictedNUMatch() RAISES {Prover.Timeout};
(* May find non-unit consequences of "F"; if so, attempts to use {\it
   depth-1 plunging} to reduce them to unit clauses, which are added
   to "lits", or an empty clause, which sets "sat" to "FALSE" and
   terminates the procedure.  Clauses that cannot be reduced to units
   are added to "clauses".  If "saveMatch" is "TRUE", records the last
   match in "Match.lastRule" and "Match.lastSub", as an aid in
   debugging matching loops. *)

PROCEDURE TightenBounds() RAISES {Prover.Timeout};
(* Invokes a procedure of the "Simplex" module (one of the decision
   procedures that represents the manifest context) that may set "sat"
   to "FALSE" or add literals to "lits". *)

PROCEDURE UnitConsequences(maxEffort: [0..3] := 3) RAISES {Prover.Timeout};
(* Attempts to deduce and assert unit consequences of the manifest
   context, setting "sat" to "FALSE" if they become unsatisfiable.
   "maxEffort" sets the maximum method tried: 0 = literal assertion, 1
   = clause scanning, 2 = unit matching, 3 = non-unit matching.
*)

PROCEDURE GetSplit(): AF.Lit;
(* Assumes that "opsEnabled[Ops.GetSplit]" is "TRUE".  If so,
   returns a literal from some clause.
*)

VAR scanGeneration: INTEGER := 0;

END Context.
