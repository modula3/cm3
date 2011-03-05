(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue May 14 21:10:56 PDT 2002 by saxe                     *)
(*      modified on Fri Jul 26 09:41:23 PDT 1996 by detlefs                  *)

INTERFACE Match;

IMPORT Clause, Wr, RefList, Prover;

VAR
  depth, maxDepthSeen: CARDINAL;

VAR
  unitMatchRules: RefList.T; (* OF MatchingRule.Rule *)
  nonUnitMatchRules: RefList.T; (* OF MatchingRule.Rule *) 

(* This interface defines two sets of "MatchingRule.T"s, one for unit
   and one for non-unit rules, and a stack of matching states.  A
   matching state is a set "rules" of matching rules currently
   asserted, and "depth".  "depth" is the number of times unrestricted
   non-unit matching has been performed along the current search path;
   "maxDepthSeen" is the maximum value this variable has taken over
   the entire search so far.  *)

PROCEDURE UnitMatch() RAISES {Prover.Timeout};
(* Matches all built-in rules whose right hand sides produce unit
   clauses.  Propagates the resulting literals.
*)

PROCEDURE NonUnitMatch(): Clause.T RAISES {Prover.Timeout};
(* Matches all built-in rules whose right hand sides produce non-unit
   clauses, returning the conjunction of the instantiations of those
   right hand sides as a clause list head.  If "saveMatch" is "TRUE",
   records the last match in "Match.lastRule" and "Match.lastSub", as
   an aid in debugging matching loops.
*)

PROCEDURE SelStoreDist(): BOOLEAN;
(* If the egraph contains any nodes of the form

| a[i := v][j]    ,

   where it is known that "i # j", and the equality

| a[i := v][j] = a[j]

   is not known, propagates that equality.  Returns true if any such
   equalities are propagated. *)

PROCEDURE MInit();
(* "aub := TRUE, fnuB := TRUE, rules := {},
    depth := 0; maxDepth := 0; Stack := []" *)

PROCEDURE MPush();
(* Save the current matching state.  That is, set

| SC:hipush((auB, fnuB, rules, depth)).
*)

PROCEDURE MPop();
(* Restore "C" to its last saved state.  That is, set

| auB, fnuB, rules, depth := SC:hipop().

*)

PROCEDURE AttributeCaseSplit(id: CARDINAL; hash: INTEGER);
(* Attribute the last case split to the rule whose identifier is "id". *)

PROCEDURE RuleCensus(wr: Wr.T);

PROCEDURE ResetRuleScores();

END Match.
