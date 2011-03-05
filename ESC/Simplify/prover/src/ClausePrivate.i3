(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed May  5 18:10:31 PDT 1999 by saxe                         *)
(*      modified on  by detlefs                      *)
(*      modified on Wed Jul 10 21:55:23 PDT 1996 by detlefs                      *)
(*      modified on Thu Mar 21 17:57:53 1996 by gnelson                      *)

(* Exports the "Intern" procedure, which is used by "Match". *)

INTERFACE ClausePrivate;

IMPORT Clause, Enode, MatchingRule, Prover, AF;
IMPORT FPrint, Wr;

TYPE
  ClauseRep = OBJECT
    pred, succ: Clause.T := NIL;
    lits: AF.LitList;
    mr: MatchingRule.T := NIL;
    score := 0.0;
    parent: Clause.T := NIL;
    uid: INTEGER;
    promoted := PromoteState.Not;
    fp: FPrint.T;
   METHODS
    init(): Clause.T;
  END (* OBJECT *);
REVEAL
  Clause.T <: ClauseRep;
TYPE
  PromoteState = { Not, Immed, Promoted };

(* The call "cl.init()" destructively modifies "cl.lits". *)

PROCEDURE Intern(
    sx: REFANY;
    READONLY sub: MatchingRule.Substitution := MatchingRule.EmptySub): Enode.T;
(* Returns the unique "Enode.T" corresponding to the term represented
   by "sx". *)

PROCEDURE SxToLiteral(
    sx: REFANY;
    READONLY sub: MatchingRule.Substitution): AF.Lit
  RAISES { Prover.Error };
(* Require that "sx" is a "<literal>", as defined in "PredSx", or a
   universally quantified formula whose body is in CNF (considering
   quantified subformulas to be literals.).  If the input does not
   meet these restrictions,  raises "Error".  Otherwise, returns the
   corresponding "AF.Lit". *)

(* REMOVE
PROCEDURE SxToConjunction(sx: REFANY): AF.Lit RAISES { Prover.Error };
(* Require "sx" to be a (possibly empty or singleton) conjunction of
   "<literal>", as defined in "PredSx", or other conjunctions.
   If the input does not meet these restrictions,  raises "Error".
   Otherwise, returns the corresponding "AF.Lit". *)
*)

PROCEDURE SxToClause(
    sx: REFANY;
    READONLY sub: MatchingRule.Substitution := MatchingRule.EmptySub): Clause.T
  RAISES { Prover.Error };
(* Require that "sx" is a clause, a disjunction of literals or a single literal.
   If the input does not meet these restrictions, raises "Error".
   Otherwise, returns the corresponding "Clause.T". *)

PROCEDURE Init();
(* Initializes "atoms" to the empty set.  Must be called before
   calling any of the procedures in "Clause." *)


PROCEDURE Covers(p1, p2: MatchingRule.Pattern): BOOLEAN;
(* Returns "TRUE" iff pattern "p1" covers pattern "p2"; that is, any
   term that matches "p2" would also match "p1". *)

PROCEDURE ListAppendD(cl1, cl2: Clause.T): Clause.T;
(* Returns the head of the list that result from (destructively)
   appending the clause list headed by "cl2" to the end of the
   clause list headed by "cl1". *)

PROCEDURE DBGPrint(wr: Wr.T; cl: Clause.T);
(* Print "cl" to "wr". *)

END ClausePrivate.
