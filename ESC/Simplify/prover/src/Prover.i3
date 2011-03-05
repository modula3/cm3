(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jun  4 12:13:13 PDT 2002 by saxe                     *)
(*      modified on Fri Oct 23 14:01:30 PDT 1998 by gnelson                  *)
(*      modified on Fri Nov  1 16:06:47 PST 1996 by detlefs                  *)

(* This is the basic interface to the theorem prover. *)

INTERFACE Prover;

(* The prover has a global set of "Axioms", which are
   "MatchingRule.Rule"s, which it uses in addition to its built-in
   knowledge about equality, disequality, and linear arithmetic.

   The prover maintains a conjunction of literals "BG", and
   a stack of conjunctions "SBG" to provide the ability to return to
   previously saved states.

   If all occurrences of any given function symbol in a formula
   have the same arity, then that formula is said to have consistent
   arity.  Consistent arity is a necessary condition for syntactic
   correctness, so "Assert", "Prove" and "IsValid" raise the error
   exceptions on formulas with inconsistent arity.  In addition, the
   arity of a use of a function in a formula passed to one of these
   functions must agree with its uses in any previous calls to
   "Assert" (that have not been undone by "Pop").  Thus, the prover
   also maintains a table mapping function symbols to their arities;
   "Push" stores, and "Pop" restores, the state of this table.
*)

IMPORT PredSx, Rd;
IMPORT AtomList, RefList, Trit, Time;

EXCEPTION Error(TEXT);
EXCEPTION Timeout;

PROCEDURE Init() RAISES { Error };
(* Must be called before using any variables or other procedures of
   this interface.  Initializes the global list of axioms to the empty
   list, "BG" to "TRUE", and "SBG" to be the empty stack.  Is idempotent. *)

PROCEDURE AddAxioms(rd: Rd.T) RAISES { Error };
(* "rd" must be a reader onto a sequence of syntactically correct
   axioms; if not, raises "Error".  Axioms are predicates in the
   syntax described in "PredSx.i3".  The call is equivalent pushing
   all the axioms in "rd" as a background predicate.
*)

PROCEDURE Push();
(* Pushes "BG" onto "SBG". *)

PROCEDURE Pop() RAISES { Error }; 
(* Sets "BG" to the top element of "SBG" and pops "SBG".  Raises
   "Error" if "SBG" is the empty stack. *)

PROCEDURE Assert(f: PredSx.T; shred: BOOLEAN) RAISES { Error };
(* Conjoins "f" to "BG".  If "f" is not syntactically correct, raises
   "Error".  Note that this procedure modifies "lit" if "shred" is
   true; to prevent excess storage retention, if "f" is a "RefList.T",
   it recursively "NIL"s out the head and tail fields of "f" and its
   descendents after it has been used.  Therefore, if clients need to
   use "f" or any subparts after the call, they must make a copy or
   set "shred" to "FALSE".*)

PROCEDURE UnitConsequences() RAISES {Timeout};
(* May augment "BG" with unit consequences. *)

TYPE
  ResKind = {Counterexample, XKillTime, XKillIter, CCLimit,
             XSubKillTime, XSubKillIter};
  ProveRes = OBJECT
    kind: ResKind;
    formula: PredSx.T := NIL;      
    lbls: AtomList.T := NIL;
    context: PredSx.T := NIL;
  END (* OBJECT *);

TYPE ResClosure =
  OBJECT METHODS
    apply(res: ProveRes)
  END;

VAR (*CONST*) InvalidRes: ProveRes;

PROCEDURE PredDef(rl: RefList.T) RAISES { Error };
(* Requires "rl" to be a "DEFPRED" or "DEFPREDMAP" form.  Adds it to
   the current prover state. *)
   
PROCEDURE Prove(form: PredSx.T;
                cl: ResClosure;
                ccLimit: INTEGER;
                shred: BOOLEAN;
                pruning := FALSE): Trit.T
    RAISES { Error };
(* If "form" is a well-formed "PredSx.T", finds up to "ccLimitParam" (or
   up to "ccLimitGlobal", if "ccLimitParam <= 0") satisfying
   contexts (of type "ProveRes") and for each such context, "res", calls
   "cl.apply(res)".  The "maxMatchDepth" argument determines the
   maximum match depth used in the proof; a value of "-1" causes the
   value "Prover.defMaxMatchDepth" to be used.  The "useTightenBounds"
   argument controls whether the "Context.TightenBounds" operation is
   used; this operation finds integer variables whose values are
   constrained to some closed range, and attempts to derive
   contradications by trying extreme values.  Note that this procedure
   modifies "form" if "shred" is true; to prevent excess storage
   retention, if "form" is a "RefList.T", it recursively "NIL"s out
   the head and tail fields of "f" and its descendents after it has
   been used.  Therefore, if clients need to use "form" or any
   subparts after the call, they must make a copy or set "shred" to
   "FALSE".  Returns Trit.False if at least one counterexample was
   found.  Returns Trit.Unknown if no counterexamples were found
   but part of the search space was unexplored due to resource limits.
   Returns Trit.True if full exploration of the search space yielded
   on counterexamples. *)

PROCEDURE Prune(sat: ProveRes): ProveRes RAISES { Error };
(* Returns a "ProverRes" logically euivalent to "sat" (with respect to
   the current context) but with redundant elements of "sat.context"
   removed.  Requires that sat be non-NIL and that sat.context be
   non-NIL. *)

TYPE SubsumptionChoice = { None, NonProxy, All, Conjunctive };

VAR useSubsumption := SubsumptionChoice.NonProxy;
(* The "useSubsumption" variable controls the use of a heuristic in
   the backtracking search.  When the theorem prover does a case split
   on a clause "(OR A B)", and tries "A" first but fails to satisfy
   it, it may safely assert "(NOT A)" before attempting to satisfy
   "B"; doing so is using {\it subsumption}.  The use of subsumption
   sometimes helps and sometimes hurts performance, so it is left
   under programmer control.  The default value is "None", meaning
   subsumption is never used.  If "All" means it is always used;
   "NonProxy" means it is used for all literals whose atomic formulas
   are not ProxyPropvars.  For added flexibility, the prover will
   initialize "useSubsumption" to "None", "All" or "NonProxy"
   if the environment variable "PROVER_SUBSUMPTION" is one of those
   strings.  If is a runtime error to set "PROVER_SUBSUMPTION" to any other
   value.
*)

VAR defMaxMatchDepth: INTEGER := 3;
(* To prevent infinite matching loops, we cap the matching depth
   (number of attempts to find non-unit matches) on any branch of the
   search tree at some limit; "defMaxMatchDepth" is the default value
   of this limit.  The prover will initialize "defMaxMatchDepth" to
   the value of the environment variable  "PROVER_MATCH_DEPTH" if that
   is a valid integer.
*)

VAR maxPruneMatchDepth: INTEGER := 0;
(* When we are computing a satisfying context, we first compute a
   context, then consider its literals one-by-one, attempting to find
   and discard literals that are implied pure consequences of the
   background predicate.  If a literal "l1" is not a consequence of
   the background predicate, this can mean that maximum allowed amount of
   non-unit matching is done without finding a contradication.  If the
   background predicate contains non-matching-rule literals that
   provide starting points for the matching process, this can be quite
   lengthy.  Thus, we have a different matching depth limit for this
   "pruning" process.  For added flexibility, the prover
   will initialize "maxPruneMatchDepth" to the value of the
   environment variable "PROVER_PRUNE_MATCH_DEPTH" if that is a valid
   integer.
*)

VAR maxMatchDepth: INTEGER;

VAR maxFNURnum: INTEGER := 100;
(* To detect matching cycles, the prover gives up if "maxFNURnum" 
   rounds of restricted non-unit matching are performed between case
   splits.  (Recall that restricted non-unit matching harvests only
   literals after depth-1 plunging.)  The prover will initialize
   "maxFNURnum" to the value of the environment variable
   "PROVER_MAX_FNUR" if that is a valid integer.
*)

VAR maxMatchFNUR := LAST(INTEGER);
(* To further limit the amount of matching done, maxFNURnum is the
   maximum number of rounds of restricted non-unit matching (that is,
   non-unit matching where only literals are harvested after depth-1
   plunging) when the match depth is equal to "maxMatchDepth".  The
   prover will initialize "maxMatchFNUR" to the value of the
   environment variable "PROVER_MAX_MATCH_FNUR" if that is a valid
   integer.  *)

VAR minDistClassSize: INTEGER := 4;
(* A "DISTINCT" literal asserted in the positive sense containing fewer
   than "minDistClassSize" terms will be implemented by asserting pairwise
   distinctions; this size or above will cause a distinction class to
   be created.  Initialized from the value of "PROVER_MIN_DIST" if
   that is a valid integer.
*)

VAR distClassNeqNodes: BOOLEAN := FALSE;
(* If "TRUE", creates "NEQ" nodes for nodes declared distinct via a
   "DISTINCT" predicate. *)

TYPE ClauseShuffle = { None, Reverse, Rand };

VAR clauseShuffle := ClauseShuffle.Reverse;
(* Determines how clauses resulting from Non-unit matching are ordered
   after depth-1 plunging.  "None" indicates no change to the
   literal list produced by matching; "Reverse" indicates that the
   literal list is reversed; and "Rand" indicates a random permutation.
   If the environment variable "PROVER_CLAUSE_SHUFFLE" is set to one
   of these enumeration constant names, the variable is initialized to
   that value.
*)

VAR unitMatchInstance := FALSE;
(* If "TRUE", creates and leaves an instantiation of every unit matching
   rule.  If the environment variable "PROVER_U_MATCH_INST" is set,
   the variable is initialized to "TRUE".
*)

VAR nuMatchInstance := FALSE;
(* If "TRUE", asserting a non-unit matching rule adds a general
   instantiation of that matching rule to the clause list.
   If the environment variable "PROVER_NU_MATCH_INST" is set,
   the variable is initialized to "TRUE".
*)


VAR unitMatchLimit := 1000000000;
(* Bound on number of rounds of unit matching performed before giving
   next tactic a chance.  Must be at least 1.  Set by the environment
   variable "PROVER_U_MATCH_LIMIT".
*)

VAR nuMatchLimit := 1000000000;
(* Bound on number of rounds of unit matching performed before giving
   next tactic a chance.  Must be at least 1.  Set by the environment
   variable "PROVER_NU_MATCH_LIMIT".
*)

VAR maxTrueCNF := 100;
(* When creating a matching rule from a quantified formula, the prover
   will attempt to convert the body of the quantified formula into a
   conjunction of clauses by first distributing FORALL over any
   conjunctions explicit in the body, yielding a conjunction of
   disjunctions (or literals).  If a disjunction contains conjunctions,
   we compute an upper bound on the number of literals in the "true CNF"
   of the disjunction; if that upper bound is less than "maxTrueCNF", we
   compute the true CNF and make clausal rules; otherwise, the
   disjunction will be treated as a rule in general form.
   If the environment variable "PROVER_MAX_TRUE_CNF" is set to a
   string naming an integer, the variable is initialized to the value
   of that integer.  *) 

VAR maxProxyPropCNF := 100;
(* The largest conjuncts/width product allowed when expanding the
   proxy prop var resulting from interning the instantiation of a
   non-clausal rule or a predicate definition.  Settable by
   "PROVER_MAX_PROXY_PROP_CNF". *)

VAR lazySimplexIntern := TRUE;
(* If "TRUE", sums, differences, etc. are conveyed to the Simplex
   tableau only when atomics formula's involving them are asserted.
   Otherwise, they are declared at CNF-time.  
   If the environment variable "PROVER_NO_LAZY_SIMPLEX_INTERN" is set,
   this variable is initialized to "FALSE".
*)

VAR allowNUMatchCycle := FALSE;
(* If "FALSE", forbids non-unit patterns whose instantiations contain larger
   matches.  Set to "FALSE" if the environment variable
   "PROVER_ALLOW_NU_CYCLE" is set. *)

VAR useImmediatePromote := TRUE;
(* If "TRUE", instances of matching rules that have been declared to
   be "immediatePromote" rules are immediately added to the (head of
   the) clause list when they are found.  Initialized to "TRUE" if the
   environment variable "PROVER_NO_USE_IMMED_PROMOTE" is set. *)

VAR d1PMaxEffort := 0;
(* Determines the "effort level" expended during depth-1 plunging.
   0 indicates "AssertLits", 1 indicates clause scanning,
   2 allows unit matching, and 3 allows restricted non-unit
   matching.  Set by the environment variable "PROVER_D1P_MAX_EFFORT". *)

VAR litSplit := FALSE;
    litSplitPct := 10;
    litSplitMinClauses := 5;

(* If "litSplit" is "TRUE", then Simplify will attempt to split on literals
   for each literal that appears in at least "litSplitMinClauses", and
   at least "litSplitPct" percent of all the clauses in the clause list.
   Initialized from the following environment variables if they are
   set: "litSplit" from "PROVER_LIT_SPLIT",  "litSplitPct" from
   "PROVER_LIT_SPLIT_PCT", and "litSplitMinClauses" from
   "PROVER_LIT_SPLIT_MIN_CLAUSES". *)

VAR promoteSize := 10;
(* The bound on the size of the set of clauses to promote.  If the
   environment variable "PROVER_PROMOTE_SIZE" is set to an integer,
   is initialized to that value. *)

VAR allowSinglePatVarPat := TRUE;
(* If true, will choose single pattern variables as patterns, thus
   guaranteeing that every quantifier can be converted to a matching
   rule.  If FALSE, crashes if a quantifier has no patterns. If the
   environment variable "PROVER_NO_ALLOW_1VAR" is set, is initialized to
   TRUE.
*)

VAR maxImmedPromote := 10;
(* The maximum number of immediate promotions that will be done between any
   two case splits on non-immedate-promote clauses.  Set by the
   environment variable "PROVER_MAX_IMMED_PROMOTE".  A negative value
   disables the limit. *)

VAR noEnodeStatus := FALSE;
(* If "TRUE", AF.Status (and AF.DeepStatus) doesn't use Enode.Status.
   Set by the environment variable "PROVER_NO_ENODE_STATUS". *)
VAR intStatus := FALSE;
(* If "TRUE", AF.Status (and AF.DeepStatus) use Simplex.GEStatus.
   Set by the environment variable "PROVER_INT_STATUS". *)
VAR deepStatus := FALSE;
(* If "TRUE" clause scanning uses a DeepStatus test that recurses down the
   tree of proxies.  Set by the environment variable "PROVER_DEEP_STATUS". *)

VAR noPlunge := FALSE;
(* If "TRUE", Non-unit matching doesn't use plunging; all matches go right
   on pending clause list, to be scanned by clause scanning.  Set by 
   environment variable "PROVER_NO_PLUNGE" (and also by Simplify
   command-line argument "-noplunge"). *)

VAR usePlungeHints := FALSE;
(* If "TRUE", Non-unit matching uses plunging only as directed by
   "PLUNGE" hints in user-supplied patterns.  Set by the environment
   variable "PROVER_USE_PLUNGE_HINTS".  This variable is irrelevant
   when "noPlunge" is "TRUE", since then plunging will not be used
   in any case. *)

VAR noSelStore := FALSE;
(* If "TRUE", the select-of-store inference method is disabled.
   Set by environment variable "PROVER_NO_SEL_STORE". *)

VAR noActivate := FALSE;
(* If "TRUE", is something of a misnomer, since it actually causes
   all enodes to be activated on creation.  Note also that it undoes
   lazy Simplex interning. Set by environment variable
   "PROVER_NO_ACTIVATE". *)

VAR forceActivateStart := LAST(INTEGER);
VAR forceActivateEnd := LAST(INTEGER);
VAR actTestNumber := 0;
(* "actTestNumber" counts the tests of "noActivate"
    (see above) over the entire run, with the.  If "noActTestNumber" is
    in the range "[forceActivateStart, forceActivateEnd)" then the
    prover will force activation regardless of the value of
    "noActivate".  "forceActivateStart" and "forceActivateEnd" are
    set by the environment variables PROVER_FORCE_ACTIVATE_START and
    PROVER_FORCE_ACTIVATE_END respectively. *)

VAR eqOptActivate := FALSE;
(* If "TRUE", peephole optimizations in interning that reduce "(EQ x y)"
   to "TRUE" (when "x" is equivalent to "y") or "FALSE" (when "x" has
   been distinguished from "y") will also activate "x" and "y").
   Set by environment variable "PROVER_EQ_OPT_ACTIVATE". *)

VAR noLabels := FALSE;
(* If "TRUE", label forms in the input become operationally, as well
   as semantically, equivalent to their formula arguments.  That is,
   the labels are ignored.  Set by environment variable
   "PROVER_NO_LABELS". *)

VAR noModTimes := FALSE;
(* If "TRUE", the prover does not use the modification time heuristic,
   and also does not update the information it uses.  Set by the
   environment variable "PROVER_NO_MODTIMES". *)

VAR noPatElems := FALSE;
(* If "TRUE", the prover does not use the pattern element optimzation,
   and also does not update (some of) the information it uses.  Set by the
   environment variable "PROVER_NO_PATELEMS". *)

VAR noObvSimpRedund := FALSE;
(* If "TRUE", the prover does not attempt to cheaply detect redundant
   simplex inequalities.  Set by the environment variable
   "PROVER_NO_OBV_SIMP_REDUND". *) 

VAR noPromoteTB := FALSE;
(* If "TRUE", the prover does not promote the priority of the TightenBounds
   tactic.  Set by the environment variable "PROVER_NO_PROMOTE_TB". *)

VAR noMinHt := FALSE;
(* If "TRUE", the prover does not use comparison of minimum heights in
   selecting the fingerprint for merged equivalance classes.  Set by
   "PROVER_NO_MINHT". *)

VAR tacticTrace := FALSE;
(* If "TRUE", the prover prints out the tactic used each time through the
   main loop in Satisfy.Search.  Set by the environment variable
   "PROVER_TACTIC_TRACE". *)

VAR propagateTrace := FALSE;
(* If "TRUE", the prover prints each atomic formula propagated to
   the context (by "ContextPrivate.Propagate").  Set by the environment
   variable "PROVER_PROPAGATE_TRACE". *)

VAR orderTrack := FALSE;
(* If "TRUE", the prover prints the state of all ordering theories
   at the start of each iteration of the main loop in "Satisfy.Search".
   Set by the environment variable "PROVER_ORDER_TRACK". *)

VAR internDebug := FALSE;
(* If "TRUE", the prover provides some tracing of conversion of
    "Sx.T"s to internal form.  Set by the environment variable
   "PROVER_INTERN_DEBUG". *)

VAR ccLimitGlobal := 1;
(* Specifies the maximum number of counterexamples contexts to be dispalyed
   for any single conjecture.  Set by the environment variable
   "PROVER_CC_LIMIT. *)

VAR envVars := "";
(* Contains the PROVER environment variables set and what they are set to. *)

VAR showCC := TRUE;
(* If "TRUE", report full counterexample contexts, and not just labels
   for invalid conjectures.  Set by a client of the Prover interface.
   (Simplify's -labelsonly and -nosc command line switches set it to
   "FALSE".) *)

VAR killContext := TRUE;
(* If "TRUE", report full contexts, and not just labels when prover
   abandons a proof or subproof due to time or iteration limits.
   Set by the environment variable "PROVER_KILL_CONTEXT". *)

VAR fruitlessSplitLimit := 1000000;
(* If more than "fruitlessSplitLimit" non-rightmost case splits are
   to reach a contradiction in some part of the search space, then
   Simplify will backtrack to the beginning of the entire sequence of
   fruitless splits (retaining scores and promotions due to the
   eventual contradiction) and try again.  Set by the environment variable
   "PROVER_FRUITLESS_SPLIT_LIMIT". *)

VAR killTime: Time.T := FLOAT(1000000000, LONGREAL);
(* Bound on time to spend in main search loop for any conjecture.  Set
   by the environment variable "PROVER_KILL_TIME". *)

VAR killIter: INTEGER := LAST(INTEGER);
(* Bound number of iterations of main search loop for any conjecture.  Set
   by the environment variable "PROVER_KILL_ITER". *)

VAR subKillTime: Time.T :=  FLOAT(1000000000, LONGREAL);
(* Bound on time to spend in main search loop for any label subproof.  Set
   by the environment variable "PROVER_SUB_KILL_TIME". *)

VAR subKillIter: INTEGER := LAST(INTEGER);
(* Bound number of iterations of main search loop for label subproof.  Set
   by the environment variable "PROVER_SUB_KILL_ITER". *)

VAR useTB := TRUE;
(* If "TRUE" causes the prover to use the `tighten bounds' tactic implemented
   by "Simplex.TightenBounds".  Set to "FALSE" by setting the environment
   variable "PROVER_NO_USE_TB". *)

VAR triggerlessWarnLimit := 1;
(* Maximum number of times to warn about triggerless matching rules
   for a single conjecture.  Set by the environment variable
   "PROVER_TRIGGERLESS_WARN_LIMIT". *)

VAR triggerlessWarnCountdown := 0;
(* Maximum number of warnings about triggerless matching rules to warn
   about for remainder of current command. *)

VAR harvestTrace := FALSE;
(* Print trace information about literals harvested by clause scanning.  Set
   to "TRUE" by setting the environment variable "PROVER_HARVEST_TRACE". *)

VAR oldOutput := FALSE;
(* Use (approximately) the old output format from before the May 2000
   release.  Set by the environment variable PROVER_OLD_OUTPUT.  Likely to
   be disabled soon.  *)

VAR skolemizeOuterTrace := 0;
(* Trace calls to PredSx.SkolemizeOuterWork to this depth and width
   (if > 0).  Set by the environment variable PROVER_SKOLEMIZE_TRACE *)

VAR initialQuiescenceDepth := 1;
(* Number of times to retry quiescence along any path in the proof tree.
   Set by the environment variable PROVER_QUIESCENCE_DEPTH. *)

VAR noMeritScoring := FALSE;
(* Disables merit heuristic.  Set by the environment variable
   PROVER_NO_MERIT_SCORING. *)

VAR pruneDebug := FALSE;
(* Print trace output for pruning.  Set by the environment variable
   PROVER_NO_MERIT_SCORING. *)

VAR inPruning := FALSE;
(* True while pruning a counterexample context *)

END Prover.








