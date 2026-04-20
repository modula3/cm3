(* LALR(1) parser table construction.
   Builds LR(0) automaton, computes LALR(1) lookaheads via
   spontaneous generation and propagation (Dragon Book Algorithm 4.63),
   then emits PDA transition lists with conflict resolution. *)

INTERFACE LALR;
IMPORT RuleList;
IMPORT PDATransListList;
IMPORT TextTextTbl;

PROCEDURE Build(rules: RuleList.T;
                codes: REF ARRAY OF INTEGER;
                symNames: REF ARRAY OF TEXT;
                warnings: TextTextTbl.T;
                VAR numStates: INTEGER;
                shiftDefault: BOOLEAN := FALSE): PDATransListList.T;
(* If shiftDefault is TRUE, shift/reduce conflicts with no precedence
   info default to SHIFT (standard yacc behavior).  If FALSE, the
   kyacc heuristic applies: epsilon rules shift, non-epsilon reduce. *)

END LALR.
