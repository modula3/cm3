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
                VAR numStates: INTEGER): PDATransListList.T;

END LALR.
