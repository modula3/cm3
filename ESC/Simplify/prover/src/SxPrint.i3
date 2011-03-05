(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
INTERFACE SxPrint;

IMPORT Wr, MatchingRule;

IMPORT Thread;

PROCEDURE Print (wr       : Wr.T;
                 sx       : REFANY;
                 READONLY sub: MatchingRule.Substitution
                                 := MatchingRule.EmptySub;
                 maxDepth : CARDINAL := LAST (CARDINAL);
                 maxLength: CARDINAL := LAST (CARDINAL)  )
  RAISES {Wr.Failure, Thread.Alerted};

END SxPrint.
