(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: DFA.i3,v 1.2 2001-09-19 15:05:08 wagner Exp $ *)

INTERFACE DFA;
IMPORT NFA;
IMPORT DFAState;
IMPORT DFAStateList;
IMPORT DFATrans;
IMPORT DFATransList;
TYPE
  T = REF RECORD
    numStates: INTEGER := 0;
    states: DFAStateList.T := NIL;
    statesArray: REF ARRAY OF DFAState.T;
    numTrans: INTEGER := 0;
    trans: DFATransList.T := NIL;
    transArray: REF ARRAY OF DFATrans.T;
    first: ARRAY CHAR OF INTEGER;
  END;
PROCEDURE FromNFA(a: NFA.T): T;
PROCEDURE Test(a: T);
END DFA.
