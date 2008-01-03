(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: DFAState.i3,v 1.2 2001-09-19 15:05:08 wagner Exp $ *)

INTERFACE DFAState;
IMPORT DFATransList;
IMPORT NFAState;
CONST
  Brand = "DFAState";
TYPE
  T = REF RECORD
    next: DFATransList.T;
    ID: INTEGER;
    output: INTEGER;
    src: NFAState.T;
  END;

PROCEDURE Equal(a,b:T):BOOLEAN;

END DFAState.
