(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: DFATrans.i3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

INTERFACE DFATrans;
IMPORT Word;
CONST
  Brand = "DFATrans";
TYPE
  T = RECORD
    keyBegin: CHAR;
    keyEnd: CHAR;
    target: INTEGER;
    prio: INTEGER := -1; (* first, prio is used in Sorting after Tally. *)
    (* then, when merging transition list tails, prio is the ID of
       the next transition in DFA.T.trans *)
    
  END;
PROCEDURE Format(a:T): TEXT;
PROCEDURE Equal(a,b:T): BOOLEAN;
PROCEDURE Hash(a:T): Word.T;
END DFATrans.
