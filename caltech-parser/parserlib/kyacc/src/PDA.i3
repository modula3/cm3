(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

INTERFACE PDA;
IMPORT RuleList;
IMPORT PDATransList;
IMPORT TokSpec;
IMPORT TextIntTbl;
TYPE
  T <: Public;
  Public = OBJECT
    statesArray: REF ARRAY OF PDATransList.T;
    lastShift: INTEGER;
  METHODS
    fmtSymbols(): TEXT;
    symInfo(VAR numSym, lastCode: INTEGER);
  END;
PROCEDURE New(rules: RuleList.T;
              tok: TokSpec.T;
              codeTbl: TextIntTbl.T): T;
PROCEDURE Test(self: T);
END PDA.
