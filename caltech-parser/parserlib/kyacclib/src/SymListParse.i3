(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

INTERFACE SymListParse;
IMPORT Rd, SymList;
IMPORT CharRange;

PROCEDURE BackGetName(rd: Rd.T): TEXT;
(* unget a char and get a word *)

PROCEDURE Parse(rd: Rd.T; allowedChars: CharRange.T): SymList.T;
(* parse list until end of line *)

END SymListParse.
